
module_server <- function(input, output, session, ...) {


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    refresh = NULL,
    staged_table = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  # What the loaded file (`header_info`), signal (`signal_info`), candidates
  # (`threshold_info`) and staged onsets (`stage_log`) came from: read by the
  # agent scripts and written into the sidecar of `save_agent_epoch`
  local_data$stage_log <- list()

  # get server tools to tweak
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$auto_recalculate(FALSE)

  error_notification <- function(e) {
    # Also print it: agents read what a script prints
    ravepipeline::logger("Error found! ", paste(e$message, collapse = "\n"),
                         level = "error")
    shidashi::show_notification(
      message = e$message,
      title = "Error found!",
      type = "danger",
      close = TRUE,
      autohide = TRUE,
      class = ns("error_notif"),
      collapse = "\n"
    )
  }

  # A known input problem. A person operating the module gets a notification
  # that says what to change; an MCP call (SHIDASHI_USING_MCP is "TRUE") gets
  # the message as the script's error. The class `rave_muffled` keeps
  # ravedash from showing the error a second time
  stop_known <- function(message, title = "Error found!") {
    if (!identical(Sys.getenv("SHIDASHI_USING_MCP"), "TRUE")) {
      shidashi::clear_notifications(class = ns("error_notif"))
      shidashi::show_notification(
        message = message,
        title = title,
        type = "danger",
        close = TRUE,
        autohide = TRUE,
        class = ns("error_notif")
      )
    }
    stop(structure(
      list(message = message, call = NULL),
      class = c("rave_muffled", "simpleError", "error", "condition")
    ))
  }

  # One entry of the staging log
  add_stage_log <- function(action, block = NULL, onsets = NULL, ...) {
    entry <- list(action = action, block = block, onsets = onsets,
                  at = Sys.time(), ...)
    local_data$stage_log <- c(local_data$stage_log, list(entry))
    invisible(entry)
  }

  # Number of staged onsets per block of the subject
  staged_counts <- function() {
    staged_table <- local_reactives$staged_table
    blocks <- local_data$subject$preprocess_settings$blocks
    vapply(blocks, function(block) {
      if (!is.data.frame(staged_table)) { return(0L) }
      sum(staged_table$block %in% block)
    }, 0L)
  }

  # "008: 57, 010: 0" ...
  format_staged_counts <- function() {
    counts <- staged_counts()
    sprintf("%s (total %d)",
            paste(sprintf("%s: %d", names(counts), counts), collapse = ", "),
            sum(counts))
  }


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if (!loaded_flag) { return() }

      project_name <- pipeline$get_settings("project_name")
      subject_code <- pipeline$get_settings("subject_code")

      subject <- ravecore::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
      local_data$subject <- subject

      epochs <- c("New epoch...", subject$epoch_names)
      shiny::updateSelectInput(
        session = session,
        inputId = "load_epoch",
        choices = epochs,
        selected = "New epoch..."
      )

      blocks <- subject$preprocess_settings$blocks
      shiny::updateSelectInput(
        session = session,
        inputId = "block",
        choices = blocks,
        selected = blocks[[1]]
      )

      # Reset outputs
      shidashi::reset_output("plot_overall", message = "This plot has been reset")
      shidashi::reset_output("plot_subset", message = "")
      local_reactives$staged_table <- NULL
      local_reactives$threshold_timestamp <- NULL
      local_reactives$epoch_header <- NULL
      local_reactives$epoch_signal <- NULL
      local_reactives$threshold <- NULL
      local_reactives$staged_table <- NULL
      local_reactives$re_threshold <- NULL
      local_reactives$refresh <- Sys.time()

      # New data: the threshold field goes empty, and the bookkeeping for the
      # agent scripts starts over
      shiny::updateNumericInput(session = session, inputId = "threshold", value = "")
      local_data$header_info <- NULL
      local_data$signal_info <- NULL
      local_data$threshold_info <- NULL
      local_data$stage_log <- list()

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      load_epoch <- input$load_epoch
      if (!length(load_epoch) || load_epoch == "New epoch...") {
        return()
      }
      shiny::invalidateLater(300)
      shiny::updateSelectInput(
        session = session,
        inputId = "load_epoch",
        selected = "New epoch..."
      )
      subject <- local_data$subject
      if (!length(subject)) { return() }

      tryCatch({
        epoch <- subject$meta_data(meta_type = "epoch", meta_name = load_epoch)
        local_reactives$staged_table <- data.frame(
          block = epoch$Block,
          time = epoch$Time
        )
        # The staged onsets now come from this epoch only
        local_data$stage_log <- list(list(
          action = "loaded",
          epoch = load_epoch,
          trials = as.list(table(epoch$Block)),
          at = Sys.time()
        ))
      }, error = function(e) {
        ravepipeline::logger_error_condition(e)
        error_notification(e)
      })

    }),
    input$load_epoch,
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  server_tools$set_script(
    name = "load_epoch_file",
    description = c(
      "Same as clicking 'Load epoch file': reads the header of `epoch_file` in",
      "block `block` and fills the `varname` choices (a MATLAB/HDF5 variable, an",
      "EDF/NEV channel, or 'raw' for an imported channel). Run it only after the",
      "user told you which file or channel marks the trials: never pick it",
      "yourself. Its output lists the variables, the selected one, and how to",
      "treat the sample rate of this file type (MATLAB: ask the user). Known",
      "problems (e.g. 'Invalid epoch file') come back as the script's error.",
      "Returns 'type=...', 'variable=...', 'variables=N'."
    ),
    expr = {

      subject <- local_data$subject
      if (is.null(subject)) {
        stop_known("Invalid subject (subject not loaded?)")
      }

      epoch_file <- input$epoch_file
      block <- input$block
      if (length(epoch_file) != 1 || !nzchar(epoch_file)) {
        stop_known("Invalid epoch file. Please choose a proper epoch file.")
      }
      shidashi::clear_notifications(class = ns("error_notif"))

      dipsaus::shiny_alert2(
        title = "Loading...",
        text = "Loading file(s): depending on the file types, the loading time varies. Please be patient",
        icon = "info",
        auto_close = FALSE,
        buttons = FALSE,
        session = session
      )
      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2(session = session)
      })

      raw_root <- NULL
      epoch_channel_file <- NULL
      if (startsWith(epoch_file, "[Channel")) {
        aux_channel <- as.integer(gsub("[^0-9]", "", epoch_file))
        repository <- ravecore::prepare_subject_raw_voltage_with_blocks(
          subject = subject,
          electrodes = aux_channel,
          blocks = block
        )
        voltage <- repository$raw_voltage[[block]]$Auxiliary
        local_reactives$epoch_header <- list(
          header = list(
            voltage = unname(subset(voltage$data, Electrode ~ Electrode == aux_channel, drop = TRUE)),
            sample_rate = voltage$sample_rate
          ),
          type = "Imported"
        )
        varnames_choices <- "raw"
        varnames <- "raw"
      } else {
        raw_root <- subject$preprocess_settings$raw_path
        epoch_channel_file <- file.path(subject$preprocess_settings$raw_path, input$block, epoch_file)
        if (length(epoch_channel_file) != 1 || !file_test("-f", epoch_channel_file)) {
          raw_root <- subject$preprocess_settings$raw_path2
          epoch_channel_file <- file.path(subject$preprocess_settings$raw_path2, epoch_file)
        }
        if (length(epoch_channel_file) != 1 || !file_test("-f", epoch_channel_file)) {
          stop_known("Epoch channel file is invalid")
        }

        # read signal
        if (endsWith(tolower(epoch_channel_file), "edf")) {

          temporary_path <- file.path(subject$cache_path, "edf")
          # ravecore import code
          # header <- ieegio::read_edf(
          #   epoch_channel_file,
          #   extract_path = temporary_path,
          #   header_only = FALSE,
          #   cache_ok = TRUE,
          #   verbose = TRUE
          # )

          header <- ieegio::read_edf(
            epoch_channel_file,
            extract_path = nullfile(),
            header_only = TRUE,
            cache_ok = TRUE,
            verbose = TRUE
          )
          header$source_path <- epoch_channel_file
          header$cache_path <- temporary_path

          varnames_choices <- header$channel_table$Label
          varnames <- sprintf("DC%d", 1:12) %OF% varnames_choices
          local_reactives$epoch_header <- list(
            header = header,
            type = "EDF"
          )
        } else if (endsWith(tolower(epoch_channel_file), "nev")) {

          temporary_path <- ravepipeline::dir_create2(file.path(subject$cache_path, "neuroevent"))
          digest <- ravepipeline::digest(file = epoch_channel_file)
          header <- ieegio::read_nsx(
            file = epoch_channel_file,
            extract_path = file.path(temporary_path, digest),
            header_only = FALSE, include_waveform = FALSE,
            cache_ok = TRUE,
            verbose = TRUE
          )

          varnames_choices <- sprintf(
            "Channel %d [%s]",
            header$channel_table$original_channel,
            header$channel_table$name
          )
          sel <- order(header$channel_table$sampling_frequency, decreasing = TRUE)[[1]]
          varnames <- varnames_choices[sel] %OF% varnames_choices
          local_reactives$epoch_header <- list(
            header = header,
            type = "NEV"
          )
        } else {
          header <- ravecore:::read_mat2(epoch_channel_file, ram = FALSE)
          nm <- ravecore:::guess_raw_trace(header, electrodes = subject$electrodes)
          varnames_choices <- names(header)
          varnames <- c(nm, "analogTraces") %OF% varnames_choices
          local_reactives$epoch_header <- list(
            header = header,
            type = "MAT"
          )
        }
      }

      shiny::updateSelectInput(
        session = session,
        inputId = "varname",
        choices = varnames_choices,
        selected = varnames
      )

      # What was loaded (for the agent scripts and the sidecar of
      # `save_agent_epoch`), and a summary for agents
      header_type <- local_reactives$epoch_header$type
      extension <- tolower(tools::file_ext(epoch_file))
      local_data$header_info <- list(
        block = block,
        file = epoch_file,
        path = epoch_channel_file,
        raw_root = raw_root,
        type = header_type,
        extension = extension
      )
      header_rate <- tryCatch({
        hdr <- local_reactives$epoch_header$header
        switch(
          header_type,
          "EDF" = hdr$channel_table$SampleRate[hdr$channel_table$Label %in% varnames],
          "NEV" = hdr$channel_table$sampling_frequency[varnames_choices %in% varnames],
          "Imported" = hdr$sample_rate,
          NULL
        )
      }, error = function(e) { NULL })
      cat(sprintf(
        "Loaded the header of '%s' (block %s, type %s). %d variable(s): %s%s. Selected: '%s'.\n",
        epoch_file, block, header_type, length(varnames_choices),
        paste(utils::head(varnames_choices, 10), collapse = ", "),
        if (length(varnames_choices) > 10) ", ..." else "",
        paste(varnames, collapse = ", ")
      ))
      if (length(header_rate) == 1) {
        cat(sprintf("Sample rate in the file: %s Hz.\n", format(header_rate)))
      }
      cat(sample_rate_rule(header_type, extension), "\n")
      cat(paste(
        "Next: check `varname` and `sample_rate` with `shiny_input_info`, settle",
        "the sample rate with the user as above, then run `load_epoch_signal`.\n"
      ))
      c(
        sprintf("type=%s", header_type),
        sprintf("variable=%s", paste(varnames, collapse = ",")),
        sprintf("variables=%d", length(varnames_choices))
      )
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      server_tools$trigger_script("load_epoch_file")
    }),
    input$load_epoch_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      if (length(input$varname) != 1) { return() }
      epoch_header <- local_reactives$epoch_header
      if (!is.list(epoch_header) ||
         !isTRUE(epoch_header$type %in% c("MAT", "EDF", "NEV", "Imported"))) {
        return()
      }
      subject <- local_data$subject
      block <- input$block

      type <- epoch_header$type
      header <- epoch_header$header

      sample_rate <- 1

      switch(
        type,
        "EDF" = {
          sample_rate <- header$channel_table$SampleRate[
            header$channel_table$Label == input$varname
          ]
        },
        "NEV" = {
          # "Channel %d [%s]"
          varnames_choices <- sprintf(
            "Channel %d [%s]",
            header$channel_table$original_channel,
            header$channel_table$name
          )
          sel <- varnames_choices == input$varname
          if (any(sel)) {
            sample_rate <- header$channel_table$sampling_frequency[sel]
          } else {
            sample_rate <- 2000
          }
        },
        "MAT" = {
          sample_rate <- input$sample_rate
          if (isTRUE(sample_rate > 1)) { return() }

          varname <- input$varname
          if (length(varname) != 1 || !varname %in% names(header)) { return() }

          s <- header[[varname]]
          dlen <- length(s)
          imported_electrodes <- subject$electrodes[subject$preprocess_settings$data_imported]
          if (!length(imported_electrodes)) { return() }
          e <- imported_electrodes[[1]]
          efile <- file.path(subject$preprocess_path, "voltage", sprintf("electrode_%s.h5", e))
          if (
            !file.exists(efile) ||
            !ieegio::io_h5_valid(efile) ||
            !sprintf("raw/%s", block) %in% gsub("^/", "", ieegio::io_h5_names(efile))
          ) { return() }

          signal_len <- length(ieegio::io_read_h5(efile, sprintf("raw/%s", block), ram = FALSE))
          sample_rate <- dlen / signal_len * subject$raw_sample_rates[subject$electrodes == e]
        },
        "Imported" = {
          sample_rate <- epoch_header$header$sample_rate
        }
      )

      if (length(sample_rate) != 1 || is.na(sample_rate) || sample_rate <= 1) {
        return()
      }
      shiny::updateNumericInput(
        session = session,
        inputId = "sample_rate",
        value = sample_rate
      )

    }),
    local_reactives$epoch_header,
    input$varname,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      shiny::updateSelectInput(
        session = session,
        inputId = "varname",
        choices = character(0)
      )

    }),
    input$epoch_file,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # update epoch file
  shiny::bindEvent(
    ravedash::safe_observe({

      subject <- local_data$subject
      if (!length(subject)) { return() }

      block_path <- file.path(subject$preprocess_settings$raw_path, input$block)
      epoch_files <- list.files(
        block_path, pattern = ".(h5|mat|edf|pd|aud|nev)",
        full.names = FALSE, recursive = FALSE,
        include.dirs = FALSE, all.files = FALSE,
        ignore.case = TRUE, no.. = TRUE
      )
      if (!length(epoch_files)) {
        block_path <- subject$preprocess_settings$raw_path2
        epoch_files <- list.files(
          block_path, pattern = ".(h5|mat|edf|pd|aud|nev)",
          full.names = FALSE, recursive = TRUE,
          include.dirs = FALSE, all.files = FALSE,
          ignore.case = TRUE, no.. = TRUE
        )
      }
      # also include auxiliary channels
      aux_channels <- subject$electrodes[subject$electrode_types %in% "Auxiliary"]
      if (length(aux_channels)) {
        epoch_files <- c(sprintf("[Channel %d]", aux_channels), epoch_files)
        guess0 <- epoch_files[[1]]
      } else {
        guess0 <- epoch_files[grepl("\\.(pd|aud|nev)$", tolower(epoch_files))]
      }

      guess1 <- epoch_files[grepl("[^0-9]1(29|30).(mat|h5)$", tolower(epoch_files))]
      guess2 <- epoch_files[endsWith(tolower(epoch_files), ".edf")]
      epoch_file <- input$epoch_file %OF% c(guess0, guess1, guess2, epoch_files)


      shiny::updateSelectInput(
        session = session, inputId = "epoch_file",
        choices = epoch_files, selected = epoch_file
      )

      # The files offered, for `epoch_channel_info` (agents cannot read the
      # choices of a select)
      local_data$epoch_file_choices <- list(
        block = input$block, files = epoch_files, selected = epoch_file,
        guesses = unique(c(guess0, guess1, guess2))
      )

    }),
    local_reactives$refresh,
    input$block,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  server_tools$set_script(
    name = "load_epoch_signal",
    description = c(
      "Same as clicking 'Load epoch signal': loads variable `varname` of the",
      "file read by `load_epoch_file` (run that first whenever `block` or",
      "`epoch_file` changed) and plots it, using `sample_rate` for the time",
      "axis. Settle the sample rate with the user first (MATLAB files: always",
      "ask). New data: the Threshold field is cleared, so set it again for this",
      "signal. Output: samples and duration, compared with the iEEG recording",
      "of the block. Known problems (no file loaded, no variable, a variable",
      "that is not a numeric signal) come back as the script's error. Returns",
      "'samples=...', 'sample_rate=...', 'duration_s=...'."
    ),
    expr = {
      epoch_header <- local_reactives$epoch_header

      if (!is.list(epoch_header) || !isTRUE(epoch_header$type %in% c("MAT", "EDF", "NEV", "Imported"))) {
        stop_known("Please load epoch file first")
      }

      dipsaus::updateActionButtonStyled(
        session = session, inputId = "load_signal_btn",
        disabled = TRUE
      )
      on.exit({
        dipsaus::updateActionButtonStyled(
          session = session, inputId = "load_signal_btn",
          disabled = FALSE
        )
      })

      varname <- input$varname
      sample_rate <- input$sample_rate

      # TODO: validate inputs
      if (length(varname) != 1 || is.na(varname) || !nzchar(varname)) {
        stop_known("Please choose the variable (channel) that holds the epoch signal.")
      }

      type <- epoch_header$type
      header <- epoch_header$header

      switch(
        type,
        "EDF" = {
          shidashi::show_notification(
            title = "Loading...",
            message = "Loading epoch signal in progress... Please wait a second.",
            type = "default",
            autohide = FALSE, class = ns("loading_notif")
          )

          edf_cache <- ieegio::read_edf(
            con = header$source_path,
            extract_path = ravepipeline::dir_create2(header$cache_path),
            header_only = FALSE,
            cache_ok = TRUE
          )
          channel <- edf_cache$get_channel(varname)
          s <- channel$value
          shidashi::clear_notifications(class = ns("loading_notif"))
        },
        "NEV" = {
          # "Channel %d [%s]"
          varnames_choices <- sprintf(
            "Channel %d [%s]",
            header$channel_table$original_channel,
            header$channel_table$name
          )
          channel <- header$channel_table$original_channel[varnames_choices == varname][[1]]
          channel_data <- header$get_channel(channel)
          s <- channel_data$value
        },
        "MAT" = {
          s <- header[[varname]][drop = TRUE]
        },
        "Imported" = {
          s <- header$voltage
        }
      )
      # A struct or text variable (e.g. 'analogInfos') is not a signal; a
      # logical (TTL) vector is
      if (!(is.numeric(s) || is.logical(s)) || length(s) <= 1) {
        stop_known(sprintf(paste(
          "Variable '%s' is not a numeric signal. Please choose the variable",
          "that holds the epoch signal (e.g. 'analogTraces')."
        ), varname))
      }
      local_reactives$epoch_signal <- as.vector(s)

      # New data: the threshold field goes empty, so a threshold is set again
      # for this signal (a double-click on the plot or a typed number)
      local_reactives$threshold <- NULL
      local_reactives$threshold_timestamp <- NULL
      local_data$threshold_info <- NULL
      shiny::updateNumericInput(session = session, inputId = "threshold", value = "")
      shidashi::clear_notifications(class = ns("threshold_notif"))

      # What the signal came from (for the agent scripts and the sidecar of
      # `save_agent_epoch`), and a summary for agents
      local_data$signal_info <- c(local_data$header_info, list(
        variable = varname,
        sample_rate = sample_rate,
        samples = length(s),
        loaded_at = Sys.time()
      ))
      n_samples <- length(s)
      block <- local_data$signal_info$block
      valid_rate <- isTRUE(sample_rate > 1)
      cat(sprintf(
        "Loaded '%s' of '%s' (block %s): %d samples%s.\n",
        varname, local_data$signal_info$file, block, n_samples,
        if (valid_rate) {
          sprintf(" = %.3f s at %s Hz", n_samples / sample_rate, format(sample_rate))
        } else {
          " (`sample_rate` is not set: ask the user for it and set it)"
        }
      ))
      ieeg <- ieeg_block_info(local_data$subject, block)
      if (length(ieeg)) {
        mismatch <- valid_rate &&
          isTRUE(abs(n_samples / sample_rate - ieeg$duration) > 0.01 * ieeg$duration)
        cat(sprintf(
          "The iEEG recording of block %s lasts %.3f s (%d samples at %s Hz)%s.\n",
          block, ieeg$duration, ieeg$samples, format(ieeg$sample_rate),
          if (mismatch) {
            "; the durations differ: check the sample rate with the user"
          } else ""
        ))
      }
      cat(paste(
        "The Threshold field was cleared for this signal. Next: set",
        "`plot_absolute`/`plot_difference`, `threshold_direction` and",
        "`num_duration`, run `threshold_scan`, wait until `threshold` is empty,",
        "then set it.\n"
      ))
      c(
        sprintf("samples=%d", n_samples),
        sprintf("sample_rate=%s", format(sample_rate)),
        sprintf("duration_s=%s", if (valid_rate) sprintf("%.3f", n_samples / sample_rate) else "NA")
      )
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      server_tools$trigger_script("load_epoch_signal")
    }),
    input$load_signal_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # generate plot signals
  shiny::bindEvent(
    ravedash::safe_observe({
      s <- local_reactives$epoch_signal
      if (length(s)) {
        if (isTRUE(input$plot_difference)) {
          s <- c(0, diff(s))
        }
        if (isTRUE(input$plot_absolute)) {
          s <- abs(s)
        }
      }
      local_reactives$plot_signal <- s
    }),
    local_reactives$epoch_signal,
    input$plot_difference,
    input$plot_absolute,
    ignoreNULL = FALSE, ignoreInit = FALSE
  )


  # MIGRATED from ravedash::register_output
  # ravedash::register_output(render_function = ..., outputId = ..., output_opts = ...)
  shidashi::register_output(
    shiny::renderPlot({
      plot_signal <- local_reactives$plot_signal
      plot_range <- input$plot_range
      sample_rate <- input$sample_rate

      shiny::validate(
        shiny::need(
          length(plot_signal) > 0,
          message = "No epoch channel imported"
        ),
        shiny::need(
          isTRUE(sample_rate > 1),
          message = "Sample rate is invalid"
        )
      )

      q <- ceiling(length(plot_signal) / 20000)

      if (q > 1) {
        plot_signal <- ravetools::decimate(plot_signal, q)
        time <- seq(0, length.out = length(plot_signal), by = q / sample_rate)
      } else {
        time <- seq_along(plot_signal)
      }

      if (length(plot_range) != 1 || is.na(plot_range) || plot_range <= 0) {
        ylim <- range(plot_signal)
      } else {
        ylim <- c(-plot_range, plot_range)
      }
      if (isTRUE(input$plot_absolute)) {
        ylim[[1]] <- 0
      }

      opt <- graphics::par(c("mai", "mar", "cex.axis"))
      on.exit(
        {
          do.call(graphics::par, opt)
        },
        add = TRUE,
        after = FALSE
      )
      graphics::par(
        mai = c(0.52, 0.4, 0.1, 0.1),
        cex.axis = 0.8
      )
      plot(
        time,
        plot_signal,
        type = "l",
        ylim = ylim,
        main = "",
        ylab = "",
        xlab = "Time (s)"
      )
      addlines()
    }),
    outputId = "plot_overall",
    output_opts = list(
      click = NULL,
      dblclick = shiny::dblclickOpts(ns("plot_overall__dblclick"), clip = TRUE),
      brush = shiny::brushOpts(
        ns("plot_overall__brush"),
        resetOnNew = TRUE,
        clip = TRUE,
        direction = "x"
      )
    ),
    download_type = "image"
  )

  # output$plot_overall <- shiny::renderPlot({
  #   plot_signal <- local_reactives$plot_signal
  #   plot_range <- input$plot_range
  #   sample_rate <- input$sample_rate
  #
  #   shiny::validate(
  #     shiny::need(
  #       length(plot_signal) > 0,
  #       message = "No epoch channel imported"
  #     ),
  #     shiny::need(
  #       isTRUE(sample_rate > 1),
  #       message = "Sample rate is invalid"
  #     )
  #   )
  #
  #   q <- ceiling(length(plot_signal) / 20000)
  #
  #   if (q > 1) {
  #     plot_signal <- ravetools::decimate(plot_signal, q)
  #     time <- seq(0, length.out = length(plot_signal), by = q / sample_rate)
  #   } else {
  #     time <- seq_along(plot_signal)
  #   }
  #
  #   if (length(plot_range) != 1 || is.na(plot_range) || plot_range <= 0) {
  #     ylim <- range(plot_signal)
  #   } else {
  #     ylim <- c(-plot_range, plot_range)
  #   }
  #   if (isTRUE(input$plot_absolute)) {
  #     ylim[[1]] <- 0
  #   }
  #
  #   opt <- graphics::par(c("mai", "mar", "cex.axis"))
  #   on.exit({ do.call(graphics::par, opt) }, add = TRUE, after = FALSE)
  #   graphics::par(
  #     mai = c(0.52, 0.4, 0.1, 0.1),
  #     cex.axis = 0.8
  #   )
  #   plot(time, plot_signal, type = "l", ylim = ylim, main = "",
  #        ylab = "", xlab = "Time (s)")
  #   addlines()
  # })

  # brush
  brush_content <- shiny::bindEvent(
    shiny::reactive({
      data <- input$plot_overall__brush
      sample_rate <- input$sample_rate
      xmin <- data$xmin
      xmax <- data$xmax
      epoch_signal <- local_reactives$epoch_signal

      if (!length(epoch_signal) || !isTRUE(sample_rate > 1) ||
         length(xmin) != 1 || length(xmax) != 1) {
        return()
      }
      idx1 <- floor(xmin * sample_rate) + 1
      if (idx1 <= 0) { idx1 <- 1 }
      idx2 <- floor(xmax * sample_rate) + 1
      if (idx2 > length(epoch_signal)) {
        idx2 <- length(epoch_signal)
      }
      if (idx2 <= idx1) {
        return()
      }


      brush_signal <- epoch_signal[seq.int(idx1, idx2)]
      dlen <- length(brush_signal)
      q <- ceiling(dlen / 20000)
      if (q > 1) {
        brush_signal <- ravetools::decimate(brush_signal, q, ftype = "fir")
        time <- (seq_along(brush_signal)) * (q / sample_rate) + (idx1 - 2) / sample_rate
      } else {
        time <- seq.int(idx1 - 1, idx2 - 1) / sample_rate
      }

      return(list(
        data = brush_signal,
        time = time
      ))

    }),
    input$plot_overall__brush,
    input$sample_rate,
    ignoreNULL = FALSE, ignoreInit = FALSE
  )

  addlines <- function() {
    threshold_timestamp <- local_reactives$threshold_timestamp
    threshold <- local_reactives$threshold
    staged_table <- local_reactives$staged_table
    stage_selected <- input$table_threshold_staged_rows_selected
    if (length(threshold_timestamp)) {
      abline(v = threshold_timestamp, col = "red", lty = 2)
    }
    if (length(threshold)) {
      abline(h = threshold, col = "blue")
    }
    if (is.data.frame(staged_table)) {
      # sorted like the staged table, so the selected rows turn purple
      time <- sort(staged_table$time[staged_table$block %in% input$block])
      if (length(time)) {
        col <- rep("green", length(time))
        if (length(stage_selected)) {
          col[stage_selected] <- "purple"
        }
        abline(v = time, col = col)
      }
    }
  }

  # brush output
  # MIGRATED from ravedash::register_output
  # ravedash::register_output(
  shidashi::register_output(
    shiny::renderPlot({
      content <- brush_content()
      shiny::validate(
        shiny::need(
          is.list(content) && length(content) == 2,
          "Please subset the figure to my left"
        )
      )

      opt <- graphics::par(c("mai", "mar", "cex.axis"))
      on.exit({ do.call(graphics::par, opt) }, add = TRUE, after = FALSE)
      graphics::par(
        mai = c(0.52, 0.4, 0.1, 0.1),
        cex.axis = 0.8
      )
      plot(content$time, content$data, type = "l", main = "",
           ylab = "", xlab = "Time (s)")
      addlines()

    }),
    outputId = "plot_subset",
    download_type = "image"
  )

  # A double-click on the overview plot fills the "Threshold" field (4
  # significant digits). Double-clicking the same value again re-runs the
  # thresholding, as every double-click did before the field existed
  shiny::bindEvent(
    ravedash::safe_observe({
      y <- input$plot_overall__dblclick$y
      if (length(y) != 1 || !isTRUE(is.finite(y))) { return() }
      y <- signif(y, 4)
      if (identical(suppressWarnings(as.numeric(input$threshold)), y)) {
        local_reactives$re_threshold <- Sys.time()
      } else {
        shiny::updateNumericInput(session = session, inputId = "threshold", value = y)
      }
    }),
    input$plot_overall__dblclick,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # threshold signals: the "Threshold" field (typed, filled by a double-click,
  # or set by agents) drives the candidate onsets. `safe_reactive` keeps the
  # last result on errors: this reactive triggers an observer below
  dblclick_evt <- shiny::debounce(ravedash::safe_reactive({
    force(local_reactives$re_threshold)
    threshold <- input$threshold
    if (length(threshold) != 1 || !isTRUE(is.finite(threshold))) {
      # No threshold (e.g. new data): no candidates until someone
      # double-clicks the plot or enters a valid number
      local_reactives$threshold <- NULL
      local_reactives$threshold_timestamp <- NULL
      local_data$threshold_info <- NULL
      shidashi::clear_notifications(class = ns("threshold_notif"))
      return()
    }
    local_reactives$threshold <- threshold
    sample_rate <- input$sample_rate
    if (!isTRUE(sample_rate > 1)) {
      return()
    }
    num_duration <- input$num_duration
    if (!length(num_duration) || is.na(num_duration) || num_duration <= 0) {
      num_duration <- 0
    }
    if (isTRUE(input$threshold_direction == "Below")) {
      direction = "Below"
    } else {
      direction = "Above"
    }
    return(list(
      threshold = threshold,
      num_duration = num_duration,
      sample_rate = sample_rate,
      direction = direction
    ))
  }), 300)
  shiny::bindEvent(
    ravedash::safe_observe({
      data <- dblclick_evt()

      if (length(data) != 4) { return() }
      plot_signal <- local_reactives$plot_signal
      if (!length(plot_signal)) { return() }

      threshold <- data$threshold
      num_duration <- data$num_duration
      sample_rate <- data$sample_rate
      direction <- data$direction

      # generate threshold data (no onsets when nothing crosses the threshold)
      epoch_onset <- find_threshold_onsets(
        signal = plot_signal,
        threshold = threshold,
        direction = direction,
        num_duration = num_duration,
        sample_rate = sample_rate
      )
      if (isTRUE(direction == "Below")) {
        direction <- "below (s <= threshold)"
      } else {
        direction <- "above (s >= threshold)"
      }

      initial_len <- length(epoch_onset)

      staged_table <- local_reactives$staged_table
      if (isTRUE(num_duration > 0) && is.data.frame(staged_table)) {
        # check staged table, remove any if the onset fall into the min-duration
        # (of the block whose signal is loaded, which may differ from the
        # selected block)
        signal_block <- local_data$signal_info$block %||% input$block
        staged_time <- staged_table$time[staged_table$block %in% signal_block]
        if (length(staged_time)) {

          for (t in staged_time) {
            epoch_onset <- epoch_onset[abs(epoch_onset - t) > num_duration]
          }
        }
      }
      removed_len <- initial_len - length(epoch_onset)


      local_reactives$threshold_timestamp <- epoch_onset

      # What these candidates came from (for the agent scripts and the
      # staging log)
      local_data$threshold_info <- list(
        threshold = threshold,
        direction = data$direction,
        num_duration = num_duration,
        sample_rate = sample_rate,
        absolute = isTRUE(input$plot_absolute),
        difference = isTRUE(input$plot_difference),
        signal = local_data$signal_info,
        found = initial_len,
        removed = removed_len,
        computed_at = Sys.time()
      )

      shidashi::clear_notifications(class = ns("threshold_notif"))

      shidashi::show_notification(
        title = "Epoch threshold",
        message = sprintf(
          paste0(
            "A new epoch threshold is set at %.2f. ",
            "Threshold direction: %s; ",
            "minimum trial durations: %.2f (seconds). ",
            "Total number of potential trials found: %d%s"
          ),
          threshold, direction, num_duration, initial_len,
          ifelse(
            removed_len > 0, sprintf(paste0(
              ", %d of them are removed as they fall ",
              "into +- minimum trial duration (%.2f seconds) ",
              "of the staged stimuli onset. Therefore only %d trials are selected"
              ),
              removed_len, num_duration, initial_len - removed_len),
            "."
          )
        ),
        type = "info", close = TRUE,
        autohide = FALSE, class = ns("threshold_notif")
      )

    }),
    dblclick_evt(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$table_threshold_initial <- DT::renderDataTable({

    threshold_timestamp <- as.vector(local_reactives$threshold_timestamp)
    shiny::validate(
      shiny::need(
        length(threshold_timestamp) > 0,
        message = "No trial selected"
      )
    )

    table <- data.frame(
      Order = seq_along(threshold_timestamp),
      Time = threshold_timestamp,
      Diff = c(0, diff(threshold_timestamp))
    )

    re <- DT::datatable(table, selection = "multiple", rownames = FALSE,
                        class = "compact", filter = "none",
                        options = list(ordering = FALSE, bFilter = 0))
    re <- DT::formatRound(re, columns = c("Time", "Diff"), digits = 2)
    re

  })
  output$table_threshold_staged <- DT::renderDataTable({

    staged_table <- local_reactives$staged_table
    shiny::validate(
      shiny::need(
        length(staged_table) > 0 && is.data.frame(staged_table),
        message = "No epoch staged"
      )
    )
    time <- sort(staged_table$time[staged_table$block %in% input$block])

    shiny::validate(
      shiny::need(
        length(time) > 0,
        message = "No trial staged for current block"
      )
    )

    table <- data.frame(
      Order = seq_along(time),
      Time = time,
      Diff = c(0, diff(time))
    )

    re <- DT::datatable(table, selection = "multiple", rownames = FALSE,
                        class = "compact", filter = "none",
                        options = list(ordering = FALSE, bFilter = 0))
    re <- DT::formatRound(re, columns = c("Time", "Diff"), digits = 2)
    re

  })

  proxy1 <- DT::dataTableProxy(outputId = "table_threshold_initial")
  proxy2 <- DT::dataTableProxy(outputId = "table_threshold_staged")

  shiny::bindEvent(
    ravedash::safe_observe({
      s <- input$table_threshold_initial_rows_selected
      threshold_timestamp <- as.vector(local_reactives$threshold_timestamp)
      idx <- seq_along(threshold_timestamp)
      idx <- idx[!idx %in% s]
      DT::selectRows(proxy1, idx)
    }),
    input$toggle_selection,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      DT::selectRows(proxy1, integer(0L))
    }),
    input$clear_selection,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  server_tools$set_script(
    name = "stage_selected",
    description = c(
      "Same as clicking 'Stage selected': moves the candidate onsets selected",
      "in the candidate table (left) into the staged table, labelled with",
      "`block`. Agents stage all candidates: click `clear_selection`, wait",
      "until `epoch_status` shows 0 selected, click `toggle_selection`, wait",
      "until all are selected, then run this. Refused when the candidates come",
      "from another block's signal, or nothing is selected. AFTER STAGING,",
      "STOP: report to the user what was staged (file, threshold, direction,",
      "minimum duration, count, gaps) and let them check the plot before the",
      "next block or saving. Returns 'block=...', 'staged_now=N',",
      "'staged_in_block=N', 'staged_total=N'."
    ),
    expr = {
      s <- input$table_threshold_initial_rows_selected
      threshold_timestamp <- as.vector(local_reactives$threshold_timestamp)

      # The candidates belong to the block of the loaded signal: staging them
      # under another block would mislabel them
      signal_block <- local_data$signal_info$block
      if (length(threshold_timestamp) && length(signal_block) == 1 &&
          !identical(as.character(signal_block), as.character(input$block))) {
        stop_known(sprintf(paste(
          "The candidate onsets were found in block %s's signal, but block %s",
          "is selected. Select block %s again, or load block %s's file and",
          "signal, before staging."
        ), signal_block, input$block, signal_block, input$block))
      }
      if (!length(threshold_timestamp)) {
        stop_known(paste(
          "There are no candidate onsets to stage: none were found at this",
          "threshold, or all of them are already staged."
        ))
      }
      # The table keeps the selected row numbers after it empties: drop the
      # ones beyond the current candidates
      s <- s[s >= 1 & s <= length(threshold_timestamp)]
      if (!length(s)) {
        stop_known(paste(
          "No candidate onsets are selected. Select rows in the candidate",
          "table (left), or click 'Toggle selection' to select all of them."
        ))
      }

      new_items <- data.frame(
        block = input$block,
        time = threshold_timestamp[s]
      )
      staged_table <- local_reactives$staged_table
      local_reactives$staged_table <- rbind(
        staged_table, new_items
      )
      local_reactives$threshold_timestamp <- threshold_timestamp[-s]

      # Staging log (for `epoch_status` and the sidecar of `save_agent_epoch`)
      add_stage_log("staged", block = input$block,
                    onsets = threshold_timestamp[s],
                    threshold = local_data$threshold_info)
      counts <- staged_counts()
      n_block <- sum(local_reactives$staged_table$block %in% input$block)
      cat(sprintf(
        "Staged %d onset(s) in block %s (%d staged in this block). Staged onsets: %s.\n",
        length(s), input$block, n_block, format_staged_counts()
      ))
      cat(paste(
        "STOP: before the next block or saving, tell the user what was staged",
        "(file, threshold, direction, minimum duration, count, gaps) and ask",
        "them to check the plot: green lines are the staged onsets of the",
        "selected block. To remove onsets, they select rows in the staged table",
        "(right; the lines turn purple) and click 'Unstage selected'; 'Unstage",
        "all' clears the block. Continue only after they agree.\n"
      ))
      c(
        sprintf("block=%s", input$block),
        sprintf("staged_now=%d", length(s)),
        sprintf("staged_in_block=%d", n_block),
        sprintf("staged_total=%d", sum(counts))
      )
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      server_tools$trigger_script("stage_selected")
    }),
    input$save_changes,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  server_tools$set_script(
    name = "unstage_selected",
    description = c(
      "Same as clicking 'Unstage selected': removes the rows selected in the",
      "staged table (right) for `block`. Only people select those rows: ask",
      "the user to select them and click the button, or run this after they",
      "selected rows. Refused when nothing is staged or selected. Returns",
      "'block=...', 'removed=N', 'staged_total=N'."
    ),
    expr = {

      staged_table <- local_reactives$staged_table
      if (!is.data.frame(staged_table)) {
        stop_known("Nothing is staged.")
      }
      stage_selected <- input$table_threshold_staged_rows_selected
      if (!length(stage_selected)) {
        stop_known("No staged onsets are selected. Select rows in the staged table (right) first.")
      }

      idx <- which(staged_table$block %in% input$block)
      if (!length(idx)) {
        stop_known(sprintf("Nothing is staged in block %s.", input$block))
      }

      time <- staged_table$time[idx]
      idx <- idx[order(time)]

      stage_selected <- stage_selected[stage_selected >= 1 &
                                         stage_selected <= length(idx)]
      if (!length(stage_selected)) {
        stop_known("No staged onsets are selected. Select rows in the staged table (right) first.")
      }

      DT::selectRows(proxy2, integer(0L))

      removed <- staged_table$time[idx[stage_selected]]
      staged_table <- staged_table[-idx[stage_selected], ]
      local_reactives$staged_table <- staged_table
      local_reactives$re_threshold <- Sys.time()

      add_stage_log("unstaged", block = input$block, onsets = removed)
      cat(sprintf("Removed %d staged onset(s) from block %s. Staged onsets: %s.\n",
                  length(removed), input$block, format_staged_counts()))
      c(
        sprintf("block=%s", input$block),
        sprintf("removed=%d", length(removed)),
        sprintf("staged_total=%d", sum(staged_counts()))
      )
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      server_tools$trigger_script("unstage_selected")
    }),
    input$undo_changes,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_preview_table <- shiny::reactive({
    staged_table <- local_reactives$staged_table
    if (!is.data.frame(staged_table)) { return() }
    # Get subject's block numbers
    subject <- local_data$subject
    if (!length(subject)) { return() }

    blocks <- subject$preprocess_settings$blocks
    re <- lapply(blocks, function(block) {
      sel <- staged_table$block %in% block
      if (!any(sel)) { return(NULL) }

      sub <- staged_table[sel, ]
      time <- sort(sub$time)
      time <- as.numeric(sprintf("%.4f", time))
      data.frame(
        Block = block,
        Time = time,
        Trial = 0L,
        Condition = "NoCondition"
      )
    })
    re <- dipsaus::drop_nulls(re)
    if (!length(re)) { return(NULL) }
    re <- do.call("rbind", re)
    re$Trial <- seq_len(nrow(re))
    re
    return(re)
  })

  output$preview_table <- shiny::renderTable({
    tbl <- get_preview_table()
    shiny::validate(shiny::need(is.data.frame(tbl), "No epoch staged"))

    tbl
  }, striped = TRUE, spacing = "xs", width = "100%",
  rownames = FALSE)

  show_preview_modal <- function() {

    if (!is.data.frame(shiny::isolate(local_reactives$staged_table))) {
      error_notification(list(message = "There is no trial epoch staged. Please stage your changes before exporting."))
      return()
    }

    shiny::showModal(shiny::modalDialog(
      title = "Preview & export",
      size = "m",
      easyClose = FALSE,
      shiny::div(
        style = "max-height: 60vh",
        class = "overflow-y-scroll overflow-x-hidden",
        shiny::tableOutput(ns("preview_table"))
      ),
      footer = shidashi::flex_container(
        align_content = "flex-end",
        style = "margin:-0.5rem",
        shidashi::flex_item(
          flex = "2 1 100px",
          shiny::textInput(
            ns("epoch_export_name"),
            "Epoch name to export",
            value = "default",
            width = "100%"
          )
        ),
        shidashi::flex_item(
          flex = "1 50px",
          shiny::div(
            style = "margin-bottom: 0.8rem",
            shiny::tags$button(
              type = "button", class = "btn btn-default fill-width",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal", "Cancel"
            )
          )
        ),
        shidashi::flex_item(
          flex = "2 1 100px",
          shiny::div(
            style = "margin-bottom: 0.8rem",
            shiny::downloadButton(
              ns("download_btn3"),
              "Save & Download",
              class = "btn btn-primary fill-width",
              icon = NULL
            )
          )
        )
      )
    ))
  }

  shiny::bindEvent(
    ravedash::safe_observe({ show_preview_modal() }),
    input$download_btn1, ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({ show_preview_modal() }),
    input$download_btn2, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_epoch_filename <- shiny::reactive({
    fname <- input$epoch_export_name
    if (length(fname) != 1 || is.na(fname)) {
      fname <- "temp"
    } else {
      fname <- gsub("csv$", "", fname, ignore.case = TRUE)
      fname <- gsub("[^a-zA-Z0-9_]+", "_", fname)
      fname <- gsub("(^_)|(_$)", "", fname)
      fname <- gsub("^reference[_]{0,1}", "", fname, ignore.case = TRUE)
      fname <- gsub("^epoch[_]{0,1}", "", fname, ignore.case = TRUE)
      if (!nchar(fname)) {
        fname <- "temp"
      }
    }
    return(structure(sprintf("epoch_%s.csv", fname), epoch_name = fname))
  })

  output$download_btn3 <- shiny::downloadHandler(
    filename = function() {
      get_epoch_filename()
    },
    content = function(con) {
      tbl <- get_preview_table()
      if (is.data.frame(tbl) && nrow(tbl)) {
        utils::write.csv(tbl, file = con, row.names = FALSE)
        # also write to subject
        subject <- local_data$subject

        path <- file.path(subject$meta_path, get_epoch_filename())
        ravecore:::safe_write_csv(tbl, path, row.names = FALSE)
      }
    }
  )

  server_tools$set_script(
    name = "unstage_all",
    description = c(
      "Same as clicking 'Unstage all': removes the staged onsets of the",
      "selected `block` only (other blocks keep theirs). Ask the user first.",
      "Returns 'block=...', 'removed=N', 'staged_total=N'."
    ),
    expr = {
      staged_table <- local_reactives$staged_table
      if (!is.data.frame(staged_table)) {
        cat("Nothing is staged.\n")
        return(c(sprintf("block=%s", input$block), "removed=0", "staged_total=0"))
      }
      removed <- staged_table$time[staged_table$block %in% input$block]
      staged_table <- staged_table[!staged_table$block %in% input$block, ]
      if (!nrow(staged_table)) { staged_table <- NULL }
      local_reactives$staged_table <- staged_table
      local_reactives$re_threshold <- Sys.time()

      add_stage_log("unstaged all", block = input$block, onsets = removed)
      cat(sprintf("Removed %d staged onset(s) from block %s. Staged onsets: %s.\n",
                  length(removed), input$block, format_staged_counts()))
      c(
        sprintf("block=%s", input$block),
        sprintf("removed=%d", length(removed)),
        sprintf("staged_total=%d", sum(staged_counts()))
      )
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      server_tools$trigger_script("unstage_all")
    }),
    input$undo_all,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  server_tools$set_script(
    name = "discard_epoch",
    description = c(
      "Same as clicking 'Discard epoch': removes the staged onsets of EVERY",
      "block. Ask the user first. Returns 'removed=N', 'staged_total=0'."
    ),
    expr = {
      if (!length(local_reactives$staged_table)) {
        cat("Nothing is staged.\n")
        return(c("removed=0", "staged_total=0"))
      }
      n_removed <- NROW(local_reactives$staged_table)
      local_reactives$staged_table <- NULL
      local_reactives$re_threshold <- Sys.time()

      # (an emptied table has no rows to discard)
      if (n_removed > 0) {
        add_stage_log("discarded", removed = n_removed)
      }
      cat(sprintf("Discarded %d staged onset(s) of all blocks.\n", n_removed))
      c(sprintf("removed=%d", n_removed), "staged_total=0")
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      server_tools$trigger_script("discard_epoch")
    }),
    input$discard_epoch,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ---- Read-only scripts and the agents' save -----------------------------

  # "<variable> of <file> (block <b>)"
  describe_signal <- function(info) {
    if (!length(info)) { return("no signal") }
    sprintf("'%s' of '%s' (block %s)", info$variable %||% "?",
            info$file %||% "?", info$block %||% "?")
  }
  describe_transform <- function(absolute, difference) {
    parts <- c(if (isTRUE(difference)) "difference", if (isTRUE(absolute)) "absolute")
    if (!length(parts)) { return("raw") }
    paste(parts, collapse = " + ")
  }
  # One line per staging-log entry
  format_stage_log <- function(entries) {
    vapply(entries, function(e) {
      at <- format(e$at, "%H:%M:%S")
      switch(
        e$action,
        "staged" = {
          th <- e$threshold
          sprintf(
            "%s staged %d in block %s from %s, %s Hz, %s %s on the %s signal, minimum duration %s s",
            at, length(e$onsets), e$block, describe_signal(th$signal),
            format(th$sample_rate %||% "?"), th$direction %||% "?",
            format(th$threshold %||% "?"),
            describe_transform(th$absolute, th$difference),
            format(th$num_duration %||% "?")
          )
        },
        "loaded" = sprintf("%s loaded epoch '%s' (%s)", at, e$epoch,
                           paste(sprintf("%s: %s", names(e$trials), unlist(e$trials)),
                                 collapse = ", ")),
        "discarded" = sprintf("%s discarded %d staged onset(s) of all blocks", at,
                              as.integer(e$removed)),
        sprintf("%s %s %d in block %s", at, e$action, length(e$onsets), e$block)
      )
    }, "")
  }

  server_tools$set_script(
    name = "epoch_channel_info",
    description = c(
      "Read-only: writes nothing, run it without asking. Reports the trigger",
      "file and signal: the files offered for the selected block (the module's",
      "likely trigger files first; show them to a user who is unsure which",
      "file to use); the selected and the loaded block, file and variable;",
      "the sample rate in `sample_rate` and where it comes from (file header,",
      "or for MATLAB/HDF5 a duration estimate: channel samples / iEEG block",
      "duration); rate hints found in the file; the rule for settling the rate",
      "with the user; full-rate quantiles of the plotted (transformed) signal",
      "for choosing a threshold; and sample rates recorded by earlier",
      "agent-generated epochs of this project. Returns 'samples=...',",
      "'sample_rate=...', 'duration_s=...', 'estimate_hz=...'."
    ),
    expr = {
      subject <- local_data$subject
      header_info <- local_data$header_info
      signal_info <- local_data$signal_info
      epoch_header <- local_reactives$epoch_header
      sample_rate <- input$sample_rate
      lines <- sprintf(
        "Subject %s; blocks %s. Selected: block %s, file '%s', variable '%s', sample rate %s.",
        subject$subject_id, paste(subject$blocks, collapse = ", "),
        input$block %||% "", input$epoch_file %||% "", input$varname %||% "",
        format(sample_rate %||% NA)
      )
      offered <- local_data$epoch_file_choices
      if (length(offered$files)) {
        others <- setdiff(offered$files, offered$guesses)
        lines <- c(lines, sprintf(
          paste(
            "Files offered for block %s (%d): likely trigger files (the module's",
            "guesses): %s; other files: %s%s. The module selected '%s' when it",
            "listed them: a guess, not the user's choice."
          ),
          offered$block, length(offered$files),
          if (length(offered$guesses)) paste(offered$guesses, collapse = ", ") else "none",
          paste(utils::head(others, 20), collapse = ", "),
          if (length(others) > 20) sprintf(", and %d more", length(others) - 20) else "",
          offered$selected %||% ""
        ))
      }

      estimate <- NULL
      rate_rule <- NULL
      if (!length(header_info) || !is.list(epoch_header)) {
        lines <- c(lines, "No file loaded: run `load_epoch_file` first.")
      } else {
        hdr <- epoch_header$header
        variables <- switch(
          header_info$type,
          "MAT" = names(hdr),
          "EDF" = hdr$channel_table$Label,
          "NEV" = sprintf("Channel %d [%s]", hdr$channel_table$original_channel,
                          hdr$channel_table$name),
          "raw"
        )
        lines <- c(lines, sprintf(
          "Loaded file: '%s' (block %s, type %s, .%s) with %d variable(s): %s%s.",
          header_info$file, header_info$block, header_info$type,
          header_info$extension, length(variables),
          paste(utils::head(variables, 10), collapse = ", "),
          if (length(variables) > 10) ", ..." else ""
        ))
        if (!identical(header_info$file, input$epoch_file) ||
            !identical(as.character(header_info$block), as.character(input$block))) {
          lines <- c(lines, paste(
            "WARNING: the selected block or file is not the loaded one: run",
            "`load_epoch_file` before `load_epoch_signal`."
          ))
        }
        varname <- input$varname
        header_rate <- tryCatch({
          switch(
            header_info$type,
            "EDF" = hdr$channel_table$SampleRate[hdr$channel_table$Label %in% varname],
            "NEV" = hdr$channel_table$sampling_frequency[variables %in% varname],
            "Imported" = hdr$sample_rate,
            NULL
          )
        }, error = function(e) { NULL })
        if (length(header_rate) == 1) {
          lines <- c(lines, sprintf("Sample rate in the file header: %s Hz.", format(header_rate)))
        }
        if (identical(header_info$type, "MAT")) {
          n_var <- tryCatch({
            if (length(varname) == 1 && varname %in% names(hdr)) length(hdr[[varname]]) else NULL
          }, error = function(e) { NULL })
          ieeg <- ieeg_block_info(subject, header_info$block)
          if (length(n_var) && length(ieeg)) {
            estimate <- n_var / ieeg$samples * ieeg$sample_rate
            lines <- c(lines, sprintf(paste(
              "Duration check: '%s' has %d samples; the iEEG recording of block",
              "%s lasts %.3f s, so if both cover the same period the rate is",
              "%s Hz."
            ), varname, n_var, header_info$block, ieeg$duration,
            format(signif(estimate, 6))))
            if (isTRUE(sample_rate > 1) &&
                isTRUE(abs(sample_rate - estimate) > 0.01 * estimate)) {
              lines <- c(lines, sprintf(
                "WARNING: `sample_rate` (%s) differs from the duration estimate: ask the user.",
                format(sample_rate)
              ))
            }
          }
          hints <- find_rate_hints(hdr)
          if (length(hints)) {
            lines <- c(lines, sprintf(
              "Rate-like values in the file (a hint only): %s.",
              paste(hints, collapse = "; ")
            ))
          }
        }
        rate_rule <- sample_rate_rule(header_info$type, header_info$extension)
      }

      sidecars <- find_agent_epoch_sidecars(subject$project_name)
      if (length(sidecars)) {
        lines <- c(lines, "Earlier agent-generated epochs in this project (suggest, then confirm with the user):",
                   paste(" ", format_agent_epoch_sidecars(sidecars)))
      } else {
        lines <- c(lines, paste(
          "No earlier agent-generated epochs in this project: no record does",
          "not mean no protocol, so ask the user."
        ))
      }

      plot_signal <- local_reactives$plot_signal
      if (length(signal_info) && length(plot_signal)) {
        n <- signal_info$samples
        lines <- c(lines, sprintf(
          "Loaded signal: %s, %d samples%s.",
          describe_signal(signal_info), n,
          if (isTRUE(sample_rate > 1)) sprintf(" = %.3f s at %s Hz", n / sample_rate, format(sample_rate)) else ""
        ))
        q <- stats::quantile(plot_signal,
                             c(0, 0.001, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 0.999, 1),
                             na.rm = TRUE)
        lines <- c(lines, sprintf(
          "Plotted (%s) signal, full resolution, quantiles: %s.",
          describe_transform(input$plot_absolute, input$plot_difference),
          paste(sprintf("%s %s", names(q), format(signif(q, 4))), collapse = ", ")
        ))
      } else {
        lines <- c(lines, "No signal loaded: run `load_epoch_signal`.")
      }
      # the rule last: agents receive the end of long reports
      lines <- c(lines, rate_rule)

      cat(paste(lines, collapse = "\n"), "\n")
      n <- signal_info$samples
      c(
        sprintf("samples=%s", if (length(n)) n else "NA"),
        sprintf("sample_rate=%s", format(sample_rate %||% NA)),
        sprintf("duration_s=%s", if (length(n) && isTRUE(sample_rate > 1)) sprintf("%.3f", n / sample_rate) else "NA"),
        sprintf("estimate_hz=%s", if (length(estimate)) format(signif(estimate, 6)) else "NA")
      )
    }
  )

  server_tools$set_script(
    name = "threshold_scan",
    description = c(
      "Read-only: writes nothing, run it without asking. Counts the onsets a",
      "threshold would find at 13 levels between the median and the extreme",
      "of the plotted signal, using the current `plot_absolute`,",
      "`plot_difference`, `threshold_direction` and `num_duration`, with the",
      "median and minimum gap between onsets. A plateau (the same count over a",
      "range of levels) marks a robust threshold. Compare the counts with what",
      "the user expects; never pick a level only to match it. Returns",
      "'level <threshold>: <count>' strings."
    ),
    expr = {
      plot_signal <- local_reactives$plot_signal
      sample_rate <- input$sample_rate
      if (!length(plot_signal)) {
        cat("No signal loaded: run `load_epoch_signal` first.\n")
        return("No signal loaded")
      }
      if (!isTRUE(sample_rate > 1)) {
        cat("`sample_rate` is not set: ask the user and set it first.\n")
        return("No sample rate")
      }
      direction <- if (isTRUE(input$threshold_direction == "Below")) "Below" else "Above"
      num_duration <- input$num_duration
      if (!length(num_duration) || is.na(num_duration) || num_duration <= 0) {
        num_duration <- 0
      }
      tbl <- scan_thresholds(plot_signal, direction, num_duration, sample_rate)
      fmt_gap <- function(x) { ifelse(is.na(x), "-", sprintf("%.3f", x)) }
      lines <- c(
        sprintf(
          "Threshold scan on the %s signal of %s, direction %s, minimum duration %s s, %d samples at %s Hz:",
          describe_transform(input$plot_absolute, input$plot_difference),
          describe_signal(local_data$signal_info), direction, format(num_duration),
          length(plot_signal), format(sample_rate)
        ),
        sprintf("%12s %8s %12s %10s", "threshold", "onsets", "median gap", "min gap"),
        sprintf("%12s %8d %12s %10s", format(tbl$threshold, trim = TRUE, drop0trailing = TRUE), tbl$onsets,
                fmt_gap(tbl$median_gap), fmt_gap(tbl$min_gap)),
        paste(
          "When the threshold is set, candidates within the minimum duration of",
          "staged onsets of this block are also removed."
        ),
        paste(
          "Next: compare the counts with the number of trials the user expects",
          "(never choose a level only to match it), then set `threshold`."
        )
      )
      cat(paste(lines, collapse = "\n"), "\n")
      sprintf("level %s: %d", format(tbl$threshold, trim = TRUE, drop0trailing = TRUE), tbl$onsets)
    }
  )

  server_tools$set_script(
    name = "epoch_status",
    description = c(
      "Read-only: writes nothing, run it without asking. Reports the threshold",
      "behind the candidate onsets (direction, minimum duration, transform),",
      "warnings when they are pending or stale (Threshold field changed, new",
      "signal, transform changed, block or file changed, stale table",
      "selection), the candidates (count, gaps, times) and how many rows are",
      "selected, the staged onsets per block, the last staging steps, and what",
      "to do next. Poll it after setting `threshold` and while staging.",
      "Returns 'candidates=N', 'selected=N', 'staged=N', 'threshold=...'."
    ),
    expr = {
      info <- local_data$threshold_info
      signal_info <- local_data$signal_info
      candidates <- as.vector(local_reactives$threshold_timestamp)
      selected_raw <- input$table_threshold_initial_rows_selected
      selected <- selected_raw[selected_raw >= 1 & selected_raw <= length(candidates)]
      counts <- staged_counts()
      block <- input$block
      lines <- sprintf("Selected block %s. Loaded signal: %s.", block %||% "",
                       describe_signal(signal_info))

      if (length(info)) {
        lines <- c(lines, sprintf(
          "Threshold %s (%s) on the %s signal, minimum duration %s s, %s Hz; computed %s.",
          format(info$threshold), info$direction,
          describe_transform(info$absolute, info$difference),
          format(info$num_duration), format(info$sample_rate),
          format(info$computed_at, "%H:%M:%S")
        ))
      } else {
        lines <- c(lines, "No threshold computed for the loaded signal.")
      }

      warnings <- character(0)
      field <- input$threshold
      if (length(field) == 1 && isTRUE(is.finite(field)) &&
          !isTRUE(all.equal(as.numeric(field), as.numeric(info$threshold %||% NA)))) {
        warnings <- c(warnings, if (!isTRUE(input$sample_rate > 1)) {
          sprintf(paste(
            "PENDING: the Threshold field shows %s, but nothing is computed",
            "while `sample_rate` is not set: ask the user for it and set it."
          ), format(field))
        } else if (!length(signal_info)) {
          sprintf("PENDING: the Threshold field shows %s, but no signal is loaded.",
                  format(field))
        } else {
          sprintf(paste(
            "PENDING: the Threshold field shows %s but no candidates were",
            "computed for it yet: run `epoch_status` again; if it stays",
            "pending, send \"\" to `threshold`, wait until it is empty, then",
            "send the number again."
          ), format(field))
        })
      }
      if (length(info) && length(signal_info) &&
          !identical(info$signal$loaded_at, signal_info$loaded_at)) {
        warnings <- c(warnings, "STALE: the candidates come from a previous signal: set `threshold` again.")
      }
      if (length(info) &&
          (!identical(info$absolute, isTRUE(input$plot_absolute)) ||
           !identical(info$difference, isTRUE(input$plot_difference)))) {
        warnings <- c(warnings, paste(
          "STALE: 'Absolute plot' or 'Difference plot' changed after the",
          "threshold was computed: set `threshold` again (send \"\" first, wait",
          "until it is empty, then the number)."
        ))
      }
      if (length(signal_info) &&
          !identical(as.character(signal_info$block), as.character(block))) {
        warnings <- c(warnings, sprintf(paste(
          "The selected block (%s) is not the loaded signal's block (%s):",
          "staging is refused until they match."
        ), block, signal_info$block))
      }
      header_info <- local_data$header_info
      if (length(header_info) && !identical(header_info$file, input$epoch_file)) {
        warnings <- c(warnings, sprintf(paste(
          "The selected file ('%s') is not the loaded file ('%s'): run",
          "`load_epoch_file` before `load_epoch_signal`."
        ), input$epoch_file %||% "", header_info$file))
      }
      if (length(selected_raw) > length(selected)) {
        warnings <- c(warnings, sprintf(paste(
          "Stale table selection: %d selected row number(s) are beyond the %d",
          "candidates and are ignored: click `clear_selection`."
        ), length(selected_raw) - length(selected), length(candidates)))
      }

      lines <- c(lines, sprintf(
        "Candidate onsets: %d (%d removed near staged onsets); %d selected in the table.",
        length(candidates), as.integer(info$removed %||% 0L), length(selected)
      ))
      if (length(candidates)) {
        lines <- c(lines, summarize_onset_gaps(candidates))
        # keep the report under the 3000 characters agents receive
        if (length(candidates) <= 50) {
          lines <- c(lines, paste0("Candidates (s): ",
                                   paste(sprintf("%.3f", candidates), collapse = ", ")))
        } else {
          lines <- c(lines, paste0(
            "Candidates (s), first and last 15 (all of them: `candidate_times`): ",
            paste(sprintf("%.3f", utils::head(candidates, 15)), collapse = ", "),
            " ... ",
            paste(sprintf("%.3f", utils::tail(candidates, 15)), collapse = ", ")
          ))
        }
      }
      lines <- c(lines, sprintf("Staged onsets: %s.", format_staged_counts()))
      log <- local_data$stage_log
      if (length(log)) {
        lines <- c(lines, "Last staging steps:",
                   paste(" ", format_stage_log(utils::tail(log, 3))))
      }

      n_block <- if (length(block) == 1 && block %in% names(counts)) counts[[block]] else 0L
      next_step <- if (!length(signal_info)) {
        paste(
          "Next: with the user's answers, set `block` and `epoch_file`, run",
          "`load_epoch_file`, settle the sample rate, run `load_epoch_signal`."
        )
      } else if (!length(info)) {
        paste(
          "Next: set the transforms, `threshold_direction` and `num_duration`,",
          "check `threshold_scan`, wait until `threshold` is empty, then set it."
        )
      } else if (length(candidates)) {
        paste(
          "Next: compare the count with the number of trials the user expects",
          "in this block and check the gaps. If they fit, stage all candidates",
          "(`clear_selection`, wait for 0 selected, `toggle_selection`, wait",
          "until all are selected, `stage_selected`). If not, tell the user",
          "and ask (reversed polarity? threshold too low? other events? trials",
          "this channel cannot mark?): never move the threshold only to match",
          "the count."
        )
      } else if (n_block > 0) {
        paste(
          "Next: STOP and let the user check the staged onsets of this block",
          "before the next block or `save_agent_epoch`."
        )
      } else {
        "Next: no candidates: adjust the threshold or direction with the user."
      }
      lines <- c(lines, warnings, next_step)

      cat(paste(lines, collapse = "\n"), "\n")
      c(
        sprintf("candidates=%d", length(candidates)),
        sprintf("selected=%d", length(selected)),
        sprintf("staged=%d", sum(counts)),
        sprintf("threshold=%s", if (length(info)) format(info$threshold) else "none")
      )
    }
  )

  server_tools$set_script(
    name = "candidate_times",
    description = c(
      "Read-only: writes nothing, run it without asking. Returns the candidate",
      "onset times in seconds (4 decimals) when there are at most 100;",
      "otherwise, or when there are none, a short message."
    ),
    expr = {
      candidates <- as.vector(local_reactives$threshold_timestamp)
      if (!length(candidates)) {
        cat("No candidate onsets.\n")
        return("candidates=0")
      }
      if (length(candidates) > 100) {
        msg <- sprintf("candidates=%d (more than 100: not listed)", length(candidates))
        cat(msg, "\n")
        return(msg)
      }
      round(candidates, 4)
    }
  )

  # One sidecar entry per staging-log entry of a block
  sidecar_action <- function(e, block, subject) {
    at <- format(e$at, "%Y-%m-%d %H:%M:%S %Z")
    if (identical(e$action, "staged")) {
      th <- e$threshold
      sig <- th$signal
      rel_file <- sig$file
      if (length(sig$raw_root) == 1 && length(sig$path) == 1) {
        root <- normalizePath(sig$raw_root, winslash = "/", mustWork = FALSE)
        path <- normalizePath(sig$path, winslash = "/", mustWork = FALSE)
        if (startsWith(path, paste0(root, "/"))) {
          rel_file <- substring(path, nchar(root) + 2L)
        }
      }
      estimate <- NULL
      if (identical(sig$type, "MAT") && length(sig$samples)) {
        ieeg <- ieeg_block_info(subject, sig$block)
        if (length(ieeg)) { estimate <- signif(sig$samples / ieeg$samples * ieeg$sample_rate, 6) }
      }
      return(dipsaus::drop_nulls(list(
        action = "staged",
        file = rel_file,
        raw_folder = sig$raw_root,
        variable = sig$variable,
        file_type = sig$type,
        sample_rate = th$sample_rate,
        duration_estimate_hz = estimate,
        channel_samples = sig$samples,
        channel_duration_seconds = if (isTRUE(th$sample_rate > 1)) sig$samples / th$sample_rate,
        transform = list(absolute = isTRUE(th$absolute), difference = isTRUE(th$difference)),
        threshold = th$threshold,
        threshold_direction = th$direction,
        minimum_duration_seconds = th$num_duration,
        candidates_found = th$found,
        removed_near_staged = th$removed,
        staged = length(e$onsets),
        onsets = round(e$onsets, 4),
        at = at
      )))
    }
    if (identical(e$action, "loaded")) {
      return(list(action = "loaded", source_epoch = e$epoch,
                  trials = as.integer(e$trials[[block]] %||% 0L), at = at))
    }
    if (identical(e$action, "discarded")) {
      return(list(action = "discarded", removed = as.integer(e$removed), at = at))
    }
    list(action = e$action, removed = length(e$onsets),
         onsets = round(e$onsets, 4), at = at)
  }

  server_tools$set_script(
    name = "save_agent_epoch",
    description = c(
      "Saves the staged onsets as a new epoch named",
      "'agent_generated_<yymmddTHHMMSS>' in the subject's meta folder:",
      "epoch_agent_generated_<time>.csv (the table people's 'Save & Download'",
      "writes: Block, Time, Trial, Condition 'NoCondition'; nothing is",
      "downloaded) and epoch_agent_generated_<time>_description.txt (YAML:",
      "per block the files, variables, sample rates, block durations,",
      "thresholds, transforms and the onsets each step staged or removed).",
      "ALWAYS show the user the staged count per block and the name first,",
      "and run it only after they agree. Refused when nothing is staged.",
      "Returns 'epoch=...', 'trials=N', 'csv=...', 'description=...'."
    ),
    expr = {
      subject <- local_data$subject
      tbl <- get_preview_table()
      if (!is.data.frame(tbl) || !nrow(tbl)) {
        stop_known("There is no trial epoch staged. Please stage onsets before saving.")
      }
      now <- Sys.time()
      epoch_name <- sprintf("agent_generated_%s", format(now, "%y%m%dT%H%M%S"))
      csv_path <- file.path(subject$meta_path, sprintf("epoch_%s.csv", epoch_name))
      sidecar_path <- file.path(subject$meta_path,
                                sprintf("epoch_%s_description.txt", epoch_name))

      # The same table that people's "Save & Download" writes
      csv_path <- ravecore:::safe_write_csv(tbl, csv_path, row.names = FALSE)

      log <- local_data$stage_log
      blocks <- unique(as.character(tbl$Block))
      info <- list(
        epoch_name = epoch_name,
        epoch_file = basename(csv_path),
        subject = subject$subject_id,
        created = format(now, "%Y-%m-%d %H:%M:%S %Z"),
        generated_by = "AI agent over MCP: RAVE module epoch_generator, script save_agent_epoch",
        total_trials = nrow(tbl),
        blocks = structure(lapply(blocks, function(block) {
          ieeg <- ieeg_block_info(subject, block)
          entries <- Filter(function(e) {
            e$action %in% c("discarded", "loaded") ||
              identical(as.character(e$block), block)
          }, log)
          entries <- Filter(function(e) {
            !identical(e$action, "loaded") || !is.null(e$trials[[block]])
          }, entries)
          dipsaus::drop_nulls(list(
            trials = sum(tbl$Block == block),
            ieeg_block_duration_seconds = ieeg$duration,
            actions = lapply(entries, sidecar_action, block = block, subject = subject)
          ))
        }), names = blocks)
      )
      write_epoch_sidecar(sidecar_path, info, comments = c(
        sprintf("Epoch %s was generated by an AI agent with the RAVE Epoch Generator module.", epoch_name),
        "Columns of the CSV: Block, Time (onset, seconds from block start), Trial, Condition (NoCondition).",
        "Each block lists its steps in order: onsets staged (with the file, variable, sample rate,",
        "transform and threshold that found them), onsets removed, epochs loaded, discards.",
        "File paths are relative to raw_folder."
      ))

      # List the new epoch under "Load existing epoch"
      shiny::updateSelectInput(
        session = session,
        inputId = "load_epoch",
        choices = c("New epoch...", subject$epoch_names),
        selected = "New epoch..."
      )

      per_block <- table(tbl$Block)
      cat(sprintf(
        "Saved epoch '%s' with %d trials (%s):\n  %s\n  %s\n",
        epoch_name, nrow(tbl),
        paste(sprintf("%s: %d", names(per_block), as.integer(per_block)), collapse = ", "),
        csv_path, sidecar_path
      ))
      c(
        sprintf("epoch=%s", epoch_name),
        sprintf("trials=%d", nrow(tbl)),
        sprintf("csv=%s", basename(csv_path)),
        sprintf("description=%s", basename(sidecar_path))
      )
    }
  )

}
