
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    refresh = NULL,
    staged_table = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)
  server_tools$auto_recalculate(FALSE)

  error_notification <- function(e) {
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


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

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

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      load_epoch <- input$load_epoch
      if(!length(load_epoch) || load_epoch == "New epoch...") {
        return()
      }
      shiny::invalidateLater(300)
      shiny::updateSelectInput(
        session = session,
        inputId = "load_epoch",
        selected = "New epoch..."
      )
      subject <- local_data$subject
      if(!length(subject)){ return() }

      tryCatch({
        epoch <- subject$meta_data(meta_type = "epoch", meta_name = load_epoch)
        local_reactives$staged_table <- data.frame(
          block = epoch$Block,
          time = epoch$Time
        )
      }, error = function(e){
        ravedash::logger_error_condition(e)
        error_notification(e)
      })

    }),
    input$load_epoch,
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      subject <- local_data$subject
      if(is.null(subject)) {
        error_notification(list(message = "Invalid subject (subject not loaded?)"))
      }

      epoch_file <- input$epoch_file
      block <- input$block
      if(length(epoch_file) != 1) {
        error_notification(list(message = "Invalid epoch file. Please choose a proper epoch file."))
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

      if(startsWith(epoch_file, "[Channel")) {
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
        epoch_channel_file <- file.path(subject$preprocess_settings$raw_path, input$block, epoch_file)
        if(length(epoch_channel_file) != 1 || !file.exists(epoch_channel_file)) {
          epoch_channel_file <- file.path(subject$preprocess_settings$raw_path2, epoch_file)
        }
        if(length(epoch_channel_file) != 1 || !file.exists(epoch_channel_file)) {
          error_notification(list(message = "Epoch channel file is invalid"))
        }

        # read signal
        if(endsWith(tolower(epoch_channel_file), "edf")) {

          temporary_path <- file.path(subject$cache_path, 'edf')
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
        } else if(endsWith(tolower(epoch_channel_file), "nev")) {

          temporary_path <- ravepipeline::dir_create2(file.path(subject$cache_path, 'neuroevent'))
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

    }),
    input$load_epoch_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      if(length(input$varname) != 1) { return() }
      epoch_header <- local_reactives$epoch_header
      if(!is.list(epoch_header) ||
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
          if(any(sel)) {
            sample_rate <- header$channel_table$sampling_frequency[sel]
          } else {
            sample_rate <- 2000
          }
        },
        "MAT" = {
          sample_rate <- input$sample_rate
          if(isTRUE(sample_rate > 1)) { return() }

          varname <- input$varname
          if(length(varname) != 1 || !varname %in% names(header)) { return() }

          s <- header[[varname]]
          dlen <- length(s)
          imported_electrodes <- subject$electrodes[subject$preprocess_settings$data_imported]
          if(!length(imported_electrodes)) { return() }
          e <- imported_electrodes[[1]]
          efile <- file.path(subject$preprocess_path, "voltage", sprintf("electrode_%s.h5", e))
          if(
            !file.exists(efile) ||
            !ieegio::io_h5_valid(efile) ||
            !sprintf("raw/%s", block) %in% gsub("^/", "", ieegio::io_h5_names(efile))
          ){ return() }

          signal_len <- length(ieegio::io_read_h5(efile, sprintf("raw/%s", block), ram = FALSE))
          sample_rate <- dlen /signal_len * subject$raw_sample_rates[subject$electrodes == e]
        },
        "Imported" = {
          sample_rate <- epoch_header$header$sample_rate
        }
      )

      if(length(sample_rate) != 1 || is.na(sample_rate) || sample_rate <= 1){
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
      if(!length(subject)){ return() }

      block_path <- file.path(subject$preprocess_settings$raw_path, input$block)
      epoch_files <- list.files(
        block_path, pattern = ".(h5|mat|edf|pd|aud|nev)",
        full.names = FALSE, recursive = FALSE,
        include.dirs = FALSE, all.files = FALSE,
        ignore.case = TRUE, no.. = TRUE
      )
      if(!length(epoch_files)) {
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
      if(length(aux_channels)) {
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

    }),
    local_reactives$refresh,
    input$block,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      epoch_header <- local_reactives$epoch_header

      if(!is.list(epoch_header) || !isTRUE(epoch_header$type %in% c("MAT", "EDF", "NEV", "Imported"))) {
        error_notification(list(message = "Please load epoch file first"))
        return()
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
      local_reactives$epoch_signal <- as.vector(s)
    }),
    input$load_signal_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # generate plot signals
  shiny::bindEvent(
    ravedash::safe_observe({
      s <- local_reactives$epoch_signal
      if(length(s)) {
        if(isTRUE(input$plot_difference)) {
          s <- c(0, diff(s))
        }
        if(isTRUE(input$plot_absolute)) {
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


  ravedash::register_output(
    render_function = shiny::renderPlot({
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

      if(q > 1) {
        plot_signal <- ravetools::decimate(plot_signal, q)
        time <- seq(0, length.out = length(plot_signal), by = q / sample_rate)
      } else {
        time <- seq_along(plot_signal)
      }

      if(length(plot_range) != 1 || is.na(plot_range) || plot_range <= 0) {
        ylim <- range(plot_signal)
      } else {
        ylim <- c(-plot_range, plot_range)
      }
      if(isTRUE(input$plot_absolute)) {
        ylim[[1]] <- 0
      }

      opt <- graphics::par(c("mai", "mar", "cex.axis"))
      on.exit({ do.call(graphics::par, opt) }, add = TRUE, after = FALSE)
      graphics::par(
        mai = c(0.52, 0.4, 0.1, 0.1),
        cex.axis = 0.8
      )
      plot(time, plot_signal, type = 'l', ylim = ylim, main = "",
           ylab = "", xlab = "Time (s)")
      addlines()
    }),
    outputId = "plot_overall",
    output_opts = list(
      click = NULL,
      dblclick = shiny::dblclickOpts(ns("plot_overall__dblclick"),
                                     clip = TRUE),
      brush = shiny::brushOpts(ns("plot_overall__brush"), resetOnNew = TRUE,
                               clip = TRUE, direction = "x")
    )
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
  #   if(q > 1) {
  #     plot_signal <- ravetools::decimate(plot_signal, q)
  #     time <- seq(0, length.out = length(plot_signal), by = q / sample_rate)
  #   } else {
  #     time <- seq_along(plot_signal)
  #   }
  #
  #   if(length(plot_range) != 1 || is.na(plot_range) || plot_range <= 0) {
  #     ylim <- range(plot_signal)
  #   } else {
  #     ylim <- c(-plot_range, plot_range)
  #   }
  #   if(isTRUE(input$plot_absolute)) {
  #     ylim[[1]] <- 0
  #   }
  #
  #   opt <- graphics::par(c("mai", "mar", "cex.axis"))
  #   on.exit({ do.call(graphics::par, opt) }, add = TRUE, after = FALSE)
  #   graphics::par(
  #     mai = c(0.52, 0.4, 0.1, 0.1),
  #     cex.axis = 0.8
  #   )
  #   plot(time, plot_signal, type = 'l', ylim = ylim, main = "",
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

      if(!length(epoch_signal) || !isTRUE(sample_rate > 1) ||
         length(xmin) != 1 || length(xmax) != 1) {
        return()
      }
      idx1 <- floor(xmin * sample_rate) + 1
      if(idx1 <= 0) { idx1 <- 1 }
      idx2 <- floor(xmax * sample_rate) + 1
      if(idx2 > length(epoch_signal)) {
        idx2 <- length(epoch_signal)
      }
      if(idx2 <= idx1){
        return()
      }


      brush_signal <- epoch_signal[seq.int(idx1, idx2)]
      dlen <- length(brush_signal)
      q <- ceiling(dlen / 20000)
      if(q > 1) {
        brush_signal <- ravetools::decimate(brush_signal, q, ftype = "fir")
        time <- (seq_along(brush_signal)) * (q / sample_rate) + (idx1 - 2) / sample_rate
      } else {
        time <- seq.int(idx1-1, idx2-1) / sample_rate
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

  addlines <- function(){
    threshold_timestamp <- local_reactives$threshold_timestamp
    threshold <- local_reactives$threshold
    staged_table <- local_reactives$staged_table
    stage_selected <- input$table_threshold_staged_rows_selected
    if(length(threshold_timestamp)) {
      abline(v = threshold_timestamp, col = "red", lty = 2)
    }
    if(length(threshold)){
      abline(h = threshold, col = "blue")
    }
    if(is.data.frame(staged_table)){
      time <- staged_table$time[staged_table$block %in% input$block]
      if(length(time)) {
        col <- rep("green", length(time))
        if(length(stage_selected)) {
          col[stage_selected] <- "purple"
        }
        abline(v = time, col = col)
      }
    }
  }

  # brush output
  ravedash::register_output(
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
      plot(content$time, content$data, type = 'l', main = "",
           ylab = "", xlab = "Time (s)")
      addlines()

    }),
    outputId = "plot_subset"
  )

  # threshold signals
  dblclick_evt <- shiny::debounce(shiny::reactive({
    force(local_reactives$re_threshold)
    data <- input$plot_overall__dblclick
    threshold <- data$y
    if(!length(threshold)) {
      threshold <- shiny::isolate(local_reactives$threshold)
    } else {
      local_reactives$threshold <- threshold
    }
    if(!length(threshold)){
      local_reactives$threshold_timestamp <- NULL
      return()
    }
    sample_rate <- input$sample_rate
    if(!isTRUE(sample_rate > 1)) {
      return()
    }
    num_duration <- input$num_duration
    if(!length(num_duration) || is.na(num_duration) || num_duration <= 0) {
      num_duration <- 0
    }
    if(isTRUE(input$threshold_direction == "Below")) {
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

      if(length(data) != 4) { return() }
      plot_signal <- local_reactives$plot_signal
      if(!length(plot_signal)) { return() }

      threshold <- data$threshold
      num_duration <- data$num_duration
      sample_rate <- data$sample_rate
      direction <- data$direction

      # generate threshold data
      if(isTRUE(direction == "Below")) {
        direction <- "below (s <= threshold)"
        breaks <- which(plot_signal <= threshold)
      } else {
        direction <- "above (s >= threshold)"
        breaks <- which(plot_signal >= threshold)
      }
      max_lag <- max(num_duration * sample_rate, 1)
      breaks <- dipsaus::deparse_svec(breaks - 1, concatenate = FALSE, max_lag = max_lag)
      breaks <- gsub("-[0-9]*", "", breaks)

      epoch_onset <- as.numeric(breaks) / sample_rate

      initial_len <- length(epoch_onset)

      staged_table <- local_reactives$staged_table
      if(isTRUE(num_duration > 0) && is.data.frame(staged_table)) {
        # check staged table, remove any if the onset fall into the min-duration
        staged_time <- staged_table$time[staged_table$block %in% input$block]
        if(length(staged_time)) {

          for(t in staged_time) {
            epoch_onset <- epoch_onset[abs(epoch_onset - t) > num_duration]
          }
        }
      }
      removed_len <- initial_len - length(epoch_onset)


      local_reactives$threshold_timestamp <- epoch_onset

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
              removed_len, num_duration, initial_len-removed_len),
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
    re <- DT::formatRound(re, columns=c('Time', 'Diff'), digits = 2)
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
    re <- DT::formatRound(re, columns=c('Time', 'Diff'), digits = 2)
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

  shiny::bindEvent(
    ravedash::safe_observe({
      s <- input$table_threshold_initial_rows_selected
      threshold_timestamp <- as.vector(local_reactives$threshold_timestamp)
      if(!length(s) || !length(threshold_timestamp)){ return() }

      new_items <- data.frame(
        block = input$block,
        time = threshold_timestamp[s]
      )
      staged_table <- local_reactives$staged_table
      local_reactives$staged_table <- rbind(
        staged_table, new_items
      )
      local_reactives$threshold_timestamp <- threshold_timestamp[-s]
    }),
    input$save_changes,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({

      staged_table <- local_reactives$staged_table
      if(!is.data.frame(staged_table)) { return() }
      stage_selected <- input$table_threshold_staged_rows_selected
      if(!length(stage_selected)) { return() }

      idx <- which(staged_table$block %in% input$block)
      if(!length(idx)) { return() }

      time <- staged_table$time[idx]
      idx <- idx[order(time)]

      stage_selected <- stage_selected[stage_selected >=1 &
                                         stage_selected <= length(idx)]
      if(!length(stage_selected)) { return() }

      DT::selectRows(proxy2, integer(0L))

      staged_table <- staged_table[-idx[stage_selected], ]
      local_reactives$staged_table <- staged_table
      local_reactives$re_threshold <- Sys.time()

    }),
    input$undo_changes,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_preview_table <- shiny::reactive({
    staged_table <- local_reactives$staged_table
    if(!is.data.frame(staged_table)){ return() }
    # Get subject's block numbers
    subject <- local_data$subject
    if(!length(subject)) { return() }

    blocks <- subject$preprocess_settings$blocks
    re <- lapply(blocks, function(block) {
      sel <- staged_table$block %in% block
      if(!any(sel)) { return(NULL) }

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
    if(!length(re)) { return(NULL) }
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

  show_preview_modal <- function(){

    if(!is.data.frame(shiny::isolate(local_reactives$staged_table))) {
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
    shiny::observe({ show_preview_modal() }),
    input$download_btn1, ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    shiny::observe({ show_preview_modal() }),
    input$download_btn2, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_epoch_filename <- shiny::reactive({
    fname <- input$epoch_export_name
    if(length(fname) != 1 || is.na(fname)) {
      fname <- "temp"
    } else {
      fname <- gsub("csv$", "", fname, ignore.case = TRUE)
      fname <- gsub("[^a-zA-Z0-9_]+", "_", fname)
      fname <- gsub("(^_)|(_$)", "", fname)
      fname <- gsub("^reference[_]{0,1}", "", fname, ignore.case = TRUE)
      fname <- gsub("^epoch[_]{0,1}", "", fname, ignore.case = TRUE)
      if(!nchar(fname)) {
        fname <- "temp"
      }
    }
    return(structure(sprintf("epoch_%s.csv", fname), epoch_name = fname))
  })

  output$download_btn3 <- shiny::downloadHandler(
    filename = function(){
      get_epoch_filename()
    },
    content = function(con) {
      tbl <- get_preview_table()
      if(is.data.frame(tbl) && nrow(tbl)) {
        utils::write.csv(tbl, file = con, row.names = FALSE)
        # also write to subject
        subject <- local_data$subject

        path <- file.path(subject$meta_path, get_epoch_filename())
        ravecore:::safe_write_csv(tbl, path, row.names = FALSE)
      }
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      staged_table <- local_reactives$staged_table
      if(!is.data.frame(staged_table)){ return() }
      staged_table <- staged_table[!staged_table$block %in% input$block, ]
      if(!nrow(staged_table)){ staged_table <- NULL }
      local_reactives$staged_table <- staged_table
      local_reactives$re_threshold <- Sys.time()
    }),
    input$undo_all,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      if(!length(local_reactives$staged_table)) { return() }
      local_reactives$staged_table <- NULL
      local_reactives$re_threshold <- Sys.time()
    }),
    input$discard_epoch,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
