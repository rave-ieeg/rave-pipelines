
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  local_data$pulses <- list()
  stream_plot_container <- ravecore:::StreamSignalPlot$new(
    n_channels = 1, sample_rates = 30000, start_time = 0
  )
  stream_proxy <- plotly::plotlyProxy(outputId = "stream_plot", session = session)

  update_plot <- function(
    start_time,
    duration,
    channel_gap,
    quality = c("performance", "balanced", "high-quality"),
    init = FALSE,
    stream_proxy
  ) {

    quality <- match.arg(quality)

    if(length(start_time) != 1 ||
       is.na(start_time)) {
      start_time <- stream_plot_container$start_time
    }
    if (length(duration) != 1 ||
        is.na(duration)) {
      start_time <- stream_plot_container$max_duration
    }
    if (length(channel_gap) != 1 ||
        is.na(channel_gap)) {
      channel_gap <- stream_plot_container$channel_gap
    }

    end_time <- start_time + duration

    current_start_time <- stream_plot_container$start_time
    current_duration <- stream_plot_container$max_duration
    current_data_range <- current_start_time + c(0, current_duration)

    stream_plot_container$channel_gap <- channel_gap
    stream_plot_container$title <- ""

    switch (
      quality,
      "high-quality" = { stream_plot_container$MAX_POINTS <- 2000000 },
      "performance" = { stream_plot_container$MAX_POINTS <- 100000 },
      { stream_plot_container$MAX_POINTS <- 500000 }
    )

    # Whether to load data from disk - performance
    data_needs_update <- init ||
      current_data_range[[1]] > start_time ||
      current_data_range[[2]] < end_time

    # Update stream_plot_container
    if( data_needs_update ) {
      ravedash::show_notification(title = "Updating figure",
                                  message = "Loading data...",
                                  class = ns("notification"),
                                  autohide = TRUE)

      if( init ) {
        load_start_time <- start_time
        load_duration <- duration
      } else {
        # preload duration
        total_sample_rates <- sum(stream_plot_container$sample_rates)
        total_timepoints <- duration * total_sample_rates
        if(total_timepoints <= 1e6) {
          # 40 MB from disk
          load_duration <- 1e7 / total_sample_rates
          load_start_time <- start_time - ((load_duration - duration) * 0.5)
          if(load_start_time < 0) {
            load_start_time <- 0
          }
        } else if(total_timepoints <= 1e7){
          # max 100 MB from disk
          load_start_time <- start_time
          load_duration <- duration + ceiling(duration * 0.75)
        } else {
          load_start_time <- start_time
          load_duration <- duration
        }
      }

      n_chans <- ncol(local_data$block_filearray)
      signal_data <- unname(subset(
        local_data$block_filearray,
        Time ~ Time >= load_start_time & Time <= (load_start_time + load_duration),
        drop = FALSE, .env = environment()
      ))

      lapply(seq_len(n_chans), function(ii) {
        stream_plot_container$set_channel_data(ii, data = signal_data[, ii])
        return()
      })

      stream_plot_container$start_time <- load_start_time
    }

    # TODO: set epoch annotations
    current_block <- local_data$block_filearray$get_header("block")
    current_sample_rate <- local_data$block_filearray$get_header("sample_rate")
    current_pulses <- local_data$pulses[[current_block]]
    if(length(current_pulses) && current_pulses$n_pulses > 0) {

      if(is.data.frame(current_pulses$annot_table)) {
        annot_table <- current_pulses$annot_table
      } else {
        .GlobalEnv$current_pulses <- current_pulses
        if(
          is.unsorted(current_pulses$onset_index) &&
          (length(current_pulses$onset_index) == length(current_pulses$offset_index))
        ) {
          o <- order(current_pulses$onset_index)
          current_pulses$onset_index <- current_pulses$onset_index[o]
          current_pulses$offset_index <- current_pulses$offset_index[o]
        }
        annot_table <- data.frame(
          time = (c(current_pulses$onset_index, current_pulses$offset_index) - 1) / current_sample_rate,
          label = c(
            sprintf("StimOn [%d]", seq_along(current_pulses$onset_index)),
            sprintf("StimOff [%d]", seq_along(current_pulses$offset_index))
          ),
          color = c(
            rep(2, length(current_pulses$onset_index)),
            rep(3, length(current_pulses$offset_index))
          )
        )
        local_data$pulses[[current_block]]$annot_table <- annot_table
      }

      clip_start <- stream_plot_container$start_time
      clip_end <- clip_start + stream_plot_container$max_duration

      sub_table <- annot_table[annot_table$time >= clip_start & annot_table$time <= clip_end, ]


      stream_plot_container$annotations <- sub_table
    } else {
      stream_plot_container$annotations <- NULL
    }


    if(!init) {
      ravedash::show_notification(title = "Updating figure",
                                  message = "Updating plot data...",
                                  class = ns("notification"),
                                  autohide = TRUE)
      stream_plot_container$update(proxy = stream_proxy,
                                   start_time = start_time,
                                   duration = duration)
    }
    ravedash::clear_notifications(class = ns("notification"))
  }

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      loaded_signals <- pipeline['loaded_signals']

      epoch_tables <- lapply(loaded_signals, function(arr) {
        # arr <- loaded_signals[[1]]
        arr <- arr$`@impl`
        block <- arr$get_header("block")
        sample_rate <- arr$get_header("sample_rate")

        pulse_info <- local_data$pulses[[block]]

        if(!is.list(pulse_info)) { return(NULL) }

        if(!isTRUE(pulse_info$n_pulses > 0)) { return(NULL) }

        onset_time <- (pulse_info$onset_index - 1L) / sample_rate

        epoch_table <- data.table::data.table(
          Block = block,
          Time = onset_time,
          Condition = "Stim"
        )
        if(length(pulse_info$offset_index) == length(onset_time)) {
          epoch_table$Event_StimOffset <- (pulse_info$offset_index - 1L) / sample_rate
        }
        epoch_table
      })

      epoch_tables <- data.table::rbindlist(epoch_tables)
      if(!nrow(epoch_tables)) {
        stop("There is no stimulation epoch found/created. Please find/generate them first.")
      }

      epoch_tables$Trial <- seq_len(nrow(epoch_tables))

      local_data$epoch_table_to_save <- epoch_tables

      shiny::showModal(shiny::modalDialog(
        title = "Stimulation Epoch Table",
        easyClose = FALSE,
        shiny::div(
          style = "max-height:70vh; overflow-y:scroll;",
          shiny::HTML(
            knitr::kable(epoch_tables, format = "html", digits = 4, row.names = FALSE, align = "r")
          )
        ),
        footer = shiny::tagList(
          shiny::textInput(inputId = ns("save_name"), label = "Epoch name to save", value = "stim", width = "100%"),
          shiny::modalButton("Dismiss"),
          dipsaus::actionButtonStyled(inputId = ns("save_btn"), "Save!")
        )
      ))

      return()

    }, error_wrapper = "notification"),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      save_name <- input$save_name
      if(!length(save_name) || is.na(save_name) || !nzchar(trimws(save_name))) {
        stop("Invalid epoch name. Please enter a valid non-blank epoch name")
      }
      if(!grepl("^[a-zA-Z0-9_-]+$", save_name)) {
        stop("Epoch name must only contain letters (a-z) and digits (0-9)")
      }
      epoch_tables <- local_data$epoch_table_to_save
      if(!is.data.frame(epoch_tables) || !nrow(epoch_tables)) {
        stop("There is no stimulation epoch found/created. Please find/generate them first.")
      }

      subject <- pipeline['subject']
      fname <- sprintf("epoch_%s.csv", save_name)
      ravecore:::safe_write_csv(x = epoch_tables, file = file.path(subject$meta_path, fname))

      shiny::removeModal(session = session)

      dipsaus::shiny_alert2(
        title = "Success", icon = "success",
        text = sprintf("The epoch file [%s] has been exported to subject's meta folder.", fname),
        buttons = "OK", session = session
      )

    }, error_wrapper = "alert"),
    input$save_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "prepare_subject_raw_voltage_with_blocks")){
        ravepipeline::logger("Repository read from the pipeline, but it is not an instance of `prepare_subject_raw_voltage_with_blocks`. Abort initialization", level = "warning")
        return()
      }
      ravepipeline::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "prepare_subject_raw_voltage_with_blocks")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravepipeline::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository
      component_container$initialize_with_new_data()

      local_data$load_block_btn_clicked <- FALSE

      # reset current block
      local_reactives$current_block <- NULL
      local_reactives$update_outputs <- FALSE

      all_blocks <- new_repository$blocks
      shiny::updateSelectInput(
        session = session,
        inputId = "recording_block",
        choices = all_blocks,
        selected = input$recording_block %OF% all_blocks
      )


      # get pulses
      loaded_signals <- pipeline['loaded_signals']
      local_data$pulses <- structure(
        names = all_blocks,
        lapply(all_blocks, function(block) {
          arr <- loaded_signals[[block]]$`@impl`
          sample_rate <- arr$get_header("sample_rate")
          epoch_table <- arr$get_header("epoch_table")
          if(!is.data.frame(epoch_table) || !nrow(epoch_table)) {
            return(NULL)
          }

          list(
            n_pulse = nrow(epoch_table),
            onset_index = round(epoch_table$Time * sample_rate) + 1,
            offset_index = round(as.double(epoch_table$Event_StimOffset) * sample_rate) + 1
          )

        })
      )

      local_reactives$current_pulse_info <- NULL

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  visualize_block <- function() {
    if(!ravedash::watch_data_loaded()) { return() }
    if(ravedash::watch_loader_opened()) { return() }
    if(length(input$recording_block) != 1) { return() }
    recording_block <- input$recording_block

    loaded_signals <- pipeline['loaded_signals']
    arr <- loaded_signals[[recording_block]]$`@impl`
    local_data$block_filearray <- arr

    sample_rate <- arr$get_header("sample_rate")
    max_duration <- arr$get_header("max_duration")
    stream_plot_container$sample_rates <- sample_rate
    stream_plot_container$channel_names

    rg <- range(arr, na.rm = TRUE)
    rg <- rg[is.finite(rg)]
    if(length(rg)) {
      channel_gap <- max(abs(rg)) * 2
      shiny::updateNumericInput(
        session = session,
        inputId = "channel_gap",
        value = ceiling(channel_gap)
      )

    }

    # Initialize figure inputs
    max_start_time <- floor(max_duration - 0.5)
    shiny::updateNumericInput(
      session = session,
      inputId = 'start_time',
      max = max_start_time
    )
    start_time <- input$start_time
    if(!isTRUE(start_time <= max_start_time)) {
      start_time <- 0
    }

    update_plot(
      start_time = start_time,
      duration = input$duration,
      channel_gap = channel_gap,
      quality = input$quality,
      init = TRUE
    )

    local_data$load_block_btn_clicked <- TRUE
    local_reactives$update_outputs <- Sys.time()
  }

  shiny::bindEvent(
    ravedash::safe_observe({

      visualize_block()
      shidashi::card_operate(title = "Data Selector",
                             method = "collapse",
                             session = session)
    }),
    input$load_block_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ---- input inter-interactions of stream plot ----------------
  shiny::bindEvent(
    ravedash::safe_observe({
      # try({
      duration <- input$duration
      if(length(duration) == 1 && !is.na(duration) && isTRUE(duration > 0)) {
        shiny::updateNumericInput(
          session = session,
          inputId = 'start_time',
          step = max(duration * 0.75, min(1, round(duration, 2)))
        )
      }
      # })
    }),
    input$duration,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      update_plot(
        start_time = input$start_time,
        duration = input$duration,
        channel_gap = input$channel_gap,
        quality = input$quality,
        init = FALSE,
        stream_proxy = stream_proxy
      )
    }),
    input$start_time,
    input$duration,
    input$channel_gap,
    input$quality,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      relayout <- as.list(plotly::event_data("plotly_relayout"))
      start_time <- as.numeric(relayout[["xaxis.range[0]"]])
      end_time <- as.numeric(relayout[["xaxis.range[1]"]])
      if(length(start_time) != 1 || is.na(start_time)) { return() }
      # start_time <- floor(start_time)
      shiny::updateNumericInput(session = session,
                                inputId = 'start_time',
                                value = start_time)

      if(length(end_time) != 1 || is.na(end_time)) { return() }
      duration <- end_time - start_time
      shiny::updateNumericInput(session = session,
                                inputId = 'duration',
                                value = duration)
    }),
    input$sync,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )


  # ---- Epoch finders --------------------------
  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(ravedash::watch_loader_opened()) { return() }

      if(!isTRUE(local_data$load_block_btn_clicked)) {
        visualize_block()
      }

      # if(length(input$recording_block) != 1) { return() }
      # recording_block <- input$recording_block
      #
      # loaded_signals <- pipeline['loaded_signals']
      # arr <- loaded_signals[[recording_block]]$`@impl`

      pulse_duration <- input$pulse_duration
      if(!isTRUE(pulse_duration > 0)) {
        stop("Invalid pulse duration. Please provide an estimate of the pulse duration in milliseconds. It provides a hint for a single pulse duration and does not need to be accurate.")
      }
      arr <- local_data$block_filearray
      current_block <- arr$get_header("block")

      pulse_count <- as.integer(input$pulse_count)
      pulse_threshold <- as.double(input$pulse_threshold)
      if(!isTRUE(pulse_count >= 1)) { pulse_count <- NA_integer_ }
      if(!isTRUE(pulse_threshold > 0)) { pulse_threshold <- NA_real_ }


      pulse_info <- ravetools::stimpulse_find(
        arr[drop = TRUE, dimnames = FALSE],
        sample_rate = arr$get_header("sample_rate"),
        pulse_duration = pulse_duration / 1000,
        n_pulses = pulse_count,
        threshold = pulse_threshold
      )

      local_data$pulses[[current_block]] <- pulse_info

      local_reactives$current_pulse_info <- pulse_info

      update_plot(
        start_time = input$start_time,
        duration = input$duration,
        channel_gap = input$channel_gap,
        quality = input$quality,
        init = FALSE,
        stream_proxy = stream_proxy
      )

    }, error_wrapper = "alert"),
    input$findall_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(ravedash::watch_loader_opened()) { return() }

      if(!isTRUE(local_data$load_block_btn_clicked)) {
        visualize_block()
      }

      arr <- local_data$block_filearray
      current_block <- arr$get_header("block")

      local_data$pulses[[current_block]] <- NULL

      local_reactives$current_pulse_info <- NULL

      update_plot(
        start_time = input$start_time,
        duration = input$duration,
        channel_gap = input$channel_gap,
        quality = input$quality,
        init = FALSE,
        stream_proxy = stream_proxy
      )

    }, error_wrapper = "alert"),
    input$clearall_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(ravedash::watch_loader_opened()) { return() }

      pulse_duration <- input$pulse_duration
      if(!isTRUE(pulse_duration > 0)) {
        stop("Invalid pulse duration. Please provide an estimate of the pulse duration in milliseconds. It provides a hint for a single pulse duration and does not need to be accurate.")
      }

      start_time <- input$start_time
      end_time <- start_time + input$duration
      if(length(start_time) != 1 || is.na(start_time) || is.na(end_time)) {
        stop("Invalid start timme and/or duration. Please make sure the figure start time and duration are positive numbers")
      }


      arr <- local_data$block_filearray
      current_block <- arr$get_header("block")
      sample_rate <- arr$get_header("sample_rate")

      start_timepoint <- floor(start_time * sample_rate) + 1
      end_timepoint <- ceiling(end_time * sample_rate) + 1
      total_timepoint <- nrow(arr)
      if(total_timepoint <= start_timepoint) {
        stop("Starting time exceed the total time of current recording block")
      }
      if(total_timepoint < end_timepoint) {
        end_timepoint <- total_timepoint
      }

      pulse_count <- as.integer(input$pulse_count)
      pulse_threshold <- as.double(input$pulse_threshold)
      if(!isTRUE(pulse_count >= 1)) { pulse_count <- NA_integer_ }
      if(!isTRUE(pulse_threshold > 0)) { pulse_threshold <- NA_real_ }

      pulse_info <- ravetools::stimpulse_find(
        arr[seq.int(start_timepoint, end_timepoint, by = 1), 1, drop = TRUE, dimnames = FALSE],
        sample_rate = sample_rate,
        pulse_duration = pulse_duration / 1000,
        n_pulses = pulse_count,
        threshold = pulse_threshold
      )

      current_info <- local_data$pulses[[current_block]]

      if(length(current_info)) {

        sel <- current_info$onset_index < start_timepoint | current_info$onset_index > end_timepoint

        if(sum(sel)) {
          current_info$n_pulses <- sum(sel)
          current_info$onset_index <- current_info$onset_index[sel]
          current_info$offset_index <- current_info$offset_index[sel]
        } else {
          current_info <- NULL
        }
      }

      if(pulse_info$n_pulses > 0) {
        pulse_info$onset_index <- pulse_info$onset_index + (start_timepoint - 1)
        pulse_info$offset_index <- pulse_info$offset_index + (start_timepoint - 1)

        if(length(current_info)) {
          current_info$n_pulses <- current_info$n_pulses + pulse_info$n_pulses
          current_info$onset_index <- c(current_info$onset_index, pulse_info$onset_index)
          current_info$offset_index <- c(current_info$offset_index, pulse_info$offset_index)
        } else {
          current_info <- pulse_info
        }
      }
      current_info$annot_table <- NULL
      local_data$pulses[[current_block]] <- current_info

      local_reactives$current_pulse_info <- current_info

      update_plot(
        start_time = input$start_time,
        duration = input$duration,
        channel_gap = input$channel_gap,
        quality = input$quality,
        init = FALSE,
        stream_proxy = stream_proxy
      )

    }, error_wrapper = "alert"),
    input$find_within_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(ravedash::watch_loader_opened()) { return() }

      arr <- local_data$block_filearray
      current_block <- arr$get_header("block")
      sample_rate <- arr$get_header("sample_rate")
      current_info <- local_data$pulses[[current_block]]

      if(!length(current_info)) { return() }

      pulse_duration <- input$pulse_duration
      if(!isTRUE(pulse_duration > 0)) {
        return()
      }

      start_time <- input$start_time
      end_time <- start_time + input$duration
      if(length(start_time) != 1 || is.na(start_time) || is.na(end_time)) {
        return()
      }


      start_timepoint <- floor(start_time * sample_rate) + 1
      end_timepoint <- ceiling(end_time * sample_rate) + 1
      total_timepoint <- nrow(arr)
      if(total_timepoint <= start_timepoint) {
        return()
      }

      current_info <- local_data$pulses[[current_block]]

      sel <- current_info$onset_index < start_timepoint | current_info$onset_index > end_timepoint
      if(sum(sel)) {
        current_info$n_pulses <- sum(sel)
        current_info$onset_index <- current_info$onset_index[sel]
        current_info$offset_index <- current_info$offset_index[sel]
      } else {
        current_info <- NULL
      }

      current_info$annot_table <- NULL
      local_data$pulses[[current_block]] <- current_info
      local_reactives$current_pulse_info <- current_info

      update_plot(
        start_time = input$start_time,
        duration = input$duration,
        channel_gap = input$channel_gap,
        quality = input$quality,
        init = FALSE,
        stream_proxy = stream_proxy
      )

    }, error_wrapper = "alert"),
    input$clear_within_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ---- Align pulses ------------------------------------------------------
  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(ravedash::watch_loader_opened()) { return() }

      ravepipeline::logger("Adjusting stimulation pulses", level = "trace")

      ravedash::show_notification("Aligning stimulation pulses.", type = "default", autohide = FALSE, class = ns("aligning"))
      on.exit({
        Sys.sleep(0.5)
        ravedash::clear_notifications(class = ns("aligning"))
      })

      arr <- local_data$block_filearray
      current_block <- arr$get_header("block")
      sample_rate <- arr$get_header("sample_rate")
      current_info <- local_data$pulses[[current_block]]

      expand_pre <- -abs(input$expand_pre)
      expand_post <- abs(input$expand_post)
      if(!is.finite(expand_pre) || !is.finite(expand_post)) {
        stop("Search time-points before/after stim onset must be an integer (cannot be missing/infinite)")
      }

      if(!length(current_info) || !isTRUE(current_info$n_pulses > 0)) {
        stop("No pulses is detected. Please find initial positions for stim onset.")
      }

      current_info <- ravetools::stimpulse_align(
        arr[drop = TRUE, dimnames = FALSE],
        pulse_info = current_info, expand_timepoints = c(expand_pre, expand_post)
      )

      local_data$pulses[[current_block]] <- current_info
      local_reactives$current_pulse_info <- current_info

      update_plot(
        start_time = input$start_time,
        duration = input$duration,
        channel_gap = input$channel_gap,
        quality = input$quality,
        init = FALSE,
        stream_proxy = stream_proxy
      )


    }, error_wrapper = "alert"),
    input$align_all_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(ravedash::watch_loader_opened()) { return() }

      ravepipeline::logger("Adjusting stimulation pulses within selected window", level = "trace")

      ravedash::show_notification("Aligning stimulation pulses.", type = "default", autohide = FALSE, class = ns("aligning"))
      on.exit({
        Sys.sleep(0.5)
        ravedash::clear_notifications(class = ns("aligning"))
      })

      arr <- local_data$block_filearray
      current_block <- arr$get_header("block")
      sample_rate <- arr$get_header("sample_rate")
      current_info <- local_data$pulses[[current_block]]

      expand_pre <- -abs(input$expand_pre)
      expand_post <- abs(input$expand_post)
      if(!is.finite(expand_pre) || !is.finite(expand_post)) {
        stop("Search time-points before/after stim onset must be an integer (cannot be missing/infinite)")
      }

      if(!length(current_info) || !isTRUE(current_info$n_pulses > 0)) {
        stop("No pulses is detected. Please find initial positions for stim onset.")
      }

      pulse_duration <- input$pulse_duration
      if(!isTRUE(pulse_duration > 0)) {
        return()
      }

      start_time <- input$start_time
      end_time <- start_time + input$duration
      if(length(start_time) != 1 || is.na(start_time) || is.na(end_time)) {
        return()
      }


      start_timepoint <- floor(start_time * sample_rate) + 1
      end_timepoint <- ceiling(end_time * sample_rate) + 1

      sel <- current_info$onset_index >= start_timepoint & current_info$onset_index <= end_timepoint
      if(sum(sel)) {
        puase_info <- list(
          n_pulses = sum(sel),
          onset_index = current_info$onset_index[sel],
          offset_index = current_info$offset_index[sel]
        )
        print(sum(sel))
      } else {
        return()
      }

      puase_info <- ravetools::stimpulse_align(
        arr[drop = TRUE, dimnames = FALSE],
        pulse_info = puase_info, expand_timepoints = c(expand_pre, expand_post)
      )

      current_info$annot_table <- NULL
      current_info$onset_index[sel] <- puase_info$onset_index
      current_info$offset_index[sel] <- puase_info$offset_index

      local_data$pulses[[current_block]] <- current_info
      local_reactives$current_pulse_info <- current_info

      update_plot(
        start_time = input$start_time,
        duration = input$duration,
        channel_gap = input$channel_gap,
        quality = input$quality,
        init = FALSE,
        stream_proxy = stream_proxy
      )


    }, error_wrapper = "alert"),
    input$align_window_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     if(!ravedash::watch_data_loaded()) { return() }
  #     if(ravedash::watch_loader_opened()) { return() }
  #
  #     arr <- local_data$block_filearray
  #     current_block <- arr$get_header("block")
  #     sample_rate <- arr$get_header("sample_rate")
  #     current_info <- local_data$pulses[[current_block]]
  #
  #     if(!length(current_info)) { return() }
  #
  #     pulse_duration <- input$pulse_duration
  #     # if(!isTRUE(pulse_duration > 0)) {
  #     #   return()
  #     # }
  #
  #     start_time <- input$start_time
  #     end_time <- start_time + input$duration
  #     # if(length(start_time) != 1 || is.na(start_time) || is.na(end_time)) {
  #     #   return()
  #     # }
  #
  #
  #     start_timepoint <- floor(start_time * sample_rate) + 1
  #     end_timepoint <- ceiling(end_time * sample_rate) + 1
  #     total_timepoint <- nrow(arr)
  #     # if(total_timepoint <= start_timepoint) {
  #     #   return()
  #     # }
  #
  #     current_info <- local_data$pulses[[current_block]]
  #
  #     sel <- current_info$onset_index < start_timepoint | current_info$onset_index > end_timepoint
  #     if(sum(sel)) {
  #       current_info$n_pulses <- sum(sel)
  #       current_info$onset_index <- current_info$onset_index[sel]
  #       current_info$offset_index <- current_info$offset_index[sel]
  #     } else {
  #       current_info <- NULL
  #     }
  #
  #     current_info$annot_table <- NULL
  #     local_data$pulses[[current_block]] <- current_info
  #     local_reactives$current_pulse_info <- current_info
  #
  #     update_plot(
  #       start_time = input$start_time,
  #       duration = input$duration,
  #       channel_gap = input$channel_gap,
  #       quality = input$quality,
  #       init = FALSE,
  #       stream_proxy = stream_proxy
  #     )
  #
  #   }, error_wrapper = "alert"),
  #   input$align_all_btn,
  #   ignoreNULL = TRUE, ignoreInit = TRUE
  # )
  #

  # ---- Register outputs --------------------------------
  output$stream_plot <- plotly::renderPlotly({
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    stream_plot_container$render()

  })


  output$snippet_plot <- shiny::renderPlot({

    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      ),
      shiny::need(
        is.list(local_reactives$current_pulse_info) &&
          isTRUE(local_reactives$current_pulse_info$n_pulses > 0),
        message = "No pulses detected"
      )
    )

    arr <- local_data$block_filearray
    if(is.null(arr)) { return() }

    current_block <- arr$get_header("block")
    sample_rate <- arr$get_header("sample_rate")

    pulse_info <- local_reactives$current_pulse_info

    n_pulses <- pulse_info$n_pulses

    expand_pre <- -abs(input$expand_pre)
    expand_post <- abs(input$expand_post)

    if(!isTRUE(expand_pre < -1)) {
      expand_pre <- -1
    }

    if(!isTRUE(expand_post > 1)) {
      expand_post <- 1
    }
    expand_timepoints <- c(expand_pre, expand_post)

    pulses_snippets <- ravetools::stimpulse_extract(
      arr[drop = TRUE, dimnames = FALSE],
      pulse_info = pulse_info,
      expand_timepoints = expand_timepoints,
      center = TRUE
    )

    snippet_time <- seq(
      expand_timepoints[[1]], by = 1,
      length.out = nrow(pulses_snippets)) / sample_rate * 1000

    matplot(snippet_time, pulses_snippets, type = 'l', lty = 1, col = 'gray80',
            xlab = "Time (millisecond)", ylab = "uV",
            main = sprintf("Centered pulses (n=%d)", n_pulses))
    lines(snippet_time, rowMeans(pulses_snippets), col = 'red')

  })


  output$snippet_window_plot <- shiny::renderPlot({

    start_time <- input$start_time
    end_time <- start_time + input$duration
    if(length(start_time) != 1 || is.na(start_time) || is.na(end_time)) {
      return()
    }


    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      ),
      shiny::need(
        is.list(local_reactives$current_pulse_info) &&
          isTRUE(local_reactives$current_pulse_info$n_pulses > 0) &&
          length(start_time) == 1 && is.finite(start_time) &&
          length(end_time) == 1 && is.finite(end_time),
        message = "No pulses detected"
      )
    )

    arr <- local_data$block_filearray
    if(is.null(arr)) { return() }

    current_block <- arr$get_header("block")
    sample_rate <- arr$get_header("sample_rate")

    pulse_info <- local_reactives$current_pulse_info

    start_timepoint <- floor(start_time * sample_rate) + 1
    end_timepoint <- ceiling(end_time * sample_rate) + 1
    total_timepoint <- nrow(arr)

    sel <- pulse_info$onset_index >= start_timepoint & pulse_info$onset_index <= end_timepoint

    n_pulses <- sum(sel)
    if(n_pulses == 0) { return() }

    pulse_info <- list(
      n_pulses = n_pulses,
      onset_index = pulse_info$onset_index[sel],
      offset_index = pulse_info$offset_index[sel]
    )

    expand_pre <- -abs(input$expand_pre)
    expand_post <- abs(input$expand_post)

    if(!isTRUE(expand_pre < -1)) {
      expand_pre <- -1
    }

    if(!isTRUE(expand_post > 1)) {
      expand_post <- 1
    }
    expand_timepoints <- c(expand_pre, expand_post)

    pulses_snippets <- ravetools::stimpulse_extract(
      arr[drop = TRUE, dimnames = FALSE],
      pulse_info = pulse_info,
      expand_timepoints = expand_timepoints,
      center = TRUE
    )

    snippet_time <- seq(
      expand_timepoints[[1]], by = 1,
      length.out = nrow(pulses_snippets)) / sample_rate * 1000

    matplot(snippet_time, pulses_snippets, type = 'l', lty = 1, col = 'gray80',
            xlab = "Time (millisecond)", ylab = "uV",
            main = sprintf("Centered pulses within the window (n=%d)", n_pulses))
    lines(snippet_time, rowMeans(pulses_snippets), col = 'red')

  })


}
