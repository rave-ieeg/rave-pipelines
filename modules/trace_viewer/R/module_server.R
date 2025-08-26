
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  server_tools$run_analysis_onchange(
    component_container$get_input_ids(c(
      "electrode_text"
    ))
  )

  stream_proxy <- plotly::plotlyProxy(outputId = "channel_viewer", session = session)

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      if(!ravedash::watch_data_loaded()) { return() }

      repository <- local_data$repository

      # Collect input data
      recording_block <- input$recording_block
      if(!length(recording_block)) {
        stop("Recording block is empty. Please choose a recording block first")
      }
      if(!isTRUE(recording_block %in% repository$blocks)) {
        stop("Invalid recording block chosen: ", paste(recording_block))
      }
      local_data$recording_block <- recording_block

      epoch_name <- input$annotation_source
      if(epoch_name == "") {
        epoch_name = NULL
      }
      epoch_events <- input$annotation_events
      epoch_events <- c("Trial Onset", epoch_events[epoch_events != "Trial Onset"])
      pipeline$set_settings(
        epoch_name = epoch_name,
        epoch_events = epoch_events
      )
      annotation_table <- pipeline$run("annotation_table")
      annotation_table$trial_name <- gsub("<br>", " - ", annotation_table$label)
      local_data$annotation_table <- annotation_table


      channels <- dipsaus::parse_svec(input$electrode_text)
      channels <- channels[channels %in% repository$electrode_list]
      if(!length(channels)) {
        channels <- repository$electrode_list
      }
      if(!length(channels)) {
        stop("No channel to visualize. Please import valid channels")
      }

      sample_rates <- repository$sample_rates
      electrode_types <- repository$subject$electrode_types[repository$subject$electrodes %in% channels]
      sample_rates <- unname(unlist(sample_rates[electrode_types]))

      o <- order(repository$electrode_table$Electrode)
      electrode_table <- repository$electrode_table[o, ]
      electrode_table <- electrode_table[electrode_table$Electrode %in% channels, ]
      electrode_table$SignalType <- electrode_types
      electrode_table$SampleRate <- sample_rates

      channel_names <- sprintf("%s|ch%d", electrode_table$Label, electrode_table$Electrode)
      local_data$electrode_table <- electrode_table

      signal_container <- repository$get_container()
      block_info <- signal_container[[recording_block]]
      preferred_names <- c("LFP", "Spike", "Auxiliary", names(block_info)[[1]])
      preferred_name <- preferred_names[preferred_names %in% names(block_info)][[1]]
      signal_info <- block_info[[preferred_name]]
      n_timepoints <- signal_info$dim[[1]]
      max_duration <- floor(n_timepoints / signal_info$sample_rate)

      shiny::updateNumericInput(
        session = session,
        input = "viewer_start_time",
        max = max_duration
      )

      channel_gap <- input$viewer_channel_gap
      if(!isTRUE(channel_gap > 0)) {
        # find signal gap by quantile
        if(n_timepoints > 60000) {
          qt <- quantile(signal_info$data[sample(n_timepoints - 60001, 1) + seq_len(60000), ], c(0.005, 0.995), na.rm = TRUE)
        } else {
          qt <- quantile(signal_info$data[], c(0.005, 0.995), na.rm = TRUE)
        }
        channel_gap <- max(0, ceiling(qt[[2]] - qt[[1]]))
        if(is.finite(channel_gap)) {

          shiny::updateNumericInput(
            session = session,
            input = "viewer_channel_gap",
            value = channel_gap
          )

        } else {
          channel_gap <- 0
        }
      }

      # build plotly instance
      stream_plot_container <- StreamSignalPlot$new(
        n_channels = length(channels),
        sample_rates = sample_rates,
        start_time = 0,
        channel_names = channel_names,
        channel_gap = channel_gap,
        title = sprintf("Recording block: %s", recording_block),
        ylab = "Channel"
      )

      if(is.data.frame(annotation_table)) {
        annotation_subset <- annotation_table[annotation_table$block %in% recording_block, ]
        stream_plot_container$annotations <- annotation_subset

        condition_label <- annotation_subset$trial_name
      } else {
        condition_label <- character()
      }

      shiny::updateSelectInput(
        session = session,
        input = "viewer_trial",
        choices = condition_label,
        selected = NA
      )

      local_reactives$stream_plot_container <- stream_plot_container

      update_plot(init = TRUE)

      return()
    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  update_plot <- function(init = FALSE) {
    show_rendering_notification(message = "Loading & processing data...", session = session)
    on.exit({
      Sys.sleep(0.5)
      hide_rendering_notification(session = session)
    })
    stream_plot_container <- shiny::isolate(local_reactives$stream_plot_container)


    start_time <- shiny::isolate(input$viewer_start_time)
    if(is.na(start_time)) { start_time <- stream_plot_container$start_time }

    duration <- shiny::isolate(input$viewer_duration)
    if(is.na(duration)) { start_time <- stream_plot_container$max_duration }

    channel_gap <- shiny::isolate(input$viewer_channel_gap)
    if(is.na(channel_gap)) { channel_gap <- stream_plot_container$channel_gap }

    highpass_freq <- local_data$highpass_freq
    lowpass_freq <- local_data$lowpass_freq
    if(!length(highpass_freq)) { highpass_freq <- NA }
    if(!length(lowpass_freq)) { lowpass_freq <- NA }
    # print(c(highpass_freq, lowpass_freq))

    auto_decimate <- paste(input$auto_decimate, collapse = "")
    end_time <- start_time + duration

    needs_update <- FALSE
    current_range <- stream_plot_container$start_time + c(0, stream_plot_container$max_duration)
    if(
      init ||
      current_range[[1]] > start_time ||
      current_range[[2]] < end_time
    ) {
      needs_update <- TRUE
    }

    stream_plot_container$channel_gap <- channel_gap
    switch (
      auto_decimate,
      "high-quality" = { stream_plot_container$MAX_POINTS <- 2000000 },
      "performance" = { stream_plot_container$MAX_POINTS <- 100000 },
      { stream_plot_container$MAX_POINTS <- 500000 }
    )

    # Update stream_plot_container
    if( needs_update ) {
      electrode_table <- local_data$electrode_table
      recording_block <- local_data$recording_block
      repository <- local_data$repository
      signal_container <- repository$get_container()
      block_data <- signal_container[[recording_block]]

      # Set annotations
      annotation_table <- local_data$annotation_table
      if(is.data.frame(annotation_table)) {
        stream_plot_container$annotations <- annotation_table[annotation_table$block %in% recording_block, ]
      } else {
        stream_plot_container$annotations <- NULL
      }

      msg <- NULL
      if(!is.na(highpass_freq)) {
        msg <- sprintf(" HighPass=%g", highpass_freq)
      }
      if(!is.na(lowpass_freq)) {
        msg <- c(msg, sprintf(" LowPass=%g", lowpass_freq))
      }
      msg <- paste(msg, collapse = ",")
      stream_plot_container$title <- sprintf("Recording block: %s%s", recording_block, msg)

      signal_types <- unique(electrode_table$SignalType)

      if( init ) {
        load_start_time <- start_time
        load_duration <- duration
      } else {
        # preload duration

        total_sample_rates <- sum(electrode_table$SampleRate)
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

      # construct filters
      filters <- list()
      if(!is.na(highpass_freq) || !is.na(lowpass_freq)) {

        # print(c(highpass_freq, lowpass_freq))

        filters[[length(filters) + 1]] <- structure(
          names = signal_types,
          lapply(signal_types, function(signal_type) {
            sample_rate <- repository$sample_rates[[signal_type]]

            max_order <- floor(load_duration * sample_rate / 3) - 1

            ravetools::design_filter(
              sample_rate = sample_rate,
              method = "firls",
              high_pass_freq = highpass_freq,
              low_pass_freq = lowpass_freq,
              filter_order = min(1600, max_order)
            )
          })
        )

      }


      lapply(signal_types, function(signal_type) {
        row_selector <- which(electrode_table$SignalType == signal_type)
        signal_info <- block_data[[signal_type]]

        channels <- electrode_table$Electrode[row_selector]

        signal_data <- unname(subset(
          signal_info$data,
          Electrode ~ Electrode %in% channels,
          Time ~ Time >= load_start_time & Time <= (load_start_time + load_duration),
          drop = FALSE, .env = environment()
        ))

        dimnames(signal_data) <- NULL


        for(filter in filters) {
          # print(filter)
          filter_impl <- filter[[signal_type]]
          if(length(filter_impl)) {
            signal_data <- ravetools::filtfilt(b = filter_impl$b, a = filter_impl$a, x = signal_data)
          }
        }

        lapply(seq_along(row_selector), function(ii) {
          stream_plot_container$set_channel_data(row_selector[[ii]], data = signal_data[, ii])
          return()
        })

        return()
      })

      stream_plot_container$start_time <- load_start_time
    }


    if(!init) {
      show_rendering_notification(message = "Updating graphics...", session = session)
      stream_plot_container$update(proxy = stream_proxy,
                                   start_time = start_time,
                                   duration = duration)
    }
    hide_rendering_notification(session = session)
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }

      stream_plot_container <- local_reactives$stream_plot_container
      if(!length(stream_plot_container)) { return() }

      annotation_subset <- stream_plot_container$annotations
      if(!is.data.frame(annotation_subset) || !nrow(annotation_subset)) { return() }

      sel <- annotation_subset$trial_name %in% input$viewer_trial
      if(!any(sel)) { return() }

      start_time <- annotation_subset$time[sel][[1]] - 0.5
      shiny::updateNumericInput(
        session = session,
        inputId = "viewer_start_time",
        value = start_time
      )

    }),
    input$viewer_trial,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  switch_trial <- function(delta = 1) {
    if(!ravedash::watch_data_loaded()) { return() }

    stream_plot_container <- local_reactives$stream_plot_container
    if(!length(stream_plot_container)) { return() }

    annotation_subset <- stream_plot_container$annotations
    if(!is.data.frame(annotation_subset) || !nrow(annotation_subset)) { return() }

    nr <- nrow(annotation_subset)
    if(!nr) { return() }

    sel <- which(annotation_subset$trial_name %in% input$viewer_trial)

    if(!length(sel)) { sel <- 0 }

    sel <- (sel + delta - 1) %% nr + 1
    new_trial <- annotation_subset$trial_name[[sel]]
    shiny::updateSelectInput(
      session = session,
      inputId = "viewer_trial",
      selected = new_trial
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      switch_trial(-1)
    }),
    input$prev_trial,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      switch_trial(1)
    }),
    input$next_trial,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      highpass_freq <- input$filter_highpass
      if(length(highpass_freq) != 1 || is.na(highpass_freq) || highpass_freq <= 0) {
        highpass_freq <- NA
      }

      lowpass_freq <- input$filter_lowpass

      if(length(lowpass_freq) != 1 || is.na(lowpass_freq) || lowpass_freq <= 0) {
        lowpass_freq <- NA
      }

      local_data$highpass_freq <- highpass_freq
      local_data$lowpass_freq <- lowpass_freq
    }),
    input$filter_highpass,
    input$filter_lowpass,
    ignoreNULL = FALSE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      duration <- input$viewer_duration
      if(!is.numeric(duration) || is.na(duration)) { return() }
      shiny::updateNumericInput(
        session = session,
        inputId = "viewer_start_time",
        step = ceiling(duration * 0.75)
      )
    }, priority = 2),
    input$viewer_duration,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }

      stream_plot_container <- local_reactives$stream_plot_container
      if(!length(stream_plot_container)) { return() }

      update_plot(init = FALSE)
    }),
    input$viewer_start_time,
    input$viewer_duration,
    input$viewer_channel_gap,
    input$auto_decimate,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )



  # check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_repository")){
        ravepipeline::logger("Repository read from the pipeline, but it is not an instance of `rave_repository`. Abort initialization", level = "warning")
        return()
      }
      ravepipeline::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_repository")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravepipeline::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "info", use_glue = TRUE)
          return()
        }
      }

      # Mark flags to update outputs
      local_data$`@reset`()
      local_data$highpass_freq <- NA
      local_data$lowpass_freq <- NA
      local_data$repository <- new_repository
      local_reactives$stream_plot_container <- NULL
      local_reactives$update_outputs <- Sys.time()

      # reset UIs to default
      settings <- pipeline$get_settings()

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository
      component_container$initialize_with_new_data()

      shiny::updateSelectInput(
        session = session,
        inputId = 'recording_block',
        choices = new_repository$blocks
      )

      annotation_source_choices <- c("", new_repository$subject$epoch_names)
      annotation_source_selected <- settings$epoch_name %OF% annotation_source_choices
      shiny::updateSelectInput(
        session = session,
        inputId = "annotation_source",
        choices = annotation_source_choices,
        selected = annotation_source_selected
      )

      # make sure selectors have correct values
      later::later(delay = 1, function() {

        shiny::updateTextInput(
          session = session,
          inputId = "electrode_text",
          value = ravecore:::deparse_svec(new_repository$electrode_list)
        )

        shiny::updateSelectInput(
          session = session,
          inputId = "annotation_source",
          selected = annotation_source_selected
        )
      })

      # find nyquist
      nyquist <- floor(min(unlist(new_repository$sample_rates)) / 2)
      shiny::updateNumericInput(
        session = session,
        inputId = 'filter_highpass',
        max = nyquist
      )

      shiny::updateNumericInput(
        session = session,
        inputId = 'filter_lowpass',
        max = nyquist
      )

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      epoch_name <- input$annotation_source

      repository <- component_container$data$repository
      if(!epoch_name %in% repository$subject$epoch_names) { return() }
      epoch <- repository$subject$get_epoch(epoch_name = epoch_name)
      available_events <- epoch$available_events
      available_events <- available_events[available_events != ""]
      annotation_events_selected <- unique(c("Trial Onset", input$annotation_events))
      shiny::updateSelectInput(
        session = session,
        inputId = "annotation_events",
        choices = c("Trial Onset", available_events),
        selected = annotation_events_selected
      )
    }),
    input$annotation_source
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      relayout <- as.list(plotly::event_data("plotly_relayout"))
      start_time <- as.numeric(relayout[["xaxis.range[0]"]])
      end_time <- as.numeric(relayout[["xaxis.range[1]"]])
      if(length(start_time) != 1 || is.na(start_time)) { return() }
      # start_time <- floor(start_time)
      shiny::updateNumericInput(session = session,
                                inputId = 'viewer_start_time',
                                value = start_time)

      if(length(end_time) != 1 || is.na(end_time)) { return() }
      duration <- end_time - start_time
      shiny::updateNumericInput(session = session,
                                inputId = 'viewer_duration',
                                value = duration)
    }),
    input$viewer_apply_brush,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )

  # Register outputs
  output$channel_viewer <- plotly::renderPlotly({

    # local_reactives$stream_plot_container
    # local_reactives$update_outputs

    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs) &&
          length(local_reactives$stream_plot_container) > 0,
        message = "Please run the module first"
      )
    )

    stream_plot_container <- local_reactives$stream_plot_container

    start_time <- shiny::isolate(input$viewer_start_time)
    if(is.na(start_time)) { start_time <- stream_plot_container$start_time }

    duration <- shiny::isolate(input$viewer_duration)
    if(is.na(duration)) { start_time <- stream_plot_container$max_duration }

    impl <- stream_plot_container$render()
    plotly::layout(
      impl,
      showlegend = FALSE,
      xaxis = list(
        range = start_time + c(0, duration)
      )
    )
  })




}
