
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

      # build plotly instance
      stream_plot_container <- StreamSignalPlot$new(
        n_channels = length(channels),
        sample_rates = sample_rates,
        start_time = 0,
        channel_names = channel_names,
        channel_gap = 0,
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

          stream_plot_container$channel_gap <- channel_gap

          shiny::updateNumericInput(
            session = session,
            input = "viewer_channel_gap",
            value = channel_gap
          )

        } else {
          channel_gap <- 0
        }
      }

      local_reactives$stream_plot_container <- stream_plot_container
      update_plot(update = FALSE)

      return()
    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  update_plot <- function(update = TRUE) {
    stream_plot_container <- shiny::isolate(local_reactives$stream_plot_container)
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
    stream_plot_container$title <- sprintf("Recording block: %s", recording_block)

    # TODO: duration, start time, gap
    start_time <- shiny::isolate(input$viewer_start_time)
    duration <- shiny::isolate(input$viewer_duration)
    channel_gap <- shiny::isolate(input$viewer_channel_gap)
    if(is.na(channel_gap)) { channel_gap <- 0 }

    end_time <- start_time + duration

    lapply(seq_len(nrow(electrode_table)), function(ii) {
      signal_type <- electrode_table$SignalType[[ii]]
      signal_info <- block_data[[signal_type]]
      channel <- electrode_table$Electrode[[ii]]
      signal_data <- subset(
        signal_info$data,
        Electrode ~ Electrode == channel,
        Time ~ Time >= start_time & Time <= end_time,
        drop = TRUE, .env = environment()
      )
      stream_plot_container$set_channel_data(ii, data = unname(signal_data))
      return()
    })
    stream_plot_container$start_time <- start_time
    stream_plot_container$channel_gap <- channel_gap
    if(update) {
      stream_plot_container$update(proxy = stream_proxy)
    }
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(!length(local_reactives$stream_plot_container)) { return() }

      annotation_table <- local_data$annotation_table
      if(!is.data.frame(annotation_table)) { return() }

      recording_block <- local_data$recording_block
      annotation_subset <- annotation_table[annotation_table$block %in% recording_block, ]
      if(!nrow(annotation_subset)) { return() }

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
    if(!length(local_reactives$stream_plot_container)) { return() }

    annotation_table <- local_data$annotation_table
    if(!is.data.frame(annotation_table)) { return() }

    recording_block <- local_data$recording_block
    annotation_subset <- annotation_table[annotation_table$block %in% recording_block, ]
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

      update_plot(update = TRUE)
    }),
    input$viewer_start_time,
    input$viewer_duration,
    input$viewer_channel_gap,
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
      later::later(delay = 0.5, function() {

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
    stream_plot_container$render()
  })




}
