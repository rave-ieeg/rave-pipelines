
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
  # server_tools$run_analysis_onchange(
  #   component_container$get_input_ids(c(
  #     "electrode_text", "baseline_choices",
  #     "analysis_ranges", "condition_groups"
  #   ))
  # )

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      repository <- component_container$data$repository

      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "baseline_choices", "condition_groups"
      ))

      time_range <- ravecore::validate_time_window(input$time_range)
      frequency_range <- input$frequency_range

      frequency <- repository$frequency
      frequency <- frequency[frequency >= frequency_range[[1]] & frequency <= frequency_range[[2]]]

      if(!length(frequency)) {
        stop("Frequency range is too narrow: there is no power frequency within that range. Please select a broader range!")
      }

      zeta_threshold <- input$zeta_threshold
      if(length(zeta_threshold) != 1 || is.na(zeta_threshold)) {
        stop("Invalid `zeta` threshold. Please set a zeta threshold within (0, 1)")
      }


      pipeline$set_settings(
        analysis_window = time_range,
        frequency_range = frequency_range,
        zeta_threshold = zeta_threshold,
        .list = settings
      )

      local_data$results <- NULL

      tryCatch(
        {
          ravepipeline::logger("Scheduled: ", pipeline$pipeline_name, level = 'debug', reset_timer = TRUE)
          pipeline$run(
            as_promise = FALSE,
            names = c("clustering_tree", "clustering_index"),
            return_values = FALSE
          )

          local_data$results <- pipeline[c('clustering_tree', 'combined_group_results', 'clustering_index')]

          ravepipeline::logger("Fulfilled: ", pipeline$pipeline_name, level = 'debug')
          shidashi::clear_notifications(class = "pipeline-error")
          local_reactives$update_outputs <- Sys.time()

          # Also update plots
          clustering_index <- pipeline['clustering_index']

          shiny::updateNumericInput(
            session = session,
            inputId = "n_clusters",
            max = max(clustering_index$scores$k),
            value = clustering_index$suggested$k
          )
        },
        error = function(e) {
          msg <- paste(e$message, collapse = "\n")
          if(inherits(e, "error")){
            ravepipeline::logger(msg, level = 'error')
            ravepipeline::logger(traceback(e), level = 'error', .sep = "\n")
            shidashi::show_notification(
              message = msg,
              title = "Error while running pipeline", type = "danger",
              autohide = FALSE, close = TRUE, class = "pipeline-error"
            )
          }
        }
      )

      return()

    }, error_wrapper = "alert"),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_prepare_power")){
        ravepipeline::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_power`. Abort initialization", level = "warning")
        return()
      }
      ravepipeline::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_power")){

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

      local_reactives$update_outputs <- FALSE

      # customized UI update

      # Time range
      full_timerange <- range(unlist(new_repository$time_windows))
      analysis_window <- range(unlist(pipeline['analysis_window']))
      analysis_window[analysis_window < full_timerange[[1]]] <- full_timerange[[1]]
      analysis_window[analysis_window > full_timerange[[2]]] <- full_timerange[[2]]
      shiny::updateSliderInput(
        session = session,
        inputId = "time_range",
        min = full_timerange[[1]],
        max = full_timerange[[2]],
        value = analysis_window
      )

      # Frequency range
      frequency_max_range <- range(new_repository$frequency)
      frequency_range <- range(unlist(pipeline['frequency_range']))
      frequency_range[frequency_range < frequency_max_range[[1]]] <- frequency_max_range[[1]]
      frequency_range[frequency_range > frequency_max_range[[2]]] <- frequency_max_range[[2]]
      shiny::updateSliderInput(
        session = session,
        inputId = "frequency_range",
        min = frequency_max_range[[1]],
        max = frequency_max_range[[2]],
        value = frequency_range
      )

      # Zeta ?
      # zeta_threshold
      zeta_threshold <- as.double(unlist(pipeline['zeta_threshold']))
      if(length(zeta_threshold) != 1 || is.na(zeta_threshold) || zeta_threshold <= 0 || zeta_threshold >= 1) {
        zeta_threshold <- 0.5
      }

      # Reset outputs
      shidashi::reset_output("collapse_over_trial")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      n_clusters <- input$n_clusters
      if(length(n_clusters) == 1 && !is.na(n_clusters)) {
        local_reactives$n_clusters <- n_clusters
      }
    }),
    input$n_clusters,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # Register outputs
  output$channel_cluster_timeseries <- shiny::renderPlot({
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    shiny::validate(
      shiny::need(
        !is.null(local_data$results),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )

    clustering_tree <- local_data$results$clustering_tree
    clustering_index <- local_data$results$clustering_index
    combined_group_results <- local_data$results$combined_group_results

    n_clusters <- local_reactives$n_clusters
    if(length(n_clusters) != 1 || is.na(n_clusters)) {
      n_clusters <- clustering_index$suggested$k %||% 1
    }

    diagnose_cluster(cluster_result = clustering_tree,
                     k = n_clusters,
                     combined_group_results = combined_group_results)

  })


  output$cluster_dendrogram_plot <- shiny::renderPlot({
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    shiny::validate(
      shiny::need(
        !is.null(local_data$results),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )

    clustering_tree <- local_data$results$clustering_tree
    combined_group_results <- local_data$results$combined_group_results
    plot(
      clustering_tree$cluster_object,
      labels = sprintf("ch%d", combined_group_results$electrode_channels)
    )

  })

  output$cluster_silhouette_plot <- shiny::renderPlot({
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    shiny::validate(
      shiny::need(
        !is.null(local_data$results),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )

    clustering_tree <- local_data$results$clustering_tree
    choose_n_clusters(clustering_tree,
                      cluster_range = c(2, max(clustering_tree$cluster_range)),
                      plot = TRUE)

  })


}
