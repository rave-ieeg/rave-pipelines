
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

      progress <- ravepipeline::rave_progress(title = "Calculating clusters", max = 4, shiny_auto_close = TRUE)

      progress$inc("Checking inputs...")

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

          progress$inc("Applying baseline...")

          pipeline$run(
            as_promise = FALSE,
            names = c("baseline_power"),
            return_values = FALSE
          )

          progress$inc("Clustering...")

          pipeline$run(
            as_promise = FALSE,
            names = c("clustering_tree", "clustering_index"),
            return_values = FALSE
          )

          progress$inc("Done.")

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
          local_reactives$update_outputs <- FALSE
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


  initialize_inputs <- function() {
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }

    new_repository <- pipeline$read("repository")

    # Reset preset UI & data
    component_container$reset_data()
    component_container$data$repository <- new_repository
    component_container$initialize_with_new_data()

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
      value = analysis_window,
      step = 0.1
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
      value = frequency_range,
      step = 1
    )

    # Zeta ?
    # zeta_threshold
    zeta_threshold <- as.double(unlist(pipeline['zeta_threshold']))
    if(length(zeta_threshold) != 1 || is.na(zeta_threshold) || zeta_threshold <= 0 || zeta_threshold >= 1) {
      zeta_threshold <- 0.5
    }
    shiny::updateSliderInput(
      session = session,
      inputId = "zeta_threshold",
      value = zeta_threshold
    )


  }

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
      initialize_inputs()

      local_reactives$update_outputs <- FALSE
      local_reactives$render_brain <- Sys.time()

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


  brain_proxy <- threeBrain::brain_proxy(outputId = "viewer", session = session)
  shiny::bindEvent(
    ravedash::safe_observe({
      if(!ravedash::watch_data_loaded()) { return() }
      if(ravedash::watch_loader_opened()) { return() }
      if(!length(local_reactives$update_outputs) || isFALSE(local_reactives$update_outputs)) { return() }
      if(!length(local_data$results)) { return()}

      n_clusters <- local_reactives$n_clusters
      if(length(n_clusters) != 1 || is.na(n_clusters) || n_clusters <= 0) { return() }

      clustering_tree <- local_data$results$clustering_tree
      combined_group_results <- local_data$results$combined_group_results

      clusters <- cutree(clustering_tree$cluster_object, k = n_clusters)

      value_table <- data.frame(
        Electrode = combined_group_results$electrode_channels,
        Loaded = TRUE,
        Cluster = factor(sprintf("class% 3d", clusters), levels = sprintf("class% 3d", seq_len(n_clusters)))
      )

      brain_proxy$set_electrode_data(value_table, palettes = list("Cluster" = threeBrain:::DEFAULT_COLOR_DISCRETE), clear_first = TRUE, update_display = TRUE)

    }),
    local_reactives$n_clusters,
    local_reactives$update_outputs,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )




  # Register outputs
  output$btn_download_settings <- shiny::downloadHandler(
    filename = "pipeline-power_clust-settings.yaml",
    content = function(con) {
      ravepipeline::save_yaml(x = pipeline$get_settings(),
                              file = con,
                              sorted = TRUE)
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::showModal(shiny::modalDialog(
        title = "Load settings",
        size = "m",
        dipsaus::fancyFileInput(
          inputId = ns("uploader_settings"),
          label = NULL,
          size = "m", width = "100%"
        )
      ))
    }),
    input$btn_load_settings,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      datapath <- input$uploader_settings$datapath
      settings <- ravepipeline::load_yaml(datapath)

      current_settings <- pipeline$get_settings()
      nms <- names(settings)
      nms <- nms[nms %in% names(current_settings)]
      nms <- nms[nms %in% c(
        "frequency_range",
        "condition_groups",
        "baseline__windows",
        "baseline__unit_of_analysis",
        "baseline__global_baseline_choice",
        "analysis_window",
        "zeta_threshold"
      )]

      if(length(nms)) {
        pipeline$set_settings(.list = settings[nms])
        initialize_inputs()
      }

      shiny::removeModal(session = session)

    }, error_wrapper = "notification"),
    input$uploader_settings,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

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

  }, res = 108)

  output$viewer <- threeBrain::renderBrain({

    if(!ravedash::watch_data_loaded()) { return() }
    local_reactives$render_brain

    repository <- component_container$data$repository
    if(!length(repository)) { return() }
    brain <- ravecore::rave_brain(repository$subject)
    if(is.null(brain)) { return("No 3D model") }

    brain$set_electrode_values(data.frame(
      Electrode = repository$electrode_list,
      Loaded = TRUE
    ))

    brain$plot()

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
    clustering_index <- local_data$results$clustering_index
    combined_group_results <- local_data$results$combined_group_results

    n_clusters <- local_reactives$n_clusters
    if(length(n_clusters) != 1 || is.na(n_clusters)) {
      n_clusters <- clustering_index$suggested$k %||% 1
    }

    hclust_object <- clustering_tree$cluster_object
    channel_names <- sprintf("Ch% 4d", combined_group_results$electrode_channels)

    plot(
      hclust_object,
      labels = channel_names,
      hang = -1,
      cex = ifelse(length(channel_names) > 10, 0.8, 1)
    )

    if(n_clusters >= 2 && n_clusters <= length(hclust_object$height)) {
      rect_hclust2(hclust_object, n_clusters)
    }


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

    clustering_index <- choose_n_clusters(
      cluster_result = clustering_tree,
      cluster_range = c(2, max(clustering_tree$cluster_range)),
      plot = TRUE
    )

    n_clusters <- local_reactives$n_clusters
    if(length(n_clusters) != 1 || is.na(n_clusters)) {
      n_clusters <- clustering_index$suggested$k %||% 1
    }

    if(isTRUE(n_clusters %in% clustering_index$scores$k)) {
      abline(v = n_clusters, lty = 2, col = 2)
    }


  })

  shiny::bindEvent(
    ravedash::safe_observe({
      click <- input$cluster_silhouette_plot_click
      if(!is.list(click)) { return() }
      click_x <- click$x
      if(length(click_x) != 1 || is.na(click_x)) { return() }
      click_x <- round(click_x)
      if(click_x < 0) { return() }
      if(isTRUE(click_x == input$n_clusters)) { return() }
      shiny::updateNumericInput(
        session = session,
        input = 'n_clusters',
        value = click_x
      )
    }),
    input$cluster_silhouette_plot_click,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  output$cluster_mean_plot <- shiny::renderPlot({

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

    clusters <- cutree(clustering_tree$cluster_object, k = n_clusters)

    n_timepoints <- nrow(combined_group_results$combined_average_responses)

    mean_responses <- lapply(seq_len(n_clusters), function(ii) {
      cluster_responses <- combined_group_results$combined_average_responses[, clusters == ii, drop = FALSE]
      mean_responses <- rowMeans(cluster_responses)
      mean_responses
    })

    n_channels <- sapply(seq_len(n_clusters), function(ii) { sum(clusters == ii) })

    mean_responses <- do.call("cbind", mean_responses)

    matplot(
      x = seq_len(n_timepoints),
      y = mean_responses,
      type = "l",
      lty = 1,
      lwd = 1,
      col = threeBrain:::DEFAULT_COLOR_DISCRETE,
      xlab = "Time",
      ylab = "Baseline-corrected power",
      main = sprintf("Cluster mean responses (k=%d)", n_clusters),
      axes = FALSE
    )

    group_n_time_points <- combined_group_results$group_n_time_points
    group_start_offset <- combined_group_results$group_start_offset

    group_finish <- cumsum(group_n_time_points)
    group_separator <- c(0, group_finish)
    group_start <- group_separator[-length(group_separator)]
    group_center <- group_finish - group_n_time_points / 2
    axis(1L, at = group_separator, labels = rep("", length(group_separator)), tick = TRUE)

    # start_events <- combined_group_results$group_event_starts
    # start_events[tolower(start_events) %in% c("", "trial onset", "trial_onset")] <- "TrialOnset"
    group_start_offset_labels <- sprintf("%.2f s", group_start_offset)
    group_start_offset_labels[group_start_offset == 0] <- "0 s"
    axis(1L, at = group_start, labels = group_start_offset_labels, tick = FALSE, hadj = -0.1, cex.axis = 0.8, line = -1)

    durations <- sprintf("%.2f s", group_start_offset + group_n_time_points / combined_group_results$sample_rate)
    axis(1L, at = group_finish, labels = durations, tick = FALSE, hadj = 1.1, cex.axis = 0.8, line = -1)

    axis(
      1L,
      at = group_center,
      labels = sprintf("%s [gID=%d]", combined_group_results$group_labels, combined_group_results$group_indexes),
      tick = FALSE, line = 0
    )

    abline(v = group_start[-1])

    axis(2L, at = pretty(mean_responses), las = 1, tick = TRUE)
    abline(h = 0, lty = 2, col = "#7F7F7F7F")

    legend("topright", sprintf("cluster=%d (n=%d)", seq_len(n_clusters), n_channels), lty = 1, col = threeBrain:::DEFAULT_COLOR_DISCRETE, box.col = NA)

  })

  output$cluster_table <- shiny::renderTable({
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

    clusters <- cutree(clustering_tree$cluster_object, k = n_clusters)


    res <- data.table::rbindlist(lapply(seq_len(n_clusters), function(ii) {
      channels <- combined_group_results$electrode_channels[clusters == ii]

      list(
        Cluster = ii,
        "Number of Channels" = length(channels),
        "Channels" = dipsaus::deparse_svec(channels)
      )
    }))

    res

  })

}
