
module_server <- function(input, output, session, ...) {


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweak
  server_tools <- ravedash::get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  # server_tools$run_analysis_onchange(
  #   component_container$get_input_ids(c(
  #     "electrode_text", "baseline_choices",
  #     "analysis_ranges", "condition_groups"
  #   ))
  # )

  # For agents: the clusters of the last run as text lines (people read the
  # plots and the table). `k` defaults to what the plots use
  cluster_summary_lines <- function(k = NULL) {
    results <- local_data$results
    if (!length(results)) { return(NULL) }
    clustering_tree <- results$clustering_tree
    clustering_index <- results$clustering_index
    combined_group_results <- results$combined_group_results

    suggested_k <- clustering_index$suggested$k
    k_source <- "suggested"
    if (length(k) != 1 || is.na(k)) {
      k <- local_reactives$n_clusters
      k_source <- "`n_clusters`"
      if (length(k) != 1 || is.na(k)) {
        k <- suggested_k %||% 1
        k_source <- "suggested"
      }
    }
    channels <- combined_group_results$electrode_channels
    scores <- clustering_index$scores
    silhouette <- sprintf(
      "Silhouette score by k: %s; suggested k=%s",
      paste(sprintf("%d=%.3f", scores$k, scores$silhouette), collapse = ", "),
      paste(suggested_k, collapse = "")
    )
    clusters <- tryCatch(
      stats::cutree(clustering_tree$cluster_object, k = k),
      error = function(e) { e }
    )
    if (inherits(clusters, "error")) {
      return(c(
        sprintf("Cannot cut the tree at k=%s: %s", k, conditionMessage(clusters)),
        silhouette
      ))
    }
    cluster_lines <- vapply(seq_len(k), function(ii) {
      sprintf("cluster %d (n=%d): %s", ii, sum(clusters == ii),
              dipsaus::deparse_svec(channels[clusters == ii]))
    }, "")
    if (length(cluster_lines) > 97) {
      cluster_lines <- c(cluster_lines[seq_len(96)],
                         sprintf("... %d more clusters", length(cluster_lines) - 96))
    }
    c(
      sprintf("Clusters at k=%d (%s) of electrodes %s; groups: %s", k, k_source,
              dipsaus::deparse_svec(channels),
              paste(combined_group_results$group_labels, collapse = ", ")),
      cluster_lines,
      silhouette
    )
  }

  # For agents: the settings `run_analysis` used, from the pipeline settings
  summarize_analysis <- function() {
    settings <- pipeline$get_settings()
    window <- range(unlist(settings$analysis_window))
    baseline_window <- range(unlist(settings$baseline__windows))
    groups <- vapply(settings$condition_groups, function(group) {
      sprintf("%s (%s) [%s to %s]", paste(group$group_name, collapse = ""),
              paste(group$group_conditions, collapse = ", "),
              paste(group$group_start_event, collapse = ""),
              paste(group$group_finish_event, collapse = ""))
    }, "")
    sprintf(
      paste(
        "Clustering done: window %s to %s s; zeta %s; baseline %s to %s s, %s,",
        "%s; groups: %s"
      ),
      window[[1]], window[[2]], settings$zeta_threshold,
      baseline_window[[1]], baseline_window[[2]],
      settings$baseline__unit_of_analysis,
      settings$baseline__global_baseline_choice,
      paste(groups, collapse = "; ")
    )
  }

  # Register event: main pipeline need to run; runs when the run-analysis
  # button is clicked, or through `server_tools$trigger_script("run_analysis")`
  server_tools$set_script(
    "run_analysis",
    description = c(
      "Save the analysis inputs to the pipeline, apply the baseline, and",
      "cluster the electrodes by their voltage responses (ERPs; same as",
      "clicking 'Run Analysis'). It reads `time_range`, `zeta_threshold`,",
      "`baseline_choices__unit_of_analysis`,",
      "`baseline_choices__global_baseline_choice`, `baseline_choices__windows`,",
      "and `condition_groups`. It writes nothing into the subject. On success",
      "it sets `n_clusters` to the suggested k and returns 'Clustering done:",
      "...' with the settings used, then the clusters at the suggested k and",
      "the silhouette score per k. Otherwise it returns 'Clustering did not",
      "run', and `output` gives the error. An invalid input (e.g. an invalid",
      "zeta threshold) also opens an 'Error found!' alert that stays",
      "until someone clicks Confirm: close it with tool `shiny_ui_operate`",
      "(action `close_alert2`). A failing pipeline step shows a toast instead;",
      "script `pipeline_progress` gives its error."
    ),
    {
    # For agents: whether this run refreshed the results
    last <- local_reactives$update_outputs
    ravedash::with_error_alert({
      progress <- ravepipeline::rave_progress(title = "Calculating clusters", max = 4, shiny_auto_close = TRUE)

      progress$inc("Checking inputs...")

      repository <- component_container$data$repository

      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "baseline_choices", "condition_groups"
      ))

      time_range <- ravecore::validate_time_window(input$time_range)

      zeta_threshold <- input$zeta_threshold
      if (length(zeta_threshold) != 1 || is.na(zeta_threshold)) {
        stop("Invalid `zeta` threshold. Please set a zeta threshold within (0, 1)")
      }


      pipeline$set_settings(
        analysis_window = time_range,
        zeta_threshold = zeta_threshold,
        .list = settings
      )

      local_data$results <- NULL

      tryCatch(
        {
          ravepipeline::logger("Scheduled: ", pipeline$pipeline_name, level = "debug", reset_timer = TRUE)

          progress$inc("Applying baseline...")

          pipeline$run(
            as_promise = FALSE,
            names = c("baseline_voltage"),
            return_values = FALSE
          )

          progress$inc("Clustering...")

          pipeline$run(
            as_promise = FALSE,
            names = c("clustering_tree", "clustering_index"),
            return_values = FALSE
          )

          progress$inc("Done.")

          local_data$results <- pipeline[c("clustering_tree", "combined_group_results", "clustering_index")]

          ravepipeline::logger("Fulfilled: ", pipeline$pipeline_name, level = "debug")
          shidashi::clear_notifications(class = "pipeline-error")
          local_reactives$update_outputs <- Sys.time()

          # Also update plots
          clustering_index <- pipeline["clustering_index"]

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
          if (inherits(e, "error")) {
            ravepipeline::logger(msg, level = "error")
            ravepipeline::logger(traceback(e), level = "error", .sep = "\n")
            shidashi::show_notification(
              message = msg,
              title = "Error while running pipeline", type = "danger",
              autohide = FALSE, close = TRUE, class = "pipeline-error"
            )
          }
        }
      )

      return()
    })
    updated <- local_reactives$update_outputs
    if (identical(last, updated) || isFALSE(updated) || !length(local_data$results)) {
      "Clustering did not run: see the error in `output`."
    } else {
      tryCatch(
        c(summarize_analysis(), cluster_summary_lines(
          k = local_data$results$clustering_index$suggested$k)),
        error = function(e) "Clustering done."
      )
    }
    }
  )

  # Read-only: the clusters as text, for agents (people read the 'Clustering
  # table' and 'Diagnostic plots' tabs)
  server_tools$set_script(
    "cluster_summary",
    description = c(
      "Read-only. The clusters of the last `run_analysis`, cut at the k the",
      "plots use (input `n_clusters`): a header with k, the electrodes, and",
      "the condition groups; one line per cluster, 'cluster <i> (n=<count>):",
      "<electrodes>' (as in the 'Clustering table' tab); and the silhouette",
      "score per k with the suggested k (as in the silhouette plot). After",
      "`load_data`, or a run that failed, there are no results (the plots then",
      "ask for a run): run `run_analysis` again."
    ),
    {
      # Same condition as the plots: no results after a load or a failed run
      updated <- local_reactives$update_outputs
      if (!length(updated) || isFALSE(updated)) {
        return("No results yet: run `run_analysis` first.")
      }
      lines <- cluster_summary_lines()
      if (!length(lines)) {
        return("No results yet: run `run_analysis` first.")
      }
      lines
    }
  )

  # Read-only: lets agents read why a pipeline target failed (same as in the
  # power explorer module)
  server_tools$set_script(
    name = "pipeline_progress",
    description = c(
      "Read-only. Progress of the latest pipeline run, one line per target:",
      "'<target>: <progress>' (dispatched, completed, errored, skipped,",
      "canceled), with the error message of errored targets, and when the",
      "progress last changed. Run it after `run_analysis` says 'Clustering did",
      "not run' without saying why."
    ),
    expr = {
      progress <- as.data.frame(pipeline$progress("details"))
      if (!nrow(progress)) {
        return("The pipeline has not run yet.")
      }
      re <- sprintf("%s: %s", progress$name, progress$progress)
      errored <- progress$progress == "errored"
      if (any(errored)) {
        errors <- as.data.frame(pipeline$with_activated(
          targets::tar_meta(fields = "error", complete_only = TRUE)
        ))
        messages <- errors$error[match(progress$name[errored], errors$name)]
        re[errored] <- paste(re[errored], "-", messages)
      }
      since <- as.data.frame(pipeline$progress("summary"))$since
      c(re, sprintf("(progress last changed %s)", since))
    }
  )


  initialize_inputs <- function() {
    loaded_flag <- ravedash::watch_data_loaded()
    if (!loaded_flag) { return() }

    new_repository <- pipeline$read("repository")

    # Reset preset UI & data
    component_container$reset_data()
    component_container$data$repository <- new_repository
    component_container$initialize_with_new_data()

    # customized UI update

    # Time range
    full_timerange <- range(unlist(new_repository$time_windows))
    analysis_window <- range(unlist(pipeline$get_settings("analysis_window")))
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

    # Zeta ?
    # zeta_threshold
    zeta_threshold <- as.double(unlist(pipeline$get_settings("zeta_threshold")))
    if (length(zeta_threshold) != 1 || is.na(zeta_threshold) || zeta_threshold <= 0 || zeta_threshold >= 1) {
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
      if (!loaded_flag) { return() }
      new_repository <- pipeline$read("repository")
      if (!inherits(new_repository, "rave_prepare_subject_voltage_with_epochs")) {
        ravepipeline::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_subject_voltage_with_epochs`. Abort initialization", level = "warning")
        return()
      }
      ravepipeline::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if (inherits(old_repository, "rave_prepare_subject_voltage_with_epochs")) {

        if ( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ) {
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
      if (length(n_clusters) == 1 && !is.na(n_clusters)) {
        local_reactives$n_clusters <- n_clusters
      }
    }),
    input$n_clusters,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  brain_proxy <- threeBrain::brain_proxy(outputId = "viewer", session = session)
  shiny::bindEvent(
    ravedash::safe_observe({
      if (!ravedash::watch_data_loaded()) { return() }
      if (ravedash::watch_loader_opened()) { return() }
      if (!length(local_reactives$update_outputs) || isFALSE(local_reactives$update_outputs)) { return() }
      if (!length(local_data$results)) { return()}

      n_clusters <- local_reactives$n_clusters
      if (length(n_clusters) != 1 || is.na(n_clusters) || n_clusters <= 0) { return() }

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
        "condition_groups",
        "baseline__windows",
        "baseline__unit_of_analysis",
        "baseline__global_baseline_choice",
        "analysis_window",
        "zeta_threshold"
      )]

      if (length(nms)) {
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
    if (length(n_clusters) != 1 || is.na(n_clusters)) {
      n_clusters <- clustering_index$suggested$k %||% 1
    }

    diagnose_cluster(cluster_result = clustering_tree,
                     k = n_clusters,
                     combined_group_results = combined_group_results)

  }, res = 108)

  output$viewer <- threeBrain::renderBrain({

    if (!ravedash::watch_data_loaded()) { return() }
    local_reactives$render_brain

    repository <- component_container$data$repository
    if (!length(repository)) { return() }
    brain <- ravecore::rave_brain(repository$subject)
    if (is.null(brain)) { return("No 3D model") }

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
    if (length(n_clusters) != 1 || is.na(n_clusters)) {
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

    if (n_clusters >= 2 && n_clusters <= length(hclust_object$height)) {
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
    if (length(n_clusters) != 1 || is.na(n_clusters)) {
      n_clusters <- clustering_index$suggested$k %||% 1
    }

    if (isTRUE(n_clusters %in% clustering_index$scores$k)) {
      abline(v = n_clusters, lty = 2, col = 2)
    }


  })

  shiny::bindEvent(
    ravedash::safe_observe({
      click <- input$cluster_silhouette_plot_click
      if (!is.list(click)) { return() }
      click_x <- click$x
      if (length(click_x) != 1 || is.na(click_x)) { return() }
      click_x <- round(click_x)
      if (click_x < 0) { return() }
      if (isTRUE(click_x == input$n_clusters)) { return() }
      shiny::updateNumericInput(
        session = session,
        input = "n_clusters",
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
    if (length(n_clusters) != 1 || is.na(n_clusters)) {
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

    zlim <- unname(quantile(abs(mean_responses), 0.995, na.rm = TRUE)) * 2
    ytick_at <- zlim * rev(seq_len(ncol(mean_responses)))

    matplot(
      x = seq_len(n_timepoints),
      y = sweep(mean_responses, MARGIN = 2L, STATS = ytick_at, FUN = "+"),
      type = "l",
      lty = 1,
      lwd = 1,
      col = threeBrain:::DEFAULT_COLOR_DISCRETE,
      xlab = "Time",
      ylab = bquote("Voltage Mean (" ~ mu ~ "V)"),
      main = sprintf("Cluster mean responses (k=%d)", n_clusters),
      axes = FALSE,
      xaxs = "i"
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

    # axis(2L, at = pretty(mean_responses), las = 1, tick = TRUE)
    axis(2L, at = ytick_at, labels = sprintf("cl=%02d\n(n=%d)", seq_along(ytick_at), n_channels), las = 1, tick = TRUE, cex.axis = 0.75)
    abline(h = ytick_at, lty = 2, col = "#7F7F7F7F")

    # legend("topright", sprintf("cluster=%d (n=%d)", seq_len(n_clusters), n_channels), lty = 1, col = threeBrain:::DEFAULT_COLOR_DISCRETE, box.col = NA)

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
    if (length(n_clusters) != 1 || is.na(n_clusters)) {
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
