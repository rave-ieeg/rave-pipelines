
module_server <- function(input, output, session, ...) {


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
      "electrode_text", "baseline_choices",
      "analysis_ranges", "condition_groups"
    ))
  )

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      # Invalidate previous results (stop them because they are no longer needed)
      if (!is.null(local_data$results)) {
        local_data$results$invalidate()
        ravepipeline::logger("Invalidating previous run", level = "trace")
      }


      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "electrode_text", "baseline_choices", "condition_groups", "analysis_ranges"
      ))

      pipeline$set_settings(.list = settings)

      # Save custom UI inputs so they can be restored on next session load
      {
        fc <- list()
        if (isTRUE(input$downsample_enabled)) {
          dec_by <- max(1L, as.integer(input$downsample_factor %||% 4L))
          fc <- c(fc, list(list(type = "decimate", by = dec_by)))
        }
        sig_fcs <- input$signal_filter_configurations
        if (is.list(sig_fcs) && length(sig_fcs)) {
          fc <- c(fc, sig_fcs)
        }
        bl_method <- input$baseline_method %||% "none"
        if (grepl("detrend", bl_method, fixed = TRUE)) {
          fc <- c(fc, list(list(type = "detrend")))
        }
        if (grepl("demean", bl_method, fixed = TRUE)) {
          fc <- c(fc, list(list(type = "demean")))
        }
        if (!identical(bl_method, "none")) {
          bl_win <- input$baseline_window
          if (length(bl_win) == 2L && is.numeric(bl_win)) {
            fc <- c(fc, list(list(type = "baseline", windows = as.numeric(bl_win))))
          }
        }
        pipeline$set_settings(
          condition_groups = input$condition_groups,
          filter_configurations = unname(fc)
        )
      }
      stop("DEBUG")

      #' Run pipeline without blocking the main session
      #' The trick to speed up is to set
      #' `async=TRUE` will run the pipeline in the background
      #' `shortcut=TRUE` will ignore the dependencies and directly run `names`
      #' `names` are the target nodes to run
      #' `scheduler="none"` will try to avoid starting any schedulers and
      #' run targets sequentially. Combined with `callr_function=NULL`,
      #' scheduler's overhead can be removed.
      #' `type="smart"` will start `future` plan in the background, allowing
      #' multicore calculation
      results <- pipeline$run(
        as_promise = TRUE,
        scheduler = "none",
        type = "smart",
        callr_function = NULL,
        progress_title = "Calculating in progress",
        async = TRUE,
        check_interval = 0.1,
        shortcut = TRUE,
        names = c(
          "settings",
          names(settings),
          "requested_electrodes", "analysis_ranges_index", "cond_groups",
          "bl_power", "collapsed_data"
        )
      )


      local_data$results <- results
      ravepipeline::logger("Scheduled: ", pipeline$pipeline_name,
                       level = "debug", reset_timer = TRUE)

      results$promise$then(
        onFulfilled = function(...) {
          ravepipeline::logger("Fulfilled: ", pipeline$pipeline_name,
                           level = "debug")
          shidashi::clear_notifications(class = "pipeline-error")
          local_reactives$update_outputs <- Sys.time()
          return(TRUE)
        },
        onRejected = function(e, ...) {
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
          return(msg)
        }
      )

      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # whether the loaded data is valid and initialize the UI inputs
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if (!loaded_flag) {
        return()
      }
      new_repository <- pipeline$read("repository")
      if (!inherits(new_repository, "rave_prepare_subject_voltage_with_epochs")) {
        ravepipeline::logger(
          "Repository read from the pipeline, but it is not an instance of `rave_prepare_subject_voltage_with_epochs`. Abort initialization",
          level = "warning"
        )
        return()
      }
      ravepipeline::logger(
        "Repository read from the pipeline; initializing the module UI",
        level = "debug"
      )

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if (inherits(old_repository, "rave_prepare_subject_voltage_with_epochs")) {
        if (
          !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature)
        ) {
          ravepipeline::logger(
            "The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization",
            level = "debug",
            use_glue = TRUE
          )
          return()
        }
      }

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository
      component_container$initialize_with_new_data()

      # Restore condition_groups, validated against available epoch conditions
      all_conditions <- sort(unique(new_repository$epoch$table$Condition))
      saved_groups   <- pipeline$get_settings("condition_groups")
      valid_groups   <- NULL
      if (is.list(saved_groups) && length(saved_groups)) {
        valid_groups <- lapply(saved_groups, function(grp) {
          grp$conditions <- intersect(grp$conditions, all_conditions)
          grp
        })
        valid_groups <- Filter(function(grp) length(grp$conditions) > 0L, valid_groups)
      }
      if (!length(valid_groups)) {
        valid_groups <- list(list(label = "All Conditions", conditions = all_conditions))
      }
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = "condition_groups",
        initialization = list(conditions = list(choices = all_conditions)),
        value = valid_groups,
        ncomp = length(valid_groups)
      )


      # Compute epoch time range (used for slider bounds and clamping)
      time_range <- tryCatch(
        range(unlist(new_repository$time_windows), na.rm = TRUE),
        error = function(e) c(-0.5, 1)
      )

      # Read saved filter_configurations from pipeline settings
      filter_configs <- pipeline$get_settings("filter_configurations")
      if (!is.list(filter_configs)) { filter_configs <- list() }
      fc_types <- vapply(filter_configs, function(fc) as.character(fc$type %||% ""), character(1L))

      # Restore baseline_method from filter_configurations
      bl_method_val <- if ("detrend" %in% fc_types && "demean" %in% fc_types) {
        "detrend+demean"
      } else if ("detrend" %in% fc_types) {
        "detrend"
      } else if ("demean" %in% fc_types) {
        "demean"
      } else {
        "none"
      }
      shiny::updateSelectInput(session = session, inputId = "baseline_method",
                               selected = bl_method_val)

      # When bl_method_val is not "none", the baseline_window should be enabled.
      # Otherwise, it should be disabled. Either way, update the baseline_window
      # slider with appropriate values.
      # Restore baseline_window slider (min/max from epoch; value from saved settings)
      bl_entries <- Filter(function(fc) identical(fc$type, "baseline"), filter_configs)
      if (length(bl_entries)) {
        saved_win <- as.numeric(unlist(bl_entries[[1L]]$windows))
        if (length(saved_win) == 2L && all(is.finite(saved_win))) {
          bl_value <- pmax(pmin(saved_win, time_range[[2L]]), time_range[[1L]])
        } else {
          bl_value <- c(time_range[[1L]], min(0, time_range[[2L]]))
        }
      } else {
        bl_value <- c(time_range[[1L]], min(0, time_range[[2L]]))
      }
      shiny::updateSliderInput(
        session = session,
        inputId = "baseline_window",
        min   = time_range[[1L]],
        max   = time_range[[2L]],
        value = bl_value
      )


      # Restore downsample_enabled / downsample_factor
      dec_entries <- Filter(function(fc) identical(fc$type, "decimate"), filter_configs)
      if (length(dec_entries)) {
        dec_factor <- max(1L, as.integer(dec_entries[[1L]]$by %||% 1L))
        shiny::updateCheckboxInput(session = session, inputId = "downsample_enabled", value = TRUE)
        shiny::updateNumericInput(session = session,  inputId = "downsample_factor",  value = dec_factor)
      } else {
        shiny::updateCheckboxInput(session = session, inputId = "downsample_enabled", value = FALSE)
        # Default downsample factor is 1 (no downsampling), which is also the
        # minimum allowed value. Setting it to 1 when disabled ensures that
        # if the user enables it later, it starts with a valid value.
        shiny::updateNumericInput(session = session,  inputId = "downsample_factor",  value = 1L)
      }


      # Restore signal_filter_configurations (FIR/IIR only; excludes meta types)
      meta_types <- c("detrend", "demean", "decimate", "baseline")
      signal_fcs <- Filter(function(fc) !isTRUE(fc$type %in% meta_types), filter_configs)
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = "signal_filter_configurations",
        value   = signal_fcs,
        ncomp   = max(0L, length(signal_fcs))
      )

      # Reset outputs
      shidashi::reset_output("figure_collapse_by_condition")
      shidashi::reset_output("figure_by_channel_condition_cond")
      shidashi::reset_output("figure_by_channel_condition_ch")
      shidashi::reset_output("figure_by_trial_per_condition")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )





  # ---- Nyquist info label (reactive to downsample inputs) ---------------
  output$ui_nyquist_info <- shiny::renderUI({
    repository <- component_container$data$repository
    if (!inherits(repository, "rave_prepare_subject_voltage_with_epoch")) {
      return(NULL)
    }
    sr <- repository$sample_rate
    if (!length(sr) || !is.numeric(sr)) { return(NULL) }

    factor <- input$downsample_factor
    if (!isTRUE(is.numeric(factor) && factor >= 1)) {
      factor <- 1L
    }
    factor <- max(1L, as.integer(factor))
    effective_sr <- sr / factor
    nyquist <- effective_sr / 2
    shiny::tagList(
      shiny::p(
        shiny::strong("Effective sample rate: "),
        sprintf("%.1f Hz", effective_sr),
        shiny::br(),
        shiny::strong("Nyquist: "),
        sprintf("%.1f Hz", nyquist)
      )
    )
  })


  # ---- Helper: check outputs are ready ------------------------------------
  .output_ready <- function() {
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
    shiny::validate(
      shiny::need(
        isTRUE(local_data$results$valid),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )
  }


  # ---- Register outputs ---------------------------------------------------

  # Mean ERP: one line per condition group, collapsed over channels
  output$figure_collapse_by_condition <- shiny::renderPlot({
    .output_ready()
    data_collapse_by_condition <- pipeline$read(var_names = "data_collapse_by_condition")
    shiny::validate(shiny::need(
      inherits(data_collapse_by_condition, "data_collapse_by_condition"),
      message = "No data available"
    ))
    time_range <- c(input$plot_time_start, input$plot_time_end)
    if (!length(time_range) || all(is.na(time_range))) {
      time_range <- c(NA, NA)
    }
    plot_collapse_by_condition(
      data_collapse_by_condition,
      crp_decoration = isTRUE(input$mean_erp_crp),
      flip_y         = isTRUE(input$mean_erp_flip_y),
      vertical_marks = input$plot_onset_mark %||% 0,
      time_range     = time_range
    )
  })


  # ERP by condition: stacked channel traces, one panel per condition group
  output$figure_by_channel_condition_cond <- shiny::renderPlot({
    .output_ready()
    data_by_channel_condition <- pipeline$read(var_names = "data_by_channel_condition")
    shiny::validate(shiny::need(
      inherits(data_by_channel_condition, "data_by_channel_condition"),
      message = "No data available"
    ))
    time_range <- c(input$plot_time_start, input$plot_time_end)
    if (!length(time_range) || all(is.na(time_range))) {
      time_range <- c(NA, NA)
    }
    plot_by_channel_condition(
      data_by_channel_condition,
      group_by           = "condition",
      channel_annotation = input$channel_annotation %||% "number",
      cex                = input$plot_cex %||% 1,
      vertical_marks     = input$plot_onset_mark %||% 0,
      time_range         = time_range
    )
  })


  # ERP by channel: one panel per electrode, condition groups overlaid
  output$figure_by_channel_condition_ch <- shiny::renderPlot({
    .output_ready()
    data_by_channel_condition <- pipeline$read(var_names = "data_by_channel_condition")
    shiny::validate(shiny::need(
      inherits(data_by_channel_condition, "data_by_channel_condition"),
      message = "No data available"
    ))
    time_range <- c(input$plot_time_start, input$plot_time_end)
    if (!length(time_range) || all(is.na(time_range))) {
      time_range <- c(NA, NA)
    }
    ncols <- input$erp_ncols
    if (!isTRUE(is.numeric(ncols) && ncols >= 1)) { ncols <- NULL }
    plot_by_channel_condition(
      data_by_channel_condition,
      group_by           = "channel",
      channel_annotation = input$channel_annotation %||% "number",
      cex                = input$plot_cex %||% 1,
      mfrow              = if (length(ncols)) c(ceiling(data_by_channel_condition$n / ncols), ncols) else NULL,
      vertical_marks     = input$plot_onset_mark %||% 0,
      time_range         = time_range
    )
  })


  # Trial heatmap: time x trial image, one panel per condition group
  output$figure_by_trial_per_condition <- shiny::renderPlot({
    .output_ready()
    data_by_trial_per_condition <- pipeline$read(var_names = "data_by_trial_per_condition")
    shiny::validate(shiny::need(
      inherits(data_by_trial_per_condition, "data_by_trial_per_condition"),
      message = "No data available"
    ))
    time_range <- c(input$plot_time_start, input$plot_time_end)
    if (!length(time_range) || all(is.na(time_range))) {
      time_range <- c(NA, NA)
    }
    sort_by <- input$trial_sort_by %||% "stimuli"
    if (!sort_by %in% c("stimuli", "channel")) { sort_by <- "stimuli" }
    plot_by_trial_per_condition(
      data_by_trial_per_condition,
      sort_by        = sort_by,
      cex            = input$plot_cex %||% 1,
      vertical_marks = input$plot_onset_mark %||% 0,
      time_range     = time_range
    )
  })


}
