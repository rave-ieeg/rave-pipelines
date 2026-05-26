
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
      "electrode_text",
      "analysis_ranges", "condition_groups"
    ))
  )

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "electrode_text", "condition_groups", "analysis_ranges"
      ))

      # Save custom UI inputs so they can be restored on next session load
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
        filter_configurations = unname(fc),
        analysis_ranges = settings$analysis_ranges,
        analysis_electrodes = settings$analysis_electrodes
      )

      dipsaus::shiny_alert2(
        title = "Running Pipeline...",
        text = ravedash::be_patient_text(),
        auto_close = FALSE,
        buttons = FALSE,
        icon = "info",
        session = session
      )

      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2(session = session)
      })

      tryCatch(
        {
          ravepipeline::logger("Scheduled: ", pipeline$pipeline_name,
                               level = "debug", reset_timer = TRUE)

          pipeline$run(
            scheduler = "none",
            type = "smart",
            shortcut = TRUE,
            names = c(
              "settings_path",
              "filter_configurations",
              "condition_groups",
              "analysis_electrodes",

              "settings",
              "condition_groups_clean",
              "analysis_electrodes_clean",
              "analysis_electrode_coordinates",

              "filtered_array",
              "data_placeholder",

              "data_by_channel_condition",
              "data_by_trial_per_condition",
              "data_collapse_by_condition"
            ),
            return_values = FALSE
          )

          ravepipeline::logger("Fulfilled: ", pipeline$pipeline_name,
                               level = "debug")
          shidashi::clear_notifications(
            class = ns("pipeline-error"), session = session)
          local_reactives$update_outputs <- Sys.time()
        },
        error = function(e) {

          ravepipeline::logger_error_condition(e)
          ravedash::error_notification(
            cond = e,
            autohide = FALSE,
            class = "pipeline-error",
            session = session
          )
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
      time_range     = time_range,
      cex            = input$plot_cex %||% 1
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
      time_range         = time_range,
      space              = 1,
      flip_y             = isTRUE(input$mean_erp_flip_y)
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
      vertical_marks     = input$plot_onset_mark %||% 0,
      time_range         = time_range,
      space              = 1,
      flip_y             = isTRUE(input$mean_erp_flip_y)
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
    sort_by <- input$trial_sort_by %OF% c("stimuli", "trial")
    cex <- input$plot_cex %||% 1
    plot_by_trial_per_condition(
      data_by_trial_per_condition,
      sort_by        = sort_by,
      cex            = cex,
      vertical_marks = input$plot_onset_mark %||% 0,
      time_range     = time_range
    )
  })


}
