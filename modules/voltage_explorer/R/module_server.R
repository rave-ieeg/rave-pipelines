
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

      if (!ravedash::watch_data_loaded()) { return() }

      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "electrode_text", "condition_groups", "analysis_ranges"
      ))

      # Save custom UI inputs so they can be restored on next session load
      # Enforced order: Detrend -> pre-Downsample -> FIR/IIR filters -> post-Downsample -> Baseline
      fc <- get_filter_configurations()

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
              "data_by_trial_channel_condition"
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


      # # Compute epoch time range (used for slider bounds and clamping)
      # time_range <- tryCatch(
      #   range(unlist(new_repository$time_windows), na.rm = TRUE),
      #   error = function(e) c(-0.5, 1)
      # )
      #
      # # Read saved filter_configurations from pipeline settings
      # filter_configs <- pipeline$get_settings("filter_configurations")
      # if (!is.list(filter_configs)) { filter_configs <- list() }
      # fc_types <- vapply(filter_configs, function(fc) as.character(fc$type %||% ""), character(1L))
      #
      # # Restore remove_drift_method from filter_configurations
      # drift_method_val <- if ("detrend" %in% fc_types && "demean" %in% fc_types) {
      #   "detrend+demean"
      # } else if ("detrend" %in% fc_types) {
      #   "detrend"
      # } else if ("demean" %in% fc_types) {
      #   "demean"
      # } else {
      #   "none"
      # }
      # shiny::updateSelectInput(session = session, inputId = "remove_drift_method",
      #                          selected = drift_method_val)
      #
      # # Restore enable_baseline_method checkbox and baseline_window slider
      # bl_entries <- Filter(function(fc) identical(fc$type, "baseline"), filter_configs)
      # shiny::updateCheckboxInput(session = session, inputId = "enable_baseline_method",
      #                            value = length(bl_entries) > 0L)
      # if (length(bl_entries)) {
      #   saved_win <- as.numeric(unlist(bl_entries[[1L]]$windows))
      #   if (length(saved_win) == 2L && all(is.finite(saved_win))) {
      #     bl_value <- pmax(pmin(saved_win, time_range[[2L]]), time_range[[1L]])
      #   } else {
      #     bl_value <- c(time_range[[1L]], min(0, time_range[[2L]]))
      #   }
      # } else {
      #   bl_value <- c(time_range[[1L]], min(0, time_range[[2L]]))
      # }
      # shiny::updateSliderInput(
      #   session = session,
      #   inputId = "baseline_window",
      #   min   = time_range[[1L]],
      #   max   = time_range[[2L]],
      #   value = bl_value
      # )
      #
      #
      # # Split decimate entries into pre-filter and post-filter by their position
      # # relative to signal filter entries in the ordered config list.
      # signal_fc_idxs  <- which(!fc_types %in% c("detrend", "demean", "decimate", "baseline"))
      # first_signal_idx <- if (length(signal_fc_idxs)) min(signal_fc_idxs) else length(fc_types) + 1L
      # decimate_idxs    <- which(fc_types == "decimate")
      # pre_dec_entries  <- filter_configs[decimate_idxs[decimate_idxs < first_signal_idx]]
      # post_dec_entries <- filter_configs[decimate_idxs[decimate_idxs >= first_signal_idx]]
      #
      # # Restore pre_downsample_factor
      # if (length(pre_dec_entries)) {
      #   shiny::updateNumericInput(session = session,  inputId = "pre_downsample_factor",
      #                             value = max(1L, as.integer(pre_dec_entries[[1L]]$by %||% 1L)))
      # } else {
      #   shiny::updateNumericInput(session = session,  inputId = "pre_downsample_factor",  value = 1L)
      # }
      #
      # # Restore post_downsample_factor
      # if (length(post_dec_entries)) {
      #   shiny::updateNumericInput(session = session,  inputId = "post_downsample_factor",
      #                             value = max(1L, as.integer(post_dec_entries[[1L]]$by %||% 1L)))
      # } else {
      #   shiny::updateNumericInput(session = session,  inputId = "post_downsample_factor",  value = 1L)
      # }
      #
      #
      # # Restore signal_filter_configurations (FIR/IIR only; excludes meta types)
      # meta_types <- c("detrend", "demean", "decimate", "baseline")
      # signal_fcs <- Filter(function(fc) !isTRUE(fc$type %in% meta_types), filter_configs)
      # dipsaus::updateCompoundInput2(
      #   session = session,
      #   inputId = "signal_filter_configurations",
      #   value   = signal_fcs,
      #   ncomp   = max(0L, length(signal_fcs))
      # )


      update_filter_configurations()

      # Reset outputs
      shidashi::reset_output("figure_by_condition_over_time")
      shidashi::reset_output("figure_by_channel_condition_cond")
      shidashi::reset_output("figure_by_channel_condition_ch")
      shidashi::reset_output("figure_by_trial_per_condition")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )


  # ---- Graphics options ---------------
  get_cex <- shiny::reactive({
    if (isTRUE(input$plot_cex > 0)) {
      cex <- use_cex(input$plot_cex)
    } else {
      cex <- use_cex()
    }
    cex
  })

  get_channel_annotation_style <- shiny::reactive({
    if (length(input$channel_annotation) > 0) {
      cex <- use_channel_annotation_style(input$channel_annotation)
    } else {
      cex <- use_channel_annotation_style()
    }
    cex
  })

  get_trial_sort_by <- shiny::reactive({
    if (length(input$trial_sort_by) > 0) {
      trial_sort_by <- use_trial_sort_by(input$trial_sort_by)
    } else {
      trial_sort_by <- use_trial_sort_by()
    }
    trial_sort_by
  })

  get_flipped_y <- shiny::reactive({
    if (length(input$mean_erp_flip_y) > 0) {
      flip_y <- use_flipped_y(input$mean_erp_flip_y)
    } else {
      flip_y <- use_flipped_y()
    }
    flip_y
  })

  get_show_crp_decoration <- shiny::reactive({
    if (length(input$mean_erp_crp) > 0) {
      show_crp <- use_show_crp_decoration(input$mean_erp_crp)
    } else {
      show_crp <- use_show_crp_decoration()
    }
    show_crp
  })

  # ---- Nyquist info label (reactive to downsample inputs) ---------------

  pre_decimate_factor <- shiny::reactive({
    if (!isTRUE(ravedash::watch_data_loaded())) { return(1L) }

    factor <- max(c(1L, as.integer(input$pre_downsample_factor %||% 1L)), na.rm = TRUE)

    if (!isTRUE(input$pre_downsample_factor_auto)) {
      return(factor)
    }

    repository <- component_container$data$repository
    if (!inherits(repository, "rave_prepare_subject_voltage_with_epochs")) {
      return(1L)
    }
    sr <- repository$sample_rates$LFP
    if (!length(sr) || !is.numeric(sr)) { return(1L) }

    sr <- sr[[1]]

    # If the passing band is not enabled, down-sample to 1000
    # Nyquist > 500 Hz, way enough for most ERP
    if (!isTRUE(input$passing_filter_enabled)) {
      return(max(floor(sr / 1000), 1L))
    }

    # Say low-pass 100Hz, then down-sample to around 300~600Hz
    cutoff_freq <- input$passing_freq1
    if (isTRUE(input$passing_filter_type %in% c("band_pass"))) {
      cutoff_freq <- c(input$passing_freq1, input$passing_freq2)
    }
    cutoff_freq <- max(c(cutoff_freq, 300), na.rm = TRUE)
    max_freq <- ceiling(cutoff_freq * 3)

    return(max(floor(sr / max_freq), 1L))

  })

  post_decimate_factor <- shiny::reactive({
    if (!isTRUE(ravedash::watch_data_loaded())) { return(1L) }

    factor <- max(c(1L, as.integer(input$post_downsample_factor %||% 1L)), na.rm = TRUE)

    if (!isTRUE(input$post_downsample_factor_auto)) {
      return(factor)
    }

    repository <- component_container$data$repository
    if (!inherits(repository, "rave_prepare_subject_voltage_with_epochs")) {
      return(1L)
    }
    sr <- repository$sample_rates$LFP
    if (!length(sr) || !is.numeric(sr)) { return(1L) }

    sr <- sr[[1]] / pre_decimate_factor()

    # If the passing band is not enabled, down-sample to 100
    if (!isTRUE(input$passing_filter_enabled)) {
      return(max(floor(sr / 100), 1L))
    }

    if (isTRUE(input$passing_filter_type %in% c("high_pass"))) {
      return(1L)
    }

    # Say low-pass 100Hz, then down-sample to around 300~600Hz
    cutoff_freq <- input$passing_freq1
    if (isTRUE(input$passing_filter_type %in% c("band_pass"))) {
      cutoff_freq <- c(input$passing_freq1, input$passing_freq2)
    }
    cutoff_freq <- max(c(cutoff_freq, 33), na.rm = TRUE)
    max_freq <- ceiling(cutoff_freq * 3)

    return(max(floor(sr / max_freq), 1L))
  })

  output$pre_ui_nyquist_info <- shiny::bindEvent(
    shiny::renderText({
      if (!isTRUE(ravedash::watch_data_loaded())) { return() }
      if (isTRUE(ravedash::watch_loader_opened())) { return() }

      factor <- pre_decimate_factor()
      if (!isTRUE(factor > 1)) { return() }

      repository <- component_container$data$repository
      if (!inherits(repository, "rave_prepare_subject_voltage_with_epochs")) {
        return()
      }
      sr <- repository$sample_rates$LFP
      if (!length(sr) || !is.numeric(sr)) { return() }

      sr <- sr[[1]]

      effective_sr <- sr / factor
      nyquist <- effective_sr / 2

      return(
        sprintf(
          "Effective sample rate before filters: %.1f Hz (Nyquist = %.1f Hz)",
          effective_sr,
          effective_sr / 2
        )
      )
    }),
    ravedash::watch_data_loaded(),
    pre_decimate_factor(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  output$post_ui_nyquist_info <- shiny::bindEvent(
    shiny::renderText({
      if (!isTRUE(ravedash::watch_data_loaded())) { return() }
      if (isTRUE(ravedash::watch_loader_opened())) { return() }

      factor <- post_decimate_factor()
      if (!isTRUE(factor > 1)) { return() }

      repository <- component_container$data$repository
      if (!inherits(repository, "rave_prepare_subject_voltage_with_epochs")) {
        return()
      }
      sr <- repository$sample_rates$LFP
      if (!length(sr) || !is.numeric(sr)) { return() }

      sr <- sr[[1]] / pre_decimate_factor()

      effective_sr <- sr / factor
      nyquist <- effective_sr / 2

      return(
        sprintf(
          "Effective sample rate for visualization: %.1f Hz (Nyquist = %.1f Hz)",
          effective_sr,
          effective_sr / 2
        )
      )
    }),
    ravedash::watch_data_loaded(),
    pre_decimate_factor(),
    post_decimate_factor(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  get_filter_configurations <- function() {
    if (!isTRUE(ravedash::watch_data_loaded())) { return(list()) }

    repository <- component_container$data$repository
    if (!inherits(repository, "rave_prepare_subject_voltage_with_epochs")) {
      return(list())
    }
    sr <- repository$sample_rates$LFP
    if (!length(sr) || !is.numeric(sr)) { return(list()) }
    sr <- sr[[1]]
    nyquist <- sr / 2


    fc <- list()

    # Detrend/mean
    drift_method <- input$remove_drift_method %||% "none"
    if (grepl("detrend", drift_method, fixed = TRUE)) {
      fc[[length(fc) + 1]] <- list(type = "detrend")
    }
    if (grepl("demean", drift_method, fixed = TRUE)) {
      fc[[length(fc) + 1]] <- list(type = "demean")
    }

    # pre-decimate
    pre_decimate_fct <- pre_decimate_factor()
    fc[[length(fc) + 1]] <- list(
      type = "decimate",
      by = pre_decimate_fct,

      # Not used by pipeline, but a flag indicating whether this should be
      # automatically set
      auto = isTRUE(input$pre_downsample_factor_auto)
    )
    nyquist <- nyquist / pre_decimate_fct

    if (isTRUE(input$passing_filter_enabled)) {
      switch (
        paste(input$passing_filter_type %||% "", collapse = ""),
        "band_pass" = {
          freq <- c(input$passing_freq1 %||% NA, input$passing_freq2 %||% NA)
          if (anyNA(freq)) {
            stop("Please enter both low-pass and high-pass frequencies for band-pass filters")
          }
          if (!all(freq > 0 & freq < nyquist)) {
            stop(sprintf("Cutoff frequencies must be within 0 ~ Nyquist (%.1f Hz)", nyquist))
          }
          freq <- sort(freq)

          fc[[length(fc) + 1]] <- list(
            type = input$passing_filter_method,
            high_pass_freq = freq[[1]],
            low_pass_freq = freq[[2]]
          )
        },
        "low_pass" = {
          freq <- input$passing_freq1 %||% NA
          if (!isTRUE(freq > 0 & freq < nyquist)) {
            stop(sprintf("Cutoff frequency must be within 0 ~ Nyquist (%.1f Hz)", nyquist))
          }
          fc[[length(fc) + 1]] <- list(
            type = input$passing_filter_method,
            low_pass_freq = freq
          )
        },
        "high_pass" = {
          freq <- input$passing_freq1 %||% NA
          if (!isTRUE(freq > 0 & freq < nyquist)) {
            stop(sprintf("Cutoff frequency must be within 0 ~ Nyquist (%.1f Hz)", nyquist))
          }
          fc[[length(fc) + 1]] <- list(
            type = input$passing_filter_method,
            high_pass_freq = freq
          )
        },
        {
          # Default do nothing
        }
      )
    }

    if (isTRUE(input$bandstop_filter_enabled)) {
      ranges <- paste(input$bandstop_filter_ranges, collapse = " ")
      # ranges <- "59-61, "
      ranges <- trimws(strsplit(ranges, split = ",")[[1]])
      ranges <- ranges[grepl("[0-9\ ]+-[0-9\ ]+", ranges)]

      for (rg in ranges) {
        rg <- trimws(strsplit(rg, "-")[[1]])
        if (length(rg) == 2) {
          freq <- sort(as.numeric(rg))

          if (!all(freq > 0 & freq < nyquist)) {
            next
          }
          fc[[length(fc) + 1]] <- list(
            type = "firls",
            high_pass_freq = freq[[2]],
            low_pass_freq = freq[[1]]
          )
        }
      }
    }

    # post-decimate
    post_decimate_fct <- post_decimate_factor()
    fc[[length(fc) + 1]] <- list(
      type = "decimate",
      by = post_decimate_fct,

      # Not used by pipeline, but a flag indicating whether this should be
      # automatically set
      auto = isTRUE(input$post_downsample_factor_auto)
    )
    nyquist <- nyquist / post_decimate_fct

    # Baseline
    if (isTRUE(input$enable_baseline_method) && length(input$baseline_window) == 2) {
      fc[[length(fc) + 1]] <- list(
        type = "baseline",
        windows = input$baseline_window
      )
    }

    return(unname(fc))
  }

  update_filter_configurations <- function(filter_configs = NULL) {

    if (!isTRUE(ravedash::watch_data_loaded())) { return() }

    # Must call after others are initialized
    repository <- component_container$data$repository

    # Compute epoch time range (used for slider bounds and clamping)
    time_range <- range(unlist(repository$time_windows), na.rm = TRUE)


    # Read saved filter_configurations from pipeline settings
    if (is.null(filter_configs)) {
      filter_configs <- pipeline$get_settings("filter_configurations")
    }
    if (!is.list(filter_configs) || !length(filter_configs)) { filter_configs <- list() }

    fc_types <- vapply(filter_configs, function(fc) as.character(fc$type %||% ""), character(1L))

    # Restore remove_drift_method from filter_configurations
    drift_method_val <- if ("detrend" %in% fc_types && "demean" %in% fc_types) {
      "detrend+demean"
    } else if ("detrend" %in% fc_types) {
      "detrend"
    } else if ("demean" %in% fc_types) {
      "demean"
    } else {
      "none"
    }
    shiny::updateSelectInput(session = session, inputId = "remove_drift_method",
                             selected = drift_method_val)

    # Restore enable_baseline_method checkbox and baseline_window slider
    bl_idx <- which(fc_types == "baseline")
    if (length(bl_idx)) {
      bl_entry <- filter_configs[[bl_idx[[1]]]]
      bl_window <- range(unlist(bl_entry$windows), na.rm = TRUE)

      shiny::updateCheckboxInput(session = session,
                                 inputId = "enable_baseline_method",
                                 value = TRUE)
      shiny::updateSliderInput(
        session = session,
        inputId = "baseline_window",
        min   = time_range[[1L]],
        max   = time_range[[2L]],
        value = bl_window
      )
    } else {
      shiny::updateCheckboxInput(session = session,
                                 inputId = "enable_baseline_method",
                                 value = FALSE)
      shiny::updateSliderInput(
        session = session,
        inputId = "baseline_window",
        min   = time_range[[1L]],
        max   = time_range[[2L]],
        value = sort(c(time_range[[1L]], min(0, time_range[[2L]])))
      )
    }


    # Split decimate entries into pre-filter and post-filter by their position
    # relative to signal filter entries in the ordered config list.
    signal_fc_idxs  <- which(!fc_types %in% c("detrend", "demean", "decimate", "baseline"))
    first_signal_idx <- if (length(signal_fc_idxs)) min(signal_fc_idxs) else length(fc_types) + 1L
    decimate_idxs    <- which(fc_types == "decimate")
    pre_dec_entries  <- filter_configs[decimate_idxs[decimate_idxs < first_signal_idx]]
    post_dec_entries <- filter_configs[decimate_idxs[decimate_idxs >= first_signal_idx]]

    # Restore pre_downsample_factor
    pre_dec_by <- 1L
    pre_dec_auto <- FALSE
    if (length(pre_dec_entries)) {
      pre_dec_entries <- pre_dec_entries[[1]]
      pre_dec_by <- max(1L, as.integer(pre_dec_entries$by %||% 1L))
      pre_dec_auto <- isTRUE(pre_dec_entries$auto)
    }
    shiny::updateNumericInput(session = session,  inputId = "pre_downsample_factor",
                              value = pre_dec_by)
    shiny::updateCheckboxInput(session = session,  inputId = "pre_downsample_factor_auto",
                               value = pre_dec_auto)

    # Restore post_downsample_factor
    post_dec_by <- 1L
    post_dec_auto <- FALSE
    if (length(post_dec_entries)) {
      post_dec_entries <- post_dec_entries[[1]]
      post_dec_by <- max(1L, as.integer(post_dec_entries$by %||% 1L))
      post_dec_auto <- isTRUE(post_dec_entries$auto)
    }
    shiny::updateNumericInput(session = session,  inputId = "post_downsample_factor",
                              value = post_dec_by)
    shiny::updateCheckboxInput(session = session,  inputId = "post_downsample_factor_auto",
                               value = post_dec_auto)


    filter_configs <- filter_configs[!fc_types %in% c("detrend", "demean", "decimate", "baseline")]
    bandstop_freqs <- NULL

    bandpass_type <- NULL
    bandpass_method <- NULL
    bandpass_freq <- NULL

    for (filter in filter_configs) {
      if (!isTRUE(filter$high_pass_freq > 0)) {
        # lowpass
        if (isTRUE(filter$low_pass_freq > 0)) {
          bandpass_type <- "low_pass"
          bandpass_method <- filter$type
          bandpass_freq <- filter$low_pass_freq
        }
      } else if (!isTRUE(filter$low_pass_freq > 0)) {
        bandpass_type <- "high_pass"
        bandpass_method <- filter$type
        bandpass_freq <- filter$high_pass_freq
      } else if (isTRUE(filter$high_pass_freq < filter$low_pass_freq)) {
        bandpass_type <- "band_pass"
        bandpass_method <- filter$type
        bandpass_freq <- c(filter$high_pass_freq, filter$low_pass_freq)
      } else {
        bandstop_freqs <- c(
          bandstop_freqs,
          sprintf("%.1f-%.1f", filter$low_pass_freq, filter$high_pass_freq)
        )
      }
    }

    if (length(bandpass_type)) {
      # Restore passing filter UI
      shiny::updateCheckboxInput(
        session = session,
        inputId = "passing_filter_enabled",
        value = TRUE
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "passing_filter_type",
        selected = bandpass_type
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "passing_filter_method",
        selected = bandpass_method %||% "firls"
      )

      shiny::updateNumericInput(
        session = session,
        inputId = "passing_freq1",
        value = as.numeric(bandpass_freq[[1]])
      )
      if (identical(bandpass_type, "band_pass") && length(bandpass_freq) > 1L) {
        shiny::updateNumericInput(
          session = session,
          inputId = "passing_freq2",
          value = as.numeric(bandpass_freq[[2]])
        )
      } else {
        shiny::updateNumericInput(
          session = session,
          inputId = "passing_freq2",
          value = NA_real_
        )
      }
    } else {
      shiny::updateCheckboxInput(
        session = session,
        inputId = "passing_filter_enabled",
        value = FALSE
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "passing_filter_type",
        selected = "low_pass"
      )
      shiny::updateSelectInput(
        session = session,
        inputId = "passing_filter_method",
        selected = "firls"
      )
      shiny::updateNumericInput(session = session, inputId = "passing_freq1", value = NA_real_)
      shiny::updateNumericInput(session = session, inputId = "passing_freq2", value = NA_real_)
    }

    if (length(bandstop_freqs)) {
      shiny::updateCheckboxInput(
        session = session,
        inputId = "bandstop_filter_enabled",
        value = TRUE
      )
      shiny::updateTextInput(
        session = session,
        inputId = "bandstop_filter_ranges",
        value = paste(bandstop_freqs, collapse = ", ")
      )
    } else {
      shiny::updateCheckboxInput(
        session = session,
        inputId = "bandstop_filter_enabled",
        value = FALSE
      )
      shiny::updateTextInput(
        session = session,
        inputId = "bandstop_filter_ranges",
        value = ""
      )
    }


  }



  # ---- Filter inspector --------------------

  shiny::bindEvent(
    ravedash::safe_observe({

      pipeline$set_settings(filter_configurations = get_filter_configurations())

      filter_freqz <- pipeline$run("filter_freqz")

      if (is.null(filter_freqz)) {
        stop("Filter inspector will not launch because user did not enter/enable any filters.")
        return()
      }

      local_reactives$filter_freqz <- filter_freqz

      shiny::showModal(
        session = session,
        shiny::modalDialog(
          title = "Filter Inspector",
          size = "l",
          footer = shiny::modalButton("OK"),
          easyClose = FALSE,

          shiny::plotOutput(
            outputId = ns("filter_inspector_plot"),
            height = "550px"
          )
        )
      )
    }, error_wrapper = "notification"),
    input$filter_inspector_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$filter_inspector_plot <- shiny::renderPlot({
    filter_freqz <- local_reactives$filter_freqz
    shiny::validate(shiny::need(!is.null(filter_freqz), message = "No filter detected"))

    plot(filter_freqz, xlim = filter_freqz$xlim)
  })

  # ---- Helper: check outputs are ready ------------------------------------
  .output_ready <- function() {
    shiny::validate(
      shiny::need(
        !isFALSE(ravedash::watch_data_loaded()),
        message = "Data is not loaded"
      ),
      shiny::need(
        !isTRUE(ravedash::watch_loader_opened()),
        message = "Loading screen is on"
      ),
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = "Please run the module first"
      )
    )
  }


  # ---- Register outputs ---------------------------------------------------

  # Mean ERP: one line per condition group, collapsed over channels
  output$figure_by_condition_over_time <- shiny::renderPlot({
    .output_ready()
    data_by_trial_channel_condition <- pipeline$read(var_names = "data_by_trial_channel_condition")
    shiny::validate(shiny::need(
      inherits(data_by_trial_channel_condition, "data_by_trial_channel_condition"),
      message = "No data available"
    ))
    time_range <- c(input$plot_time_start, input$plot_time_end)
    if (!length(time_range) || all(is.na(time_range))) {
      time_range <- c(NA, NA)
    }
    plot_trials_per_condition(
      data_by_trial_channel_condition = data_by_trial_channel_condition,
      # type = "collapse_trial",
      vertical_marks = input$plot_onset_mark %||% 0,
      crp = isTRUE(input$mean_erp_crp),
      time_range = time_range,
      cex = get_cex()
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
      channel_annotation = get_channel_annotation_style(),
      cex                = get_cex(),
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
    plot_by_channel_condition(
      data_by_channel_condition,
      group_by           = "channel",
      channel_annotation = get_channel_annotation_style(),
      cex                = get_cex(),
      vertical_marks     = input$plot_onset_mark %||% 0,
      time_range         = time_range,
      space              = 1,
      flip_y             = isTRUE(input$mean_erp_flip_y)
    )
  })


  # Trial heatmap: time x trial image, one panel per condition group
  output$figure_by_trial_per_condition <- shiny::renderPlot({
    .output_ready()
    data_by_trial_channel_condition <- pipeline$read(var_names = "data_by_trial_channel_condition")
    shiny::validate(shiny::need(
      inherits(data_by_trial_channel_condition, "data_by_trial_channel_condition"),
      message = "No data available"
    ))
    time_range <- c(input$plot_time_start, input$plot_time_end)
    if (!length(time_range) || all(is.na(time_range))) {
      time_range <- c(NA, NA)
    }
    plot_by_trials_per_condition_multilines(
      data_by_trial_channel_condition = data_by_trial_channel_condition,
      sort_by        = get_trial_sort_by(),
      cex            = get_cex(),
      crp            = isTRUE(input$mean_erp_crp),
      vertical_marks = input$plot_onset_mark %||% 0,
      time_range     = time_range
    )
  })


}
