
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  plot_env <- new.env()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  server_tools$run_analysis_onchange(
    component_container$get_input_ids(c(
      "electrode_text",
      "analysis_ranges"
    ))
  )

  # 3D viewer controller
  local_data$viewer_controller <- list(
    `Display Coordinates` = FALSE, `Show Panels` = FALSE,
    `Overlay Coronal` = TRUE, `Overlay Axial` = TRUE, `Overlay Sagittal` = TRUE,
    `Left Hemisphere` = "normal", `Right Hemisphere` = "normal",
    `Left Opacity` = 0.3, `Right Opacity` = 0.3,
    `Voxel Display` = "hidden", `Voxel Opacity` = 0L, `Voxel Label` = "0",
    `Display Data` = "Filtered", `Display Range` = "70",
    Speed = 0.05, `Show Legend` = TRUE, `Show Time` = TRUE, `Highlight Box` = TRUE,
    `Info Text` = TRUE)

  local_data$viewer_camera <- list(
    position = c(0, 500, 0), up = c(0, 0, 1), zoom = 1
  )
  local_data$has_viewer <- FALSE
  local_data$viewer_card_initialized <- FALSE


  # Input validators
  sv_bandpass <- validator_bandpass(input, output, session)
  sv_notch <- validator_notch(input, output, session)

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      if(!sv_bandpass$is_valid()) {
        ravedash::error_notification(
          list(message = "Please check the band-passing filter settings and correct the inputs as suggested.")
        )
        return()
      }
      if(!sv_notch$is_valid()) {
        ravedash::error_notification(
          list(message = "Please check the Notch filter settings and correct the inputs as suggested.")
        )
        return()
      }

      local_data$results <- list(valid = FALSE)

      if(isTRUE(input$filter_bandpass__enabled)) {
        filter_bandpass <- list(
          enabled = TRUE,
          range = sort(c(
            input$filter_bandpass__freq_lb,
            input$filter_bandpass__freq_ub
          ))
        )
      } else {
        filter_bandpass <- list(enabled = FALSE)
      }

      if( isTRUE(input$filter_notch__enabled) ){
        base_frequency <- as.numeric(input$filter_notch__base_frequency)
        harmonics <- dipsaus::parse_svec(input$filter_notch__harmonics, unique = FALSE)
        half_bandwidths <- dipsaus::parse_svec(input$filter_notch__bandwidths, unique = FALSE)
        filter_notch <- list(
          enabled = TRUE,
          lower_bounds = base_frequency * abs(harmonics) - abs(half_bandwidths),
          upper_bounds = base_frequency * abs(harmonics) + abs(half_bandwidths)
        )
      } else { filter_notch <- list(enabled = FALSE) }

      # Collect input data
      pipeline$set_settings(
        analysis_electrodes = input$electrode_text,
        analysis_block = input$analysis_block,
        filter_bandpass = filter_bandpass,
        filter_notch = filter_notch,
        pwelch_params = list(
          window_size = input$pwelch_window_size,
          noverlap = ceiling(input$pwelch_window_size * input$pwelch_window_overlap / 100)
        )
      )

      # Invalidate previous results (stop them because they are no longer needed)
      if(!is.null(local_data$pipeline_promise)) {
        try({
          ravepipeline::logger("Invalidating previous run", level = "trace")
          local_data$pipeline_promise$invalidate()
        }, silent = TRUE)
      }

      async <- FALSE

      if(!async) {
        dipsaus::shiny_alert2(
          title = "Running the pipeline",
          text = "Filtering signals, generating Welch-Periodograms...",
          icon = "info", danger_mode = FALSE, auto_close = FALSE, buttons = FALSE
        )
      }

      time_start <- Sys.time()

      local_data$pipeline_promise <- pipeline$run(
        names = c("settings_path", "settings", "analysis_block", "pwelch_params2",
                  "analysis_electrodes", "filter_notch", "filter_bandpass",
                  "pwelch_params", "subject", "cleaned_inputs", "analysis_electrodes2",
                  "filtered_data", "pwelch_data"),
        shortcut = TRUE, async = async, as_promise = TRUE, scheduler = "none", type = "smart"
      )

      time_end <- Sys.time()
      delta <- dipsaus::time_delta(time_start, time_end)
      if(!async && delta < 0.5) {
        Sys.sleep(1.0 - delta)
      }

      promises::then(
        local_data$pipeline_promise,
        onFulfilled = function(...) {
          shidashi::clear_notifications(class = "pipeline-error")
          dipsaus::close_alert2()

          ravedash::with_error_notification({
            # update 3D viewer params
            analysis_block <- pipeline$read("analysis_block")
            # repository <- pipeline$read("repository")
            repository <- component_container$data$repository
            sample_rate <- repository$sample_rate
            indata <- repository$data[[analysis_block]]
            max_time <- floor(nrow(indata) / sample_rate)
            shiny::updateSliderInput(
              session = session,
              inputId = "viewer_timestart",
              max = max_time
            )

            if(isTRUE(local_data$has_viewer) && isFALSE(local_data$viewer_card_initialized)) {
              shidashi::card_operate(inputId = "card_3dviewer", method = "expand")
              shidashi::card_operate(inputId = "card_viewer_setting", method = "expand")
              local_data$viewer_card_initialized <- TRUE
            }

          })

          local_reactives$update_outputs <- Sys.time()
          local_reactives$update_selection <- Sys.time()
          local_data$results <- list(valid = TRUE)
        },
        onRejected = function(e) {
          ravedash::error_notification(e, title = "Pipeline error!", class = "pipeline-error")
          dipsaus::close_alert2()
          local_data$results <- list(valid = FALSE)
        }
      )

      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_prepare_raw_voltage")) {
        ravepipeline::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_raw_voltage`. Abort initialization", level = "warning")
        return()
      }
      ravepipeline::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_raw_voltage")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravepipeline::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # Reset preset UI & data
      component_container$reset_data()
      srate <- min(new_repository$subject$raw_sample_rates[new_repository$subject$electrodes %in% new_repository$electrode_list])
      component_container$data$repository <- new_repository
      component_container$data$sample_rate <- srate
      component_container$initialize_with_new_data()

      # source("~/Dropbox (PENN Neurotrauma)/dipterix/projects/rave-pipelines/modules/block_explorer/R/aa.R")
      # new_repository <- pipeline$read("repository")

      # Set input - analysis_block
      shiny::updateSelectInput(
        session = session,
        inputId = "analysis_block",
        choices = new_repository$blocks,
        selected = pipeline$get_settings(
          key = "analysis_block",
          default = input$analysis_block,
          constraint = new_repository$blocks
        )
      )

      # set input - electrode_text
      electrode_text <- dipsaus::deparse_svec(new_repository$electrode_list)
      shiny::updateTextInput(
        session = session,
        inputId = "electrode_text",
        label = sprintf("Electrodes to visualize (loaded: %s)", electrode_text),
        value = electrode_text
      )


      # set input group - band-passing
      bp_lb <- input$filter_bandpass__freq_lb
      bp_ub <- input$filter_bandpass__freq_ub
      max_bp_rate <- floor(srate / 2)
      if(!length(bp_lb) || is.na(bp_lb) || bp_lb <= 0 || bp_lb > max_bp_rate) {
        bp_lb <- 0
      }
      if(!length(bp_ub) || is.na(bp_ub) || bp_ub <= bp_lb || bp_ub > max_bp_rate) {
        bp_ub <- max_bp_rate
      }
      shiny::updateSliderInput(
        session = session,
        inputId = "filter_bandpass__freq_lb",
        max = max_bp_rate, value = bp_lb
      )
      shiny::updateSliderInput(
        session = session,
        inputId = "filter_bandpass__freq_ub",
        max = max_bp_rate, value = bp_ub
      )

      # set input group - pwelch
      pwelch_fh <- srate * 2
      pwelch_fl <- min(100, pwelch_fh)
      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_window_size",
        min = pwelch_fl,
        max = pwelch_fh
      )
      shiny::updateSliderInput(
        session = session,
        inputId = "pwelch_frequency_limit",
        max = floor(srate / 2)
      )

      # check if the brain exists
      brain <- raveio::rave_brain(new_repository$subject)
      shidashi::card_operate(inputId = "card_3dviewer", method = "collapse")
      shidashi::card_operate(inputId = "card_viewer_setting", method = "collapse")

      if(inherits(brain, "rave-brain")) {
        local_data$has_viewer <- TRUE
      } else {
        local_data$has_viewer <- FALSE
      }

      local_data$viewer_card_initialized <- FALSE
      local_reactives$update_outputs <- FALSE
      local_reactives$brain_widget <- NULL

      try({
        promise2 <- local_reactives$pipeline_promise2
        if(!is.null(promise2)) {
          promise2$invalidate()
        }
      }, silent = TRUE)


      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      blocks <- component_container$data$repository$blocks
      if(!length(blocks)) { return() }
      current_idx <- which(blocks %in% input$analysis_block)
      if(length(current_idx)) {
        current_idx <- current_idx[[1]] - 1
        if(current_idx <= 0) {
          current_idx <- length(blocks)
        }
      } else {
        current_idx <- 1
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "analysis_block",
        selected = blocks[[current_idx]]
      )
    }),
    input$analysis_block__prev,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      blocks <- component_container$data$repository$blocks
      if(!length(blocks)) { return() }
      current_idx <- which(blocks %in% input$analysis_block)
      if(length(current_idx)) {
        current_idx <- current_idx[[1]] + 1
        if(current_idx > length(blocks)) {
          current_idx <- 1
        }
      } else {
        current_idx <- 1
      }
      shiny::updateSelectInput(
        session = session,
        inputId = "analysis_block",
        selected = blocks[[current_idx]]
      )
    }),
    input$analysis_block__next,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  outputs_need_update <- function(message = "Please run the module first") {
    shiny::validate(
      shiny::need(
        length(local_reactives$update_outputs) &&
          !isFALSE(local_reactives$update_outputs),
        message = message
      )
    )
    shiny::validate(
      shiny::need(
        isTRUE(local_data$results$valid),
        message = "One or more errors while executing pipeline. Please check the notification."
      )
    )
  }

  # gather plot parameters
  get_subset_details <- shiny::debounce(
    millis = 300,
    shiny::bindEvent(
      shiny::reactive({
        loaded_flag <- ravedash::watch_data_loaded()
        if(!loaded_flag){ return() }

        tryCatch({
          # analysis_time - subset
          analysis_time <- NULL
          brush <- input$plot_filtered_signals__brush
          if(is.list(brush)) {
            time_min <- max(brush$xmin, 0)
            time_max <- brush$xmax
            if(isTRUE(time_min < time_max)) {
              analysis_time <- c(time_min, time_max)
            }
          }

          # get vertical spacing
          vspacing <- as.numeric(input$vertical_spacing)
          if(length(vspacing) != 1 || is.na(vspacing) || vspacing <= 0) {
            vspacing <- 0.999
          }

          # Get display & hide electrodes
          highlight_electrodes <- dipsaus::parse_svec(input$highlight_channels, sort = TRUE)
          hide_electrodes <- dipsaus::parse_svec(input$hide_channels, sort = TRUE)

          if(length(hide_electrodes)) {
            hide_electrodes <- hide_electrodes[hide_electrodes == round(hide_electrodes)]
          }

          if(length(highlight_electrodes)) {
            highlight_electrodes <- highlight_electrodes[highlight_electrodes == round(highlight_electrodes)]
            highlight_electrodes <- highlight_electrodes[!highlight_electrodes %in% hide_electrodes]
          }

          pwelch_frequency_limit <- input$pwelch_frequency_limit

          return(list(analysis_time = analysis_time,
                      vertical_spacing = vspacing,
                      highlight_electrodes = highlight_electrodes,
                      pwelch_frequency_limit = pwelch_frequency_limit,
                      hide_electrodes = hide_electrodes))
        }, error = function(e) {
          ravedash::error_notification(e)
          NULL
        })
      }),
      ignoreNULL = FALSE, ignoreInit = FALSE,
      ravedash::watch_data_loaded(),
      input$plot_filtered_signals__brush,
      input$vertical_spacing,
      input$highlight_channels,
      input$hide_channels,
      input$pwelch_frequency_limit
    )
  )

  # Update pipeline plot data
  shiny::bindEvent(
    ravedash::safe_observe({
      settings <- get_subset_details()
      if(!is.list(settings) || !length(settings)) { return() }
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      if(!length(local_reactives$update_outputs) || isFALSE(local_reactives$update_outputs)) { return() }

      pipeline$set_settings(
        analysis_time = settings$analysis_time,
        vertical_spacing = settings$vspacing,
        highlight_electrodes = dipsaus::deparse_svec(settings$highlight_electrodes),
        hide_electrodes = dipsaus::deparse_svec(settings$hide_electrodes),
        pwelch_frequency_limit = settings$pwelch_frequency_limit
      )
      if(!is.null(local_reactives$pipeline_promise2)) {
        local_reactives$pipeline_promise2$invalidate()
      }
      local_reactives$pipeline_promise2 <- pipeline$run(
        scheduler = "none", type = "vanilla",
        names = c("settings_path", "settings", "pwelch_frequency_limit", "analysis_time",
                  "analysis_electrodes", "analysis_electrodes2",
                  "highlight_electrodes", "vertical_spacing", "hide_electrodes", "hide_electrodes2",
                  "highlight_electrodes2", "sel_electrodes", "sel_highlights",
                  "analysis_time2", "plot_data_label_colors", "plot_data_filtered_voltage_overall",
                  "vertical_spacing2", "plot_data_filtered_voltage_subset", "plot_data_pwelch",
                  "plot_data_pwelch_subset"),
        shortcut = TRUE,
        async = TRUE, as_promise = TRUE
      )

      promises::then(
        local_reactives$pipeline_promise2,
        onFulfilled = function(...) {

          vnames <- c("settings_path", "settings", "analysis_block", "pwelch_frequency_limit",
                      "analysis_electrodes", "filter_notch", "loaded_electrodes", "analysis_time",
                      "highlight_electrodes", "subject_code", "vertical_spacing", "filter_bandpass",
                      "project_name", "block", "hide_electrodes", "pwelch_params",
                      "subject", "repository", "cleaned_inputs", "pwelch_params2",
                      "analysis_electrodes2", "filtered_data", "pwelch_data", "hide_electrodes2",
                      "highlight_electrodes2", "sel_electrodes", "sel_highlights",
                      "analysis_time2", "plot_data_label_colors", "plot_data_filtered_voltage_overall",
                      "vertical_spacing2", "plot_data_filtered_voltage_subset", "plot_data_pwelch",
                      "plot_data_pwelch_subset")
          res <- pipeline$read(vnames)
          list2env(res, envir = plot_env)
          local_reactives$update_outputs <- Sys.time()
        },
        onRejected = function(e) {
          ravedash::error_notification(e)
        }
      )
    }),
    get_subset_details(),
    local_reactives$update_selection,
    ignoreInit = FALSE, ignoreNULL = TRUE
  )

  # get brush
  get_brush <- shiny::reactive({
    brush <- input$plot_filtered_signals__brush
    if(!is.list(brush)) { return(NULL) }
    time_min <- max(brush$xmin, 0)
    time_max <- brush$xmax
    if(!isTRUE(time_min < time_max)) { return(NULL) }
    return(c(time_min, time_max))
  })

  # get_show_hide_electrodes <- shiny::debounce(
  #   millis = 200,
  #   shiny::reactive({
  #     loaded_flag <- ravedash::watch_data_loaded()
  #     if(!loaded_flag){
  #       return(list(
  #         highlight = NULL,
  #         hide = NULL
  #       ))
  #     }
  #     highlight_electrodes <- dipsaus::parse_svec(input$highlight_channels, sort = TRUE)
  #     hide_electrodes <- dipsaus::parse_svec(input$hide_channels, sort = TRUE)
  #
  #     if(!length(highlight_electrodes) && !length(hide_electrodes)) {
  #       return(list(
  #         highlight = NULL,
  #         hide = NULL
  #       ))
  #     }
  #
  #     electrode_list <- tryCatch({
  #       pipeline$read("analysis_electrodes2")
  #     }, error = function(e) {
  #       component_container$data$repository$electrode_list
  #     })
  #
  #     highlight_electrodes <- highlight_electrodes[highlight_electrodes %in% electrode_list]
  #     hide_electrodes <- hide_electrodes[hide_electrodes %in% electrode_list]
  #
  #     return(list(
  #       highlight = highlight_electrodes,
  #       hide = hide_electrodes
  #     ))
  #   })
  # )
  #
  # get_vspacing <- shiny::debounce(
  #   millis = 200,
  #   shiny::reactive({
  #     vspacing <- as.numeric(input$vertical_spacing)
  #     if(length(vspacing) != 1 || is.na(vspacing) || vspacing <= 0) {
  #       vspacing <- 1
  #     }
  #     vspacing
  #   })
  # )


  # Register outputs
  ravedash::register_output(
    outputId = "plot_filtered_signals",
    render_function = shiny::renderPlot({
      outputs_need_update()

      env <- pipeline$eval(names = "plot_filtered_signals", env = plot_env, clean = FALSE, shortcut = TRUE)
      msg <- env$plot_filtered_signals
      shiny::validate(
        shiny::need(isTRUE(msg), message = paste(msg, collapse = ""))
      )
    }),
    output_opts = list(
      brush = shiny::brushOpts(
        id = ns("plot_filtered_signals__brush"),
        direction = "x",
        clip = TRUE,
        delayType = "debounce",
        delay = 300,
        opacity = 0.25,
        resetOnNew = FALSE
      )
    )
  )

  ravedash::register_output(
    outputId = "plot_filtered_signals_subset",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")

      env <- pipeline$eval(names = "plot_filtered_signals_subset", env = plot_env, clean = FALSE, shortcut = TRUE)
      msg <- env$plot_filtered_signals_subset
      shiny::validate(
        shiny::need(isTRUE(msg), message = paste(msg, collapse = "Please draw a range from figure to my left."))
      )
    })
  )

  ravedash::register_output(
    outputId = "plot_pwelch",
    render_function = shiny::renderPlot({
      outputs_need_update(message = "")

      env <- pipeline$eval(names = "plot_pwelch", env = plot_env, clean = FALSE, shortcut = TRUE)
      msg <- env$plot_pwelch
      shiny::validate(
        shiny::need(isTRUE(msg), message = paste(msg, collapse = ""))
      )
    }),
    output_opts = list(click = shiny::clickOpts(id = ns("plot_pwelch__click"), clip = TRUE))
  )

  get_pwelch_click <- function(plot_data, click) {
    if(!is.list(click) || length(click$x) != 1 || length(click$y) != 1) { return() }
    # plot_data <- pipeline$read("plot_data_pwelch")
    # plot_data <- plot_env[[name]]
    if(!is.list(plot_data) || !length(plot_data)) { return() }

    x <- click$x
    y <- click$y

    # find nearest frequency
    fidx <- which.min(abs(plot_data$frequencies - x))[[1]]
    nearest_freq <- plot_data$frequencies[fidx]

    power <- plot_data$data[fidx, ]
    if(!length(power)) { return() }

    pidx1 <- which.min(abs(power - y))[[1]]
    nearest_electrode1 <- plot_data$electrodes[pidx1]
    power1 <- power[pidx1]

    re <- list(
      x = x, y = y,
      nearest_freq = nearest_freq,
      nearest_electrode1 = nearest_electrode1,
      power1 = power1
    )

    if(plot_data$has_highlight) {
      highlighted_electrode <- plot_data$highlights
      tmp <- power[plot_data$electrodes %in% highlighted_electrode]
      pidx2 <- which.min(abs(tmp - y))[[1]]
      nearest_electrode2 <- highlighted_electrode[pidx2]
      power2 <- tmp[pidx2]

      re$nearest_electrode2 <- nearest_electrode2
      re$power2 <- power2
    }
    re
  }

  interactive_summary <- shiny::debounce(
    millis = 100,
    shiny::bindEvent(
      shiny::reactive({
        pwelch_clickinfo <- get_pwelch_click(plot_env$plot_data_pwelch, input$plot_pwelch__click)
        pwelch_subset_clickinfo <- get_pwelch_click(plot_env$plot_data_pwelch_subset, input$plot_pwelch_subset__click)
        analysis_time2 <- plot_env$analysis_time2

        plot_data <- plot_env$plot_data_filtered_voltage_overall

        list(
          pwelch_clickinfo = pwelch_clickinfo,
          pwelch_subset_clickinfo = pwelch_subset_clickinfo,
          analysis_time2 = analysis_time2,
          displayed_electrodes = dipsaus::deparse_svec(plot_data$labels)
        )
      }),
      input$plot_pwelch__click,
      input$plot_pwelch_subset__click,
      local_reactives$update_outputs,
      local_reactives$update_selection,
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  )

  output$graphic_summary <- shiny::renderUI({

    summary <- interactive_summary()

    voltage_brush <- summary$analysis_time2
    pwelch_clickinfo <- summary$pwelch_clickinfo
    pwelch_subset_clickinfo <- summary$pwelch_subset_clickinfo

    info_displayed <- summary$displayed_electrodes

    # brush selection
    if(length(voltage_brush) < 2) {
      info_brush <- "(No selection)"
    } else {
      voltage_brush <- sort(as.numeric(voltage_brush))
      info_brush <- sprintf("%.1f ~ %.1f (%.2f sec)",
                            voltage_brush[1], voltage_brush[2], voltage_brush[2] - voltage_brush[1])
    }

    if(!is.list(pwelch_clickinfo) || !length(pwelch_clickinfo)) {
      info_pwelch1 <- "(No selection)"
      info_pwelch2 <- ""
      info_pwelch3 <- ""
    } else {
      info_pwelch1 <- sprintf("%.1fHz", pwelch_clickinfo$nearest_freq)
      info_pwelch2 <- sprintf("%.0f (%.2f dB)",
                              pwelch_clickinfo$nearest_electrode1,
                              pwelch_clickinfo$power1)
      if(length(pwelch_clickinfo$nearest_electrode2)) {
        info_pwelch3 <- sprintf("%.0f (%.2f dB)",
                                pwelch_clickinfo$nearest_electrode2,
                                pwelch_clickinfo$power2)
      } else {
        info_pwelch3 <- "None"
      }
    }

    if(!is.list(pwelch_subset_clickinfo) || !length(pwelch_subset_clickinfo)) {
      info_pwelch_subset1 <- "(No selection)"
      info_pwelch_subset2 <- ""
      info_pwelch_subset3 <- ""
    } else {
      info_pwelch_subset1 <- sprintf("%.1fHz", pwelch_subset_clickinfo$nearest_freq)
      info_pwelch_subset2 <- sprintf("%.0f (%.2f dB)",
                              pwelch_subset_clickinfo$nearest_electrode1,
                              pwelch_subset_clickinfo$power1)
      if(length(pwelch_subset_clickinfo$nearest_electrode2)) {
        info_pwelch_subset3 <- sprintf("%.0f (%.2f dB)",
                                pwelch_subset_clickinfo$nearest_electrode2,
                                pwelch_subset_clickinfo$power2)
      } else {
        info_pwelch_subset3 <- "None"
      }
    }


    shiny::tags$dl(
      class = "row",

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Displayed:",
                     title = "Electrode channels being displayed in the plots"),
      shiny::tags$dd(class = "col-sm-6 text-nowrap overflow-hidden shidashi-clipboard-output clipboard-btn",
                     info_displayed, title = info_displayed, `data-clipboard-text` = info_displayed, role = "button"),

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Subset time:",
                     title = "Draw a range on the left voltage plot"),
      shiny::tags$dd(class = "col-sm-2 text-nowrap overflow-hidden", info_brush, title = info_brush),

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Frequency (left):",
                     title = "Click on the either Welch-Periodogram"),
      shiny::tags$dd(class = "col-sm-2 text-nowrap overflow-hidden", info_pwelch1, title = info_pwelch1),

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Nearest pick (left):",
                     title = "Nearest electrode & power to your mouse click"),
      shiny::tags$dd(class = "col-sm-2 text-nowrap overflow-hidden", info_pwelch2, title = info_pwelch2),

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Highlighted (left):",
                     title = "Nearest highlighted electrode & power to your mouse click"),
      shiny::tags$dd(class = "col-sm-2 text-nowrap overflow-hidden", info_pwelch3, title = info_pwelch3),

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Frequency (right):",
                     title = "Click on the either Welch-Periodogram"),
      shiny::tags$dd(class = "col-sm-2 text-nowrap overflow-hidden", info_pwelch_subset1, title = info_pwelch_subset1),

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Nearest pick (right):",
                     title = "Nearest electrode & power to your mouse click"),
      shiny::tags$dd(class = "col-sm-2 text-nowrap overflow-hidden", info_pwelch_subset2, title = info_pwelch_subset2),

      shiny::tags$dt(class = "col-sm-2 text-nowrap overflow-hidden", "Highlighted (right):",
                     title = "Nearest highlighted electrode & power to your mouse click"),
      shiny::tags$dd(class = "col-sm-2 text-nowrap overflow-hidden", info_pwelch_subset3, title = info_pwelch_subset3)

    )

  })



  # get_pwelch_subset_clickinfo <- shiny::reactive({
  #   info <- input$plot_pwelch_subset__click
  #   if(!is.list(info) || length(info$x) != 1 || length(info$y) != 1) { return() }
  #   pwelch_subset_data <- get_subset_pwelch()
  #   if(!(is.list(pwelch_subset_data) && length(pwelch_subset_data))) { return() }
  #
  #   x <- info$x
  #   y <- info$y
  #   # find nearest frequency
  #   fidx <- which.min(abs(pwelch_subset_data$freq - x))[[1]]
  #   nearest_freq <- pwelch_subset_data$freq[fidx]
  #
  #   power <- pwelch_subset_data$spec[fidx, ]
  #   pidx1 <- which.min(abs(power - y))[[1]]
  #   nearest_electrode1 <- pwelch_subset_data$display_electrodes[pidx1]
  #   power1 <- power[pidx1]
  #
  #   re <- list(
  #     x = x, y = y,
  #     nearest_freq = nearest_freq,
  #     nearest_electrode1 = nearest_electrode1,
  #     power1 = power1
  #   )
  #
  #   if(pwelch_subset_data$has_highlight) {
  #     highlighted_electrode <- pwelch_subset_data$display_electrodes[pwelch_subset_data$highlight_sel]
  #     tmp <- power[pwelch_subset_data$highlight_sel]
  #     pidx2 <- which.min(abs(tmp - y))[[1]]
  #     nearest_electrode2 <- highlighted_electrode[pidx2]
  #     power2 <- tmp[pidx2]
  #
  #     re$nearest_electrode2 <- nearest_electrode2
  #     re$power2 <- power2
  #   }
  #   return(re)
  # })

  ravedash::register_output(
    outputId = "plot_pwelch_subset",
    render_function = shiny::renderPlot({

      outputs_need_update(message = "")

      env <- pipeline$eval(names = "plot_pwelch_subset", env = plot_env, clean = FALSE, shortcut = TRUE)
      msg <- env$plot_pwelch_subset
      shiny::validate(
        shiny::need(isTRUE(msg), message = paste(msg, collapse = ""))
      )

    }),
    output_opts = list(click = shiny::clickOpts(id = ns("plot_pwelch_subset__click")))
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      analysis_time2 <- pipeline$read("analysis_time2")
      if(length(analysis_time2) != 2) { return() }

      start_time <- round(analysis_time2[1], digits = 2)
      shiny::updateSliderInput(
        session = session,
        inputId = "viewer_timestart",
        value = start_time
      )
      duration <- analysis_time2[2] - analysis_time2[1]
      if(duration > 10) {
        duration <- 10
      } else {
        duration <- round(duration, digits = 2)
      }
      shiny::updateSliderInput(
        session = session,
        inputId = "viewer_timesduration",
        value = duration
      )


    }),
    input$viewer_sync,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      ravedash::with_error_notification({
        loaded_flag <- ravedash::watch_data_loaded()
        if(!loaded_flag){
          stop("Please load data first")
        }
        if(!length(local_reactives$update_outputs) || isFALSE(local_reactives$update_outputs)) {
          stop("Please run the analysis first")
        }

        repository <- component_container$data$repository
        if(!length(repository)) {
          stop("Please load data first")
        }

        time_start <- input$viewer_timestart
        if(length(time_start) != 1 || is.na(time_start) || time_start < 0) {
          stop("Viewer start time is invalid")
        }

        sample_rate <- repository$sample_rate
        duration <- input$viewer_timesduration
        if(length(duration) != 1 || is.na(duration)) {
          stop("Viewer duration is invalid")
        }

        brain <- raveio::rave_brain(repository$subject)
        if(!inherits(brain, "rave-brain")) {
          stop("Cannot find 3D models from the subject.")
        }

        # get data
        filtered_data <- pipeline$read("filtered_data")

        sel_highlights <- pipeline$read("sel_highlights")
        sel_electrodes <- pipeline$read("sel_electrodes")


        electrodes <- repository$subject$electrodes
        sel <- electrodes %in% repository$electrode_list
        nelec <- sum(sel)

        status <- rep("Inactive", length(electrodes))
        status[sel_electrodes] <- "Displayed"
        status[sel_highlights] <- "Highlighted"
        status <- status[sel]
        status <- factor(status, levels = c("Inactive", "Displayed", "Highlighted"))


        render_start <- Sys.time()
        dipsaus::shiny_alert2(title = "Generating viewer", icon = "info", auto_close = FALSE, buttons = FALSE,
                              text = "Rendering animation in progress...")
        on.exit({
          render_stop <- Sys.time()
          delta <- dipsaus::time_delta(render_start, render_stop)
          if(delta < 1) {
            Sys.sleep(1 - delta)
          }
          dipsaus::close_alert2()
        }, add = TRUE, after = TRUE)

        # Get filtered data
        has_signals <- FALSE
        tidx_start <- round(time_start * sample_rate) + 1
        tidx_end <- tidx_start + round(duration * sample_rate)
        ntp <- nrow(filtered_data)
        if(tidx_start < 1) { tidx_start <- 1 }
        if(tidx_start > ntp){ tidx_start <- ntp}
        if( tidx_end < tidx_start ) { tidx_end <- tidx_start }
        if(tidx_end > ntp) { tidx_end <- ntp}

        ntp <- tidx_end - tidx_start + 1
        if(nelec * ntp > 4000001) {
          drate <- ceiling(nelec * ntp / 4000001)
        } else {
          drate <- 1
        }

        tidx <- seq.int(tidx_start, tidx_end, by = drate)
        new_sample_rate <- sample_rate / drate

        time <- tidx / sample_rate
        vdata <- filtered_data[tidx, sel, drop = FALSE, dimnames = FALSE]

        ntidx <- length(tidx)

        elecs <- electrodes[sel]

        data_table <- data.frame(
          Electrode = rep(elecs, each = ntidx),
          Time = rep(time, length(elecs)),
          Status = rep(status, each = ntidx),
          Filtered = as.vector(vdata)
        )

        brain$set_electrode_values(data_table)

        # get current viewer data
        background <- proxy$background
        if(length(background) != 1) {
          theme <- ravedash::current_shiny_theme()
          background <- theme$background
        }
        background <- dipsaus::col2hexStr(background)

        controllers <- local_data$viewer_controller
        zoom_level <- local_data$viewer_camera$zoom
        position <- local_data$viewer_camera$position
        up <- local_data$viewer_camera$up

        local_reactives$brain_widget <- brain$plot(
          show_modal = FALSE,
          background = background,
          controllers = controllers,
          start_zoom = zoom_level,
          # send signals to update parameters such as camera, zoom-level...
          custom_javascript = raveio::glue(
            '
            // Remove the focus box
            if( canvas.focus_box ) {
              canvas.focus_box.visible = false;
            }

            // set camera
            canvas.mainCamera.position.set(
              {{ position[[1]] }} ,
              {{ position[[2]] }} ,
              {{ position[[3]] }}
            );
            canvas.mainCamera.up.set(
              {{ up[[1]] }} ,
              {{ up[[2]] }} ,
              {{ up[[3]] }}
            )
            canvas.mainCamera.updateProjectionMatrix();

            // Force render one frame (update the canvas)
            canvas.needsUpdate = true;
            ',
            .open = "{{", .close = "}}"
          )
        )

      })

      dipsaus::close_alert2()

    }),
    input$viewer_generate,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  proxy <- threeBrain::brain_proxy("3dviewer")

  ravedash::register_output(
    outputId = "3dviewer",
    output_type = "threeBrain",
    render_function = threeBrain::renderBrain({
      wg <- local_reactives$brain_widget
      shiny::validate(
        shiny::need(!is.null(wg),
                    message = "Please click the [Generate viewer] button in the [3D Viewer Configurations] input panel")
      )
      wg
    })
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      theme <- ravedash::current_shiny_theme()
      proxy$set_background(col = theme$background)
    }),
    ravedash::current_shiny_theme()
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      controllers <- proxy$controllers
      if(is.list(controllers) && length(controllers)) {
        local_data$viewer_controller <- controllers
      }

    }),
    proxy$controllers
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      main_camera <- proxy$main_camera
      if(all(c("position", "zoom", "up") %in% names(main_camera))) {

        zoom_level <- main_camera$zoom
        if(length(zoom_level) == 1 && is.numeric(zoom_level) && isTRUE(zoom_level > 0)) {
          local_data$viewer_camera$zoom <- zoom_level
        }

        position <- unname(unlist(main_camera$position))
        up <- unname(unlist(main_camera$up))
        if(length(position) == 3 && length(up) == 3 &&
           is.numeric(position) && is.numeric(up) && !anyNA(c(position, up))) {
          local_data$viewer_camera$position <- position
          local_data$viewer_camera$up <- up
        }



      }

    }),
    proxy$main_camera
  )

  shiny::outputOptions(output, "3dviewer", suspendWhenHidden = FALSE)
}
