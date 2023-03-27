module_server <- function(input, output, session, ...){

  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL,
    current_analysis_settings=NULL,
    per_electrode_statistics_chooser=NULL
  )

  brain_proxy <- threeBrain::brain_proxy("brain_viewer", session = session)

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  local_data$brain <- NULL

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  server_tools$run_analysis_onchange(
    component_container$get_input_ids(c(
      "electrode_text"#,
      #"baseline_choices",
      #"analysis_ranges", #"condition_groups"
    ))
  )

  ### function to anlyze data
  run_analysis <- function() {
    settings <- component_container$collect_settings(ids = c(
      "electrode_text"
      # "baseline_choices",
      # "condition_groups"#,
      # "analysis_ranges"
    ))


    pipeline$set_settings(
      baseline_settings = list(
        window=list(input$baseline_window),
        scope = input$baseline_scope,
        unit_of_analysis = input$baseline_unit
      ),
      selected_electrodes = settings$analysis_electrodes,
      first_condition_groupings = input$first_condition_groupings,
      analysis_settings = input$ui_analysis_settings
    )

    # print(dput(pipeline$get_settings()))

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
      as_promise = FALSE,
      scheduler = "none",
      type = "smart",
      callr_function = NULL,
      # progress_title = "Calculating in progress",
      async = FALSE,
      # check_interval = 0.1,
      shortcut = FALSE,
      names = c(
        "settings", 'analysis_settings_clean', 'baselined_power'
      )
    )

    #local_data = list()
    local_data$results <- results
    local_reactives$update_outputs <- Sys.time()
    local_data$env <- pipeline$eval(names = c('analysis_settings_clean', 'analysis_groups', 'pluriform_power',
                                              'overall_tf_data', 'tf_correlation_data', 'by_electrode_tf_data',
                                              'over_time_data', 'scatter_bar_data', 'analysis_data', 'omnibus_results'))

    or <- rownames(local_data$env$omnibus_results$stats)
    choices_list <- unique(stringr::str_remove_all(or, '(m+\\(|t+\\(|p+\\(|p_fdr\\(|\\))'))

    ## update the by-electrode analysis viewer switcher

    current_choice <- input$per_electrode_statistics_chooser
    shiny::updateSelectInput(inputId = 'per_electrode_statistics_chooser',
                             choices = choices_list, selected = current_choice %OF% choices_list)

    return()
  }


  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({
      # Invalidate previous results (stop them because they are no longer needed)
      # if(!is.null(local_data$results)) {
      # local_data$results$invalidate()
      # ravedash::logger("Invalidating previous run", level = "trace")
      # }

      # Collect input data
      settings <- component_container$collect_settings(ids = c(
        "electrode_text"
        # "baseline_choices",
        # "condition_groups"#,
        # "analysis_ranges"
      ))

      pipeline$set_settings(
        baseline_settings = list(
          window=list(input$baseline_window),
          scope = input$baseline_scope,
          unit_of_analysis = input$baseline_unit
        ),
        selected_electrodes = settings$analysis_electrodes,
        first_condition_groupings = input$first_condition_groupings,
        analysis_settings = input$ui_analysis_settings
      )

      # print(dput(pipeline$get_settings()))

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
        async = FALSE,
        check_interval = 0.1,
        shortcut = FALSE,
        names = c(
          "settings", 'analysis_settings_clean', 'baselined_power'
        )
      )

      #local_data = list()
      local_data$results <- results
      ravedash::logger("Scheduled: ", pipeline$pipeline_name, level = 'debug', reset_timer = TRUE)

      results$promise$then(
        onFulfilled = function(...){
          ravedash::logger("Fulfilled: ", pipeline$pipeline_name, level = 'debug')
          shidashi::clear_notifications(class = "pipeline-error")
          local_reactives$update_outputs <- Sys.time()
          return(TRUE)
        },
        onRejected = function(e, ...){
          msg <- paste(e$message, collapse = "\n")
          if(inherits(e, "error")){
            ravedash::logger(msg, level = 'error')
            ravedash::logger(traceback(e), level = 'error', .sep = "\n")
            shidashi::show_notification(
              message = msg,
              title = "Error while running pipeline", type = "danger",
              autohide = FALSE, close = TRUE, class = "pipeline-error"
            )
          }
          return(msg)
        }
      )
      local_data$env <- pipeline$eval(names = c('analysis_settings_clean', 'analysis_groups', 'pluriform_power',
        'overall_tf_data', 'tf_correlation_data', 'by_electrode_tf_data',
        'over_time_data', 'scatter_bar_data', 'analysis_data', 'omnibus_results'))
      # local_data$env$analysis_settings

      or <- rownames(local_data$env$omnibus_results$stats)
      choices_list <- unique(stringr::str_remove_all(or, '(m+\\(|t+\\(|p+\\(|p_fdr\\(|\\))'))

      ## update the by-electrode analysis viewer switcher
      shiny::updateSelectInput(inputId = 'per_electrode_statistics_chooser',
        choices = choices_list, selected = choices_list[1])
      return()

      # run_analysis()
    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(ravedash::safe_observe({
    local_reactives$per_electrode_statistics_chooser = input$per_electrode_statistics_chooser
  }), input$per_electrode_statistics_chooser, ignoreNULL = FALSE, ignoreInit=TRUE)


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_prepare_power")){
        ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_power`. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_power")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # TODO: reset UIs to default

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository

      #--handle input initialization
      # default preset initialization
      component_container$initialize_with_new_data()

      # custom input init
      # new_repository <- pipeline$read('repository')

      ##--loading the default baseline settings
      baseline_settings <- new_repository$subject$get_default('baseline_settings',
        default_if_missing = pipeline$get_settings('baseline_settings'), namespace = module_id)

      shiny::updateSliderInput(session = session, inputId = 'baseline_window',
        value = unname(unlist(baseline_settings$window)), min = min(new_repository$time_points),
        max = max(new_repository$time_points)
      )
      shiny::updateSelectInput(session = session, inputId = 'baseline_scope',
        selected = baseline_settings$scope
      )

      ##--loading the default analysis settings
      as <- new_repository$subject$get_default('analysis_settings',
        default_if_missing = pipeline$get_settings('analysis_settings'), namespace = module_id)
      def <- list(
        list(label='A1', event='Trial Onset', time=0:1, frequency=c(70,150))
      )

      if(is.null(as) || !all(sapply(as, function(aa) all(c('event', 'frequency', 'label', 'time') %in% names(aa) )))) {
        as <- def
      }

      n_analysis <- length(as)
      dipsaus::updateCompoundInput2(session = session,
        inputId = 'ui_analysis_settings',
        initialization = list(
          event = list(selected = 'Trial Onset',
            choices=get_available_events(columns=new_repository$epoch$columns)
          ),
          time = list(min=min(new_repository$time_points), max=max(new_repository$time_points)),
          frequency = list(min=min(new_repository$frequency), max=max(new_repository$frequency))
        ), value=as, ncomp = n_analysis)

      # shiny::textInput(inputId = "label", label = "Label"),
      # shiny::selectInput(inputId = "event", label = "Event", choices=NULL,selected = NULL),
      # shiny::sliderInput(inputId = "time", label = "Time", min=0,max=1,value = c(0,1),step = .1),
      # shiny::sliderInput(inputId = "frequency", label = "Frequency", min=0,max=200,value = c(70,100), step = 1)

      ## default condition groups
      cond_tbl <- table(new_repository$epoch$table$Condition)
      cond_tbl <- cond_tbl[order(names(cond_tbl))]
      conditions <- names(cond_tbl)
      def <- list(
        list(label = "All Conditions", conditions = conditions)
      )

      val <- new_repository$subject$get_default('first_condition_groupings')
      if (!length(val) || !is.list(val) || !all(val$conditions %in% conditions)) {
        val <- def
      }
      dipsaus::updateCompoundInput2(session = session,
        inputId = 'first_condition_groupings',
        initialization = list(conditions = list(choices = conditions)),
        value = val, ncomp = length(val))


      # grab new brain
      local_data$brain = raveio::rave_brain(new_repository$subject$subject_id)

      # Reset outputs
      shidashi::reset_output("collapse_over_trial")
      shidashi::reset_output("activity_over_time_by_electrode")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )


  #### tracking clicks on 3dViewer
  shiny::bindEvent(
    ravedash::safe_observe({
      ravedash::logger('3dBrain double click')

      info <- as.list(brain_proxy$mouse_event_double_click)
      if(!isTRUE(info$is_electrode)) {
        return()
      }

      # ravedash::logger(str(info))
      # ravedash::logger('trigger analysis run')

      id <- component_container$get_input_ids("electrode_text")
      shiny::updateTextInput(inputId=electrode_text, value=paste0(info$electrode_number))

      run_analysis()
    }),

    brain_proxy$mouse_event_double_click,


    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      ravedash::logger('3dBrain single click')
    }),
    brain_proxy$mouse_event_click,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # # # tracking frequency window changes
  shiny::bindEvent(
    ravedash::safe_observe({
      local_reactives$current_analysis_settings = input$ui_analysis_settings
    }),
    input$ui_analysis_settings, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  basic_checks <- function(flag) {
    shiny::validate(shiny::need(flag, 'Results not yet available, please click RAVE!'))
  }

  # shiny::validate(
  #   shiny::need(
  #     length(local_reactives$update_outputs) &&
  #       !isFALSE(local_reactives$update_outputs),
  #     message = "Please run the module first"
  #   )
  # )
  # shiny::validate(
  #   shiny::need(
  #     isTRUE(local_data$results$valid),
  #     message = "One or more errors while executing pipeline. Please check the notification."
  #   )
  # )
  # }

  # Register outputs



  #### 3d brain viewer
  output$brain_viewer <- threeBrain::renderBrain({
    basic_checks(local_reactives$update_outputs)

    df <- data.frame(t(local_data$env$omnibus_results$stats))

    # fix some column names
    names(df) = str_replace_all(names(df), '\\.\\.\\.', ' vs ')

    names(df) = str_replace_all(names(df), '\\.', ' ')

    names(df) = str_replace_all(names(df), '\\ $', '')

    df$Electrode = as.integer(rownames(df))

    local_data$brain$set_electrode_values(df)
    local_data$brain$render(outputId = "brain_viewer", session = session)
  })

  ### export button download handler
  output$btn_export_electrodes <- downloadHandler(
    filename=function(...) {
      paste0('power_explorer_output',
        format(Sys.time(), "%b_%d_%Y_%H_%M_%S"), '.csv')
    },
    content = function(conn) {
      # export settings
      pipeline$set_settings(
        electrode_export_file_type = input$electrode_export_file_type,
        frequencies_to_export = input$frequencies_to_export,
        times_to_export = input$times_to_export,
        trials_to_export = input$trials_to_export,
        electrodes_to_export = input$electrodes_to_export
      )

      run_analysis()

      pip_env <- pipeline$eval('data_for_export')

      tf <- temp_file(fileext = '.csv')

      write.csv(pip_env$data_for_export, file=tf)

      file.copy(tf, conn)
    }
  )


  output$per_electrode_results_table <- DT::renderDataTable({
    basic_checks(local_reactives$update_outputs)

    omnibus_data <- local_data$env$omnibus_results
    # repo <- local_data$env$repository

    # assign('od', omnibus_data, envir = globalenv())

    mat <- t(omnibus_data$stats)
    mat <- cbind('Electrode'=as.numeric(rownames(mat)), mat)
    rownames(mat)=NULL

    df <- data.frame(mat)

    pcols <- str_detect(colnames(mat), 'p\\(|p_fdr\\(')
    df[pcols] %<>% lapply(function(v)as.numeric(round_pval(v)))

    dt <- DT::datatable(df, colnames=colnames(mat), rownames = FALSE,
      extensions = c('FixedColumns', 'FixedHeader', 'Scroller', 'Buttons'),
      options=list(autoWidth=TRUE, scroller=TRUE, scrollX=TRUE, scrollY='500px',
        buttons = list(list(extend = 'copy', title = NULL)),
        fixedColumns = list(leftColumns = 1),
        server=TRUE,order=TRUE,
        columnDefs = list(list(width = '50px', targets = "_all")
        ),
        dom = 'Brt'
      )
    )

    to_round <- df[!pcols] %>% sapply(get_pretty_digits)
    for(ur in unique(to_round)) {
      nms <- names(which(to_round == ur))
      dt %<>% DT::formatRound(nms, digits=ur+1)
    }

    #electrodes should always be integer
    dt %<>% DT::formatRound('Electrode', digits=0)

    return (dt)
  })

  output$activity_over_time_by_electrode <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    # check if we are in a multiple event situation
    etf_data <- local_data$env$by_electrode_tf_data
    repo <- local_data$env$repository
    analysis_groups <- local_data$env$analysis_groups
    # analysis_settings <- local_data$env$analysis_settings_clean
    baseline_settings <- local_data$env$baseline_settings

    if(all(c('data', 'xlab', 'ylab') %in% names(etf_data[[1]]))) {
      draw_many_heat_maps(
        hmaps = etf_data,
        meta_data = list(
          subject_code = repo$subject$subject_code,
          groups=analysis_groups,
          baseline=baseline_settings
        ),
        PANEL.LAST = time_frequency_decorator(analysis_window_type = 'line')
      )
    } else {
      # we need to flatten the data before it can be plotted
      maps <- vector('list', length = prod(length(etf_data), length(etf_data[[1]])))
      curr_map = 1
      for(ii in seq_along(etf_data)) {
        for(jj in seq_along(etf_data[[ii]])) {
          maps[[curr_map]] = etf_data[[ii]][[jj]]
          curr_map = curr_map + 1
        }
      }

      draw_many_heat_maps(
        hmaps = maps,
        meta_data = list(
          subject_code = repo$subject$subject_code,
          groups=analysis_groups,
          baseline=baseline_settings
        ),
        PANEL.LAST = time_frequency_decorator(analysis_window_type = 'line')
      )
    }
  })

  output$activity_over_time_by_frequency <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    # check if we are in a multiple event situation
    tf_data <- local_data$env$overall_tf_data

    # here we want to check and see if any of the settings need to
    # be updated
    if(exists('local_reactives') && !is.null(local_reactives$current_analysis_settings) && length(local_reactives)) {
      nms <- c('label', 'event', 'time', 'frequency')
      for(ii in seq_along(tf_data)) {
        for(si in seq_along(local_reactives$current_analysis_settings)) {
          #add a new setting if required
          if(si > length(tf_data[[ii]]$settings)) {
            tf_data[[ii]]$settings[[si]] = list()
          }

          tf_data[[ii]]$settings[[si]][nms] = local_reactives$current_analysis_settings[[si]][nms]
        }
        k <- length(local_reactives$current_analysis_settings)
        # remove unused settings
        if(length(tf_data[[ii]]$settings) > k) {
          tf_data[[ii]]$settings = tf_data[[ii]]$settings[
            -seq(from=(k+1), to=length(tf_data[[ii]]$settings) )
          ]
        }
      }
    }

    repo <- local_data$env$repository
    analysis_groups <- local_data$env$analysis_groups
    analysis_settings <- local_data$env$analysis_settings_clean

    # check if current_freq_windows (from the UI) is different from the
    # pipeline settings
    baseline_settings <- local_data$env$baseline_settings

    if(1 == length(unique(sapply(analysis_settings, `[[`, 'event')))) {
      draw_many_heat_maps(
        hmaps = tf_data,
        meta_data = list(
          subject_code = repo$subject$subject_code,
          groups=analysis_groups,
          # analysis=analysis_settings,
          baseline=baseline_settings
        ),
        PANEL.LAST = time_frequency_decorator()
      )
    }
  })

  output$frequency_correlation_plot <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)
    # res <- pipeline$eval('betfd_success')
    # shiny::validate(
    #     shiny::need(isTRUE(res$betfd_success), message = res$betfd_success$message)
    # )

    # check if we are in a multiple event situation
    tf_correlation_data <- local_data$env$tf_correlation_data
    repo <- local_data$env$repository
    analysis_groups <- local_data$env$analysis_groups
    analysis_settings <- local_data$env$analysis_settings_clean
    baseline_settings <- local_data$env$baseline_settings

    if(1 == length(unique(sapply(analysis_settings, `[[`, 'event')))) {
      old_pty <- par(pty='s')
      draw_many_heat_maps(
        hmaps = tf_correlation_data,
        meta_data = list(
          subject_code = repo$subject$subject_code,
          groups=analysis_groups,
          baseline=baseline_settings
        ),
        PANEL.LAST = function(data, ...) {
          rave_axis_labels('Frequency', 'Frequency')
          # put the name of the trial grouping at the top of the spectrogram
          lbl <- if(is.null(data$settings$label)) {
            data$settings[[1]]$label
          } else {
            data$settings$label
          }
          rave_title(paste(lbl, '|', data$name))
        }
      )
      par(pty=old_pty)
    } else {
      image(matrix(1:20))
    }

  })

  output$per_electrode_statistics <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    stats <- local_data$env$omnibus_results$stats

    if(exists('local_reactives') && !is.null(local_reactives$per_electrode_statistics_chooser)) {
      requested_stat = local_reactives$per_electrode_statistics_chooser
    }

    plot_per_electrode_statistics(stats, requested_stat)
  })

  output$over_time_separated_all <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)
    plot_over_time_by_condition(local_data$env$over_time_data,
      combine_events=FALSE, combine_conditions=FALSE)
  })

  output$over_time_combined_conditions <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    plot_over_time_by_condition(local_data$env$over_time_data,
      combine_events = FALSE,
      combine_conditions=TRUE)
  })

  output$over_time_combined_events <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    plot_over_time_by_condition(local_data$env$over_time_data,
      combine_events = TRUE,
      combine_conditions=FALSE)
  })

  output$over_time_combined_all <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    plot_over_time_by_condition(local_data$env$over_time_data,
      combine_conditions=TRUE,
      combine_events = TRUE)
  })

}

