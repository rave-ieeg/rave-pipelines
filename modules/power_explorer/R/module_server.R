module_server <- function(input, output, session, ...){

  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL,
    update_line_plots = NULL,
    update_3dviewer = NULL,
    update_by_trial_plot = NULL,
    update_over_time_plot = NULL,
    current_analysis_settings=NULL,
    per_electrode_statistics_chooser=NULL,
    pes_electrode_hover=NULL,
    pes_electrode_clicks=NULL,
    pes_selected_electrodes=NULL,
    pes_display_threshold=NULL,
    pes_manual_threshold=NULL,
  )

  brain_proxy <- threeBrain::brain_proxy("brain_viewer", session = session)
  brain_proxy_movies <- threeBrain::brain_proxy("brain_viewer_movies",
                                                session = session)

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  local_data$brain <- NULL
  local_data$brain_movies <- NULL
  local_data$available_electrodes = integer()

  # the defaults here should match the defaults in the UI / plot function
  local_data$grouped_plot_options <- list(
    'xvar' = 'Factor1',
    'gvar' = 'AnalysisLabel',
    'yvar' = 'y',
    'panelvar' = 'none',
    'plot_options' = list('pt.alpha' = 100, 'pt.cex' = 1),
    'types' = c('jitter points', 'means', 'ebar polygons'),
    'jitter_seed' = Sys.time()
  )

  # this is used to get ROI variables
  local_data$electrode_meta_data <- NULL

  # get server tools to tweak
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

  ### function to analyze data
  run_analysis <- function(trigger_3dviewer=TRUE, force_settings=list(),
                           progress, ...) {
    if(missing(progress)) {
      progress = shidashi::shiny_progress("Running analysis", max=4)
    }
    if(is.null(progress)) {
      progress = list(inc=function(...){},close=function(...){})
    }

    on.exit({
      progress$close("Done!")
    }, add=TRUE)

    progress$inc("collect settings")

    # settings <- component_container$collect_settings(ids = c(
    #   "electrode_text"
    # ))
    settings <- dipsaus::fastmap2()

    if ('electrode_text' %in% names(force_settings)) {
      settings$analysis_electrodes = paste0(force_settings$electrode_text)
    } else {
      settings$analysis_electrodes <- input$electrode_text
    }


    pipeline$set_settings(
      baseline_settings = list(
        window=list(input$baseline_window),
        scope = input$baseline_scope,
        unit_of_analysis = input$baseline_unit
      ),
      analysis_electrodes = settings$analysis_electrodes,
      first_condition_groupings = input$first_condition_groupings,
      second_condition_groupings = input$second_condition_groupings,
      enable_second_condition_groupings = isTRUE(input$enable_second_condition_groupings),
      enable_custom_ROI = isTRUE(input$enable_custom_ROI),
      custom_roi_type = input$custom_roi_type,
      custom_roi_variable = input$custom_roi_variable,
      custom_roi_groupings = input$custom_roi_groupings,
      analysis_settings = input$ui_analysis_settings,

      # TODO: create UIs for these variables
      time_censor = list(
        enabled = FALSE,
        window = c(0, 1)
      ),
      trial_outliers_list = NULL,
      electrode_export_file_type = input$electrode_export_file_type,
      electrode_export_data_type = input$electrode_export_data_type,
      electrodes_to_export_roi_name = input$electrodes_to_export_roi_name,
      electrodes_to_export_roi_categories = input$electrodes_to_export_roi_categories,
      frequencies_to_export = input$frequencies_to_export,
      times_to_export = input$times_to_export,
      trials_to_export = input$trials_to_export,
      electrodes_to_export = input$electrodes_to_export
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
    #'

    progress$inc("Baseline data")

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
        "settings", 'analysis_settings_clean', 'baselined_power', 'analysis_groups',

        'over_time_by_electrode_and_group',

        'omnibus_results'
      )
    )

    #local_data = list()
    local_data$results <- results

    progress$inc("Build statistical models")
    eval_names <- c('analysis_settings_clean', 'analysis_groups', 'pluriform_power',
                    'overall_tf_data', 'tf_correlation_data', 'by_electrode_tf_data',
                    'over_time_by_trial_data',
                    'over_time_data', 'scatter_bar_data', 'analysis_data')
    local_data$results[eval_names] <- as.list(pipeline$eval(names = eval_names))[eval_names]


    progress$inc("Done pipeline eval")

    or <- rownames(local_data$results$omnibus_results$stats)
    choices_list <- unique(stringr::str_remove_all(or, '(m+\\(|t+\\(|p+\\(|p_fdr\\(|\\))'))

    ## update the by-electrode analysis viewer switcher

    current_choice <- input$per_electrode_statistics_chooser
    shiny::updateSelectInput(inputId = 'per_electrode_statistics_chooser',
                             choices = choices_list, selected = current_choice %OF% choices_list)


    #### this is where we add Factor 2 to the analysis
    if(is.null(local_data$results$omnibus_results$data$Factor2)) {
      shiny::updateSelectInput(inputId = 'btp_gvar',
                               choices=c('Analysis Group', 'First Factor')
      )

      shiny::updateSelectInput(inputId = 'btp_xvar',
                               choices=c('First Factor', 'Analysis Group')
      )

    } else {
      shiny::updateSelectInput(inputId = 'btp_gvar',
                               choices=c('Analysis Group', 'First Factor', 'Second Factor', 'First:Second')
      )

      shiny::updateSelectInput(inputId = 'btp_xvar',
                               choices=c('First Factor', 'Second Factor', 'First:Second', 'Analysis Group')
      )
    }

    local_reactives$update_outputs <- Sys.time()
    if(trigger_3dviewer) {
      local_reactives$update_3dviewer <- Sys.time()
    }
    return()
  }

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({
      run_analysis()
    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     if(input$over_time_tabset_config > 1) {
  #       input$over_time_tabset_config = 0
  #     }
  #   }),
  #   input$over_time_tabset_config,
  #   ignoreNULL = TRUE, ignoreInit = TRUE
  # )

  shiny::bindEvent(ravedash::safe_observe({
    local_reactives$per_electrode_statistics_chooser = input$per_electrode_statistics_chooser
  }), input$per_electrode_statistics_chooser, ignoreNULL = FALSE, ignoreInit=TRUE)


  ### select electrodes using the univariate stat output
  shiny::bindEvent(ravedash::safe_observe({

    if(input$pes_select_mode %in% c('Clear labels', 'Invert selection', 'Manual threshold')) {
      on.exit({
        shiny::updateSelectInput(inputId = 'pes_select_mode', selected = 'Label maker')
      })
    }

    if(input$pes_select_mode=='Clear labels') {
      local_reactives$pes_electrode_hover=NULL
      local_reactives$pes_electrode_clicks=NULL
      local_reactives$pes_selected_electrodes=NULL
      local_reactives$pes_manual_threshold=NULL

    } else if (input$pes_select_mode == 'Invert selection') {
      if(shiny::isTruthy(local_data$results$omnibus_results$stats)) {
        curr <- local_reactives$pes_selected_electrodes
        el_numbers <- as.integer(colnames(local_data$results$omnibus_results$stats))
        if(is.null(curr)) {
          local_reactives$pes_selected_electrodes = el_numbers
        } else {
          local_reactives$pes_selected_electrodes = setdiff(el_numbers, curr)
        }

        ## if there is a currently set threshold, flip that as well
        if(!is.null(local_reactives$pes_manual_threshold)) {
          curr = local_reactives$pes_manual_threshold$operator_string
          new = switch(curr,
                       ">" = "<=",
                       "<" = ">=",
                       ">=" = "<",
                       "<=" = ">",
                       "==" = "!=",
                       "!=" = "=="
          )
          local_reactives$pes_manual_threshold$operator_string = new
        }
      }
    }
    else if(input$pes_select_mode=='Manual threshold') {
      shiny::showModal(shiny::modalDialog(
        title = "Create a threshold",
        size = "m",
        easyClose = TRUE,
        shiny::fluidRow(
          shiny::column(width=3,
                        shiny::selectInput(ns('pes_manual_threshold_stat'),
                                           label = 'Criterion', choices=c('mean', 't-stat', 'p-value'))),
          shiny::column(width=2, offset=0,
                        shiny::selectInput(ns('pes_manual_threshold_operator'),
                                           label = 'Operator', choices=c('>', '<', '==', '>=', '<='))),
          shiny::column(width=5, offset=0,
                        shiny::textInput(ns('pes_manual_threshold_comparator'),
                                         label = 'Comparator (numeric)')),
          shiny::column(width=2, style='margin-top: 32px',
                        shiny::actionButton(ns('pes_manual_threshold_doit'), label = 'Apply'))
        ), footer=NULL
      ))
    }

  }), input$pes_select_mode, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::bindEvent(ravedash::safe_observe({
    on.exit({shiny::removeModal()})

    # gather material
    if(shiny::isTruthy(local_data$results$omnibus_results$stats)) {

      ptype = which(input$pes_manual_threshold_stat == c('mean', 't-stat', 'p-value'))
      click = list(x=NA, y=as.numeric(input$pes_manual_threshold_comparator))

      dipsaus::cat2('Trying manual threshold', ptype, click$x, click$y)
      print(str(match.fun(input$pes_manual_threshold_operator)))

      update_pes_clicks(click,
                        plot=c('m', 't', 'p')[ptype],
                        operator = input$pes_manual_threshold_operator,
                        select_mode='Manual threshold')
    }

  }), input$pes_manual_threshold_doit, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::bindEvent(ravedash::safe_observe({

    etext <- dipsaus::deparse_svec(local_reactives$pes_selected_electrodes)
    if(nchar(etext) > 0) {
      on.exit({
        Sys.sleep(1)
        shiny::updateSelectInput(inputId = 'pes_selected_action', selected = 'Click for choices')
      })

      if(input$pes_selected_action == 'Analyze selection') {
        shiny::updateTextInput(inputId = 'electrode_text', value = etext)
        ravedash::show_notification(sprintf('Running analysis on electrodes: [%s]',etext), title='Analyze',
                                    type='info', delay = 2000)
        on.exit({
          run_analysis(force_settings = list(electrode_text=etext))
        }, add = TRUE, after = TRUE)
      } else if (input$pes_selected_action == 'Send selection to export') {

        shiny::updateTextInput(inputId = 'electrodes_to_export', value = etext)
        ravedash::show_notification(sprintf('Sending [%s] to export input box',etext), title='Export',
                                    type='info', delay = 2000)
      }
    }

  }), input$pes_selected_action, ignoreNULL = TRUE, ignoreInit = TRUE)


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

      if(is.null(as) || !all(sapply(as, function(aa) {all(c('event', 'frequency', 'label', 'time') %in% names(aa))}))){
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

      ## default condition groups
      cond_tbl <- table(new_repository$epoch$table$Condition)
      cond_tbl <- cond_tbl[order(names(cond_tbl))]
      conditions <- names(cond_tbl)
      def <- list(
        list(label = "All Conditions", conditions = conditions)
      )

      val <- new_repository$subject$get_default('first_condition_groupings',
                                                default_if_missing = pipeline$get_settings('first_condition_groupings'),
                                                namespace = module_id)
      if (!length(val) ||
          !is.list(val) ||
          !all(unlist(sapply(val, `[[`, 'conditions')) %in% conditions)) {
        val <- def
      }

      all_fcg_conditions <- unname(unlist(sapply(val, `[[`, 'conditions')))
      dipsaus::updateCompoundInput2(session = session,
                                    inputId = 'first_condition_groupings',
                                    initialization = list(conditions =
                                                            list(choices = conditions)
                                    ),
                                    value = val, ncomp = length(val))

      # default condition groups for factor 2
      val_scg <- new_repository$subject$get_default('second_condition_groupings',
                                                    default_if_missing = pipeline$get_settings('second_condition_groupings'),
                                                    namespace = module_id)
      def_s2 = list(list(label="Factor2 G1", conditions = all_fcg_conditions))
      # the available  onditions
      if (!length(val_scg) || !is.list(val_scg) || !all(val_scg$conditions %in% val$conditions)) {
        val_scg <- def_s2
      }
      dipsaus::updateCompoundInput2(session = session,
                                    inputId = 'second_condition_groupings',
                                    initialization = list(conditions = list(choices = all_fcg_conditions)
                                    ),
                                    value = val_scg, ncomp = length(val_scg))

      ## update ROI variable choices
      # any variable with between 2 and 20 unique values can be an ROI
      tbl <- new_repository$electrode_table

      local_data$electrode_meta_data = tbl[tbl$Electrode %in% new_repository$power$dimnames$Electrode,]

      roi_vars <- names(which(sapply(local_data$electrode_meta_data, count_elements) %within% c(2, nrow(local_data$electrode_meta_data)-1)))
      roi_vars = roi_vars[!(roi_vars %in% c('isLoaded'))]

      local_data$available_roi_vars = roi_vars

      shiny::updateSelectInput(session=session,
                               inputId = 'custom_roi_variable',
                               selected="none",
                               choices = c("none", roi_vars)
      )

      # grab new brain
      local_data$brain = raveio::rave_brain(new_repository$subject$subject_id)
      local_data$brain_movies = raveio::rave_brain(new_repository$subject$subject_id)

      local_data$available_electrodes = new_repository$power$dimnames$Electrode


      # update export electrode categories
      new_choices = c('none', roi_vars)
      if(isTRUE(input$enable_custom_ROI)) {
        new_choices %<>% c('custom ROI')
      }

      shiny::updateSelectInput(inputId='electrodes_to_export_roi_name',
                               selected = 'none', choices = new_choices
      )
      # Reset outputs
      shidashi::reset_output("collapse_over_trial")
      shidashi::reset_output("activity_over_time_by_electrode")


      #TODO update UI selectors to possibly cached values
    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )


  #### tracking clicks on 3dViewer
  shiny::bindEvent(
    ravedash::safe_observe({
      ravedash::logger('3dBrain double click')
      ravedash::clear_notifications(class=ns('threedviewer'))

      info <- as.list(brain_proxy$mouse_event_double_click)
      if(!isTRUE(info$is_electrode)) {
        return()
      }

      if(! (info$electrode_number %in% local_data$available_electrodes)) {
        ravedash::show_notification(
          sprintf("Selected electrode (%s) not loaded", info$electrode_number),
          title='3dViewer Info',
          type='warning', class=ns('threedviewer_no'),
          delay=2000
        )
        return()
      } else {
        ravedash::show_notification(paste0("Trying to load data for electrode: ",
                                           info$electrode_number),
                                    class=ns('threedviewer_yes'),
                                    title='3dViewer Info', delay=2000,
                                    type = 'info')

        on.exit(add=TRUE, {
          ravedash::clear_notifications(ns('threedviewer_yes'))
        })
      }

      # ravedash::logger(str(info))
      id <- electrode_selector$get_sub_element_id(with_namespace = FALSE)

      shiny::updateTextInput(inputId=id, value=paste0(info$electrode_number))

      run_analysis(trigger_3dviewer = FALSE,
                   force_settings=list(electrode_text = info$electrode_number)
      )
    }),

    brain_proxy$mouse_event_double_click,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # shiny::bindEvent(
  #   ravedash::safe_observe(
  #     shiny::updateTextInput(inputId='electrodes_to_export', value=
  #                              input[[electrode_selector$get_sub_element_id(with_namespace = FALSE)]]
  #     )
  #   ),
  #   # input[[electrode_selector$get_sub_element_id(with_namespace = FALSE)]]
  # )


  shiny::bindEvent(
    ravedash::safe_observe({
      ravedash::logger('3dBrain single click')
    }),
    brain_proxy$mouse_event_click,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # track changes to the First condition group so we can update the
  # second condition group as needed
  shiny::bindEvent(
    ravedash::safe_observe({
      if(isTRUE(input$enable_second_condition_groupings)) {
        ensure_scg_matches_fcg()
      }
    }),
    input$first_condition_groupings, ignoreNULL = FALSE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(isTRUE(input$enable_second_condition_groupings)) {
        # ensure all second condition group vars have usable levels
        ensure_scg_matches_fcg()
      }
    }), input$enable_second_condition_groupings, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  ensure_scg_matches_fcg <- function() {
    all_fcg <- unname(unlist(sapply(input$first_condition_groupings, `[[`, 'conditions')))

    scg <- input$second_condition_groupings
    all_scg <- c(unname(unlist(sapply(scg, `[[`, 'conditions'))))

    if (!all(all_scg %in% all_fcg)) {
      for(ii in seq_along(scg)) {
        scg[[ii]]$conditions = intersect(scg[[ii]]$conditions, all_fcg)
      }
    }
    # ravedash::logger("available choices should be: ", paste(all_fcg, collapse=','))

    dipsaus::updateCompoundInput2(session = session,
                                  inputId = 'second_condition_groupings',
                                  initialization = list(conditions =
                                                          list(choices = all_fcg)
                                  ),
                                  value = scg, ncomp = length(scg))
  }


  # track changes to the ROI grouping variable selector
  shiny::bindEvent(
    ravedash::safe_observe({
      if(isTRUE(input$enable_custom_ROI)) {
        load_roi_conditions()
      } else {
        # remove Custom ROI from choices in Export window
        shiny::updateSelectInput(inputId = 'electrodes_to_export_roi_name',
                                 choices = c('none', local_data$available_roi_vars))

      }
    }), input$enable_custom_ROI, input$custom_roi_variable,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  load_roi_conditions <- function() {
    if(is.null(local_data$electrode_meta_data)) return (FALSE)

    # get ROI variable
    vv <- input$custom_roi_variable

    # get unique values of ROI var
    roi_choices <- unique(local_data$electrode_meta_data[[vv]])

    old_val = input$custom_roi_groupings

    # if(!all(roi_choices %in% sapply(old_val, `[[`, 'conditions'))) {
    ravedash::logger("LOAD ROI COND into SEL")

    # load into selector
    dipsaus::updateCompoundInput2(session=session,
                                  inputId = 'custom_roi_groupings',
                                  initialization = list(conditions =list(choices = roi_choices), label=list(placeholder='ROI group name')),
                                  value = old_val
    )

    ##update the exporter to allow for addition of the custom ROI variable
    shiny::updateSelectInput(inputId = 'electrodes_to_export_roi_name',
                             choices = c('none', local_data$available_roi_vars, 'Custom ROI'))

    # }
    # enable_custom_ROI = isTRUE(),
    # custom_roi_type = input$custom_roi_type,
    # custom_roi_groupings = input$custom_roi_groupings,
  }

  # track changes to by trial grouped plot
  shiny::bindEvent(
    ravedash::safe_observe({

      any_changes = FALSE

      for(nm in names(local_data$grouped_plot_options)) {
        lbl <- paste0('btp_', nm)

        if(nm == 'types') {
          if(!setequal(input[[lbl]], local_data$grouped_plot_options[[nm]])) {
            any_changes = TRUE
            local_data$grouped_plot_options[[nm]] <- input[[lbl]]
          }
        } else if(!is.null(input[[lbl]])) {
          # ravedash::logger("observe plot options change")
          if(any(input[[lbl]] != local_data$grouped_plot_options[[nm]])) {
            any_changes = TRUE
            local_data$grouped_plot_options[[nm]] <- input[[lbl]]
          }
        }
      }

      ## also check the plot options
      for(nm in names(local_data$grouped_plot_options$plot_options)) {
        lbl <- paste0('btp_', nm)

        if(!is.null(input[[lbl]])) {
          # ravedash::logger("observe plot options change")
          if(input[[lbl]] != local_data$grouped_plot_options$plot_options[[nm]]) {
            any_changes = TRUE
            local_data$grouped_plot_options$plot_options[[nm]] <- input[[lbl]]
          }
        }
      }

      if(any_changes) local_reactives$update_by_trial_plot = Sys.time()

    }), input$btp_types, input$btp_xvar, input$btp_panelvar, input$btp_gvar,
    input$btp_pt.alpha, input$btp_pt.cex,
    ignoreNULL=TRUE, ignoreInit=TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      local_reactives$update_over_time_plot = Sys.time()

    }), input$ot_condition_switch, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      str = paste("<b>Current tab:</b> ", input$over_time_tabset)

      shiny::showModal(shiny::modalDialog(
        title = "Download image data",
        size = "m",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::downloadButton(
            outputId = ns("do_download_over_time_tabset"),
            label = "Download image",
            class = "btn-primary"
          )
        ),
        shiny::HTML(str)
      ))

    }),
    input$over_time_tabset_camera, ignoreNULL = TRUE, ignoreInit = TRUE
  )

shiny::bindEvent(
  ravedash::safe_observe({
    print("test test test")
  }),
  input$test, ignoreNULL = TRUE, ignoreInit = TRUE
)

  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     local_reactives$pes_electrode_hover = input$pes_hover
  #   }),
  #   input$pes_hover, ignoreInit = TRUE
  # )

  get_omni_stat_row <- function(row) {
    stopifnot(shiny::isTruthy(local_data$results$omnibus_results$stats))

    omni_stats <- local_data$results$omnibus_results$stats
    pesc <- tryCatch(input$per_electrode_statistics_chooser, error=function(...) {
      'overall'
    })

    ind <- which(str_detect(rownames(omni_stats), pesc))

    if(row[1] %within% seq_along(ind)) {
      return(omni_stats[ind[row[1]],])
    }

    return(omni_stats[ind[1,],])
  }

  update_pes_clicks <- function(click, plot=c('m', 't', 'p'), select_mode,
                                operator_string="", transform_string="force") {

    plot_choices <- c('m', 't', 'p')
    plot = match.arg(plot, choices = plot_choices, several.ok = FALSE)

    # this should always be true, but maybe not if we're being called directly
    if(shiny::isTruthy(local_data$results$omnibus_results$stats)) {
      select_mode %?<-% input$pes_select_mode

      # for picking based on stats block
      omni_stats <- local_data$results$omnibus_results$stats

      # electrodes names from the columns
      elec_in_order = as.integer(colnames(omni_stats))

      # make sure the colnames are as we thought they were
      stopifnot(all(is.integer(elec_in_order)))

      # determine which row we're interested in
      yy <- get_omni_stat_row(which(plot==plot_choices))

      # currently labelled electrodes
      current_selection <- local_reactives$pes_selected_electrodes

      # click location, note x is integer, y is numeric
      new_loc = list(x=round(click$x), y=click$y)

      # dipsaus::cat2('select mode: ', input$pes_select_mode, level='INFO')
      if (select_mode == 'Label maker') {
        new_elec <- elec_in_order[new_loc$x]

        # add if new location, remove if old location
        if(new_elec %in% current_selection) {
          local_reactives$pes_selected_electrodes = current_selection[-which(new_elec == current_selection)]
        } else {
          local_reactives$pes_selected_electrodes <- sort(c(current_selection, new_elec))
        }

      } else {
        vals <- list('m' = 'mean', 't' = 't', 'p' = 'p')

        local_reactives$pes_manual_threshold <- list(
          'operand_string' = plot,
          'condition_string' =input$per_electrode_statistics_chooser,
          'operator_string' = operator_string,
          'comparator' = smart_round(click$y),
          'transform_string' = transform_string
        )

        # if(plot == 'p') {
        #   local_reactives$pes_manual_threshold$transform <- function(x) {-log10(x)}
        # }

        if (select_mode == 'Threshold |v| > x') {
          local_reactives$pes_manual_threshold$operator_string = '>'
          local_reactives$pes_manual_threshold$transform_string = 'abs'

        } else if (select_mode == 'Threshold v > x') {
          local_reactives$pes_manual_threshold$operator_string = '>'
        } else if (select_mode == 'Threshold v < x') {
          local_reactives$pes_manual_threshold$operator_string = '<'
        } else if (select_mode == 'Manual threshold') {

          # for manual thresholds, do no rounding
          local_reactives$pes_manual_threshold$comparator = click$y
        }

        # run the threshold check
        FF <- match.fun(local_reactives$pes_manual_threshold$operator_string)
        TX <- match.fun(local_reactives$pes_manual_threshold$transform_string)
        tx_y = TX(yy)
        pass = FF(tx_y, local_reactives$pes_manual_threshold$comparator)
        local_reactives$pes_selected_electrodes = elec_in_order[pass]
      }
    }

    # if nothing passes, use NULL rather than integer(0)
    if(length(local_reactives$pes_selected_electrodes) < 1) {
      local_reactives$pes_selected_electrodes = NULL
    }
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      update_pes_clicks(click=input$pes_click_m,plot='m')
    }),
    input$pes_click_m, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      update_pes_clicks(click=input$pes_click_t,plot='t')
    }),
    input$pes_click_t, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      # transform the click value (-log10(p)) back to a p-value
      # so thresholding works as expected
      click = input$pes_click_p
      click$y = 10^(-click$y)

      update_pes_clicks(click=click, plot='p')
    }),
    input$pes_click_p, ignoreInit = TRUE, ignoreNULL = TRUE
  )

  #
  #     local_reactives$pes_electrode_hover = input$pes_mean_click
  #   },
  #   input$pes_mean_click, ignoreInit = TRUE
  # )

  ### tracking changes to global plot options
  shiny::bindEvent(
    ravedash::safe_observe({
      pe_graphics_settings_cache$set(key='line_color_palette',
                                     signature=input$gpo_lines_palette,
                                     value = input$gpo_lines_palette)

      local_reactives$update_line_plots = Sys.time()

    }), input$gpo_lines_palette, ignoreNULL = TRUE, ignoreInit = TRUE
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

  # Register outputs

  #### 3d brain viewer
  output$brain_viewer <- threeBrain::renderBrain({
    basic_checks(local_reactives$update_3dviewer)
    df <- data.frame(t(local_data$results$omnibus_results$stats))

    # fix some column names
    names(df) = str_replace_all(names(df), '\\.\\.\\.', ' vs ')

    names(df) = str_replace_all(names(df), '\\.', ' ')

    names(df) = str_replace_all(names(df), '\\ $', '')

    df$Electrode = as.integer(rownames(df))

    res <- build_palettes_and_ranges_for_omnibus_data(df)

    local_data$brain$set_electrode_values(df)
    local_data$brain$render(outputId = "brain_viewer", session = session,
                            palettes=res$palettes, value_ranges=res$val_ranges,
                            control_display = FALSE, side_display=FALSE,
                            timestamp=FALSE)

  })

  output$brain_viewer_movies <- threeBrain::renderBrain({
    basic_checks(local_reactives$update_3dviewer)

    df <- local_data$results$over_time_by_electrode_and_group

    local_data$brain_movies$set_electrode_values(df)
    res <- build_palettes_and_ranges_for_omnibus_data(df)
    local_data$brain_movies$render(outputId = "brain_viewer_movies",
                                   session = session,
                                   palettes = res$palettes)
  })

  ### export button download handler

  shiny::bindEvent(
    ravedash::safe_observe({
      # if the requested type is CSV, the format must be flat, not tensor
      if(input$electrode_export_file_type %in% c('Compressed CSV', 'FST') &&
         input$electrode_export_data_type == 'tensor') {
        ravedash::show_notification('Only flattened data are supported with FST | CSV output',
                                    title='Export data type updated')

        shiny::updateSelectInput(inputId ='electrode_export_data_type', selected = 'flat')
      }

    }), input$electrode_export_file_type, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(input$electrode_export_data_type == 'tensor' &&
         input$electrode_export_file_type %in% c('Compressed CSV', 'FST')) {

        ravedash::show_notification('Only flattened data are supported with FST | CSV output',
                                    title='Export data type updated')

        shiny::updateSelectInput(inputId ='electrode_export_data_type', selected = 'flat')



      }
    }),
    input$electrode_export_data_type, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      dipsaus::shiny_alert2(title = "Preparing for exporting",
                            text = "...", icon = "info",
                            danger_mode = FALSE, auto_close = FALSE, buttons = FALSE)
      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2()
      })

      # make sure we have something to export
      els <- dipsaus::parse_svec(input$electrodes_to_export)
      avail = els[els %in% local_data$electrode_meta_data$Electrode]

      if(length(avail) < 1) {
        dipsaus::close_alert2()
        dipsaus::shiny_alert2(text="No electrodes selected for export",
                              title='Export not started',
                              auto_close = TRUE, buttons = list('OK'=TRUE))

        # clean out the exit expression
        on.exit({})
        return()
      }

      pipeline$set_settings(
        electrode_export_file_type = input$electrode_export_file_type,
        frequencies_to_export = input$frequencies_to_export,
        times_to_export = input$times_to_export,
        trials_to_export = input$trials_to_export,
        electrodes_to_export = input$electrodes_to_export,
        electrode_export_data_type = input$electrode_export_data_type,
        electrodes_to_export_roi_name = input$electrodes_to_export_roi_name,
        electrodes_to_export_roi_categories = input$electrodes_to_export_roi_categories
      )

      run_analysis(
        trigger_3dviewer = FALSE,
        force_settings = list(electrode_text=input$electrodes_to_export)
      )

      # make sure this is available for export later
      env <- pipeline$eval('data_for_export')
      dfe <- env$data_for_export

      kv <- list(
        'Output type' = pretty(dfe$type)
      )
      key = ifelse(dfe$type == 'tensor', 'Data size (Freq, Time, Trial, Elec)',
                   'Data size (Rows, Columns)')

      kv[[key]] = sapply(dfe$data_names, function(nm) {
        dd = unname(dim(dfe[[nm]]$data))

        paste0(c('[', paste(dd, collapse=', '), ']'), collapse='')
      }) %>% paste(collapse=', ')

      kv[['Output file type']] = env$electrode_export_file_type

      # description_str = "<h2>Data Description</h2>";
      tokens <- mapply(function(x,y) {
        sprintf("<p><strong>%s</strong>: %s</p>", x, y)
      }, names(kv), kv, SIMPLIFY =F, USE.NAMES = FALSE)

      str = do.call(paste, tokens)

      shiny::showModal(shiny::modalDialog(
        title = "Download export data",
        size = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::downloadButton(
            outputId = ns("do_download_export"),
            label = "Download data",
            class = "btn-primary"
          )
        ),
        shiny::HTML(str)
      ))
    }),
    input$btn_export_electrodes,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(ravedash::safe_observe({

    ## when this name changes, we need to look up the new category choices
    if(input$electrodes_to_export_roi_name != 'none') {

      # if we're using a Custom ROI, load up the newly created labels
      # we have to make sure there are underlying conditions for these
      # labels though. These seems like a possibly NULL, length==0 situation
      # be sure to check this when doing the checks for export
      if(input$electrodes_to_export_roi_name == 'Custom ROI') {
        groups <- sapply(input$custom_roi_groupings, `[[`, 'label')

        len <- sapply(input$custom_roi_groupings, function(x) {
          length(x$conditions)
        }) %>% sum

        if(len < 1) {
          new_choices <- character(0)
        } else {
          new_choices = unname(unlist(groups))
        }

      } else {
        new_choices=unique(local_data$electrode_meta_data[[input$electrodes_to_export_roi_name]])
      }

      shiny::updateSelectInput(inputId = 'electrodes_to_export_roi_categories',
                               choices = new_choices, selected = new_choices)
    }

  }),


  input$electrodes_to_export_roi_name, ignoreNULL = TRUE, ignoreInit = TRUE)

  output$do_download_export <- downloadHandler(
    filename=function(...) {

      extensions <- list(
        'HDF5' = '.h5',
        'Compressed CSV' = '.csv.zip',
        'FST' = '.fst',
        'RDS' = '.rdata'
      )

      paste0('power_explorer_output',
             format(Sys.time(), "%b_%d_%Y_%H_%M_%S"),
             extensions[[input$electrode_export_file_type]]
      )
    },
    content = function(conn) {
      env <- pipeline$eval("data_for_export")
      dfe <- env$data_for_export


      data_to_write <- dfe[dfe$data_names]
      if(dfe$type == 'flat_data') {
        # for flat data, grab the data out of the list
        data_to_write = data_to_write[[1]]$data
      }

      export_method <- switch(input$electrode_export_file_type,
                              'FST' = function() {
                                fst::write.fst(data_to_write, path = conn,
                                               compress=99)
                              },
                              'HDF5' = function() {
                                h5file = temp_file('h5_out', fileext = 'h5')

                                # recursive method for saving to h5 file
                                save_list_to_h5 <- function(v, nm) {
                                  if(is.list(v)) {
                                    mapply(save_list_to_h5, v,
                                           paste0(nm, '/', names(v)))
                                  } else {
                                    raveio::save_h5(x=v, file=h5file, name=nm,
                                                    level=ifelse(is.numeric(v), 4, 9),
                                                    ctype=ifelse(is.numeric(v), 'numeric', 'character'),
                                                    replace = TRUE
                                    )
                                  }
                                }

                                ## put the numeric data in a matrix
                                ## put the categorical data in separate variables?
                                mapply(save_list_to_h5, data_to_write, names(data_to_write))

                                file.rename(h5file, conn)
                              },
                              'Compressed CSV' = function() {

                                tf <- temp_file(
                                  pattern = paste0('data_export_', format(Sys.time(), "%b_%d_%Y_%H_%M")),
                                  fileext = '.csv'
                                )
                                data.table::fwrite(data_to_write, file=tf)

                                zf <- temp_file(
                                  pattern = 'pow_expl_', fileext = '.csv.zip'
                                )
                                utils::zip(zipfile = zf, files = tf, extras='-j')

                                file.rename(zf, conn)
                              },
                              'RDS' = function() {
                                base::saveRDS(data_to_write, file=conn)
                              }
      )

      ravedash::with_error_notification({
        export_method()
        shiny::removeModal()
      })

    }
  )

  smart_round <- function(x) {
    m <- floor(log10(min(abs(x))))

    if(m > 0) {
      m = m-1
    }

    round(x, -m)
  }

  output$pes_threshold_string <- shiny::renderText({
    if(is.null(local_reactives$pes_manual_threshold)) {
      return ("")
    }

    pmt <- local_reactives$pes_manual_threshold

    sprintf("Current threshold: %s %s (%s) %s %s",
            ifelse(pmt$transform_string=='force', "", pmt$transform_string),
            pmt$operand_string,
            pmt$condition_string,
            pmt$operator_string,
            pmt$comparator
    )
  })

  output$per_electrode_results_table <- DT::renderDataTable({
    basic_checks(local_reactives$update_outputs)

    omnibus_data <- local_data$results$omnibus_results
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

  output$by_condition_grouped_plot <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    force(local_reactives$update_by_trial_plot)
    force(local_reactives$update_line_plots)

    data <- local_data$results$omnibus_results$data
    po <- local_data$grouped_plot_options

    # based on shiny input, change xvar/yvar
    count=function(x) {
      if(is.null(x)) return (1)
      length(unique(x))
    }

    k = 10 + 2.5*(count(data$Factor1)*count(data$AnalysisLabel)-1)
    if(!is.null(data$Factor2)) {
      k = k + 2.5 * (count(data$Factor2)-1)
    }

    MAX = 30
    layout(matrix(c(0,1,0), nrow=1), widths = c(1,lcm(min(k, MAX)),1))
    oom <- get_order_of_magnitude(median(abs(pretty(data$y))))

    line = 2.75 + oom

    #read in group label posision
    label_position = 'top' #c('none', 'bottom', 'top')


    ### if there is a panel var, split the data and do multiple plots
    po$panelvar = NULL

    # fix the names of xvar and gvar
    for(nm in c('xvar', 'gvar')) {
      if(po[[nm]] == 'First Factor') po[[nm]] = 'Factor1'
      if(po[[nm]] == 'Second Factor') po[[nm]] = 'Factor2'
      if(po[[nm]] == 'First:Second') po[[nm]] = 'Factor1Factor2'
      if(po[[nm]] == 'Analysis Group') po[[nm]] = 'AnalysisLabel'
    }

    # we need to collapse over electrode
    nms <- names(data)
    nms <- nms[!(nms %in% c('y', 'Electrode'))]

    # ravedash::logger(paste0(nms, collapse=', '))

    # microbenchmark::microbenchmark({
    # mat = aggregate(as.formula(paste0('y ~ ', paste(collapse='+',nms))), mean, data=data)
    # mat = mat[order(mat$Trial),]
    # }, {

    # assign('nms', nms, envir = globalenv())
    # assign('data', data, envir = globalenv())

    mat = data.table::data.table(data)
    mat = mat[, list(y=mean(get('y'))), keyby=nms]
    # }, times = 3, check = 'equivalent')

    par('mar'=c(4, 4.5+oom, ifelse(label_position=='top',3.5,2), 2)+.1)
    do.call(plot_grouped_data,
            append(po, list(mat=as.data.frame(mat), do_axes=TRUE, names.pos=label_position))
    )

    uoa = local_data$results$settings$baseline_settings$unit_of_analysis
    rave_axis_labels(ylab=uoa, outer=FALSE, line=line, xpd=TRUE)
  })

  output$activity_over_time_by_electrode <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    # check if we are in a multiple event situation
    etf_data <- local_data$results$by_electrode_tf_data
    repo <- local_data$results$repository
    analysis_groups <- local_data$results$analysis_groups
    # analysis_settings <- local_data$env$analysis_settings_clean
    baseline_settings <- local_data$results$baseline_settings

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
      maps <- vector('list', length = prod(length(etf_data),
                                           length(etf_data[[1]])))
      curr_map = 1
      for(ii in seq_along(etf_data)) {
        for(jj in seq_along(etf_data[[ii]])) {
          maps[[curr_map]] = etf_data[[ii]][[jj]]
          curr_map = curr_map + 1
        }
      }

      draw_many_heat_maps(
        hmaps = maps,
        byrow = FALSE,
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
    tf_data <- local_data$results$overall_tf_data

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

    repo <- local_data$results$repository
    analysis_groups <- local_data$results$analysis_groups
    analysis_settings <- local_data$results$analysis_settings_clean

    # check if current_freq_windows (from the UI) is different from the
    # pipeline settings
    baseline_settings <- local_data$results$baseline_settings

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
    tf_correlation_data <- local_data$results$tf_correlation_data
    repo <- local_data$results$repository
    analysis_groups <- local_data$results$analysis_groups
    analysis_settings <- local_data$results$analysis_settings_clean
    baseline_settings <- local_data$results$baseline_settings

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

  # output$per_electrode_statistics <- shiny::renderPlot({
  #   basic_checks(local_reactives$update_outputs)
  #
  #   stats <- local_data$results$omnibus_results$stats
  #
  #   if(exists('local_reactives') && !is.null(local_reactives$per_electrode_statistics_chooser)) {
  #     requested_stat = local_reactives$per_electrode_statistics_chooser
  #   }
  #
  #   plot_per_electrode_statistics(stats, requested_stat)
  # })




  get_threshold <- function(ptype) {
    th = NULL
    if(!is.null(local_reactives$pes_manual_threshold)) {
      if(local_reactives$pes_manual_threshold$operand == ptype) {
        th = local_reactives$pes_manual_threshold$comparator

        if(local_reactives$pes_manual_threshold$transform == 'abs') {
          th = sort(c(-th, th))
        }
      }
    }
    return(th)
  }

  output$per_electrode_statistics_mean <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    stats <- local_data$results$omnibus_results$stats

    lbl_elecs = NULL
    requested_stat = NULL

    if(!is.null(local_reactives$per_electrode_statistics_chooser)) {
      requested_stat = local_reactives$per_electrode_statistics_chooser

      if(!is.null(local_reactives$pes_selected_electrodes)) {
        lbl_elecs <- local_reactives$pes_selected_electrodes
      }


    }
    plot_per_electrode_statistics(stats, requested_stat, which_plots = 'm',
                                  draw_threshold=get_threshold('m'),
                                  label_electrodes=lbl_elecs, label_type = input$pes_label_type)
  })
  output$per_electrode_statistics_tstat <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    stats <- local_data$results$omnibus_results$stats

    lbl_elecs = NULL
    requested_stat = NULL

    if(exists('local_reactives') && !is.null(local_reactives$per_electrode_statistics_chooser)) {
      requested_stat = local_reactives$per_electrode_statistics_chooser

      if(!is.null(local_reactives$pes_selected_electrodes)) {
        lbl_elecs <- local_reactives$pes_selected_electrodes
      }
    }

    plot_per_electrode_statistics(stats, requested_stat, which_plots = 't',
                                  draw_threshold=get_threshold('t'),
                                  label_electrodes = lbl_elecs, label_type = input$pes_label_type)
  })
  output$per_electrode_statistics_fdrp <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    stats <- local_data$results$omnibus_results$stats

    lbl_elecs = NULL
    requested_stat = NULL
    if(exists('local_reactives') && !is.null(local_reactives$per_electrode_statistics_chooser)) {
      requested_stat = local_reactives$per_electrode_statistics_chooser

      if(!is.null(local_reactives$pes_selected_electrodes)) {
        lbl_elecs <- local_reactives$pes_selected_electrodes
      }
    }
    plot_per_electrode_statistics(stats, requested_stat, which_plots = 'p',
                                  draw_threshold = get_threshold('p'),
                                  label_electrodes = lbl_elecs, label_type = input$pes_label_type)
  })

  ### by trial over time plot
  output$activity_over_time_by_trial <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    # check if we are in a multiple event situation
    otbt_data <- local_data$results$over_time_by_trial_data
    repo <- local_data$results$repository
    analysis_groups <- local_data$results$analysis_groups
    baseline_settings <- local_data$results$baseline_settings

    meta_data = list(
      subject_code = repo$subject$subject_code,
      groups=analysis_groups,
      baseline=baseline_settings
    )

    # the data are already in order, just plot them
    if(all(c('data', 'xlab', 'ylab') %in% names(otbt_data[[1]]))) {
      plot_over_time_by_trial_data(
        hmaps = otbt_data,
        meta_data = meta_data)
    } else {
      # we need to flatten the data before it can be plotted
      maps <- vector('list', length = prod(length(otbt_data),
                                           length(otbt_data[[1]])))
      curr_map = 1
      for(ii in seq_along(otbt_data)) {
        for(jj in seq_along(otbt_data[[ii]])) {
          maps[[curr_map]] = otbt_data[[ii]][[jj]]
          curr_map = curr_map + 1
        }
      }
      plot_over_time_by_trial_data(maps, meta_data)
    }
  })

  output$over_time_with_switch <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)
    force(local_reactives$update_line_plots)
    force(local_reactives$update_over_time_plot)

    switch (input$ot_condition_switch,
            'Separate all' = {
              plot_over_time_by_condition(local_data$results$over_time_data,
                                          combine_events=FALSE, combine_conditions=FALSE)
            },
            'Combine conditions' = {
              plot_over_time_by_condition(local_data$results$over_time_data,
                                          combine_events = FALSE, combine_conditions=TRUE)
            },
            'Combine events' = {
              plot_over_time_by_condition(local_data$results$over_time_data,
                                          combine_events = TRUE, combine_conditions=FALSE)
            },
            'Combine all' = {
              plot_over_time_by_condition(local_data$results$over_time_data,
                                          combine_events = TRUE, combine_conditions=TRUE)
            }
    )
  })
}

