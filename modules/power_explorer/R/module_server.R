module_server <- function(input, output, session, ...){

  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL,
    update_line_plots = NULL,
    update_heatmap_plots = NULL,
    update_by_frequency_over_time_plot = NULL,
    update_over_time_by_trial_plot = NULL,
    update_by_frequency_correlation_plot = NULL,
    update_3dviewer = NULL,
    update_by_condition_plot = NULL,
    update_pairwise_contrasts = NULL,
    update_over_time_plot = NULL,
    current_analysis_settings=NULL,
    update_per_electrode_results_table=NULL,
    update_pes_plot=NULL,
    update_by_electrode_custom_plot=NULL,
    bec_only_selected_electrodes = 'All Electrodes',
    update_click_info = NULL,
    update_click_table = NULL,
  )

  brain_proxy <- threeBrain::brain_proxy("brain_viewer", session = session)
  brain_proxy_movies <- threeBrain::brain_proxy("brain_viewer_movies",
                                                session = session)

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  local_data$pes <- list(
    per_electrode_statistics_chooser=NULL,
    pes_selected_electrodes=NULL,
    pes_manual_threshold=NULL
  )

  local_data$available_electrodes = integer()
  local_data$epoch_table=NULL

  local_data$download_plot_info <- list(
    id = 'none',
    w = 7, h = 4,
    file_type = 'pdf'
  )

  # the defaults here should match the defaults in the UI / plot function
  local_data$grouped_plot_options <- list(
    'xvar' = 'Factor1',
    'gvar' = 'AnalysisLabel',
    'yvar' = 'y',
    'basic_unit' = 'Trials',
    'panelvar' = 'none',
    'plot_options' = list('pt.alpha' = 100, 'pt.cex' = 1),
    'types' = c('jitter points', 'means', 'ebar polygons'),
    'jitter_seed' = Sys.time(),
    'plot_width_scale' = 1,
    'highlight_clicks' = c('labels'),
    'highlight_text_location' = 'top'
  )
  # this item will store a list of data + points locations
  local_data$by_condition_by_trial_data_locations <- NULL
  # this stores the trial numbers of the clicked points
  local_data$bcbt_click_log <- NULL
  local_data$trial_outliers_list <- NULL
  # this stores the whole table (some trials have multiple rows, so can't just
  # index into bcbt_click_log to look up details)
  local_data$bcbt_click_table <- NULL

  local_data$by_electrode_custom_plot_options <- list(
    'plot_options' = list(pch=19, pt.cex=1, pt.alpha=100, plot_width_scale=1),
    'yvars' = 'overall',
    'yunit' = 'm',
    'xvars' = 'electrode',
    'xunit' = 'm',
    'collapse_xvars' = PE_COLLAPSE_METHODS[1],
    'collapse_yvars' = PE_COLLAPSE_METHODS[1],
    'plot_decorations' = list()
  )

  # this is used to get ROI variables and provide extra columns for table
  local_data$electrode_meta_data <- NULL

  # All of these heat map plots should have the same settings
  local_data$by_frequency_correlation_plot_options <- list(
    max_zlim = 1,
    percentile_range = FALSE,
    ncol = 3, byrow=TRUE,
    show_window = TRUE,
    xlim = c(0,1)
  )

  local_data$by_frequency_over_time_plot_options <- list(
    max_zlim = 99,
    percentile_range = TRUE,
    ncol = 3, byrow=TRUE,
    show_window = TRUE,
    xlim = c(0,1)
  )

  local_data$over_time_by_trial_plot_options <- list(
    max_zlim = 99,
    percentile_range = TRUE,
    ncol = 3, byrow=TRUE,
    show_window = TRUE,
    xlim = c(0,1)
  )

  ### ---

  # get server tools to tweak
  server_tools <- ravedash::get_default_handlers(session = session)

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
                           progress, ..., extra_names=c()) {
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


    # ensure mutually exclusive trial categories (no duplicate trial types across diff levels of a factor)
    fcg <- input$first_condition_groupings
    all_fcg <- lapply(fcg, `[[`, 'conditions')
    if(any(duplicated(unname(unlist(all_fcg))))) {
      ravedash::show_notification(title = "Duplicated conditions in First Factor",
                                  "Duplicated trial types across multiple levels of the same factor. Keeping only the first usage", autohide = FALSE)

      for(ii in seq_along(all_fcg)[-1]) {
        other_cond <- unname(unlist(all_fcg[1:(ii-1)]))

        fcg[[ii]]$conditions = setdiff(all_fcg[[ii]], other_cond)

        if(length(fcg[[ii]]$conditions) < 1) {
          ravedash::show_notification(title="Insufficient Data",
                                      "No conditions available for analysis after removing duplicates. Analysis not run.", type='danger',
                                      autohide = FALSE)
          return()
        }
      }
    }

    scg <- input$second_condition_groupings
    if(isTRUE(input$enable_second_condition_groupings)) {
      all_scg <- c(unname(unlist(sapply(scg, `[[`, 'conditions'))))
      if(any(duplicated(unname(unlist(all_scg))))) {
        ravedash::show_notification(title = "Duplicated conditions in Second Factor",
                                    "Duplicated trial types across multiple levels of the same factor for second factor. Keeping only the first usage", autohide = FALSE)

        for(ii in seq_along(all_scg)[-1]) {
          other_cond <- unname(unlist(all_scg[1:(ii-1)]))

          scg[[ii]]$conditions = setdiff(all_scg[[ii]], other_cond)

          if(length(scg[[ii]]$conditions) < 1) {
            ravedash::show_notification(title="Insufficient Data",
                                        "No conditions available for analysis after removing duplicates. Analysis not run.", type='danger',
                                        autohide = FALSE)
            return()
          }
        }
      }
    }

    ##----


    # check for duplicated analysis settings
    as <- input$ui_analysis_settings

    if(length(as) > 1) {

      is_overlapped <- function(a1, a2) {
        get_containment_type <- function(varname) {
          v1 = as.numeric(a1[[varname]])
          v2 = as.numeric(a2[[varname]])

          re <- if(all(v1 %within% v2) || all(v2 %within% v1)){
            2
          } else if (
            any(v1 %within% v2) || any(v2 %within% v1)) {
            1
          }

          return(re)
        }

        f1 <- f2 <- 0
        if(a1$event == a2$event) {
          # events match, check if time and frequencies are heavily overlapped
          f1 <- get_containment_type('time')
          f2 <- get_containment_type('frequency')
        }

        return(f1*f2)
      }

      re <- combn(seq_along(as), 2, FUN = function(ij) {
        is_overlapped(a1 = as[[ij[1]]], a2 = as[[ij[2]]])
      })

      if(any(re >= 4)) {
        ravedash::show_notification(
          message = paste0(
            "Two or more analysis settings are identical or have excessive overlap.\n"
            ,"Please check event names, time windows, and frequency windows"),
          title = "Analysis not run", type = "danger"
        )
        return()
      } else if (any(re > 0)) {
        ravedash::show_notification(
          message = "Two or more analysis settings have some overlap. If the analysis fails, please check event names, time windows, and frequency windows",
          title = "Analysis run with warning", type = "warning"
        )
      }

    }



    # Pipeline setup and run
    pipeline$set_settings(
      baseline_settings = list(
        window=list(input$baseline_window),
        scope = input$baseline_scope,
        unit_of_analysis = input$baseline_unit
      ),
      analysis_electrodes = settings$analysis_electrodes,
      condition_variable = input$condition_variable,
      first_condition_groupings = fcg,
      second_condition_groupings = scg,
      enable_second_condition_groupings = isTRUE(input$enable_second_condition_groupings),
      enable_custom_ROI = isTRUE(input$enable_custom_ROI),
      custom_roi_type = input$custom_roi_type,
      custom_roi_variable = input$custom_roi_variable,
      custom_roi_groupings = input$custom_roi_groupings,
      analysis_settings = input$ui_analysis_settings,
      omnibus_includes_all_electrodes = isTRUE(input$omnibus_includes_all_electrodes),
      # TODO: create UIs for time censor and trial outliers
      time_censor = list(
        enabled = FALSE,
        window = c(0, 1)
      ),
      trial_outliers_list = local_data$trial_outliers_list,
      # electrode_export_file_type = input$electrode_export_file_type,
      # electrode_export_data_type = input$electrode_export_data_type,
      electrodes_to_export_roi_name = input$electrodes_to_export_roi_name,
      electrodes_to_export_roi_categories = input$electrodes_to_export_roi_categories,
      frequencies_to_export = input$frequencies_to_export,
      times_to_export = input$times_to_export,
      trials_to_export = input$trials_to_export,
      electrodes_to_export = input$electrodes_to_export
    )


    ## target list
    eval_names <- c('by_frequency_over_time_data',
                    'by_frequency_correlation_data',
                    'over_time_by_trial_data',
                    'over_time_by_electrode_data',
                    # 'over_time_by_electrode_dataframe',
                    'omnibus_results',
                    'over_time_by_condition_data',
                    'across_electrode_statistics'
    )
    if(isTRUE(input$do_over_time_by_electrode_dataframe)) {
      eval_names %<>% c('over_time_by_electrode_dataframe')
    } else {
      local_data$results['over_time_by_electrode_dataframe'] = NULL
    }

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

    progress$inc("Starting pipeline [main loop]")

    # in the initial run we're just checking the settings and
    # doing the baseline

    normal_names = c('analysis_settings_clean',
                     'baseline_settings',
                     'baselined_power',
                     'analysis_groups',
                     'pluriform_power')

    if(isTRUE(input$quick_omnibus_only)) {
      results <- pipeline$run(
        as_promise = FALSE,
        scheduler = "none",
        type = "smart",
        callr_function = NULL,
        # progress_title = "Calculating in progress",
        async = FALSE,
        # check_interval = 0.1,
        shortcut = FALSE,
        names = c('over_time_by_electrode_data', 'omnibus_results')
      )

      local_data$results <- results
      # list(
      #   omnibus_results = results
      # )

    } else {
      target_names <- unique(c(normal_names, extra_names, eval_names))

      results <- pipeline$run(
        as_promise = FALSE,
        scheduler = "none",
        type = "smart",
        callr_function = NULL,
        # progress_title = "Calculating in progress",
        async = FALSE,
        # check_interval = 0.1,
        shortcut = FALSE,
        names = target_names
      )
      #local_data = list()
      local_data$results <- results
    }

    ravedash::logger(
      'RESULTS AVAIL:\n', paste0(collapse=', ', names(local_data$results))
    )

    # progress$inc("Evaluating pipeline targets")
    # here we're transforming the pluriform data into the graphs that we need
    # eval_names <- c('by_frequency_over_time_data',
    #                 'by_frequency_correlation_data',
    #                 'over_time_by_trial_data',
    #                 'over_time_by_electrode_data',
    #                 # 'over_time_by_electrode_dataframe',
    #                 'omnibus_results',
    #                 'over_time_by_condition_data'
    # )
    # if(isTRUE(input$do_over_time_by_electrode_dataframe)) {
    #   eval_names %<>% c('over_time_by_electrode_dataframe')
    # } else {
    #   local_data$results['over_time_by_electrode_dataframe'] = NULL
    # }

    # local_data$results[eval_names] <- as.list(pipeline$eval(names = eval_names,
    # shortcut=FALSE))[eval_names]
    progress$inc("Done pipeline eval")

    ### bring down the univariate stats to a more convenient place
    # so they fit the data/plotting naming convention
    local_data$results$by_electrode_graphical_results_data <-
      local_data$results$omnibus_results$stats

    local_data$results$by_condition_by_trial_data <- local_data$results$omnibus_results$data
    local_data$results$by_condition_by_trial_data_with_outliers <- local_data$results$omnibus_results$data_with_outliers

    local_data$results$by_electrode_custom_plot_data <- local_data$results$omnibus_results$stats

    or <- rownames(local_data$results$omnibus_results$stats)
    choices_list <- unique(stringr::str_remove_all(or, '(m+\\(|t+\\(|p+\\(|p_fdr\\(|\\))'))

    # remove choices we don't care about
    choices_list = choices_list[choices_list!='currently_selected']

    ## update the by-electrode analysis viewer switcher
    current_choice <- input$per_electrode_statistics_chooser
    shiny::updateSelectInput(inputId = 'per_electrode_statistics_chooser',
                             choices = choices_list, selected = current_choice %OF% choices_list)

    # remove more choices we don't care about
    ind = stringr::str_detect(choices_list, ' - ')
    starting_contrasts = which(ind)[1]

    if(!is.na(starting_contrasts) && starting_contrasts > 1) {
      choices_list = choices_list[1:(starting_contrasts-1)]
    }

    # update custom plot options
    selY = if(input$bec_yvar_is_multi) {
      input$bec_yvar_chooser_multi
    } else {
      input$bec_yvar_chooser
    }
    shiny::updateSelectInput(inputId = 'bec_yvar_chooser',
                             choices = choices_list, selected = selY[1] %OF% choices_list)
    shiny::updateSelectInput(inputId = 'bec_yvar_chooser_multi',
                             choices = choices_list, selected = selY[selY %in% choices_list])


    choices_list <- c('electrode', choices_list)
    selX = if(input$bec_xvar_is_multi) {
      input$bec_xvar_chooser_multi
    } else {
      input$bec_xvar_chooser
    }
    shiny::updateSelectInput(inputId = 'bec_xvar_chooser',
                             choices = choices_list, selected = selX[1] %OF% choices_list)

    shiny::updateSelectInput(inputId = 'bec_xvar_chooser_multi',
                             choices = choices_list, selected = selX[selX %in% choices_list])


    # update the variable selector for the table as well
    bet_vts <- input$bet_variables_to_hide
    bet_vts <- bet_vts[bet_vts %in% choices_list]
    if(length(bet_vts) < 1) bet_vts = character(0)

    # remove electrode as a column choice
    bet_vts <- bet_vts[tolower(bet_vts) != 'electrode']

    shiny::updateSelectInput(inputId = 'bet_variables_to_hide',
                             choices = choices_list[choices_list!='electrode'],
                             selected = bet_vts)


    # add in electrode meta data chooser
    bet_md <- input$bet_meta_data
    md_choices <- c('Subject', 'MNI152_x', 'MNI152_y', 'MNI152_z', 'Label', 'FSLabel') %>%
      intersect(names(local_data$electrode_meta_data))
    bet_md <- bet_md[bet_md %in% md_choices]
    if(length(bet_md) < 1) bet_md = character(0)

    shiny::updateSelectInput(inputId = 'bet_meta_data',
                             choices = md_choices,
                             selected = bet_md)

    #### this is where we add Factor 2 to the analysis
    if(is.null(local_data$results$omnibus_results$data$Factor2)) {
      shiny::updateSelectInput(inputId = 'btp_xvar',
                               choices=c('First Factor', 'Analysis Group')
      )

      shiny::updateSelectInput(inputId = 'btp_gvar',
                               choices=c('none', 'Analysis Group', 'First Factor')
      )

    } else {
      shiny::updateSelectInput(inputId = 'btp_xvar',
                               choices=c('First Factor', 'Second Factor', 'First:Second', 'Analysis Group')
      )

      shiny::updateSelectInput(inputId = 'btp_gvar',
                               choices=c('none', 'Analysis Group', 'First Factor', 'Second Factor', 'First:Second')
      )


      shiny::updateSelectInput(inputId = 'btp_panelvar',
                               choices=c('none', 'First Factor', 'Second Factor', 'First:Second', 'Analysis Group')
      )
    }


    if(isTRUE(input$quick_omnibus_only)) {
      local_reactives$update_pes_plot <- Sys.time()
    } else {
      ## set the plot time range limits for plots
      newly_available_time <- get_recursive_summary(
        local_data$results$over_time_by_condition_data, 'x'
      )
      shiny::updateSliderInput(session = session, inputId = 'over_time_by_condition_plot_range',
                               value = range(newly_available_time),
                               min = min(newly_available_time),
                               max = max(newly_available_time)
      )

      # update the timing range variable for heatmap plots
      sapply(c('bfot', 'otbt'), function(nm) {
        shiny::updateSliderInput(session = session, inputId = paste0(nm, '_xlim'),
                                 value = range(newly_available_time),
                                 min = min(newly_available_time),
                                 max = max(newly_available_time)
        )
      })


      # we need to check which kind of contrasts are available
      aes <- local_data$results$across_electrode_statistics
      if(length(aes$fixed_effects) > 1) {
        shiny::updateSelectInput(session, inputId = 'bcs_choose_contrasts',
                                 choices=c('All-possible pairwise',
                                           'Stratified contrasts (more power!)',
                                           'ITX Contrasts (diff of diff)')
        )

        update_contrast_choices()

      } else {
        shiny::updateSelectInput(session, inputId = 'bcs_choose_contrasts',
                                 selected = c('All-possible pairwise'), choices = c('All-possible pairwise')
        )
      }

      spec_choices = 'None available'
      if(!is.null(aes$stratified_contrasts)) {
        spec_choices = names(aes$stratified_contrasts)
      }

      local_reactives$update_outputs <- Sys.time()
      # pes plots have a separate update cycle
      local_reactives$update_pes_plots <- Sys.time()
    }

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
    local_data$pes$per_electrode_statistics_chooser = input$per_electrode_statistics_chooser

    local_reactives$update_pes_plot = Sys.time()

  }), input$per_electrode_statistics_chooser, ignoreNULL = FALSE, ignoreInit=TRUE)


  ### select electrodes using the univariate stat output
  shiny::bindEvent(ravedash::safe_observe({

    if(input$pes_select_mode %in% c('Clear labels', 'Invert selection', 'Manual threshold')) {
      on.exit({
        shiny::updateSelectInput(inputId = 'pes_select_mode', selected = 'Label maker')
      })
    }

    if(input$pes_select_mode=='Clear labels') {
      local_data$pes$pes_selected_electrodes=NULL
      local_data$pes$pes_manual_threshold=NULL

    } else if (input$pes_select_mode == 'Invert selection') {
      if(shiny::isTruthy(local_data$results$omnibus_results$stats)) {
        curr <- local_data$pes$pes_selected_electrodes
        el_numbers <- as.integer(colnames(local_data$results$omnibus_results$stats))
        if(is.null(curr)) {
          local_data$pes$pes_selected_electrodes = el_numbers
        } else {
          local_data$pes$pes_selected_electrodes = setdiff(el_numbers, curr)
        }

        ## if there is a currently set threshold, flip that as well
        if(!is.null(local_reactives$pes_manual_threshold)) {
          curr = local_data$pes$pes_manual_threshold$operator_string
          new = switch(curr,
                       ">" = "<=",
                       "<" = ">=",
                       ">=" = "<",
                       "<=" = ">",
                       "==" = "!=",
                       "!=" = "=="
          )
          local_data$pes$pes_manual_threshold$operator_string = new
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

    etext <- dipsaus::deparse_svec(local_data$pes$pes_selected_electrodes)
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



  ### track changes to by_electrode_custom_plot -- NB: can't change state of hidden input


  # handle adding plot decorations
  shiny::bindEvent(ravedash::safe_observe({

    if(is.null(input$bec_plot_decorators)) {
      local_data$by_electrode_custom_plot_options$plot_decorations = list()
    } else {
      local_data$by_electrode_custom_plot_options$plot_decorations = unlist(input$bec_plot_decorators)
    }

    local_reactives$update_by_electrode_custom_plot = Sys.time()

  }), input$bec_plot_decorators, ignoreInit=TRUE, ignoreNULL=FALSE)


  # handle toggle multi/single
  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$yvars =
      if(input$bec_yvar_is_multi) {
        input$bec_yvar_chooser_multi
      } else {
        input$bec_yvar_chooser
      }
    local_reactives$update_by_electrode_custom_plot = Sys.time()
  }), input$bec_yvar_is_multi, ignoreInit=TRUE, ignoreNULL=TRUE)


  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$xvars =
      if(input$bec_xvar_is_multi) {
        input$bec_xvar_chooser_multi
      } else {
        input$bec_xvar_chooser
      }

    local_reactives$update_by_electrode_custom_plot = Sys.time()
  }), input$bec_xvar_is_multi, ignoreInit=TRUE, ignoreNULL=TRUE)

  shiny::bindEvent(ravedash::safe_observe({

    local_data$by_electrode_custom_plot_options$xvars =
      if(input$bec_xvar_is_multi) {
        input$bec_xvar_chooser_multi
      } else {
        input$bec_xvar_chooser
      }

    local_reactives$update_by_electrode_custom_plot = Sys.time()

  }), input$bec_xvar_chooser, input$bec_xvar_chooser_multi, ignoreInit=TRUE, ignoreNULL=FALSE)


  ## changing collapse function

  shiny::bindEvent(ravedash::safe_observe({

    local_data$by_electrode_custom_plot_options$collapse_yvars = input$bec_yvar_collapser
    local_reactives$update_by_electrode_custom_plot = Sys.time()

  }), input$bec_yvar_collapser, ignoreInit = TRUE, ignoreNULL = TRUE)


  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$collapse_xvars = input$bec_xvar_collapser
    local_reactives$update_by_electrode_custom_plot = Sys.time()

  }), input$bec_xvar_collapser, ignoreInit = TRUE, ignoreNULL = TRUE)


  ##-


  shiny::bindEvent(ravedash::safe_observe({

    local_data$by_electrode_custom_plot_options$yvars =
      if(input$bec_yvar_is_multi) {
        input$bec_yvar_chooser_multi
      } else {
        input$bec_yvar_chooser
      }

    local_reactives$update_by_electrode_custom_plot = Sys.time()

  }), input$bec_yvar_chooser, input$bec_yvar_chooser_multi, ignoreInit=TRUE, ignoreNULL=TRUE)

  shiny::bindEvent(ravedash::safe_observe({
    if(!is.null(local_data$results)) {

      # default is to use the whole dataset
      mat = local_data$results$omnibus_results$stats

      if(input$bec_only_selected_electrodes == 'Currently selected') {
        ind <- which(rownames(mat) == 'currently_selected')
        if(length(ind) && sum(mat[ind,]) > 0) {
          ci = which(mat[ind,]==1)
          mat = mat[,ci,drop=FALSE]
        }
      }

      local_data$results$by_electrode_custom_plot_data <- mat
      local_reactives$update_by_electrode_custom_plot = Sys.time()
    }

  }), input$bec_only_selected_electrodes, ignoreInit=TRUE, ignoreNULL=TRUE)

  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$yunit = input$bec_yvar_unit

    local_reactives$update_by_electrode_custom_plot = Sys.time()
  }), input$bec_yvar_unit, ignoreInit=TRUE, ignoreNULL=TRUE)

  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$xunit = input$bec_xvar_unit

    local_reactives$update_by_electrode_custom_plot = Sys.time()
  }), input$bec_xvar_unit, ignoreInit=TRUE, ignoreNULL=TRUE)

  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$plot_options$plot_width_scale = max(0.1, as.numeric(input$bec_plot_width_scale), na.rm=TRUE)
    local_reactives$update_by_electrode_custom_plot = Sys.time()

  }), input$bec_plot_width_scale, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::bindEvent(ravedash::safe_observe({
    local_reactives$bec_only_selected_electrodes = input$bec_only_selected_electrodes
  }), input$bec_only_selected_electrodes, ignoreNULL = TRUE, ignoreInit = TRUE)


  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$plot_options$pt.alpha = input$bec_pt.alpha

    local_reactives$update_by_electrode_custom_plot = Sys.time()
  }), input$bec_pt.alpha, ignoreNULL = TRUE, ignoreInit = TRUE)

  shiny::bindEvent(ravedash::safe_observe({
    local_data$by_electrode_custom_plot_options$plot_options$pt.cex = input$bec_pt.cex
    local_reactives$update_by_electrode_custom_plot = Sys.time()
  }), input$bec_pt.cex, ignoreNULL = TRUE, ignoreInit = TRUE)


  # handle a newly uploaded settings file
  update_all_settings <- function(settings_list) {

    ravedash::show_notification('Loading new settings from file', "Settings update")

    ## baseline
    shiny::updateSliderInput(session = session, inputId = 'baseline_window',
                             value = unname(unlist(settings_list$baseline_settings$window)),
    )
    shiny::updateSelectInput(session = session, inputId = 'baseline_scope',
                             selected = settings_list$baseline_settings$scope
    )

    # condition variable
    shiny::updateSelectInput(session = session, inputId = 'condition_variable',
                             selected = settings_list$condition_variable
    )

    ## analysis settings
    as <- settings_list$analysis_settings
    n_analysis <- length(as)

    dipsaus::updateCompoundInput2(
      session = session,
      inputId = 'ui_analysis_settings',
      value=as, ncomp = n_analysis
    )

    fcg <- settings_list$first_condition_groupings

    dipsaus::updateCompoundInput2(session = session,
                                  inputId = 'first_condition_groupings',
                                  value = fcg, ncomp = length(fcg))

    # default condition groups for factor 2
    val_scg <- settings_list$second_condition_groupings

    dipsaus::updateCompoundInput2(session = session,
                                  inputId = 'second_condition_groupings',
                                  value = val_scg, ncomp = length(val_scg)
    )

    shiny::updateSelectInput(session=session,
                             inputId = 'custom_roi_variable',
                             selected=settings_list$custom_roi_variable,
    )


    # reset all outputs
    local_reactives$update_outputs <- NULL
    local_reactives$update_line_plots <- NULL
    local_reactives$update_heatmap_plots <- NULL
    local_reactives$update_3dviewer <- NULL
    local_reactives$update_by_condition_plot <- NULL
    local_reactives$outliers_updated <- NULL
    local_reactives$update_over_time_plot <- NULL
    local_reactives$update_pairwise_contrasts <- NULL
  }


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

      tmp <- names(new_repository$epoch$table)
      condition_vars <- tmp[which(stringr::str_detect(tmp, 'Condition'))]

      shiny::updateSelectInput(session = session, inputId = 'condition_variable',
                               choices = condition_vars,
                               selected = input$condition_variable %OF% condition_vars
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
      available_events <- get_available_events(columns=new_repository$epoch$columns)
      as <- lapply(as, function(val) {
        val$event <- val$event %OF% available_events
        val
      })

      n_analysis <- length(as)
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = 'ui_analysis_settings',
        initialization = list(
          event = list(
            choices = get_available_events(columns=new_repository$epoch$columns),
            selected = 'Trial Onset'
          ),
          time = list(min=min(new_repository$time_points), max=max(new_repository$time_points)),
          frequency = list(min=min(new_repository$frequency), max=max(new_repository$frequency))
        ), value=as, ncomp = n_analysis
      )


      ## set the plot time range limits for plots
      shiny::updateSliderInput(session = session, inputId = 'over_time_by_condition_plot_range',
                               value = range(new_repository$time_points),
                               min = min(new_repository$time_points),
                               max = max(new_repository$time_points)
      )


      # update the timing range variable for heatmap plots
      pnames = c('bfot' = 'by_frequency_over_time_plot',
                 'bfc' = 'by_frequency_correlation_plot',
                 'otbt' = 'over_time_by_trial_plot')

      sapply(c('bfot', 'otbt'), function(nm) {
        shiny::updateSliderInput(session = session, inputId = paste0(nm, '_xlim'),
                                 value = range(new_repository$time_points),
                                 min = min(new_repository$time_points),
                                 max = max(new_repository$time_points)
        )
      })




      ## default condition groups
      cvar = 'Condition'
      if(input$condition_variable != "Condition") {
        tmp_cvar = input$condition_variable
        if(!is.null(new_repository$epoch$table[[tmp_cvar]])) {
          cvar = tmp_cvar
        }
      }

      local_data$epoch_table = new_repository$epoch$table

      cond_tbl <- table(new_repository$epoch$table[[cvar]])
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

      # let the Subject column be added
      local_data$electrode_meta_data$Subject = new_repository$subject$subject_code

      roi_vars <- names(which(sapply(local_data$electrode_meta_data, count_elements) %within% c(2, nrow(local_data$electrode_meta_data)-1)))
      roi_vars = roi_vars[!(roi_vars %in% c('isLoaded'))]

      local_data$available_roi_vars = roi_vars

      shiny::updateSelectInput(session=session,
                               inputId = 'custom_roi_variable',
                               selected="none",
                               choices = c("none", roi_vars)
      )

      # grab new brain
      brain <- raveio::rave_brain(new_repository$subject$subject_id)
      local_data$available_electrodes = new_repository$power$dimnames$Electrode

      # update export electrode categories
      new_choices = c('none', roi_vars)
      if(isTRUE(input$enable_custom_ROI)) {
        new_choices %<>% c('custom ROI')
      }

      shiny::updateSelectInput(inputId='electrodes_to_export_roi_name',
                               selected = 'none', choices = new_choices
      )

      # update the list of available forked pipelines
      update_available_forked_pipelines()

      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")
      # shidashi::reset_output("over_time_by_electrode_data")
      if(is.null(brain)) {
        # no 3D brain available, collapse the first cardset
        shidashi::card_operate(title = "Brain Viewers", method = "collapse")
      } else {
        shidashi::card_operate(title = "Brain Viewers", method = "expand")
      }
      local_reactives$update_outputs <- NULL
      local_reactives$update_line_plots <- NULL
      local_reactives$update_heatmap_plots <- NULL
      local_reactives$update_3dviewer <- NULL
      local_reactives$update_by_condition_plot <- NULL
      local_reactives$outliers_updated <- NULL
      local_reactives$update_over_time_plot <- NULL

      local_reactives$update_by_frequency_over_time_plot <- NULL
      local_reactives$update_over_time_by_trial_plot <- NULL
      local_reactives$update_by_frequency_correlation_plot <- NULL

      local_data$bcbt_click_log <- NULL

      #TODO update UI selectors to possibly cached values
    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )


  #### tracking clicks on 3dViewer
  track_3dviewer_clicks <- function(proxy) {
    shiny::bindEvent(
      ravedash::safe_observe({
        ravedash::logger('3dBrain double click')
        ravedash::clear_notifications(class=ns('threedviewer'))

        info <- as.list(proxy$mouse_event_double_click)
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

      proxy$mouse_event_double_click,
      # brain_proxy_movies$mouse_event_double_click,
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
  }
  track_3dviewer_clicks(brain_proxy)
  track_3dviewer_clicks(brain_proxy_movies)

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

  get_available_conditions <- function() {
    cvar = 'Condition'

    if(input$condition_variable != "Condition") {
      tmp_cvar = input$condition_variable
      if(!is.null(local_data$epoch_table[[tmp_cvar]])) {
        cvar = tmp_cvar
      }
    }

    cond_tbl <- table(local_data$epoch_table[[cvar]])
    cond_tbl <- cond_tbl[order(names(cond_tbl))]
    conditions <- names(cond_tbl)
  }

  shiny::bindEvent(
    ravedash::safe_observe({

      conditions <- get_available_conditions()

      val <- list(
        list(label = "All Conditions", conditions = conditions)
      )

      all_fcg_conditions <- unname(unlist(sapply(val, `[[`, 'conditions')))
      dipsaus::updateCompoundInput2(session = session,
                                    inputId = 'first_condition_groupings',
                                    initialization = list(conditions =
                                                            list(choices = conditions)
                                    ),
                                    value = val, ncomp = length(val))

    }), input$condition_variable, ignoreNULL = TRUE, ignoreInit=TRUE
  )


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

      # if(any_changes) {
      # if we've made changes to the plot, the clicks need to be adjusted
      local_reactives$update_by_condition_plot = Sys.time()
      # local_data$bcbt_click_log <- NULL
      # }

    }), input$btp_types, input$btp_xvar, input$btp_panelvar, input$btp_gvar,
    input$btp_pt.alpha, input$btp_pt.cex, input$btp_basic_unit,
    ignoreNULL=TRUE, ignoreInit=TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      local_reactives$update_by_condition_plot = Sys.time()
    }), input$bcbt_show_outliers, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      local_data$grouped_plot_options$highlight_clicks <- input$btp_highlight_clicks
      local_data$grouped_plot_options$highlight_text_location <- input$btp_highlight_text_location

      local_reactives$update_by_condition_plot = Sys.time()

    }), input$btp_highlight_clicks, input$btp_highlight_text_location,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  update_contrast_choices <- function() {
    aes = local_data$results$across_electrode_statistics

    if(input$bcs_choose_contrasts == 'Stratified contrasts (more power!)') {
      shiny::updateSelectInput(session,
                               inputId = 'bcs_choose_specific_contrast',
                               selected=names(aes$stratified_contrasts)[1],
                               choices=names(aes$stratified_contrasts)
      )
    } else if (input$bcs_choose_contrasts == 'ITX Contrasts (diff of diff)') {
      shiny::updateSelectInput(session,
                               inputId = 'bcs_choose_specific_contrast',
                               selected=names(aes$itx_contrasts)[1],
                               choices=names(aes$itx_contrasts)
      )
    }
  }


  shiny::bindEvent(
    ravedash::safe_observe({
      local_reactives$update_pairwise_contrasts <- Sys.time()

      if(input$bcs_choose_contrasts != 'All-possible pairwise') {
        update_contrast_choices()
      }

    }), input$bcs_choose_contrasts,
    ignoreNULL=TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      local_reactives$update_pairwise_contrasts <- Sys.time()
    }), input$bcs_choose_specific_contrast,
    ignoreNULL=TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      local_reactives$update_over_time_plot = Sys.time()

    }), input$over_time_by_condition_switch, input$over_time_plot_range,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  update_heatmap_controls <- function(prefix, po_name) {
    optname = po_name %&% '_options'
    upname = 'update_' %&% po_name

    # ravedash::logger(level='warning', optname, upname)

    new_lim = abs(as.numeric(input[[prefix %&% '_range']]))

    # ignore the change if they've just (say) deleted the previous value
    if(any(is.na(new_lim), length(new_lim) == 0, !is.numeric(new_lim))) {
      local_data[[optname]]$max_zlim = 0
    } else {
      local_data[[optname]]$max_zlim = new_lim
    }

    local_data[[optname]]$percentile_range =
      isTRUE(input[[prefix %&% '_range_is_percentile']])

    local_data[[optname]]$ncol = input[[prefix %&% '_ncol']]

    local_data[[optname]]$byrow = input[[prefix %&% '_byrow']]

    local_data[[optname]]$show_window = input[[prefix %&% '_show_window']]

    local_data[[optname]]$xlim = input[[prefix %&% '_xlim']]

    local_reactives[[upname]] = Sys.time()
  }

  # controls for heatmaps
  pnames = c('bfot' = 'by_frequency_over_time_plot',
             'bfc' = 'by_frequency_correlation_plot',
             'otbt' = 'over_time_by_trial_plot')

  mapply(function(prf, po) {
    shiny::bindEvent(
      ravedash::safe_observe({
        update_heatmap_controls(prf, po_name = po)
      }), input[[prf %&% '_range_is_percentile']], input[[prf %&% '_range']],
      input[[prf %&% '_ncol']], input[[prf %&% '_byrow']],
      input[[prf %&% '_show_window']], input[[prf %&% '_xlim']],
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
  }, names(pnames), pnames, SIMPLIFY = FALSE)


  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     update_heatmap_controls('otbt', po_name = pnames['otbt'])
  #   }), input[['otbt' %&% '_range_is_percentile']], input[['otbt' %&% '_range']],
  #   input[['otbt' %&% '_ncol']], input[['otbt' %&% '_byrow']],
  #   ignoreNULL = TRUE, ignoreInit = TRUE
  # )
  #
  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     update_heatmap_controls('bfc', po_name = pnames['bfc'])
  #   }), input[['bfc' %&% '_range_is_percentile']], input[['bfc' %&% '_range']],
  #   input[['bfc' %&% '_ncol']], input[['bfc' %&% '_byrow']],
  #   ignoreNULL = TRUE, ignoreInit = TRUE
  # )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::req(local_data$results)

      local_data$download_plot_info$id =
        paste0("over_time_", unpretty(input$over_time_tabset))

      build_modal_plot_download(local_data$download_plot_info)
    }),
    input$over_time_tabset_camera, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::req(local_data$results)
      local_data$download_plot_info$id =
        paste0("by_electrode_", unpretty(input$by_electrode_tabset))

      build_modal_plot_download(local_data$download_plot_info)
    }),
    input$by_electrode_tabset_camera, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::req(local_data$results)

      local_data$download_plot_info$id =
        paste0("by_condition_", unpretty(input$by_condition_tabset))
      build_modal_plot_download(local_data$download_plot_info)
    }),
    input$by_condition_tabset_camera, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # listen to camera on the by_frequency tabset
  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::req(local_data$results)
      local_data$download_plot_info$id =
        paste0("by_frequency_", unpretty(input$by_frequency_tabset))

      build_modal_plot_download(local_data$download_plot_info)
    }),
    input$by_frequency_tabset_camera, ignoreNULL = TRUE, ignoreInit = TRUE
  )



  get_omni_stat_row <- function(row) {
    stopifnot(shiny::isTruthy(local_data$results$omnibus_results$stats))

    omni_stats <- local_data$results$omnibus_results$stats
    pesc <- tryCatch(input$per_electrode_statistics_chooser, error=function(...) {
      'overall'
    })

    ind <- which(stringr::str_detect(rownames(omni_stats), pesc))

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
      current_selection <- local_data$pes$pes_selected_electrodes

      # click location, note x is integer, y is numeric
      new_loc = list(x=round(click$x), y=click$y)

      # dipsaus::cat2('select mode: ', input$pes_select_mode, level='INFO')
      if (select_mode == 'Label maker') {

        # weight in the Y-axis location to find nearest electrode
        tx_y = yy
        if(!is.null(local_data$pes$pes_manual_threshold$transform_string) &&
           local_data$pes$pes_manual_threshold$transform_string != "force") {
          TX <- match.fun(local_data$pes$pes_manual_threshold$transform_string)
          tx_y = TX(yy)
        }

        pt_dist = (abs(tx_y - click$y)^2 / max(tx_y)) +
          ((click$x - seq_along(elec_in_order))^2 / length(elec_in_order))

        new_elec <- elec_in_order[which.min(pt_dist)]

        # add if new location, remove if old location
        if(new_elec %in% current_selection) {
          local_data$pes$pes_selected_electrodes <- current_selection[-which(new_elec == current_selection)]
        } else {
          local_data$pes$pes_selected_electrodes <- sort(c(current_selection, new_elec))
        }

      } else {
        vals <- list('m' = 'mean', 't' = 't', 'p' = 'p')

        local_data$pes$pes_manual_threshold <- list(
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
          local_data$pes$pes_manual_threshold$operator_string = '>'
          local_data$pes$pes_manual_threshold$transform_string = 'abs'

        } else if (select_mode == 'Threshold v > x') {
          local_data$pes$pes_manual_threshold$operator_string = '>'
        } else if (select_mode == 'Threshold v < x') {
          local_data$pes$pes_manual_threshold$operator_string = '<'
        } else if (select_mode == 'Manual threshold') {

          # for manual thresholds, do no rounding
          local_data$pes$pes_manual_threshold$comparator = click$y
        }

        # run the threshold check
        FF <- match.fun(local_data$pes$pes_manual_threshold$operator_string)
        TX <- match.fun(local_data$pes$pes_manual_threshold$transform_string)
        tx_y = TX(yy)
        pass = FF(tx_y, local_data$pes$pes_manual_threshold$comparator)
        local_data$pes$pes_selected_electrodes = elec_in_order[pass]
      }
    }

    # if nothing passes, use NULL rather than integer(0)
    if(length(local_data$pes$pes_selected_electrodes) < 1) {
      local_data$pes$pes_selected_electrodes = NULL
    }


    local_reactives$update_pes_plot <- Sys.time()
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

  ### tracking changes to clicks on the by-trial by-condition plot

  is_paneling <- function(show_mesg=TRUE) {
    if(is.null(input$btp_panelvar) || input$btp_panelvar == 'none') {
      return (FALSE)

    } else {
      # if there is a panel variable, matching clicks to data is too tricky
      if(show_mesg) {
        ravedash::show_notification('Points cannot be labelled when
                                      panelling.', 'Locations not available.')
      }

      return (TRUE)
    }
  }

  points_are_trials <- function(show_mesg=TRUE) {
    # choices=c('Trials', 'Electrodes'))
    if(input$btp_basic_unit == 'Trials') {
      return (TRUE)

    } else {
      # if there is a panel variable, matching clicks to data is too tricky
      if(show_mesg) {
        ravedash::show_notification('Points are not labelled when
                                      plotting as electrodes (post a request on Slack if you need this behavior)', 'Locations not available.')
      }

      return (FALSE)
    }
  }

  locate_bcbt_click <- function(click, data_loc) {
    # ravedash::logger("Writing out clicks")
    # base::assign('click', click, envir = globalenv())
    # base::assign('data_loc', data_loc, envir = globalenv())

    # we first make a hard decision about which data group the data are in
    ind <- which.min(abs(click$x - data_loc$bars.x))

    if(length(ind) != 1 || is.null(data_loc$y[[ind]]) ) {
      return(NULL)
    }

    # determine weights for the distances so that X and Y loc
    # get roughly equal weight
    wx = diff(range(data_loc$y[[ind]]$y)) / diff(range(data_loc$x[[ind]]))

    # now we do a search for the data point
    loc <- which.min(
      abs(click$x - data_loc$x[[ind]]) * wx +
        abs(click$y - data_loc$y[[ind]]$y)
    )

    #return the trial number
    data_loc$y[[ind]]$Trial[loc]
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      req(local_data$by_condition_by_trial_data_locations)

      if(! is_paneling() && points_are_trials()) {
        click = input$btbc_click

        new_row <- locate_bcbt_click(click, local_data$by_condition_by_trial_data_locations)

        # add this information to our click log. If the point is already in the log, remove it
        if(is.null(local_data$bcbt_click_log)) {
          local_data$bcbt_click_log <- new_row
        } else {
          if(new_row %in% local_data$bcbt_click_log) {
            local_data$bcbt_click_log <- setdiff(local_data$bcbt_click_log, new_row)
          } else {
            local_data$bcbt_click_log %<>% c(new_row)
          }
        }

        local_reactives$update_click_info <- Sys.time()
      }


    }), input$btbc_click, ignoreInit = TRUE, ignoreNULL = TRUE
  )


  # ?ravedash::register_output(
  # output_type = 'no-download',
  # outputId = 'by_condition_by_trial_clicks',
  # render_function =
  output$by_condition_by_trial_clicks <- DT::renderDataTable({

    basic_checks(local_reactives$update_outputs)
    force(local_reactives$update_click_table)
    shiny::req(local_data$by_condition_by_trial_data_locations)

    # this is a little wasteful, but we need to get the Z-scores and the column names
    # of the conditional means are changeable
    all_cond <- rutabaga::rbind_list(
      lapply(local_data$by_condition_by_trial_data_locations$y, function(yy) {
        medy <- fast_median(yy$y)
        yy$Zg <- round(d=1,scale(yy$y, center=medy, scale = mad(yy$y, center=medy)))
        return(yy)
      })
    )

    # overall m/sd
    df <- all_cond[Trial %in% c(local_data$bcbt_click_log,local_data$trial_outliers_list),
                   setdiff(names(all_cond), 'is_clean'), with=FALSE]

    if(is.null(df) || nrow(df) < 1) {
      return(DT::datatable(
        df,
        options =list(language = list(emptyTable = 'No Clicks / Outliers'))
      ))
    }

    o.med <- fast_median(all_cond$y)
    df$Z = round(d=1, scale(df$y, center=o.med, scale = mad(all_cond$y, center=o.med)))
    df$y %<>% pretty_round

    df$Odd = as.integer(df$Trial %in% local_data$trial_outliers_list)

    local_data$bcbt_click_table <- df

    rfloat <- runif(1)
    dt <- DT::datatable(
      df, caption =

        shiny::tags$caption(
          style='caption-side:top; text-align:center; color:black; margin-top:-15px; margin-bottom:-10px; font-size:110%',
          shiny::p('Click Details',
                   shiny::span(style='font-size:90%',shiny::br(),
                               shiny::actionLink(inputId = ns('clear_rows'),
                                                 onclick=sprintf("Shiny.setInputValue(id = '%s', value = '%s');", ns('clear_rows'), rfloat),
                                                 label = 'Clear Selected', icon = ravedash::shiny_icons$trash), '|',
                               label = 'Clear Selected', icon = ravedash::shiny_icons$trash), '|',
                   shiny::actionLink(inputId = ns('nominate_outliers'),
                                     onclick=sprintf("Shiny.setInputValue(id = '%s', value = '%s');", ns('nominate_outliers'), rfloat),
                                     label = 'Flag Selected (requires re-RAVE)', icon = ravedash::shiny_icons$magic)
          )
        ),

      colnames=names(df), rownames = FALSE, extensions='Buttons',
      options=list(autoWidth=FALSE, scroller=TRUE, scrollX=TRUE, scrollY='300px',
                   server=FALSE, paging=FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = '_all')),
                   dom = 't'#, buttons='csv'
      )
    )

    return (dt)
  }, server = FALSE)


  get_clicked_trials <- function(trials_only=TRUE) {
    rows <- input$by_condition_by_trial_clicks_rows_selected
    if(length(rows) < 1) return(NULL)

    clicked_rows <- local_data$bcbt_click_table[rows,]
    clicked_trials <- unique(clicked_rows$Trial)
    if(trials_only) {
      return(clicked_trials)
    }

    list(
      row_numbers=rows,
      row_data = clicked_rows,
      trials = clicked_trials
    )
  }

  # clear the selected rows (also removes them from the flagged list)
  shiny::bindEvent(ravedash::safe_observe({
    click_data <- get_clicked_trials(FALSE)

    if(is.null(click_data)) return(FALSE)

    local_data$bcbt_click_log <- setdiff(local_data$bcbt_click_log, click_data$trials)
    local_data$bcbt_click_table <- local_data$bcbt_click_table[-click_data$row_numbers]
    local_data$trial_outliers_list <- setdiff(local_data$trial_outliers_list, click_data$trials)

    if(length(local_data$trial_outliers_list) < 1) {
      local_data$trial_outliers_list <- NULL
    }

    local_reactives$update_by_condition_plot <- Sys.time()

  }), input$clear_rows, ignoreInit = TRUE, ignoreNULL = TRUE)


  shiny::bindEvent(ravedash::safe_observe({
    click_data <- get_clicked_trials(FALSE)

    if(is.null(local_data$trial_outliers_list)) {
      local_data$trial_outliers_list <- click_data$trials
    } else {
      # add in new rows
      to_add <- setdiff(click_data$trials, local_data$trial_outliers_list)
      to_rem <- intersect(click_data$trials, local_data$trial_outliers_list)

      tmp_list <- c(to_add, setdiff(local_data$trial_outliers_list, to_rem))
      if(length(tmp_list) < 1) {
        local_data$trial_outliers_list <- NULL
      } else {
        local_data$trial_outliers_list <- tmp_list
      }
    }

    # local_reactives$update_click_table <- Sys.time()
    local_reactives$update_by_condition_plot <- Sys.time()
    local_reactives$outliers_updated <- Sys.time()

  }), input$nominate_outliers, ignoreInit = TRUE, ignoreNULL = TRUE)

  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     req(local_data$by_condition_by_trial_data_locations)
  #
  #     if(! is_paneling()) {
  #       click = input$btbc_click
  #       # local_data$by_condition_by_trial_data_locations
  #     }
  #   }), input$btbc_dblclick, ignoreInit = TRUE, ignoreNULL = TRUE
  # )


  ### tracking changes to global plot options
  shiny::bindEvent(
    ravedash::safe_observe({

      set_currently_active_line_palette( input$gpo_lines_palette )

      local_reactives$update_line_plots = Sys.time()

    }), input$gpo_lines_palette, ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      local_data$grouped_plot_options$plot_width_scale = input$scale_pbtbc
      local_reactives$update_by_condition_plot = Sys.time()

    }), input$scale_pbtbc, ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      set_currently_active_heatmap( input$gpo_heatmap_palette )
      local_reactives$update_heatmap_plots = Sys.time()

    }), input$gpo_heatmap_palette, ignoreNULL = TRUE, ignoreInit = FALSE
  )


  # # # tracking frequency window changes
  shiny::bindEvent(
    ravedash::safe_observe({

      tmp = input$ui_analysis_settings

      # check if any frequency drop-downs were chosen
      for(ii in seq_along(tmp)) {
        if(tmp[[ii]]$frequency_dd != 'Select one') {

          freq <- list('delta (1-4)' = c(1,4),
                       'theta (4-8)' = c(4,8),
                       'alpha (8-12)' = c(8,12),
                       'beta (13-30)' = c(13,30),
                       'gamma (30-70)' = c(30,70),
                       'high gamma (70-150)' = c(70,150)
          )[[
            tmp[[ii]]$frequency_dd
          ]]

          tmp[[ii]]$frequency = freq

          # set it back to select one so we don't have to track changes
          tmp[[ii]]$frequency_dd <- 'Select one'

          # refresh the whole component
          dipsaus::updateCompoundInput2(session, 'ui_analysis_settings', tmp, ncomp=length(tmp))
        }
      }

      local_reactives$current_analysis_settings = input$ui_analysis_settings
    }),
    input$ui_analysis_settings, ignoreNULL = TRUE, ignoreInit = TRUE
  )



  basic_checks <- function(flag, check_uni=TRUE) {
    cond <- !is.null(flag)

    ss <- ''
    if(check_uni) {
      if(isTRUE(input$quick_omnibus_only)) {
        ss <- 'uncheck "Just get univariate stats", then '
      }
    }

    str <- sprintf('No results available (%sclick RAVE!)', ss)

    shiny::validate(shiny::need(cond, str))
    cond
  }

  # Register outputs

  #### 3d brain viewer
  ravedash::register_output(
    outputId = "brain_viewer",
    output_type = "threeBrain",
    render_function = threeBrain::renderBrain({
      cond <- basic_checks(local_reactives$update_3dviewer, check_uni = FALSE)

      brain <- raveio::rave_brain(component_container$data$repository$subject)

      if(!cond || is.null(brain)) {
        return(threeBrain::threejs_brain(title = "No 3D model found"))
      }

      df <- data.frame(t(local_data$results$omnibus_results$stats))

      # fix some column names
      # Avoid changing `df` multiple times
      # names(df) = stringr::str_replace_all(names(df), '\\.\\.\\.', ' vs ')
      cnames <- stringr::str_replace_all(names(df), '\\.\\.\\.', ' vs ')

      # names(df) = stringr::str_replace_all(names(df), '\\.', ' ')
      cnames <- stringr::str_replace_all(cnames, '\\.', ' ')

      # names(df) = stringr::str_replace_all(names(df), '\\ $', '')
      cnames <- stringr::str_replace_all(cnames, '\\ $', '')
      names(df) <- cnames

      if("currently_selected" %in% cnames) {
        df$currently_selected <- factor(
          c("Yes", "No")[ 2L - as.integer(df$currently_selected) ],
          levels = c("Yes", "No")
        )
      }

      df$Electrode = as.integer(rownames(df))

      res <- build_palettes_and_ranges_for_omnibus_data(df)

      brain$set_electrode_values(df)

      brain$render(outputId = "brain_viewer", session = session,
                   palettes=res$palettes, value_ranges=res$val_ranges,
                   control_display = FALSE, side_display=FALSE,
                   timestamp=FALSE)
    })
  )

  ravedash::register_output(
    outputId = "brain_viewer_movies",
    output_type = "threeBrain",
    render_function = threeBrain::renderBrain({
      cond <- basic_checks(local_reactives$update_3dviewer)

      brain <- raveio::rave_brain(component_container$data$repository$subject)

      if(!cond || is.null(brain)) {
        return(threeBrain::threejs_brain(title = "No 3D model found"))
      }

      df <- local_data$results$over_time_by_electrode_dataframe
      if(is.null(df)) {
        return(threeBrain::threejs_brain(title = "No data availalble. Check box 'Calculate electrode over time (movie maker)' in Global Plot Options"))
      } else {
        brain$set_electrode_values(df)
        res <- build_palettes_and_ranges_for_omnibus_data(df)
        brain$render(
          outputId = "brain_viewer_movies",
          session = session,
          palettes = res$palettes,
          title = 'Click "Play/Pause" to start animation'
        )
      }

    })
  )

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

  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     if(input$electrode_export_data_type == 'tensor' &&
  #        input$electrode_export_file_type %in% c('Compressed CSV', 'FST')) {
  #
  #       ravedash::show_notification('Only flattened data are supported with FST | CSV output',
  #                                   title='Export data type updated')
  #
  #       shiny::updateSelectInput(inputId ='electrode_export_data_type', selected = 'flat')
  #     }
  #   }),
  #   input$electrode_export_data_type, ignoreNULL = TRUE, ignoreInit = TRUE
  # )

  shiny::bindEvent(
    ravedash::safe_observe({

      dipsaus::shiny_alert2(title = "Preparing for exporting",
                            text = "...", icon = "info",
                            danger_mode = FALSE, auto_close = FALSE, buttons = FALSE)

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
        # electrode_export_file_type = input$electrode_export_file_type,
        # electrode_export_data_type = input$electrode_export_data_type,
        frequencies_to_export = input$frequencies_to_export,
        condition_variable = input$condition_variable,
        times_to_export = input$times_to_export,
        trials_to_export = input$trials_to_export,
        electrodes_to_export = input$electrodes_to_export,
        electrodes_to_export_roi_name = input$electrodes_to_export_roi_name,
        electrodes_to_export_roi_categories = input$electrodes_to_export_roi_categories
      )

      run_analysis(extra_names = "data_for_export",
                   trigger_3dviewer = TRUE,
                   force_settings = list(electrode_text=input$electrodes_to_export)
      )

      # close the previous alert
      dipsaus::close_alert2()

      dipsaus::shiny_alert2(title = "Done with exporting!",
                            text = sprintf('Check %s for your files. Remember to remove unused exports as they can quickly take up disk space!', local_data$results$data_for_export),
                            icon = "info",
                            danger_mode = FALSE, auto_close = FALSE, buttons = TRUE)


      # clear out the previous on.exit if we've made it this far
      on.exit({}, add = FALSE)


      # make sure this is available for export later
      #   env <- pipeline$eval('data_for_export', shortcut=TRUE)
      #   dfe <- env$data_for_export
      #
      #   kv <- list(
      #     'Output type' = pretty(dfe$type)
      #   )
      #   key = ifelse(dfe$type == 'tensor', 'Data size (Freq, Time, Trial, Elec)',
      #                'Data size (Rows, Columns)')
      #
      #   kv[[key]] = sapply(dfe$data_names, function(nm) {
      #     dd = unname(dim(dfe[[nm]]$data))
      #
      #     paste0(c('[', paste(dd, collapse=', '), ']'), collapse='')
      #   }) %>% paste(collapse=', ')
      #
      #   kv[['Output file type']] = env$electrode_export_file_type
      #
      #   # description_str = "<h2>Data Description</h2>";
      #   tokens <- mapply(function(x,y) {
      #     sprintf("<p><strong>%s</strong>: %s</p>", x, y)
      #   }, names(kv), kv, SIMPLIFY =F, USE.NAMES = FALSE)
      #
      #   str = do.call(paste, tokens)
      #
      #   shiny::showModal(shiny::modalDialog(
      #     title = "Download export data",
      #     size = "l",
      #     easyClose = FALSE,
      #     footer = shiny::tagList(
      #       shiny::modalButton("Cancel"),
      #       shiny::downloadButton(
      #         outputId = ns("do_download_export"),
      #         label = "Download data",
      #         class = "btn-primary"
      #       )
      #     ),
      #     shiny::HTML(str)
      #   ))
    })
    ,
    input$btn_export_electrodes,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(ravedash::safe_observe({
    export_powerpoint()
  }), input$btn_export_powerpoint)


  output$btn_export_powerpoint <- shiny::downloadHandler(
    filename=function(...) {
      paste0('rave_powerpoint_',
             format(Sys.time(), "%b_%d_%Y_%H_%M_%S"),
             '.pptx'
      )
    },
    content = function(conn) {
      req(local_data$results)

      tf <- ravedash::temp_file(
        pattern = 'powexpl',
        fileext = '.pptx'
      )

      asc <- local_data$results$analysis_settings_clean

      rmarkdown::render('modules/power_explorer/templates/power_explorer_ppt.Rmd',
                        params = list(
                          subject = asc[[1]]$subject_code,
                          project = asc[[1]]$project_name
                        ),
                        output_dir = dirname(tf), clean = TRUE,
                        output_file = basename(tf),
                        output_format = rmarkdown::powerpoint_presentation(
                          reference_doc='RAVE_white_bg_16by9-template.pptx',
                          keep_md=FALSE)
      )

      file.copy(tf, conn)

    }
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



  shiny::bindEvent(
    ravedash::safe_observe({
      settings <- tryCatch(
        yaml::read_yaml(input$file_load_settings$datapath),
        error = function(e) {
          return (NULL)
        }
      )

      if(!is.null(settings)) {
        update_all_settings(settings)
      }
    }),
    input$file_load_settings,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  output$btn_save_settings <- shiny::downloadHandler(
    filename = function(...) {
      paste0('power_explorer_settings_',
             format(Sys.time(), "%b_%d_%Y_%H_%M_%S"),
             '.yaml'
      )
    },
    content = function(conn) {
      tf <- ravedash::temp_file('pexpl_settings', fileext = '.yaml')
      yaml::write_yaml(pipeline$get_settings(), file=tf)
      file.rename(tf, conn)
    })


  update_available_forked_pipelines <- function() {

    repo <- pipeline$read('repository')
    all_pipes <- repo$subject$list_pipelines('power_explorer' )
    all_pipes <- all_pipes[all_pipes$policy == 'group_analysis', drop=FALSE]

    if(nrow(all_pipes) > 0) {
      lbls <- mapply(function(lbl, pol) {
        stringr::str_remove(lbl, stringr::fixed(pol))
      }, all_pipes$label, paste0(all_pipes$policy, '-'))

      curr_sel <- input$replace_existing_group_anlysis_pipeline

      pipe_choices <- unname(c('Create New', lbls))

      # ravedash::logger('updating forks', level='trace')

      shiny::updateSelectInput(
        inputId = 'replace_existing_group_anlysis_pipeline',
        choices=pipe_choices, selected = curr_sel %OF% pipe_choices
      )
    }
  }


  shiny::bindEvent(ravedash::safe_observe({

    if(input$replace_existing_group_anlysis_pipeline != 'Create New') {
      shiny::updateTextInput(inputId = 'save_pipeline_for_group_analysis_label', value=input$replace_existing_group_anlysis_pipeline)
    }

  }), input$replace_existing_group_anlysis_pipeline, ignoreNULL = TRUE, ignoreInit = TRUE)


  shiny::bindEvent(
    ravedash::safe_observe({

      if(is.null(local_data$results)) {
        ravedash::shiny_alert2("Could not save results", "No results are available. Try clicking RAVE!",
                               icon='warning')

      } else if (!nzchar(input$save_pipeline_for_group_analysis_label)) {
        ravedash::shiny_alert2("Could not save results", "Saved results must have a label.",
                               icon='warning')

      } else {
        # make sure this is available
        prog <- raveio::progress_with_logger("Saving results for group analysis", max = 3)
        prog$inc('Load data')
        pipeline$run('data_for_group_analysis')

        lbl <- input$save_pipeline_for_group_analysis_label

        delete_old <- input$replace_existing_group_anlysis_pipeline != 'Create New'

        prog$inc(paste('Save to server:', lbl))
        repo <- pipeline$read('repository')
        pipeline$fork_to_subject(
          subject = repo$subject,
          label = lbl,
          delete_old = delete_old,
          policy = "group_analysis"
        )

        prog$close(paste0("Done!\n"))

        # if a new pipeline was written, add it to the list
        update_available_forked_pipelines()
      }

    }), input$save_pipeline_for_group_analysis,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  output$do_download_export <- shiny::downloadHandler(
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
      env <- pipeline$eval("data_for_export", shortcut=TRUE)
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
                                h5file = ravedash::temp_file('h5_out', fileext = 'h5')

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

                                tf <- ravedash::temp_file(
                                  pattern = paste0('data_export_', format(Sys.time(), "%b_%d_%Y_%H_%M")),
                                  fileext = '.csv'
                                )
                                data.table::fwrite(data_to_write, file=tf)

                                zf <- ravedash::temp_file(
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

  output$do_download_plot <- shiny::downloadHandler(
    filename = function() {
      type = input$download_plot_type
      if(type == 'tiff (lzw)') {
        type = 'tiff'
      }
      nm = local_data$download_plot_info$id

      paste(paste("plot", nm, Sys.Date(), type, sep="_"), type, sep='.')
    },
    content = function(file) {

      type = input$download_plot_info
      nm = local_data$download_plot_info$id

      FUN = get(
        paste0('plot_', local_data$download_plot_info$id)
      )

      dset <- paste0(local_data$download_plot_info$id, '_data')

      args <- list()
      args[[dset]] = local_data$results[[dset]]


      ### some functions need special arguments
      if(dset == 'over_time_by_condition_data') {
        args$condition_switch = input$over_time_by_condition_switch
      }

      if(dset == 'over_time_by_trial_data') {
        args$plot_options = local_data$over_time_by_trial_plot_options
      }

      if(dset == 'by_condition_by_trial_data') {
        args$grouped_plot_options = local_data$grouped_plot_options
        args$ylab = local_data$results$baseline_settings$unit_of_analysis
      }

      if(dset == 'by_frequency_over_time_data') {
        args$plot_args = local_data$by_frequency_over_time_plot_options
      }

      if(dset == 'by_electrode_custom_plot_data') {
        args[names(local_data$by_electrode_custom_plot_options)] <- local_data$by_electrode_custom_plot_options
        args$do_layout=FALSE
      }

      ###---


      ### open up the proper plotting device
      OPEN_FILE <- switch(input$download_plot_type,
                          'pdf' = function() {
                            pdf(file, width=input$download_plot_width,
                                height=input$download_plot_height,
                                useDingbats = FALSE,
                                title = 'RAVE Plot Export')
                          },
                          'png' = function() {
                            scl = 1
                            png(file, width=scl*input$download_plot_width,
                                height=scl*input$download_plot_height,
                                units = 'in', res=72*4)
                          },
                          'jpeg' = function(){
                            jpeg(file,width=input$download_plot_width,
                                 height=input$download_plot_height,
                                 units = 'in', quality = 90, res=72*4)

                          },
                          'bmp' = function(){
                            bmp(file,width=input$download_plot_width,
                                height=input$download_plot_height,
                                units = 'in', res=72*4)

                          },
                          'tiff (lzw)' = function(){
                            tiff(file,compression='lzw',
                                 width=input$download_plot_width,
                                 height=input$download_plot_height,
                                 units = 'in', res=72*4)

                          }
      )

      OPEN_FILE()
      on.exit(dev.off())

      do.call(FUN, args)
    })

  smart_round <- function(x) {
    m <- floor(log10(min(abs(x))))

    if(m > 0) {
      m = m-1
    }

    round(x, -m)
  }

  output$pes_threshold_string <- shiny::renderText({
    if(is.null(local_data$pes$pes_manual_threshold)) {
      return ("")
    }

    pmt <- local_data$pes$pes_manual_threshold

    sprintf("Current threshold: %s %s (%s) %s %s",
            ifelse(pmt$transform_string=='force', "", pmt$transform_string),
            pmt$operand_string,
            pmt$condition_string,
            pmt$operator_string,
            pmt$comparator
    )
  })

  ravedash::register_output(
    outputId = 'by_electrode_custom_plot',
    render_function = shiny::renderPlot({
      # basic_checks(local_reactives$update_outputs)
      basic_checks(local_reactives$update_pes_plot, check_uni = FALSE)
      force(local_reactives$update_outputs)
      force(local_reactives$update_by_electrode_custom_plot)
      force(local_reactives$update_line_plots)

      po <- local_data$by_electrode_custom_plot_options

      po$by_electrode_custom_plot_data = local_data$results$by_electrode_custom_plot_data

      do.call(plot_by_electrode_custom_plot, po)
    })
  )


  output$per_electrode_results_table <- DT::renderDataTable({
    basic_checks(local_reactives$update_pes_plot, check_uni = FALSE)
    force(local_reactives$update_outputs)

    force(local_reactives$update_per_electrode_results_table)

    omnibus_data <- local_data$results$omnibus_results

    # repo <- local_data$env$repository
    # assign('od', omnibus_data, envir = globalenv())

    mat <- t(omnibus_data$stats)
    mat <- cbind('Electrode'=as.numeric(rownames(mat)), mat)
    rownames(mat)=NULL

    df <- data.frame(mat)
    cnames <- colnames(mat)

    # add in meta data
    if(length(input$bet_meta_data) > 0) {
      bmd <- local_data$electrode_meta_data[,c('Electrode',input$bet_meta_data),drop=FALSE]
      bmd.names <- names(bmd)[names(bmd)!='Electrode']
      if(!is.null(bmd)) {
        df %<>% merge(bmd, by='Electrode', all.y=FALSE)
        cnames %<>% c(bmd.names)
      }
      ravedash::logger('done adding vars', level='trace')
    }

    # remove columns the user doesn't want (this is exclusion rule)
    vars_to_hide <- input$bet_variables_to_hide
    col_to_hide <- rep(FALSE, length(cnames))

    if(length(vars_to_hide) > 0) {
      col_to_hide = sapply(cnames, function(x) {
        any(stringr::str_detect(x, vars_to_hide))
      })
    }

    if(! input$bet_show_contrasts) {
      # first find columns with p_fdr, as those are definitely contrasts
      ind <- stringr::str_detect(cnames, stringr::fixed('p_fdr('))
      # now find the contrast label
      contr <- unique(stringr::str_remove_all(cnames[ind], '(p_fdr|\\(|\\))'))
      # now detect these strings
      is_contr <- sapply(cnames, function(str) any(stringr::str_detect(str, contr)))
      col_to_hide = col_to_hide | is_contr
    }

    ## remove metrics that aren't wanted (this is inclusion rule)
    metrics_to_keep <- input$bet_metrics_to_show

    if(length(metrics_to_keep) == 0) {
      metrics_to_keep = 'm'
      ravedash::show_notification('Must have at least one metric! Showing m(...)',
                                  'Need a metric', type='warning')
    } else if ('p' %in% metrics_to_keep) {
      metrics_to_keep %<>% c('p_fdr')
    }

    met_patt =  paste(collapse='|', paste0(metrics_to_keep, '\\('))

    good_met <- stringr::str_detect(cnames, met_patt)

    ## add back any columns that don't have a "(" because they are special
    good_met = good_met | stringr::str_detect(cnames, stringr::fixed('('),
                                              negate = TRUE)
    col_to_hide = col_to_hide | (!good_met)

    # we need to keep df and its names in synch
    df <- df[!col_to_hide]
    cnames <- cnames[!col_to_hide]

    pcols <- stringr::str_detect(cnames, 'p\\(|p_fdr\\(')
    df[pcols] %<>% lapply(function(v)as.numeric(round_pval(v)))


    dt <- DT::datatable(
      df, colnames=cnames, rownames = FALSE,
      # extensions = c('FixedColumns', 'FixedHeader', 'Scroller', 'Buttons'),
      extensions = c("Buttons"),
      options=list(autoWidth=TRUE, scroller=TRUE, scrollX=TRUE, scrollY='500px',
                   buttons = list(list(extend = 'copy', text='Copy', title = NULL)),
                   fixedColumns = list(leftColumns = 1),
                   server=FALSE, #order=TRUE,
                   # columnDefs = list(
                   # list(width = '50px', targets = "_all")
                   # ),
                   dom = 'Brt'
      )
    )

    char = sapply(df, function(x) any(!is.numeric(x)))
    to_round <- df[!(pcols|char)] %>% sapply(get_pretty_digits)

    for(ur in unique(to_round)) {
      nms <- names(which(to_round == ur))
      dt %<>% DT::formatRound(nms, digits=ur+1)
    }

    #electrodes should always be integer
    dt %<>% DT::formatRound('Electrode', digits=0)

    return (dt)
  }, server = FALSE)

  # ravedash::register_output(
  # outputId = "by_condition_by_trial",
  # render_function = shiny::renderPlot({
  output$by_condition_by_trial <- shiny::renderPlot({
    basic_checks(local_reactives$update_outputs)

    force(local_reactives$update_by_condition_plot)
    force(local_reactives$update_click_info)
    force(local_reactives$update_line_plots)

    dd <- if(isTRUE(input$bcbt_show_outliers)) {
      local_data$results$by_condition_by_trial_data_with_outliers
    } else {
      local_data$results$by_condition_by_trial_data
    }

    final_data_locations <- plot_by_condition_by_trial(
      by_condition_by_trial_data = dd,
      grouped_plot_options = local_data$grouped_plot_options,
      ylab = local_data$results$baseline_settings$unit_of_analysis,
      highlight_trials = local_data$bcbt_click_log
    )

    local_data$by_condition_by_trial_data_locations <- final_data_locations

    # we have new data locations, so update the click table
    local_reactives$update_click_table <- Sys.time()
  })
  # )

  ravedash::register_output(outputId='by_condition_statistics',
                            render_function = shiny::renderUI({
                              basic_checks(local_reactives$update_outputs)

                              aes <- local_data$results$across_electrode_statistics$model
                              summ <- local_data$results$across_electrode_statistics$aov

                              ff <- Reduce(paste, deparse(formula(aes)))

                              shiny::div(
                                shiny::h5(attr(summ, 'heading')[1]),
                                shiny::p(shiny::strong("Model Formula: "), ff),
                                htmltable_coefmat(summ),
                              )

                            }))

  ravedash::register_output(outputId='by_condition_statistics_emmeans',
                            render_function = shiny::renderUI({
                              basic_checks(local_reactives$update_outputs)
                              ee <- summary(local_data$results$across_electrode_statistics$em)
                              ee <- ee[,colnames(ee)!='emmean']
                              em <- merge(
                                local_data$results$across_electrode_statistics$condition_means,
                                ee
                              )

                              names(em)[which(names(em)=='SE')] = 'emm_SE'

                              # fe <- attr(terms(aes), 'term.labels')

                              # contrasts <- emmeans::emmeans(
                              #   aes, as.formula(sprintf("pairwise ~ %s", paste(fe, collapse='*'))), infer=c(F,T)
                              # )

                              shiny::div(
                                shiny::h5("Within condition comparison against 0 (i.e., vs. baseline)"),
                                as_html.emmGrid(em),
                              )
                            }))


  ravedash::register_output(
    outputId='by_condition_statistics_contrasts',
    render_function = shiny::renderUI({
      basic_checks(local_reactives$update_outputs)
      force(local_reactives$update_pairwise_contrasts)

      aes <- local_data$results$across_electrode_statistics

      get_contr <- function(contrasts) {
        nm <- input$bcs_choose_specific_contrast
        if(nm %in% names(contrasts)) {
          contrasts = contrasts[[nm]]
        } else if (is.list(contrasts) && ! ('emmGrid' %in% class(contrasts))) {
          contrasts = contrasts[[1]]
        }
        contrasts
      }


      if(input$bcs_choose_contrasts == 'All-possible pairwise') {
        contrasts <- aes$pairwise_contrasts
      } else if(input$bcs_choose_contrasts == 'Stratified contrasts (more power!)') {
        contrasts <- get_contr(aes$stratified_contrasts)$contrasts
      } else if(input$bcs_choose_contrasts == 'ITX Contrasts (diff of diff)') {
        contrasts <- get_contr(aes$itx_contrasts)
      }

      res <- shiny::div(
        shiny::h5("Between-condition comparison"),
        as_html(contrasts),
      )

    }))



  ravedash::register_output(
    outputId = "over_time_by_electrode",
    shiny::renderPlot({
      # basic_checks(local_reactives$update_outputs, check_uni = F)

      basic_checks(local_reactives$update_pes_plot, check_uni = FALSE)
      force(local_reactives$update_outputs)
      force(local_reactives$update_heatmap_plots)

      plot_over_time_by_electrode(local_data$results$over_time_by_electrode_data)
    })
  )

  ravedash::register_output(
    outputId = "by_frequency_over_time",

    render_function = shiny::renderPlot({
      # req(FALSE)

      basic_checks(local_reactives$update_outputs)
      force(local_reactives$update_heatmap_plots)
      force(local_reactives$update_by_frequency_over_time_plot)

      by_frequency_over_time_data = local_data$results$by_frequency_over_time_data

      # if there are any changes to the analysis window, we want to highlight
      # them on the graph
      for(ii in seq_along(local_reactives$current_analysis_settings)) {
        ss <- local_reactives$current_analysis_settings[[ii]]

        new_time = unlist(ss$time)
        new_freq = unlist(ss$frequency)

        jj = which(endsWith(names(by_frequency_over_time_data), ss$label))

        for(curr_map in jj) {
          if(any(new_time != by_frequency_over_time_data[[curr_map]]$analysis_window,
                 new_freq != by_frequency_over_time_data[[curr_map]]$analysis_frequency)) {
            by_frequency_over_time_data[[curr_map]]$analysis_window_tmp = new_time
            by_frequency_over_time_data[[curr_map]]$analysis_frequency_tmp = new_freq
          }
        }
      }

      ravedash::logger(level='info', "plot_by_frequency_over_time")

      plot_by_frequency_over_time(
        by_frequency_over_time_data,
        plot_args = local_data$by_frequency_over_time_plot_options
      )

    })
  )

  ravedash::register_output(
    outputId = "by_frequency_correlation",
    render_function = shiny::renderPlot({
      basic_checks(local_reactives$update_outputs)
      force(local_reactives$update_heatmap_plots)
      force(local_reactives$update_by_frequency_correlation_plot)

      plot_by_frequency_correlation(
        local_data$results$by_frequency_correlation_data,
        plot_options = local_data$by_frequency_correlation_plot_options
      )
    })
  )

  get_threshold <- function(ptype) {
    th = NULL
    if(!is.null(local_data$pes$pes_manual_threshold)) {
      if(local_data$pes$pes_manual_threshold$operand == ptype) {
        th = local_data$pes$pes_manual_threshold$comparator

        if(local_data$pes$pes_manual_threshold$transform == 'abs') {
          th = sort(c(-th, th))
        }
      }
    }
    return(th)
  }

  ravedash::register_output(
    outputId = "per_electrode_statistics_mean",
    render_function = shiny::renderPlot({
      basic_checks(local_reactives$update_pes_plot, check_uni=FALSE)

      force(local_reactives$update_outputs)

      # force(local_reactives$update_pes_plot)

      stats <- local_data$results$omnibus_results$stats

      lbl_elecs = NULL
      requested_stat = NULL

      if(!is.null(local_data$pes$per_electrode_statistics_chooser)) {
        requested_stat = local_data$pes$per_electrode_statistics_chooser

        if(!is.null(local_data$pes$pes_selected_electrodes)) {
          lbl_elecs <- local_data$pes$pes_selected_electrodes
        }


      }
      plot_per_electrode_statistics(stats, requested_stat, which_plots = 'm',
                                    draw_threshold=get_threshold('m'),
                                    label_electrodes=lbl_elecs, label_type = input$pes_label_type)
    })
  )

  ravedash::register_output(
    outputId = "per_electrode_statistics_tstat",
    render_function = shiny::renderPlot({
      basic_checks(local_reactives$update_pes_plot, check_uni=FALSE)
      force(local_reactives$update_outputs)
      # force(local_reactives$update_pes_plot)

      stats <- local_data$results$omnibus_results$stats

      lbl_elecs = NULL
      requested_stat = NULL

      if(!is.null(local_data$pes$per_electrode_statistics_chooser)) {
        requested_stat = local_data$pes$per_electrode_statistics_chooser

        if(!is.null(local_data$pes$pes_selected_electrodes)) {
          lbl_elecs <- local_data$pes$pes_selected_electrodes
        }
      }

      plot_per_electrode_statistics(stats, requested_stat, which_plots = 't',
                                    draw_threshold=get_threshold('t'),
                                    label_electrodes = lbl_elecs, label_type = input$pes_label_type)
    })
  )

  ravedash::register_output(
    outputId = "per_electrode_statistics_fdrp",
    render_function = shiny::renderPlot({
      # basic_checks(local_reactives$update_outputs)
      # force(local_reactives$update_pes_plot)
      basic_checks(local_reactives$update_pes_plot, check_uni=FALSE)
      force(local_reactives$update_outputs)
      stats <- local_data$results$omnibus_results$stats

      lbl_elecs = NULL
      requested_stat = NULL

      if(!is.null(local_data$pes$per_electrode_statistics_chooser)) {
        requested_stat = local_data$pes$per_electrode_statistics_chooser

        if(!is.null(local_data$pes$pes_selected_electrodes)) {
          lbl_elecs <- local_data$pes$pes_selected_electrodes
        }
      }
      plot_per_electrode_statistics(stats, requested_stat, which_plots = 'p',
                                    draw_threshold = get_threshold('p'),
                                    label_electrodes = lbl_elecs, label_type = input$pes_label_type)
    })
  )

  # special function used by the downloader to get all 3 plots in one
  plot_univariate_statistics_graphical_results <- function(
    univariate_statistics_graphical_results_data) {

    par(mfrow=c(1,3))

    lbl_elecs = NULL
    requested_stat = NULL

    if(!is.null(local_data$pes$per_electrode_statistics_chooser)) {
      requested_stat = local_data$pes$per_electrode_statistics_chooser

      if(!is.null(local_data$pes$pes_selected_electrodes)) {
        lbl_elecs <- local_data$pes$pes_selected_electrodes
      }
    }

    sapply(c('m', 't', 'p'), function(st) {
      plot_per_electrode_statistics(univariate_statistics_graphical_results_data,
                                    requested_stat, which_plots = st,
                                    draw_threshold=get_threshold(st),
                                    label_electrodes=lbl_elecs, label_type = input$pes_label_type)
    })
  }


  ### by trial over time plot
  ravedash::register_output(
    outputId = "over_time_by_trial",
    render_function = shiny::renderPlot({
      basic_checks(local_reactives$update_outputs)

      force(local_reactives$update_heatmap_plots)
      force(local_reactives$update_over_time_by_trial_plot)
      force(local_reactives$outliers_updated)

      # check if we are in a multiple event situation
      plot_over_time_by_trial(
        local_data$results$over_time_by_trial_data,
        local_data$over_time_by_trial_plot_options
      )
    })
  )

  ravedash::register_output(
    outputId = 'by_condition_tabset_clipboard',
    render_function = shidashi::renderClipboard(
      {
        basic_checks(local_reactives$update_outputs)

        ## need to get tab separate values from the data frame
        utils::capture.output(
          write.table(local_data$results$by_condition_by_trial_data_with_outliers,
                      file = "", sep = '\t', row.names =FALSE)
        )
      }
    )
  )

  ravedash::register_output(
    outputId = "over_time_by_condition",
    render_function = shiny::renderPlot({
      basic_checks(local_reactives$update_outputs)

      force(local_reactives$update_line_plots)
      force(local_reactives$update_over_time_plot)

      plot_over_time_by_condition(
        local_data$results$over_time_by_condition_data,
        condition_switch=input$over_time_by_condition_switch,
        plot_range = input$over_time_by_condition_plot_range
      )
    })
  )
}
