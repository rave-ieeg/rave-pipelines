
module_html <- function(){

  line_palettes <- get_line_palette(get_palette_names = TRUE)
  heatmap_palettes <- get_heatmap_palette(get_palette_names = TRUE)

  shiny::fluidPage(shiny::fluidRow(

      # ---- Input tabs (width = 3) -------------------------------

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            # ---- Input tab: Electrode selector -------------------------------

            electrode_selector$ui_func(),

            # ---- Input tab: Custom ROI ---------------------------------------

            ravedash::input_card(
              class_header = "shidashi-anchor",
              title = shiny::checkboxInput(ns('enable_custom_ROI'), 'Custom ROI', value = FALSE),
              shiny::conditionalPanel(
                condition = 'input["power_explorer-enable_custom_ROI"] == 1',
                shiny::p('ROI variables are read from electrodes.csv and are based on currently available electrodes.', shiny::br(), 'Choices here will override selected eletrode(s) above. Any changes require re-RAVE! to update.'),
                shiny::selectInput(
                  inputId = ns('custom_roi_variable'), label='ROI Variable',
                  selected = character(0), choices=character(0)),
                shiny::selectInput(
                  inputId = ns('custom_roi_type'), label = 'How to use ROI',
                  selected = 'Filter only',
                  choices = c('Filter only',
                              'Group/Stratify results',
                              'Interaction model')),
                shiny::actionButton(ns('auto_assign_levels_to_roi_groupings'),
                                    'Assign all ROI levels to groups', icon = ravedash::shiny_icons$magic),
                shiny::HTML("<p>&nbsp;</p>"),
                dipsaus::compoundInput2(
                  inputId = ns('custom_roi_groupings'),
                  label = "ROI Group", initial_ncomp = 1L, min_ncomp = 1L,
                  max_ncomp = 15L,
                  components = shiny::div(
                    shiny::textInput(inputId = "label", label = "Label"),
                    shiny::selectInput(inputId = "conditions", label = "Categories", choices = "",
                                       multiple = TRUE))
                )
              )
            ),


            # ---- Input tab: Save/Load Settings -----------------------------------------

            ravedash::input_card(
              title = "Save/Load Settings",class_header = "shidashi-anchor",

              shiny::fileInput(inputId = ns("file_load_settings"), "Load Settings",
                               multiple = FALSE, accept = c('.yaml')),
              shiny::hr(),
              shiny::p("Settings are updated after an analysis has been run (press RAVE!)"),

              shiny::downloadButton(
                outputId = ns("btn_save_settings"),
                label = "Save settings",
              )
            ),

            # ---- Input tab: Baseline -----------------------------------------

            ravedash::input_card(
              title = "Baseline",class_header = "shidashi-anchor",

              shiny::checkboxInput(inputId=ns('omnibus_includes_all_electrodes'),
                                   'Baseline unselected electrodes (for inclusion in stats/3dViewer, but takes time)', value=FALSE
              ),
              shiny::sliderInput(
                inputId = ns("baseline_window"), "Window", value = c(0,1),
                min =0, max=1, step = 0.01, dragRange = TRUE),
              shiny::selectInput(
                inputId = ns("baseline_scope"), label = 'Baseline Scope',
                selected=get_baseline_scope(names=TRUE)[1],
                choices =get_baseline_scope(names=TRUE)),
              shiny::selectInput(
                inputId = ns("baseline_unit"), label = 'Unit of Analysis',
                selected=get_unit_of_analysis(names=TRUE)[1],
                choices=get_unit_of_analysis(names=TRUE))
            ),

            # ---- Input tab: Analysis window ----------------------------------

            ravedash::input_card(
              class_header = "shidashi-anchor",
              title = "Analysis Windows",

              shiny::checkboxInput(
                inputId=ns('quick_omnibus_only'), 'Just get univariate stats + 3dViewer (fast)', value=FALSE
              ),

              dipsaus::compoundInput2(
                inputId = ns('ui_analysis_settings'),
                label = "Analysis Window",
                initial_ncomp = 1L, min_ncomp = 1L, max_ncomp = 5L,
                components = shiny::div(
                  shiny::textInput(inputId = "label", label = "Label"),
                  shiny::selectInput(
                    inputId = "event",
                    label = "Event",
                    choices = NULL,
                    selected = NULL
                  ),
                  shiny::sliderInput(
                    inputId = "time",
                    label = "Time",
                    min = 0,
                    max = 1,
                    value = c(0, 1),
                    step = 1 / 100,
                    dragRange = TRUE
                  ),
                  shiny::selectInput('frequency_dd', 'Choose preset band',
                                     choices=c('delta (1-4)', 'theta (4-8)', 'alpha (8-12)', 'beta (13-30)',
                                               'gamma (30-70)', 'high gamma (70-150)', 'Select one'), selected='Select one'),
                  shiny::sliderInput(
                    inputId = "frequency",
                    label = "Frequency",
                    min = 0,
                    max = 200,
                    value = c(70, 150),
                    step = 1
                  )
                )
              )
            ),


            # ---- Input tab: First Trial Factor -------------------------------

            ravedash::input_card(
              class_header = "shidashi-anchor",
              title = "First Trial Factor",
              shiny::selectInput(inputId=ns('condition_variable'),
                                 "Condition Variable", choices=c('Condition')),
              dipsaus::compoundInput2(
                inputId = ns('first_condition_groupings'),
                label = "Trial Group", initial_ncomp = 1L, min_ncomp = 1L,
                max_ncomp = 15L,
                components = shiny::div(
                  shiny::textInput(inputId = "label", label = "Label"),
                  shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE)))
            ),



            # ---- Input tab: Second Trial Factor ------------------------------

            ravedash::input_card(
              class_header = "shidashi-anchor",
              title = shiny::checkboxInput(ns('enable_second_condition_groupings'), 'Second Trial Factor', value = FALSE),
              shiny::conditionalPanel(
                condition = 'input["power_explorer-enable_second_condition_groupings"] == 1',
                shiny::p("To create a second factor, specify at least 2 levels and ensure all available conditions are assigned to exactly 1 level."),
                dipsaus::compoundInput2(
                  inputId = ns('second_condition_groupings'),
                  label = "Trial Group", initial_ncomp = 2L, min_ncomp = 2L,
                  max_ncomp = 15L,
                  components = shiny::div(
                    shiny::textInput(inputId = "label", label = "Label"),
                    shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE))
                )
              )
            ),

            # ---- Input tab: Plot Options -------------------------------------

            ravedash::input_card(
              class_header='shidashi-anchor', title='Global plot options',
              shiny::checkboxInput(ns('do_over_time_by_electrode_dataframe'), 'Calculate electrode over time (movie maker)', value=FALSE),
              shiny::selectInput(ns("gpo_lines_palette"), "Lines/Points palette",
                                 choices = line_palettes,
                                 selected = pe_graphics_settings_cache$get('line_color_palette') %OF% line_palettes),
              shiny::selectInput(ns("gpo_heatmap_palette"), "Heatmap palette",
                                 choices = heatmap_palettes,
                                 selected = pe_graphics_settings_cache$get('heatmap_color_palette') %OF% heatmap_palettes
              )
            ),
            # ---- Input tab: Save for group analysis --------------------------------

            ravedash::input_card(
              class_header = "shidashi-anchor", title = "Save for Group Analysis",
              shiny::p("Data from currently-selected electrodes will be saved on the RAVE server in the subject's directory.", shiny::br(),
                       "Be sure to provide a unique label (timestamps are automatic). ",
                       "If you select Create New, but reuse an existing label, the original export will not be deleted, "
                       , "but it may become inaccessible from within RAVE."),
              shiny::p("Using the same label across multiple subjects will greatly facilitate group analysis"),
              shiny::selectInput(ns("replace_existing_group_anlysis_pipeline"), label = 'Create new / Replace existing pipeline',
                                 choices = 'Create New'),
              shiny::textInput(
                inputId = ns("save_pipeline_for_group_analysis_label"),
                label = "Label for results (spaces/special chars will be replaced)",
                placeholder = "compare_A_and_B_pct_signal_change"
              ),

              dipsaus::actionButtonStyled(ns('save_pipeline_for_group_analysis'), 'Save!', icon = ravedash::shiny_icons$save)
            ),

            # ---- Input tab: Export Electrodes --------------------------------

            ravedash::input_card(
              class_header = "shidashi-anchor", title = "Export electrodes to csv",
              shiny::p("All exported data are baseline corrected according to ",
                       "the current analysis settings.",
                       "Use the options below to fine-tune the export."),
              shiny::textInput(
                inputId = ns("electrodes_to_export"),
                label = "Electrodes to export",
                placeholder = "1-20,80-100"
              ),
              shiny::selectInput(
                inputId = ns("electrodes_to_export_roi_name"),
                label = "Add ROI filter",
                choices = c('none', 'anything else')
              ),
              shiny::conditionalPanel(
                condition = "input.electrodes_to_export_roi_name != 'none'",
                ns = ns,
                shiny::selectInput(
                  inputId = ns("electrodes_to_export_roi_categories"),
                  multiple = TRUE,
                  label = "Categories to include in export",
                  choices = 'unknown'
                )
              ),
              shiny::selectInput(
                inputId = ns("frequencies_to_export"),
                label = "How to export frequency",
                choices = c(
                  'Collapsed, Analysis window(s) only',
                  'Raw, Analysis window(s) only',
                  'Raw, All available frequencies'
                )
              ),
              shiny::selectInput(
                inputId = ns("times_to_export"),
                label = "How to export time",
                choices = c(
                  'Collapsed, Analysis window(s) only',
                  'Raw, Analysis window(s) only',
                  'Raw, All available times'
                ),
                selected = 'Raw, All available times'
              ),
              shiny::selectInput(
                inputId = ns("trials_to_export"),
                label = "How to export trial",
                choices = c(
                  'Raw, Only trials used in grouping factors',
                  'Raw, All available trials',
                  'Collapsed by grouping factors'
                ),
                # 'Collapsed by condition',

                selected = 'Raw, only Conditions used in grouping factors'
              ),
              # shiny::selectInput(
              #   inputId = ns('electrode_export_file_type'),
              #   label = "Export format",
              #   choices = c('HDF5', 'FST',
              #               'Compressed CSV', 'RDS')
              # ),
              # shiny::selectInput(
              #   inputId = ns('electrode_export_data_type'),
              #   label = "Data structure",
              #   choices = c('tensor', 'flat')
              # ),
              shiny::actionButton(
                inputId = ns('btn_export_electrodes'),
                label = "Export",
                icon = ravedash::shiny_icons$export
              )
            ),


            # ---- Input tab: Export Power Point --------------------------------

            ravedash::input_card(
              class_header='shidashi-anchor', title='Export PowerPoint',
              shiny::selectInput(ns('epp_ppt_template'), label = 'PPT Template',
                                 choices='RAVE 16:9 White Background'),
              shiny::selectInput(ns('epp_markdown_template'),
                                 label = 'Markdown Template', choices='All Plots'),

              shiny::downloadButton(
                outputId = ns('btn_export_powerpoint'),
                label = "Generate Report",
                class='btn-primary',
                icon = ravedash::shiny_icons$export
              )
            )



            # baseline_choices$ui_func(),
            # comp_analysis_ranges$ui_func()
          )
        )
      ),

      # ---- Output tabs (width = 9) -------------------------------

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,


            # ---- Output tab-set: Brain Viewers -------------------------------

            ravedash::output_cardset(
              inputId = ns('brain_viewers'), title = 'Brain Viewers',
              class_body = "no-padding min-height-400 height-400 resize-vertical",
              tools = list(
              ),
              append_tools = FALSE,
              `Results Viewer` =shiny::div(class='position-relative fill',
                                           ravedash::output_gadget_container(
                                             threeBrain::threejsBrainOutput(
                                               outputId = ns("brain_viewer"), height = "100%"
                                             )
                                           )
              )
              ,
              `Movie Maker` =
                ravedash::output_gadget_container(
                  threeBrain::threejsBrainOutput(
                    outputId = ns("brain_viewer_movies"), height = "100%"
                  )
                )
            ),


            # ---- Output tab-set: By Electrode -----------------------

            ravedash::output_cardset(
              inputId = ns('by_electrode_tabset'),
              title = "By Electrode",
              class_body = "no-padding fill-width",
              append_tools = FALSE,
              tools = list(
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("by_electrode_tabset_config")
                ),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("by_electrode_tabset_camera")
                )
              ),

              `Over Time` = shiny::tagList(
                shiny::div(
                  # opens a fluid container
                  class = "container-fluid",
                  make_heatmap_control_panel(prefix = 'otbe', config = 'by_electrode_tabset_config', do_xlim=FALSE),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] %% 2 == 1", 'by_electrode_tabset_config'),
                    ns = ns,
                    shiny::div(
                      class = "container-fluid",
                      shiny::fluidRow(
                        shiny::column(width = 3L,
                                      shiny::selectInput(ns('otbe_yaxis_sort'), label = 'How to sort electrodes',
                                                         choices = c('Electrode #',
                                                                     'Activity Correlation',
                                                                     'Activity Correlation (spearman)',
                                                                     'Activity distance (Euclidean)',
                                                                     'Coordinate distance', #'Coordinate distance (ignore Hemi)',
                                                                     'ROI distance'#, 'ROI distance (ignore Hemi)'
                                                                     )
                                      )),
                        shiny::column(width = 3L,
                                      shiny::selectInput(ns('otbe_yaxis_sort_combine_rule'),
                                                         label = 'Combine across conditions with:',
                                                         choices = c('total (sum across conditions)', 'min (smallest distance wins)', 'max (largest distance wins)')
                                      )
                        ),
                        shiny::column(width = 1L,
                                      shiny::numericInput(ns('otbe_yaxis_cluster_k'),
                                                          label = '# Clusters', value = 2, min = 2, max = 8
                                      )

                        ),
                        shiny::column(width = 1L,
                                      shiny::selectInput(ns('otbe_yaxis_cluster_palette'),
                                                         label = 'Palette',
                                                         choices = rev(get_line_palette(get_palette_names = T)), selected ='Set2'
                                      )
                        ),
                        shiny::column(width = 2L,
                                      shiny::actionButton(ns('otbe_update_3dviewer'),
                                                          label = 'Send to brain viewer',class='btn-small',
                                                          icon = ravedash::shiny_icons$arrow_up
                                      ),
                                      shiny::actionButton(ns('otbe_create_roi'),
                                                          label = 'Cluster -> ROI',class='btn-small',
                                                          icon = ravedash::shiny_icons$magic
                                      )
                        ),
                        shiny::column(width=2,
                                      clipboardOutput(outputId=ns('otbe_cluster_clipboard'), as_card_tool = FALSE,
                                                      message='to clipboard', class='btn-small'),
                                      shiny::actionButton(ns('otbe_cluster_to_electrodes_csv'), width = "75%",
                                                          label = 'to electrodes.csv', class='clipboard-btn btn btn-default'
                                      )
                        )
                      )
                    )
                  )
                ),
                shiny::div(
                  class = "fill-width no-padding min-height-400 resize-vertical",
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('over_time_by_electrode'),
                      min_height = 400
                    )
                  )
                )
              ),

              # ---- Output tab: By Electrode > Graphical Results -----
              `By Condition` = shiny::div(
                shiny::conditionalPanel(
                  condition = "input['by_electrode_tabset_config']%2 == 1",
                  ns = ns,
                  shiny::div(
                    class = "container-fluid",
                    shiny::fluidRow(
                      shiny::column(
                        width = 3L,
                        shiny::selectInput(
                          inputId = ns('per_electrode_statistics_chooser'),
                          label = 'Data group to display',
                          choices = c('No groups available')
                        )
                      ),
                      shiny::column(
                        width = 3L,
                        shiny::selectInput(
                          inputId = ns('pes_select_mode'),
                          label = 'Select mode',
                          choices=c('Label maker', 'Threshold |v| > x', 'Threshold v > x',
                                    'Threshold v < x', 'Manual threshold', 'Invert selection',
                                    'Clear labels')
                        )
                      ),
                      shiny::column(
                        width = 3L,
                        shiny::selectInput(
                          inputId = ns('pes_label_type'),
                          label = 'Label type',
                          multiple = TRUE,
                          selected='number',
                          choices = c('number', 'name', 'color', 'showcase')
                        )
                      ),
                      # shiny::column(width = 2,
                      #               shiny::selectInput(ns('pes_magic_thresholding'), multiple = TRUE,
                      #                                  label = 'Auto-threshold', selected = character(0),
                      #                                  choices=c('p < 0.05','v > x','v < x'))
                      # ),
                      shiny::column(
                        width = 3L,
                        shiny::selectInput(
                          inputId = ns('pes_selected_action'),
                          multiple = FALSE,
                          label = 'Actions',
                          selected = character(0),
                          choices = c(
                            'Click for choices', 'Analyze selection', 'Send selection to export'
                            # 'Select nearby electrodes'
                          )
                        )
                      )
                    ),

                    shiny::fluidRow(
                      shiny::column(
                        width = 12L,
                        shiny::textOutput(outputId = ns('pes_threshold_string'))
                      )
                    )
                  )
                ),
                shiny::div(
                  # a container with 100% width, 400px initial (and minimum) height, vertically resizable
                  class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                  shiny::div(
                    class = "row fill-height",
                    shiny::div(class = "col-sm-4 fill-height",
                               ravedash::plotOutput2(
                                 outputId = ns('per_electrode_statistics_mean'),
                                 click = ns('pes_click_m'),
                                 min_height = 400
                               )),
                    shiny::div(class = "col-sm-4 fill-height",
                               ravedash::plotOutput2(
                                 outputId = ns('per_electrode_statistics_tstat'),
                                 click = ns('pes_click_t'),
                                 min_height = 400
                               )),
                    shiny::div(class = "col-sm-4 fill-height",
                               ravedash::plotOutput2(
                                 outputId = ns('per_electrode_statistics_fdrp'),
                                 click = ns('pes_click_p'),
                                 min_height = 400
                               ))
                  )
                )
              ),

              # ---- Output tab: By Electrode > Tabular Results -------
              `Tabular Results` = shiny::div(
                class = "fill-width min-height-400",
                shiny::conditionalPanel(
                  condition = "input['by_electrode_tabset_config']%2 == 1",
                  ns = ns,
                  shiny::div(
                    class = "container-fluid",
                    shiny::fluidRow(
                      shiny::column(width=4, shiny::selectInput(
                        inputId = ns('bet_variables_to_hide'),
                        label = 'Variables to remove',
                        choices = character(0), multiple = TRUE),
                      ),
                      shiny::column(width=2, shiny::selectInput(
                        inputId = ns('bet_metrics_to_show'),
                        label = 'Metrics to include',choices = c('m', 't', 'p'),
                        selected = c('m', 't', 'p'), multiple = TRUE),
                      ),
                      shiny::column(width=2, style='text-align: left; margin-top:37px; margin-left:0px',
                                    shiny::checkboxInput(
                                      inputId = ns('bet_show_contrasts'), label = 'Show Contrasts',
                                      value = TRUE
                                    )),
                      shiny::column(width=4, shiny::selectInput(
                        inputId = ns('bet_meta_data'),
                        label = 'Meta data to include',
                        choices = character(0), multiple = TRUE),
                      )
                    )
                  )
                ),
                DT::dataTableOutput(outputId = ns('per_electrode_results_table'))
              ),

              # ---- Output tab: By Electrode > Custom Plot -------
              `Custom Plot` = shiny::div(
                class = "fill-width min-height-400",
                shiny::conditionalPanel(
                  condition = "input['by_electrode_tabset_config']%2 == 1",
                  ns = ns,
                  shiny::div(
                    class = "container-fluid",
                    shiny::fluidRow(
                      shiny::column(width = 2L,
                                    shiny::conditionalPanel(condition = 'input["power_explorer-bec_yvar_is_multi"] == 1',
                                                            shiny::selectInput(inputId = ns('bec_yvar_chooser_multi'), label = 'Data for Y axis',
                                                                               choices = c('Empty'), multiple = TRUE),
                                    ),
                                    shiny::conditionalPanel(condition = 'input["power_explorer-bec_yvar_is_multi"] == 0',
                                                            shiny::selectInput(inputId = ns('bec_yvar_chooser'), label = 'Data for Y axis',
                                                                               choices = c('Empty'), multiple = FALSE),
                                    )
                      ),
                      shiny::conditionalPanel(condition = 'input["power_explorer-bec_yvar_is_multi"] == 1',
                                              shiny::column(width = 12,
                                                            shiny::selectInput(inputId = ns('bec_yvar_collapser'), label = 'Collapse', choices = PE_COLLAPSE_METHODS)
                                              )
                      ),
                      shiny::column(width = 1L, style='text-align: left; margin-top:37px; margin-left:0px',
                                    shiny::checkboxInput(inputId = ns('bec_yvar_is_multi'), label='multi', value = FALSE)
                      ),
                      shiny::column(width = 1L,
                                    shiny::selectInput(inputId = ns('bec_yvar_unit'), label = 'unit', choices = c('m', 't', 'p'))
                      ),
                      shiny::column(width = 2L, offset=1,
                                    shiny::conditionalPanel(condition = 'input["power_explorer-bec_xvar_is_multi"] == 1',
                                                            shiny::selectInput(inputId = ns('bec_xvar_chooser_multi'), label = 'Data for X axis',
                                                                               choices = c('Empty'), multiple = TRUE),
                                    ),
                                    shiny::conditionalPanel(condition = 'input["power_explorer-bec_xvar_is_multi"] == 0',
                                                            shiny::selectInput(inputId = ns('bec_xvar_chooser'), label = 'Data for X axis',
                                                                               choices = c('Empty'), multiple = FALSE),
                                    )
                      ),
                      shiny::conditionalPanel(condition = 'input["power_explorer-bec_xvar_is_multi"] == 1',
                                              shiny::column(width = 12,
                                                            shiny::selectInput(inputId = ns('bec_xvar_collapser'), label = 'Collapse', choices = PE_COLLAPSE_METHODS)
                                              )
                      ),
                      shiny::column(width = 1L, style='text-align: left; margin-top: 37px; margin-left:0px',
                                    shiny::checkboxInput(inputId = ns('bec_xvar_is_multi'), label='multi', value = FALSE)
                      ),
                      shiny::column(width = 1L,
                                    shiny::selectInput(inputId = ns('bec_xvar_unit'), label = 'unit', choices = c('m', 't', 'p'))
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(width = 2L,
                                    shiny::selectInput(inputId = ns('bec_only_selected_electrodes'), label = 'Display',
                                                       choices=c('All Electrodes', 'Currently selected'))
                      ),
                      shiny::column(width = 1L,
                                    shiny::numericInput(ns('bec_plot_width_scale'), 'Plot width',
                                                        value = 1, min=0.1, max=10, step = .1)
                      ),
                      shiny::column(width=1,
                                    shiny::numericInput(ns('bec_pt.alpha'), 'Pt alpha', value = 100, min=0, max=100)
                      ),
                      shiny::column(width=1,
                                    shiny::numericInput(ns('bec_pt.cex'), 'Pt scale', value = 1, min=0.1, max=10, step = .1)
                      ),
                      shiny::column(width=2,
                                    shiny::selectInput(ns('bec_plot_decorators'), 'Plot Decor', choices=get_plot_decorators(names_only=TRUE), multiple = TRUE)
                      )


                      #,
                      # shiny::column(width = 1L,
                      #               shiny::numericInput(ns('bec_plot_width_scale'), 'Scale plot width',
                      #                                   value = 1, min=0.1, max=10, step = .1)
                      # )
                    )
                  )
                ),
                ravedash::plotOutput2(outputId = ns('by_electrode_custom_plot'))
              )
            ),

            # ---- Output tab-set: By Frequency --------------------------------
            make_by_frequency_tabset(),

            # ---- Output tab-set: Over Time -----------------------------------
            ravedash::output_cardset(
              inputId = ns('over_time_tabset'),
              title='Over Time',
              class_body="no-padding fill-width",
              append_tools = FALSE,
              tools = list(
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("over_time_tabset_config")
                ),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("over_time_tabset_camera")
                )
              ),

              # ---- Output tab: Over Time > By Condition ----------------------
              `By Condition` = shiny::tagList(
                shiny::div(
                  # opens a fluid container
                  class = "container-fluid",
                  shiny::conditionalPanel(
                    condition = "input['over_time_tabset_config']%2 == 1",
                    ns = ns,
                    shiny::fluidRow(
                      shiny::column(
                        width = 3L,
                        shiny::selectInput(
                          inputId = ns('over_time_by_condition_switch'),
                          label='Plot type', selected = 'Combine conditions',
                          choices = c('Combine conditions', 'Combine events',
                                      'Combine all', 'Separate all')
                        )),
                      shiny::column(offset = 1,
                                    width = 4L,
                                    shiny::sliderInput(
                                      inputId = ns('over_time_by_condition_plot_range'),
                                      label='Plot range', value = c(0,1),
                                      min =0, max=1, step = 0.01, dragRange = TRUE
                                    )
                      )
                    )
                  )
                ),
                shiny::div(
                  class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('over_time_by_condition'),
                      min_height = 400)
                  )
                )
              ),
              `By Trial` = shiny::tagList(
                shiny::div(
                  class = "fill-width no-padding min-height-400 resize-vertical",
                  make_heatmap_control_panel(prefix = 'otbt', config = 'over_time_tabset_config'),
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('over_time_by_trial'),
                      min_height = 400)
                  )
                )
              )
            ),

            # ---- Output tab-set: By Condition ------------------------------------
            ravedash::output_cardset(
              inputId = ns('by_condition_tabset'),
              title='By Condition',
              class_body='',
              append_tools = FALSE,
              tools = list(
                clipboardOutput(outputId=ns('by_condition_tabset_clipboard'), as_card_tool = TRUE, message='copy data'),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("by_condition_tabset_config")
                ),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("by_condition_tabset_camera")
                )
              ),
              `By Trial` = #shiny::tagList(
                shiny::div(
                  # id='makeinline',
                  # class='height-400 fill-width container-fluid',
                  # class = "no-padding",
                  {shiny::conditionalPanel(
                    "input['power_explorer-by_condition_tabset_config'] %2 == 1",
                    shiny::fluidRow(
                      shiny::column(
                        width=2, shiny::selectInput(ns('btp_basic_unit'),
                                                    label = 'Points are: ',
                                                    choices=c('Trials', 'Electrodes'))
                      ),
                      shiny::column(width=1, style='text-align: left; margin-top:37px; margin-left:0px',
                                    shiny::checkboxInput(ns('bcbt_show_outliers'), "Show Outliers", value = TRUE)
                      ),
                      shiny::column(
                        width=3,
                        shiny::selectInput(ns("btp_types"), label = 'Plot types',
                                           multiple = TRUE,
                                           choices = c('jitter points', 'means', 'ebar polygons', 'sd polygons',
                                                       'points', 'connect points',
                                                       'densities', 'density polygons',
                                                       'bars', 'borders', 'ebars'),
                                           selected=c('jitter points', 'means', 'ebar polygons')
                        )),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_xvar"),
                                                       "X-axis", choices=c('First Factor',
                                                                           'Analysis Group'))
                      ),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_gvar"),
                                                       "Group by", choices=c('none', 'Analysis Group', 'First Factor')),
                      ),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_panelvar"),
                                                       "Panel by", choices=c('none', 'Analysis Group',
                                                                             'First Factor'))
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(width=3,
                                    shiny::numericInput(ns('btp_pt.alpha'), 'Point alpha (opacity)', value = 100, min=0, max=100)
                      ),
                      shiny::column(width=3,
                                    shiny::numericInput(ns('btp_pt.cex'), 'Point scaling', value = 1, min=0.1, max=10, step = .1)
                      ),
                      shiny::column(width=3,
                                    shiny::numericInput(ns('scale_pbtbc'), 'Plot width scaling', value = 1, min=0.1, max=10, step = .1)
                      ),
                      shiny::column(width=2,
                                    shiny::selectInput(ns("btp_highlight_clicks"), selected='labels',
                                                       "Highlight Clicks", choices=c('points', 'lines',
                                                                                     'labels'), multiple = TRUE)
                      ),
                      shiny::column(width=1,
                                    shiny::selectInput(ns("btp_highlight_text_location"), selected='top',
                                                       "Highlight pos", choices=c('center', 'bottom', 'left', 'top', 'right'))
                      )
                    )
                  )},
                  shiny::fluidRow(
                    shiny::column(width=7,
                                  # shiny::div(class='col-sm-8',
                                  # ravedash::output_gadget_container(
                                  ravedash::plotOutput2(outputId = ns('by_condition_by_trial'),
                                                        click = shiny::clickOpts(ns('btbc_click'), clip=TRUE),
                                                        dblclick =ns('btbc_dblclick')
                                                        # hover=ns('btbc_hover')
                                  ),
                    ),
                    #),
                    shiny::column(width=5,
                                  # shiny::div(class='col-sm-4',
                                  # ravedash::output_gadget_container(
                                  DT::dataTableOutput(outputId = ns('by_condition_by_trial_clicks'))
                                  # )
                    )
                    # )
                  )
                ),
              # ), # end of By trial tag list
              `Overall model test` = shiny::div(style='margin:20px', class="",
                                                shiny::htmlOutput(ns('by_condition_statistics'))
              ),
              `Conditions vs. Baseline` = shiny::div(style='margin:20px',
                                                     class="",
                                                     shiny::fluidRow(
                                                       shiny::column(width = 4,
                                                                     shiny::selectInput(ns('bcs_choose_emmeans'),
                                                                                        "Which means to display?", choices=c('All possible')))
                                                     ),

                                                     shiny::htmlOutput(ns('by_condition_statistics_emmeans'))
              ),
              `Pairwise comparisons` = shiny::div(style='margin:20px',
                                                  class="",
                                                  shiny::fluidRow(
                                                    shiny::column(width = 5,
                                                                  shiny::selectInput(ns('bcs_choose_contrasts'),
                                                                                     "Which contrasts to display?", selected = 'All-possible pairwise',
                                                                                     choices=c('All-possible pairwise',
                                                                                               'Stratified contrasts (more power!)',
                                                                                               'ITX Contrasts (diff of diff)')
                                                                  )
                                                    ),
                                                    shiny::column(width=4,
                                                                  shiny::conditionalPanel('input["power_explorer-bcs_choose_contrasts"] != "All-possible pairwise"',
                                                                                          shiny::selectInput(ns('bcs_choose_specific_contrast'),
                                                                                                             'Choose layer/grouping', choices='')))
                                                  ),
                                                  shiny::htmlOutput(ns('by_condition_statistics_contrasts'))
              )
            )

            #   `card with flip` = shidashi::flip_box(
            #     front = shidashi::info_box("Side A"),
            #     back = shidashi::info_box("Side B"),
            #     inputId = 'flip_box1')
            # ),
          )
        )
      )
    )
  )
}
