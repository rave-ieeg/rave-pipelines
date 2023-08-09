
module_html <- function(){
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,
            electrode_selector$ui_func(),
            ravedash::input_card(class_header = "shidashi-anchor",
                                 title = shiny::checkboxInput(ns('enable_custom_ROI'), 'Custom ROI', value = FALSE),
                                 shiny::conditionalPanel(condition = 'input["power_explorer-enable_custom_ROI"] == 1',
                                                         shiny::p('ROI variables are read from electrodes.csv and are based on currently available electrodes'),
                                                         shiny::selectInput(ns('custom_roi_variable'), label='ROI Variable',
                                                                            selected = character(0), choices=character(0)),
                                                         shiny::selectInput(ns('custom_roi_type'), label = 'How to use ROI',
                                                                            selected = 'Filter only',
                                                                            choices = c('Filter only',
                                                                                        'Group/Stratify results',
                                                                                        'Interaction model')),
                                                         dipsaus::compoundInput2(inputId = ns('custom_roi_groupings'),
                                                                                 label = "ROI Group", initial_ncomp = 1L, min_ncomp = 1L,
                                                                                 max_ncomp = 15L,
                                                                                 components = shiny::div(
                                                                                   shiny::textInput(inputId = "label", label = "Label"),
                                                                                   shiny::selectInput(inputId = "conditions", label = "Categories", choices = "",
                                                                                                      multiple = TRUE))
                                                         )
                                 )
            ),
            ravedash::input_card("Baseline",class_header = "shidashi-anchor",
                                 shiny::sliderInput(ns("baseline_window"), "Window", value = c(0,1),
                                                    min =0, max=1, step = 0.01, dragRange = TRUE),
                                 shiny::selectInput(ns("baseline_scope"), label = 'Baseline Scope',
                                                    selected=get_baseline_scope(names=TRUE)[1],
                                                    choices =get_baseline_scope(names=TRUE)),
                                 shiny::selectInput(ns("baseline_unit"), label = 'Unit of Analysis',
                                                    selected=get_unit_of_analysis(names=TRUE)[1],
                                                    choices=get_unit_of_analysis(names=TRUE))
            ),

            ravedash::input_card(class_header = "shidashi-anchor",
                                 title = "Analysis windows",
                                 dipsaus::compoundInput2(inputId = ns('ui_analysis_settings'),
                                                         label = "Analysis Window",
                                                         initial_ncomp = 1L, min_ncomp = 1L, max_ncomp = 5L,
                                                         components = shiny::div(
                                                           shiny::textInput(inputId = "label", label = "Label"),
                                                           shiny::selectInput(inputId = "event", label = "Event", choices=NULL,selected = NULL),
                                                           shiny::sliderInput(inputId = "time", label = "Time",
                                                                              min=0,max=1,value = c(0,1),step = 1/100, dragRange = TRUE),
                                                           shiny::sliderInput(inputId = "frequency", label = "Frequency", min=0,max=200,value = c(70,100), step = 1)
                                                         )
                                 )
            ),

            ravedash::input_card(class_header = "shidashi-anchor",
                                 title = "First trial factor",
                                 dipsaus::compoundInput2(inputId = ns('first_condition_groupings'),
                                                         label = "Trial Group", initial_ncomp = 1L, min_ncomp = 1L,
                                                         max_ncomp = 15L,
                                                         components = shiny::div(
                                                           shiny::textInput(inputId = "label", label = "Label"),
                                                           shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE)))
            ),
            ravedash::input_card(class_header = "shidashi-anchor",
                                 title = shiny::checkboxInput(ns('enable_second_condition_groupings'), 'Second trial factor', value = FALSE),
                                 shiny::conditionalPanel(condition = 'input["power_explorer-enable_second_condition_groupings"] == 1',
                                                         shiny::p("To create a second factor, specify at least 2 levels and ensure all available conditions are assigned to exactly 1 level."),
                                                         dipsaus::compoundInput2(inputId = ns('second_condition_groupings'),
                                                                                 label = "Trial Group", initial_ncomp = 2L, min_ncomp = 2L,
                                                                                 max_ncomp = 15L,
                                                                                 components = shiny::div(
                                                                                   shiny::textInput(inputId = "label", label = "Label"),
                                                                                   shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE))
                                                         )
                                 )
            ),

            ravedash::input_card(class_header='shidashi-anchor', title='Plot options',
                                 shiny::selectInput(inputId = 'plot_customizer_select',
                                                    label = 'Which plot to customize',
                                                    choices = c('Global plot options', 'By Trial', 'By Frequency', 'By Electrode', 'Over Time'),
                                                    selected = 'by trial'),
                                 shiny::conditionalPanel("input.plot_customizer_select == 'Global plot options'",
                                                         shiny::selectInput(ns("gpo_lines_palette"), "Lines/Points palette",
                                                                            choices=get_line_palette(get_palette_names = T)),
                                                         shiny::selectInput(ns("gpo_heatmap_palette"), "Heatmap palette",
                                                                            choices=get_heatmap_palette(get_palette_names = TRUE)
                                                         )

                                 ),
                                 shiny::conditionalPanel("input.plot_customizer_select == 'By Electrode'",
                                                         shiny::p("By Electrode Panel")),
                                 # shiny::conditionalPanel("input.plot_customizer_select == 'Over Time'",
                                 #                         shiny::selectInput(ns('ot_condition_switch'), label='Plot type',
                                 #                                            selected = 'Combine conditions',
                                 #                                            choices = c('Combine conditions',
                                 #                                                        'Combine events',
                                 #                                                        'Combine all',
                                 #                                                        'Separate all'))
                                 # ),
                                 shiny::conditionalPanel("input.plot_customizer_select == 'By Frequency'",
                                                         shiny::p("By Frequency Panel")),
                                 shiny::conditionalPanel("input.plot_customizer_select == 'By Trial'",
                                                         shiny::selectInput(ns("btp_types"), label = 'Plot types',
                                                                            multiple = TRUE,
                                                                            choices = c('jitter points', 'means', 'ebar polygons',
                                                                                        'points', 'connect points',
                                                                                        'densities', 'density polygons',
                                                                                        'bars', 'borders', 'ebars'),
                                                                            selected=c('jitter points', 'means', 'ebar polygons')
                                                         ),
                                                         shiny::selectInput(ns("btp_xvar"),
                                                                            "X-axis variable", choices=c('First Factor',
                                                                                                         'Analysis Group')),
                                                         shiny::selectInput(ns("btp_gvar"),
                                                                            "Grouping variable", choices=c('Analysis Group', 'First Factor')),
                                                         shiny::selectInput(ns("btp_panelvar"),
                                                                            "Panel variable", choices=c('none', 'Analysis Group',
                                                                                                        'First Factor')),
                                                         shiny::numericInput(ns('btp_pt.alpha'), 'Point alpha (opacity)', value = 100, min=0, max=100),
                                                         shiny::numericInput(ns('btp_pt.cex'), 'Point scaling', value = 1, min=0.1, max=10, step = .1)
                                 )
            ),
            ravedash::input_card(class_header = "shidashi-anchor", title = "Export Electrodes",
                                 shiny::p("All exported data are baseline corrected according to the current analysis settings.
                                                      Use the options below to fine-tune the export."),
                                 shiny::textInput(ns("electrodes_to_export"), label = "Electrodes to export", placeholder = "1-20,80-100"),
                                 shiny::selectInput(ns("electrodes_to_export_roi_name"), label = "Add ROI filter", choices=c('none', 'anything else')),
                                 shiny::conditionalPanel("input['power_explorer-electrodes_to_export_roi_name'] != 'none'",
                                                         shiny::selectInput(ns("electrodes_to_export_roi_categories"), multiple = TRUE,
                                                                            label = "Categories to include in export", choices='unknown')
                                 ),
                                 shiny::selectInput(ns("frequencies_to_export"), label = "How to export frequency",
                                                    choices = c('Collapsed, Analysis window(s) only',
                                                                'Raw, Analysis window(s) only',
                                                                'Raw, All available frequencies')
                                 ),
                                 shiny::selectInput(ns("times_to_export"), label = "How to export time",
                                                    choices = c('Collapsed, analysis window(s) only',
                                                                'Raw, Analysis window(s) only',
                                                                'Raw, All available times'),
                                                    selected = 'Raw, All available times'
                                 ),
                                 shiny::selectInput(ns("trials_to_export"), label = "How to export trial",
                                                    choices = c(
                                                      'Raw, Only trials used in grouping factors',
                                                      'Raw, All available trials',
                                                      'Collapsed by grouping factors'),
                                                                # 'Collapsed by condition',

                                                    selected = 'Raw, only Conditions used in grouping factors'
                                 ),
                                 shiny::selectInput(ns('electrode_export_file_type'),
                                                    "Export format",
                                                    choices=c('HDF5', 'FST',
                                                              'Compressed CSV', 'RDS')),
                                 shiny::selectInput(ns('electrode_export_data_type'),
                                                    "Data structure",
                                                    choices=c('tensor', 'flat')),
                                 shiny::actionButton(ns('btn_export_electrodes'), "Export",
                                                     icon=ravedash::shiny_icons$export)
            )
            # baseline_choices$ui_func(),
            # comp_analysis_ranges$ui_func()
          )
        )
      ),
      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,
            shidashi::card_tabset(inputId=ns('brain_viewers'), title = 'Brain Viewers',
                                  tools = list(
                                    shidashi::card_tool(widget = "maximize"),
                                    shidashi::card_tool(widget = "collapse")
                                  ),
                                  `Results Viewer` = threeBrain::threejsBrainOutput(ns("brain_viewer"), height = "40vh"),
                                  `Movie Maker` = threeBrain::threejsBrainOutput(ns("brain_viewer_movies"), height = "40vh")
            ),
            shidashi::card_tabset(
              inputId = ns('by_electrode_output'),
              title = "Univariate statistics",
              class_body = "no-padding fill-width resize-vertical",
              tools = list(
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("by_electrode_tabset_config")
                ),
                shidashi::card_tool(widget = "maximize"),shidashi::card_tool(widget = "collapse")#,
                # shidashi::card_tool(widget = "flip")
              ),
              `Graphical results` = shiny::div(id='makeinline',
                                               shiny::conditionalPanel("input['power_explorer-by_electrode_tabset_config']%2 == 1",
                                                                       shiny::fluidRow(style='margin-left:20px; margin-top: 5px; margin-bottom:5px',
                                                                                       shiny::column(width = 3, offset = 0,
                                                                                                     shiny::selectInput(ns('per_electrode_statistics_chooser'),
                                                                                                                        label = 'Data group to display', choices = c('No groups available'))
                                                                                       ),
                                                                                       # shiny::column(width = 3, offset = 0,
                                                                                       #               shiny::actionLink(ns('clear_pes_annotations'),
                                                                                       #                                 label = 'Clear labels', icon = ravedash::shiny_icons$trash)
                                                                                       # ),
                                                                                       shiny::column(width = 3, offset = 0,
                                                                                                     shiny::selectInput(ns('pes_select_mode'),
                                                                                                                        label = 'Select mode', choices=c('Label maker',
                                                                                                                                                         'Threshold |v| > x',
                                                                                                                                                         'Threshold v > x',
                                                                                                                                                         'Threshold v < x',
                                                                                                                                                         'Manual threshold',
                                                                                                                                                         'Invert selection',
                                                                                                                                                         'Clear labels'))
                                                                                       ),
                                                                                       shiny::column(width = 3, offset = 0,
                                                                                                     shiny::selectInput(ns('pes_label_type'),
                                                                                                                        label = 'Label type', choices=c('number',
                                                                                                                                                        'name',
                                                                                                                                                        'color',
                                                                                                                                                        'showcase')
                                                                                                     )),
                                                                                       # shiny::column(width = 2, offset = 0,
                                                                                       #               shiny::selectInput(ns('pes_magic_thresholding'), multiple = TRUE,
                                                                                       #                                  label = 'Auto-threshold', selected = character(0),
                                                                                       #                                  choices=c('p < 0.05','v > x','v < x'))
                                                                                       # ),
                                                                                       shiny::column(width = 3, offset = 0,
                                                                                                     shiny::selectInput(ns('pes_selected_action'), multiple = FALSE,
                                                                                                                        label = 'Actions', selected = character(0),
                                                                                                                        choices=c('Click for choices',
                                                                                                                                  'Analyze selection',
                                                                                                                                  'Send selection to export'
                                                                                                                                  # 'Select nearby electrodes'
                                                                                                                                  ))
                                                                                       )
                                                                       ),
                                                                       shiny::fluidRow(shiny::div(class='makeinline', style='margin-top:-15px; margin-bottom:-2px; margin-left:40px;font-size:11pt',
                                                                                                  shiny::textOutput(outputId = ns('pes_threshold_string'))))
                                               ),
                                               shiny::fluidRow(
                                                 shiny::column(width = 4, shiny::plotOutput(ns('per_electrode_statistics_mean'),
                                                                                            # hover = ns('pes_hover'),
                                                                                            click = ns('pes_click_m'))),
                                                 shiny::column(width = 4, shiny::plotOutput(ns('per_electrode_statistics_tstat'),
                                                                                            # hover = ns('pes_hover'),
                                                                                            click = ns('pes_click_t'))),
                                                 shiny::column(width = 4, shiny::plotOutput(ns('per_electrode_statistics_fdrp'),
                                                                                            # hover = ns('pes_hover'),
                                                                                            click = ns('pes_click_p')))
                                               )
              ),
              `Tabular Results` = DT::dataTableOutput(ns('per_electrode_results_table'))
            ),
            shidashi::card_tabset(inputId = ns('by_frequency_output'),
                                  title='By Frequency',
                                  class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
                                  tools = list(shidashi::card_tool(widget = "maximize"),
                                               shidashi::card_tool(widget = "collapse")),
                                  `Over time` = shiny::plotOutput(ns("activity_over_time_by_frequency")),
                                  `Correlations` = shiny::plotOutput(ns("frequency_correlation_plot"))
            ),
            shidashi::card_tabset(inputId = ns('over_time_tabset'),
                                  title='Over Time',
                                  class_body="no-padding fill-width resize-vertical",
                                  tools = list(
                                    shidashi::card_tool(
                                      widget = "custom", icon = ravedash::shiny_icons$puzzle,
                                      inputId = ns("over_time_tabset_config")
                                    ),
                                    shidashi::card_tool(
                                      widget = "custom", icon = ravedash::shiny_icons$camera,
                                      inputId = ns("over_time_tabset_camera")
                                    ),
                                    shidashi::card_tool(widget = "maximize"),
                                    shidashi::card_tool(widget = "collapse")
                                  ),
                                  `By Condition` = shiny::div(id='makeinline',
                                                              shiny::fluidRow(style='margin-left:20px; margin-top: 5px; margin-bottom:5px',
                                                                              shiny::column(width=3,
                                                                                            shiny::conditionalPanel("input['power_explorer-over_time_tabset_config']%2 == 1",
                                                                                                                    shiny::selectInput(ns('ot_condition_switch'), label='Plot type',
                                                                                                                                       selected = 'Combine conditions',
                                                                                                                                       choices = c('Combine conditions',
                                                                                                                                                   'Combine events',
                                                                                                                                                   'Combine all',
                                                                                                                                                   'Separate all'))
                                                                                            )
                                                                              )
                                                              ), shiny::fluidRow(style='margin-left:5px;',
                                                                shiny::plotOutput(ns('over_time_with_switch'))
                                                              )),
                                  `By Electrode` = shiny::plotOutput(ns('activity_over_time_by_electrode')),
                                  `By Trial` = shiny::plotOutput(ns('activity_over_time_by_trial'))
            ),
            shidashi::card_tabset(inputId = ns('by_condition_tabset'),
                                  title='By Trial',
                                  class_body='',
                                  tools = list(shidashi::card_tool(widget = "maximize"),shidashi::card_tool(widget = "collapse")),
                                  `Grouped Plot` = shiny::plotOutput(ns('by_condition_grouped_plot'))
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
