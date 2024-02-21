
module_html <- function(){

  line_palettes <- get_line_palette(get_palette_names = TRUE)
  heatmap_palettes <- get_heatmap_palette(get_palette_names = TRUE)

  shiny::fluidPage(
    shiny::fluidRow(

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
                shiny::p('ROI variables are read from electrodes.csv and are based on currently available electrodes'),
                shiny::selectInput(
                  inputId = ns('custom_roi_variable'), label='ROI Variable',
                  selected = character(0), choices=character(0)),
                shiny::selectInput(
                  inputId = ns('custom_roi_type'), label = 'How to use ROI',
                  selected = 'Filter only',
                  choices = c('Filter only',
                              'Group/Stratify results',
                              'Interaction model')),
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


            # ---- Input tab: Baseline -----------------------------------------

            ravedash::input_card(
              title = "Baseline",class_header = "shidashi-anchor",
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
                  shiny::sliderInput(
                    inputId = "frequency",
                    label = "Frequency",
                    min = 0,
                    max = 200,
                    value = c(70, 100),
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
              shiny::selectInput(ns("gpo_lines_palette"), "Lines/Points palette",
                                 choices = line_palettes,
                                 selected = pe_graphics_settings_cache$get('line_color_palette') %OF% line_palettes),
              shiny::selectInput(ns("gpo_heatmap_palette"), "Heatmap palette",
                                 choices = heatmap_palettes,
                                 selected = pe_graphics_settings_cache$get('heatmap_color_palette') %OF% heatmap_palettes
              )
            ),
            # ---- Input tab: Export Electrodes --------------------------------

            ravedash::input_card(
              class_header = "shidashi-anchor", title = "Export Electrodes",
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
                  'Collapsed, analysis window(s) only',
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
              shiny::selectInput(
                inputId = ns('electrode_export_file_type'),
                label = "Export format",
                choices = c('HDF5', 'FST',
                            'Compressed CSV', 'RDS')
              ),
              shiny::selectInput(
                inputId = ns('electrode_export_data_type'),
                label = "Data structure",
                choices = c('tensor', 'flat')
              ),
              shiny::actionButton(
                inputId = ns('btn_export_electrodes'),
                label = "Export",
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
              `Results Viewer` =
                ravedash::output_gadget_container(
                  threeBrain::threejsBrainOutput(
                    outputId = ns("brain_viewer"), height = "100%"
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


            # ---- Output tab-set: Univariate statistics -----------------------

            ravedash::output_cardset(
              inputId = ns('univariate_statistics_tabset'),
              title = "Univariate Statistics",
              class_body = "no-padding fill-width",
              append_tools = FALSE,
              tools = list(
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("univariate_statistics_tabset_config")
                ),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("univariate_statistics_tabset_camera")
                )
              ),

              # ---- Output tab: Univariate statistics > Graphical Results -----
              `Graphical Results` = shiny::div(
                shiny::conditionalPanel(
                  condition = "input['univariate_statistics_tabset_config']%2 == 1",
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

              # ---- Output tab: Univariate statistics > Tabular Results -------
              `Tabular Results` = shiny::div(
                class = "fill-width min-height-400",
                DT::dataTableOutput(outputId = ns('per_electrode_results_table'))
              )
            ),

            # ---- Output tab-set: By Frequency --------------------------------
            ravedash::output_cardset(
              inputId = ns('by_frequency_tabset'),
              title='By Frequency',
              class_body = "no-padding fill-width height-400 min-height-400 resize-vertical",
              tools = list(
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("by_frequency_tabset_camera")
                )
              ),
              append_tools = FALSE,
              `Over time` = ravedash::output_gadget_container(
                ravedash::plotOutput2(
                  outputId = ns("by_frequency_over_time"),
                  min_height = 400
                )
              ),
              `Correlation` = ravedash::output_gadget_container(
                ravedash::plotOutput2(
                  outputId = ns("by_frequency_correlation"),
                  min_height = 400
                )
              )
            ),

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
              `By Electrode` = shiny::tagList(
                shiny::div(
                  class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('over_time_by_electrode'),
                      min_height = 400
                    )
                  )
                )
              ),
              `By Trial` = shiny::tagList(
                shiny::div(
                  class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('over_time_by_trial'),
                      min_height = 400)
                  )
                )
              )
            ),

            # ---- Output tab-set: By Trial ------------------------------------
            ravedash::output_cardset(
              inputId = ns('by_trial_tabset'),
              title='By Trial',
              class_body='no-padding fill-width',
              append_tools = FALSE,
              tools = list(
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$puzzle,
                  inputId = ns("by_trial_tabset_config")
                ),
                shidashi::card_tool(
                  widget = "custom", icon = ravedash::shiny_icons$camera,
                  inputId = ns("by_trial_tabset_camera")
                )
              ),
              `By Condition` = shiny::div(
                id='makeinline',
                style='margin-left:20px; margin-top: 5px; margin-bottom:5px',
                shiny::conditionalPanel(
                  "input['power_explorer-by_trial_tabset_config'] %2 == 1",
                  shiny::fluidRow(
                    shiny::column(
                      width=6,
                      shiny::selectInput(ns("btp_types"), label = 'Plot types',
                                         multiple = TRUE,
                                         choices = c('jitter points', 'means', 'ebar polygons',
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
                                                     "Group by", choices=c('Analysis Group', 'First Factor')),
                    ),
                    shiny::column(width=2,
                                  shiny::selectInput(ns("btp_panelvar"),
                                                     "Panel by", choices=c('none', 'Analysis Group',
                                                                           'First Factor'))
                    ),
                  ),
                  shiny::fluidRow(
                    shiny::column(width=3,
                                  shiny::numericInput(ns('btp_pt.alpha'), 'Point alpha (opacity)', value = 100, min=0, max=100)
                    ),
                    shiny::column(width=3,
                                  shiny::numericInput(ns('btp_pt.cex'), 'Point scaling', value = 1, min=0.1, max=10, step = .1)
                    )
                  )

                ),
                # shiny::plotOutput(ns('by_trial_by_condition'))
                ravedash::plotOutput2(ns('by_trial_by_condition'),
                                      click = ns('btbc_click'),
                                      # dblClick=ns('btbc_dblclick'),
                                      hover=ns('btbc_hover')
                )
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
