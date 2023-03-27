



module_html <- function(){
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll padding-bottom-70",
          shiny::column(
            width = 12L,
            electrode_selector$ui_func(),
            ravedash::input_card("Baseline",
              shiny::sliderInput(ns("baseline_window"), "Window", value = c(0,1),
                min =0, max=1, step = .1, dragRange = TRUE),
              shiny::selectInput(ns("baseline_scope"), label = 'Baseline Scope',
                selected=get_baseline_scope(names=TRUE)[1],
                choices =get_baseline_scope(names=TRUE)),
              shiny::selectInput(ns("baseline_unit"), label = 'Unit of Analysis',
                selected=get_unit_of_analysis(names=TRUE)[1],
                choices =get_unit_of_analysis(names=TRUE))
            ),

            ravedash::input_card(class_header = "shidashi-anchor",
              title = "Analysis windows",
              dipsaus::compoundInput2(inputId = ns('ui_analysis_settings'),
                label = "Analysis Window",
                initial_ncomp = 1L, min_ncomp = 1L, max_ncomp = 5L,
                components = shiny::div(
                  shiny::textInput(inputId = "label", label = "Label"),
                  shiny::selectInput(inputId = "event", label = "Event", choices=NULL,selected = NULL),
                  shiny::sliderInput(inputId = "time", label = "Time", min=0,max=1,value = c(0,1),step = .1),
                  shiny::sliderInput(inputId = "frequency", label = "Frequency", min=0,max=200,value = c(70,100), step = 1)
                )
              )
            ),

            ravedash::input_card(class_header = "shidashi-anchor",
              title = "First grouping factor",
              dipsaus::compoundInput2(inputId = ns('first_condition_groupings'),
                label = "Trial Group", initial_ncomp = 1L, min_ncomp = 1L,
                max_ncomp = 15L,
                components = shiny::div(
                  shiny::textInput(inputId = "label", label = "Label"),
                  shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE)))
            ),
            ravedash::input_card(class_header = "shidashi-anchor", title = "Export Electrodes",
              shiny::p("All exported data are baseline corrected according to the current analysis settings.
                                                      Use the options below to fine-tune the export."),
              shiny::textInput(ns("electrodes_to_export"), label = "Electrodes to export", placeholder = "1-20,80-100"),

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
                choices = c('Collapsed by grouping factors',
                  'Collapsed by condition column',
                  'Raw, Conditions used in grouping factors',
                  'Raw, All available trials'),
                selected = 'Raw, only Conditions used in grouping factors'
              ),
              shiny::selectInput(ns('electrode_export_file_type'), "Export format",
                choices=c('FST', 'H5', 'Compressed CSV', 'RDS')),
              customDownloadButton(ns('btn_export_electrodes'), icon_lbl="export")
              # shiny::actionButton(ns('export_electrodes'), "Export", icon=ravedash::shiny_icons$export)
            )
            # baseline_choices$ui_func(),
            # comp_analysis_ranges$ui_func()
          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll padding-bottom-70 output-wrapper",
          shiny::column(
            width = 12L,
            shidashi::card(title = 'Brain Viewer', tools = list(
              shidashi::card_tool(widget = "maximize"),
              shidashi::card_tool(widget = "collapse")
              ),
              threeBrain::threejsBrainOutput(ns("brain_viewer"), height = "40vh")
            ),
            shidashi::card_tabset(
              inputId = ns('by_electrode_output'),
              title = "By Electrode",
              class_body = "no-padding fill-width height-500 min-height-500 resize-vertical",
              tools = list(
                shidashi::card_tool(widget = "collapse")#,
                # shidashi::card_tool(widget = "flip")
              ),
              `Over time` = shiny::plotOutput(ns('activity_over_time_by_electrode')),
              `Univariate results` = shiny::div(id='makeinline',
                shiny::fluidRow(style='margin-left:20px; margin-top: 5px; margin-bottom:5px',
                  shiny::column(width = 3, offset = 0,
                    shiny::selectInput(ns('per_electrode_statistics_chooser'),
                      label = 'Data group to display', choices = c('No groups available'))
                  )
                ),
                shiny::fluidRow(
                  shiny::column(width=12,
                    shiny::plotOutput(ns('per_electrode_statistics'))
                  )
                )
              ),
              `Univariate results table` = DT::dataTableOutput(ns('per_electrode_results_table'))
            ),
            shidashi::card_tabset(inputId = ns('by_frequency_output'),
              title='By Frequency',
              class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
              tools = list(shidashi::card_tool(widget = "collapse")),
              `Over time` = shiny::plotOutput(ns("activity_over_time_by_frequency")),
              `Correlations` = shiny::plotOutput(ns("frequency_correlation_plot"))
            ),
            shidashi::card_tabset(inputId = ns('by_condition_output'),
              title='By Condition',
              class_body='',
              tools=list(shidashi::card_tool(width='collapse')),
              `Separated conditions/events` = shiny::plotOutput(ns('over_time_separated_all')),
              `Combined conditions` = shiny::plotOutput(ns('over_time_combined_conditions')),
              `Combined events` = shiny::plotOutput(ns('over_time_combined_events')),
              `Combined conditions/events` = shiny::plotOutput(ns('over_time_combined_all'))
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
