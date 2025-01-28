

module_html <- function(){

  all_modules <- sort(unique(raveio::pipeline_list()))

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Upload electrode table",
              # shiny::selectInput(
              #   inputId = ns("data_source"),
              #   label = "Data source",
              #   choices = c("Uploads", "Saved pipelines/modules", "None"),
              #   selected = pipeline$get_settings(
              #     key = "data_source",
              #     constraint = c("Uploads", "Saved pipelines/modules", "None")
              #   )
              # ),
              shiny::p("Columns must include (case sensitive): SubjectCode, Electrode, Label, x, y, z"),
              shiny::p("Coordinates should be in MNI152 space"),
              dipsaus::fancyFileInput(multiple=FALSE,
                                      inputId = ns("uploaded_electrode_table"),
                                      label = "Upload csv/xlsx table",
                                      width = "100%",
                                      size = "s"
              )#,

              # footer = shiny::div(
              #   class = "text-right fill-width",
              #   shiny::tags$small(
              #     # shiny::actionLink(
              #     #   inputId = ns("viewer_reset"),
              #     #   label = "Reset controller option"
              #     # ),
              #     # " or ",
              #     ravedash::run_analysis_button(
              #       label = "Re-generate the viewer",
              #       icon = ravedash::shiny_icons$arrow_right,
              #       btn_type = "link")
              #
              #   )
              # )
            ),
            ravedash::input_card(
              title = "Upload data file",
              # shiny::selectInput(
              #   inputId = ns("data_source"),
              #   label = "Data source",
              #   choices = c("Uploads", "Saved pipelines/modules", "None"),
              #   selected = pipeline$get_settings(
              #     key = "data_source",
              #     constraint = c("Uploads", "Saved pipelines/modules", "None")
              #   )
              # ),
              shiny::p("Rectangular, columns are variables, rows are electrodes. Must include a column called 'Electrode'"),
              dipsaus::fancyFileInput(multiple=FALSE,
                                      inputId = ns("uploaded_data_file"),
                                      label = "Upload csv/xlsx table",
                                      width = "100%",
                                      size = "s"
              )
            ),
            ravedash::input_card(
              title = "Upload time series data",
              shiny::p("Rectangular data, # Rows = # Time points x # Electrodes. Must include a column called 'Electrode'"),
              dipsaus::fancyFileInput(multiple=FALSE,
                                      inputId = ns("uploaded_connectivity_matrix"),
                                      label = "Upload csv/xlsx table",
                                      width = "100%",
                                      size = "s"
              )
            ),
            ravedash::input_card(
              title = "Upload connectivity matrix",
              shiny::p("Square matrix, # Rows = # Cols = # Electrodes"),
              dipsaus::fancyFileInput(multiple=FALSE,
                                      inputId = ns("uploaded_connectivity_matrix"),
                                      label = "Upload csv/xlsx table",
                                      width = "100%",
                                      size = "s"
              )
            )
          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,
            ravedash::output_card(
              title = 'Viewer',
              # class_body = "no-padding fill-width height-vh80 min-height-450 resize-vertical",
              class_body = "no-padding min-height-400 height-400 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                ravedash::output_gadget_container(
                  threeBrain::threejsBrainOutput(
                    outputId = ns("viewer"),
                    height = "100%"
                  )
                )
              )
            ),
            ravedash::output_cardset(
              inputId = ns('over_time_tabset'),
              title='Visualize electrode data',
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
              `By Electrode` = shiny::tagList(
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
                          inputId = ns('by_electrode_variable_selector'),
                          label='Display Variable', selected = 'N/A',
                          choices = c('N/A')
                        ))
                      # ,
                      # shiny::column(offset = 1,
                      #               width = 4L,
                      #               shiny::sliderInput(
                      #                 inputId = ns('over_time_by_condition_plot_range'),
                      #                 label='Plot range', value = c(0,1),
                      #                 min =0, max=1, step = 0.01, dragRange = TRUE
                      #               )
                      # )
                    )
                  )
                ),
                shiny::div(
                  class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('by_electrode'),
                      min_height = 400)
                  )
                )
              ),
              `Aggregated data` = shiny::tagList(
                shiny::div(
                  class = "fill-width no-padding min-height-400 resize-vertical",
                  ravedash::output_gadget_container(
                    ravedash::plotOutput2(
                      outputId = ns('aggregate_over_electrode'),
                      min_height = 400
                    )
                  )
                )
              )#,
              # `By Trial` = shiny::tagList(
              #   shiny::div(
              #     class = "fill-width no-padding min-height-400 resize-vertical",
              #     # make_heatmap_control_panel(prefix = 'otbt', config = 'over_time_tabset_config'),
              #     ravedash::output_gadget_container(
              #       ravedash::plotOutput2(
              #         outputId = ns('over_time_by_trial'),
              #         min_height = 400)
              #     )
              #   )
              # )
            )
          )
        )
      )

    )
  )
}
