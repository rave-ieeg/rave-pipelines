

module_html <- function(){

  all_modules <- sort(unique(ravepipeline::pipeline_list()))

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
              title = "Data selector",
              shiny::selectInput(
                inputId = ns("data_source"),
                label = "Data source",
                choices = c("Uploads", "Saved pipelines/modules", "None"),
                selected = pipeline$get_settings(
                  key = "data_source",
                  constraint = c("Uploads", "Saved pipelines/modules", "None")
                )
              ),
              shiny::conditionalPanel(
                condition = sprintf(
                  "input['%s'] === 'Saved pipelines/modules'",
                  ns("data_source")
                ),
                shiny::selectInput(
                  inputId = ns("data_source_project"),
                  label = "Select a project",
                  choices = character(0L)
                ),
                shiny::selectInput(
                  inputId = ns("data_source_pipeline"),
                  label = "Select a saved pipeline",
                  choices = character(0L)
                ),
                shiny::conditionalPanel(
                  condition = sprintf(
                    "input['%s'] !== ''",
                    ns("data_source_pipeline")),
                  shiny::selectInput(
                    inputId = ns("data_source_pipeline_target"),
                    label = "Select a target variable from the pipeline",
                    choices = character(0L)
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = sprintf(
                  "input['%s'] === 'Uploads'",
                  ns("data_source")
                ),
                shiny::selectInput(
                  inputId = ns("uploaded_source"),
                  label = "Select an uploaded data",
                  selected = character(0L),
                  choices = character(0L)
                ),

                shiny::conditionalPanel(
                  condition = sprintf(
                    "input['%s'] === '[New Uploads]'",
                    ns("uploaded_source")
                  ),
                  dipsaus::fancyFileInput(
                    inputId = ns("uploaded_file"),
                    label = "Upload csv/fst/xlsx table",
                    width = "100%",
                    size = "s"
                  ),
                  # shiny::fileInput(
                  #   inputId = ns("uploaded_file"),
                  #   label = "Upload csv/fst table",
                  #   multiple = FALSE
                  # ),
                  shiny::actionLink(
                    inputId = ns("download_template_btn"),
                    label = "Show/Download a template table"
                  )
                )
              ),

              footer = shiny::div(
                class = "text-right fill-width",
                shiny::tags$small(
                  shiny::actionLink(
                    inputId = ns("viewer_reset"),
                    label = "Reset controller option"
                  ),
                  " or ",
                  ravedash::run_analysis_button(
                    label = "Re-generate the viewer",
                    icon = ravedash::shiny_icons$arrow_right,
                    btn_type = "link")

                )
              )
            ),

            ravedash::input_card(
              title = "Viewer status",
              class_body = "no-padding min-height-250 height-300 resize-vertical",
              tools = list(
                shidashi::card_tool(widget = "flip")
              ),
              footer = shiny::div(
                class = "text-right",
                shiny::tags$small(
                  shiny::actionLink(
                    inputId = ns("flip_viewer_status"),
                    label = "* Click here to toggle visualization for time-series data."
                  )
                )
              ),
              shiny::div(
                id = ns("flip_viewer_wrapper"),
                class = "flip-box fill",
                `data-toggle` = "click-front",
                shiny::div(
                  class = "flip-box-inner fill",
                  shiny::div(
                    class = "flip-box-back fill",
                    ravedash::output_gadget_container(
                      shiny::plotOutput(
                        outputId = ns("viewer_selected_data"),
                        height = "100%",
                        click = shiny::clickOpts(
                          id = ns("viewer_selected_data_click"),
                          clip = TRUE
                        )
                      )
                    )
                  ),
                  shiny::div(
                    class = "flip-box-front fill-width",
                    shiny::div(
                      class = "padding-10",
                      shiny::uiOutput(
                        outputId = ns("viewer_status")
                      )
                    )
                  )
                )
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
              title = 'RAVE 3D Viewer',
              class_body = "no-padding fill-width height-vh80 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                ravedash::output_gadget_container(
                  threeBrain::threejsBrainOutput(
                    outputId = ns("viewer"),
                    height = "100%"
                  )
                )
              )
            )
          )
        )
      )

    )
  )
}
