
module_html <- function() {

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
              title = "Electrode value selector",
              shiny::selectInput(
                inputId = ns("data_source"),
                label = "Data source",
                choices = c(
                  "Uploads",
                  # "Saved pipelines/modules",
                  "None"
                ),
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
                    ns("data_source_pipeline")
                  ),
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
                    btn_type = "link"
                  )
                )
              )
            ),

            ravedash::input_card(
              title = "Quick analysis",
              class_body = "row screen-height overflow-y-scroll",

              ravedash::group_box(
                title = "Object selection",
                shiny::column(
                  width = 12L,

                  shiny::radioButtons(
                    inputId = ns("object_selector"),
                    label = "Object type",
                    choices = c(
                      "Electrode",
                      "Mesh surface",
                      "3D volume",
                      "Streamlines"
                    ),
                    selected = character(),
                    inline = TRUE,
                    width = "100%"
                  ),

                  shiny::conditionalPanel(
                    condition = sprintf(
                      "input['%s'] === 'Electrode'",
                      ns("object_selector")
                    ),
                    shiny::selectizeInput(
                      inputId = ns("object_selector_electrode"),
                      label = "Highlight an electrode or select from below",
                      choices = list("[Double-click electrode]" = "")
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf(
                      "input['%s'] === 'Mesh surface'",
                      ns("object_selector")
                    ),
                    shiny::selectizeInput(
                      inputId = ns("object_selector_surface"),
                      label = "Choose a surface object",
                      choices = character()
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf(
                      "input['%s'] === '3D volume'",
                      ns("object_selector")
                    ),
                    shiny::selectizeInput(
                      inputId = ns("object_selector_volume"),
                      label = "Choose a volume object",
                      choices = c("[Current active overlay]")
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf(
                      "input['%s'] === 'Streamlines'",
                      ns("object_selector")
                    ),
                    shiny::selectizeInput(
                      inputId = ns("object_selector_streamlines"),
                      label = "Choose a streamline bundle",
                      choices = c("[Current active streamlines]")
                    )
                  ),

                  shiny::conditionalPanel(
                    condition = sprintf(
                      "typeof input['%s'] === 'string'",
                      ns("object_selector")
                    ),

                    shiny::p(
                      shiny::tags$small(
                        style = "color: #808080",
                        shiny::textOutput(
                          outputId = ns("object_selector_text"),
                          container = shiny::span
                        )
                      )
                    ),

                    shiny::actionButton(
                      inputId = ns("object_selector_add"),
                      label = "Add object",
                      width = "100%",
                      icon = ravedash::shiny_icons$plus
                    ),

                    shiny::p()
                  )
                ) # col-12
              ), # Group box: Object selection

              ravedash::group_box(
                title = "Analysis choices",

                shiny::column(
                  width = 12L,

                  shidashi::objectListInput(
                    inputId = ns("object_selector_list"),
                    label = "Choose & sort objects",
                    placeholder = "(No object added yet)",
                    allow_readd = TRUE,
                    sortable = TRUE,
                    removable = TRUE
                  ),

                  shiny::selectInput(
                    inputId = ns("analysis_selector"),
                    label = "Analysis type",
                    choices = structure(
                      names = vapply(analysis_registry, "[[", FUN.VALUE = "", "description"),
                      as.list(names(analysis_registry))
                    )
                  ),

                  dipsaus::actionButtonStyled(
                    inputId = ns("analysis_configure"),
                    label = "Configure & Run...",
                    icon = ravedash::shiny_icons$arrow_right,
                    btn_type = "link",
                    width = "100%"
                  )
                ) # col-12
              ) # Group box: Analysis choices
            ),

            ravedash::input_card(
              title = "Viewer status",
              class_body = "no-padding min-height-250 height-300 resize-vertical overflow-hidden",
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
                    # MIGRATED: removed ravedash::output_gadget_container() wrapper
                    # ravedash::output_gadget_container(
                    shiny::plotOutput(
                      outputId = ns("viewer_selected_data"),
                      height = "100%",
                      click = shiny::clickOpts(
                        id = ns("viewer_selected_data_click"),
                        clip = TRUE
                      )
                    )
                    # )
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
              title = "RAVE 3D Viewer",
              class_body = "no-padding fill-width height-vh80 min-height-450 resize-vertical",
              shiny::div(
                class = "position-relative fill",
                threeBrain::threejsBrainOutput(
                  outputId = ns("viewer"),
                  height = "100%"
                )
              ),
              footer = NULL
            ),
            ravedash::output_card(
              title = "Analysis results",
              shiny::uiOutput(outputId = ns("analysis_results"))
            )
          )
        )
      )
    )
  )
}
