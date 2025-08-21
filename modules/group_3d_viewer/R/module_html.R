

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

            ravedash::input_card(
              title = "Mapping Configurations",

              shiny::fluidRow(
                shiny::column(
                  width = 12L,

                  # ravedash::flex_group_box(
                  #   title = "Subject selection",

                    shidashi::flex_item(
                      shiny::selectInput(
                        inputId = ns("subject_codes"),
                        label = "Subjects to be included",
                        multiple = TRUE,
                        choices = character()
                      )
                    ),

                  # ), # flex-box - Subject selection

                  dipsaus::fancyFileInput(
                    inputId = ns("data_uploader"),
                    width = "100%",
                    label = "Electrode values",
                    size = "s",
                    multiple = FALSE
                  ),

                  shiny::p(
                    shiny::actionLink(
                      inputId = ns('data_uploader_example_btn'),
                      label = "Show an example"
                    )
                  ),

                  shiny::actionButton(
                    inputId = ns("generate_viewer_btn"),
                    label = "(Re-)Map to template", width = "100%"
                  )

                ) # .col-12
              ) # .row
            ), # Mapping Configurations

            ravedash::input_card(
              title = "Viewer Settings",

              shiny::fluidRow(
                shiny::column(
                  width = 12L,

                  shiny::selectInput(
                    inputId = ns("additional_volumes"),
                    label = "Additional Volumes",
                    multiple = TRUE,
                    choices = character(),
                    selected = character()
                  ),

                  shiny::selectInput(
                    inputId = ns("additional_surfaces"),
                    label = "Additional Surfaces",
                    multiple = TRUE,
                    choices = character(),
                    selected = character()
                  ),

                  shiny::selectInput(
                    inputId = ns("additional_annots"),
                    label = "Additional Annotations",
                    multiple = TRUE,
                    choices = character(),
                    selected = character()
                  ),


                  shiny::numericInput(
                    inputId = ns("electrode_radius"),
                    label = "Override electrode radius (mm)",
                    min = 0, max = 5, step = 0.01, width = "100%",
                    value = NA
                  )

                ) # .col-12
              ), # .row



              shiny::fluidRow(
                shiny::column(
                  width = 12L,

                  ravedash::run_analysis_button(label = "Update group viewer", width = "100%")

                ) # .col-12
              ) # .row

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
              'RAVE Group 3D viewer',
              class_body = "no-padding fill-width height-vh80 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                ravedash::output_gadget_container(
                  threeBrain::threejsBrainOutput(
                    outputId = ns("viewer3d"),
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
