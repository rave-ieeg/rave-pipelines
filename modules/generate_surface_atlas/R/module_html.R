# Subject codes
# mapping methods
# flip_hemispheres
# value_name <- "Fun_Channels"
# value_selector

# threshold distance when generating template atlas - when electrode contact
# is too faraway from the surface, the surface value should not be derived
# from the electrode
mapping_threshold <- 1

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
                  shiny::selectInput(
                    inputId = ns("subject_codes"),
                    label = "Subject selector",
                    multiple = TRUE,
                    choices = "",
                    selected = '',
                    width = "100%"
                  )
                ), # col-12

                shiny::column(
                  width = 12L,
                  shiny::selectInput(
                    inputId = ns("mapping_method"),
                    label = "Mapping method",
                    multiple = FALSE,
                    choices = c("high-density ECoG"),
                    selected = "high-density ECoG",
                    width = "100%"
                  ),

                  # shiny::conditionalPanel(
                    condition = sprintf('input["%s"] === "high-density micro-ECoG"', ns('mapping_method')),
                    shiny::fluidRow(
                      shiny::column(
                        width = 6,
                        shiny::numericInput(
                          inputId = ns("mapping_param_micro_ecog_width"),
                          label = "Segment width",
                          value = 16,
                          min = 1L,
                          step = 1L,
                          max = 40,
                          width = "100%"
                        )
                      ), # col-6
                      shiny::column(
                        width = 6,
                        shiny::numericInput(
                          inputId = ns("mapping_param_micro_ecog_height"),
                          label = "Segment height",
                          value = 16,
                          min = 1L,
                          step = 1L,
                          max = 40,
                          width = "100%"
                        )
                      ),

                      shiny::column(
                        width = 12,

                        shiny::sliderInput(
                          inputId = ns("mapping_param_micro_ecog_interpolator"),
                          label = "Volume-surface trade-off",
                          min = 0,
                          max = 1,
                          value = 0.3,
                          step = 0.1,
                          round = -1,
                          width = "100%"
                        ),
                        shiny::tags$small("0=fully volumetric, 1=fully surface-based")
                      ),

                      # flip_hemisphere = NULL
                      shiny::column(
                        width = 12,

                        shiny::selectInput(
                          inputId = ns("mapping_param_micro_ecog_flip_hemisphere"),
                          label = "Flip hemisphere",
                          choices = character(),
                          selected = character(),
                          multiple = TRUE,
                          width = "100%"
                        )
                      )

                    ), #.row

                  # ), # condition === 'high-density ECoG'

                  dipsaus::actionButtonStyled(
                    inputId = ns("map_to_template"),
                    label = "Map to template",
                    width = "100%"
                  )

                ), # col-12
              ) # .row
            ), # Subject Configurations

            ravedash::input_card(
              title = "Data Configurations",
              shiny::fluidRow(
                shiny::column(
                  width = 12L,

                  shiny::selectInput(
                    inputId = ns("value_name"),
                    label = "Data name",
                    choices = c("[None]", "[Subject]"),
                    selected = "[None]"
                  ),

                  shiny::selectInput(
                    inputId = ns("value_type"),
                    label = "Data type",
                    choices = c("categorical", "numerical"),
                    selected = "categorical"
                  ),

                  shiny::numericInput(
                    inputId = ns("mapping_threshold"),
                    label = "Distance threshold (mm)",
                    min = 0, max = 100, step = 0.1, value = 1
                  ),

                  dipsaus::actionButtonStyled(
                    inputId = ns("generate_atlas"),
                    label = "Generate group atlas",
                    icon = ravedash::shiny_icons$magic,
                    width = "100%"
                  )

                )

              ) #.row
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
              'RAVE Template Brain',
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
