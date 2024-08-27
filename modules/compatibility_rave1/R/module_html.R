

module_html <- function(){

  shiny::fixedPage(
    shiny::fixedRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Quick access",
              shiny::tags$ul(
                shiny::tags$li(
                  shiny::actionLink(
                    inputId = ns("quickaccess_data_integrity"),
                    label = "Data integrity check"
                  )
                ),
                shiny::tags$li(
                  shiny::actionLink(
                    inputId = ns("quickaccess_compatibility"),
                    label = "Backward compatibility"
                  )
                ),
                shiny::tags$li(
                  shiny::actionLink(
                    inputId = ns("quickaccess_export"),
                    label = "Export data"
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
              title = 'Data integrity check',
              shiny::p("Data integrity check examines the data files for any possible issues within the subject. Basic checks only validate small files such as preprocess configuration and meta data. Full checks will also  validate large data, looking for broken or obsolete files."),
              shidashi::flip_box(
                front = shiny::uiOutput(ns("validation_check")),
                back = DT::DTOutput(ns("validation_table"), width = "100%")
              ),
              footer = shidashi::flex_container(
                align_content = "flex-end",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("validation_version"),
                    label = "Data version",
                    choices = c("2", "1"),
                    selected = "2",
                    selectize = FALSE
                  )
                ),
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("validation_mode"),
                    label = "Validation mode",
                    choices = c("normal", "basic"),
                    selected = "normal",
                    selectize = FALSE
                  )
                ),
                shidashi::flex_item(
                  shiny::div(
                    class = "form-group",
                    ravedash::run_analysis_button(label = "Validate subject", width = "100%")
                  )
                )
              )
            ),

            ravedash::output_card(
              title = 'Backward compatibility',
              shiny::p("Convert data format so subjects can be loaded by RAVE 1.0 modules."),
              dipsaus::actionButtonStyled(
                inputId = ns("compatibility_do"),
                label = "Make this subject RAVE 1.0 compatible"
              )
            ),

            ravedash::output_card(
              title = "Export data",

              shiny::fluidRow(

                shiny::column(
                  width = 6L,

                  shiny::selectInput(
                    inputId = ns("export_type"),
                    label = "Data type",
                    choices = c("power", "voltage", "raw-voltage")
                  ),

                  shiny::textInput(
                    inputId = ns("export_electrode"),
                    label = "Electrode channels",
                    value = "",
                    placeholder = "Leave blank to export all"
                  ),

                  shiny::selectInput(
                    inputId = ns("export_reference"),
                    label = "Reference name",
                    choices = character()
                  )

                ),

                shiny::column(
                  width = 6L,

                  shiny::selectInput(
                    inputId = ns("export_epoch"),
                    label = "Epoch name",
                    choices = character()
                  ),

                  shiny::fluidRow(
                    shiny::column(
                      width = 6L,
                      shiny::numericInput(
                        inputId = ns("export_pre"),
                        label = "Pre-onset",
                        max = 0, step = 0.1, value = -1
                      )
                    ),

                    shiny::column(
                      width = 6L,
                      shiny::numericInput(
                        inputId = ns("export_post"),
                        label = "Post-onset",
                        min = 0, step = 0.1, value = 2
                      )
                    )
                  )

                )

              ),


              dipsaus::actionButtonStyled(
                inputId = ns("export_do"),
                label = "Generate exports"
              ),
              shiny::downloadButton(
                outputId = ns("export_download_do"),
                label = "Export & download",
                icon = ravedash::shiny_icons$download
              )
            )
          )
        )
      )

    )
  )
}
