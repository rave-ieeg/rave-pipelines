# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fixedRow(
      shiny::column(
        width = 6L, offset = 3L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          ravedash::flex_group_box(
            title = "Project and Subject",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_template_name"),
                label = "Template name",
                choices = names(threeBrain::available_templates()),
                selected = ravepipeline::raveio_getopt(
                  "threeBrain_template_subject", default = "cvs_avg35_inMNI152")
              )
            )
          ),

          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Load subject",
              type = "primary",
              width = "100%"
            )
          )

        )
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name"
        )
      )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline$set_settings(template_name = input$loader_template_name,
                            .list = settings)

      ravedash::shiny_alert2(
        title = "Checking template & project information",
        text = "The script might need to download template brain if missing. Please be patient...",
        icon = "info",
        auto_close = FALSE,
        buttons = FALSE
      )

      tryCatch(
        {
          pipeline$run(
            as_promise = FALSE,
            names = c("template_info", "subject_codes_filtered"),
            scheduler = "none",
            type = "callr"
          )
          Sys.sleep(0.5)
          ravedash::close_alert2(session = session)

          ravedash::fire_rave_event('data_changed', Sys.time())
        },
        error = function(e) {

          Sys.sleep(0.5)
          ravedash::close_alert2(session = session)

          ravedash::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the template data:\n\n",
              paste(e$message, collapse = "\n")
            ),
            icon = "error",
            danger_mode = TRUE,
            auto_close = FALSE
          )
        }
      )

    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
