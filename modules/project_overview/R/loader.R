# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  all_projects <- raveio::get_projects(refresh = FALSE)
  available_template_subjects <- names(threeBrain::available_templates())

  shiny::div(
    class = "container",
    shiny::fixedRow(
      shiny::column(
        width = 6L, offset = 3L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          ravedash::flex_group_box(
            title = "Projects",

            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_project_names"),
                label = "Project names",
                choices = all_projects,
                selected = pipeline$get_settings("project_names"),
                multiple = TRUE
              ),
              shiny::p(
                shiny::tags$small("Leave it blank to select all projects.")
              )
            ),

            shidashi::flex_break(),

            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_template_subject"),
                label = "Template name",
                choices = available_template_subjects,
                selected = pipeline$get_settings(
                  "template_subject",
                  default = ravepipeline::raveio_getopt("threeBrain_template_subject", default = "cvs_avg35_inMNI152"),
                  constraint = available_template_subjects
                )
              )
            )
          ),

          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Generate reports",
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
      settings <- list(
        project_names = input$loader_project_names,
        template_subject = input$loader_template_subject
      )

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      ravedash::shiny_alert2(
        title = "Generating reports",
        text = "Please be patient...",
        icon = "info",
        auto_close = FALSE,
        buttons = FALSE
      )

      tryCatch(
        {
          pipeline$run(
            as_promise = FALSE,
            names = c("project_overview"),
            scheduler = "none",
            type = "vanilla"
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
              "Found an error while generating the report:\n\n",
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
