# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

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
            ravedash::load_data_button(label = "Load subject", width = "100%")
          )

        )
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...) {

  # Runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    {
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

      pipeline$run(
        as_promise = FALSE,
        names = c("template_info", "subject_codes_filtered"),
        scheduler = "none",
        type = "callr"
      )
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(
      title = "Checking template & project information",
      text = "The script might need to download template brain if missing. Please be patient..."
    )
  )

}
