# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(width = 3L),
      shiny::column(
        width = 6L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          ravedash::flex_group_box(
            title = "Project and Subject",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              loader_subject$ui_func()
            )
          ),

          footer = shiny::tagList(
            loader_sync1$ui_func(),
            shiny::br(),
            loader_sync2$ui_func(),
            shiny::hr(),
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
          "loader_project_name",
          "loader_subject_code"
        )
      )

      subject <- ravecore::RAVESubject$new(project_name = settings$project_name,
                                         subject_code = settings$subject_code,
                                         strict = FALSE)
      if (!length(subject$blocks)) {
        stop("The subject has no session blocks. Please import signals first")
      }

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      ravepipeline::logger("Data has been loaded loaded")

      # Save session-based state: project name & subject code
      ravedash::session_setopt(
        project_name = settings$project_name,
        subject_code = settings$subject_code
      )
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(title = "Loading in progress")
  )

}
