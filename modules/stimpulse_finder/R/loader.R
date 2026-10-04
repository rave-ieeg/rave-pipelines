# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = "max-width:1444px;",
    input_ui = {
      # project & subject
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

        ravedash::flex_group_box(
          title = "Electrodes and Epoch",

          shidashi::flex_item(
            loader_electrodes$ui_func()
          ),

          shidashi::flex_item(
            shiny::selectInput(
              inputId = ns("loader_epoch"),
              label = "Stimulation epoch",
              choices = "[None]",
              selected = "[None]"
            )
          )
        ),

        footer = shiny::tagList(
          ravedash::load_data_button(label = "Load subject", width = "100%")
        )

      )
    },
    output_ui = {
      ravedash::output_card(
        title = "3D Viewer",
        class_body = "no-padding min-height-650 height-650",
        loader_viewer$ui_func()
      )
    }
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
          "loader_subject_code",
          "loader_electrode_text"
        )
      )

      epoch_name <- input$loader_epoch
      if (length(epoch_name) == 1 && epoch_name != "[None]") {
        settings$epoch_name <- epoch_name
      } else {
        settings$epoch_name <- NULL
      }

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      # --------------------- Run the pipeline! ---------------------

      pipeline$run(names = "loaded_signals")

      ravepipeline::logger("Data has been loaded loaded")
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(
      title = "Loading in progress",
      text = "Everything takes time. Some might need more patience than others."
    )
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      project_name <- loader_project$current_value
      subject_code <- loader_subject$current_value
      if (length(project_name) == 1 && length(subject_code) == 1) {
        subject <- ravecore::new_rave_subject(
          project_name = project_name, subject_code = subject_code, strict = FALSE)
        epoch_names <- c("[None]", subject$epoch_names)
        shiny::updateSelectInput(
          session = session,
          inputId = "loader_epoch",
          choices = epoch_names
        )
      }
    }),
    loader_project$current_value,
    loader_subject$current_value,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


}
