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
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              loader_sync1$ui_func()
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
    description = c(
      "Load the project and subject chosen in the loader (same as clicking",
      "'Load subject'; inputs `loader_project_name`, `loader_subject_code`).",
      "Any subject loads, even an incomplete or broken one: checking such",
      "subjects is what this module is for. Returns a summary: the electrodes,",
      "those Notch-filtered and those with wavelet (power), and the epoch and",
      "reference names, which are the choices of `export_epoch` and",
      "`export_reference`."
    ),
    {
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code"
        )
      )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      pipeline$run(
        names = "subject",
        scheduler = "none",
        type = "vanilla",
        # async = TRUE,
        callr_function = NULL
      )

      subject <- pipeline$read("subject")
      if (inherits(subject, "RAVESubject")) {
        component_container$data$subject <- subject
      }

      ravepipeline::logger("Data has been loaded loaded")

      # Save session-based state: project name & subject code
      ravedash::session_setopt(
        project_name = settings$project_name,
        subject_code = settings$subject_code
      )

      # Summary for agents: the choices of the export inputs, and what has
      # been preprocessed. A broken subject must still load: never fail here
      tryCatch({
        or_none <- function(x) {
          x <- as.character(x)
          x <- x[nzchar(x)]
          if (length(x)) paste(x, collapse = ", ") else "none"
        }
        electrodes <- subject$electrodes
        sprintf(
          "Loaded %s: electrodes %s; Notch-filtered %s; wavelet %s; epochs: %s; references: %s",
          subject$subject_id,
          or_none(dipsaus::deparse_svec(electrodes)),
          or_none(dipsaus::deparse_svec(electrodes[subject$notch_filtered])),
          or_none(dipsaus::deparse_svec(electrodes[subject$has_wavelet])),
          or_none(subject$epoch_names),
          or_none(subject$reference_names)
        )
      }, error = function(e) {
        sprintf("Loaded %s/%s", settings$project_name, settings$subject_code)
      })
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed"
  )


}
