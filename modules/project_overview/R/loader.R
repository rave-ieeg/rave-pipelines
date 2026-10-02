# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

  all_projects <- ravecore::get_projects(refresh = FALSE)
  saved_project <- pipeline$get_settings("project_name", default = "")

  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = "max-width:1444px;",
    input_ui = {
      ravedash::input_card(
        title = "Data Selection",
        class_header = "",

        ravedash::flex_group_box(
          title = "Project",

          shidashi::flex_item(
            shidashi::register_input(
              shiny::selectInput(
                inputId = ns("loader_project_name"),
                label = "Project name",
                choices = all_projects,
                selected = saved_project %OF% all_projects
              ),
              tooltip = "The RAVE project to summarize.",
              inputId = "loader_project_name",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "RAVE project to load (a project name such as 'demo'). The",
                "loader card 'Project Info' lists its subjects. Read by script",
                "`load_data`."
              )
            )
          )
        ),

        footer = shiny::tagList(
          ravedash::load_data_button(label = "Load project", width = "100%")
        )
      )
    },
    output_ui = {
      ravedash::output_card(
        title = "Project Info",
        class_body = "padding-10",
        shiny::uiOutput(ns("loader_project_info"))
      )
    }
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...) {

  # Show basic project info in the output panel
  output$loader_project_info <- shiny::renderUI({
    project_name <- input$loader_project_name
    if (!length(project_name) || !nzchar(project_name)) {
      return(shiny::p("Select a project to load.", class = "text-muted"))
    }

    project <- tryCatch(
      ravecore::as_rave_project(project_name, strict = FALSE),
      error = function(e) NULL
    )
    if (is.null(project)) {
      return(shiny::p("Invalid project.", class = "text-danger"))
    }

    subjects <- project$subjects()
    shiny::tagList(
      shiny::tags$dl(
        shiny::tags$dt("Project"),
        shiny::tags$dd(project_name),
        shiny::tags$dt("Subjects"),
        shiny::tags$dd(sprintf("%d subjects: %s", length(subjects),
                               paste(subjects, collapse = ", ")))
      )
    )
  })

  # Load project: runs when `ravedash::load_data_button()` is clicked, or
  # through `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    description = c(
      "Load the project chosen in `loader_project_name` (same as clicking",
      "'Load project'): it saves the project to the pipeline settings and",
      "builds the per-subject summary (target `subject_summary`). It writes",
      "nothing into the project. Returns one line per subject (at most 99):",
      "'<code> (<n> electrodes; imported/notch/wavelet/localized; epochs <n>,",
      "references <n>)', where each preprocessing step is 'yes' only when it",
      "is done for every electrode. The subject codes are the choices of",
      "`subject_codes`. A failed load returns the error ('Please select a",
      "project.' when none is chosen); people then see 'Found an error while",
      "running script'."
    ),
    {
      project_name <- input$loader_project_name
      if (!length(project_name) || !nzchar(project_name)) {
        stop("Please select a project.")
      }

      # Save project_name into pipeline settings
      pipeline$set_settings(project_name = project_name)

      # Run pipeline through subject_summary (validates project + collects info)
      pipeline$run(
        names = "subject_summary",
        return_values = FALSE
      )

      ravepipeline::logger("Project data has been loaded")
      ravedash::session_setopt(project_name = project_name)

      # Summary for agents: the subjects and their state. It must never make
      # the load fail
      tryCatch({
        subject_summary <- pipeline$read("subject_summary")
        yes_no <- function(x) { ifelse(x, "yes", "no") }
        lines <- sprintf(
          "%s (%d electrodes; imported %s, notch %s, wavelet %s, localized %s; epochs %d, references %d)",
          subject_summary$Subject, subject_summary$Electrodes,
          yes_no(subject_summary$Imported), yes_no(subject_summary$Notch),
          yes_no(subject_summary$Wavelet), yes_no(subject_summary$Localized),
          subject_summary$Epoch_tables, subject_summary$Reference_tables
        )
        if (length(lines) > 98) {
          lines <- c(lines[seq_len(97)],
                     sprintf("... %d more subjects", length(lines) - 97))
        }
        c(sprintf("Loaded project %s: %d subjects", project_name,
                  nrow(subject_summary)), lines)
      }, error = function(e) {
        sprintf("Loaded project %s", project_name)
      })
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(title = "Loading in progress")
  )

}
