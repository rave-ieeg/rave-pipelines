# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

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
            shiny::selectInput(
              inputId = ns("loader_project_name"),
              label = "Project name",
              choices = all_projects,
              selected = saved_project %OF% all_projects
            )
          )
        ),

        footer = shiny::tagList(
          dipsaus::actionButtonStyled(
            inputId = ns("loader_ready_btn"),
            label = "Load project",
            type = "primary",
            width = "100%"
          )
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
loader_server <- function(input, output, session, ...){

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

  # Load project button
  shiny::bindEvent(
    ravedash::safe_observe({

      project_name <- input$loader_project_name
      if (!length(project_name) || !nzchar(project_name)) {
        ravedash::shiny_alert2(
          title = "Error",
          text = "Please select a project.",
          icon = "error",
          session = session
        )
        return()
      }

      # Save project_name into pipeline settings
      pipeline$set_settings(project_name = project_name)

      # Run pipeline through subject_summary (validates project + collects info)
      res <- pipeline$run(
        as_promise = TRUE,
        names = "subject_summary",
        return_values = FALSE
      )

      res$promise$then(
        onFulfilled = function(e) {
          dipsaus::close_alert2()
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravepipeline::logger("Project data has been loaded")
          ravedash::session_setopt(project_name = project_name)
        },
        onRejected = ravedash::error_alert
      )

    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
