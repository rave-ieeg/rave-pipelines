# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    shiny::div(
      style = "width:100vw; height:80vh; display:flex; align-items:center; justify-content:center;",
      shiny::h2("Loading report... Please wait...", style = "margin:0;")
    ),
    shiny::uiOutput(ns("viewer"), container = function(...) {
      shiny::div(
        style = "position:fixed; padding:0; margin:0; width:100vw; height:100vh; top:0; left:0; z-index: 9999999; background:white",
        ...
      )
    })
  )
}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  # query_string <- "/?type=widget&output_id=plot_overall&rave_id=Pnd8MuxNVsZGcbrRWn8G&module=standalone_viewer"
  query_string <- session$clientData$url_search

  query_list <- httr::parse_url(query_string)

  # This is a widget that should belongs to some module
  output$viewer <- shiny::renderUI({
    # ..../test/DemoSubject/reports/report-diagnostics_datetime-250811T165425_notch_filter/report.html
    project_name <- query_list$query$project_name
    subject_code <- query_list$query$subject_code
    report_filename <- query_list$query$report_filename

    # project_name <- "test"
    # subject_code <- "DemoSubject"
    # report_filename <- "report-diagnostics_datetime-250811T165425_notch_filter"
    # 127.0.0.1:17283/?type=widget&project_name=test&subject_code=DemoSubject&report_filename=report-diagnostics_datetime-250811T165425_notch_filter&module=standalone_report&shared_id=kJKCof6lYRNqaFRst6Yr5Je6xp

    # validate the parameters
    subject <- ravecore::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
    report_path <- file.path(subject$report_path, report_filename, "report.html")

    shiny::validate(
      shiny::need(dir.exists(subject$report_path), message = "Subject has no report path"),
      shiny::need(grepl("^[a-zA-Z0-9_-]+$", report_filename), message = "Invalid report filename"),
      shiny::need(length(report_path) == 1 && !is.na(report_path) &&
                    file.exists(report_path), message = "Report path is invalid")
    )

    srcdoc <- paste(readLines(report_path), collapse = "\n")
    shiny::tags$iframe(srcdoc = srcdoc, class = "fill no-marging no-padding no-border display-block")
  })

}
