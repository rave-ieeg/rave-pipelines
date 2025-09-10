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

  local_data <- dipsaus::fastmap2()

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
    # ?type=widget&project_name=YAEL&subject_code=yael_demo_001&report_name=electrodeview&module=standalone_report

    # validate the parameters
    subject <- ravecore::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
    report_html <- NULL
    if(length(report_filename)) {
      report_html <- file.path(subject$report_path, report_filename, "report.html")
    } else {
      report_name <- query_list$query$report_name
      reports <- list.files(
        subject$report_path,
        pattern = sprintf("^report-%s_datetime-[0-9]{6}T[0-9]{6}_", report_name)
      )
      if(length(reports)) {
        report_filename <- sort(reports, decreasing = TRUE)[[1]]
        report_html <- file.path(subject$report_path, report_filename, "report.html")
      }
    }

    local_data$report_html <- report_html

    shiny::validate(
      shiny::need(dir.exists(subject$report_path), message = "Subject has no report path"),
      shiny::need(length(report_filename) == 1 && grepl("^[a-zA-Z0-9_-]+$", report_filename),
                  message = "Invalid report filename"),
      shiny::need(length(report_html) == 1 && !is.na(report_html) &&
                    file.exists(report_html), message = "Report path is invalid")
    )

    srcdoc <- paste(readLines(report_html), collapse = "\n")
    shiny::tagList(
      shiny::div(
        style = "position:absolute;left:0;top:0;",
        shiny::downloadButton(
          outputId = ns("download_btn"),
          label = "Save this report"
        )
      ),
      shiny::tags$iframe(srcdoc = srcdoc, class = "fill no-marging no-padding no-border display-block")
    )
  })

  output$download_btn <- shiny::downloadHandler(
    filename = function(...) {
      if(!length(local_data$report_html)) { return("report.html") }
      name <- basename(dirname(local_data$report_html))
      if(!endsWith(name, ".html")) {
        name <- sprintf("%s.html", name)
      }
      name
    },
    content = function(con) {
      file.copy(local_data$report_html, con, overwrite = TRUE)
    }
  )

}
