
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  server_tools <- get_default_handlers(session = session)

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({
      # Zip the
      ravedash::show_notification(
        message = "Packaging the reports in the background...",
        title = "Exporting",
        type = "info",
        autohide = FALSE,
        session = session,
        class = ns("export_report")
      )

      job_id <- ravepipeline::start_job(
        fun_args = list(pipeline_path = pipeline$pipeline_path),
        packages = "utils", method = "mirai",
        fun = function(pipeline_path) {
          src_path <- file.path(pipeline_path, "build", "_site")
          dst_path <- file.path(pipeline_path, "build", "reports.zip")
          if(file.exists(dst_path)) {
            unlink(dst_path)
          }

          cwd <- getwd()
          setwd(src_path)
          on.exit({ setwd(cwd) })

          utils::zip(zipfile = "../reports.zip", files = list.files("."))

          return(dst_path)
        }
      )

      promise <- promises::as.promise(job_id)

      promise$then(
        onFulfilled = function(dst_path) {
          ravedash::clear_notifications(class = ns("export_report"), session = session)

          shiny::showModal(
            session = session,
            shiny::modalDialog(
              title = "Download packaged reports",
              easyClose = FALSE,
              size = "m",
              footer = shiny::tagList(
                shiny::modalButton("Close"),
                shiny::downloadButton(
                  outputId = ns("download_btn"),
                  label = "Download",
                  class = "btn-primary",
                  icon = ravedash::shiny_icons$download
                )
              ),
              shiny::p(
                "The overview reports have been generated at ",
                shiny::br(),
                dst_path
              )
            )
          )
        },
        onRejected = function(e) {
          ravedash::clear_notifications(class = ns("export_report"), session = session)
          ravedash::error_notification(e, session = session)
        }
      )

      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$download_btn <- shiny::downloadHandler(
    filename = "project_reports.zip",
    contentType = "application/zip",
    content = function(con) {
      dst_path <- file.path(pipeline$pipeline_path, "build", "reports.zip")
      file.copy(dst_path, con)
      try(silent = TRUE, { shiny::removeModal(session = session) })
      return(con)
    }
  )

  output$report_container <- shiny::renderUI({

    loaded_flag <- ravedash::watch_data_loaded()
    shiny::validate(
      shiny::need(loaded_flag, message = "Report has not been generated yet.")
    )


    shiny::tags$iframe(
      src = sprintf("project_overview/index.html?t=%s", dipsaus::base64_urlencode(strftime(Sys.time()))),
      # style = "height:100vh; width:100%; border:none;",
      style = "border:none;",
      class = "fill", onLoad = "resizeIframe(this)"
    )

  })

}
