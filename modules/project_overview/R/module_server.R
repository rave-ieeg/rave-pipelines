
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({
      # Zip the
      src_path <- file.path(pipeline$pipeline_path, "build", "_site")
      dst_path <- file.path(pipeline$pipeline_path, "build", "reports.zip")
      if(file.exists(dst_path)) {
        unlink(dst_path)
      }
      utils::zip(zipfile = dst_path, files = src_path)
    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  output$report_container <- shiny::renderUI({

    loaded_flag <- ravedash::watch_data_loaded()
    shiny::validate(
      shiny::need(loaded_flag, message = "Report has not been generated yet.")
    )

    shiny::tags$iframe(
      src = sprintf("project_overview/index.html#%s", dipsaus::base64_urlencode(strftime(Sys.time()))),
      style = "height:100vh; width:100%; border:none;",
      class = "screen-height"
    )
  })

}
