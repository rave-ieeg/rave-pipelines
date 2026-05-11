
module_ui_main <- function() {
  shiny::uiOutput(ns("jupyter_iframe"), style = "width:100%; height:100vh")
}

module_server <- function(input, output, session, ...) {

  output$jupyter_iframe <- shiny::renderUI({
    data_loaded <- ravedash::watch_data_loaded()
    if (data_loaded) {

      jupyter_url <- sprintf("http://%s:%s/jupyter/lab?token=%s",
                             local_data$host, local_data$port, local_data$token)

      shiny::tags$iframe(
        src = jupyter_url,
        width = "100%",
        height = "100%"
      )

    }
  })

}
