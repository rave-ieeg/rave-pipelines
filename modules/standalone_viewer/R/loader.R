# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

  shiny::uiOutput(shiny::NS("standalone_viewer", "viewer_content"),
                   class = "fill-width",
                   style = "width:100%;height:100%;")

  # COMMENTED OUT: old ravedash standalone viewer UI
  # shiny::uiOutput("viewer", container = function(...) {
  #   shiny::div(
  #     class = "no-padding no-margin",
  #     style = "width:100vw; height:100vh",
  #     ...
  #   )
  # })
}


# Server functions for loader
loader_server <- function(input, output, session, ...) {

  shidashi::server_standalone_viewer(input, output, session, ...)

}
