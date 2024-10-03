library(shiny)
library(shidashi)

server <- function(input, output, session, ...){
  # For loader
  options("ravedash.auto_watch_data" = FALSE)
  loader_server(input, output, session, ...)

  # For the main module
  options("ravedash.auto_watch_data" = TRUE)
  module_server(input, output, session, ...)

  options("ravedash.auto_watch_data" = FALSE)
}
