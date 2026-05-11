library(shiny)
library(shidashi)

server <- function(input, output, session, ...) {

  server_loader(input, output, session, ...)
  module_server(input, output, session, ...)


}
