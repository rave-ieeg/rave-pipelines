# .module_id  <-  "jupyterlab"
# if(interactive() && !dipsaus::shiny_is_running()){
#   setwd(rstudioapi::getActiveProject())
#   source('./modules/jupyterlab/R/aaa.R')
# }


module_ui_loader <- function(session = shiny::getDefaultReactiveDomain()) {


  ravedash::simple_layout(
    {
      shiny::div(
        shiny::p("Jupyter has been disabled. Please check RAVE settings, or ask your system administrator for help.")
      )
    }, {}, input_width = 10L
  )


}

server_loader <- function(input, output, session, ...) {
  # shiny::observe({
  #   print(session$clientData)
  # })

}
