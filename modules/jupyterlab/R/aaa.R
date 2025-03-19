library(ravedash)
# global variables for the module

# Stores global variables
pipeline_name <- "jupyterlab"
module_id <- "jupyterlab"
debug <- TRUE
local_data  <- dipsaus::fastmap2()


#' Function to check whether data is loaded.
#' @param first_time whether this function is run for the first time
#' @details The function will be called whenever \code{data_changed} event is
#' triggered. This function should only return either \code{TRUE} or
#' \code{FALSE} indicating the check results. If \code{TRUE} is returned,
#' \code{module_ui_main} will be called, and module 'UI' should be displayed.
#' If \code{FALSE} is returned, \code{open_loader} event will be dispatched,
#' resulting in calling function \code{module_ui_loader}.
#' @return Logical variable of length one.
check_data_loaded <- function(first_time = FALSE){

  if(isTRUE(ravepipeline::raveio_getopt("jupyter_disabled"))){
    ravedash::logger("Jupyter server is disabled", level = "error", use_glue = TRUE)
    return(FALSE)
  }
  if(!length(local_data$token)) {
    ravedash::logger("Jupyter token not found. Trying to obtain it")
    settings <- raveio::load_yaml("jupyter.yaml")
    local_data$host <- settings$host
    local_data$port <- settings$port
    servers <- rpymat::jupyter_server_list()
    local_data$token <- servers$token[servers$port == local_data$port]
  }

  session <- shiny::getDefaultReactiveDomain()

  if(length(local_data$token)) {
    session$sendCustomMessage("shidashi.hide_header", list())
    return(TRUE)
  }
  session$sendCustomMessage("shidashi.show_header", list())
  ravedash::logger("No active Jupyter server is detected", level = "error", use_glue = TRUE)
  return(FALSE)
}

# ----------- Some Utility functions for modules -----------


if(exists('debug') && isTRUE(get('debug'))){
  assign(".module_debug", environment(), envir = globalenv())
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}

