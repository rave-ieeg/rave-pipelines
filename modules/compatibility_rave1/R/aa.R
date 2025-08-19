library(ravedash)
# global variables for the module

# Stores global variables. These are required
module_id <- "compatibility_rave1"
pipeline <- ravepipeline::pipeline(
  pipeline_name = "compatibility_rave1",
  settings_file = "settings.yaml",
  paths = "./modules")
debug <- TRUE

#' Function to check whether data is loaded.
#' @param first_time whether this function is run for the first time
#' @details The function will be called whenever \code{data_changed} event is
#' triggered. This function should only return either \code{TRUE} or
#' \code{FALSE} indicating the check results. If \code{TRUE} is returned,
#' \code{module_html} will be called, and module 'UI' should be displayed.
#' If \code{FALSE} is returned, \code{open_loader} event will be dispatched,
#' resulting in calling function \code{loader_html}.
#' @return Logical variable of length one.
check_data_loaded <- function(first_time = FALSE){
  if(first_time) { return(FALSE) }

  subject <- component_container$data$subject
  if(inherits(subject, "RAVESubject")) {
    ravedash::fire_rave_event('loader_message', subject$subject_id)
    return(TRUE)
  }

  ravedash::fire_rave_event('loader_message', NULL)
  return(FALSE)
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravepipeline::logger_threshold("trace", module_id = module_id)
} else {
  ravepipeline::logger_threshold("info", module_id = module_id)
}



