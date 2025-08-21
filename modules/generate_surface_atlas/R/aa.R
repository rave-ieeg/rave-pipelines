library(ravedash)
# global variables for the module

# Stores global variables. These are required
module_id <- "generate_surface_atlas"
pipeline <- ravepipeline::pipeline(
  pipeline_name = "generate_surface_atlas",
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
#' @returns Logical variable of length one.
check_data_loaded <- function(first_time = FALSE){
  if(first_time) { return(FALSE) }

  project_name <- pipeline$get_settings("project_name")
  template_name <- pipeline$get_settings("template_name")

  msg <- sprintf("%s -> %s", project_name, template_name)

  ravedash::fire_rave_event('loader_message', msg)
  return(TRUE)

}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}



