library(ravedash)
# global variables for the module

# Stores global variables. These are required
module_id <- "stimpulse_finder"
pipeline <- ravepipeline::pipeline(
  pipeline_name = "stimpulse_finder",
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
  # Always use loading screen
  if(first_time) { return(FALSE) }

  loaded_signals <- pipeline['loaded_signals']
  array <- loaded_signals[[1]]$`@impl`
  subject_id <- array$get_header("subject_id")
  electrode <- dimnames(array)$Electrode

  ravedash::fire_rave_event(
    'loader_message',
    sprintf("%s [%s]", subject_id, electrode[[1]])
  )
  return(TRUE)
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravepipeline::logger_threshold("trace", module_id = module_id)
} else {
  ravepipeline::logger_threshold("info", module_id = module_id)
}



