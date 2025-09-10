# global variables for the module

# Stores global variables. These are required
pipeline_name <- "notch_filter"
pipeline_settings_file <- "settings.yaml"
module_id <- "notch_filter"
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
  if( first_time ) { return(FALSE) }

  project_name <- pipeline$get_settings("project_name")
  subject_code <- pipeline$get_settings("subject_code")

  # Invalid inputs
  if(length(project_name) != 1 || length(subject_code) != 1 ||
     is.na(project_name) || is.na(subject_code)) {
    ravedash::fire_rave_event('loader_message', NULL)
    return(FALSE)
  }

  subject <-  ravecore::RAVESubject$new(project_name = project_name,
                                        subject_code = subject_code,
                                        strict = FALSE)

  # If not imported at all, then returns FALSE
  if(any(subject$preprocess_settings$data_imported)) {
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

# Register RAVE pipeline instance so it's "globally" accessible

pipeline <- ravepipeline::pipeline(pipeline_name = pipeline_name,
                             settings_file = pipeline_settings_file,
                             paths = "./modules")

