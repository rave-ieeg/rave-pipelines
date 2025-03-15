# global variables for the module

# Stores global variables. These are required
module_id <- "custom_3d_viewer"
pipeline <- ravepipeline::pipeline(
  pipeline_name = "custom_3d_viewer",
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
  # Always use loading screen
  if( first_time ) {
    ravedash::fire_rave_event('loader_message', NULL)
    return(FALSE)
  }

  tryCatch({
    loaded_brain <- pipeline$read("loaded_brain")

    has_electrode <- is.data.frame(loaded_brain$electrode_table)

    msg <- sprintf("%s, electrodes [%s]", loaded_brain$subject_code,
                   ifelse(has_electrode, nrow(loaded_brain$electrode_table),
                          "missing"))

    ravedash::fire_rave_event('loader_message', msg)
    return(TRUE)

  }, error = function(e) {
    ravedash::fire_rave_event('loader_message', NULL)
    return(FALSE)
  })



}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}



