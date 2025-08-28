library(ravedash)
# global variables for the module

# Stores global variables. These are required
module_id <- "power_clust"
pipeline <- ravepipeline::pipeline(
  pipeline_name = "power_clust",
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
  if(first_time){
    ravedash::fire_rave_event('loader_message', NULL)
    return(FALSE)
  }

  re <- tryCatch({
    repo <- pipeline$read('repository')
    if(!inherits(repo, "rave_prepare_power")) {
      stop("No repository found")
    }
    short_msg <- sprintf("%s [%s, %s]", repo$subject$subject_id, repo$epoch_name, repo$reference_name)
    ravedash::fire_rave_event('loader_message', short_msg)
    TRUE
  }, error = function(e){
    ravedash::fire_rave_event('loader_message', NULL)
    FALSE
  })

  re
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravepipeline::logger_threshold("trace", module_id = module_id)
} else {
  ravepipeline::logger_threshold("info", module_id = module_id)
}



