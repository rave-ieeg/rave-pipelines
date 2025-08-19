library(ravedash)
# global variables for the module

# Stores global variables. These are required
module_id <- "block_explorer"
pipeline <- ravepipeline::pipeline(
  pipeline_name = "block_explorer",
  settings_file = "settings.yaml",
  paths = "./modules")
debug <- TRUE

graphics_matplot_max_points <- 200000

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
  # if(first_time) { return(FALSE) }
  re <- tryCatch({
    repo <- pipeline$read('repository')
    if(!inherits(repo, "rave_prepare_raw_voltage")) {
      stop("No repository found")
    }
    ravedash::fire_rave_event('loader_message', repo$subject$subject_id)
    TRUE
  }, error = function(e){
    ravedash::fire_rave_event('loader_message', NULL)
    FALSE
  })
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravepipeline::logger_threshold("trace", module_id = module_id)
} else {
  ravepipeline::logger_threshold("info", module_id = module_id)
}



