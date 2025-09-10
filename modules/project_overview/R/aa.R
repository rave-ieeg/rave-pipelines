library(ravedash)
# global variables for the module

# Stores global variables. These are required
module_id <- "project_overview"
pipeline <- ravepipeline::pipeline(
  pipeline_name = "project_overview",
  settings_file = "settings.yaml",
  paths = "./modules")
debug <- TRUE

static_path_added <- FALSE

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
  # check if the build/_site/index.html exists
  site_path <- "modules/project_overview/build/_site/"
  built_index_path <- file.path(site_path, "index.html")
  if(file.exists(built_index_path)) {

    if(!static_path_added) {
      shiny::addResourcePath(prefix = "project_overview",
                             directoryPath = normalizePath(site_path, winslash = "/"))
      static_path_added <<- TRUE
    }

    return(TRUE)
  }
  FALSE
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravepipeline::logger_threshold("trace", module_id = module_id)
} else {
  ravepipeline::logger_threshold("info", module_id = module_id)
}



