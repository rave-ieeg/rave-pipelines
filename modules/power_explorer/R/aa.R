
# global variables for the module

# Stores global variables
module_id <- "power_explorer"
debug <- TRUE
pipeline <- raveio::pipeline(
  pipeline_name = "power_explorer",
  paths = "./modules")


# get_my_preference <- raveio::use_global_preferences(
#   name = "ravebuiltins", # <- give it a name just for your global prefs
#   item1 = "A", ... # <- default items
# )
# get_my_preference("item1", "default is not found")
#

# analysis_lock_choices <- c("Unlocked", "Lock frequency", "Lock time")
# max_analysis_ranges <- 2
# gray_label_color <- "#c8c9ca"

# auto_recalculate_onchange <- c(
#   "merge_hemisphere_labels",
#   "analysis_lock",
#   "baseline_windows",
#   "global_baseline_choice",
#   "unit_of_analysis",
#   "analysis_ranges",
#   "electrode_text",
#   "condition_groups"
# )



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

  # if(first_time){
  #   ravedash::fire_rave_event('loader_message', NULL)
  #   re <- FALSE
  #   return(re)
  # }

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



# ----------- Some Utility functions for modules -----------


if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}

