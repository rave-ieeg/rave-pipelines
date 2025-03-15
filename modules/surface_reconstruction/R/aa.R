library(ravedash)
# global variables for the module

# Stores global variables. These are required
module_id <- "surface_reconstruction"
debug <- TRUE
pipeline <- ravepipeline::pipeline(
  pipeline_name = "surface_reconstruction",
  paths = "./modules")

# autorecon_flags <- c(
#   "-autorecon1", "-all", "-autorecon2", "-autorecon3",
#   "-autorecon2-cp", "-autorecon2-wm", "-autorecon2-pial"
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
  if( first_time ) {
    ravedash::fire_rave_event('loader_message', NULL)
    return(FALSE)
  }
  tryCatch({
    check_result <- pipeline$read("check_result")
    if(is.list(check_result) && all(
      c("project_name", "subject_code", "fs_path", "fs_reconstructed",
        "skip_recon", "skip_coregistration", "has_dcm2niix", "has_freesurfer",
        "has_flirt", "path_mri", "path_ct", "messages", "warnings") %in% names(check_result)
    )) {
      ravedash::fire_rave_event('loader_message', sprintf("Subject: %s", paste(check_result$subject_code, collapse = "")))
      return(TRUE)
    }
  }, error = function(e){

  })
  ravedash::fire_rave_event('loader_message', NULL)
  return(FALSE)
}



# ----------- Initial configurations -----------

# Change the logger level when `debug` is enabled
if(exists('debug', inherits = FALSE) && isTRUE(get('debug'))){
  ravedash::logger_threshold("trace", module_id = module_id)
} else {
  ravedash::logger_threshold("info", module_id = module_id)
}


