library(ravedash)
# global variables for the module

# Stores global variables
pipeline_name <- "import_lfp_native"
module_id <- "import_lfp_native"
pipeline_settings_file <- "settings.yaml"
debug <- TRUE

check_data_loaded <- function(first_time = FALSE){
  FALSE
}


# ----------- Some Utility functions for modules -----------


if(exists('debug') && isTRUE(get('debug'))){
  assign(".module_debug", environment(), envir = globalenv())
  ravepipeline::logger_threshold("trace", module_id = module_id)
} else {
  ravepipeline::logger_threshold("info", module_id = module_id)
}

pipeline <- ravepipeline::pipeline(
  pipeline_name = pipeline_name,
  settings_file = pipeline_settings_file,
  paths = "./modules"
)


