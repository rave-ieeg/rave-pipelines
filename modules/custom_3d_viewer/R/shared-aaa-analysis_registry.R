
# Registry of available "quick analysis" definitions for the object
# selector. Each entry maps a user-facing label to the name of an analysis
# definition (a list with `$ui` and `$analysis`) defined elsewhere, e.g.
# R/shared-analysis_collision_detection.R. The modal UI and the settings
# persisted to `settings.yaml` are always generated from that definition's
# own `$ui` list -- never hand-coded per analysis in the module UI files.
analysis_registry <- list()

# Looks up an analysis definition by internal name. The shape of the
# definition is validated once, at registration time, by `create_analysis`.
get_analysis_definition <- function(name) {
  if (!isTRUE(name %in% names(analysis_registry))) {
    return(NULL)
  }
  analysis_registry[[name]]
}

create_analysis <- function(name, description) {
  # ravepipeline::pipeline_setup_rmd("custom_3d_viewer")
  analysis <- ravepipeline::RAVEPipelineAnalysis$new(
    name = name,
    description = paste(description, collapse = ""),
    namespace = "custom_3d_viewer"
  )
  analysis_registry[[name]] <<- analysis
  analysis
}


