#' This file runs right after `aa.R`. The goal of this script
#' is to generate a bunch of preset input components
#' and create a component container that collate all the components
NULL

# Create a component container
component_container <- ravedash::new_rave_shiny_component_container(
  module_id = module_id, pipeline_name = pipeline$pipeline_name
)


# Define components
loader_project <- ravedash::presets_loader_project()
loader_subject <- ravedash::presets_loader_subject(checks = "notch")
loader_reference <- ravedash::presets_loader_reference(
  mode = "create", label = "Create from existing reference profile")

loader_sync1 <- ravedash::presets_loader_sync_project_subject(id = "loader_sync_from_recent_project_subject")
loader_sync2 <- ravedash::presets_loader_sync_project_subject(
  id = "loader_sync_from_wavelet_project_subject",
  from_module = "wavelet_module",
  label = "Sync subject from module [Wavelet]"
)

# Register the components
component_container$add_components(
  loader_project, loader_subject, loader_reference,
  loader_sync1, loader_sync2
)


