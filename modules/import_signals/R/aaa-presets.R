#' This file runs right after `aa.R`. The goal of this script
#' is to generate a bunch of preset input components
#' and create a component container that collate all the components
NULL

# Create a component container
component_container <- ravedash::new_rave_shiny_component_container(
  module_id = module_id, pipeline_name = pipeline$pipeline_name,
  settings_file = "settings.yaml"
)


# Register the components
# component_container$add_components(
  # loader_project, loader_subject, loader_epoch,
  # loader_electrodes, loader_reference, loader_viewer,
  # electrode_selector, import_export_pipeline, baseline_choices,
  # comp_condition_groups, comp_analysis_ranges
# )


