#' This file runs right after `aa.R`. The goal of this script
#' is to generate a bunch of preset input components
#' and create a component container that collate all the components
NULL

# Create a component container
component_container <- ravedash::new_rave_shiny_component_container(
  module_id = module_id, pipeline_name = pipeline$pipeline_name,
  settings_file = "settings.yaml"
)


# Define components
loader_project <- ravedash::presets_loader_project()
loader_subject <- ravedash::presets_loader_subject(checks = NULL)
loader_electrodes <- with(asNamespace("ravedash"), {
  function (id = "loader_electrode_text", varname = "loaded_electrodes",
            label = "Stim Electrode", loader_project_id = "loader_project_name",
            loader_subject_id = "loader_subject_code")
  {
    comp <- RAVEShinyComponent$new(id = id, varname = varname)
    comp$depends <- c(loader_project_id, loader_subject_id)
    comp$ui_func <- function(id, value, depends) {
      shiny::textInput(inputId = id, label = label, placeholder = "E.g. 87",
                       value = value)
    }
    comp$server_func <- function(input, output, session) {
      loader_project <- comp$get_dependent_component(loader_project_id)
      loader_subject <- comp$get_dependent_component(loader_subject_id)
      get_subject <- loader_subject$get_tool("get_subject")
      shiny::bindEvent(observe({
        if (!loader_subject$sv$is_valid()) {
          return()
        }
        subject <- get_subject()
        if (is.null(subject)) {
          return()
        }
        electrode_text <- dipsaus::deparse_svec(subject$electrodes)
        shiny::updateTextInput(
          session = session, inputId = id,
          placeholder = electrode_text
        )
      }), loader_project$current_value, loader_subject$current_value,
      ignoreNULL = TRUE)
    }
    comp
  }
})()
loader_viewer <- ravedash::presets_loader_3dviewer(height = "100%")


# Register the components
component_container$add_components(
  loader_project, loader_subject,
  # loader_epoch,
  loader_electrodes, loader_viewer
)


