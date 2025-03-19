library(targets)
library(ravepipeline)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
._._env_._.$pipeline <- pipeline_from_path(".")
lapply(sort(list.files(
  "R/", ignore.case = TRUE,
  pattern = "^shared-.*\\.R", 
  full.names = TRUE
)), function(f) {
  source(f, local = ._._env_._., chdir = TRUE)
})
targets::tar_option_set(envir = ._._env_._.)
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_selected_template = targets::tar_target_raw("selected_template", 
        quote({
            settings[["selected_template"]]
        }), deps = "settings"), input_shiny_outputId = targets::tar_target_raw("shiny_outputId", 
        quote({
            settings[["shiny_outputId"]]
        }), deps = "settings"), input_main_camera = targets::tar_target_raw("main_camera", 
        quote({
            settings[["main_camera"]]
        }), deps = "settings"), input_controllers = targets::tar_target_raw("controllers", 
        quote({
            settings[["controllers"]]
        }), deps = "settings"), input_uploaded_source = targets::tar_target_raw("uploaded_source", 
        quote({
            settings[["uploaded_source"]]
        }), deps = "settings"), input_use_template = targets::tar_target_raw("use_template", 
        quote({
            settings[["use_template"]]
        }), deps = "settings"), input_surface_types = targets::tar_target_raw("surface_types", 
        quote({
            settings[["surface_types"]]
        }), deps = "settings"), input_baseline_method = targets::tar_target_raw("baseline_method", 
        quote({
            settings[["baseline_method"]]
        }), deps = "settings"), input_baseline = targets::tar_target_raw("baseline", 
        quote({
            settings[["baseline"]]
        }), deps = "settings"), input_type = targets::tar_target_raw("type", 
        quote({
            settings[["type"]]
        }), deps = "settings"), input_intervals = targets::tar_target_raw("intervals", 
        quote({
            settings[["intervals"]]
        }), deps = "settings"), input_reference = targets::tar_target_raw("reference", 
        quote({
            settings[["reference"]]
        }), deps = "settings"), input_electrodes = targets::tar_target_raw("electrodes", 
        quote({
            settings[["electrodes"]]
        }), deps = "settings"), input_epoch = targets::tar_target_raw("epoch", 
        quote({
            settings[["epoch"]]
        }), deps = "settings"), input_save_pipeline = targets::tar_target_raw("save_pipeline", 
        quote({
            settings[["save_pipeline"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_path_to_electrodes = targets::tar_target_raw("path_to_electrodes", 
        quote({
            settings[["path_to_electrodes"]]
        }), deps = "settings"), input_path_to_connectivity_matrix = targets::tar_target_raw("path_to_connectivity_matrix", 
        quote({
            settings[["path_to_connectivity_matrix"]]
        }), deps = "settings"), get_template_details = targets::tar_target_raw(name = "template_details", 
        command = quote({
            .__target_expr__. <- quote({
                template_details <- list(selected_template = selected_template, 
                  selected_surfaces = surface_types)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(template_details)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "template_details", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "template_details", target_expr = quote({
                {
                  template_details <- list(selected_template = selected_template, 
                    selected_surfaces = surface_types)
                }
                template_details
            }), target_depends = c("selected_template", "surface_types"
            )), deps = c("selected_template", "surface_types"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
