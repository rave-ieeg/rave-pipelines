library(targets)
library(raveio)
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
    input_condition_groupings = targets::tar_target_raw("condition_groupings", 
        quote({
            settings[["condition_groupings"]]
        }), deps = "settings"), input_analysis_settings = targets::tar_target_raw("analysis_settings", 
        quote({
            settings[["analysis_settings"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_epoch_choice = targets::tar_target_raw("epoch_choice", 
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_epoch_choice__trial_starts = targets::tar_target_raw("epoch_choice__trial_starts", 
        quote({
            settings[["epoch_choice__trial_starts"]]
        }), deps = "settings"), input_epoch_choice__trial_ends = targets::tar_target_raw("epoch_choice__trial_ends", 
        quote({
            settings[["epoch_choice__trial_ends"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_baseline_method = targets::tar_target_raw("baseline_method", 
        quote({
            settings[["baseline_method"]]
        }), deps = "settings"), input_baseline = targets::tar_target_raw("baseline", 
        quote({
            settings[["baseline"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
                print(subject)
                subject$epoch_names
                subject$reference_names
                subject$blocks
                subject$electrodes
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "subject", target_expr = quote({
                {
                  subject <- raveio::RAVESubject$new(project_name = project_name, 
                    subject_code = subject_code)
                  print(subject)
                  subject$epoch_names
                  subject$reference_names
                  subject$blocks
                  subject$electrodes
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_data_repository = targets::tar_target_raw(name = "repository", 
        command = quote({
            .__target_expr__. <- quote({
                repository <- raveio::prepare_subject_voltage_with_epoch(subject = subject, 
                  electrodes = loaded_electrodes, epoch_name = epoch_choice, 
                  time_windows = c(epoch_choice__trial_starts, 
                    epoch_choice__trial_ends), reference_name = reference_name)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "repository", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave_prepare_subject_voltage_with_epoch", 
            target_export = "repository", target_expr = quote({
                {
                  repository <- raveio::prepare_subject_voltage_with_epoch(subject = subject, 
                    electrodes = loaded_electrodes, epoch_name = epoch_choice, 
                    time_windows = c(epoch_choice__trial_starts, 
                      epoch_choice__trial_ends), reference_name = reference_name)
                }
                repository
            }), target_depends = c("subject", "loaded_electrodes", 
            "epoch_choice", "epoch_choice__trial_starts", "epoch_choice__trial_ends", 
            "reference_name")), deps = c("subject", "loaded_electrodes", 
        "epoch_choice", "epoch_choice__trial_starts", "epoch_choice__trial_ends", 
        "reference_name"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), calculate_baseline = targets::tar_target_raw(name = "baselined_voltage", 
        command = quote({
            .__target_expr__. <- quote({
                raveio::voltage_baseline(x = repository, baseline_windows = baseline, 
                  method = "subtract_mean")
                baselined_voltage <- repository$voltage$baselined
                baselined_voltage
            })
            tryCatch({
                eval(.__target_expr__.)
                return(baselined_voltage)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "baselined_voltage", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "user-defined-r", 
            target_export = "baselined_voltage", target_expr = quote({
                {
                  raveio::voltage_baseline(x = repository, baseline_windows = baseline, 
                    method = "subtract_mean")
                  baselined_voltage <- repository$voltage$baselined
                  baselined_voltage
                }
                baselined_voltage
            }), target_depends = c("repository", "baseline")), 
        deps = c("repository", "baseline"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), build_analysis_ranges = targets::tar_target_raw(name = "analysis_ranges", 
        command = quote({
            .__target_expr__. <- quote({
                analysis_ranges <- build_analysis_ranges(repository, 
                  analysis_settings)
                analysis_ranges
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_ranges)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "analysis_ranges", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "constrained_vars", 
            target_export = "analysis_ranges", target_expr = quote({
                {
                  analysis_ranges <- build_analysis_ranges(repository, 
                    analysis_settings)
                  analysis_ranges
                }
                analysis_ranges
            }), target_depends = c("repository", "analysis_settings"
            )), deps = c("repository", "analysis_settings"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    build_condition_groups = targets::tar_target_raw(name = "condition_groups", 
        command = quote({
            .__target_expr__. <- quote({
                condition_groups <- build_condition_groups(repository, 
                  condition_groupings)
                condition_groups
            })
            tryCatch({
                eval(.__target_expr__.)
                return(condition_groups)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "condition_groups", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "constrained_vars", 
            target_export = "condition_groups", target_expr = quote({
                {
                  condition_groups <- build_condition_groups(repository, 
                    condition_groupings)
                  condition_groups
                }
                condition_groups
            }), target_depends = c("repository", "condition_groupings"
            )), deps = c("repository", "condition_groupings"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    build_parameter_grid_of_cond_and_analz_ranges = targets::tar_target_raw(name = "parameter_grid", 
        command = quote({
            .__target_expr__. <- quote({
                parameter_grid <- build_parameter_grid(condition_groups, 
                  analysis_ranges)
                parameter_grid
            })
            tryCatch({
                eval(.__target_expr__.)
                return(parameter_grid)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "parameter_grid", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "constrained_vars", 
            target_export = "parameter_grid", target_expr = quote({
                {
                  parameter_grid <- build_parameter_grid(condition_groups, 
                    analysis_ranges)
                  parameter_grid
                }
                parameter_grid
            }), target_depends = c("condition_groups", "analysis_ranges"
            )), deps = c("condition_groups", "analysis_ranges"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"))
