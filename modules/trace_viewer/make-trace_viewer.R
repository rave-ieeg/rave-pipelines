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
    input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_pre_downsample = targets::tar_target_raw("pre_downsample", 
        quote({
            settings[["pre_downsample"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_epoch_name = targets::tar_target_raw("epoch_name", 
        quote({
            settings[["epoch_name"]]
        }), deps = "settings"), input_epoch_events = targets::tar_target_raw("epoch_events", 
        quote({
            settings[["epoch_events"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- ravecore::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "subject", target_expr = quote({
                {
                  subject <- ravecore::RAVESubject$new(project_name = project_name, 
                    subject_code = subject_code)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), load_repository = targets::tar_target_raw(name = "repository", 
        command = quote({
            .__target_expr__. <- quote({
                repository <- ravecore::prepare_subject_voltage_with_blocks(subject = subject, 
                  electrodes = loaded_electrodes, blocks = subject$blocks, 
                  reference_name = reference_name, strict = TRUE, 
                  downsample = pre_downsample, lazy_load = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "repository", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "repository", target_expr = quote({
                {
                  repository <- ravecore::prepare_subject_voltage_with_blocks(subject = subject, 
                    electrodes = loaded_electrodes, blocks = subject$blocks, 
                    reference_name = reference_name, strict = TRUE, 
                    downsample = pre_downsample, lazy_load = FALSE)
                }
                repository
            }), target_depends = c("subject", "loaded_electrodes", 
            "reference_name", "pre_downsample")), deps = c("subject", 
        "loaded_electrodes", "reference_name", "pre_downsample"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), generate_annotation_table = targets::tar_target_raw(name = "annotation_table", 
        command = quote({
            .__target_expr__. <- quote({
                if (is.null(epoch_name) || is.na(epoch_name)) {
                  annotation_table <- NULL
                } else {
                  epoch <- ravecore::RAVEEpoch$new(subject = subject, 
                    name = epoch_name)
                  epoch_events <- unique(c("", epoch_events))
                  epoch_events <- epoch_events[epoch_events %in% 
                    epoch$available_events]
                  annotation_table <- data.table::rbindlist(lapply(seq_along(epoch_events), 
                    function(ii) {
                      event_name <- epoch_events[[ii]]
                      event_cname <- epoch$get_event_colname(event = event_name, 
                        missing = "warning")
                      if (event_name == "") {
                        event_name <- "Onset"
                      }
                      annotation_table <- data.frame(block = epoch$table$Block, 
                        time = epoch$table[[event_cname]], label = sprintf("%s[%s,t=%.1f]<br>%s", 
                          event_name, epoch$table$Trial, epoch$table[[event_cname]], 
                          epoch$table$Condition), group = event_name, 
                        color = ii + 1)
                    }))
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(annotation_table)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "annotation_table", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "annotation_table", target_expr = quote({
                {
                  if (is.null(epoch_name) || is.na(epoch_name)) {
                    annotation_table <- NULL
                  } else {
                    epoch <- ravecore::RAVEEpoch$new(subject = subject, 
                      name = epoch_name)
                    epoch_events <- unique(c("", epoch_events))
                    epoch_events <- epoch_events[epoch_events %in% 
                      epoch$available_events]
                    annotation_table <- data.table::rbindlist(lapply(seq_along(epoch_events), 
                      function(ii) {
                        event_name <- epoch_events[[ii]]
                        event_cname <- epoch$get_event_colname(event = event_name, 
                          missing = "warning")
                        if (event_name == "") {
                          event_name <- "Onset"
                        }
                        annotation_table <- data.frame(block = epoch$table$Block, 
                          time = epoch$table[[event_cname]], 
                          label = sprintf("%s[%s,t=%.1f]<br>%s", 
                            event_name, epoch$table$Trial, epoch$table[[event_cname]], 
                            epoch$table$Condition), group = event_name, 
                          color = ii + 1)
                      }))
                  }
                }
                annotation_table
            }), target_depends = c("epoch_name", "subject", "epoch_events"
            )), deps = c("epoch_name", "subject", "epoch_events"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"))
