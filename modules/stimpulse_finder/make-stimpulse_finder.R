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
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_epoch_name = targets::tar_target_raw("epoch_name", 
        quote({
            settings[["epoch_name"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- ravecore::new_rave_subject(project_name = project_name, 
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
                  subject <- ravecore::new_rave_subject(project_name = project_name, 
                    subject_code = subject_code)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), clean_loaded_electrodes = targets::tar_target_raw(name = "loaded_electrodes_cleaned", 
        command = quote({
            .__target_expr__. <- quote({
                loaded_electrodes <- ravecore:::parse_svec(loaded_electrodes)
                if (!length(loaded_electrodes) %in% c(1, 2)) {
                  stop("Only two electrode channels are allowed. Please enter one channel, or two channels (for bipolar re-reference)")
                }
                electrode_types <- subject$electrode_types[subject$electrodes %in% 
                  loaded_electrodes]
                electrode_types <- unique(electrode_types)
                if (length(electrode_types) != 1) {
                  stop("Electrode channels must have the same signal type (e.g. both are LFP, or Aux, but not either).")
                }
                loaded_electrodes_cleaned <- sort(loaded_electrodes)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(loaded_electrodes_cleaned)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "loaded_electrodes_cleaned", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "loaded_electrodes_cleaned", target_expr = quote({
                {
                  loaded_electrodes <- ravecore:::parse_svec(loaded_electrodes)
                  if (!length(loaded_electrodes) %in% c(1, 2)) {
                    stop("Only two electrode channels are allowed. Please enter one channel, or two channels (for bipolar re-reference)")
                  }
                  electrode_types <- subject$electrode_types[subject$electrodes %in% 
                    loaded_electrodes]
                  electrode_types <- unique(electrode_types)
                  if (length(electrode_types) != 1) {
                    stop("Electrode channels must have the same signal type (e.g. both are LFP, or Aux, but not either).")
                  }
                  loaded_electrodes_cleaned <- sort(loaded_electrodes)
                }
                loaded_electrodes_cleaned
            }), target_depends = c("loaded_electrodes", "subject"
            )), deps = c("loaded_electrodes", "subject"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), load_repository = targets::tar_target_raw(name = "repository", 
        command = quote({
            .__target_expr__. <- quote({
                repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = subject, 
                  electrodes = loaded_electrodes_cleaned, downsample = NA)
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
                  repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = subject, 
                    electrodes = loaded_electrodes_cleaned, downsample = NA)
                }
                repository
            }), target_depends = c("subject", "loaded_electrodes_cleaned"
            )), deps = c("subject", "loaded_electrodes_cleaned"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), preload_stim_epoch = targets::tar_target_raw(name = "preloaded_stim_epoch", 
        command = quote({
            .__target_expr__. <- quote({
                if (isTRUE(epoch_name %in% subject$epoch_names)) {
                  preloaded_stim_epoch <- subject$get_epoch(epoch_name)
                } else {
                  preloaded_stim_epoch <- NULL
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(preloaded_stim_epoch)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "preloaded_stim_epoch", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "preloaded_stim_epoch", target_expr = quote({
                {
                  if (isTRUE(epoch_name %in% subject$epoch_names)) {
                    preloaded_stim_epoch <- subject$get_epoch(epoch_name)
                  } else {
                    preloaded_stim_epoch <- NULL
                  }
                }
                preloaded_stim_epoch
            }), target_depends = c("epoch_name", "subject")), 
        deps = c("epoch_name", "subject"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), load_signals = targets::tar_target_raw(name = "loaded_signals", 
        command = quote({
            .__target_expr__. <- quote({
                filarray_root <- file.path("data", "raw-voltage")
                if (!dir.exists(filarray_root)) {
                  dir.create(filarray_root, showWarnings = FALSE, 
                    recursive = TRUE)
                }
                container <- repository$get_container()
                loaded_signals <- lapply(subject$blocks, function(block) {
                  fbase <- file.path(filarray_root, block)
                  block_data <- container[[block]]
                  stype <- names(block_data)[[1]]
                  signal_info <- container[[block]][[stype]]
                  electrode_list <- repository$electrode_list
                  signals <- signal_info$data[, signal_info$dimnames$Electrode %in% 
                    electrode_list, drop = FALSE, dimnames = FALSE]
                  if (file.exists(fbase)) {
                    unlink(fbase, recursive = TRUE)
                  }
                  farr <- filearray::filearray_create(filebase = fbase, 
                    dimension = c(nrow(signals), 1L), type = "float", 
                    partition_size = 1L, initialize = FALSE)
                  farr$set_header(key = "subject_id", value = repository$subject$subject_id, 
                    save = FALSE)
                  farr$set_header(key = "block", value = block, 
                    save = FALSE)
                  farr$set_header(key = "sample_rate", value = signal_info$sample_rate, 
                    save = FALSE)
                  farr$set_header(key = "max_duration", value = max(signal_info$dimnames$Time), 
                    save = FALSE)
                  if (!is.null(preloaded_stim_epoch)) {
                    epoch_table <- preloaded_stim_epoch$table
                    if (is.data.frame(epoch_table) && nrow(epoch_table)) {
                      block_epoch_table <- epoch_table[epoch_table$Block == 
                        block, ]
                      if (nrow(block_epoch_table)) {
                        farr$set_header("epoch_table", block_epoch_table, 
                          save = FALSE)
                      }
                    }
                  }
                  if (length(electrode_list) == 1) {
                    farr[] <- signals
                    dimnames(farr) <- list(Time = signal_info$dimnames$Time, 
                      Electrode = electrode_list)
                  } else {
                    farr[] <- signals[, 2] - signals[, 1]
                    dimnames(farr) <- list(Time = signal_info$dimnames$Time, 
                      Electrode = sprintf("%s-%s", electrode_list[[2]], 
                        electrode_list[[1]]))
                  }
                  ravepipeline::RAVEFileArray$new(x = farr, temporary = FALSE)
                })
                names(loaded_signals) <- subject$blocks
            })
            tryCatch({
                eval(.__target_expr__.)
                return(loaded_signals)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "loaded_signals", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "loaded_signals", target_expr = quote({
                {
                  filarray_root <- file.path("data", "raw-voltage")
                  if (!dir.exists(filarray_root)) {
                    dir.create(filarray_root, showWarnings = FALSE, 
                      recursive = TRUE)
                  }
                  container <- repository$get_container()
                  loaded_signals <- lapply(subject$blocks, function(block) {
                    fbase <- file.path(filarray_root, block)
                    block_data <- container[[block]]
                    stype <- names(block_data)[[1]]
                    signal_info <- container[[block]][[stype]]
                    electrode_list <- repository$electrode_list
                    signals <- signal_info$data[, signal_info$dimnames$Electrode %in% 
                      electrode_list, drop = FALSE, dimnames = FALSE]
                    if (file.exists(fbase)) {
                      unlink(fbase, recursive = TRUE)
                    }
                    farr <- filearray::filearray_create(filebase = fbase, 
                      dimension = c(nrow(signals), 1L), type = "float", 
                      partition_size = 1L, initialize = FALSE)
                    farr$set_header(key = "subject_id", value = repository$subject$subject_id, 
                      save = FALSE)
                    farr$set_header(key = "block", value = block, 
                      save = FALSE)
                    farr$set_header(key = "sample_rate", value = signal_info$sample_rate, 
                      save = FALSE)
                    farr$set_header(key = "max_duration", value = max(signal_info$dimnames$Time), 
                      save = FALSE)
                    if (!is.null(preloaded_stim_epoch)) {
                      epoch_table <- preloaded_stim_epoch$table
                      if (is.data.frame(epoch_table) && nrow(epoch_table)) {
                        block_epoch_table <- epoch_table[epoch_table$Block == 
                          block, ]
                        if (nrow(block_epoch_table)) {
                          farr$set_header("epoch_table", block_epoch_table, 
                            save = FALSE)
                        }
                      }
                    }
                    if (length(electrode_list) == 1) {
                      farr[] <- signals
                      dimnames(farr) <- list(Time = signal_info$dimnames$Time, 
                        Electrode = electrode_list)
                    } else {
                      farr[] <- signals[, 2] - signals[, 1]
                      dimnames(farr) <- list(Time = signal_info$dimnames$Time, 
                        Electrode = sprintf("%s-%s", electrode_list[[2]], 
                          electrode_list[[1]]))
                    }
                    ravepipeline::RAVEFileArray$new(x = farr, 
                      temporary = FALSE)
                  })
                  names(loaded_signals) <- subject$blocks
                }
                loaded_signals
            }), target_depends = c("repository", "subject", "preloaded_stim_epoch"
            )), deps = c("repository", "subject", "preloaded_stim_epoch"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"))
