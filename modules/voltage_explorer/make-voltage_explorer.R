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
        }), deps = "settings"), input_plot_range = targets::tar_target_raw("plot_range", 
        quote({
            settings[["plot_range"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_filter_configurations = targets::tar_target_raw("filter_configurations", 
        quote({
            settings[["filter_configurations"]]
        }), deps = "settings"), input_epoch_choice__trial_starts_rel_to_event = targets::tar_target_raw("epoch_choice__trial_starts_rel_to_event", 
        quote({
            settings[["epoch_choice__trial_starts_rel_to_event"]]
        }), deps = "settings"), input_epoch_choice__trial_starts = targets::tar_target_raw("epoch_choice__trial_starts", 
        quote({
            settings[["epoch_choice__trial_starts"]]
        }), deps = "settings"), input_epoch_choice__trial_ends_rel_to_event = targets::tar_target_raw("epoch_choice__trial_ends_rel_to_event", 
        quote({
            settings[["epoch_choice__trial_ends_rel_to_event"]]
        }), deps = "settings"), input_epoch_choice__trial_ends = targets::tar_target_raw("epoch_choice__trial_ends", 
        quote({
            settings[["epoch_choice__trial_ends"]]
        }), deps = "settings"), input_epoch_choice = targets::tar_target_raw("epoch_choice", 
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_condition_groupings = targets::tar_target_raw("condition_groupings", 
        quote({
            settings[["condition_groupings"]]
        }), deps = "settings"), input_analysis_settings = targets::tar_target_raw("analysis_settings", 
        quote({
            settings[["analysis_settings"]]
        }), deps = "settings"), input_analysis_channels = targets::tar_target_raw("analysis_channels", 
        quote({
            settings[["analysis_channels"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- ravecore::new_rave_subject(project_name = project_name, 
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
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "subject", target_expr = quote({
                {
                  subject <- ravecore::new_rave_subject(project_name = project_name, 
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
                repository <- ravecore::prepare_subject_voltage_with_epochs(subject = subject, 
                  electrodes = loaded_electrodes, epoch_name = epoch_choice, 
                  time_windows = c(epoch_choice__trial_starts, 
                    epoch_choice__trial_ends), reference_name = reference_name)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "repository", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "rave_prepare_subject_voltage_with_epoch", 
            target_export = "repository", target_expr = quote({
                {
                  repository <- ravecore::prepare_subject_voltage_with_epochs(subject = subject, 
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
        pattern = NULL, iteration = "list"), clean_analysis_channels = targets::tar_target_raw(name = "analysis_channels_clean", 
        command = quote({
            .__target_expr__. <- quote({
                available_channels <- repository$electrode_list
                analysis_channels_clean <- dipsaus::parse_svec(unlist(analysis_channels))
                analysis_channels_clean <- analysis_channels_clean[analysis_channels_clean %in% 
                  available_channels]
                if (!length(analysis_channels_clean)) {
                  stop("No channels selected for analysis. Please specify the electrode channels from the following loaded: ", 
                    dipsaus::deparse_svec(available_channels))
                }
                analysis_channels_clean <- as.integer(sort(analysis_channels_clean))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_channels_clean)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "analysis_channels_clean", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "analysis_channels_clean", target_expr = quote({
                {
                  available_channels <- repository$electrode_list
                  analysis_channels_clean <- dipsaus::parse_svec(unlist(analysis_channels))
                  analysis_channels_clean <- analysis_channels_clean[analysis_channels_clean %in% 
                    available_channels]
                  if (!length(analysis_channels_clean)) {
                    stop("No channels selected for analysis. Please specify the electrode channels from the following loaded: ", 
                      dipsaus::deparse_svec(available_channels))
                  }
                  analysis_channels_clean <- as.integer(sort(analysis_channels_clean))
                }
                analysis_channels_clean
            }), target_depends = c("repository", "analysis_channels"
            )), deps = c("repository", "analysis_channels"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    prepare_pre_analysis_filters = targets::tar_target_raw(name = "filtered_array", 
        command = quote({
            .__target_expr__. <- quote({
                filtered_array <- prepare_filtered_data(data_path = "data/filtered_voltage", 
                  repository = repository, filter_configurations = filter_configurations)
                filtered_array <- ravepipeline::RAVEFileArray$new(filtered_array)
                ravepipeline::with_rave_parallel({
                  ravepipeline::lapply_jobs(analysis_channels_clean, 
                    function(ch) {
                      sel <- repository$electrode_list %in% ch
                      source_array <- repository$voltage$data_list[[which(sel)]]
                      signals <- source_array[reshape = dim(source_array)[c(1, 
                        2)]]
                      filtered_array_impl <- filtered_array$`@impl`
                      filter_configs <- filtered_array_impl$get_header("filter_configurations")
                      filtered <- apply_filters_to_signals(signals = signals, 
                        filter_configs = filter_configs)
                      filtered_array_impl$.mode <- "readwrite"
                      filtered_array_impl[, , sel] <- filtered
                      return(ch)
                    }, .globals = list(repository = repository, 
                      filtered_array = filtered_array, apply_filters_to_signals = apply_filters_to_signals, 
                      apply_filter = apply_filter, ALLOWED_FILTER_TYPES = ALLOWED_FILTER_TYPES, 
                      `%?<-%` = `%?<-%`), callback = function(ch) {
                      sprintf("Filtering channel|%d", ch)
                    })
                })
                filtered_array$`@impl`
            })
            tryCatch({
                eval(.__target_expr__.)
                return(filtered_array)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "filtered_array", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "filtered_array", target_expr = quote({
                {
                  filtered_array <- prepare_filtered_data(data_path = "data/filtered_voltage", 
                    repository = repository, filter_configurations = filter_configurations)
                  filtered_array <- ravepipeline::RAVEFileArray$new(filtered_array)
                  ravepipeline::with_rave_parallel({
                    ravepipeline::lapply_jobs(analysis_channels_clean, 
                      function(ch) {
                        sel <- repository$electrode_list %in% 
                          ch
                        source_array <- repository$voltage$data_list[[which(sel)]]
                        signals <- source_array[reshape = dim(source_array)[c(1, 
                          2)]]
                        filtered_array_impl <- filtered_array$`@impl`
                        filter_configs <- filtered_array_impl$get_header("filter_configurations")
                        filtered <- apply_filters_to_signals(signals = signals, 
                          filter_configs = filter_configs)
                        filtered_array_impl$.mode <- "readwrite"
                        filtered_array_impl[, , sel] <- filtered
                        return(ch)
                      }, .globals = list(repository = repository, 
                        filtered_array = filtered_array, apply_filters_to_signals = apply_filters_to_signals, 
                        apply_filter = apply_filter, ALLOWED_FILTER_TYPES = ALLOWED_FILTER_TYPES, 
                        `%?<-%` = `%?<-%`), callback = function(ch) {
                        sprintf("Filtering channel|%d", ch)
                      })
                  })
                  filtered_array$`@impl`
                }
                filtered_array
            }), target_depends = c("repository", "filter_configurations", 
            "analysis_channels_clean")), deps = c("repository", 
        "filter_configurations", "analysis_channels_clean"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    get_electrode_coordinate_table = targets::tar_target_raw(name = "analysis_electrode_coordinates", 
        command = quote({
            .__target_expr__. <- quote({
                analysis_electrode_coordinates <- subject$get_electrode_table(electrodes = analysis_channels_clean, 
                  reference_name = repository$reference_name, 
                  subset = TRUE)
                labels <- analysis_electrode_coordinates$Label
                if (!length(analysis_electrode_coordinates$LabelPrefix)) {
                  analysis_electrode_coordinates$LabelPrefix <- gsub("[0-9]+", 
                    "", labels)
                }
                label_prefix <- analysis_electrode_coordinates$LabelPrefix
                label_prefix_lag1 <- c("", label_prefix[-length(label_prefix)])
                is_lead_channel <- label_prefix != label_prefix_lag1
                analysis_electrode_coordinates$ShortLabel <- ifelse(!is_lead_channel, 
                  gsub("^[a-zA-Z_-]+", "", labels), labels)
                analysis_electrode_coordinates$LeadChannel <- is_lead_channel
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_electrode_coordinates)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "analysis_electrode_coordinates", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "analysis_electrode_coordinates", 
            target_expr = quote({
                {
                  analysis_electrode_coordinates <- subject$get_electrode_table(electrodes = analysis_channels_clean, 
                    reference_name = repository$reference_name, 
                    subset = TRUE)
                  labels <- analysis_electrode_coordinates$Label
                  if (!length(analysis_electrode_coordinates$LabelPrefix)) {
                    analysis_electrode_coordinates$LabelPrefix <- gsub("[0-9]+", 
                      "", labels)
                  }
                  label_prefix <- analysis_electrode_coordinates$LabelPrefix
                  label_prefix_lag1 <- c("", label_prefix[-length(label_prefix)])
                  is_lead_channel <- label_prefix != label_prefix_lag1
                  analysis_electrode_coordinates$ShortLabel <- ifelse(!is_lead_channel, 
                    gsub("^[a-zA-Z_-]+", "", labels), labels)
                  analysis_electrode_coordinates$LeadChannel <- is_lead_channel
                }
                analysis_electrode_coordinates
            }), target_depends = c("subject", "analysis_channels_clean", 
            "repository")), deps = c("subject", "analysis_channels_clean", 
        "repository"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), clean_condition_groupings = targets::tar_target_raw(name = "condition_groupings_clean", 
        command = quote({
            .__target_expr__. <- quote({
                condition_groupings_clean <- ravecore::validate_condition_groupings(condition_groupings = condition_groupings, 
                  epoch = repository$epoch)
                if (!isTRUE(condition_groupings_clean$n > 0)) {
                  stop("No valid condition group specified. Please check the condition groupings input.")
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(condition_groupings_clean)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "condition_groupings_clean", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "condition_groupings_clean", target_expr = quote({
                {
                  condition_groupings_clean <- ravecore::validate_condition_groupings(condition_groupings = condition_groupings, 
                    epoch = repository$epoch)
                  if (!isTRUE(condition_groupings_clean$n > 0)) {
                    stop("No valid condition group specified. Please check the condition groupings input.")
                  }
                }
                condition_groupings_clean
            }), target_depends = c("condition_groupings", "repository"
            )), deps = c("condition_groupings", "repository"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    prepare_plot_data_placeholder = targets::tar_target_raw(name = "data_placeholder", 
        command = quote({
            .__target_expr__. <- quote({
                data_placeholder <- condition_groupings_clean
                if (inherits(filtered_array, "RAVEFileArray")) {
                  filtered_array_impl <- filtered_array$`@impl`
                } else {
                  filtered_array_impl <- filtered_array
                }
                dnames <- dimnames(filtered_array_impl)
                data_placeholder$sample_rate <- filtered_array_impl$get_header("sample_rate")
                data_placeholder$coord_table <- analysis_electrode_coordinates
                data_placeholder$time_points <- dnames$Time
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_placeholder)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "data_placeholder", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "data_placeholder", target_expr = quote({
                {
                  data_placeholder <- condition_groupings_clean
                  if (inherits(filtered_array, "RAVEFileArray")) {
                    filtered_array_impl <- filtered_array$`@impl`
                  } else {
                    filtered_array_impl <- filtered_array
                  }
                  dnames <- dimnames(filtered_array_impl)
                  data_placeholder$sample_rate <- filtered_array_impl$get_header("sample_rate")
                  data_placeholder$coord_table <- analysis_electrode_coordinates
                  data_placeholder$time_points <- dnames$Time
                }
                data_placeholder
            }), target_depends = c("condition_groupings_clean", 
            "filtered_array", "analysis_electrode_coordinates"
            )), deps = c("condition_groupings_clean", "filtered_array", 
        "analysis_electrode_coordinates"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_voltage_over_channel_and_condition_by_collapsing_trials = targets::tar_target_raw(name = "data_by_channel_condition", 
        command = quote({
            .__target_expr__. <- quote({
                if (inherits(filtered_array, "RAVEFileArray")) {
                  filtered_array_impl <- filtered_array$`@impl`
                } else {
                  filtered_array_impl <- filtered_array
                }
                group_data <- lapply(data_placeholder$groups, 
                  function(group) {
                    sub_array <- subset(filtered_array_impl, 
                      Electrode ~ Electrode %in% analysis_channels_clean, 
                      Trial ~ Trial %in% group$trials_included, 
                      drop = FALSE)
                    dimnames(sub_array) <- NULL
                    ravetools::collapse(x = sub_array, keep = c(1L, 
                      3L), average = TRUE)
                  })
                group_data <- simplify2array(group_data)
                data_by_channel_condition <- data_placeholder
                data_by_channel_condition$data <- group_data
                class(data_by_channel_condition) <- "data_by_channel_condition"
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_by_channel_condition)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "data_by_channel_condition", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "data_by_channel_condition", target_expr = quote({
                {
                  if (inherits(filtered_array, "RAVEFileArray")) {
                    filtered_array_impl <- filtered_array$`@impl`
                  } else {
                    filtered_array_impl <- filtered_array
                  }
                  group_data <- lapply(data_placeholder$groups, 
                    function(group) {
                      sub_array <- subset(filtered_array_impl, 
                        Electrode ~ Electrode %in% analysis_channels_clean, 
                        Trial ~ Trial %in% group$trials_included, 
                        drop = FALSE)
                      dimnames(sub_array) <- NULL
                      ravetools::collapse(x = sub_array, keep = c(1L, 
                        3L), average = TRUE)
                    })
                  group_data <- simplify2array(group_data)
                  data_by_channel_condition <- data_placeholder
                  data_by_channel_condition$data <- group_data
                  class(data_by_channel_condition) <- "data_by_channel_condition"
                }
                data_by_channel_condition
            }), target_depends = c("filtered_array", "data_placeholder", 
            "analysis_channels_clean")), deps = c("filtered_array", 
        "data_placeholder", "analysis_channels_clean"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_voltage_collapsed_over_channels_per_condition = targets::tar_target_raw(name = "data_by_trial_per_condition", 
        command = quote({
            .__target_expr__. <- quote({
                if (inherits(filtered_array, "RAVEFileArray")) {
                  filtered_array_impl <- filtered_array$`@impl`
                } else {
                  filtered_array_impl <- filtered_array
                }
                dnames <- dimnames(filtered_array_impl)
                trial_numbers <- dnames$Trial
                group_data <- lapply(data_placeholder$groups, 
                  function(group) {
                    sub_array <- subset(filtered_array_impl, 
                      Electrode ~ Electrode %in% analysis_channels_clean, 
                      Trial ~ match(group$trials_included, trial_numbers), 
                      drop = FALSE)
                    dimnames(sub_array) <- NULL
                    voltage_by_trial <- ravetools::collapse(x = sub_array, 
                      keep = c(1L, 2L), average = TRUE)
                    return(voltage_by_trial)
                  })
                data_by_trial_per_condition <- data_placeholder
                data_by_trial_per_condition$data <- group_data
                class(data_by_trial_per_condition) <- "data_by_trial_per_condition"
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_by_trial_per_condition)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "data_by_trial_per_condition", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "data_by_trial_per_condition", target_expr = quote({
                {
                  if (inherits(filtered_array, "RAVEFileArray")) {
                    filtered_array_impl <- filtered_array$`@impl`
                  } else {
                    filtered_array_impl <- filtered_array
                  }
                  dnames <- dimnames(filtered_array_impl)
                  trial_numbers <- dnames$Trial
                  group_data <- lapply(data_placeholder$groups, 
                    function(group) {
                      sub_array <- subset(filtered_array_impl, 
                        Electrode ~ Electrode %in% analysis_channels_clean, 
                        Trial ~ match(group$trials_included, 
                          trial_numbers), drop = FALSE)
                      dimnames(sub_array) <- NULL
                      voltage_by_trial <- ravetools::collapse(x = sub_array, 
                        keep = c(1L, 2L), average = TRUE)
                      return(voltage_by_trial)
                    })
                  data_by_trial_per_condition <- data_placeholder
                  data_by_trial_per_condition$data <- group_data
                  class(data_by_trial_per_condition) <- "data_by_trial_per_condition"
                }
                data_by_trial_per_condition
            }), target_depends = c("filtered_array", "data_placeholder", 
            "analysis_channels_clean")), deps = c("filtered_array", 
        "data_placeholder", "analysis_channels_clean"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_collapsed_voltage_per_condition = targets::tar_target_raw(name = "data_collapse_by_condition", 
        command = quote({
            .__target_expr__. <- quote({
                data_collapse_by_condition <- data_by_trial_per_condition
                time_points <- data_collapse_by_condition$time_points
                sample_rate <- data_collapse_by_condition$sample_rate
                time_range <- range(time_points)
                crp_time_begin <- 0.01
                if (crp_time_begin > time_range[[2]]) {
                  crp_time_begin <- 0
                }
                crp_time_end <- time_range[[2]]
                crp_time_step <- max(floor(sample_rate/50), 5)
                if ((crp_time_end - crp_time_begin) * sample_rate <= 
                  10) {
                  crp_enabled <- FALSE
                } else {
                  crp_enabled <- TRUE
                }
                group_data <- lapply(seq_along(data_collapse_by_condition$groups), 
                  function(ii) {
                    group <- data_collapse_by_condition$groups[[ii]]
                    voltage_by_trial <- data_collapse_by_condition$data[[ii]]
                    if (crp_enabled) {
                      crp_result <- ravetools::crp(voltage_by_trial, 
                        time = time_points, t_start = crp_time_begin, 
                        t_end = crp_time_end, time_step = crp_time_step, 
                        threshold_quantile = 0.98, artifact_interval = "tR", 
                        remove_artifacts = FALSE)
                    } else {
                      crp_result <- NULL
                    }
                    if (length(crp_result$bad_trials)) {
                      voltage_by_trial <- voltage_by_trial[, 
                        -crp_result$bad_trials, drop = FALSE]
                    }
                    n_samples <- ncol(voltage_by_trial)
                    mean_erp <- rowMeans(voltage_by_trial)
                    return(list(mean = mean_erp, bad_trials = group$trials_included[crp_result$bad_trials], 
                      tau_r = c(crp_result$tau_R_lower, crp_result$tau_R, 
                        crp_result$tau_R_upper)))
                  })
                mean_erp <- simplify2array(lapply(group_data, 
                  "[[", "mean"))
                bad_trials <- lapply(group_data, "[[", "bad_trials")
                crp_tau <- lapply(group_data, "[[", "tau_r")
                data_collapse_by_condition$data <- list(mean_erp = mean_erp, 
                  bad_trials = bad_trials, crp_enabled = crp_enabled, 
                  crp_time_range = c(crp_time_begin, crp_time_end), 
                  crp_tau = crp_tau)
                class(data_collapse_by_condition) <- "data_collapse_by_condition"
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_collapse_by_condition)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "data_collapse_by_condition", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "data_collapse_by_condition", target_expr = quote({
                {
                  data_collapse_by_condition <- data_by_trial_per_condition
                  time_points <- data_collapse_by_condition$time_points
                  sample_rate <- data_collapse_by_condition$sample_rate
                  time_range <- range(time_points)
                  crp_time_begin <- 0.01
                  if (crp_time_begin > time_range[[2]]) {
                    crp_time_begin <- 0
                  }
                  crp_time_end <- time_range[[2]]
                  crp_time_step <- max(floor(sample_rate/50), 
                    5)
                  if ((crp_time_end - crp_time_begin) * sample_rate <= 
                    10) {
                    crp_enabled <- FALSE
                  } else {
                    crp_enabled <- TRUE
                  }
                  group_data <- lapply(seq_along(data_collapse_by_condition$groups), 
                    function(ii) {
                      group <- data_collapse_by_condition$groups[[ii]]
                      voltage_by_trial <- data_collapse_by_condition$data[[ii]]
                      if (crp_enabled) {
                        crp_result <- ravetools::crp(voltage_by_trial, 
                          time = time_points, t_start = crp_time_begin, 
                          t_end = crp_time_end, time_step = crp_time_step, 
                          threshold_quantile = 0.98, artifact_interval = "tR", 
                          remove_artifacts = FALSE)
                      } else {
                        crp_result <- NULL
                      }
                      if (length(crp_result$bad_trials)) {
                        voltage_by_trial <- voltage_by_trial[, 
                          -crp_result$bad_trials, drop = FALSE]
                      }
                      n_samples <- ncol(voltage_by_trial)
                      mean_erp <- rowMeans(voltage_by_trial)
                      return(list(mean = mean_erp, bad_trials = group$trials_included[crp_result$bad_trials], 
                        tau_r = c(crp_result$tau_R_lower, crp_result$tau_R, 
                          crp_result$tau_R_upper)))
                    })
                  mean_erp <- simplify2array(lapply(group_data, 
                    "[[", "mean"))
                  bad_trials <- lapply(group_data, "[[", "bad_trials")
                  crp_tau <- lapply(group_data, "[[", "tau_r")
                  data_collapse_by_condition$data <- list(mean_erp = mean_erp, 
                    bad_trials = bad_trials, crp_enabled = crp_enabled, 
                    crp_time_range = c(crp_time_begin, crp_time_end), 
                    crp_tau = crp_tau)
                  class(data_collapse_by_condition) <- "data_collapse_by_condition"
                }
                data_collapse_by_condition
            }), target_depends = "data_by_trial_per_condition"), 
        deps = "data_by_trial_per_condition", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
