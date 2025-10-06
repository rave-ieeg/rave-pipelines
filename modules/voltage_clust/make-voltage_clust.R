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
    input_zeta_threshold = targets::tar_target_raw("zeta_threshold", 
        quote({
            settings[["zeta_threshold"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
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
        }), deps = "settings"), input_condition_groups = targets::tar_target_raw("condition_groups", 
        quote({
            settings[["condition_groups"]]
        }), deps = "settings"), input_baseline__windows = targets::tar_target_raw("baseline__windows", 
        quote({
            settings[["baseline__windows"]]
        }), deps = "settings"), input_analysis_window = targets::tar_target_raw("analysis_window", 
        quote({
            settings[["analysis_window"]]
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
        pattern = NULL, iteration = "list"), get_LFP_electrodes = targets::tar_target_raw(name = "loaded_LFP_electrodes", 
        command = quote({
            .__target_expr__. <- quote({
                loaded_LFP_electrodes <- ravecore:::parse_svec(loaded_electrodes)
                if (!length(loaded_LFP_electrodes)) {
                  loaded_LFP_electrodes <- subject$electrodes
                }
                loaded_LFP_electrodes <- loaded_LFP_electrodes[subject$electrode_types == 
                  "LFP"]
            })
            tryCatch({
                eval(.__target_expr__.)
                return(loaded_LFP_electrodes)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "loaded_LFP_electrodes", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "loaded_LFP_electrodes", target_expr = quote({
                {
                  loaded_LFP_electrodes <- ravecore:::parse_svec(loaded_electrodes)
                  if (!length(loaded_LFP_electrodes)) {
                    loaded_LFP_electrodes <- subject$electrodes
                  }
                  loaded_LFP_electrodes <- loaded_LFP_electrodes[subject$electrode_types == 
                    "LFP"]
                }
                loaded_LFP_electrodes
            }), target_depends = c("loaded_electrodes", "subject"
            )), deps = c("loaded_electrodes", "subject"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), clean_baseline_window = targets::tar_target_raw(name = "baseline_window_cleaned", 
        command = quote({
            .__target_expr__. <- quote({
                baseline_window_cleaned <- ravecore::validate_time_window(unlist(baseline__windows))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(baseline_window_cleaned)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "baseline_window_cleaned", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "baseline_window_cleaned", target_expr = quote({
                {
                  baseline_window_cleaned <- ravecore::validate_time_window(unlist(baseline__windows))
                }
                baseline_window_cleaned
            }), target_depends = "baseline__windows"), deps = "baseline__windows", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    load_epoch = targets::tar_target_raw(name = "repository", 
        command = quote({
            .__target_expr__. <- quote({
                repository <- ravecore::prepare_subject_voltage_with_epochs(subject = subject, 
                  electrodes = loaded_LFP_electrodes, reference_name = reference_name, 
                  epoch_name = epoch_choice, time_windows = c(epoch_choice__trial_starts, 
                    epoch_choice__trial_ends), stitch_events = c(epoch_choice__trial_starts_rel_to_event, 
                    epoch_choice__trial_ends_rel_to_event))
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
                  repository <- ravecore::prepare_subject_voltage_with_epochs(subject = subject, 
                    electrodes = loaded_LFP_electrodes, reference_name = reference_name, 
                    epoch_name = epoch_choice, time_windows = c(epoch_choice__trial_starts, 
                      epoch_choice__trial_ends), stitch_events = c(epoch_choice__trial_starts_rel_to_event, 
                      epoch_choice__trial_ends_rel_to_event))
                }
                repository
            }), target_depends = c("subject", "loaded_LFP_electrodes", 
            "reference_name", "epoch_choice", "epoch_choice__trial_starts", 
            "epoch_choice__trial_ends", "epoch_choice__trial_starts_rel_to_event", 
            "epoch_choice__trial_ends_rel_to_event")), deps = c("subject", 
        "loaded_LFP_electrodes", "reference_name", "epoch_choice", 
        "epoch_choice__trial_starts", "epoch_choice__trial_ends", 
        "epoch_choice__trial_starts_rel_to_event", "epoch_choice__trial_ends_rel_to_event"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), calculate_baseline = targets::tar_target_raw(name = "baseline_voltage", 
        command = quote({
            .__target_expr__. <- quote({
                baseline_voltage <- voltage_baseline(repository = repository, 
                  baseline_window_cleaned = baseline_window_cleaned)
                baseline_voltage <- ravepipeline::RAVEFileArray$new(baseline_voltage, 
                  temporary = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(baseline_voltage)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "baseline_voltage", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "baseline_voltage", target_expr = quote({
                {
                  baseline_voltage <- voltage_baseline(repository = repository, 
                    baseline_window_cleaned = baseline_window_cleaned)
                  baseline_voltage <- ravepipeline::RAVEFileArray$new(baseline_voltage, 
                    temporary = FALSE)
                }
                baseline_voltage
            }), target_depends = c("repository", "baseline_window_cleaned"
            )), deps = c("repository", "baseline_window_cleaned"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), prepare_clustering_input = targets::tar_target_raw(name = "group_data", 
        command = quote({
            .__target_expr__. <- quote({
                electrode_list <- repository$electrode_list
                sample_rate <- repository$sample_rates$LFP
                analysis_window_clean <- range(unlist(analysis_window))
                default_duration <- max(analysis_window_clean[[2]], 
                  0)
                start_offset <- analysis_window_clean[[1]]
                if (default_duration < 2/sample_rate) {
                  stop("Analysis time duration is too narrow. Please choose a larger time window")
                }
                default_zeta_threshold <- as.numeric(zeta_threshold)
                if (length(default_zeta_threshold) != 1 || is.na(default_zeta_threshold) || 
                  default_zeta_threshold <= 0 || default_zeta_threshold >= 
                  1) {
                  default_zeta_threshold <- 0.5
                }
                group_data <- unname(lapply(seq_along(condition_groups), 
                  function(group_ii) {
                    group <- condition_groups[[group_ii]]
                    label <- trimws(paste(group$group_name, collapse = ""))
                    if (is.na(label) || !nzchar(label)) {
                      label <- sprintf("group%02d", group_ii)
                    }
                    conditions <- group$group_conditions
                    conditions <- conditions[conditions %in% 
                      repository$epoch_table$Condition]
                    if (!length(conditions)) {
                      return(NULL)
                    }
                    initial_rank <- group$initial_rank
                    if (length(initial_rank) != 1 || is.na(initial_rank)) {
                      initial_rank <- min(length(conditions), 
                        length(electrode_list))
                    }
                    zeta_threshold <- group$zeta_threshold %||% 
                      default_zeta_threshold
                    event_start <- group$group_start_event %||% 
                      ""
                    if (tolower(event_start) %in% c("trial onset", 
                      "trial_onset")) {
                      event_start <- ""
                    }
                    event_end <- group$group_finish_event
                    if (length(event_end) == 1 && startsWith(event_end, 
                      "[")) {
                      event_end <- NULL
                    }
                    duration <- group$duration
                    if (length(duration) == 0 || is.na(duration)) {
                      duration <- default_duration
                    }
                    group_fpca_data <- prepare_fpca_data(repository = repository, 
                      baseline_voltage = baseline_voltage, frequency_range = frequency_range, 
                      group_conditions = conditions, event_start = event_start, 
                      event_end = event_end, duration = duration, 
                      start_offset = start_offset)
                    list(group_index = group_ii, label = label, 
                      conditions = conditions, similarity_matrix = group_fpca_data$similarity_matrix, 
                      average_responses = group_fpca_data$average_responses, 
                      event_start = event_start, event_end = event_end, 
                      start_offset = start_offset, duration = duration, 
                      initial_rank = initial_rank, zeta_threshold = zeta_threshold, 
                      electrode_channels = electrode_list, sample_rate = sample_rate)
                  }))
                group_data <- group_data[!vapply(group_data, 
                  is.null, FALSE)]
            })
            tryCatch({
                eval(.__target_expr__.)
                return(group_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "group_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "group_data", target_expr = quote({
                {
                  electrode_list <- repository$electrode_list
                  sample_rate <- repository$sample_rates$LFP
                  analysis_window_clean <- range(unlist(analysis_window))
                  default_duration <- max(analysis_window_clean[[2]], 
                    0)
                  start_offset <- analysis_window_clean[[1]]
                  if (default_duration < 2/sample_rate) {
                    stop("Analysis time duration is too narrow. Please choose a larger time window")
                  }
                  default_zeta_threshold <- as.numeric(zeta_threshold)
                  if (length(default_zeta_threshold) != 1 || 
                    is.na(default_zeta_threshold) || default_zeta_threshold <= 
                    0 || default_zeta_threshold >= 1) {
                    default_zeta_threshold <- 0.5
                  }
                  group_data <- unname(lapply(seq_along(condition_groups), 
                    function(group_ii) {
                      group <- condition_groups[[group_ii]]
                      label <- trimws(paste(group$group_name, 
                        collapse = ""))
                      if (is.na(label) || !nzchar(label)) {
                        label <- sprintf("group%02d", group_ii)
                      }
                      conditions <- group$group_conditions
                      conditions <- conditions[conditions %in% 
                        repository$epoch_table$Condition]
                      if (!length(conditions)) {
                        return(NULL)
                      }
                      initial_rank <- group$initial_rank
                      if (length(initial_rank) != 1 || is.na(initial_rank)) {
                        initial_rank <- min(length(conditions), 
                          length(electrode_list))
                      }
                      zeta_threshold <- group$zeta_threshold %||% 
                        default_zeta_threshold
                      event_start <- group$group_start_event %||% 
                        ""
                      if (tolower(event_start) %in% c("trial onset", 
                        "trial_onset")) {
                        event_start <- ""
                      }
                      event_end <- group$group_finish_event
                      if (length(event_end) == 1 && startsWith(event_end, 
                        "[")) {
                        event_end <- NULL
                      }
                      duration <- group$duration
                      if (length(duration) == 0 || is.na(duration)) {
                        duration <- default_duration
                      }
                      group_fpca_data <- prepare_fpca_data(repository = repository, 
                        baseline_voltage = baseline_voltage, 
                        frequency_range = frequency_range, group_conditions = conditions, 
                        event_start = event_start, event_end = event_end, 
                        duration = duration, start_offset = start_offset)
                      list(group_index = group_ii, label = label, 
                        conditions = conditions, similarity_matrix = group_fpca_data$similarity_matrix, 
                        average_responses = group_fpca_data$average_responses, 
                        event_start = event_start, event_end = event_end, 
                        start_offset = start_offset, duration = duration, 
                        initial_rank = initial_rank, zeta_threshold = zeta_threshold, 
                        electrode_channels = electrode_list, 
                        sample_rate = sample_rate)
                    }))
                  group_data <- group_data[!vapply(group_data, 
                    is.null, FALSE)]
                }
                group_data
            }), target_depends = c("repository", "analysis_window", 
            "zeta_threshold", "condition_groups", "baseline_voltage"
            )), deps = c("repository", "analysis_window", "zeta_threshold", 
        "condition_groups", "baseline_voltage"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_fpca_per_condition = targets::tar_target_raw(name = "combined_group_results", 
        command = quote({
            .__target_expr__. <- quote({
                initial_decomposition <- lapply(group_data, function(group) {
                  res <- prepare_fpca_per_condition(group = group)
                  res$electrode_channels <- group$electrode_channels
                  res$sample_rate <- group$sample_rate
                  res$event_start <- group$event_start
                  res$start_offset <- group$start_offset
                  res
                })
                combined_group_results <- combine_condition_groups(.list = initial_decomposition)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(combined_group_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "combined_group_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "combined_group_results", target_expr = quote({
                {
                  initial_decomposition <- lapply(group_data, 
                    function(group) {
                      res <- prepare_fpca_per_condition(group = group)
                      res$electrode_channels <- group$electrode_channels
                      res$sample_rate <- group$sample_rate
                      res$event_start <- group$event_start
                      res$start_offset <- group$start_offset
                      res
                    })
                  combined_group_results <- combine_condition_groups(.list = initial_decomposition)
                }
                combined_group_results
            }), target_depends = "group_data"), deps = "group_data", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    final_clustering_tree = targets::tar_target_raw(name = "clustering_tree", 
        command = quote({
            .__target_expr__. <- quote({
                clustering_tree <- calc_final_cluster_tree(combined_group_results, 
                  distance_method = "euclidean", cluster_method = "ward.D2")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(clustering_tree)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "clustering_tree", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "clustering_tree", target_expr = quote({
                {
                  clustering_tree <- calc_final_cluster_tree(combined_group_results, 
                    distance_method = "euclidean", cluster_method = "ward.D2")
                }
                clustering_tree
            }), target_depends = "combined_group_results"), deps = "combined_group_results", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    calculate_clustering_index = targets::tar_target_raw(name = "clustering_index", 
        command = quote({
            .__target_expr__. <- quote({
                max_n_clusters <- max(clustering_tree$cluster_range)
                clustering_index <- choose_n_clusters(clustering_tree, 
                  cluster_range = c(2, max_n_clusters), plot = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(clustering_index)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "clustering_index", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "clustering_index", target_expr = quote({
                {
                  max_n_clusters <- max(clustering_tree$cluster_range)
                  clustering_index <- choose_n_clusters(clustering_tree, 
                    cluster_range = c(2, max_n_clusters), plot = FALSE)
                }
                clustering_index
            }), target_depends = "clustering_tree"), deps = "clustering_tree", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"))
