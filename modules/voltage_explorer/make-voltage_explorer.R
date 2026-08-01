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
        }), deps = "settings"), input_crp_time_step = targets::tar_target_raw("crp_time_step", 
        quote({
            settings[["crp_time_step"]]
        }), deps = "settings"), input_crp_threshold_quantile = targets::tar_target_raw("crp_threshold_quantile", 
        quote({
            settings[["crp_threshold_quantile"]]
        }), deps = "settings"), input_crp_onset_border = targets::tar_target_raw("crp_onset_border", 
        quote({
            settings[["crp_onset_border"]]
        }), deps = "settings"), input_crp_detection_window = targets::tar_target_raw("crp_detection_window", 
        quote({
            settings[["crp_detection_window"]]
        }), deps = "settings"), input_condition_groups = targets::tar_target_raw("condition_groups", 
        quote({
            settings[["condition_groups"]]
        }), deps = "settings"), input_analysis_ranges = targets::tar_target_raw("analysis_ranges", 
        quote({
            settings[["analysis_ranges"]]
        }), deps = "settings"), input_analysis_event = targets::tar_target_raw("analysis_event", 
        quote({
            settings[["analysis_event"]]
        }), deps = "settings"), input_analysis_electrodes = targets::tar_target_raw("analysis_electrodes", 
        quote({
            settings[["analysis_electrodes"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- ravecore::new_rave_subject(project_name = project_name, 
                  subject_code = subject_code)
                print(subject)
                subject$epoch_names
                subject$reference_names
                subject$blocks
                dipsaus::deparse_svec(subject$electrodes)
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
                  dipsaus::deparse_svec(subject$electrodes)
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
                    epoch_choice__trial_ends), reference_name = reference_name, 
                  stitch_events = c(epoch_choice__trial_starts_rel_to_event, 
                    epoch_choice__trial_ends_rel_to_event), quiet = TRUE)
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
                    electrodes = loaded_electrodes, epoch_name = epoch_choice, 
                    time_windows = c(epoch_choice__trial_starts, 
                      epoch_choice__trial_ends), reference_name = reference_name, 
                    stitch_events = c(epoch_choice__trial_starts_rel_to_event, 
                      epoch_choice__trial_ends_rel_to_event), 
                    quiet = TRUE)
                }
                repository
            }), target_depends = c("subject", "loaded_electrodes", 
            "epoch_choice", "epoch_choice__trial_starts", "epoch_choice__trial_ends", 
            "reference_name", "epoch_choice__trial_starts_rel_to_event", 
            "epoch_choice__trial_ends_rel_to_event")), deps = c("subject", 
        "loaded_electrodes", "epoch_choice", "epoch_choice__trial_starts", 
        "epoch_choice__trial_ends", "reference_name", "epoch_choice__trial_starts_rel_to_event", 
        "epoch_choice__trial_ends_rel_to_event"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_coordinate_table = targets::tar_target_raw(name = "electrode_coordinates", 
        command = quote({
            .__target_expr__. <- quote({
                electrode_coordinates <- load_electrode_coordinates_cleaned(repository)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(electrode_coordinates)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "electrode_coordinates", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "electrode_coordinates", target_expr = quote({
                {
                  electrode_coordinates <- load_electrode_coordinates_cleaned(repository)
                }
                electrode_coordinates
            }), target_depends = "repository"), deps = "repository", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    get_loaded_electrodes = targets::tar_target_raw(name = "loaded_electrodes_clean", 
        command = quote({
            .__target_expr__. <- quote({
                loaded_electrodes_clean <- filter_electrodes(repository)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(loaded_electrodes_clean)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "loaded_electrodes_clean", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "loaded_electrodes_clean", target_expr = quote({
                {
                  loaded_electrodes_clean <- filter_electrodes(repository)
                }
                loaded_electrodes_clean
            }), target_depends = "repository"), deps = "repository", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    get_loaded_electrode_coordinate_table = targets::tar_target_raw(name = "loaded_electrode_coordinates", 
        command = quote({
            .__target_expr__. <- quote({
                loaded_electrode_coordinates <- load_electrode_coordinates_cleaned(repository = repository, 
                  electrodes = loaded_electrodes_clean)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(loaded_electrode_coordinates)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "loaded_electrode_coordinates", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "loaded_electrode_coordinates", target_expr = quote({
                {
                  loaded_electrode_coordinates <- load_electrode_coordinates_cleaned(repository = repository, 
                    electrodes = loaded_electrodes_clean)
                }
                loaded_electrode_coordinates
            }), target_depends = c("repository", "loaded_electrodes_clean"
            )), deps = c("repository", "loaded_electrodes_clean"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), clean_analysis_event = targets::tar_target_raw(name = "analysis_event_colname", 
        command = quote({
            .__target_expr__. <- quote({
                analysis_event_colname <- repository$epoch$get_event_colname(event = analysis_event, 
                  missing = "warning")
                analysis_event_colname
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_event_colname)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "analysis_event_colname", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "analysis_event_colname", target_expr = quote({
                {
                  analysis_event_colname <- repository$epoch$get_event_colname(event = analysis_event, 
                    missing = "warning")
                  analysis_event_colname
                }
                analysis_event_colname
            }), target_depends = c("repository", "analysis_event"
            )), deps = c("repository", "analysis_event"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), diagnose_filters = targets::tar_target_raw(name = "filter_freqz", 
        command = quote({
            .__target_expr__. <- quote({
                filter_freqz <- get_filter_freqz(repository = repository, 
                  filter_configurations = filter_configurations)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(filter_freqz)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "filter_freqz", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "filter_freqz", target_expr = quote({
                {
                  filter_freqz <- get_filter_freqz(repository = repository, 
                    filter_configurations = filter_configurations)
                }
                filter_freqz
            }), target_depends = c("repository", "filter_configurations"
            )), deps = c("repository", "filter_configurations"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), prepare_pre_analysis_filters = targets::tar_target_raw(name = "filtered_array", 
        command = quote({
            .__target_expr__. <- quote({
                filtered_array <- filter_repository(repository, 
                  filter_configurations)
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
                  filtered_array <- filter_repository(repository, 
                    filter_configurations)
                }
                filtered_array
            }), target_depends = c("repository", "filter_configurations"
            )), deps = c("repository", "filter_configurations"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), align_to_analysis_event = targets::tar_target_raw(name = "aligned_array", 
        command = quote({
            .__target_expr__. <- quote({
                aligned_array <- align_trials(filtered_array, 
                  analysis_event_colname)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(aligned_array)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "aligned_array", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "aligned_array", target_expr = quote({
                {
                  aligned_array <- align_trials(filtered_array, 
                    analysis_event_colname)
                }
                aligned_array
            }), target_depends = c("filtered_array", "analysis_event_colname"
            )), deps = c("filtered_array", "analysis_event_colname"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), clean_condition_groups = targets::tar_target_raw(name = "condition_groups_clean", 
        command = quote({
            .__target_expr__. <- quote({
                condition_groups_clean <- ravecore::validate_condition_groupings(condition_groups, 
                  epoch = repository$epoch)
                if (!isTRUE(condition_groups_clean$n > 0)) {
                  stop("No valid condition group specified. Please check the condition groupings input.")
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(condition_groups_clean)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "condition_groups_clean", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "condition_groups_clean", target_expr = quote({
                {
                  condition_groups_clean <- ravecore::validate_condition_groupings(condition_groups, 
                    epoch = repository$epoch)
                  if (!isTRUE(condition_groups_clean$n > 0)) {
                    stop("No valid condition group specified. Please check the condition groupings input.")
                  }
                }
                condition_groups_clean
            }), target_depends = c("condition_groups", "repository"
            )), deps = c("condition_groups", "repository"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_crp_settings = targets::tar_target_raw(name = "crp_settings", 
        command = quote({
            .__target_expr__. <- quote({
                crp_settings <- prepare_crp_settings(filtered_array = filtered_array, 
                  crp_detection_window = crp_detection_window, 
                  crp_time_step = crp_time_step, crp_threshold_quantile = crp_threshold_quantile, 
                  crp_onset_border = crp_onset_border)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(crp_settings)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "crp_settings", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "crp_settings", target_expr = quote({
                {
                  crp_settings <- prepare_crp_settings(filtered_array = filtered_array, 
                    crp_detection_window = crp_detection_window, 
                    crp_time_step = crp_time_step, crp_threshold_quantile = crp_threshold_quantile, 
                    crp_onset_border = crp_onset_border)
                }
                crp_settings
            }), target_depends = c("filtered_array", "crp_detection_window", 
            "crp_time_step", "crp_threshold_quantile", "crp_onset_border"
            )), deps = c("filtered_array", "crp_detection_window", 
        "crp_time_step", "crp_threshold_quantile", "crp_onset_border"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), calculating_erp_durations = targets::tar_target_raw(name = "crp_results", 
        command = quote({
            .__target_expr__. <- quote({
                crp_results <- run_crp_on_channels(aligned_array = aligned_array, 
                  crp_settings = crp_settings, condition_groups_clean = condition_groups_clean)
                if (length(crp_results)) {
                  crp_results <- crp_results_to_df(crp_results)
                } else {
                  crp_results <- NULL
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(crp_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "crp_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "crp_results", target_expr = quote({
                {
                  crp_results <- run_crp_on_channels(aligned_array = aligned_array, 
                    crp_settings = crp_settings, condition_groups_clean = condition_groups_clean)
                  if (length(crp_results)) {
                    crp_results <- crp_results_to_df(crp_results)
                  } else {
                    crp_results <- NULL
                  }
                }
                crp_results
            }), target_depends = c("aligned_array", "crp_settings", 
            "condition_groups_clean")), deps = c("aligned_array", 
        "crp_settings", "condition_groups_clean"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_erp_duration_for_viewer = targets::tar_target_raw(name = "erp_results_for_viewer", 
        command = quote({
            .__target_expr__. <- quote({
                erp_results_for_viewer <- crp_results
                if (length(crp_results)) {
                  sub_table <- crp_results[, c("electrode", "group_index", 
                    "tau_R", "t_value_tR", "tau_onset", "al_mean", 
                    "al_p_mean", "snr_mean", "expl_var_mean")]
                  sub_table$group_index <- condition_groups_clean$group_labels[match(sub_table$group_index, 
                    condition_groups_clean$group_indexes)]
                  sub_table <- data.table::melt(sub_table, c("electrode", 
                    "group_index"), value.name = "value")
                  sub_table <- sub_table[complete.cases(sub_table), 
                    ]
                  sub_table <- data.table::data.table(Electrode = sub_table$electrode, 
                    vname = sprintf("%s (%s)", sub_table$variable, 
                      sub_table$group_index), value = sub_table$value)
                  erp_results_for_viewer <- data.table::dcast(sub_table, 
                    Electrode ~ vname, value.var = "value")
                  erp_results_for_viewer$Subject <- subject$subject_code
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(erp_results_for_viewer)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "erp_results_for_viewer", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "erp_results_for_viewer", target_expr = quote({
                {
                  erp_results_for_viewer <- crp_results
                  if (length(crp_results)) {
                    sub_table <- crp_results[, c("electrode", 
                      "group_index", "tau_R", "t_value_tR", "tau_onset", 
                      "al_mean", "al_p_mean", "snr_mean", "expl_var_mean")]
                    sub_table$group_index <- condition_groups_clean$group_labels[match(sub_table$group_index, 
                      condition_groups_clean$group_indexes)]
                    sub_table <- data.table::melt(sub_table, 
                      c("electrode", "group_index"), value.name = "value")
                    sub_table <- sub_table[complete.cases(sub_table), 
                      ]
                    sub_table <- data.table::data.table(Electrode = sub_table$electrode, 
                      vname = sprintf("%s (%s)", sub_table$variable, 
                        sub_table$group_index), value = sub_table$value)
                    erp_results_for_viewer <- data.table::dcast(sub_table, 
                      Electrode ~ vname, value.var = "value")
                    erp_results_for_viewer$Subject <- subject$subject_code
                  }
                }
                erp_results_for_viewer
            }), target_depends = c("crp_results", "condition_groups_clean", 
            "subject")), deps = c("crp_results", "condition_groups_clean", 
        "subject"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), prepare_plot_data_placeholder = targets::tar_target_raw(name = "data_placeholder", 
        command = quote({
            .__target_expr__. <- quote({
                data_placeholder <- condition_groups_clean
                aligned_array_impl <- aligned_array$`@impl`
                dnames <- dimnames(aligned_array_impl)
                data_placeholder$sample_rate <- aligned_array_impl$get_header("sample_rate")
                data_placeholder$loaded_lectrodes <- loaded_electrodes_clean
                data_placeholder$coord_table <- loaded_electrode_coordinates
                data_placeholder$time_points <- dnames$Time
                attr(data_placeholder, "signature") <- aligned_array_impl$get_header("signature_filters")
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
                  data_placeholder <- condition_groups_clean
                  aligned_array_impl <- aligned_array$`@impl`
                  dnames <- dimnames(aligned_array_impl)
                  data_placeholder$sample_rate <- aligned_array_impl$get_header("sample_rate")
                  data_placeholder$loaded_lectrodes <- loaded_electrodes_clean
                  data_placeholder$coord_table <- loaded_electrode_coordinates
                  data_placeholder$time_points <- dnames$Time
                  attr(data_placeholder, "signature") <- aligned_array_impl$get_header("signature_filters")
                }
                data_placeholder
            }), target_depends = c("condition_groups_clean", 
            "aligned_array", "loaded_electrodes_clean", "loaded_electrode_coordinates"
            )), deps = c("condition_groups_clean", "aligned_array", 
        "loaded_electrodes_clean", "loaded_electrode_coordinates"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), prepare_CRP_response = targets::tar_target_raw(name = "crp_by_channel", 
        command = quote({
            .__target_expr__. <- quote({
                crp_by_channel <- prepare_data_crp_by_channel(crp_df = crp_results, 
                  data_placeholder = data_placeholder)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(crp_by_channel)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "crp_by_channel", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "crp_by_channel", target_expr = quote({
                {
                  crp_by_channel <- prepare_data_crp_by_channel(crp_df = crp_results, 
                    data_placeholder = data_placeholder)
                }
                crp_by_channel
            }), target_depends = c("crp_results", "data_placeholder"
            )), deps = c("crp_results", "data_placeholder"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    prepare_voltage_over_channel_and_condition_by_collapsing_trials = targets::tar_target_raw(name = "data_by_channel_condition", 
        command = quote({
            .__target_expr__. <- quote({
                data_by_channel_condition <- prepare_data_by_channel_condition(aligned_array = aligned_array, 
                  data_placeholder = data_placeholder)
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
                  data_by_channel_condition <- prepare_data_by_channel_condition(aligned_array = aligned_array, 
                    data_placeholder = data_placeholder)
                }
                data_by_channel_condition
            }), target_depends = c("aligned_array", "data_placeholder"
            )), deps = c("aligned_array", "data_placeholder"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"))
