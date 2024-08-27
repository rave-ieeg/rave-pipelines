library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
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
    input_smooth_erp = targets::tar_target_raw("smooth_erp", 
        quote({
            settings[["smooth_erp"]]
        }), deps = "settings"), input_analysis_frequency = targets::tar_target_raw("analysis_frequency", 
        quote({
            settings[["analysis_frequency"]]
        }), deps = "settings"), input_analysis_time = targets::tar_target_raw("analysis_time", 
        quote({
            settings[["analysis_time"]]
        }), deps = "settings"), input_erp_baseline = targets::tar_target_raw("erp_baseline", 
        quote({
            settings[["erp_baseline"]]
        }), deps = "settings"), input_erp_analysiswindow = targets::tar_target_raw("erp_analysiswindow", 
        quote({
            settings[["erp_analysiswindow"]]
        }), deps = "settings"), input_analyze_electrodes = targets::tar_target_raw("analyze_electrodes", 
        quote({
            settings[["analyze_electrodes"]]
        }), deps = "settings"), input_erp_downsample_rate_to = targets::tar_target_raw("erp_downsample_rate_to", 
        quote({
            settings[["erp_downsample_rate_to"]]
        }), deps = "settings"), input_condition_groups = targets::tar_target_raw("condition_groups", 
        quote({
            settings[["condition_groups"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_epoch_choice__trial_ends = targets::tar_target_raw("epoch_choice__trial_ends", 
        quote({
            settings[["epoch_choice__trial_ends"]]
        }), deps = "settings"), input_epoch_choice__trial_starts = targets::tar_target_raw("epoch_choice__trial_starts", 
        quote({
            settings[["epoch_choice__trial_starts"]]
        }), deps = "settings"), input_epoch_choice = targets::tar_target_raw("epoch_choice", 
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
                subject_id <- subject$id
                print(subject_id)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave-subject", 
            target_export = "subject", target_expr = quote({
                {
                  subject <- RAVESubject$new(project_name = project_name, 
                    subject_code = subject_code)
                  subject_id <- subject$id
                  print(subject_id)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), check_loader_inputs = targets::tar_target_raw(name = "loader_params", 
        command = quote({
            .__target_expr__. <- quote({
                loader_params <- verify_loader_inputs(subject = subject, 
                  loaded_electrodes = loaded_electrodes, epoch_choice = epoch_choice, 
                  reference_name = reference_name, epoch_choice__trial_starts = epoch_choice__trial_starts, 
                  epoch_choice__trial_ends = epoch_choice__trial_ends)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(loader_params)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "loader_params", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "loader_params", target_expr = quote({
                {
                  loader_params <- verify_loader_inputs(subject = subject, 
                    loaded_electrodes = loaded_electrodes, epoch_choice = epoch_choice, 
                    reference_name = reference_name, epoch_choice__trial_starts = epoch_choice__trial_starts, 
                    epoch_choice__trial_ends = epoch_choice__trial_ends)
                }
                loader_params
            }), target_depends = c("subject", "loaded_electrodes", 
            "epoch_choice", "reference_name", "epoch_choice__trial_starts", 
            "epoch_choice__trial_ends")), deps = c("subject", 
        "loaded_electrodes", "epoch_choice", "reference_name", 
        "epoch_choice__trial_starts", "epoch_choice__trial_ends"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), load_phase_data = targets::tar_target_raw(name = "phase_repository", 
        command = quote({
            .__target_expr__. <- quote({
                phase_repository <- raveio::prepare_subject_phase(subject = subject, 
                  electrodes = loader_params$electrodes, epoch_name = loader_params$epoch, 
                  reference_name = loader_params$reference, time_windows = loader_params$time_window)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(phase_repository)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "phase_repository", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave_prepare_phase", 
            target_export = "phase_repository", target_expr = quote({
                {
                  phase_repository <- raveio::prepare_subject_phase(subject = subject, 
                    electrodes = loader_params$electrodes, epoch_name = loader_params$epoch, 
                    reference_name = loader_params$reference, 
                    time_windows = loader_params$time_window)
                }
                phase_repository
            }), target_depends = c("subject", "loader_params"
            )), deps = c("subject", "loader_params"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), load_voltage_data = targets::tar_target_raw(name = "voltage_repository", 
        command = quote({
            .__target_expr__. <- quote({
                voltage_repository <- raveio::prepare_subject_voltage_with_epoch(subject = subject, 
                  electrodes = loader_params$electrodes, epoch_name = loader_params$epoch, 
                  reference_name = loader_params$reference, time_windows = loader_params$time_window)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(voltage_repository)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "voltage_repository", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave_prepare_subject_voltage_with_epoch", 
            target_export = "voltage_repository", target_expr = quote({
                {
                  voltage_repository <- raveio::prepare_subject_voltage_with_epoch(subject = subject, 
                    electrodes = loader_params$electrodes, epoch_name = loader_params$epoch, 
                    reference_name = loader_params$reference, 
                    time_windows = loader_params$time_window)
                }
                voltage_repository
            }), target_depends = c("subject", "loader_params"
            )), deps = c("subject", "loader_params"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_requested_electrodes_for_analysis = targets::tar_target_raw(name = "requested_electrodes", 
        command = quote({
            .__target_expr__. <- quote({
                requested_electrodes <- dipsaus::parse_svec(analyze_electrodes)
                requested_electrodes <- requested_electrodes[requested_electrodes %in% 
                  phase_repository$electrode_list]
            })
            tryCatch({
                eval(.__target_expr__.)
                return(requested_electrodes)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "requested_electrodes", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "requested_electrodes", target_expr = quote({
                {
                  requested_electrodes <- dipsaus::parse_svec(analyze_electrodes)
                  requested_electrodes <- requested_electrodes[requested_electrodes %in% 
                    phase_repository$electrode_list]
                }
                requested_electrodes
            }), target_depends = c("analyze_electrodes", "phase_repository"
            )), deps = c("analyze_electrodes", "phase_repository"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), clean_condition_group_settings = targets::tar_target_raw(name = "group_cleaned", 
        command = quote({
            .__target_expr__. <- quote({
                epoch_data <- voltage_repository$epoch_table
                group_names <- sapply(seq_along(condition_groups), 
                  function(ii) {
                    g <- condition_groups[[ii]]
                    if (length(g$group_name) == 1 && nzchar(g$group_name)) {
                      g$group_name[[1]]
                    } else {
                      sprintf("Group%s", ii)
                    }
                  })
                group_has_trials <- sapply(condition_groups, 
                  function(g) {
                    cond <- g$group_conditions[g$group_conditions %in% 
                      epoch_data$Condition]
                    if (length(cond)) {
                      return(TRUE)
                    } else {
                      return(FALSE)
                    }
                  })
                duplicated_group_names <- group_names[group_has_trials]
                duplicated_group_names <- duplicated_group_names[duplicated(duplicated_group_names)]
                if (length(duplicated_group_names)) {
                  stop("Condition group name must not be duplicated. Please rename group: ", 
                    paste(duplicated_group_names, collapse = ", "))
                }
                group_cleaned <- dipsaus::drop_nulls(lapply(seq_along(condition_groups), 
                  function(idx) {
                    if (!group_has_trials[[idx]]) {
                      return()
                    }
                    g = condition_groups[[idx]]
                    cond <- g$group_conditions[g$group_conditions %in% 
                      epoch_data$Condition]
                    trial_selection <- epoch_data$Condition %in% 
                      unlist(g$group_conditions)
                    trial_number <- epoch_data$Trial[trial_selection]
                    list(name = g$group_name, group_index = idx, 
                      conditions = cond, trial_number = trial_number)
                  }))
                if (!length(group_cleaned)) {
                  stop("No valid condition group specified.")
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(group_cleaned)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "group_cleaned", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "group_cleaned", target_expr = quote({
                {
                  epoch_data <- voltage_repository$epoch_table
                  group_names <- sapply(seq_along(condition_groups), 
                    function(ii) {
                      g <- condition_groups[[ii]]
                      if (length(g$group_name) == 1 && nzchar(g$group_name)) {
                        g$group_name[[1]]
                      } else {
                        sprintf("Group%s", ii)
                      }
                    })
                  group_has_trials <- sapply(condition_groups, 
                    function(g) {
                      cond <- g$group_conditions[g$group_conditions %in% 
                        epoch_data$Condition]
                      if (length(cond)) {
                        return(TRUE)
                      } else {
                        return(FALSE)
                      }
                    })
                  duplicated_group_names <- group_names[group_has_trials]
                  duplicated_group_names <- duplicated_group_names[duplicated(duplicated_group_names)]
                  if (length(duplicated_group_names)) {
                    stop("Condition group name must not be duplicated. Please rename group: ", 
                      paste(duplicated_group_names, collapse = ", "))
                  }
                  group_cleaned <- dipsaus::drop_nulls(lapply(seq_along(condition_groups), 
                    function(idx) {
                      if (!group_has_trials[[idx]]) {
                        return()
                      }
                      g = condition_groups[[idx]]
                      cond <- g$group_conditions[g$group_conditions %in% 
                        epoch_data$Condition]
                      trial_selection <- epoch_data$Condition %in% 
                        unlist(g$group_conditions)
                      trial_number <- epoch_data$Trial[trial_selection]
                      list(name = g$group_name, group_index = idx, 
                        conditions = cond, trial_number = trial_number)
                    }))
                  if (!length(group_cleaned)) {
                    stop("No valid condition group specified.")
                  }
                }
                group_cleaned
            }), target_depends = c("voltage_repository", "condition_groups"
            )), deps = c("voltage_repository", "condition_groups"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), collapse_phase_across_electrodes = targets::tar_target_raw(name = "inter_electrode_phase_coherence", 
        command = quote({
            .__target_expr__. <- quote({
                inter_electrode_phase_coherence <- 0
                lapply(phase_repository$phase$data_list[phase_repository$electrode_list %in% 
                  requested_electrodes], function(arr) {
                  arr <- arr[reshape = dim(arr)[1:3], dimnames = FALSE]
                  inter_electrode_phase_coherence <<- inter_electrode_phase_coherence + 
                    exp((0+1i) * arr)
                  return()
                })
                inter_electrode_phase_coherence <- inter_electrode_phase_coherence/length(requested_electrodes)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(inter_electrode_phase_coherence)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "inter_electrode_phase_coherence", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "filearray", 
            target_export = "inter_electrode_phase_coherence", 
            target_expr = quote({
                {
                  inter_electrode_phase_coherence <- 0
                  lapply(phase_repository$phase$data_list[phase_repository$electrode_list %in% 
                    requested_electrodes], function(arr) {
                    arr <- arr[reshape = dim(arr)[1:3], dimnames = FALSE]
                    inter_electrode_phase_coherence <<- inter_electrode_phase_coherence + 
                      exp((0+1i) * arr)
                    return()
                  })
                  inter_electrode_phase_coherence <- inter_electrode_phase_coherence/length(requested_electrodes)
                }
                inter_electrode_phase_coherence
            }), target_depends = c("phase_repository", "requested_electrodes"
            )), deps = c("phase_repository", "requested_electrodes"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), downsample_ERP_signals = targets::tar_target_raw(name = "voltage_sub", 
        command = quote({
            .__target_expr__. <- quote({
                voltage_dnames <- voltage_repository$voltage$dimnames
                voltage_time <- voltage_dnames$Time
                erp_dsrate <- voltage_repository$sample_rate/erp_downsample_rate_to
                erp_timepoint_idx <- round(seq(from = 1L, to = length(voltage_time), 
                  by = erp_dsrate))
                filebase <- file.path(extdata_path, "erp_downsampled")
                voltage_subdim <- voltage_repository$voltage$dim
                voltage_subdim[[1]] <- length(erp_timepoint_idx)
                voltage_sub <- filearray::filearray_load_or_create(filebase = filebase, 
                  mode = "readonly", type = "float", dimension = voltage_subdim, 
                  partition_size = 1L, symlink_ok = FALSE, initialize = FALSE, 
                  signal_data_type = "voltage, epoched", repository_signature = voltage_repository$voltage$signature, 
                  erp_timepoint_idx = erp_timepoint_idx, on_missing = function(arr) {
                    dipsaus::lapply_async2(voltage_repository$voltage$data_list, 
                      function(x) {
                        subarray <- x[erp_timepoint_idx, , , 
                          drop = FALSE]
                        idx <- which(voltage_dnames$Electrode == 
                          dimnames(x)$Electrode)
                        arr[, , idx] <- subarray
                        return()
                      }, plan = FALSE)
                    voltage_dnames$Time <- voltage_dnames$Time[erp_timepoint_idx]
                    dimnames(arr) <- voltage_dnames
                  })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(voltage_sub)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "voltage_sub", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "filearray", 
            target_export = "voltage_sub", target_expr = quote({
                {
                  voltage_dnames <- voltage_repository$voltage$dimnames
                  voltage_time <- voltage_dnames$Time
                  erp_dsrate <- voltage_repository$sample_rate/erp_downsample_rate_to
                  erp_timepoint_idx <- round(seq(from = 1L, to = length(voltage_time), 
                    by = erp_dsrate))
                  filebase <- file.path(extdata_path, "erp_downsampled")
                  voltage_subdim <- voltage_repository$voltage$dim
                  voltage_subdim[[1]] <- length(erp_timepoint_idx)
                  voltage_sub <- filearray::filearray_load_or_create(filebase = filebase, 
                    mode = "readonly", type = "float", dimension = voltage_subdim, 
                    partition_size = 1L, symlink_ok = FALSE, 
                    initialize = FALSE, signal_data_type = "voltage, epoched", 
                    repository_signature = voltage_repository$voltage$signature, 
                    erp_timepoint_idx = erp_timepoint_idx, on_missing = function(arr) {
                      dipsaus::lapply_async2(voltage_repository$voltage$data_list, 
                        function(x) {
                          subarray <- x[erp_timepoint_idx, , 
                            , drop = FALSE]
                          idx <- which(voltage_dnames$Electrode == 
                            dimnames(x)$Electrode)
                          arr[, , idx] <- subarray
                          return()
                        }, plan = FALSE)
                      voltage_dnames$Time <- voltage_dnames$Time[erp_timepoint_idx]
                      dimnames(arr) <- voltage_dnames
                    })
                }
                voltage_sub
            }), target_depends = c("voltage_repository", "erp_downsample_rate_to"
            )), deps = c("voltage_repository", "erp_downsample_rate_to"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), generate_plot_data_ITPC_heatmap = targets::tar_target_raw(name = "plot_data_itpc_plot_heatmap", 
        command = quote({
            .__target_expr__. <- quote({
                plot_data_itpc_plot_heatmap <- generate_plot_data_itpc_plot_heatmap(phase_repository = phase_repository, 
                  requested_electrodes = requested_electrodes, 
                  group_cleaned = group_cleaned, inter_electrode_phase_coherence = inter_electrode_phase_coherence)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_data_itpc_plot_heatmap)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_data_itpc_plot_heatmap", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_data_itpc_plot_heatmap", target_expr = quote({
                {
                  plot_data_itpc_plot_heatmap <- generate_plot_data_itpc_plot_heatmap(phase_repository = phase_repository, 
                    requested_electrodes = requested_electrodes, 
                    group_cleaned = group_cleaned, inter_electrode_phase_coherence = inter_electrode_phase_coherence)
                }
                plot_data_itpc_plot_heatmap
            }), target_depends = c("phase_repository", "requested_electrodes", 
            "group_cleaned", "inter_electrode_phase_coherence"
            )), deps = c("phase_repository", "requested_electrodes", 
        "group_cleaned", "inter_electrode_phase_coherence"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    plot_ITPC_heatmap = targets::tar_target_raw(name = "plot_ITPC_heatmap_result", 
        command = quote({
            .__target_expr__. <- quote({
                plot_data <- plot_data_itpc_plot_heatmap
                ngroups <- plot_data$ngroups
                tryCatch({
                  plot_ITPC_heatmap_result <- plot_ITPC_heatmap(x = plot_data, 
                    analysis_time = analysis_time, analysis_frequency = analysis_frequency, 
                    zmax = 0, nrows = (ngroups >= 4) + 1, legend_size = ifelse(ngroups > 
                      1, lcm(3.5), lcm(4.5)), useRaster = TRUE)
                }, error = function(e) {
                  traceback(e)
                  stop(e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_ITPC_heatmap_result)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_ITPC_heatmap_result", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_ITPC_heatmap_result", target_expr = quote({
                {
                  plot_data <- plot_data_itpc_plot_heatmap
                  ngroups <- plot_data$ngroups
                  tryCatch({
                    plot_ITPC_heatmap_result <- plot_ITPC_heatmap(x = plot_data, 
                      analysis_time = analysis_time, analysis_frequency = analysis_frequency, 
                      zmax = 0, nrows = (ngroups >= 4) + 1, legend_size = ifelse(ngroups > 
                        1, lcm(3.5), lcm(4.5)), useRaster = TRUE)
                  }, error = function(e) {
                    traceback(e)
                    stop(e)
                  })
                }
                plot_ITPC_heatmap_result
            }), target_depends = c("plot_data_itpc_plot_heatmap", 
            "analysis_time", "analysis_frequency")), deps = c("plot_data_itpc_plot_heatmap", 
        "analysis_time", "analysis_frequency"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), generate_plot_data_erp_over_time = targets::tar_target_raw(name = "plot_data_erp_over_time", 
        command = quote({
            .__target_expr__. <- quote({
                plot_data_erp_over_time <- list(time = dimnames(voltage_sub)$Time, 
                  electrodes = requested_electrodes, ngroups = length(group_cleaned), 
                  group_names = sapply(group_cleaned, "[[", "name"), 
                  group_indices = sapply(group_cleaned, "[[", 
                    "group_index"), group_data = lapply(group_cleaned, 
                    function(g) {
                      all_data <- subset(voltage_sub, Trial ~ 
                        Trial %in% g$trial_number, Electrode ~ 
                        Electrode %in% requested_electrodes, 
                        drop = FALSE)
                      g$voltage_range <- range(all_data)
                      all_data <- ravetools::collapse(all_data, 
                        keep = c(2L, 1L), average = TRUE)
                      g$mean_se <- apply(all_data, 2, dipsaus::mean_se)
                      g$mean_se_range <- range(plus_minus(g$mean_se[1, 
                        ], g$mean_se[2, ]))
                      g$N <- nrow(all_data)
                      return(g)
                    }))
                plot_data_erp_over_time$erp_mean_se_range <- range(unlist(lapply(plot_data_erp_over_time$group_data, 
                  "[[", "mean_se_range")), na.rm = TRUE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_data_erp_over_time)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_data_erp_over_time", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_data_erp_over_time", target_expr = quote({
                {
                  plot_data_erp_over_time <- list(time = dimnames(voltage_sub)$Time, 
                    electrodes = requested_electrodes, ngroups = length(group_cleaned), 
                    group_names = sapply(group_cleaned, "[[", 
                      "name"), group_indices = sapply(group_cleaned, 
                      "[[", "group_index"), group_data = lapply(group_cleaned, 
                      function(g) {
                        all_data <- subset(voltage_sub, Trial ~ 
                          Trial %in% g$trial_number, Electrode ~ 
                          Electrode %in% requested_electrodes, 
                          drop = FALSE)
                        g$voltage_range <- range(all_data)
                        all_data <- ravetools::collapse(all_data, 
                          keep = c(2L, 1L), average = TRUE)
                        g$mean_se <- apply(all_data, 2, dipsaus::mean_se)
                        g$mean_se_range <- range(plus_minus(g$mean_se[1, 
                          ], g$mean_se[2, ]))
                        g$N <- nrow(all_data)
                        return(g)
                      }))
                  plot_data_erp_over_time$erp_mean_se_range <- range(unlist(lapply(plot_data_erp_over_time$group_data, 
                    "[[", "mean_se_range")), na.rm = TRUE)
                }
                plot_data_erp_over_time
            }), target_depends = c("voltage_sub", "requested_electrodes", 
            "group_cleaned")), deps = c("voltage_sub", "requested_electrodes", 
        "group_cleaned"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), plot_itpc_erp_time_plot = targets::tar_target_raw(name = "plot_itpc_erp_time_result", 
        command = quote({
            .__target_expr__. <- quote({
                plot_itpc_erp_time_result <- Sys.time()
                itpc_erp_time_plot(plot_data_erp_over_time = plot_data_erp_over_time, 
                  plot_data_itpc_plot_heatmap = plot_data_itpc_plot_heatmap, 
                  merge_plots = FALSE, phase_frequency = 2, smooth_erp = 2, 
                  index_time_range = c(0, 1.25))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_itpc_erp_time_result)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_itpc_erp_time_result", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_itpc_erp_time_result", target_expr = quote({
                {
                  plot_itpc_erp_time_result <- Sys.time()
                  itpc_erp_time_plot(plot_data_erp_over_time = plot_data_erp_over_time, 
                    plot_data_itpc_plot_heatmap = plot_data_itpc_plot_heatmap, 
                    merge_plots = FALSE, phase_frequency = 2, 
                    smooth_erp = 2, index_time_range = c(0, 1.25))
                }
                plot_itpc_erp_time_result
            }), target_depends = c("plot_data_erp_over_time", 
            "plot_data_itpc_plot_heatmap")), deps = c("plot_data_erp_over_time", 
        "plot_data_itpc_plot_heatmap"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"))
