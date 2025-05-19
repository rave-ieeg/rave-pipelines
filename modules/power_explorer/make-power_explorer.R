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
    input_trials_to_export = targets::tar_target_raw("trials_to_export", 
        quote({
            settings[["trials_to_export"]]
        }), deps = "settings"), input_trial_outliers_list = targets::tar_target_raw("trial_outliers_list", 
        quote({
            settings[["trial_outliers_list"]]
        }), deps = "settings"), input_times_to_export = targets::tar_target_raw("times_to_export", 
        quote({
            settings[["times_to_export"]]
        }), deps = "settings"), input_time_censor = targets::tar_target_raw("time_censor", 
        quote({
            settings[["time_censor"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_second_condition_groupings = targets::tar_target_raw("second_condition_groupings", 
        quote({
            settings[["second_condition_groupings"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_omnibus_includes_all_electrodes = targets::tar_target_raw("omnibus_includes_all_electrodes", 
        quote({
            settings[["omnibus_includes_all_electrodes"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes", 
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_frequencies_to_export = targets::tar_target_raw("frequencies_to_export", 
        quote({
            settings[["frequencies_to_export"]]
        }), deps = "settings"), input_first_condition_groupings = targets::tar_target_raw("first_condition_groupings", 
        quote({
            settings[["first_condition_groupings"]]
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
        }), deps = "settings"), input_epoch_choice__load_single_trial = targets::tar_target_raw("epoch_choice__load_single_trial", 
        quote({
            settings[["epoch_choice__load_single_trial"]]
        }), deps = "settings"), input_epoch_choice = targets::tar_target_raw("epoch_choice", 
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_enable_second_condition_groupings = targets::tar_target_raw("enable_second_condition_groupings", 
        quote({
            settings[["enable_second_condition_groupings"]]
        }), deps = "settings"), input_enable_custom_ROI = targets::tar_target_raw("enable_custom_ROI", 
        quote({
            settings[["enable_custom_ROI"]]
        }), deps = "settings"), input_electrodes_to_export_roi_name = targets::tar_target_raw("electrodes_to_export_roi_name", 
        quote({
            settings[["electrodes_to_export_roi_name"]]
        }), deps = "settings"), input_electrodes_to_export_roi_categories = targets::tar_target_raw("electrodes_to_export_roi_categories", 
        quote({
            settings[["electrodes_to_export_roi_categories"]]
        }), deps = "settings"), input_electrodes_to_export = targets::tar_target_raw("electrodes_to_export", 
        quote({
            settings[["electrodes_to_export"]]
        }), deps = "settings"), input_electrode_export_file_type = targets::tar_target_raw("electrode_export_file_type", 
        quote({
            settings[["electrode_export_file_type"]]
        }), deps = "settings"), input_electrode_export_data_type = targets::tar_target_raw("electrode_export_data_type", 
        quote({
            settings[["electrode_export_data_type"]]
        }), deps = "settings"), input_custom_roi_variable = targets::tar_target_raw("custom_roi_variable", 
        quote({
            settings[["custom_roi_variable"]]
        }), deps = "settings"), input_custom_roi_type = targets::tar_target_raw("custom_roi_type", 
        quote({
            settings[["custom_roi_type"]]
        }), deps = "settings"), input_custom_roi_groupings = targets::tar_target_raw("custom_roi_groupings", 
        quote({
            settings[["custom_roi_groupings"]]
        }), deps = "settings"), input_condition_variable = targets::tar_target_raw("condition_variable", 
        quote({
            settings[["condition_variable"]]
        }), deps = "settings"), input_baseline_settings = targets::tar_target_raw("baseline_settings", 
        quote({
            settings[["baseline_settings"]]
        }), deps = "settings"), input_analysis_settings = targets::tar_target_raw("analysis_settings", 
        quote({
            settings[["analysis_settings"]]
        }), deps = "settings"), input_analysis_electrodes = targets::tar_target_raw("analysis_electrodes", 
        quote({
            settings[["analysis_electrodes"]]
        }), deps = "settings"), check_load_power = targets::tar_target_raw(name = "repository", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::as_rave_subject(subject_id = sprintf("%s/%s", 
                  project_name, subject_code))
                if (exists("epoch_choice__load_single_trial") && 
                  isTRUE(epoch_choice__load_single_trial)) {
                  to_ep <- function(en) {
                    file.path(subject$meta_path, paste0("epoch_", 
                      en, ".csv"))
                  }
                  ec2 <- paste0("single_trial_", epoch_choice)
                  ec2_path <- to_ep(ec2)
                  ep <- read.csv(to_ep(epoch_choice))
                  block1 <- subset(ep, ep$Block == ep$Block[1])
                  mx_time <- max(block1$Time) + epoch_choice__trial_ends
                  write.csv(x = block1[1, , drop = FALSE], file = ec2_path, 
                    row.names = FALSE)
                  repository <- raveio::prepare_subject_power(subject = subject, 
                    electrodes = loaded_electrodes, epoch_name = ec2, 
                    reference_name = reference_name, time_windows = c(epoch_choice__trial_starts, 
                      mx_time + epoch_choice__trial_ends))
                } else {
                  repository <- raveio::prepare_subject_power(subject = subject, 
                    electrodes = loaded_electrodes, epoch_name = epoch_choice, 
                    reference_name = reference_name, time_windows = c(epoch_choice__trial_starts, 
                      epoch_choice__trial_ends))
                }
                repository
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "repository", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "rave_prepare_power", 
            target_export = "repository", target_expr = quote({
                {
                  subject <- raveio::as_rave_subject(subject_id = sprintf("%s/%s", 
                    project_name, subject_code))
                  if (exists("epoch_choice__load_single_trial") && 
                    isTRUE(epoch_choice__load_single_trial)) {
                    to_ep <- function(en) {
                      file.path(subject$meta_path, paste0("epoch_", 
                        en, ".csv"))
                    }
                    ec2 <- paste0("single_trial_", epoch_choice)
                    ec2_path <- to_ep(ec2)
                    ep <- read.csv(to_ep(epoch_choice))
                    block1 <- subset(ep, ep$Block == ep$Block[1])
                    mx_time <- max(block1$Time) + epoch_choice__trial_ends
                    write.csv(x = block1[1, , drop = FALSE], 
                      file = ec2_path, row.names = FALSE)
                    repository <- raveio::prepare_subject_power(subject = subject, 
                      electrodes = loaded_electrodes, epoch_name = ec2, 
                      reference_name = reference_name, time_windows = c(epoch_choice__trial_starts, 
                        mx_time + epoch_choice__trial_ends))
                  } else {
                    repository <- raveio::prepare_subject_power(subject = subject, 
                      electrodes = loaded_electrodes, epoch_name = epoch_choice, 
                      reference_name = reference_name, time_windows = c(epoch_choice__trial_starts, 
                        epoch_choice__trial_ends))
                  }
                  repository
                }
                repository
            }), target_depends = c("project_name", "subject_code", 
            "epoch_choice__load_single_trial", "epoch_choice", 
            "epoch_choice__trial_ends", "loaded_electrodes", 
            "reference_name", "epoch_choice__trial_starts")), 
        deps = c("project_name", "subject_code", "epoch_choice__load_single_trial", 
        "epoch_choice", "epoch_choice__trial_ends", "loaded_electrodes", 
        "reference_name", "epoch_choice__trial_starts"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), check_requested_electrodes = targets::tar_target_raw(name = "requested_electrodes", 
        command = quote({
            .__target_expr__. <- quote({
                requested_electrodes <- dipsaus::parse_svec(analysis_electrodes, 
                  sep = ",|;", connect = ":-")
                requested_electrodes <- requested_electrodes[requested_electrodes %in% 
                  repository$power$dimnames$Electrode]
                if (!length(requested_electrodes)) {
                  stop("No electrode selected")
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(requested_electrodes)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "requested_electrodes", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "requested_electrodes", target_expr = quote({
                {
                  requested_electrodes <- dipsaus::parse_svec(analysis_electrodes, 
                    sep = ",|;", connect = ":-")
                  requested_electrodes <- requested_electrodes[requested_electrodes %in% 
                    repository$power$dimnames$Electrode]
                  if (!length(requested_electrodes)) {
                    stop("No electrode selected")
                  }
                }
                requested_electrodes
            }), target_depends = c("analysis_electrodes", "repository"
            )), deps = c("analysis_electrodes", "repository"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    check_analysis_settings = targets::tar_target_raw(name = "analysis_settings_clean", 
        command = quote({
            .__target_expr__. <- quote({
                check_range <- function(x, lim, lbl) {
                  if (!all(x %within% lim)) stop(sprintf("Requested %s [%s] not within available range [%s]", 
                    lbl, str_collapse(range(x), ":"), str_collapse(range(lim), 
                      ":")), call. = FALSE)
                }
                if (length(repository$time_windows) != 1) stop("discontinuous time windows not supported")
                analysis_settings_clean <- lapply(analysis_settings, 
                  function(as) {
                    as$time %<>% unlist
                    as$frequency %<>% unlist
                    if (is.null(as$label) || nchar(as$label) < 
                      1) {
                      as$label <- paste("Window", rand_string(length = 4))
                    }
                    if (is.null(as$censor_info)) {
                      as$censor_info <- list(enabled = FALSE, 
                        window = 0:1)
                    }
                    return(as)
                  })
                ua <- get_unit_of_analysis(names = TRUE)
                if (!baseline_settings$unit_of_analysis %in% 
                  ua) {
                  stop(sprintf("Requested unit of analysis \"%s\" must be one of: %s", 
                    baseline_settings$unit_of_analysis, str_collapse(ua)))
                }
                ua <- get_baseline_scope(names = TRUE)
                if (!baseline_settings$scope %in% ua) {
                  stop(sprintf("Requested baseline scope \"%s\" must be one of: %s", 
                    baseline_settings$scope, str_collapse(ua)))
                }
                epoch_event_types = get_available_events(repository$epoch$columns)
                lapply(analysis_settings_clean, function(setting) {
                  check_range(setting$frequency, unlist(repository$frequency), 
                    "frequency")
                  check_range(setting$time, unlist(repository$time_windows), 
                    "analysis time")
                  if (!(setting$event %in% epoch_event_types)) {
                    stop(sprintf("Requested event \"%s\" must be one of: %s", 
                      setting$event, str_collapse(epoch_event_types)))
                  }
                })
                names(analysis_settings_clean) <- sapply(analysis_settings_clean, 
                  `[[`, "label")
                dd <- duplicated(sapply(analysis_settings_clean, 
                  `[[`, "label"))
                while (sum(dd)) {
                  for (w in which(dd)) {
                    analysis_settings_clean[[w]]$label = paste(analysis_settings_clean[[w]]$label, 
                      rand_string(length = 4))
                  }
                  dd <- duplicated(sapply(analysis_settings_clean, 
                    `[[`, "label"))
                }
                for (ii in seq_along(analysis_settings_clean)) {
                  analysis_settings_clean[[ii]]$censor_info = time_censor
                  analysis_settings_clean[[ii]]$censor_info$window %<>% 
                    unlist
                }
                for (ii in seq_along(first_condition_groupings)) {
                  if (!nzchar(first_condition_groupings[[ii]]$label)) {
                    first_condition_groupings[[ii]]$label = paste("Group", 
                      ii)
                  }
                }
                dd <- duplicated(sapply(first_condition_groupings, 
                  `[[`, "label"))
                while (sum(dd)) {
                  for (w in which(dd)) {
                    first_condition_groupings[[w]]$label = paste(first_condition_groupings[[w]]$label, 
                      rand_string(length = 4))
                  }
                  dd <- duplicated(sapply(first_condition_groupings, 
                    `[[`, "label"))
                }
                fcg <- c(unlist(sapply(first_condition_groupings, 
                  `[[`, "conditions")))
                if (isTRUE(enable_second_condition_groupings)) {
                  scg <- c(unlist(sapply(second_condition_groupings, 
                    `[[`, "conditions")))
                  stopifnot(setequal(scg, fcg))
                  stopifnot(all(!duplicated(scg)))
                }
                if (any(duplicated(fcg))) {
                  warning("Duplication in first factor, results may be unreliable")
                }
                if (is.list(trial_outliers_list)) {
                  trial_outliers_list %<>% unlist
                }
                for (ii in seq_along(analysis_settings_clean)) {
                  analysis_settings_clean[[ii]]$subject_code = subject_code
                  analysis_settings_clean[[ii]]$project_name = project_name
                }
                analysis_checks_passed = TRUE
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_settings_clean)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "analysis_settings_clean", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "analysis_settings_clean", target_expr = quote({
                {
                  check_range <- function(x, lim, lbl) {
                    if (!all(x %within% lim)) stop(sprintf("Requested %s [%s] not within available range [%s]", 
                      lbl, str_collapse(range(x), ":"), str_collapse(range(lim), 
                        ":")), call. = FALSE)
                  }
                  if (length(repository$time_windows) != 1) stop("discontinuous time windows not supported")
                  analysis_settings_clean <- lapply(analysis_settings, 
                    function(as) {
                      as$time %<>% unlist
                      as$frequency %<>% unlist
                      if (is.null(as$label) || nchar(as$label) < 
                        1) {
                        as$label <- paste("Window", rand_string(length = 4))
                      }
                      if (is.null(as$censor_info)) {
                        as$censor_info <- list(enabled = FALSE, 
                          window = 0:1)
                      }
                      return(as)
                    })
                  ua <- get_unit_of_analysis(names = TRUE)
                  if (!baseline_settings$unit_of_analysis %in% 
                    ua) {
                    stop(sprintf("Requested unit of analysis \"%s\" must be one of: %s", 
                      baseline_settings$unit_of_analysis, str_collapse(ua)))
                  }
                  ua <- get_baseline_scope(names = TRUE)
                  if (!baseline_settings$scope %in% ua) {
                    stop(sprintf("Requested baseline scope \"%s\" must be one of: %s", 
                      baseline_settings$scope, str_collapse(ua)))
                  }
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  lapply(analysis_settings_clean, function(setting) {
                    check_range(setting$frequency, unlist(repository$frequency), 
                      "frequency")
                    check_range(setting$time, unlist(repository$time_windows), 
                      "analysis time")
                    if (!(setting$event %in% epoch_event_types)) {
                      stop(sprintf("Requested event \"%s\" must be one of: %s", 
                        setting$event, str_collapse(epoch_event_types)))
                    }
                  })
                  names(analysis_settings_clean) <- sapply(analysis_settings_clean, 
                    `[[`, "label")
                  dd <- duplicated(sapply(analysis_settings_clean, 
                    `[[`, "label"))
                  while (sum(dd)) {
                    for (w in which(dd)) {
                      analysis_settings_clean[[w]]$label = paste(analysis_settings_clean[[w]]$label, 
                        rand_string(length = 4))
                    }
                    dd <- duplicated(sapply(analysis_settings_clean, 
                      `[[`, "label"))
                  }
                  for (ii in seq_along(analysis_settings_clean)) {
                    analysis_settings_clean[[ii]]$censor_info = time_censor
                    analysis_settings_clean[[ii]]$censor_info$window %<>% 
                      unlist
                  }
                  for (ii in seq_along(first_condition_groupings)) {
                    if (!nzchar(first_condition_groupings[[ii]]$label)) {
                      first_condition_groupings[[ii]]$label = paste("Group", 
                        ii)
                    }
                  }
                  dd <- duplicated(sapply(first_condition_groupings, 
                    `[[`, "label"))
                  while (sum(dd)) {
                    for (w in which(dd)) {
                      first_condition_groupings[[w]]$label = paste(first_condition_groupings[[w]]$label, 
                        rand_string(length = 4))
                    }
                    dd <- duplicated(sapply(first_condition_groupings, 
                      `[[`, "label"))
                  }
                  fcg <- c(unlist(sapply(first_condition_groupings, 
                    `[[`, "conditions")))
                  if (isTRUE(enable_second_condition_groupings)) {
                    scg <- c(unlist(sapply(second_condition_groupings, 
                      `[[`, "conditions")))
                    stopifnot(setequal(scg, fcg))
                    stopifnot(all(!duplicated(scg)))
                  }
                  if (any(duplicated(fcg))) {
                    warning("Duplication in first factor, results may be unreliable")
                  }
                  if (is.list(trial_outliers_list)) {
                    trial_outliers_list %<>% unlist
                  }
                  for (ii in seq_along(analysis_settings_clean)) {
                    analysis_settings_clean[[ii]]$subject_code = subject_code
                    analysis_settings_clean[[ii]]$project_name = project_name
                  }
                  analysis_checks_passed = TRUE
                }
                analysis_settings_clean
            }), target_depends = c("repository", "analysis_settings", 
            "baseline_settings", "time_censor", "first_condition_groupings", 
            "enable_second_condition_groupings", "second_condition_groupings", 
            "trial_outliers_list", "subject_code", "project_name"
            )), deps = c("repository", "analysis_settings", "baseline_settings", 
        "time_censor", "first_condition_groupings", "enable_second_condition_groupings", 
        "second_condition_groupings", "trial_outliers_list", 
        "subject_code", "project_name"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), calculate_baseline = targets::tar_target_raw(name = "baselined_power", 
        command = quote({
            .__target_expr__. <- quote({
                if (exists("epoch_choice__load_single_trial") && 
                  isTRUE(epoch_choice__load_single_trial)) {
                  raveio::power_baseline(x = repository, baseline_windows = list(range(repository$time_points)), 
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                    units = get_baseline_scope(baseline_settings$scope), 
                    signal_type = "LFP", electrodes = requested_electrodes)
                } else {
                  raveio::power_baseline(x = repository, baseline_windows = baseline_settings$window, 
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                    units = get_baseline_scope(baseline_settings$scope), 
                    signal_type = "LFP", electrodes = requested_electrodes)
                }
                baselined_power <- repository$power$baselined
            })
            tryCatch({
                eval(.__target_expr__.)
                return(baselined_power)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "baselined_power", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "user-defined-r", 
            target_export = "baselined_power", target_expr = quote({
                {
                  if (exists("epoch_choice__load_single_trial") && 
                    isTRUE(epoch_choice__load_single_trial)) {
                    raveio::power_baseline(x = repository, baseline_windows = list(range(repository$time_points)), 
                      method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                      units = get_baseline_scope(baseline_settings$scope), 
                      signal_type = "LFP", electrodes = requested_electrodes)
                  } else {
                    raveio::power_baseline(x = repository, baseline_windows = baseline_settings$window, 
                      method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                      units = get_baseline_scope(baseline_settings$scope), 
                      signal_type = "LFP", electrodes = requested_electrodes)
                  }
                  baselined_power <- repository$power$baselined
                }
                baselined_power
            }), target_depends = c("epoch_choice__load_single_trial", 
            "repository", "baseline_settings", "requested_electrodes"
            )), deps = c("epoch_choice__load_single_trial", "repository", 
        "baseline_settings", "requested_electrodes"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), build_trial_details = targets::tar_target_raw(name = "trial_details", 
        command = quote({
            .__target_expr__. <- quote({
                k = sapply(lapply(first_condition_groupings, 
                  `[[`, "conditions"), length)
                fcgs <- first_condition_groupings[k > 0]
                all_trials <- c(unname(unlist(lapply(fcgs, `[[`, 
                  "conditions"))))
                ep_table <- repository$epoch$table
                tbl <- subset(ep_table, ep_table[[condition_variable]] %in% 
                  all_trials, select = c("Trial", condition_variable))
                f1 <- rutabaga::rbind_list(lapply(fcgs, function(ff) {
                  df = list()
                  df[[condition_variable]] = ff$conditions
                  df$Factor1 = ff$label
                  as.data.frame(df)
                }))
                trial_details <- merge(tbl, f1, by = condition_variable)
                if (isTRUE(enable_second_condition_groupings)) {
                  f2 <- rutabaga::rbind_list(lapply(second_condition_groupings, 
                    function(ff) {
                      df = list()
                      df[[condition_variable]] = ff$conditions
                      df$Factor2 = ff$label
                      as.data.frame(df)
                    }))
                  trial_details %<>% merge(f2, by = condition_variable)
                }
                trial_details = trial_details[order(trial_details$Trial), 
                  ]
                trial_details$Factor1 %<>% factor(levels = sapply(fcgs, 
                  `[[`, "label"))
                if (!is.null(trial_details$Factor2)) {
                  trial_details$Factor2 %<>% factor(levels = sapply(second_condition_groupings, 
                    `[[`, "label"))
                }
                rownames(trial_details) = trial_details$Trial
            })
            tryCatch({
                eval(.__target_expr__.)
                return(trial_details)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "trial_details", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "trial_details", target_expr = quote({
                {
                  k = sapply(lapply(first_condition_groupings, 
                    `[[`, "conditions"), length)
                  fcgs <- first_condition_groupings[k > 0]
                  all_trials <- c(unname(unlist(lapply(fcgs, 
                    `[[`, "conditions"))))
                  ep_table <- repository$epoch$table
                  tbl <- subset(ep_table, ep_table[[condition_variable]] %in% 
                    all_trials, select = c("Trial", condition_variable))
                  f1 <- rutabaga::rbind_list(lapply(fcgs, function(ff) {
                    df = list()
                    df[[condition_variable]] = ff$conditions
                    df$Factor1 = ff$label
                    as.data.frame(df)
                  }))
                  trial_details <- merge(tbl, f1, by = condition_variable)
                  if (isTRUE(enable_second_condition_groupings)) {
                    f2 <- rutabaga::rbind_list(lapply(second_condition_groupings, 
                      function(ff) {
                        df = list()
                        df[[condition_variable]] = ff$conditions
                        df$Factor2 = ff$label
                        as.data.frame(df)
                      }))
                    trial_details %<>% merge(f2, by = condition_variable)
                  }
                  trial_details = trial_details[order(trial_details$Trial), 
                    ]
                  trial_details$Factor1 %<>% factor(levels = sapply(fcgs, 
                    `[[`, "label"))
                  if (!is.null(trial_details$Factor2)) {
                    trial_details$Factor2 %<>% factor(levels = sapply(second_condition_groupings, 
                      `[[`, "label"))
                  }
                  rownames(trial_details) = trial_details$Trial
                }
                trial_details
            }), target_depends = c("first_condition_groupings", 
            "repository", "condition_variable", "enable_second_condition_groupings", 
            "second_condition_groupings")), deps = c("first_condition_groupings", 
        "repository", "condition_variable", "enable_second_condition_groupings", 
        "second_condition_groupings"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), build_analysis_groups = targets::tar_target_raw(name = "analysis_groups", 
        command = quote({
            .__target_expr__. <- quote({
                if (isTRUE(enable_second_condition_groupings)) {
                  by_group <- split(trial_details, list(trial_details$Factor1, 
                    trial_details$Factor2))
                  analysis_groups <- vector("list", length(by_group))
                  for (ii in seq_along(by_group)) {
                    analysis_groups[[ii]] <- list(label = names(by_group)[[ii]], 
                      conditions = unique(by_group[[ii]]$Condition), 
                      condition_per_trial = by_group[[ii]]$Condition, 
                      trials = by_group[[ii]]$Trial, index = ii, 
                      has_trials = TRUE)
                  }
                  attr(analysis_groups, "meta") <- trial_details
                } else {
                  analysis_groups <- mapply(function(cg, ii) {
                    trials <- c()
                    if (length(cg$conditions) > 0) {
                      trials <- repository$epoch$table$Trial[repository$epoch$table[[condition_variable]] %in% 
                        cg$conditions]
                    }
                    list(label = cg$label, conditions = cg$conditions, 
                      trials = trials, index = ii, has_trials = length(trials) > 
                        0)
                  }, first_condition_groupings, seq_along(first_condition_groupings), 
                    SIMPLIFY = FALSE)
                  has_trials <- which(sapply(analysis_groups, 
                    `[[`, "has_trials"))
                  analysis_groups = analysis_groups[has_trials]
                }
                if (!exists("epoch_choice__load_single_trial") || 
                  !isTRUE(epoch_choice__load_single_trial)) {
                  if (length(analysis_groups) < 1) {
                    stop("No trials available in condition groups")
                  }
                }
                names(analysis_groups) <- sapply(analysis_groups, 
                  `[[`, "label")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_groups)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "analysis_groups", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "analysis_groups", target_expr = quote({
                {
                  if (isTRUE(enable_second_condition_groupings)) {
                    by_group <- split(trial_details, list(trial_details$Factor1, 
                      trial_details$Factor2))
                    analysis_groups <- vector("list", length(by_group))
                    for (ii in seq_along(by_group)) {
                      analysis_groups[[ii]] <- list(label = names(by_group)[[ii]], 
                        conditions = unique(by_group[[ii]]$Condition), 
                        condition_per_trial = by_group[[ii]]$Condition, 
                        trials = by_group[[ii]]$Trial, index = ii, 
                        has_trials = TRUE)
                    }
                    attr(analysis_groups, "meta") <- trial_details
                  } else {
                    analysis_groups <- mapply(function(cg, ii) {
                      trials <- c()
                      if (length(cg$conditions) > 0) {
                        trials <- repository$epoch$table$Trial[repository$epoch$table[[condition_variable]] %in% 
                          cg$conditions]
                      }
                      list(label = cg$label, conditions = cg$conditions, 
                        trials = trials, index = ii, has_trials = length(trials) > 
                          0)
                    }, first_condition_groupings, seq_along(first_condition_groupings), 
                      SIMPLIFY = FALSE)
                    has_trials <- which(sapply(analysis_groups, 
                      `[[`, "has_trials"))
                    analysis_groups = analysis_groups[has_trials]
                  }
                  if (!exists("epoch_choice__load_single_trial") || 
                    !isTRUE(epoch_choice__load_single_trial)) {
                    if (length(analysis_groups) < 1) {
                      stop("No trials available in condition groups")
                    }
                  }
                  names(analysis_groups) <- sapply(analysis_groups, 
                    `[[`, "label")
                }
                analysis_groups
            }), target_depends = c("enable_second_condition_groupings", 
            "trial_details", "repository", "condition_variable", 
            "first_condition_groupings", "epoch_choice__load_single_trial"
            )), deps = c("enable_second_condition_groupings", 
        "trial_details", "repository", "condition_variable", 
        "first_condition_groupings", "epoch_choice__load_single_trial"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_pluriform_power = targets::tar_target_raw(name = "pluriform_power", 
        command = quote({
            .__target_expr__. <- quote({
                pluriform_power <- NULL
            })
            tryCatch({
                eval(.__target_expr__.)
                return(pluriform_power)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "pluriform_power", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "user-defined-r", 
            target_export = "pluriform_power", target_expr = quote({
                {
                  pluriform_power <- NULL
                }
                pluriform_power
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    build_by_frequency_over_time_data = targets::tar_target_raw(name = "by_frequency_over_time_data", 
        command = quote({
            .__target_expr__. <- quote({
                baselined_power_data <- subset(baselined_power, 
                  Electrode ~ Electrode %in% requested_electrodes)
                epoch_event_types = get_available_events(repository$epoch$columns)
                by_frequency_over_time_data <- do.call(c, sapply(analysis_settings_clean, 
                  function(asc) {
                    collapse_e <- raveio::collapse2(baselined_power_data, 
                      keep = 1:3, method = "mean")
                    dim(collapse_e) = c(dim(baselined_power_data)[1:3], 
                      1)
                    dimnames(collapse_e) <- append(dimnames(baselined_power_data)[1:3], 
                      list(Electrode = "Avg"))
                    re <- shift_baselined_power(baselined_data = collapse_e, 
                      events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                      event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                    all_ag <- sapply(analysis_groups, function(ag) {
                      trials_to_keep <- ag$trials
                      if (!is.null(trial_outliers_list)) {
                        trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                          trial_outliers_list]
                      }
                      ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                        trials_to_keep
                      ag$data = raveio::collapse2(re$data[, , 
                        ti, , drop = FALSE], keep = 2:1, method = "mean")
                      ag$x = as.numeric(dimnames(re$data)$Time)
                      ag$y = as.numeric(dimnames(re$data)$Frequency)
                      ag$xlab = "Time (s)"
                      ag$ylab = "Frequency"
                      ag$zlab = "Mean " %&% baseline_settings$unit_of_analysis
                      ag$condition_group = ag$label
                      ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                      ag$outliers = trial_outliers_list
                      ag$events = subset(re$events, Trial %in% 
                        ag$trials)
                      ag %<>% add_analysis_settings(asc, baseline_settings)
                      ag$range = range(ag$data)
                      ag
                    }, simplify = FALSE)
                    return(all_ag)
                  }, simplify = FALSE, USE.NAMES = TRUE))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_frequency_over_time_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "by_frequency_over_time_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "by_frequency_over_time_data", target_expr = quote({
                {
                  baselined_power_data <- subset(baselined_power, 
                    Electrode ~ Electrode %in% requested_electrodes)
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  by_frequency_over_time_data <- do.call(c, sapply(analysis_settings_clean, 
                    function(asc) {
                      collapse_e <- raveio::collapse2(baselined_power_data, 
                        keep = 1:3, method = "mean")
                      dim(collapse_e) = c(dim(baselined_power_data)[1:3], 
                        1)
                      dimnames(collapse_e) <- append(dimnames(baselined_power_data)[1:3], 
                        list(Electrode = "Avg"))
                      re <- shift_baselined_power(baselined_data = collapse_e, 
                        events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                        event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                      all_ag <- sapply(analysis_groups, function(ag) {
                        trials_to_keep <- ag$trials
                        if (!is.null(trial_outliers_list)) {
                          trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                            trial_outliers_list]
                        }
                        ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                          trials_to_keep
                        ag$data = raveio::collapse2(re$data[, 
                          , ti, , drop = FALSE], keep = 2:1, 
                          method = "mean")
                        ag$x = as.numeric(dimnames(re$data)$Time)
                        ag$y = as.numeric(dimnames(re$data)$Frequency)
                        ag$xlab = "Time (s)"
                        ag$ylab = "Frequency"
                        ag$zlab = "Mean " %&% baseline_settings$unit_of_analysis
                        ag$condition_group = ag$label
                        ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                        ag$outliers = trial_outliers_list
                        ag$events = subset(re$events, Trial %in% 
                          ag$trials)
                        ag %<>% add_analysis_settings(asc, baseline_settings)
                        ag$range = range(ag$data)
                        ag
                      }, simplify = FALSE)
                      return(all_ag)
                    }, simplify = FALSE, USE.NAMES = TRUE))
                }
                by_frequency_over_time_data
            }), target_depends = c("baselined_power", "requested_electrodes", 
            "repository", "analysis_settings_clean", "analysis_groups", 
            "trial_outliers_list", "baseline_settings")), deps = c("baselined_power", 
        "requested_electrodes", "repository", "analysis_settings_clean", 
        "analysis_groups", "trial_outliers_list", "baseline_settings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_by_frequency_correlation_data = targets::tar_target_raw(name = "by_frequency_correlation_data", 
        command = quote({
            .__target_expr__. <- quote({
                baselined_power_data <- subset(baselined_power, 
                  Electrode ~ Electrode %in% requested_electrodes)
                epoch_event_types = get_available_events(repository$epoch$columns)
                by_frequency_correlation_data <- do.call(c, sapply(analysis_settings_clean, 
                  function(asc) {
                    collapse_e <- raveio::collapse2(baselined_power_data, 
                      keep = 1:3, method = "mean")
                    dim(collapse_e) = c(dim(baselined_power_data)[1:3], 
                      1)
                    dimnames(collapse_e) <- append(dimnames(baselined_power_data)[1:3], 
                      list(Electrode = "Avg"))
                    re <- shift_baselined_power(baselined_data = collapse_e, 
                      events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                      event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                    all_ag <- sapply(analysis_groups, function(ag) {
                      trials_to_keep <- ag$trials
                      if (!is.null(trial_outliers_list)) {
                        trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                          trial_outliers_list]
                      }
                      ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                        trials_to_keep
                      aw <- as.numeric(dimnames(re$data)$Time) %within% 
                        asc$time
                      tmp = raveio::collapse2(re$data[, aw, ti, 
                        , drop = FALSE], keep = c(3, 1), method = "mean")
                      ag$data = cor(tmp)
                      ag$y = as.numeric(dimnames(re$data)$Frequency)
                      ag$x = ag$y
                      ag$xlab = "Frequency"
                      ag$ylab = "Frequency"
                      ag$zlab = "Pearson correlation, Trial-level response"
                      ag$range = c(-1, 1)
                      ag$condition_group = ag$label
                      ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                      ag$outliers = trial_outliers_list
                      ag$events = subset(re$events, Trial %in% 
                        ag$trials)
                      ag %<>% add_analysis_settings(asc, baseline_settings)
                      ag
                    }, simplify = FALSE)
                    return(all_ag)
                  }, simplify = FALSE, USE.NAMES = TRUE))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_frequency_correlation_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "by_frequency_correlation_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "by_frequency_correlation_data", 
            target_expr = quote({
                {
                  baselined_power_data <- subset(baselined_power, 
                    Electrode ~ Electrode %in% requested_electrodes)
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  by_frequency_correlation_data <- do.call(c, 
                    sapply(analysis_settings_clean, function(asc) {
                      collapse_e <- raveio::collapse2(baselined_power_data, 
                        keep = 1:3, method = "mean")
                      dim(collapse_e) = c(dim(baselined_power_data)[1:3], 
                        1)
                      dimnames(collapse_e) <- append(dimnames(baselined_power_data)[1:3], 
                        list(Electrode = "Avg"))
                      re <- shift_baselined_power(baselined_data = collapse_e, 
                        events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                        event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                      all_ag <- sapply(analysis_groups, function(ag) {
                        trials_to_keep <- ag$trials
                        if (!is.null(trial_outliers_list)) {
                          trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                            trial_outliers_list]
                        }
                        ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                          trials_to_keep
                        aw <- as.numeric(dimnames(re$data)$Time) %within% 
                          asc$time
                        tmp = raveio::collapse2(re$data[, aw, 
                          ti, , drop = FALSE], keep = c(3, 1), 
                          method = "mean")
                        ag$data = cor(tmp)
                        ag$y = as.numeric(dimnames(re$data)$Frequency)
                        ag$x = ag$y
                        ag$xlab = "Frequency"
                        ag$ylab = "Frequency"
                        ag$zlab = "Pearson correlation, Trial-level response"
                        ag$range = c(-1, 1)
                        ag$condition_group = ag$label
                        ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                        ag$outliers = trial_outliers_list
                        ag$events = subset(re$events, Trial %in% 
                          ag$trials)
                        ag %<>% add_analysis_settings(asc, baseline_settings)
                        ag
                      }, simplify = FALSE)
                      return(all_ag)
                    }, simplify = FALSE, USE.NAMES = TRUE))
                }
                by_frequency_correlation_data
            }), target_depends = c("baselined_power", "requested_electrodes", 
            "repository", "analysis_settings_clean", "analysis_groups", 
            "trial_outliers_list", "baseline_settings")), deps = c("baselined_power", 
        "requested_electrodes", "repository", "analysis_settings_clean", 
        "analysis_groups", "trial_outliers_list", "baseline_settings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_by_trial_tf_data = targets::tar_target_raw(name = "by_trial_tf_data", 
        command = quote({
            .__target_expr__. <- quote({
                by_trial_tf_data = NULL
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_trial_tf_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "by_trial_tf_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "by_trial_tf_data", target_expr = quote({
                {
                  by_trial_tf_data = NULL
                }
                by_trial_tf_data
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    build_over_time_by_electrode_data = targets::tar_target_raw(name = "over_time_by_electrode_data", 
        command = quote({
            .__target_expr__. <- quote({
                baselined_power_data <- subset(baselined_power, 
                  Electrode ~ Electrode %in% requested_electrodes)
                epoch_event_types = get_available_events(repository$epoch$columns)
                over_time_by_electrode_data <- do.call(c, sapply(analysis_settings_clean, 
                  function(asc) {
                    fi <- as.integer(dimnames(baselined_power_data)$Frequency) %within% 
                      asc$frequency
                    collapse_f <- raveio::collapse2(baselined_power_data[fi, 
                      , , , drop = FALSE], keep = 2:4, method = "mean")
                    dim(collapse_f) = c(1, dim(baselined_power_data)[2:4])
                    dimnames(collapse_f) <- append(list(Frequency = "Avg"), 
                      dimnames(baselined_power_data)[2:4])
                    re <- shift_baselined_power(baselined_data = collapse_f, 
                      events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                      event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                    all_ag <- sapply(analysis_groups, function(ag) {
                      trials_to_keep <- ag$trials
                      if (!is.null(trial_outliers_list)) {
                        trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                          trial_outliers_list]
                      }
                      ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                        trials_to_keep
                      ag$data = raveio::collapse2(re$data[, , 
                        ti, , drop = FALSE], keep = c(2, 4), 
                        method = "mean")
                      ag$x = as.numeric(dimnames(re$data)$Time)
                      ag$y = as.numeric(dimnames(re$data)$Electrode)
                      ag$xlab = "Time (s)"
                      ag$ylab = "Electrode"
                      ag$zlab = "Mean " %&% baseline_settings$unit_of_analysis
                      ag$N <- sum(ti)
                      ag$condition_group = ag$label
                      ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                      ag$outliers = trial_outliers_list
                      ag$events = subset(re$events, Trial %in% 
                        ag$trials)
                      ag %<>% add_analysis_settings(asc, baseline_settings)
                      ag$range = range(ag$data)
                      ag
                    }, simplify = FALSE)
                    return(all_ag)
                  }, simplify = FALSE, USE.NAMES = TRUE))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_electrode_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "over_time_by_electrode_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "over_time_by_electrode_data", target_expr = quote({
                {
                  baselined_power_data <- subset(baselined_power, 
                    Electrode ~ Electrode %in% requested_electrodes)
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  over_time_by_electrode_data <- do.call(c, sapply(analysis_settings_clean, 
                    function(asc) {
                      fi <- as.integer(dimnames(baselined_power_data)$Frequency) %within% 
                        asc$frequency
                      collapse_f <- raveio::collapse2(baselined_power_data[fi, 
                        , , , drop = FALSE], keep = 2:4, method = "mean")
                      dim(collapse_f) = c(1, dim(baselined_power_data)[2:4])
                      dimnames(collapse_f) <- append(list(Frequency = "Avg"), 
                        dimnames(baselined_power_data)[2:4])
                      re <- shift_baselined_power(baselined_data = collapse_f, 
                        events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                        event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                      all_ag <- sapply(analysis_groups, function(ag) {
                        trials_to_keep <- ag$trials
                        if (!is.null(trial_outliers_list)) {
                          trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                            trial_outliers_list]
                        }
                        ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                          trials_to_keep
                        ag$data = raveio::collapse2(re$data[, 
                          , ti, , drop = FALSE], keep = c(2, 
                          4), method = "mean")
                        ag$x = as.numeric(dimnames(re$data)$Time)
                        ag$y = as.numeric(dimnames(re$data)$Electrode)
                        ag$xlab = "Time (s)"
                        ag$ylab = "Electrode"
                        ag$zlab = "Mean " %&% baseline_settings$unit_of_analysis
                        ag$N <- sum(ti)
                        ag$condition_group = ag$label
                        ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                        ag$outliers = trial_outliers_list
                        ag$events = subset(re$events, Trial %in% 
                          ag$trials)
                        ag %<>% add_analysis_settings(asc, baseline_settings)
                        ag$range = range(ag$data)
                        ag
                      }, simplify = FALSE)
                      return(all_ag)
                    }, simplify = FALSE, USE.NAMES = TRUE))
                }
                over_time_by_electrode_data
            }), target_depends = c("baselined_power", "requested_electrodes", 
            "repository", "analysis_settings_clean", "analysis_groups", 
            "trial_outliers_list", "baseline_settings")), deps = c("baselined_power", 
        "requested_electrodes", "repository", "analysis_settings_clean", 
        "analysis_groups", "trial_outliers_list", "baseline_settings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_over_time_by_electrode_similiarity_data = targets::tar_target_raw(name = "by_electrode_similarity_data", 
        command = quote({
            .__target_expr__. <- quote({
                curr_electrodes <- data.frame(NA)
                if (length(over_time_by_electrode_data) > 0) {
                  curr_electrodes <- subset(repository$electrode_table, 
                    Electrode %in% over_time_by_electrode_data[[1]]$electrodes)
                }
                if (nrow(curr_electrodes) < 3) {
                  by_electrode_similarity_data = NULL
                } else {
                  rownames(curr_electrodes) = curr_electrodes$Electrode
                  all_distances <- lapply(over_time_by_electrode_data, 
                    function(dd) {
                      etbl <- curr_electrodes[as.character(dd$electrodes), 
                        ]
                      correlation_distance <- as.dist(1 - cor(dd$data))
                      spearman_distance <- as.dist(1 - cor(dd$data, 
                        method = "spearman"))
                      euclidean_distance <- dist(t(dd$data), 
                        method = "euclidean")
                      coordinate_distance <- dist(etbl[, c("Coord_x", 
                        "Coord_y", "Coord_z")])
                      fslabel = "FSLabel"
                      if (is.null(etbl[[fslabel]])) {
                        if (!is.null(etbl["Area_fs"])) {
                          etbl[[fslabel]] = etbl$Area_fs
                        }
                      }
                      fslabel_distance = NULL
                      if (!is.null(etbl[[fslabel]])) {
                        tmp <- merge(etbl, aggregate(cbind(CX = Coord_x, 
                          CY = Coord_y, CZ = Coord_z) ~ FSLabel, 
                          FUN = mean, data = etbl))
                        fslabel_distance <- dist(tmp[, c("CX", 
                          "CY", "CZ")])
                      }
                      list(correlation = correlation_distance, 
                        spearman = spearman_distance, euclidean = euclidean_distance, 
                        coordinate = coordinate_distance, FSLabel = fslabel_distance)
                    })
                  distance_names <- names(all_distances[[1]])
                  all_dist_mats <- sapply(c("total", "max", "min"), 
                    function(metric) {
                      sapply(distance_names, function(dn) {
                        FUN <- switch(metric, total = `+`, max = pmax, 
                          min = pmin)
                        Reduce(FUN, lapply(all_distances, `[[`, 
                          dn))
                      }, simplify = FALSE)
                    }, simplify = FALSE)
                  by_electrode_similarity_data <- lapply(all_dist_mats, 
                    function(x) {
                      lapply(x, function(xx) {
                        if (is.null(xx) || length(xx) == 0) {
                          return(NULL)
                        }
                        re <- hclust(xx, method = "ward.D2")
                        re$distances <- xx
                        return(re)
                      })
                    })
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_electrode_similarity_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "by_electrode_similarity_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "by_electrode_similarity_data", target_expr = quote({
                {
                  curr_electrodes <- data.frame(NA)
                  if (length(over_time_by_electrode_data) > 0) {
                    curr_electrodes <- subset(repository$electrode_table, 
                      Electrode %in% over_time_by_electrode_data[[1]]$electrodes)
                  }
                  if (nrow(curr_electrodes) < 3) {
                    by_electrode_similarity_data = NULL
                  } else {
                    rownames(curr_electrodes) = curr_electrodes$Electrode
                    all_distances <- lapply(over_time_by_electrode_data, 
                      function(dd) {
                        etbl <- curr_electrodes[as.character(dd$electrodes), 
                          ]
                        correlation_distance <- as.dist(1 - cor(dd$data))
                        spearman_distance <- as.dist(1 - cor(dd$data, 
                          method = "spearman"))
                        euclidean_distance <- dist(t(dd$data), 
                          method = "euclidean")
                        coordinate_distance <- dist(etbl[, c("Coord_x", 
                          "Coord_y", "Coord_z")])
                        fslabel = "FSLabel"
                        if (is.null(etbl[[fslabel]])) {
                          if (!is.null(etbl["Area_fs"])) {
                            etbl[[fslabel]] = etbl$Area_fs
                          }
                        }
                        fslabel_distance = NULL
                        if (!is.null(etbl[[fslabel]])) {
                          tmp <- merge(etbl, aggregate(cbind(CX = Coord_x, 
                            CY = Coord_y, CZ = Coord_z) ~ FSLabel, 
                            FUN = mean, data = etbl))
                          fslabel_distance <- dist(tmp[, c("CX", 
                            "CY", "CZ")])
                        }
                        list(correlation = correlation_distance, 
                          spearman = spearman_distance, euclidean = euclidean_distance, 
                          coordinate = coordinate_distance, FSLabel = fslabel_distance)
                      })
                    distance_names <- names(all_distances[[1]])
                    all_dist_mats <- sapply(c("total", "max", 
                      "min"), function(metric) {
                      sapply(distance_names, function(dn) {
                        FUN <- switch(metric, total = `+`, max = pmax, 
                          min = pmin)
                        Reduce(FUN, lapply(all_distances, `[[`, 
                          dn))
                      }, simplify = FALSE)
                    }, simplify = FALSE)
                    by_electrode_similarity_data <- lapply(all_dist_mats, 
                      function(x) {
                        lapply(x, function(xx) {
                          if (is.null(xx) || length(xx) == 0) {
                            return(NULL)
                          }
                          re <- hclust(xx, method = "ward.D2")
                          re$distances <- xx
                          return(re)
                        })
                      })
                  }
                }
                by_electrode_similarity_data
            }), target_depends = c("over_time_by_electrode_data", 
            "repository")), deps = c("over_time_by_electrode_data", 
        "repository"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_over_time_by_electrode_and_trial_data = targets::tar_target_raw(name = "over_time_by_electrode_and_trial_data", 
        command = quote({
            .__target_expr__. <- quote({
                over_time_by_electrode_and_trial_data <- NULL
                build_data <- function(data, analysis_settings, 
                  condition_group, baseline_settings, ...) {
                  dm <- dimnames(data)
                  to_keep <- sapply(c("Time", "Electrode", "Trial"), 
                    which.equal, names(dm))
                  res <- list(data = ravetools::collapse(data, 
                    keep = to_keep), xlab = "Time (s)", ylab = "Electrode #", 
                    zlab = "Trial", analysis_unit = baseline_settings$unit_of_analysis, 
                    condition_group = condition_group$label, 
                    electrodes = as.integer(dm$Electrode))
                  res[c("x", "y", "z")] <- dimnames(data)[to_keep] %>% 
                    lapply(as.numeric)
                  dimnames(res$data) = dimnames(data)[to_keep]
                  res$N = length(dm$Trial)
                  if (isTRUE(analysis_settings$censor_info$enabled)) {
                    ti = res$x %within% analysis_settings$censor_info$window
                    res$range <- range(res$data[!ti, , ])
                  } else {
                    res$range <- range(res$data)
                  }
                  return(res)
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_electrode_and_trial_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "over_time_by_electrode_and_trial_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "over_time_by_electrode_and_trial_data", 
            target_expr = quote({
                {
                  over_time_by_electrode_and_trial_data <- NULL
                  build_data <- function(data, analysis_settings, 
                    condition_group, baseline_settings, ...) {
                    dm <- dimnames(data)
                    to_keep <- sapply(c("Time", "Electrode", 
                      "Trial"), which.equal, names(dm))
                    res <- list(data = ravetools::collapse(data, 
                      keep = to_keep), xlab = "Time (s)", ylab = "Electrode #", 
                      zlab = "Trial", analysis_unit = baseline_settings$unit_of_analysis, 
                      condition_group = condition_group$label, 
                      electrodes = as.integer(dm$Electrode))
                    res[c("x", "y", "z")] <- dimnames(data)[to_keep] %>% 
                      lapply(as.numeric)
                    dimnames(res$data) = dimnames(data)[to_keep]
                    res$N = length(dm$Trial)
                    if (isTRUE(analysis_settings$censor_info$enabled)) {
                      ti = res$x %within% analysis_settings$censor_info$window
                      res$range <- range(res$data[!ti, , ])
                    } else {
                      res$range <- range(res$data)
                    }
                    return(res)
                  }
                }
                over_time_by_electrode_and_trial_data
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    build_over_time_by_condition_data = targets::tar_target_raw(name = "over_time_by_condition_data", 
        command = quote({
            .__target_expr__. <- quote({
                baselined_power_data <- subset(baselined_power, 
                  Electrode ~ Electrode %in% requested_electrodes)
                epoch_event_types = get_available_events(repository$epoch$columns)
                re <- sapply(analysis_settings_clean, function(asc) {
                  fi <- as.integer(dimnames(baselined_power_data)$Frequency) %within% 
                    asc$frequency
                  collapse_f <- raveio::collapse2(baselined_power_data[fi, 
                    , , , drop = FALSE], keep = 2:4, method = "mean")
                  dim(collapse_f) = c(1, dim(baselined_power_data)[2:4])
                  dimnames(collapse_f) <- append(list(Frequency = "Avg"), 
                    dimnames(baselined_power_data)[2:4])
                  re <- shift_baselined_power(baselined_data = collapse_f, 
                    events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                    event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                  all_ag <- sapply(analysis_groups, function(ag) {
                    trials_to_keep <- ag$trials
                    if (!is.null(trial_outliers_list)) {
                      trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                        trial_outliers_list]
                    }
                    ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                      trials_to_keep
                    coll_trial = raveio::collapse2(re$data[, 
                      , ti, , drop = FALSE], keep = c(2, 4), 
                      method = "mean")
                    ag$data <- cbind(.rowMeans(coll_trial, nrow(coll_trial), 
                      ncol(coll_trial)), sqrt(diag(dipsaus::fastcov2(t(coll_trial)))/ncol(coll_trial)))
                    ag$x = as.numeric(dimnames(re$data)$Time)
                    ag$y = NA
                    ag$xlab = "Time (s)"
                    ag$ylab = "Mean " %&% baseline_settings$unit_of_analysis
                    ag$N <- dim(re$data)[4]
                    ag$condition_group = ag$label
                    ag$electrodes <- as.integer(dimnames(re$data)$Electrode)
                    ag$outliers = trial_outliers_list
                    ag$events = subset(re$events, Trial %in% 
                      ag$trials)
                    ag %<>% add_analysis_settings(asc, baseline_settings)
                    ag$range = range(rutabaga::plus_minus(ag$data))
                    ag
                  }, simplify = FALSE)
                  return(all_ag)
                }, simplify = FALSE, USE.NAMES = TRUE)
                over_time_by_condition_data <- vector("list", 
                  length = length(re[[1]]))
                names(over_time_by_condition_data) = names(re[[1]])
                for (ii in names(over_time_by_condition_data)) {
                  for (jj in names(re)) {
                    over_time_by_condition_data[[ii]][[jj]] = re[[jj]][[ii]]
                  }
                }
                for (ii in seq_along(over_time_by_condition_data)) {
                  for (jj in seq_along(over_time_by_condition_data[[ii]])) {
                    over_time_by_condition_data[[ii]][[jj]]$data_label = names(over_time_by_condition_data)[[ii]]
                    over_time_by_condition_data[[ii]][[jj]]$time_window_label = names(over_time_by_condition_data[[ii]])[[jj]]
                  }
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_condition_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "over_time_by_condition_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "over_time_by_condition_data", target_expr = quote({
                {
                  baselined_power_data <- subset(baselined_power, 
                    Electrode ~ Electrode %in% requested_electrodes)
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  re <- sapply(analysis_settings_clean, function(asc) {
                    fi <- as.integer(dimnames(baselined_power_data)$Frequency) %within% 
                      asc$frequency
                    collapse_f <- raveio::collapse2(baselined_power_data[fi, 
                      , , , drop = FALSE], keep = 2:4, method = "mean")
                    dim(collapse_f) = c(1, dim(baselined_power_data)[2:4])
                    dimnames(collapse_f) <- append(list(Frequency = "Avg"), 
                      dimnames(baselined_power_data)[2:4])
                    re <- shift_baselined_power(baselined_data = collapse_f, 
                      events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                      event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                    all_ag <- sapply(analysis_groups, function(ag) {
                      trials_to_keep <- ag$trials
                      if (!is.null(trial_outliers_list)) {
                        trials_to_keep <- trials_to_keep[!trials_to_keep %in% 
                          trial_outliers_list]
                      }
                      ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                        trials_to_keep
                      coll_trial = raveio::collapse2(re$data[, 
                        , ti, , drop = FALSE], keep = c(2, 4), 
                        method = "mean")
                      ag$data <- cbind(.rowMeans(coll_trial, 
                        nrow(coll_trial), ncol(coll_trial)), 
                        sqrt(diag(dipsaus::fastcov2(t(coll_trial)))/ncol(coll_trial)))
                      ag$x = as.numeric(dimnames(re$data)$Time)
                      ag$y = NA
                      ag$xlab = "Time (s)"
                      ag$ylab = "Mean " %&% baseline_settings$unit_of_analysis
                      ag$N <- dim(re$data)[4]
                      ag$condition_group = ag$label
                      ag$electrodes <- as.integer(dimnames(re$data)$Electrode)
                      ag$outliers = trial_outliers_list
                      ag$events = subset(re$events, Trial %in% 
                        ag$trials)
                      ag %<>% add_analysis_settings(asc, baseline_settings)
                      ag$range = range(rutabaga::plus_minus(ag$data))
                      ag
                    }, simplify = FALSE)
                    return(all_ag)
                  }, simplify = FALSE, USE.NAMES = TRUE)
                  over_time_by_condition_data <- vector("list", 
                    length = length(re[[1]]))
                  names(over_time_by_condition_data) = names(re[[1]])
                  for (ii in names(over_time_by_condition_data)) {
                    for (jj in names(re)) {
                      over_time_by_condition_data[[ii]][[jj]] = re[[jj]][[ii]]
                    }
                  }
                  for (ii in seq_along(over_time_by_condition_data)) {
                    for (jj in seq_along(over_time_by_condition_data[[ii]])) {
                      over_time_by_condition_data[[ii]][[jj]]$data_label = names(over_time_by_condition_data)[[ii]]
                      over_time_by_condition_data[[ii]][[jj]]$time_window_label = names(over_time_by_condition_data[[ii]])[[jj]]
                    }
                  }
                }
                over_time_by_condition_data
            }), target_depends = c("baselined_power", "requested_electrodes", 
            "repository", "analysis_settings_clean", "analysis_groups", 
            "trial_outliers_list", "baseline_settings")), deps = c("baselined_power", 
        "requested_electrodes", "repository", "analysis_settings_clean", 
        "analysis_groups", "trial_outliers_list", "baseline_settings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_over_time_by_electrode_dataframe = targets::tar_target_raw(name = "over_time_by_electrode_dataframe", 
        command = quote({
            .__target_expr__. <- quote({
                over_time_by_electrode_dataframe <- NULL
                raveio::power_baseline(repository, baseline_windows = baseline_settings$window, 
                  method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                  units = get_baseline_scope(baseline_settings$scope), 
                  signal_type = "LFP", electrodes = repository$electrode_list)
                by_condition_group <- lapply(analysis_groups, 
                  FUN = function(ag) {
                    ravedash::logger(paste(c("[build_over_time_by_electrode_dataframe] working on:", 
                      ag$label, "data"), collapse = " "))
                    res <- lapply(analysis_settings_clean, function(as) {
                      fi <- repository$frequency %within% as$frequency
                      p <- get_pluriform_power(baselined_data = repository$power$baselined[fi, 
                        , , , drop = FALSE], trial_indices = ag$trials, 
                        events = repository$epoch$table, epoch_event_types = get_available_events(repository$epoch$columns), 
                        trial_outliers_list = unlist(trial_outliers_list), 
                        event_of_interest = as$event, final_data_only = TRUE, 
                        sample_rate = repository$subject$power_sample_rate)
                      stopifnot(names(dimnames(p)) == c("Frequency", 
                        "Time", "Trial", "Electrode"))
                      enames = as.integer(dimnames(p)$Electrode)
                      times = as.numeric(dimnames(p)$Time)
                      m <- ravetools::collapse(p[drop = FALSE], 
                        keep = c(4, 2))
                      df <- data.frame(reshape2::melt(m, value.name = paste(sep = "_", 
                        as$label, ag$label)))
                      names(df)[1:2] = c("Electrode", "Time")
                      df$Electrode = enames[df$Electrode]
                      df$Time = times[df$Time]
                      return(df)
                    })
                    if (length(res) == 1) {
                      return(res[[1]])
                    }
                    merged_res <- res[[1]]
                    for (ri in seq_along(res)[-1]) {
                      merged_res = merge(merged_res, res[[ri]], 
                        all = TRUE)
                    }
                    return(merged_res)
                  })
                over_time_by_electrode_dataframe <- NULL
                if (length(by_condition_group) > 0) {
                  over_time_by_electrode_dataframe <- by_condition_group[[1]]
                  if (length(by_condition_group) > 1) {
                    for (ii in seq_along(by_condition_group)[-1]) {
                      over_time_by_electrode_dataframe = merge(over_time_by_electrode_dataframe, 
                        by_condition_group[[ii]], all = TRUE)
                    }
                  }
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_electrode_dataframe)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "over_time_by_electrode_dataframe", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "over_time_by_electrode_dataframe", 
            target_expr = quote({
                {
                  over_time_by_electrode_dataframe <- NULL
                  raveio::power_baseline(repository, baseline_windows = baseline_settings$window, 
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                    units = get_baseline_scope(baseline_settings$scope), 
                    signal_type = "LFP", electrodes = repository$electrode_list)
                  by_condition_group <- lapply(analysis_groups, 
                    FUN = function(ag) {
                      ravedash::logger(paste(c("[build_over_time_by_electrode_dataframe] working on:", 
                        ag$label, "data"), collapse = " "))
                      res <- lapply(analysis_settings_clean, 
                        function(as) {
                          fi <- repository$frequency %within% 
                            as$frequency
                          p <- get_pluriform_power(baselined_data = repository$power$baselined[fi, 
                            , , , drop = FALSE], trial_indices = ag$trials, 
                            events = repository$epoch$table, 
                            epoch_event_types = get_available_events(repository$epoch$columns), 
                            trial_outliers_list = unlist(trial_outliers_list), 
                            event_of_interest = as$event, final_data_only = TRUE, 
                            sample_rate = repository$subject$power_sample_rate)
                          stopifnot(names(dimnames(p)) == c("Frequency", 
                            "Time", "Trial", "Electrode"))
                          enames = as.integer(dimnames(p)$Electrode)
                          times = as.numeric(dimnames(p)$Time)
                          m <- ravetools::collapse(p[drop = FALSE], 
                            keep = c(4, 2))
                          df <- data.frame(reshape2::melt(m, 
                            value.name = paste(sep = "_", as$label, 
                              ag$label)))
                          names(df)[1:2] = c("Electrode", "Time")
                          df$Electrode = enames[df$Electrode]
                          df$Time = times[df$Time]
                          return(df)
                        })
                      if (length(res) == 1) {
                        return(res[[1]])
                      }
                      merged_res <- res[[1]]
                      for (ri in seq_along(res)[-1]) {
                        merged_res = merge(merged_res, res[[ri]], 
                          all = TRUE)
                      }
                      return(merged_res)
                    })
                  over_time_by_electrode_dataframe <- NULL
                  if (length(by_condition_group) > 0) {
                    over_time_by_electrode_dataframe <- by_condition_group[[1]]
                    if (length(by_condition_group) > 1) {
                      for (ii in seq_along(by_condition_group)[-1]) {
                        over_time_by_electrode_dataframe = merge(over_time_by_electrode_dataframe, 
                          by_condition_group[[ii]], all = TRUE)
                      }
                    }
                  }
                }
                over_time_by_electrode_dataframe
            }), target_depends = c("repository", "baseline_settings", 
            "analysis_groups", "analysis_settings_clean", "trial_outliers_list"
            )), deps = c("repository", "baseline_settings", "analysis_groups", 
        "analysis_settings_clean", "trial_outliers_list"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), build_over_time_by_trial = targets::tar_target_raw(name = "over_time_by_trial_data", 
        command = quote({
            .__target_expr__. <- quote({
                baselined_power_data <- subset(baselined_power, 
                  Electrode ~ Electrode %in% requested_electrodes)
                epoch_event_types = get_available_events(repository$epoch$columns)
                over_time_by_trial_data <- do.call(c, sapply(analysis_settings_clean, 
                  function(asc) {
                    fi <- as.integer(dimnames(baselined_power_data)$Frequency) %within% 
                      asc$frequency
                    collapse_fe <- raveio::collapse2(baselined_power_data[fi, 
                      , , , drop = FALSE], keep = 2:3, method = "mean")
                    dim(collapse_fe) = c(1, dim(baselined_power_data)[2:3], 
                      1)
                    dimnames(collapse_fe) <- append(append(list(Frequency = "Avg"), 
                      dimnames(baselined_power_data)[2:3]), list(Electrode = "Avg"))
                    re <- shift_baselined_power(baselined_data = collapse_fe, 
                      events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                      event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                    all_ag <- sapply(analysis_groups, function(ag) {
                      ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                        ag$trials
                      ag$data = re$data[, , ti, , drop = TRUE]
                      if (!is.matrix(ag$data)) {
                        dim(ag$data) = c(length(ag$data), 1)
                      }
                      ag$x = as.numeric(dimnames(re$data)$Time)
                      ag$xlab = "Time (s)"
                      ag$ylab = "Trial (sorted by condition)"
                      ag$zlab = sprintf("Mean %s", baseline_settings$unit_of_analysis)
                      ag$range <- range(ag$data)
                      cnds <- ag$conditions
                      tt <- ag$trials
                      ag$y <- trial_details[as.character(tt), 
                        condition_variable]
                      cf <- factor(ag$y, levels = cnds)
                      ord = order(cf, tt)
                      ag$y <- ag$y[ord]
                      ag$trial_number = tt[ord]
                      ag$is_outlier = tt[ord] %in% trial_outliers_list
                      ag$data <- ag$data[, ord, drop = FALSE]
                      ag$N <- dim(baselined_power_data)[4]
                      ag$condition_group = ag$label
                      ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                      ag$outliers = trial_outliers_list
                      ag$events = subset(re$events, Trial %in% 
                        ag$trials)
                      ag %<>% add_analysis_settings(asc, baseline_settings)
                      ag
                    }, simplify = FALSE)
                    return(all_ag)
                  }, simplify = FALSE, USE.NAMES = TRUE))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_trial_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "over_time_by_trial_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "over_time_by_trial_data", target_expr = quote({
                {
                  baselined_power_data <- subset(baselined_power, 
                    Electrode ~ Electrode %in% requested_electrodes)
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  over_time_by_trial_data <- do.call(c, sapply(analysis_settings_clean, 
                    function(asc) {
                      fi <- as.integer(dimnames(baselined_power_data)$Frequency) %within% 
                        asc$frequency
                      collapse_fe <- raveio::collapse2(baselined_power_data[fi, 
                        , , , drop = FALSE], keep = 2:3, method = "mean")
                      dim(collapse_fe) = c(1, dim(baselined_power_data)[2:3], 
                        1)
                      dimnames(collapse_fe) <- append(append(list(Frequency = "Avg"), 
                        dimnames(baselined_power_data)[2:3]), 
                        list(Electrode = "Avg"))
                      re <- shift_baselined_power(baselined_data = collapse_fe, 
                        events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                        event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate)
                      all_ag <- sapply(analysis_groups, function(ag) {
                        ti <- as.numeric(dimnames(re$data)$Trial) %in% 
                          ag$trials
                        ag$data = re$data[, , ti, , drop = TRUE]
                        if (!is.matrix(ag$data)) {
                          dim(ag$data) = c(length(ag$data), 1)
                        }
                        ag$x = as.numeric(dimnames(re$data)$Time)
                        ag$xlab = "Time (s)"
                        ag$ylab = "Trial (sorted by condition)"
                        ag$zlab = sprintf("Mean %s", baseline_settings$unit_of_analysis)
                        ag$range <- range(ag$data)
                        cnds <- ag$conditions
                        tt <- ag$trials
                        ag$y <- trial_details[as.character(tt), 
                          condition_variable]
                        cf <- factor(ag$y, levels = cnds)
                        ord = order(cf, tt)
                        ag$y <- ag$y[ord]
                        ag$trial_number = tt[ord]
                        ag$is_outlier = tt[ord] %in% trial_outliers_list
                        ag$data <- ag$data[, ord, drop = FALSE]
                        ag$N <- dim(baselined_power_data)[4]
                        ag$condition_group = ag$label
                        ag$electrodes <- as.integer(dimnames(baselined_power_data)$Electrode)
                        ag$outliers = trial_outliers_list
                        ag$events = subset(re$events, Trial %in% 
                          ag$trials)
                        ag %<>% add_analysis_settings(asc, baseline_settings)
                        ag
                      }, simplify = FALSE)
                      return(all_ag)
                    }, simplify = FALSE, USE.NAMES = TRUE))
                }
                over_time_by_trial_data
            }), target_depends = c("baselined_power", "requested_electrodes", 
            "repository", "analysis_settings_clean", "analysis_groups", 
            "baseline_settings", "trial_details", "condition_variable", 
            "trial_outliers_list")), deps = c("baselined_power", 
        "requested_electrodes", "repository", "analysis_settings_clean", 
        "analysis_groups", "baseline_settings", "trial_details", 
        "condition_variable", "trial_outliers_list"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), build_internal_omnibus_results = targets::tar_target_raw(name = "internal_omnibus_results", 
        command = quote({
            .__target_expr__. <- quote({
                ravedash::logger("top of IOR", level = "debug")
                if (isTRUE(omnibus_includes_all_electrodes)) {
                  raveio::power_baseline(repository, baseline_windows = baseline_settings$window, 
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                    units = get_baseline_scope(baseline_settings$scope), 
                    signal_type = "LFP", electrodes = repository$electrode_list)
                  ob_baselined_power <- repository$power$baselined
                } else {
                  ob_baselined_power <- subset(baselined_power, 
                    Electrode ~ Electrode %in% requested_electrodes)
                }
                epoch_event_types = get_available_events(repository$epoch$columns)
                by_condition_group <- lapply(analysis_groups, 
                  function(ag) {
                    lapply(analysis_settings_clean, function(as) {
                      fi <- as.integer(dimnames(ob_baselined_power)$Frequency) %within% 
                        as$frequency
                      trials <- as.integer(dimnames(ob_baselined_power)$Trial) %in% 
                        ag$trials
                      collapse_f <- raveio::collapse2(ob_baselined_power[fi, 
                        , trials, , drop = FALSE], keep = 2:4, 
                        method = "mean")
                      dn <- dimnames(ob_baselined_power)[-1]
                      dn$Trial <- dn$Trial[trials]
                      dim(collapse_f) = c(1, dim(collapse_f))
                      dimnames(collapse_f) <- append(list(Frequency = "Avg"), 
                        dn)
                      re <- shift_baselined_power(baselined_data = collapse_f, 
                        events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                        event_of_interest = as$event, sample_rate = repository$subject$power_sample_rate)
                      p <- re$data
                      ti = as.numeric(dimnames(p)$Time) %within% 
                        as$time
                      stopifnot(names(dimnames(p))[2] == "Time")
                      m <- ravetools::collapse(p[, ti, , , drop = FALSE], 
                        keep = 3:4)
                      mse <- apply(m, 2, rutabaga::m_se)
                      ts = mse[1, ]/mse[2, ]
                      collapsed <- cbind(mse[1, ], ts, 2 * pt(abs(ts), 
                        df = nrow(m) - 1, lower.tail = F))
                      enames = dimnames(p)$Electrode
                      rownames(collapsed) = enames
                      colnames(collapsed) = paste0(c("m", "t", 
                        "p"), "(", ag$label, "; ", as$label, 
                        ")")
                      trial_column <- rep(dimnames(p)$Trial, 
                        times = ncol(m))
                      by_trial <- data.frame(y = c(m), Electrode = rep(as.numeric(enames), 
                        each = nrow(m)), Trial = trial_column, 
                        is_clean = !(trial_column %in% trial_outliers_list), 
                        Factor1 = ag$label, Time = "t" %&% str_collapse(as$time, 
                          "-"), Freq = "f" %&% str_collapse(as$frequency, 
                          "-"), Event = as$event, AnalysisLabel = as$label)
                      return(list(df = by_trial, collapsed = collapsed))
                    })
                  })
                all_data <- rutabaga::rbind_list(sapply(by_condition_group, 
                  rutabaga::get_list_elements, "df", use_sapply = FALSE))
                if (isTRUE(enable_second_condition_groupings)) {
                  meta_table <- attr(analysis_groups, "meta")
                  stopifnot(is.data.frame(meta_table))
                  all_data$Factor1 = NULL
                  all_data %<>% merge(meta_table, by = c("Trial"))
                  all_data$Factor1Factor2 = mapply(paste, all_data$Factor1, 
                    all_data$Factor2, sep = ".")
                  all_data$Factor1Factor2 %<>% factor(levels = names(analysis_groups))
                } else {
                  if (!is.factor(all_data$Factor1)) {
                    all_data$Factor1 %<>% factor(levels = names(by_condition_group))
                  }
                }
                if (!is.null(all_data$AnalysisLabel)) {
                  all_data$AnalysisLabel %<>% factor(levels = names(analysis_settings_clean))
                }
                all_data_clean <- all_data
                if (!is.null(all_data[["is_clean"]])) {
                  all_data_clean <- subset(all_data, is_clean)
                }
                get_factor_length <- function(x) length(unique(all_data_clean[[x]]))
                repeated_factors <- "AnalysisLabel"
                unrepeated_factors <- c("Factor1", "Factor2")
                factor_lengths <- sapply(c(repeated_factors, 
                  unrepeated_factors), get_factor_length)
                fixed_effects <- names(factor_lengths[factor_lengths > 
                  1])
                formula_str <- paste0("y ~ ", str_collapse(fixed_effects, 
                  "*"))
                if (formula_str == "y ~ ") formula_str = "y ~ 1"
                has_re <- any(repeated_factors %in% fixed_effects)
                stat_fun <- stats::lm
                if (has_re) {
                  formula_str %<>% paste("+ (1|Trial)")
                  stat_fun <- lmerTest::lmer
                }
                formula_frm = as.formula(formula_str)
                run_stats <- function(el) {
                  mod <- stat_fun(formula_frm, data = el)
                  if (length(coef(mod)) == 1 && class(mod) != 
                    "lmerModLmerTest") {
                    lsm <- emmeans::emmeans(mod, specs = "1")
                    summ <- summary(lsm, infer = TRUE)
                    emm = matrix(unlist(t(summ[c("emmean", "t.ratio", 
                      "p.value")])))
                    lbls <- as.character(summ[[1]])
                    rownames(emm) = c(outer(c("m(", "t(", "p("), 
                      lbls, paste0)) %&% ")"
                    res <- emm
                  } else {
                    lsm <- emmeans::emmeans(mod, as.formula("pairwise ~" %&% 
                      str_collapse(fixed_effects, "*")))
                    summ <- summary(lsm$emmeans, infer = TRUE)
                    emm = matrix(unlist(t(summ[c("emmean", "t.ratio", 
                      "p.value")])))
                    lbls <- apply(summ[, fixed_effects, drop = FALSE], 
                      1, str_collapse, by = " ")
                    rownames(emm) = c(outer(c("m(", "t(", "p("), 
                      lbls, paste0)) %&% ")"
                    cntr = summary(lsm, adjust = "fdr")$contrasts
                    cmat = matrix(unlist(t(cntr[, c("estimate", 
                      "t.ratio", "p.value")])))
                    rownames(cmat) = c(t(sapply(c("m(", "t(", 
                      "p_fdr("), paste0, cntr$contrast))) %&% 
                      ")"
                    tmp <- summary(emmeans::emmeans(mod, specs = "1"), 
                      infer = TRUE)
                    tmp.emm = matrix(unlist(t(tmp[c("emmean", 
                      "t.ratio", "p.value")])))
                    tmp.lbls <- as.character(tmp[[1]])
                    rownames(tmp.emm) = c(outer(c("m(", "t(", 
                      "p("), tmp.lbls, paste0)) %&% ")"
                    res <- rbind(tmp.emm, emm, cmat)
                  }
                  colnames(res) = el$Electrode[1]
                  return(res)
                }
                ravedash::logger("starting pes", level = "debug")
                stats <- NULL
                if (length(all_data_clean) > 1) {
                  stats <- all_data_clean %>% split((.)$Electrode) %>% 
                    raveio::lapply_async(function(el) {
                      if (length(el$y) < 2 || var(el$y) < 1e-12) {
                        return(NULL)
                      }
                      run_stats(el)
                    }) %>% rutabaga::cbind_list()
                }
                ravedash::logger("done pes", level = "debug")
                if (!is.null(stats)) {
                  attr(stats, "electrode_labels") = repository$electrode_table$Label
                }
                all_data %<>% merge(repository$epoch$table[, 
                  c("Block", "Trial")], sort = FALSE)
                internal_omnibus_results = list(data_with_outliers = all_data, 
                  data = all_data_clean, stats = stats)
                ravedash::logger("done with IOR", level = "debug")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(internal_omnibus_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "internal_omnibus_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "internal_omnibus_results", target_expr = quote({
                {
                  ravedash::logger("top of IOR", level = "debug")
                  if (isTRUE(omnibus_includes_all_electrodes)) {
                    raveio::power_baseline(repository, baseline_windows = baseline_settings$window, 
                      method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                      units = get_baseline_scope(baseline_settings$scope), 
                      signal_type = "LFP", electrodes = repository$electrode_list)
                    ob_baselined_power <- repository$power$baselined
                  } else {
                    ob_baselined_power <- subset(baselined_power, 
                      Electrode ~ Electrode %in% requested_electrodes)
                  }
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  by_condition_group <- lapply(analysis_groups, 
                    function(ag) {
                      lapply(analysis_settings_clean, function(as) {
                        fi <- as.integer(dimnames(ob_baselined_power)$Frequency) %within% 
                          as$frequency
                        trials <- as.integer(dimnames(ob_baselined_power)$Trial) %in% 
                          ag$trials
                        collapse_f <- raveio::collapse2(ob_baselined_power[fi, 
                          , trials, , drop = FALSE], keep = 2:4, 
                          method = "mean")
                        dn <- dimnames(ob_baselined_power)[-1]
                        dn$Trial <- dn$Trial[trials]
                        dim(collapse_f) = c(1, dim(collapse_f))
                        dimnames(collapse_f) <- append(list(Frequency = "Avg"), 
                          dn)
                        re <- shift_baselined_power(baselined_data = collapse_f, 
                          events = repository$epoch$table, epoch_event_types = epoch_event_types, 
                          event_of_interest = as$event, sample_rate = repository$subject$power_sample_rate)
                        p <- re$data
                        ti = as.numeric(dimnames(p)$Time) %within% 
                          as$time
                        stopifnot(names(dimnames(p))[2] == "Time")
                        m <- ravetools::collapse(p[, ti, , , 
                          drop = FALSE], keep = 3:4)
                        mse <- apply(m, 2, rutabaga::m_se)
                        ts = mse[1, ]/mse[2, ]
                        collapsed <- cbind(mse[1, ], ts, 2 * 
                          pt(abs(ts), df = nrow(m) - 1, lower.tail = F))
                        enames = dimnames(p)$Electrode
                        rownames(collapsed) = enames
                        colnames(collapsed) = paste0(c("m", "t", 
                          "p"), "(", ag$label, "; ", as$label, 
                          ")")
                        trial_column <- rep(dimnames(p)$Trial, 
                          times = ncol(m))
                        by_trial <- data.frame(y = c(m), Electrode = rep(as.numeric(enames), 
                          each = nrow(m)), Trial = trial_column, 
                          is_clean = !(trial_column %in% trial_outliers_list), 
                          Factor1 = ag$label, Time = "t" %&% 
                            str_collapse(as$time, "-"), Freq = "f" %&% 
                            str_collapse(as$frequency, "-"), 
                          Event = as$event, AnalysisLabel = as$label)
                        return(list(df = by_trial, collapsed = collapsed))
                      })
                    })
                  all_data <- rutabaga::rbind_list(sapply(by_condition_group, 
                    rutabaga::get_list_elements, "df", use_sapply = FALSE))
                  if (isTRUE(enable_second_condition_groupings)) {
                    meta_table <- attr(analysis_groups, "meta")
                    stopifnot(is.data.frame(meta_table))
                    all_data$Factor1 = NULL
                    all_data %<>% merge(meta_table, by = c("Trial"))
                    all_data$Factor1Factor2 = mapply(paste, all_data$Factor1, 
                      all_data$Factor2, sep = ".")
                    all_data$Factor1Factor2 %<>% factor(levels = names(analysis_groups))
                  } else {
                    if (!is.factor(all_data$Factor1)) {
                      all_data$Factor1 %<>% factor(levels = names(by_condition_group))
                    }
                  }
                  if (!is.null(all_data$AnalysisLabel)) {
                    all_data$AnalysisLabel %<>% factor(levels = names(analysis_settings_clean))
                  }
                  all_data_clean <- all_data
                  if (!is.null(all_data[["is_clean"]])) {
                    all_data_clean <- subset(all_data, is_clean)
                  }
                  get_factor_length <- function(x) length(unique(all_data_clean[[x]]))
                  repeated_factors <- "AnalysisLabel"
                  unrepeated_factors <- c("Factor1", "Factor2")
                  factor_lengths <- sapply(c(repeated_factors, 
                    unrepeated_factors), get_factor_length)
                  fixed_effects <- names(factor_lengths[factor_lengths > 
                    1])
                  formula_str <- paste0("y ~ ", str_collapse(fixed_effects, 
                    "*"))
                  if (formula_str == "y ~ ") formula_str = "y ~ 1"
                  has_re <- any(repeated_factors %in% fixed_effects)
                  stat_fun <- stats::lm
                  if (has_re) {
                    formula_str %<>% paste("+ (1|Trial)")
                    stat_fun <- lmerTest::lmer
                  }
                  formula_frm = as.formula(formula_str)
                  run_stats <- function(el) {
                    mod <- stat_fun(formula_frm, data = el)
                    if (length(coef(mod)) == 1 && class(mod) != 
                      "lmerModLmerTest") {
                      lsm <- emmeans::emmeans(mod, specs = "1")
                      summ <- summary(lsm, infer = TRUE)
                      emm = matrix(unlist(t(summ[c("emmean", 
                        "t.ratio", "p.value")])))
                      lbls <- as.character(summ[[1]])
                      rownames(emm) = c(outer(c("m(", "t(", "p("), 
                        lbls, paste0)) %&% ")"
                      res <- emm
                    } else {
                      lsm <- emmeans::emmeans(mod, as.formula("pairwise ~" %&% 
                        str_collapse(fixed_effects, "*")))
                      summ <- summary(lsm$emmeans, infer = TRUE)
                      emm = matrix(unlist(t(summ[c("emmean", 
                        "t.ratio", "p.value")])))
                      lbls <- apply(summ[, fixed_effects, drop = FALSE], 
                        1, str_collapse, by = " ")
                      rownames(emm) = c(outer(c("m(", "t(", "p("), 
                        lbls, paste0)) %&% ")"
                      cntr = summary(lsm, adjust = "fdr")$contrasts
                      cmat = matrix(unlist(t(cntr[, c("estimate", 
                        "t.ratio", "p.value")])))
                      rownames(cmat) = c(t(sapply(c("m(", "t(", 
                        "p_fdr("), paste0, cntr$contrast))) %&% 
                        ")"
                      tmp <- summary(emmeans::emmeans(mod, specs = "1"), 
                        infer = TRUE)
                      tmp.emm = matrix(unlist(t(tmp[c("emmean", 
                        "t.ratio", "p.value")])))
                      tmp.lbls <- as.character(tmp[[1]])
                      rownames(tmp.emm) = c(outer(c("m(", "t(", 
                        "p("), tmp.lbls, paste0)) %&% ")"
                      res <- rbind(tmp.emm, emm, cmat)
                    }
                    colnames(res) = el$Electrode[1]
                    return(res)
                  }
                  ravedash::logger("starting pes", level = "debug")
                  stats <- NULL
                  if (length(all_data_clean) > 1) {
                    stats <- all_data_clean %>% split((.)$Electrode) %>% 
                      raveio::lapply_async(function(el) {
                        if (length(el$y) < 2 || var(el$y) < 1e-12) {
                          return(NULL)
                        }
                        run_stats(el)
                      }) %>% rutabaga::cbind_list()
                  }
                  ravedash::logger("done pes", level = "debug")
                  if (!is.null(stats)) {
                    attr(stats, "electrode_labels") = repository$electrode_table$Label
                  }
                  all_data %<>% merge(repository$epoch$table[, 
                    c("Block", "Trial")], sort = FALSE)
                  internal_omnibus_results = list(data_with_outliers = all_data, 
                    data = all_data_clean, stats = stats)
                  ravedash::logger("done with IOR", level = "debug")
                }
                internal_omnibus_results
            }), target_depends = c("omnibus_includes_all_electrodes", 
            "repository", "baseline_settings", "baselined_power", 
            "requested_electrodes", "analysis_groups", "analysis_settings_clean", 
            "trial_outliers_list", "enable_second_condition_groupings"
            )), deps = c("omnibus_includes_all_electrodes", "repository", 
        "baseline_settings", "baselined_power", "requested_electrodes", 
        "analysis_groups", "analysis_settings_clean", "trial_outliers_list", 
        "enable_second_condition_groupings"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), build_omnibus_results = targets::tar_target_raw(name = "omnibus_results", 
        command = quote({
            .__target_expr__. <- quote({
                omnibus_results = internal_omnibus_results
                rn <- "currently_selected"
                while (rn %in% names(omnibus_results$data)) {
                  rn <- "PWR_EXPLR_" %&% rn
                }
                omnibus_results$data[[rn]] = omnibus_results$data$Electrode %in% 
                  requested_electrodes
                omnibus_results$data_with_outliers[[rn]] = omnibus_results$data_with_outliers$Electrode %in% 
                  requested_electrodes
                rn <- "currently_selected"
                while (rn %in% rownames(omnibus_results$stats)) {
                  rn = "RAVE_" %&% rn
                }
                val = matrix(nrow = 1, as.integer(colnames(omnibus_results$stats) %in% 
                  as.character(requested_electrodes)), dimnames = list(rn))
                omnibus_results$stats %<>% rbind(val)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(omnibus_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "omnibus_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "omnibus_results", target_expr = quote({
                {
                  omnibus_results = internal_omnibus_results
                  rn <- "currently_selected"
                  while (rn %in% names(omnibus_results$data)) {
                    rn <- "PWR_EXPLR_" %&% rn
                  }
                  omnibus_results$data[[rn]] = omnibus_results$data$Electrode %in% 
                    requested_electrodes
                  omnibus_results$data_with_outliers[[rn]] = omnibus_results$data_with_outliers$Electrode %in% 
                    requested_electrodes
                  rn <- "currently_selected"
                  while (rn %in% rownames(omnibus_results$stats)) {
                    rn = "RAVE_" %&% rn
                  }
                  val = matrix(nrow = 1, as.integer(colnames(omnibus_results$stats) %in% 
                    as.character(requested_electrodes)), dimnames = list(rn))
                  omnibus_results$stats %<>% rbind(val)
                }
                omnibus_results
            }), target_depends = c("internal_omnibus_results", 
            "requested_electrodes")), deps = c("internal_omnibus_results", 
        "requested_electrodes"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), build_across_electrode_statistics = targets::tar_target_raw(name = "across_electrode_statistics", 
        command = quote({
            .__target_expr__. <- quote({
                require(data.table)
                .datatable.aware = TRUE
                across_electrode_statistics <- NULL
                if (length(omnibus_results$data) > 3) {
                  dd <- subset(omnibus_results$data, currently_selected)
                  emmeans::emm_options(lmer.df = "satterthwaite")
                  ravedash::logger("top of BAES", calc_delta = TRUE)
                  rand_effects <- c("Block", "Electrode") %>% 
                    intersect(names(dd))
                  fixed_effects <- c("Factor1", "Factor2", "AnalysisLabel") %>% 
                    intersect(names(dd))
                  for (ff in c(rand_effects, fixed_effects)) {
                    dd[[ff]] %<>% as.factor
                  }
                  fe <- names(which(sapply(dd[fixed_effects], 
                    nlevels) > 1))
                  re <- names(which(sapply(dd[rand_effects], 
                    nlevels) > 1))
                  if ("AnalysisLabel" %in% fe) {
                    if ("Block" %in% re) {
                      if ("Electrode" %in% re) {
                        re[which(re == "Block")] = "Block/Trial"
                      }
                    } else {
                      re %<>% c("Trial")
                    }
                  }
                  re_str = NULL
                  if (length(re) > 0) {
                    re_str <- paste(collapse = "+", sapply(re, 
                      function(x) sprintf("(1|%s)", x)))
                  }
                  dt <- data.table::as.data.table(dd)
                  condition_means <- dt[, list(y = mean(y), sd = sd(y), 
                    se = rutabaga:::se(y), n = .N), keyby = fe]
                  ravedash::logger("got condition means", calc_delta = TRUE)
                  fe <- paste0(fe, collapse = "*")
                  if (!nzchar(fe)) {
                    fe <- "1"
                  }
                  frm <- as.formula(paste("y ~", paste(c(fe, 
                    re_str), collapse = " + ")))
                  if (length(unique(dd$Trial)) < 2) {
                    mod <- NULL
                    .aov <- NULL
                    em <- NULL
                    pairwise <- NULL
                  } else {
                    FUN <- ifelse(is.null(re_str), stats::lm, 
                      lme4::lmer)
                    mod <- do.call(FUN, list(formula = frm, data = dd))
                    .aov <- car::Anova(mod, type = ifelse(fe[1] == 
                      "1", "III", "II"))
                    ravedash::logger("built linear model, starting post hoc tests", 
                      calc_delta = TRUE)
                    em <- emmeans::emmeans(mod, as.formula(sprintf(" ~ %s", 
                      fe)), infer = c(F, T))
                    ravedash::logger("Got emm", calc_delta = TRUE)
                    pairwise <- emmeans::contrast(em, "pairwise")
                    ravedash::logger("Got pairwise contrasts", 
                      calc_delta = TRUE)
                    ravedash::logger("Got ANOVA", calc_delta = TRUE)
                  }
                  stratified_contrasts <- NULL
                  if (length(fe) > 1) {
                    stratified_contrasts <- get_stratified_contrasts(em)
                  }
                  itx_contrasts <- NULL
                  if (length(fe) == 2) {
                    nm <- paste0(fe, collapse = "_")
                    itx_contrasts <- list(emmeans::contrast(em, 
                      interaction = c("pairwise", "pairwise"))) %>% 
                      setNames(nm)
                  }
                  if (length(fe) > 2) {
                    fe_combn <- combn(fe, 2, simplify = FALSE)
                    itx_contrasts <- sapply(fe_combn, function(groups) {
                      emmeans::contrast(em, interaction = c("pairwise", 
                        "pairwise"), by = fe[!fe %in% groups])
                    }) %>% setNames(sapply(fe_combn, paste0, 
                      collapse = "."))
                  }
                  ravedash::logger("Got other contrasts", calc_delta = TRUE)
                  across_electrode_statistics <- list(condition_means = condition_means, 
                    model = mod, model_type = class(mod)[1], 
                    aov = .aov, emmeans = em, pairwise_contrasts = pairwise, 
                    fixed_effects = fe, random_effects = re, 
                    stratified_contrasts = stratified_contrasts, 
                    itx_contrasts = itx_contrasts, trials_not_included = sort(trial_outliers_list))
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(across_electrode_statistics)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "across_electrode_statistics", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "across_electrode_statistics", target_expr = quote({
                {
                  require(data.table)
                  .datatable.aware = TRUE
                  across_electrode_statistics <- NULL
                  if (length(omnibus_results$data) > 3) {
                    dd <- subset(omnibus_results$data, currently_selected)
                    emmeans::emm_options(lmer.df = "satterthwaite")
                    ravedash::logger("top of BAES", calc_delta = TRUE)
                    rand_effects <- c("Block", "Electrode") %>% 
                      intersect(names(dd))
                    fixed_effects <- c("Factor1", "Factor2", 
                      "AnalysisLabel") %>% intersect(names(dd))
                    for (ff in c(rand_effects, fixed_effects)) {
                      dd[[ff]] %<>% as.factor
                    }
                    fe <- names(which(sapply(dd[fixed_effects], 
                      nlevels) > 1))
                    re <- names(which(sapply(dd[rand_effects], 
                      nlevels) > 1))
                    if ("AnalysisLabel" %in% fe) {
                      if ("Block" %in% re) {
                        if ("Electrode" %in% re) {
                          re[which(re == "Block")] = "Block/Trial"
                        }
                      } else {
                        re %<>% c("Trial")
                      }
                    }
                    re_str = NULL
                    if (length(re) > 0) {
                      re_str <- paste(collapse = "+", sapply(re, 
                        function(x) sprintf("(1|%s)", x)))
                    }
                    dt <- data.table::as.data.table(dd)
                    condition_means <- dt[, list(y = mean(y), 
                      sd = sd(y), se = rutabaga:::se(y), n = .N), 
                      keyby = fe]
                    ravedash::logger("got condition means", calc_delta = TRUE)
                    fe <- paste0(fe, collapse = "*")
                    if (!nzchar(fe)) {
                      fe <- "1"
                    }
                    frm <- as.formula(paste("y ~", paste(c(fe, 
                      re_str), collapse = " + ")))
                    if (length(unique(dd$Trial)) < 2) {
                      mod <- NULL
                      .aov <- NULL
                      em <- NULL
                      pairwise <- NULL
                    } else {
                      FUN <- ifelse(is.null(re_str), stats::lm, 
                        lme4::lmer)
                      mod <- do.call(FUN, list(formula = frm, 
                        data = dd))
                      .aov <- car::Anova(mod, type = ifelse(fe[1] == 
                        "1", "III", "II"))
                      ravedash::logger("built linear model, starting post hoc tests", 
                        calc_delta = TRUE)
                      em <- emmeans::emmeans(mod, as.formula(sprintf(" ~ %s", 
                        fe)), infer = c(F, T))
                      ravedash::logger("Got emm", calc_delta = TRUE)
                      pairwise <- emmeans::contrast(em, "pairwise")
                      ravedash::logger("Got pairwise contrasts", 
                        calc_delta = TRUE)
                      ravedash::logger("Got ANOVA", calc_delta = TRUE)
                    }
                    stratified_contrasts <- NULL
                    if (length(fe) > 1) {
                      stratified_contrasts <- get_stratified_contrasts(em)
                    }
                    itx_contrasts <- NULL
                    if (length(fe) == 2) {
                      nm <- paste0(fe, collapse = "_")
                      itx_contrasts <- list(emmeans::contrast(em, 
                        interaction = c("pairwise", "pairwise"))) %>% 
                        setNames(nm)
                    }
                    if (length(fe) > 2) {
                      fe_combn <- combn(fe, 2, simplify = FALSE)
                      itx_contrasts <- sapply(fe_combn, function(groups) {
                        emmeans::contrast(em, interaction = c("pairwise", 
                          "pairwise"), by = fe[!fe %in% groups])
                      }) %>% setNames(sapply(fe_combn, paste0, 
                        collapse = "."))
                    }
                    ravedash::logger("Got other contrasts", calc_delta = TRUE)
                    across_electrode_statistics <- list(condition_means = condition_means, 
                      model = mod, model_type = class(mod)[1], 
                      aov = .aov, emmeans = em, pairwise_contrasts = pairwise, 
                      fixed_effects = fe, random_effects = re, 
                      stratified_contrasts = stratified_contrasts, 
                      itx_contrasts = itx_contrasts, trials_not_included = sort(trial_outliers_list))
                  }
                }
                across_electrode_statistics
            }), target_depends = c("omnibus_results", "trial_outliers_list"
            )), deps = c("omnibus_results", "trial_outliers_list"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_by_trial_electrode_similarity_data = targets::tar_target_raw(name = "by_trial_electrode_similarity_data", 
        command = quote({
            .__target_expr__. <- quote({
                dd <- omnibus_results$data
                by_trial_electrode_similarity_data <- NULL
                if (!is.null(dd[["AnalysisLabel"]])) {
                  if (enable_second_condition_groupings) {
                  } else {
                    by_trial_electrode_similarity_data <- lapply(split(dd, 
                      list(dd$AnalysisLabel, dd$Factor1)), function(bb) {
                      bb <- bb[order(bb$Electrode, bb$Trial), 
                        ]
                      mat <- matrix(c(bb$y), nrow = length(unique(bb$Trial)), 
                        dimnames = list(NULL, unique(bb$Electrode)))
                      cor(mat)
                    })
                  }
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_trial_electrode_similarity_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "by_trial_electrode_similarity_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "by_trial_electrode_similarity_data", 
            target_expr = quote({
                {
                  dd <- omnibus_results$data
                  by_trial_electrode_similarity_data <- NULL
                  if (!is.null(dd[["AnalysisLabel"]])) {
                    if (enable_second_condition_groupings) {
                    } else {
                      by_trial_electrode_similarity_data <- lapply(split(dd, 
                        list(dd$AnalysisLabel, dd$Factor1)), 
                        function(bb) {
                          bb <- bb[order(bb$Electrode, bb$Trial), 
                            ]
                          mat <- matrix(c(bb$y), nrow = length(unique(bb$Trial)), 
                            dimnames = list(NULL, unique(bb$Electrode)))
                          cor(mat)
                        })
                    }
                  }
                }
                by_trial_electrode_similarity_data
            }), target_depends = c("omnibus_results", "enable_second_condition_groupings"
            )), deps = c("omnibus_results", "enable_second_condition_groupings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), build_data_for_export = targets::tar_target_raw(name = "data_for_export", 
        command = quote({
            .__target_expr__. <- quote({
                if (getOption("knit_rave_pipelines", default = FALSE)) {
                  data_for_export <- NULL
                } else {
                  prog <- shidashi::shiny_progress("Building export data", 
                    max = 4, shiny_auto_close = TRUE)
                  data_for_export = FALSE
                  electrodes_to_keep <- dipsaus::parse_svec(electrodes_to_export, 
                    sep = ",|;", connect = ":-")
                  electrodes_to_keep %<>% remove_from_arr(repository$power$dimnames$Electrode, 
                    `%in%`, negate = TRUE)
                  if (electrodes_to_export_roi_name != "none") {
                    v = if (electrodes_to_export_roi_name == 
                      "Custom ROI") {
                    } else {
                      electrodes_to_export_roi_name
                    }
                    lbls <- subset(repository$electrode_table, 
                      Electrode %in% electrodes_to_keep, select = v, 
                      drop = TRUE)
                    electrodes_to_keep = electrodes_to_keep[lbls %in% 
                      electrodes_to_export_roi_categories]
                  }
                  if (!length(electrodes_to_keep)) {
                    stop("No electrodes were found passing all selection criteria")
                  }
                  prog$inc("Baseline data [export loop]")
                  raveio::power_baseline(repository, baseline_windows = baseline_settings$window, 
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                    units = get_baseline_scope(baseline_settings$scope), 
                    signal_type = repository$signal_type, electrodes = electrodes_to_keep)
                  dd <- paste0("pe_export_", stringr::str_replace_all(format(Sys.time(), 
                    "%X__%b_%d_%Y"), stringr::fixed(":"), "_"))
                  out_path <- file.path(repository$subject$path, 
                    "power_explorer", dd)
                  raveio::dir_create2(out_path)
                  uoa <- get_unit_of_analysis_varname(baseline_settings$unit_of_analysis)
                  for (current_electrode in electrodes_to_keep) {
                    tensors <- lapply(analysis_settings_clean, 
                      function(asc) {
                        ravedash::logger("Working on ", asc$label)
                        current_tensor = subset(repository$power$baselined, 
                          Electrode ~ Electrode %in% current_electrode)
                        tet <- trial_export_types()
                        trials_to_keep = repository$power$dimnames$Trial
                        if (trials_to_export %in% c(tet$RAW_GRP, 
                          tet$CLP_GRP, tet$CLP_CND)) {
                          trials_to_keep <- sort(unique(c(unlist(sapply(analysis_groups, 
                            `[[`, "trials")))))
                        }
                        shifted_tensor <- get_pluriform_power(baselined_data = current_tensor, 
                          trial_indices = trials_to_keep, events = repository$epoch$table, 
                          epoch_event_types = get_available_events(repository$epoch$columns), 
                          trial_outliers_list = unlist(trial_outliers_list), 
                          event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate, 
                          final_data_only = TRUE)
                        dn = lapply(dimnames(shifted_tensor), 
                          as.numeric)
                        if (trials_to_export == tet$CLP_GRP) {
                          with_trials <- which_have_trials(analysis_groups)
                          by_group <- sapply(analysis_groups[with_trials], 
                            function(ag) {
                              ind <- (dn$Trial %in% ag$trials)
                              ravetools::collapse(shifted_tensor[, 
                                , ind, , drop = FALSE], keep = c(1, 
                                2, 4))
                            })
                          shifted_tensor = tensor_reshape(mat = by_group, 
                            orig_dim = dim(shifted_tensor), pivot = 3)
                          dn$Trial = unname(sapply(analysis_groups[with_trials], 
                            `[[`, "label"))
                          dimnames(shifted_tensor) = dn
                        }
                        attr_TrialLabel = subset(repository$epoch$table, 
                          Trial %in% dn$Trial, select = c("Trial", 
                            condition_variable)) %>% data.table::setorder("Trial")
                        attr_TrialLabel$OrigCondition = attr_TrialLabel[[condition_variable]]
                        for (ag in which_have_trials(analysis_groups)) {
                          ind = attr_TrialLabel$Trial %in% analysis_groups[[ag]]$trials
                          attr_TrialLabel$Condition[ind] = names(analysis_groups)[ag]
                        }
                        tmet <- time_export_types()
                        if (times_to_export %in% c(tmet$CLP_AWO, 
                          tmet$RAW_AWO)) {
                          ind <- dn$Time %within% asc$time
                          shifted_tensor = shifted_tensor[, ind, 
                            , , drop = FALSE]
                          dn$Time = as.numeric(dimnames(shifted_tensor)$Time)
                        }
                        if (times_to_export == tmet$CLP_AWO) {
                          tmp = ravetools::collapse(shifted_tensor, 
                            keep = c(1, 3:4))
                          dim(tmp) = c(dim(tmp), 1)
                          shifted_tensor <- aperm(tmp, c(1, 4, 
                            2, 3))
                          dn$Time = asc$label
                          dimnames(shifted_tensor) = dn
                        }
                        fet = frequency_export_types()
                        if (frequencies_to_export %in% c(fet$CLP_AWO, 
                          fet$RAW_AWO)) {
                          ff <- dn$Frequency %within% asc$frequency
                          shifted_tensor = shifted_tensor[ff, 
                            , , , drop = FALSE]
                        }
                        dn$Frequency = as.numeric(dimnames(shifted_tensor)$Frequency)
                        if (frequencies_to_export == fet$CLP_AWO) {
                          tmp = ravetools::collapse(shifted_tensor, 
                            keep = 2:4)
                          dim(tmp) = c(dim(tmp), 1)
                          shifted_tensor <- aperm(tmp, c(4, 1:3))
                          dn$Frequency = asc$label
                          dimnames(shifted_tensor) = dn
                        }
                        if (length(attr_TrialLabel$Condition) == 
                          length(dn$Trial) && all(0 == (dn$Trial - 
                          attr_TrialLabel$Trial))) {
                          attr(shifted_tensor, "TrialLabel") = attr_TrialLabel$Condition
                          attr(shifted_tensor, "OrigTrialLabel") = attr_TrialLabel$OrigCondition
                        } else {
                          ravedash::logger(level = "warning", 
                            "Could not apply trial labels. Length mismatch between labels (", 
                            nrow(attr_TrialLabel), ") and  trials (", 
                            length(dn$Trial), "), or trial numbers didn't line up")
                        }
                        return(shifted_tensor)
                      })
                    flat_tables <- mapply(function(tensor, asc) {
                      tbl <- data.table::as.data.table(reshape2::melt(tensor[drop = FALSE], 
                        value.name = uoa))
                      tbl$AnalysisGroup = asc$label
                      if (!is.null(attributes(tensor)[["TrialLabel"]])) {
                        df <- data.table::data.table(Trial = as.numeric(dimnames(tensor)$Trial), 
                          TrialLabel = attributes(tensor)[["TrialLabel"]], 
                          OrigTrialLabel = attributes(tensor)[["OrigTrialLabel"]])
                        tbl %<>% merge(df, all.y = FALSE, all.x = TRUE)
                      }
                      return(tbl)
                    }, tensors, analysis_settings_clean, SIMPLIFY = FALSE)
                    if (!data.table::is.data.table(flat_tables)) {
                      flat_tables <- rutabaga::rbind_list(flat_tables)
                    }
                    flat_tables %<>% lapply(function(x) {
                      if (is.factor(x)) {
                        x <- as.character(x)
                      }
                      x
                    }) %>% as.data.frame
                    if (identical(flat_tables$Frequency, flat_tables$AnalysisGroup)) {
                      flat_tables$Frequency <- NULL
                    }
                    if (identical(flat_tables$Time, flat_tables$AnalysisGroup)) {
                      flat_tables$Time <- NULL
                    }
                    data.table::fwrite(row.names = FALSE, flat_tables, 
                      file = file.path(out_path, sprintf("%s_%s_e%04d.csv", 
                        repository$subject$project_name, repository$subject$subject_code, 
                        current_electrode)))
                  }
                  clean_settings <- function(ascs) {
                    lapply(ascs, function(asc) {
                      asc$frequency_dd <- NULL
                      asc$subject_code <- NULL
                      asc$project_name <- NULL
                      if (!isTRUE(asc$censor_info$enabled)) {
                        asc$censor_info <- NULL
                      }
                      asc
                    })
                  }
                  metadata = list(subject = repository$subject$subject_code, 
                    project = repository$subject$project_name, 
                    baseline_window = paste0(collapse = ":", 
                      baseline_settings$window[[1]]), baseline_scope = baseline_settings$scope[[1]], 
                    unit = uoa, signal_type = repository$signal_type, 
                    analyis_settings = clean_settings(analysis_settings_clean), 
                    reference = subset(repository$reference_table, 
                      Electrode %in% electrodes_to_keep), electrodes = subset(repository$electrode_table, 
                      Electrode %in% electrodes_to_keep))
                  raveio::save_yaml(metadata, file = file.path(out_path, 
                    "metadata.yaml"))
                  data_for_export <- out_path
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_for_export)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "data_for_export", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "data_for_export", target_expr = quote({
                {
                  if (getOption("knit_rave_pipelines", default = FALSE)) {
                    data_for_export <- NULL
                  } else {
                    prog <- shidashi::shiny_progress("Building export data", 
                      max = 4, shiny_auto_close = TRUE)
                    data_for_export = FALSE
                    electrodes_to_keep <- dipsaus::parse_svec(electrodes_to_export, 
                      sep = ",|;", connect = ":-")
                    electrodes_to_keep %<>% remove_from_arr(repository$power$dimnames$Electrode, 
                      `%in%`, negate = TRUE)
                    if (electrodes_to_export_roi_name != "none") {
                      v = if (electrodes_to_export_roi_name == 
                        "Custom ROI") {
                      } else {
                        electrodes_to_export_roi_name
                      }
                      lbls <- subset(repository$electrode_table, 
                        Electrode %in% electrodes_to_keep, select = v, 
                        drop = TRUE)
                      electrodes_to_keep = electrodes_to_keep[lbls %in% 
                        electrodes_to_export_roi_categories]
                    }
                    if (!length(electrodes_to_keep)) {
                      stop("No electrodes were found passing all selection criteria")
                    }
                    prog$inc("Baseline data [export loop]")
                    raveio::power_baseline(repository, baseline_windows = baseline_settings$window, 
                      method = get_unit_of_analysis(baseline_settings$unit_of_analysis), 
                      units = get_baseline_scope(baseline_settings$scope), 
                      signal_type = repository$signal_type, electrodes = electrodes_to_keep)
                    dd <- paste0("pe_export_", stringr::str_replace_all(format(Sys.time(), 
                      "%X__%b_%d_%Y"), stringr::fixed(":"), "_"))
                    out_path <- file.path(repository$subject$path, 
                      "power_explorer", dd)
                    raveio::dir_create2(out_path)
                    uoa <- get_unit_of_analysis_varname(baseline_settings$unit_of_analysis)
                    for (current_electrode in electrodes_to_keep) {
                      tensors <- lapply(analysis_settings_clean, 
                        function(asc) {
                          ravedash::logger("Working on ", asc$label)
                          current_tensor = subset(repository$power$baselined, 
                            Electrode ~ Electrode %in% current_electrode)
                          tet <- trial_export_types()
                          trials_to_keep = repository$power$dimnames$Trial
                          if (trials_to_export %in% c(tet$RAW_GRP, 
                            tet$CLP_GRP, tet$CLP_CND)) {
                            trials_to_keep <- sort(unique(c(unlist(sapply(analysis_groups, 
                              `[[`, "trials")))))
                          }
                          shifted_tensor <- get_pluriform_power(baselined_data = current_tensor, 
                            trial_indices = trials_to_keep, events = repository$epoch$table, 
                            epoch_event_types = get_available_events(repository$epoch$columns), 
                            trial_outliers_list = unlist(trial_outliers_list), 
                            event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate, 
                            final_data_only = TRUE)
                          dn = lapply(dimnames(shifted_tensor), 
                            as.numeric)
                          if (trials_to_export == tet$CLP_GRP) {
                            with_trials <- which_have_trials(analysis_groups)
                            by_group <- sapply(analysis_groups[with_trials], 
                              function(ag) {
                                ind <- (dn$Trial %in% ag$trials)
                                ravetools::collapse(shifted_tensor[, 
                                  , ind, , drop = FALSE], keep = c(1, 
                                  2, 4))
                              })
                            shifted_tensor = tensor_reshape(mat = by_group, 
                              orig_dim = dim(shifted_tensor), 
                              pivot = 3)
                            dn$Trial = unname(sapply(analysis_groups[with_trials], 
                              `[[`, "label"))
                            dimnames(shifted_tensor) = dn
                          }
                          attr_TrialLabel = subset(repository$epoch$table, 
                            Trial %in% dn$Trial, select = c("Trial", 
                              condition_variable)) %>% data.table::setorder("Trial")
                          attr_TrialLabel$OrigCondition = attr_TrialLabel[[condition_variable]]
                          for (ag in which_have_trials(analysis_groups)) {
                            ind = attr_TrialLabel$Trial %in% 
                              analysis_groups[[ag]]$trials
                            attr_TrialLabel$Condition[ind] = names(analysis_groups)[ag]
                          }
                          tmet <- time_export_types()
                          if (times_to_export %in% c(tmet$CLP_AWO, 
                            tmet$RAW_AWO)) {
                            ind <- dn$Time %within% asc$time
                            shifted_tensor = shifted_tensor[, 
                              ind, , , drop = FALSE]
                            dn$Time = as.numeric(dimnames(shifted_tensor)$Time)
                          }
                          if (times_to_export == tmet$CLP_AWO) {
                            tmp = ravetools::collapse(shifted_tensor, 
                              keep = c(1, 3:4))
                            dim(tmp) = c(dim(tmp), 1)
                            shifted_tensor <- aperm(tmp, c(1, 
                              4, 2, 3))
                            dn$Time = asc$label
                            dimnames(shifted_tensor) = dn
                          }
                          fet = frequency_export_types()
                          if (frequencies_to_export %in% c(fet$CLP_AWO, 
                            fet$RAW_AWO)) {
                            ff <- dn$Frequency %within% asc$frequency
                            shifted_tensor = shifted_tensor[ff, 
                              , , , drop = FALSE]
                          }
                          dn$Frequency = as.numeric(dimnames(shifted_tensor)$Frequency)
                          if (frequencies_to_export == fet$CLP_AWO) {
                            tmp = ravetools::collapse(shifted_tensor, 
                              keep = 2:4)
                            dim(tmp) = c(dim(tmp), 1)
                            shifted_tensor <- aperm(tmp, c(4, 
                              1:3))
                            dn$Frequency = asc$label
                            dimnames(shifted_tensor) = dn
                          }
                          if (length(attr_TrialLabel$Condition) == 
                            length(dn$Trial) && all(0 == (dn$Trial - 
                            attr_TrialLabel$Trial))) {
                            attr(shifted_tensor, "TrialLabel") = attr_TrialLabel$Condition
                            attr(shifted_tensor, "OrigTrialLabel") = attr_TrialLabel$OrigCondition
                          } else {
                            ravedash::logger(level = "warning", 
                              "Could not apply trial labels. Length mismatch between labels (", 
                              nrow(attr_TrialLabel), ") and  trials (", 
                              length(dn$Trial), "), or trial numbers didn't line up")
                          }
                          return(shifted_tensor)
                        })
                      flat_tables <- mapply(function(tensor, 
                        asc) {
                        tbl <- data.table::as.data.table(reshape2::melt(tensor[drop = FALSE], 
                          value.name = uoa))
                        tbl$AnalysisGroup = asc$label
                        if (!is.null(attributes(tensor)[["TrialLabel"]])) {
                          df <- data.table::data.table(Trial = as.numeric(dimnames(tensor)$Trial), 
                            TrialLabel = attributes(tensor)[["TrialLabel"]], 
                            OrigTrialLabel = attributes(tensor)[["OrigTrialLabel"]])
                          tbl %<>% merge(df, all.y = FALSE, all.x = TRUE)
                        }
                        return(tbl)
                      }, tensors, analysis_settings_clean, SIMPLIFY = FALSE)
                      if (!data.table::is.data.table(flat_tables)) {
                        flat_tables <- rutabaga::rbind_list(flat_tables)
                      }
                      flat_tables %<>% lapply(function(x) {
                        if (is.factor(x)) {
                          x <- as.character(x)
                        }
                        x
                      }) %>% as.data.frame
                      if (identical(flat_tables$Frequency, flat_tables$AnalysisGroup)) {
                        flat_tables$Frequency <- NULL
                      }
                      if (identical(flat_tables$Time, flat_tables$AnalysisGroup)) {
                        flat_tables$Time <- NULL
                      }
                      data.table::fwrite(row.names = FALSE, flat_tables, 
                        file = file.path(out_path, sprintf("%s_%s_e%04d.csv", 
                          repository$subject$project_name, repository$subject$subject_code, 
                          current_electrode)))
                    }
                    clean_settings <- function(ascs) {
                      lapply(ascs, function(asc) {
                        asc$frequency_dd <- NULL
                        asc$subject_code <- NULL
                        asc$project_name <- NULL
                        if (!isTRUE(asc$censor_info$enabled)) {
                          asc$censor_info <- NULL
                        }
                        asc
                      })
                    }
                    metadata = list(subject = repository$subject$subject_code, 
                      project = repository$subject$project_name, 
                      baseline_window = paste0(collapse = ":", 
                        baseline_settings$window[[1]]), baseline_scope = baseline_settings$scope[[1]], 
                      unit = uoa, signal_type = repository$signal_type, 
                      analyis_settings = clean_settings(analysis_settings_clean), 
                      reference = subset(repository$reference_table, 
                        Electrode %in% electrodes_to_keep), electrodes = subset(repository$electrode_table, 
                        Electrode %in% electrodes_to_keep))
                    raveio::save_yaml(metadata, file = file.path(out_path, 
                      "metadata.yaml"))
                    data_for_export <- out_path
                  }
                }
                data_for_export
            }), target_depends = c("electrodes_to_export", "repository", 
            "electrodes_to_export_roi_name", "electrodes_to_export_roi_categories", 
            "baseline_settings", "analysis_settings_clean", "trials_to_export", 
            "analysis_groups", "trial_outliers_list", "condition_variable", 
            "times_to_export", "frequencies_to_export")), deps = c("electrodes_to_export", 
        "repository", "electrodes_to_export_roi_name", "electrodes_to_export_roi_categories", 
        "baseline_settings", "analysis_settings_clean", "trials_to_export", 
        "analysis_groups", "trial_outliers_list", "condition_variable", 
        "times_to_export", "frequencies_to_export"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), build_data_for_group_analysis = targets::tar_target_raw(name = "data_for_group_analysis", 
        command = quote({
            .__target_expr__. <- quote({
                data_for_group_analysis <- list()
                data_for_group_analysis$baseline_settings <- baseline_settings
                data_for_group_analysis$analysis_settings_clean <- analysis_settings_clean
                data_for_group_analysis$electrode_information <- subset(repository$electrode_table, 
                  repository$electrode_table$Electrode %in% repository$electrode_list)
                data_for_group_analysis$over_time_by_electrode_data <- over_time_by_electrode_data
                data_for_group_analysis$omnibus_stats <- omnibus_results$stats
                data_for_group_analysis$omnibus_data <- omnibus_results$data_with_outliers
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_for_group_analysis)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "data_for_group_analysis", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "data_for_group_analysis", target_expr = quote({
                {
                  data_for_group_analysis <- list()
                  data_for_group_analysis$baseline_settings <- baseline_settings
                  data_for_group_analysis$analysis_settings_clean <- analysis_settings_clean
                  data_for_group_analysis$electrode_information <- subset(repository$electrode_table, 
                    repository$electrode_table$Electrode %in% 
                      repository$electrode_list)
                  data_for_group_analysis$over_time_by_electrode_data <- over_time_by_electrode_data
                  data_for_group_analysis$omnibus_stats <- omnibus_results$stats
                  data_for_group_analysis$omnibus_data <- omnibus_results$data_with_outliers
                }
                data_for_group_analysis
            }), target_depends = c("baseline_settings", "analysis_settings_clean", 
            "repository", "over_time_by_electrode_data", "omnibus_results"
            )), deps = c("baseline_settings", "analysis_settings_clean", 
        "repository", "over_time_by_electrode_data", "omnibus_results"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"))
