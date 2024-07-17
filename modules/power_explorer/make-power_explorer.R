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
    input_electrodes_to_export_roi_categories = targets::tar_target_raw("electrodes_to_export_roi_categories",
        quote({
            settings[["electrodes_to_export_roi_categories"]]
        }), deps = "settings"), input_electrodes_to_export_roi_name = targets::tar_target_raw("electrodes_to_export_roi_name",
        quote({
            settings[["electrodes_to_export_roi_name"]]
        }), deps = "settings"), input_analysis_settings = targets::tar_target_raw("analysis_settings",
        quote({
            settings[["analysis_settings"]]
        }), deps = "settings"), input_custom_roi_groupings = targets::tar_target_raw("custom_roi_groupings",
        quote({
            settings[["custom_roi_groupings"]]
        }), deps = "settings"), input_custom_roi_variable = targets::tar_target_raw("custom_roi_variable",
        quote({
            settings[["custom_roi_variable"]]
        }), deps = "settings"), input_custom_roi_type = targets::tar_target_raw("custom_roi_type",
        quote({
            settings[["custom_roi_type"]]
        }), deps = "settings"), input_enable_custom_ROI = targets::tar_target_raw("enable_custom_ROI",
        quote({
            settings[["enable_custom_ROI"]]
        }), deps = "settings"), input_condition_variable = targets::tar_target_raw("condition_variable",
        quote({
            settings[["condition_variable"]]
        }), deps = "settings"), input_enable_second_condition_groupings = targets::tar_target_raw("enable_second_condition_groupings",
        quote({
            settings[["enable_second_condition_groupings"]]
        }), deps = "settings"), input_second_condition_groupings = targets::tar_target_raw("second_condition_groupings",
        quote({
            settings[["second_condition_groupings"]]
        }), deps = "settings"), input_first_condition_groupings = targets::tar_target_raw("first_condition_groupings",
        quote({
            settings[["first_condition_groupings"]]
        }), deps = "settings"), input_analysis_electrodes = targets::tar_target_raw("analysis_electrodes",
        quote({
            settings[["analysis_electrodes"]]
        }), deps = "settings"), input_baseline_settings = targets::tar_target_raw("baseline_settings",
        quote({
            settings[["baseline_settings"]]
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
        }), deps = "settings"), input_time_censor = targets::tar_target_raw("time_censor",
        quote({
            settings[["time_censor"]]
        }), deps = "settings"), input_trial_outliers_list = targets::tar_target_raw("trial_outliers_list",
        quote({
            settings[["trial_outliers_list"]]
        }), deps = "settings"), input_electrode_export_file_type = targets::tar_target_raw("electrode_export_file_type",
        quote({
            settings[["electrode_export_file_type"]]
        }), deps = "settings"), input_frequencies_to_export = targets::tar_target_raw("frequencies_to_export",
        quote({
            settings[["frequencies_to_export"]]
        }), deps = "settings"), input_times_to_export = targets::tar_target_raw("times_to_export",
        quote({
            settings[["times_to_export"]]
        }), deps = "settings"), input_trials_to_export = targets::tar_target_raw("trials_to_export",
        quote({
            settings[["trials_to_export"]]
        }), deps = "settings"), input_electrodes_to_export = targets::tar_target_raw("electrodes_to_export",
        quote({
            settings[["electrodes_to_export"]]
        }), deps = "settings"), input_electrode_export_data_type = targets::tar_target_raw("electrode_export_data_type",
        quote({
            settings[["electrode_export_data_type"]]
        }), deps = "settings"), check_load_power = targets::tar_target_raw(name = "repository",
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::as_rave_subject(subject_id = sprintf("%s/%s",
                  project_name, subject_code))
                repository <- raveio::prepare_subject_power(subject = subject,
                  electrodes = loaded_electrodes, epoch_name = epoch_choice,
                  reference_name = reference_name, time_windows = c(epoch_choice__trial_starts,
                    epoch_choice__trial_ends))
                repository
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "repository",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave_prepare_power",
            target_export = "repository", target_expr = quote({
                {
                  subject <- raveio::as_rave_subject(subject_id = sprintf("%s/%s",
                    project_name, subject_code))
                  repository <- raveio::prepare_subject_power(subject = subject,
                    electrodes = loaded_electrodes, epoch_name = epoch_choice,
                    reference_name = reference_name, time_windows = c(epoch_choice__trial_starts,
                      epoch_choice__trial_ends))
                  repository
                }
                repository
            }), target_depends = c("project_name", "subject_code",
            "loaded_electrodes", "epoch_choice", "reference_name",
            "epoch_choice__trial_starts", "epoch_choice__trial_ends"
            )), deps = c("project_name", "subject_code", "loaded_electrodes",
        "epoch_choice", "reference_name", "epoch_choice__trial_starts",
        "epoch_choice__trial_ends"), cue = targets::tar_cue("thorough"),
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
                asNamespace("raveio")$resolve_pipeline_error(name = "requested_electrodes",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
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
                lapply(analysis_settings_clean, function(setting) {
                  check_range(setting$frequency, unlist(repository$frequency),
                    "frequency")
                  check_range(setting$time, unlist(repository$time_windows),
                    "analysis time")
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
                asNamespace("raveio")$resolve_pipeline_error(name = "analysis_settings_clean",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
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
                  lapply(analysis_settings_clean, function(setting) {
                    check_range(setting$frequency, unlist(repository$frequency),
                      "frequency")
                    check_range(setting$time, unlist(repository$time_windows),
                      "analysis time")
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
                raveio::power_baseline(x = repository, baseline_windows = baseline_settings$window,
                  method = get_unit_of_analysis(baseline_settings$unit_of_analysis),
                  units = get_baseline_scope(baseline_settings$scope),
                  signal_type = "LFP", electrodes = requested_electrodes)
                baselined_power <- repository$power$baselined
            })
            tryCatch({
                eval(.__target_expr__.)
                return(baselined_power)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "baselined_power",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "user-defined-r",
            target_export = "baselined_power", target_expr = quote({
                {
                  raveio::power_baseline(x = repository, baseline_windows = baseline_settings$window,
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis),
                    units = get_baseline_scope(baseline_settings$scope),
                    signal_type = "LFP", electrodes = requested_electrodes)
                  baselined_power <- repository$power$baselined
                }
                baselined_power
            }), target_depends = c("repository", "baseline_settings",
            "requested_electrodes")), deps = c("repository",
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
                asNamespace("raveio")$resolve_pipeline_error(name = "trial_details",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
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
                if (length(analysis_groups) < 1) stop("No trials available in condition groups")
                names(analysis_groups) <- sapply(analysis_groups,
                  `[[`, "label")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_groups)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "analysis_groups",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
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
                  if (length(analysis_groups) < 1) stop("No trials available in condition groups")
                  names(analysis_groups) <- sapply(analysis_groups,
                    `[[`, "label")
                }
                analysis_groups
            }), target_depends = c("enable_second_condition_groupings",
            "trial_details", "repository", "condition_variable",
            "first_condition_groupings")), deps = c("enable_second_condition_groupings",
        "trial_details", "repository", "condition_variable",
        "first_condition_groupings"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_pluriform_power = targets::tar_target_raw(name = "pluriform_power",
        command = quote({
            .__target_expr__. <- quote({
                options(future.globals.maxSize = 6 * 1024^3)
                epoch_event_types = get_available_events(repository$epoch$columns)
                baselined_power_data <- subset(baselined_power,
                  Electrode ~ Electrode %in% requested_electrodes)
                pluriform_power <- raveio::lapply_async(analysis_groups,
                  callback = function(x) {
                    paste("[build_pluriform_power] working on:",
                      x$label, "data")
                  }, FUN = function(ag) {
                    sapply(analysis_settings_clean, function(as) {
                      p <- get_pluriform_power(baselined_data = baselined_power_data,
                        trial_indices = ag$trials, events = repository$epoch$table,
                        epoch_event_types = epoch_event_types,
                        trial_outliers_list = unlist(trial_outliers_list),
                        event_of_interest = as$event, sample_rate = repository$subject$power_sample_rate)
                      list(data = p, settings = as, outliers = trial_outliers_list)
                    }, simplify = FALSE, USE.NAMES = TRUE)
                  })
                for (gg in seq_along(pluriform_power)) {
                  for (aa in seq_along(pluriform_power[[gg]])) {
                    fi <- as.numeric(dimnames(pluriform_power[[gg]][[aa]]$data$shifted_data)$Frequency) %within%
                      unlist(pluriform_power[[gg]][[aa]]$settings$frequency)
                    pluriform_power[[gg]][[aa]]$data$shifted_data_Fsub <- pluriform_power[[gg]][[aa]]$data$shifted_data[fi,
                      , , , drop = FALSE]
                    pluriform_power[[gg]][[aa]]$data$shifted_clean_data_Fsub <- pluriform_power[[gg]][[aa]]$data$shifted_clean_data[fi,
                      , , , drop = FALSE]
                  }
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(pluriform_power)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "pluriform_power",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "user-defined-r",
            target_export = "pluriform_power", target_expr = quote({
                {
                  options(future.globals.maxSize = 6 * 1024^3)
                  epoch_event_types = get_available_events(repository$epoch$columns)
                  baselined_power_data <- subset(baselined_power,
                    Electrode ~ Electrode %in% requested_electrodes)
                  pluriform_power <- raveio::lapply_async(analysis_groups,
                    callback = function(x) {
                      paste("[build_pluriform_power] working on:",
                        x$label, "data")
                    }, FUN = function(ag) {
                      sapply(analysis_settings_clean, function(as) {
                        p <- get_pluriform_power(baselined_data = baselined_power_data,
                          trial_indices = ag$trials, events = repository$epoch$table,
                          epoch_event_types = epoch_event_types,
                          trial_outliers_list = unlist(trial_outliers_list),
                          event_of_interest = as$event, sample_rate = repository$subject$power_sample_rate)
                        list(data = p, settings = as, outliers = trial_outliers_list)
                      }, simplify = FALSE, USE.NAMES = TRUE)
                    })
                  for (gg in seq_along(pluriform_power)) {
                    for (aa in seq_along(pluriform_power[[gg]])) {
                      fi <- as.numeric(dimnames(pluriform_power[[gg]][[aa]]$data$shifted_data)$Frequency) %within%
                        unlist(pluriform_power[[gg]][[aa]]$settings$frequency)
                      pluriform_power[[gg]][[aa]]$data$shifted_data_Fsub <- pluriform_power[[gg]][[aa]]$data$shifted_data[fi,
                        , , , drop = FALSE]
                      pluriform_power[[gg]][[aa]]$data$shifted_clean_data_Fsub <- pluriform_power[[gg]][[aa]]$data$shifted_clean_data[fi,
                        , , , drop = FALSE]
                    }
                  }
                }
                pluriform_power
            }), target_depends = c("repository", "baselined_power",
            "requested_electrodes", "analysis_groups", "analysis_settings_clean",
            "trial_outliers_list")), deps = c("repository", "baselined_power",
        "requested_electrodes", "analysis_groups", "analysis_settings_clean",
        "trial_outliers_list"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_overall_tf_data = targets::tar_target_raw(name = "by_frequency_over_time_data",
        command = quote({
            .__target_expr__. <- quote({
                build_data <- function(data, analysis_settings,
                  condition_group, baseline_settings, ...) {
                  dn <- dimnames(data)
                  stopifnot(c("Time", "Frequency") == names(dn)[2:1])
                  res <- list(data = raveio::collapse2(data,
                    keep = 2:1, method = "mean"), x = as.numeric(dn$Time),
                    y = as.numeric(dn$Frequency), xlab = "Time (s)",
                    ylab = "Frequency", zlab = "Mean " %&% baseline_settings$unit_of_analysis)
                  if (isTRUE(analysis_settings$censor_info$enabled)) {
                    ti = res$x %within% settings$censor_info$window
                    res$range <- range(res$data[!ti, ])
                  } else {
                    res$range <- range(res$data)
                  }
                  return(res)
                }
                by_frequency_over_time_data <- data_builder(pluriform_power = pluriform_power,
                  condition_group = analysis_groups, baseline_settings = baseline_settings,
                  build_data, data_type = "shifted_clean_data")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_frequency_over_time_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "by_frequency_over_time_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "by_frequency_over_time_data", target_expr = quote({
                {
                  build_data <- function(data, analysis_settings,
                    condition_group, baseline_settings, ...) {
                    dn <- dimnames(data)
                    stopifnot(c("Time", "Frequency") == names(dn)[2:1])
                    res <- list(data = raveio::collapse2(data,
                      keep = 2:1, method = "mean"), x = as.numeric(dn$Time),
                      y = as.numeric(dn$Frequency), xlab = "Time (s)",
                      ylab = "Frequency", zlab = "Mean " %&%
                        baseline_settings$unit_of_analysis)
                    if (isTRUE(analysis_settings$censor_info$enabled)) {
                      ti = res$x %within% settings$censor_info$window
                      res$range <- range(res$data[!ti, ])
                    } else {
                      res$range <- range(res$data)
                    }
                    return(res)
                  }
                  by_frequency_over_time_data <- data_builder(pluriform_power = pluriform_power,
                    condition_group = analysis_groups, baseline_settings = baseline_settings,
                    build_data, data_type = "shifted_clean_data")
                }
                by_frequency_over_time_data
            }), target_depends = c("pluriform_power", "analysis_groups",
            "baseline_settings")), deps = c("pluriform_power",
        "analysis_groups", "baseline_settings"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_tf_correlation_data = targets::tar_target_raw(name = "by_frequency_correlation_data",
        command = quote({
            .__target_expr__. <- quote({
                build_data <- function(data, analysis_settings,
                  condition_group, baseline_settings, ...) {
                  dn <- dimnames(data)
                  tm <- as.numeric(dn$Time)
                  ti <- tm %within% analysis_settings$time
                  if (isTRUE(analysis_settings$censor_info$enabled)) {
                    ti = ti & !(tm %within% analysis_settings$censor_info$window)
                  }
                  rawdata <- ravetools::collapse(data[, ti, ,
                    , drop = FALSE], keep = c(3, 1))
                  res <- list(data = cor(rawdata), xlab = "Frequency",
                    ylab = "Frequency", zlab = "Pearson correlation across trials",
                    x = as.numeric(dn$Frequency), y = as.numeric(dn$Frequency),
                    rawdata <- rawdata)
                  res$range = range(res$data)
                  return(res)
                }
                by_frequency_correlation_data <- data_builder(pluriform_power = pluriform_power,
                  condition_group = analysis_groups, baseline_settings = baseline_settings,
                  build_data, data_type = "shifted_clean_data")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_frequency_correlation_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "by_frequency_correlation_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "by_frequency_correlation_data",
            target_expr = quote({
                {
                  build_data <- function(data, analysis_settings,
                    condition_group, baseline_settings, ...) {
                    dn <- dimnames(data)
                    tm <- as.numeric(dn$Time)
                    ti <- tm %within% analysis_settings$time
                    if (isTRUE(analysis_settings$censor_info$enabled)) {
                      ti = ti & !(tm %within% analysis_settings$censor_info$window)
                    }
                    rawdata <- ravetools::collapse(data[, ti,
                      , , drop = FALSE], keep = c(3, 1))
                    res <- list(data = cor(rawdata), xlab = "Frequency",
                      ylab = "Frequency", zlab = "Pearson correlation across trials",
                      x = as.numeric(dn$Frequency), y = as.numeric(dn$Frequency),
                      rawdata <- rawdata)
                    res$range = range(res$data)
                    return(res)
                  }
                  by_frequency_correlation_data <- data_builder(pluriform_power = pluriform_power,
                    condition_group = analysis_groups, baseline_settings = baseline_settings,
                    build_data, data_type = "shifted_clean_data")
                }
                by_frequency_correlation_data
            }), target_depends = c("pluriform_power", "analysis_groups",
            "baseline_settings")), deps = c("pluriform_power",
        "analysis_groups", "baseline_settings"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), plot_bfcd = targets::tar_target_raw(name = "plot_bfcd_success",
        command = quote({
            .__target_expr__. <- quote({
                plot_bfcd_success = FALSE
                plot_bfcd_success = TRUE
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_bfcd_success)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_bfcd_success",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "plot_bfcd_success", target_expr = quote({
                {
                  plot_bfcd_success = FALSE
                  plot_bfcd_success = TRUE
                }
                plot_bfcd_success
            }), target_depends = character(0)), deps = character(0),
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
    build_by_trial_tf_data = targets::tar_target_raw(name = "by_trial_tf_data",
        command = quote({
            .__target_expr__. <- quote({
                build_data <- function(dd, settings) {
                  to_keep <- sapply(c("Time", "Trial"), which.equal,
                    names(dimnames(dd)))
                  res <- list(data = ravetools::collapse(dd,
                    keep = to_keep), xlab = "Time (s)", ylab = "Original Trial #",
                    zlab = "Mean " %&% baseline_settings$unit_of_analysis)
                  res[c("x", "y")] <- dimnames(dd)[to_keep] %>%
                    lapply(as.numeric)
                  res$N = dim(dd)[4L]
                  if (isTRUE(settings$censor_info$enabled)) {
                    ti = res$x %within% settings$censor_info$window
                    res$range <- range(res$data[!ti, ])
                  } else {
                    res$range <- range(res$data)
                  }
                  return(res)
                }
                by_trial_tf_data <- lapply(pluriform_power, function(pp) {
                  if (all(1 == length(table(sapply(pp, function(pi) pi$settings$event))),
                    1 == length(table(sapply(pp, function(pi) str_collapse(pi$settings$frequency)))))) {
                    build_data(pp[[1]]$data$shifted_data_Fsub,
                      pp[[1]]$settings)
                  } else {
                    sapply(pp, function(ppa) {
                      build_data(ppa$data$shifted_data_Fsub,
                        ppa$settings)
                    }, simplify = FALSE, USE.NAMES = TRUE)
                  }
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_trial_tf_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "by_trial_tf_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "by_trial_tf_data", target_expr = quote({
                {
                  build_data <- function(dd, settings) {
                    to_keep <- sapply(c("Time", "Trial"), which.equal,
                      names(dimnames(dd)))
                    res <- list(data = ravetools::collapse(dd,
                      keep = to_keep), xlab = "Time (s)", ylab = "Original Trial #",
                      zlab = "Mean " %&% baseline_settings$unit_of_analysis)
                    res[c("x", "y")] <- dimnames(dd)[to_keep] %>%
                      lapply(as.numeric)
                    res$N = dim(dd)[4L]
                    if (isTRUE(settings$censor_info$enabled)) {
                      ti = res$x %within% settings$censor_info$window
                      res$range <- range(res$data[!ti, ])
                    } else {
                      res$range <- range(res$data)
                    }
                    return(res)
                  }
                  by_trial_tf_data <- lapply(pluriform_power,
                    function(pp) {
                      if (all(1 == length(table(sapply(pp, function(pi) pi$settings$event))),
                        1 == length(table(sapply(pp, function(pi) str_collapse(pi$settings$frequency)))))) {
                        build_data(pp[[1]]$data$shifted_data_Fsub,
                          pp[[1]]$settings)
                      } else {
                        sapply(pp, function(ppa) {
                          build_data(ppa$data$shifted_data_Fsub,
                            ppa$settings)
                        }, simplify = FALSE, USE.NAMES = TRUE)
                      }
                    })
                }
                by_trial_tf_data
            }), target_depends = c("baseline_settings", "pluriform_power"
            )), deps = c("baseline_settings", "pluriform_power"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), build_over_time_by_electrode_data = targets::tar_target_raw(name = "over_time_by_electrode_data",
        command = quote({
            .__target_expr__. <- quote({
                build_data <- function(data, analysis_settings,
                  condition_group, baseline_settings, ...) {
                  dm <- dimnames(data)
                  to_keep <- sapply(c("Time", "Electrode"), which.equal,
                    names(dm))
                  res <- list(data = ravetools::collapse(data,
                    keep = to_keep), xlab = "Time (s)", ylab = "Electrode #",
                    zlab = "Mean " %&% baseline_settings$unit_of_analysis,
                    condition_group = condition_group$label,
                    electrodes = as.integer(dm$Electrode), subject_code = subject_code)
                  res[c("x", "y")] <- dimnames(data)[to_keep] %>%
                    lapply(as.numeric)
                  res$N = length(dm$Trial)
                  if (isTRUE(analysis_settings$censor_info$enabled)) {
                    ti = res$x %within% analysis_settings$censor_info$window
                    res$range <- range(res$data[!ti, ])
                  } else {
                    res$range <- range(res$data)
                  }
                  return(res)
                }
                over_time_by_electrode_data <- data_builder(pluriform_power = pluriform_power,
                  condition_group = analysis_groups, baseline_settings = baseline_settings,
                  build_data)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_electrode_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "over_time_by_electrode_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "over_time_by_electrode_data", target_expr = quote({
                {
                  build_data <- function(data, analysis_settings,
                    condition_group, baseline_settings, ...) {
                    dm <- dimnames(data)
                    to_keep <- sapply(c("Time", "Electrode"),
                      which.equal, names(dm))
                    res <- list(data = ravetools::collapse(data,
                      keep = to_keep), xlab = "Time (s)", ylab = "Electrode #",
                      zlab = "Mean " %&% baseline_settings$unit_of_analysis,
                      condition_group = condition_group$label,
                      electrodes = as.integer(dm$Electrode),
                      subject_code = subject_code)
                    res[c("x", "y")] <- dimnames(data)[to_keep] %>%
                      lapply(as.numeric)
                    res$N = length(dm$Trial)
                    if (isTRUE(analysis_settings$censor_info$enabled)) {
                      ti = res$x %within% analysis_settings$censor_info$window
                      res$range <- range(res$data[!ti, ])
                    } else {
                      res$range <- range(res$data)
                    }
                    return(res)
                  }
                  over_time_by_electrode_data <- data_builder(pluriform_power = pluriform_power,
                    condition_group = analysis_groups, baseline_settings = baseline_settings,
                    build_data)
                }
                over_time_by_electrode_data
            }), target_depends = c("subject_code", "pluriform_power",
            "analysis_groups", "baseline_settings")), deps = c("subject_code",
        "pluriform_power", "analysis_groups", "baseline_settings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), plot_over_time_by_electrode_data_data = targets::tar_target_raw(name = "betfd_success",
        command = quote({
            .__target_expr__. <- quote({
                betfd_success <- tryCatch({
                  draw_many_heat_maps(over_time_by_electrode_data)
                  TRUE
                }, error = function(e) {
                  e
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(betfd_success)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "betfd_success",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "betfd_success", target_expr = quote({
                {
                  betfd_success <- tryCatch({
                    draw_many_heat_maps(over_time_by_electrode_data)
                    TRUE
                  }, error = function(e) {
                    e
                  })
                }
                betfd_success
            }), target_depends = "over_time_by_electrode_data"),
        deps = "over_time_by_electrode_data", cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), build_over_time_by_condition_data = targets::tar_target_raw(name = "over_time_by_condition_data",
        command = quote({
            .__target_expr__. <- quote({
                build_data <- function(dd, settings) {
                  to_keep <- sapply(c("Time", "Electrode"), which.equal,
                    names(dimnames(dd)))
                  res <- list(data = ravetools::collapse(dd,
                    keep = to_keep), xlab = "Time (s)", ylab = "Mean " %&%
                    baseline_settings$unit_of_analysis, zlab = NA)
                  res$data <- cbind(.rowMeans(res$data, nrow(res$data),
                    ncol(res$data)), sqrt(diag(dipsaus::fastcov2(t(res$data)))/ncol(res$data)))
                  ind <- is.nan(res$data[, 2]) | !is.finite(res$data[,
                    2])
                  if (length(ind) > 0) {
                    res$data[ind, 2] = 0
                  }
                  res$x <- as.numeric(dimnames(dd)$Time)
                  res$y <- NA
                  res$N = length(dimnames(dd)$Electrode)
                  if (isTRUE(settings$censor_info$enabled)) {
                    ti = res$x %within% settings$censor_info$window
                    res$range <- range(rutabaga::plus_minus(res$data[!ti,
                      ]))
                  } else {
                    res$range <- range(rutabaga::plus_minus(res$data))
                  }
                  res$settings = settings
                  return(res)
                }
                over_time_by_condition_data <- lapply(pluriform_power,
                  function(pp) {
                    sapply(pp, function(ppa) {
                      build_data(ppa$data$shifted_clean_data_Fsub,
                        ppa$settings)
                    }, simplify = FALSE, USE.NAMES = TRUE)
                  })
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
                asNamespace("raveio")$resolve_pipeline_error(name = "over_time_by_condition_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "over_time_by_condition_data", target_expr = quote({
                {
                  build_data <- function(dd, settings) {
                    to_keep <- sapply(c("Time", "Electrode"),
                      which.equal, names(dimnames(dd)))
                    res <- list(data = ravetools::collapse(dd,
                      keep = to_keep), xlab = "Time (s)", ylab = "Mean " %&%
                      baseline_settings$unit_of_analysis, zlab = NA)
                    res$data <- cbind(.rowMeans(res$data, nrow(res$data),
                      ncol(res$data)), sqrt(diag(dipsaus::fastcov2(t(res$data)))/ncol(res$data)))
                    ind <- is.nan(res$data[, 2]) | !is.finite(res$data[,
                      2])
                    if (length(ind) > 0) {
                      res$data[ind, 2] = 0
                    }
                    res$x <- as.numeric(dimnames(dd)$Time)
                    res$y <- NA
                    res$N = length(dimnames(dd)$Electrode)
                    if (isTRUE(settings$censor_info$enabled)) {
                      ti = res$x %within% settings$censor_info$window
                      res$range <- range(rutabaga::plus_minus(res$data[!ti,
                        ]))
                    } else {
                      res$range <- range(rutabaga::plus_minus(res$data))
                    }
                    res$settings = settings
                    return(res)
                  }
                  over_time_by_condition_data <- lapply(pluriform_power,
                    function(pp) {
                      sapply(pp, function(ppa) {
                        build_data(ppa$data$shifted_clean_data_Fsub,
                          ppa$settings)
                      }, simplify = FALSE, USE.NAMES = TRUE)
                    })
                  for (ii in seq_along(over_time_by_condition_data)) {
                    for (jj in seq_along(over_time_by_condition_data[[ii]])) {
                      over_time_by_condition_data[[ii]][[jj]]$data_label = names(over_time_by_condition_data)[[ii]]
                      over_time_by_condition_data[[ii]][[jj]]$time_window_label = names(over_time_by_condition_data[[ii]])[[jj]]
                    }
                  }
                }
                over_time_by_condition_data
            }), target_depends = c("baseline_settings", "pluriform_power"
            )), deps = c("baseline_settings", "pluriform_power"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), plot_over_time_data = targets::tar_target_raw(name = "plot_over_time_by_condition_result",
        command = quote({
            .__target_expr__. <- quote({
                plot_over_time_by_condition_result = TRUE
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_over_time_by_condition_result)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_over_time_by_condition_result",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "plot_over_time_by_condition_result",
            target_expr = quote({
                {
                  plot_over_time_by_condition_result = TRUE
                }
                plot_over_time_by_condition_result
            }), target_depends = character(0)), deps = character(0),
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"),
    build_scatter_bar_data = targets::tar_target_raw(name = "scatter_bar_data",
        command = quote({
            .__target_expr__. <- quote({
                build_data <- function(dd, settings) {
                  dm <- dimnames(dd)
                  to_keep <- which.equal("Trial", names(dm))
                  stopifnot(which.equal("Time", names(dm)) ==
                    2)
                  t_ind <- as.numeric(dm$Time) %within% unlist(settings$time)
                  if (isTRUE(settings$censor_info$enabled)) {
                    t_ind = t_ind & !(as.numeric(dm$Time) %within%
                      unlist(settings$censor_info$window))
                  }
                  res <- list(data = ravetools::collapse(dd[,
                    t_ind, , , drop = FALSE], keep = to_keep),
                    xlab = "Group", ylab = "Mean " %&% baseline_settings$unit_of_analysis,
                    zlab = NA)
                  res$range <- range(res$data)
                  res$x <- NA
                  res$y <- NA
                  res$N = length(dimnames(dd)$Trial)
                  return(res)
                }
                scatter_bar_data = FALSE
            })
            tryCatch({
                eval(.__target_expr__.)
                return(scatter_bar_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "scatter_bar_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "scatter_bar_data", target_expr = quote({
                {
                  build_data <- function(dd, settings) {
                    dm <- dimnames(dd)
                    to_keep <- which.equal("Trial", names(dm))
                    stopifnot(which.equal("Time", names(dm)) ==
                      2)
                    t_ind <- as.numeric(dm$Time) %within% unlist(settings$time)
                    if (isTRUE(settings$censor_info$enabled)) {
                      t_ind = t_ind & !(as.numeric(dm$Time) %within%
                        unlist(settings$censor_info$window))
                    }
                    res <- list(data = ravetools::collapse(dd[,
                      t_ind, , , drop = FALSE], keep = to_keep),
                      xlab = "Group", ylab = "Mean " %&% baseline_settings$unit_of_analysis,
                      zlab = NA)
                    res$range <- range(res$data)
                    res$x <- NA
                    res$y <- NA
                    res$N = length(dimnames(dd)$Trial)
                    return(res)
                  }
                  scatter_bar_data = FALSE
                }
                scatter_bar_data
            }), target_depends = "baseline_settings"), deps = "baseline_settings",
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
    build_analysis_data = targets::tar_target_raw(name = "analysis_data",
        command = quote({
            .__target_expr__. <- quote({
                has_data <- which(sapply(analysis_groups, `[[`,
                  "has_trials"))
                analysis_data <- list()
                analysis_data$datatype <- baseline_settings$unit_of_analysis
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "analysis_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "analysis_data", target_expr = quote({
                {
                  has_data <- which(sapply(analysis_groups, `[[`,
                    "has_trials"))
                  analysis_data <- list()
                  analysis_data$datatype <- baseline_settings$unit_of_analysis
                }
                analysis_data
            }), target_depends = c("analysis_groups", "baseline_settings"
            )), deps = c("analysis_groups", "baseline_settings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), build_over_time_by_electrode_dataframe = targets::tar_target_raw(name = "over_time_by_electrode_dataframe",
        command = quote({
            .__target_expr__. <- quote({
                over_time_by_electrode_dataframe <- NULL
                raveio::power_baseline(repository, baseline_windows = baseline_settings$window,
                  method = get_unit_of_analysis(baseline_settings$unit_of_analysis),
                  units = get_baseline_scope(baseline_settings$scope),
                  signal_type = "LFP", electrodes = repository$electrode_list)
                non_empty_groups <- which(rutabaga::get_list_elements(analysis_groups,
                  "has_trials"))
                by_condition_group <- raveio::lapply_async(analysis_groups,
                  callback = function(x) {
                    paste("[build_over_time_by_electrode_dataframe] working on:",
                      x$label, "data")
                  }, FUN = function(ag) {
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
                over_time_by_electrode_dataframe <- by_condition_group[[1]]
                if (length(by_condition_group) > 1) {
                  for (ii in seq_along(by_condition_group)[-1]) {
                    over_time_by_electrode_dataframe = merge(over_time_by_electrode_dataframe,
                      by_condition_group[[ii]], all = TRUE)
                  }
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_electrode_dataframe)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "over_time_by_electrode_dataframe",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "over_time_by_electrode_dataframe",
            target_expr = quote({
                {
                  over_time_by_electrode_dataframe <- NULL
                  raveio::power_baseline(repository, baseline_windows = baseline_settings$window,
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis),
                    units = get_baseline_scope(baseline_settings$scope),
                    signal_type = "LFP", electrodes = repository$electrode_list)
                  non_empty_groups <- which(rutabaga::get_list_elements(analysis_groups,
                    "has_trials"))
                  by_condition_group <- raveio::lapply_async(analysis_groups,
                    callback = function(x) {
                      paste("[build_over_time_by_electrode_dataframe] working on:",
                        x$label, "data")
                    }, FUN = function(ag) {
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
                  over_time_by_electrode_dataframe <- by_condition_group[[1]]
                  if (length(by_condition_group) > 1) {
                    for (ii in seq_along(by_condition_group)[-1]) {
                      over_time_by_electrode_dataframe = merge(over_time_by_electrode_dataframe,
                        by_condition_group[[ii]], all = TRUE)
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
                build_data <- function(data, analysis_settings,
                  condition_group, baseline_settings, ...) {
                  dm <- dimnames(data)
                  to_keep <- c(which.equal("Time", names(dm)),
                    which.equal("Trial", names(dm)))
                  res <- list(data = ravetools::collapse(data[,
                    drop = FALSE], keep = to_keep), x = as.numeric(dm$Time),
                    xlab = "Time", ylab = "Trial (sorted by condition)",
                    zlab = sprintf("Mean %s", baseline_settings$unit_of_analysis))
                  res$range <- range(res$data)
                  cnds <- condition_group$conditions
                  tt <- condition_group$trials
                  res$y <- trial_details[as.character(tt), condition_variable]
                  cf <- factor(res$y, levels = cnds)
                  ord = order(cf, tt)
                  res$y <- res$y[ord]
                  res$trial_number = tt[ord]
                  res$is_outlier = tt[ord] %in% trial_outliers_list
                  res$data <- res$data[, ord]
                  return(res)
                }
                over_time_by_trial_data <- data_builder(pluriform_power,
                  condition_groups = analysis_groups, baseline_settings = baseline_settings,
                  BUILDER_FUN = build_data, data_type = "shifted_data_Fsub")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(over_time_by_trial_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "over_time_by_trial_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "over_time_by_trial_data", target_expr = quote({
                {
                  build_data <- function(data, analysis_settings,
                    condition_group, baseline_settings, ...) {
                    dm <- dimnames(data)
                    to_keep <- c(which.equal("Time", names(dm)),
                      which.equal("Trial", names(dm)))
                    res <- list(data = ravetools::collapse(data[,
                      drop = FALSE], keep = to_keep), x = as.numeric(dm$Time),
                      xlab = "Time", ylab = "Trial (sorted by condition)",
                      zlab = sprintf("Mean %s", baseline_settings$unit_of_analysis))
                    res$range <- range(res$data)
                    cnds <- condition_group$conditions
                    tt <- condition_group$trials
                    res$y <- trial_details[as.character(tt),
                      condition_variable]
                    cf <- factor(res$y, levels = cnds)
                    ord = order(cf, tt)
                    res$y <- res$y[ord]
                    res$trial_number = tt[ord]
                    res$is_outlier = tt[ord] %in% trial_outliers_list
                    res$data <- res$data[, ord]
                    return(res)
                  }
                  over_time_by_trial_data <- data_builder(pluriform_power,
                    condition_groups = analysis_groups, baseline_settings = baseline_settings,
                    BUILDER_FUN = build_data, data_type = "shifted_data_Fsub")
                }
                over_time_by_trial_data
            }), target_depends = c("trial_details", "condition_variable",
            "trial_outliers_list", "pluriform_power", "analysis_groups",
            "baseline_settings")), deps = c("trial_details",
        "condition_variable", "trial_outliers_list", "pluriform_power",
        "analysis_groups", "baseline_settings"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_internal_omnibus_results = targets::tar_target_raw(name = "internal_omnibus_results",
        command = quote({
            .__target_expr__. <- quote({
                options(future.globals.maxSize = 6 * 1024^3)
                raveio::power_baseline(repository, baseline_windows = baseline_settings$window,
                  method = get_unit_of_analysis(baseline_settings$unit_of_analysis),
                  units = get_baseline_scope(baseline_settings$scope),
                  signal_type = "LFP", electrodes = repository$electrode_list)
                non_empty_groups <- which(rutabaga::get_list_elements(analysis_groups,
                  "has_trials"))
                by_condition_group <- raveio::lapply_async(x = analysis_groups[non_empty_groups],
                  function(ag) {
                    lapply(analysis_settings_clean, function(as) {
                      fi <- repository$frequency %within% as$frequency
                      all_p <- get_pluriform_power(baselined_data = repository$power$baselined[fi,
                        , , , drop = FALSE], trial_indices = ag$trials,
                        events = repository$epoch$table, epoch_event_types = get_available_events(repository$epoch$columns),
                        trial_outliers_list = unlist(trial_outliers_list),
                        event_of_interest = as$event, sample_rate = repository$subject$power_sample_rate)
                      p <- all_p$shifted_data
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
                all_data_clean <- subset(all_data, is_clean)
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
                stats <- all_data_clean %>% split((.)$Electrode) %>%
                  raveio::lapply_async(function(el) {
                    run_stats(el)
                  }) %>% rutabaga::cbind_list()
                attr(stats, "electrode_labels") = repository$electrode_table$Label
                all_data %<>% merge(repository$epoch$table[,
                  c("Block", "Trial")], sort = FALSE)
                internal_omnibus_results = list(data_with_outliers = all_data,
                  data = all_data_clean, stats = stats)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(internal_omnibus_results)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "internal_omnibus_results",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "internal_omnibus_results", target_expr = quote({
                {
                  options(future.globals.maxSize = 6 * 1024^3)
                  raveio::power_baseline(repository, baseline_windows = baseline_settings$window,
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis),
                    units = get_baseline_scope(baseline_settings$scope),
                    signal_type = "LFP", electrodes = repository$electrode_list)
                  non_empty_groups <- which(rutabaga::get_list_elements(analysis_groups,
                    "has_trials"))
                  by_condition_group <- raveio::lapply_async(x = analysis_groups[non_empty_groups],
                    function(ag) {
                      lapply(analysis_settings_clean, function(as) {
                        fi <- repository$frequency %within% as$frequency
                        all_p <- get_pluriform_power(baselined_data = repository$power$baselined[fi,
                          , , , drop = FALSE], trial_indices = ag$trials,
                          events = repository$epoch$table, epoch_event_types = get_available_events(repository$epoch$columns),
                          trial_outliers_list = unlist(trial_outliers_list),
                          event_of_interest = as$event, sample_rate = repository$subject$power_sample_rate)
                        p <- all_p$shifted_data
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
                  all_data_clean <- subset(all_data, is_clean)
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
                  stats <- all_data_clean %>% split((.)$Electrode) %>%
                    raveio::lapply_async(function(el) {
                      run_stats(el)
                    }) %>% rutabaga::cbind_list()
                  attr(stats, "electrode_labels") = repository$electrode_table$Label
                  all_data %<>% merge(repository$epoch$table[,
                    c("Block", "Trial")], sort = FALSE)
                  internal_omnibus_results = list(data_with_outliers = all_data,
                    data = all_data_clean, stats = stats)
                }
                internal_omnibus_results
            }), target_depends = c("repository", "baseline_settings",
            "analysis_groups", "analysis_settings_clean", "trial_outliers_list",
            "enable_second_condition_groupings")), deps = c("repository",
        "baseline_settings", "analysis_groups", "analysis_settings_clean",
        "trial_outliers_list", "enable_second_condition_groupings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), build_omnibus_results = targets::tar_target_raw(name = "omnibus_results",
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
                asNamespace("raveio")$resolve_pipeline_error(name = "omnibus_results",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
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
                dd <- subset(omnibus_results$data, currently_selected)
                emmeans::emm_options(lmer.df = "satterthwaite")
                ravedash::logger("top of BAES", calc_delta = TRUE)
                rand_effects <- c("Block", "Electrode") %>% intersect(names(dd))
                fixed_effects <- c("Factor1", "Factor2", "AnalysisLabel") %>%
                  intersect(names(dd))
                for (ff in c(rand_effects, fixed_effects)) {
                  dd[[ff]] %<>% as.factor
                }
                fe <- names(which(sapply(dd[fixed_effects], nlevels) >
                  1))
                re <- names(which(sapply(dd[rand_effects], nlevels) >
                  1))
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
                ravedash::logger("Trying data.table collapse. checking DTA: ",
                  data.table:::cedta(), calc_delta = TRUE)
                dt <- data.table::as.data.table(dd)
                condition_means <- dt[, list(y = mean(y), sd = sd(y),
                  se = rutabaga:::se(y), n = .N), keyby = fe]
                ravedash::logger("got condition means", calc_delta = TRUE)
                fe <- paste0(fe, collapse = "*")
                if (!nzchar(fe)) {
                  fe <- "1"
                }
                frm <- as.formula(paste("y ~", paste(c(fe, re_str),
                  collapse = " + ")))
                FUN <- ifelse(is.null(re_str), stats::lm, lme4::lmer)
                mod <- do.call(FUN, list(formula = frm, data = dd))
                ravedash::logger("built linear model, starting post hoc tests",
                  calc_delta = TRUE)
                em <- emmeans::emmeans(mod, as.formula(sprintf(" ~ %s",
                  fe)), infer = c(F, T))
                ravedash::logger("Got emm", calc_delta = TRUE)
                pairwise <- emmeans::contrast(em, "pairwise")
                ravedash::logger("Got pairwise contrasts", calc_delta = TRUE)
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
                  }) %>% setNames(sapply(fe_combn, paste0, collapse = "."))
                }
                ravedash::logger("Got other contrasts", calc_delta = TRUE)
                .aov <- car::Anova(mod, type = ifelse(fe[1] ==
                  "1", "III", "II"))
                ravedash::logger("Got ANOVA", calc_delta = TRUE)
                across_electrode_statistics <- list(condition_means = condition_means,
                  model = mod, model_type = class(mod)[1], aov = .aov,
                  emmeans = em, pairwise_contrasts = pairwise,
                  fixed_effects = fe, random_effects = re, stratified_contrasts = stratified_contrasts,
                  itx_contrasts = itx_contrasts, trials_not_included = sort(trial_outliers_list))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(across_electrode_statistics)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "across_electrode_statistics",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "across_electrode_statistics", target_expr = quote({
                {
                  require(data.table)
                  .datatable.aware = TRUE
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
                  ravedash::logger("Trying data.table collapse. checking DTA: ",
                    data.table:::cedta(), calc_delta = TRUE)
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
                  FUN <- ifelse(is.null(re_str), stats::lm, lme4::lmer)
                  mod <- do.call(FUN, list(formula = frm, data = dd))
                  ravedash::logger("built linear model, starting post hoc tests",
                    calc_delta = TRUE)
                  em <- emmeans::emmeans(mod, as.formula(sprintf(" ~ %s",
                    fe)), infer = c(F, T))
                  ravedash::logger("Got emm", calc_delta = TRUE)
                  pairwise <- emmeans::contrast(em, "pairwise")
                  ravedash::logger("Got pairwise contrasts",
                    calc_delta = TRUE)
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
                  .aov <- car::Anova(mod, type = ifelse(fe[1] ==
                    "1", "III", "II"))
                  ravedash::logger("Got ANOVA", calc_delta = TRUE)
                  across_electrode_statistics <- list(condition_means = condition_means,
                    model = mod, model_type = class(mod)[1],
                    aov = .aov, emmeans = em, pairwise_contrasts = pairwise,
                    fixed_effects = fe, random_effects = re,
                    stratified_contrasts = stratified_contrasts,
                    itx_contrasts = itx_contrasts, trials_not_included = sort(trial_outliers_list))
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
                if (enable_second_condition_groupings) {
                } else {
                  by_trial_electrode_similarity_data <- lapply(split(dd,
                    list(dd$AnalysisLabel, dd$Factor1)), function(bb) {
                    bb <- bb[order(bb$Electrode, bb$Trial), ]
                    mat <- matrix(c(bb$y), nrow = length(unique(bb$Trial)),
                      dimnames = list(NULL, unique(bb$Electrode)))
                    cor(mat)
                  })
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(by_trial_electrode_similarity_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "by_trial_electrode_similarity_data",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "by_trial_electrode_similarity_data",
            target_expr = quote({
                {
                  dd <- omnibus_results$data
                  by_trial_electrode_similarity_data <- NULL
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
                by_trial_electrode_similarity_data
            }), target_depends = c("omnibus_results", "enable_second_condition_groupings"
            )), deps = c("omnibus_results", "enable_second_condition_groupings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), build_data_for_export = targets::tar_target_raw(name = "data_for_export",
        command = quote({
            .__target_expr__. <- quote({
                warning("Overlapping time/frequency windows will not be coded properly in the export file")
                if (getOption("knit_rave_pipelines", default = FALSE)) {
                  list2env(list(electrodes_to_export = repository$power$dimnames$Electrode[1]),
                    envir = environment())
                } else {
                }
                prog <- shidashi::shiny_progress("Building export data",
                  max = 4, shiny_auto_close = TRUE)
                data_for_export = FALSE
                electrodes_to_keep <- dipsaus::parse_svec(electrodes_to_export,
                  sep = ",|;", connect = ":-")
                electrodes_to_keep %<>% remove_from_arr(repository$power$dimnames$Electrode,
                  `%in%`, negate = TRUE)
                if (electrodes_to_export_roi_name != "none") {
                  v = if (electrodes_to_export_roi_name == "Custom ROI") {
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
                  signal_type = "LFP", electrodes = electrodes_to_keep)
                tensors <- lapply(analysis_settings_clean, function(asc) {
                  ravedash::logger("Working on ", asc$label)
                  current_tensor = subset(repository$power$baselined,
                    Electrode ~ Electrode %in% electrodes_to_keep)
                  tet <- trial_export_types()
                  trials_to_keep = repository$power$dimnames$Trial
                  if (trials_to_export %in% c(tet$RAW_GRP, tet$CLP_GRP,
                    tet$CLP_CND)) {
                    trials_to_keep <- sort(unique(c(unlist(sapply(analysis_groups,
                      `[[`, "trials")))))
                  }
                  shifted_tensor <- get_pluriform_power(baselined_data = current_tensor,
                    trial_indices = trials_to_keep, events = repository$epoch$table,
                    epoch_event_types = get_available_events(repository$epoch$columns),
                    trial_outliers_list = unlist(trial_outliers_list),
                    event_of_interest = asc$event, sample_rate = repository$subject$power_sample_rate,
                    final_data_only = TRUE)
                  dn = lapply(dimnames(shifted_tensor), as.numeric)
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
                  if (times_to_export %in% c(tmet$CLP_AWO, tmet$RAW_AWO)) {
                    ind <- dn$Time %within% asc$time
                    shifted_tensor = shifted_tensor[, ind, ,
                      , drop = FALSE]
                    dn$Time = as.numeric(dimnames(shifted_tensor)$Time)
                  }
                  if (times_to_export == tmet$CLP_AWO) {
                    tmp = ravetools::collapse(shifted_tensor,
                      keep = c(1, 3:4))
                    dim(tmp) = c(dim(tmp), 1)
                    shifted_tensor <- aperm(tmp, c(1, 4, 2, 3))
                    dn$Time = asc$label
                    dimnames(shifted_tensor) = dn
                  }
                  fet = frequency_export_types()
                  if (frequencies_to_export %in% c(fet$CLP_AWO,
                    fet$RAW_AWO)) {
                    ff <- dn$Frequency %within% asc$frequency
                    shifted_tensor = shifted_tensor[ff, , , ,
                      drop = FALSE]
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
                  if (length(attr_TrialLabel$Condition) == length(dn$Trial) &&
                    all(0 == (dn$Trial - attr_TrialLabel$Trial))) {
                    attr(shifted_tensor, "TrialLabel") = attr_TrialLabel$Condition
                    attr(shifted_tensor, "OrigTrialLabel") = attr_TrialLabel$OrigCondition
                  } else {
                    ravedash::logger(level = "warning", "Could not apply trial labels. Length mismatch between labels (",
                      nrow(attr_TrialLabel), ") and  trials (",
                      length(dn$Trial), "), or trial numbers didn't line up")
                  }
                  return(shifted_tensor)
                })
                uoa = get_unit_of_analysis_varname(baseline_settings$unit_of_analysis)
                if (electrode_export_data_type == "tensor") {
                  data_for_export = mapply(function(tensor, asc) {
                    dn <- dimnames(tensor)
                    dn %<>% lapply(function(d) {
                      nd <- suppressWarnings(as.numeric(d))
                      if (any(is.na(nd))) return(d)
                      nd
                    })
                    res <- list(data = tensor)
                    res[names(dn)] = dn
                    res$unit = uoa
                    res$baseline_window = baseline_settings$window[[1]]
                    res$baseline_scope = baseline_settings$scope[[1]]
                    if (!is.null(attributes(tensor)[["TrialLabel"]])) {
                      res$TrialLabel = attr(tensor, "TrialLabel")
                      res$OrigTrialLabel = attr(tensor, "OrigTrialLabel")
                    }
                    return(res)
                  }, tensors, analysis_settings_clean, SIMPLIFY = FALSE)
                  names(data_for_export) = names(analysis_settings_clean)
                  data_for_export$data_names = names(data_for_export)
                  data_for_export$type = "tensor_data"
                } else {
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
                  all_elecs <- as.integer(unique(sapply(tensors,
                    function(tn) dimnames(tn)$Electrode)))
                  et <- subset(repository$electrode_table, Electrode %in%
                    all_elecs)
                  flat_tables %<>% merge(et)
                  data_for_export = list(type = "flat_data",
                    data_names = "all_data", all_data = list(data = flat_tables),
                    metadata = list(unit = uoa, baseline_window = paste0(collapse = ":",
                      baseline_settings$window[[1]]), baseline_scope = baseline_settings$scope[[1]]))
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_for_export)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "data_for_export",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "data_for_export", target_expr = quote({
                {
                  warning("Overlapping time/frequency windows will not be coded properly in the export file")
                  if (getOption("knit_rave_pipelines", default = FALSE)) {
                    list2env(list(electrodes_to_export = repository$power$dimnames$Electrode[1]),
                      envir = environment())
                  } else {
                  }
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
                    signal_type = "LFP", electrodes = electrodes_to_keep)
                  tensors <- lapply(analysis_settings_clean,
                    function(asc) {
                      ravedash::logger("Working on ", asc$label)
                      current_tensor = subset(repository$power$baselined,
                        Electrode ~ Electrode %in% electrodes_to_keep)
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
                      dn = lapply(dimnames(shifted_tensor), as.numeric)
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
                        shifted_tensor = shifted_tensor[ff, ,
                          , , drop = FALSE]
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
                        ravedash::logger(level = "warning", "Could not apply trial labels. Length mismatch between labels (",
                          nrow(attr_TrialLabel), ") and  trials (",
                          length(dn$Trial), "), or trial numbers didn't line up")
                      }
                      return(shifted_tensor)
                    })
                  uoa = get_unit_of_analysis_varname(baseline_settings$unit_of_analysis)
                  if (electrode_export_data_type == "tensor") {
                    data_for_export = mapply(function(tensor,
                      asc) {
                      dn <- dimnames(tensor)
                      dn %<>% lapply(function(d) {
                        nd <- suppressWarnings(as.numeric(d))
                        if (any(is.na(nd))) return(d)
                        nd
                      })
                      res <- list(data = tensor)
                      res[names(dn)] = dn
                      res$unit = uoa
                      res$baseline_window = baseline_settings$window[[1]]
                      res$baseline_scope = baseline_settings$scope[[1]]
                      if (!is.null(attributes(tensor)[["TrialLabel"]])) {
                        res$TrialLabel = attr(tensor, "TrialLabel")
                        res$OrigTrialLabel = attr(tensor, "OrigTrialLabel")
                      }
                      return(res)
                    }, tensors, analysis_settings_clean, SIMPLIFY = FALSE)
                    names(data_for_export) = names(analysis_settings_clean)
                    data_for_export$data_names = names(data_for_export)
                    data_for_export$type = "tensor_data"
                  } else {
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
                    all_elecs <- as.integer(unique(sapply(tensors,
                      function(tn) dimnames(tn)$Electrode)))
                    et <- subset(repository$electrode_table,
                      Electrode %in% all_elecs)
                    flat_tables %<>% merge(et)
                    data_for_export = list(type = "flat_data",
                      data_names = "all_data", all_data = list(data = flat_tables),
                      metadata = list(unit = uoa, baseline_window = paste0(collapse = ":",
                        baseline_settings$window[[1]]), baseline_scope = baseline_settings$scope[[1]]))
                  }
                }
                data_for_export
            }), target_depends = c("repository", "electrodes_to_export",
            "electrodes_to_export_roi_name", "electrodes_to_export_roi_categories",
            "baseline_settings", "analysis_settings_clean", "trials_to_export",
            "analysis_groups", "trial_outliers_list", "condition_variable",
            "times_to_export", "frequencies_to_export", "electrode_export_data_type"
            )), deps = c("repository", "electrodes_to_export",
        "electrodes_to_export_roi_name", "electrodes_to_export_roi_categories",
        "baseline_settings", "analysis_settings_clean", "trials_to_export",
        "analysis_groups", "trial_outliers_list", "condition_variable",
        "times_to_export", "frequencies_to_export", "electrode_export_data_type"
        ), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"))
