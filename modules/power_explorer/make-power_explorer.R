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
        load_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")),
    input_analysis_settings = targets::tar_target_raw("analysis_settings",
        quote({
            settings[["analysis_settings"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name",
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_baseline_settings = targets::tar_target_raw("baseline_settings",
        quote({
            settings[["baseline_settings"]]
        }), deps = "settings"), input_electrodes_list = targets::tar_target_raw("electrodes_list",
        quote({
            settings[["electrodes_list"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_first_condition_groupings = targets::tar_target_raw("first_condition_groupings",
        quote({
            settings[["first_condition_groupings"]]
        }), deps = "settings"), input_analysis_electrodes__category = targets::tar_target_raw("analysis_electrodes__category",
        quote({
            settings[["analysis_electrodes__category"]]
        }), deps = "settings"), input_selected_electrodes = targets::tar_target_raw("selected_electrodes",
        quote({
            settings[["selected_electrodes"]]
        }), deps = "settings"), input_epoch_choice = targets::tar_target_raw("epoch_choice",
        quote({
            settings[["epoch_choice"]]
        }), deps = "settings"), input_time_censor = targets::tar_target_raw("time_censor",
        quote({
            settings[["time_censor"]]
        }), deps = "settings"), input_subject_name = targets::tar_target_raw("subject_name",
        quote({
            settings[["subject_name"]]
        }), deps = "settings"), input_tensor_collapse_method = targets::tar_target_raw("tensor_collapse_method",
        quote({
            settings[["tensor_collapse_method"]]
        }), deps = "settings"), input_electrode_groupings = targets::tar_target_raw("electrode_groupings",
        quote({
            settings[["electrode_groupings"]]
        }), deps = "settings"), input_trial_outliers_list = targets::tar_target_raw("trial_outliers_list",
        quote({
            settings[["trial_outliers_list"]]
        }), deps = "settings"), input_epoch_choice__trial_ends = targets::tar_target_raw("epoch_choice__trial_ends",
        quote({
            settings[["epoch_choice__trial_ends"]]
        }), deps = "settings"), input_epoch_choice__trial_starts = targets::tar_target_raw("epoch_choice__trial_starts",
        quote({
            settings[["epoch_choice__trial_starts"]]
        }), deps = "settings"), check_load_power = targets::tar_target_raw(name = "repository",
        command = quote({
            {
                proj_subj <- sprintf("%s/%s", project_name, subject_name)
                repository <- raveio::prepare_subject_power(subject = proj_subj,
                  electrodes = electrodes_list, epoch_name = epoch_choice,
                  reference_name = reference_name, time_windows = c(epoch_choice__trial_starts,
                    epoch_choice__trial_ends))
                repository
            }
            return(repository)
        }), deps = c("project_name", "subject_name", "electrodes_list",
        "epoch_choice", "reference_name", "epoch_choice__trial_starts",
        "epoch_choice__trial_ends"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), check_requested_electrodes = targets::tar_target_raw(name = "requested_electrodes",
        command = quote({
            {
                requested_electrodes <- dipsaus::parse_svec(selected_electrodes,
                  sep = ",|;", connect = ":-")
                requested_electrodes <- requested_electrodes[requested_electrodes %in%
                  repository$power$dimnames$Electrode]
                if (!length(requested_electrodes)) {
                  stop("No electrode selected")
                }
            }
            return(requested_electrodes)
        }), deps = c("selected_electrodes", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), check_analysis_settings = targets::tar_target_raw(name = "analysis_checks_passed",
        command = quote({
            .__target_expr__. <- quote({
                check_range <- function(x, lim, lbl) {
                  if (!all(x %within% lim)) stop(sprintf("Requested %s [%s] not within available range [%s]", 
                    lbl, str_collapse(range(x), ":"), str_collapse(range(lim), 
                      ":")), call. = FALSE)
                }
                if (length(repository$time_windows) != 1) stop("discontinuous time windows not supported")
                analysis_settings %<>% lapply(function(as) {
                  as$time %<>% unlist
                  as$frequency %<>% unlist
                  if (is.null(as$label) || nchar(as$label) <
                    1) {
                    as$label <- paste("Window", stri_rand_strings(1,
                      4))
                  }
                  if (is.null(as$censor_info)) {
                    as$censor_info <- list(enabled = FALSE, window = 0:1)
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
                sapply(analysis_settings, function(setting) {
                  check_range(setting$frequency, unlist(repository$frequency),
                    "frequency")
                  check_range(setting$time, unlist(repository$time_windows), 
                    "analysis time")
                })
                names(analysis_settings) <- sapply(analysis_settings,
                  `[[`, "label")
                dd <- duplicated(sapply(analysis_settings, `[[`,
                  "label"))
                while (sum(dd)) {
                  for (w in which(dd)) {
                    analysis_settings[[w]]$label = paste(analysis_settings[[w]]$label,
                      stringi::stri_rand_strings(n = 1, length = 4))
                  }
                  dd <- duplicated(sapply(analysis_settings,
                    `[[`, "label"))
                }
                for (ii in seq_along(analysis_settings)) {
                  analysis_settings[[ii]]$censor_info = time_censor
                  analysis_settings[[ii]]$censor_info$window %<>%
                    unlist
                }
                for (ii in seq_along(first_condition_groupings)) {
                  if (nchar(first_condition_groupings[[ii]]$label) <
                    1) {
                    first_condition_groupings[[ii]]$label = paste("Group",
                      ii)
                  }
                }
                dd <- duplicated(sapply(first_condition_groupings,
                  `[[`, "label"))
                while (sum(dd)) {
                  for (w in which(dd)) {
                    first_condition_groupings[[w]]$label = paste(first_condition_groupings[[w]]$label,
                      stringi::stri_rand_strings(n = 1, length = 4))
                  }
                  dd <- duplicated(sapply(first_condition_groupings,
                    `[[`, "label"))
                }
                if (is.list(trial_outliers_list)) {
                  trial_outliers_list %<>% unlist
                }
                analysis_checks_passed = TRUE
            }
            return(analysis_checks_passed)
        }), deps = c("repository", "analysis_settings", "baseline_settings",
        "time_censor", "first_condition_groupings", "trial_outliers_list"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), calculate_baseline = targets::tar_target_raw(name = "baselined_power",
        command = quote({
            {
                stopifnot(analysis_checks_passed)
                raveio::with_future_parallel({
                  raveio::power_baseline(x = repository, baseline_windows = unlist(baseline_settings$window[[1]]),
                    method = get_unit_of_analysis(baseline_settings$unit_of_analysis),
                    units = get_baseline_scope(baseline_settings$scope),
                    signal_type = "LFP", electrodes = requested_electrodes)
                })
                baselined_power <- subset(repository$power$baselined,
                  Electrode ~ Electrode %in% requested_electrodes)
            }
            return(baselined_power)
        }), deps = c("analysis_checks_passed", "repository",
        "baseline_settings", "requested_electrodes"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), build_trial_groupings = targets::tar_target_raw(name = "analysis_groups",
        command = quote({
            {
                analysis_groups <- mapply(function(cg, ii) {
                  trials <- c()
                  if (length(cg$conditions) > 0) {
                    trials <- repository$epoch$table$Trial[repository$epoch$table$Condition %in%
                      cg$conditions]
                  }
                  list(label = cg$label, conditions = cg$conditions,
                    trials = trials, index = ii, has_trials = length(trials) >
                      0, electrodes = requested_electrodes)
                }, first_condition_groupings, seq_along(first_condition_groupings),
                  SIMPLIFY = FALSE)
                names(analysis_groups) <- sapply(analysis_groups,
                  `[[`, "label")
                if (!any(vapply(analysis_groups, `[[`, FALSE,
                  "has_trials"))) stop("No trials available in condition groups")
            }
            return(analysis_groups)
        }), deps = c("repository", "requested_electrodes", "first_condition_groupings"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), build_pluriform_power = targets::tar_target_raw(name = "pluriform_power",
        command = quote({
            {
                epoch_event_types = get_available_events(repository$epoch$columns)
                pluriform_power <- sapply(analysis_groups, function(ag) {
                  sapply(analysis_settings, function(as) {
                    p <- get_pluriform_power(baselined_data = baselined_power,
                      trial_indices = ag$trials, events = repository$epoch$table,
                      epoch_event_types = epoch_event_types,
                      trial_outliers_list = unlist(trial_outliers_list),
                      event_of_interest = as$event, )
                    list(data = p, settings = as)
                  }, simplify = FALSE, USE.NAMES = TRUE)
                }, simplify = FALSE, USE.NAMES = TRUE)
                for (gg in seq_along(pluriform_power)) {
                  for (aa in seq_along(pluriform_power[[gg]])) {
                    fi <- as.numeric(dimnames(pluriform_power[[gg]][[aa]]$data$shifted_data)$Frequency) %within%
                      unlist(pluriform_power[[gg]][[aa]]$settings$frequency)
                    pluriform_power[[gg]][[aa]]$data$shifted_data_Fsub <- pluriform_power[[gg]][[aa]]$data$shifted_data[fi,
                      , , , drop = FALSE]
                    pluriform_power[[gg]][[aa]]$data$shifted_clean_data_Fsub = pluriform_power[[gg]][[aa]]$data$shifted_clean_data[fi,
                      , , , drop = FALSE]
                  }
                }
            }
            return(pluriform_power)
        }), deps = c("repository", "analysis_groups", "analysis_settings",
        "baselined_power", "trial_outliers_list"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_overall_tf_data = targets::tar_target_raw(name = "overall_tf_data",
        command = quote({
            {
                build_tfd <- function(dd, trial_groups, settings) {
                  res <- list(data = ravetools::collapse(dd,
                    keep = 2:1), xlab = "Time (s)", ylab = "Frequency",
                    zlab = "Mean " %&% baseline_settings$unit_of_analysis)
                  res$x = as.numeric(dimnames(dd)$Time)
                  res$y = as.numeric(dimnames(dd)$Frequency)
                  res$N = dim(dd)[4L]
                  res$name = trial_groups$label
                  res$settings = settings
                  if (isTRUE(settings$censor_info$enabled)) {
                    ti = res$x %within% settings$censor_info$window
                    res$range <- range(res$data[!ti, ])
                  } else {
                    res$range <- range(res$data)
                  }
                  return(res)
                }
                overall_tf_data <- mapply(function(pp, ag) {
                  if (length(unique(sapply(pp, function(pi) pi$settings$event))) ==
                    1) {
                    .settings = list(A = pp[[1]]$settings)
                    for (ii in seq_along(pp[-1])) {
                      .settings[[LETTERS[ii + 1]]] = pp[[ii +
                        1]]$settings
                    }
                    names(.settings) <- names(pp)
                    build_tfd(dd = pp[[1]]$data$shifted_clean_data,
                      trial_groups = ag, settings = .settings)
                  } else {
                    sapply(pp, function(ppa) {
                      build_tfd(ppa$data$shifted_clean_data,
                        trial_groups = ag, ppa$settings)
                    }, simplify = FALSE, USE.NAMES = TRUE)
                  }
                }, pluriform_power, analysis_groups, SIMPLIFY = FALSE)
            }
            return(overall_tf_data)
        }), deps = c("baseline_settings", "pluriform_power",
        "analysis_groups"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_tf_correlation_data = targets::tar_target_raw(name = "tf_correlation_data",
        command = quote({
            {
                if (length(analysis_settings) == 1) {
                  tf_correlation_data <- vector("list", length = length(overall_tf_data))
                  for (ii in seq_along(tf_correlation_data)) {
                    d <- pluriform_power[[ii]][[1]]$data$shifted_clean_data
                    tm <- as.numeric(dimnames(d)$Time)
                    ti <- tm %within% unlist(pluriform_power[[ii]][[1]]$settings$time)
                    if (isTRUE(pluriform_power[[ii]][[1]]$settings$censor_info$enabled)) {
                      ti = ti & (tm %within% unlist(pluriform_power[[ii]][[1]]$settings$censor_info$window))
                    }
                    stopifnot(c("Frequency", "Time") == names(dimnames(d))[1:2])
                    d_collapsed <- ravetools::collapse(d[, ti,
                      , , drop = FALSE], keep = c(2, 1))
                    tf_correlation_data[[ii]] <- list(data = cor(d_collapsed),
                      xlab = "", ylab = "", zlab = "Pearson correlation",
                      x = as.numeric(dimnames(d)[[1]]), y = as.numeric(dimnames(d)[[1]]))
                  }
                } else {
                  stop(">1 analysis not yet support for tf_correlation_data")
                }
            }
            return(tf_correlation_data)
        }), deps = c("analysis_settings", "overall_tf_data",
        "pluriform_power"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), build_by_trial_tf_data = targets::tar_target_raw(name = "by_trial_tf_data",
        command = quote({
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
            }
            return(by_trial_tf_data)
        }), deps = c("baseline_settings", "pluriform_power"),
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
    build_by_electrode_tf_data = targets::tar_target_raw(name = "by_electrode_tf_data",
        command = quote({
            {
                build_data <- function(dd, settings) {
                  to_keep <- sapply(c("Time", "Electrode"), which.equal,
                    names(dimnames(dd)))
                  res <- list(data = ravetools::collapse(dd,
                    keep = to_keep), xlab = "Time (s)", ylab = "Electrode #",
                    zlab = "Mean " %&% baseline_settings$unit_of_analysis)
                  res[c("x", "y")] <- dimnames(dd)[to_keep] %>%
                    lapply(as.numeric)
                  res$N = length(dimnames(dd)$Trial)
                  if (isTRUE(settings$censor_info$enabled)) {
                    ti = res$x %within% settings$censor_info$window
                    res$range <- range(res$data[!ti, ])
                  } else {
                    res$range <- range(res$data)
                  }
                  return(res)
                }
                by_electrode_tf_data <- lapply(pluriform_power,
                  function(pp) {
                    if (length(pp) == 1 || all(1 == length(table(sapply(pp,
                      function(pi) pi$settings$event))), 1 ==
                      length(table(sapply(pp, function(pi) str_collapse(pi$settings$frequency)))))) {
                      build_data(dd = pp[[1]]$data$shifted_clean_data_Fsub,
                        settings = pp[[1]]$settings)
                    } else {
                      sapply(pp, function(ppa) {
                        build_data(ppa$data$shifted_clean_data_Fsub,
                          ppa$settings)
                      }, simplify = FALSE, USE.NAMES = TRUE)
                    }
                  })
            }
            return(by_electrode_tf_data)
        }), deps = c("baseline_settings", "pluriform_power"),
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
    build_over_time_data = targets::tar_target_raw(name = "over_time_data",
        command = quote({
            {
                build_data <- function(dd, settings) {
                  to_keep <- sapply(c("Time", "Electrode"), which.equal, 
                    names(dimnames(dd)))
                  res <- list(data = ravetools::collapse(dd, 
                    keep = to_keep), xlab = "Time (s)", ylab = "Mean " %&% 
                    baseline_settings$unit_of_analysis, zlab = NA)
                  res$data <- cbind(.rowMeans(res$data, nrow(res$data),
                    ncol(res$data)), sqrt(diag(fastcov2(t(res$data)))/ncol(res$data)))
                  res$x <- as.numeric(dimnames(dd)$Time)
                  res$y <- NA
                  res$N = length(dimnames(dd)$Electrode)
                  if (isTRUE(settings$censor_info$enabled)) {
                    ti = res$x %within% settings$censor_info$window
                    res$range <- range(plus_minus(res$data[!ti,
                      ]))
                  } else {
                    res$range <- range(rutabaga::plus_minus(res$data))
                  }
                  return(res)
                }
                over_time_data <- lapply(pluriform_power, function(pp) {
                  if (length(pp) == 1 || all(1 == length(table(sapply(pp,
                    function(pi) pi$settings$event))), 1 == length(table(sapply(pp,
                    function(pi) str_collapse(pi$settings$frequency)))))) {
                    build_data(pp[[1]]$data$shifted_clean_data_Fsub,
                      pp[[1]]$settings)
                  } else {
                    sapply(pp, function(ppa) {
                      build_data(ppa$data$shifted_clean_data_Fsub,
                        ppa$settings)
                    }, simplify = FALSE, USE.NAMES = TRUE)
                  }
                })
            }
            return(over_time_data)
        }), deps = c("baseline_settings", "pluriform_power"),
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
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
                scatter_bar_data <- lapply(pluriform_power, function(pp) {
                  sapply(pp, function(ppa) {
                    build_data(ppa$data$shifted_clean_data_Fsub,
                      ppa$settings)
                  }, simplify = FALSE, USE.NAMES = TRUE)
                })
            }
            return(scatter_bar_data)
        }), deps = c("baseline_settings", "pluriform_power"),
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
    build_analysis_data = targets::tar_target_raw(name = "analysis_data",
        command = quote({
            {
                has_data <- which(sapply(analysis_groups, `[[`,
                  "has_trials"))
                analysis_data <- list()
                analysis_data$datatype <- baseline_settings$unit_of_analysis
            }
            return(analysis_data)
        }), deps = c("analysis_groups", "baseline_settings"),
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"),
    build_omnibus_results = targets::tar_target_raw(name = "omnibus_results",
        command = quote({
            {
                omnibus_results <- 1
            }
            return(omnibus_results)
        }), deps = character(0), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"))
