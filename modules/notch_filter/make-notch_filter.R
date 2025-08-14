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
        }), deps = "settings"), input_notch_filter_upperbound = targets::tar_target_raw("notch_filter_upperbound", 
        quote({
            settings[["notch_filter_upperbound"]]
        }), deps = "settings"), input_notch_filter_lowerbound = targets::tar_target_raw("notch_filter_lowerbound", 
        quote({
            settings[["notch_filter_lowerbound"]]
        }), deps = "settings"), input_diagnostic_plot_path = targets::tar_target_raw("diagnostic_plot_path", 
        quote({
            settings[["diagnostic_plot_path"]]
        }), deps = "settings"), input_diagnostic_plot_params = targets::tar_target_raw("diagnostic_plot_params", 
        quote({
            settings[["diagnostic_plot_params"]]
        }), deps = "settings"), input_channel_types = targets::tar_target_raw("channel_types", 
        quote({
            settings[["channel_types"]]
        }), deps = "settings"), input_background = targets::tar_target_raw("background", 
        quote({
            settings[["background"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- ravecore::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code, strict = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "rave-subject", 
            target_export = "subject", target_expr = quote({
                {
                  subject <- ravecore::RAVESubject$new(project_name = project_name, 
                    subject_code = subject_code, strict = FALSE)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), check_imported_electrodes = targets::tar_target_raw(name = "imported_electrodes", 
        command = quote({
            .__target_expr__. <- quote({
                imported_electrodes <- subject$electrodes[subject$preprocess_settings$data_imported]
                if (!length(imported_electrodes)) {
                  stop("The subject exists but its signal has not been imported yet.")
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(imported_electrodes)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "imported_electrodes", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "imported_electrodes", target_expr = quote({
                {
                  imported_electrodes <- subject$electrodes[subject$preprocess_settings$data_imported]
                  if (!length(imported_electrodes)) {
                    stop("The subject exists but its signal has not been imported yet.")
                  }
                }
                imported_electrodes
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    check_filter_settings = targets::tar_target_raw(name = "filter_settings", 
        command = quote({
            .__target_expr__. <- quote({
                lb <- unlist(notch_filter_lowerbound)
                ub <- unlist(notch_filter_upperbound)
                if (length(lb) != length(ub)) {
                  stop(sprintf("Notch filter lower bound length should match with the upper bound length (%d vs %d)", 
                    length(lb), length(ub)))
                }
                if (length(lb)) {
                  if (!all(lb < ub)) {
                    sel <- lb >= ub
                    lb <- lb[sel]
                    ub <- ub[sel]
                    stop("Notch filter lower bounds must be uniformly smaller than the upper bounds: (", 
                      paste0(lb, ">", ub, collapse = ", "), ")")
                  }
                }
                filter_settings <- list(lb = lb, ub = ub, domain = 1)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(filter_settings)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "filter_settings", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "filter_settings", target_expr = quote({
                {
                  lb <- unlist(notch_filter_lowerbound)
                  ub <- unlist(notch_filter_upperbound)
                  if (length(lb) != length(ub)) {
                    stop(sprintf("Notch filter lower bound length should match with the upper bound length (%d vs %d)", 
                      length(lb), length(ub)))
                  }
                  if (length(lb)) {
                    if (!all(lb < ub)) {
                      sel <- lb >= ub
                      lb <- lb[sel]
                      ub <- ub[sel]
                      stop("Notch filter lower bounds must be uniformly smaller than the upper bounds: (", 
                        paste0(lb, ">", ub, collapse = ", "), 
                        ")")
                    }
                  }
                  filter_settings <- list(lb = lb, ub = ub, domain = 1)
                }
                filter_settings
            }), target_depends = c("notch_filter_lowerbound", 
            "notch_filter_upperbound")), deps = c("notch_filter_lowerbound", 
        "notch_filter_upperbound"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), check_electrode_channels_to_filter = targets::tar_target_raw(name = "channels_to_apply_filters", 
        command = quote({
            .__target_expr__. <- quote({
                channels_to_apply_filters <- imported_electrodes
                channel_types_lower <- tolower(channel_types)
                signal_types <- ravecore::SIGNAL_TYPES
                signal_types <- signal_types[tolower(signal_types) %in% 
                  channel_types_lower]
                channels <- subject$electrodes[subject$electrode_types %in% 
                  signal_types]
                channels_to_apply_filters <- channels_to_apply_filters[channels_to_apply_filters %in% 
                  channels]
            })
            tryCatch({
                eval(.__target_expr__.)
                return(channels_to_apply_filters)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "channels_to_apply_filters", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "channels_to_apply_filters", target_expr = quote({
                {
                  channels_to_apply_filters <- imported_electrodes
                  channel_types_lower <- tolower(channel_types)
                  signal_types <- ravecore::SIGNAL_TYPES
                  signal_types <- signal_types[tolower(signal_types) %in% 
                    channel_types_lower]
                  channels <- subject$electrodes[subject$electrode_types %in% 
                    signal_types]
                  channels_to_apply_filters <- channels_to_apply_filters[channels_to_apply_filters %in% 
                    channels]
                }
                channels_to_apply_filters
            }), target_depends = c("imported_electrodes", "channel_types", 
            "subject")), deps = c("imported_electrodes", "channel_types", 
        "subject"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), apply_Notch_filters = targets::tar_target_raw(name = "apply_notch", 
        command = quote({
            .__target_expr__. <- quote({
                blocks <- subject$preprocess_settings$blocks
                electrodes <- imported_electrodes
                filters <- filter_settings
                fmt <- file.path(subject$preprocess_path, "voltage", 
                  "electrode_%d.h5")
                sample_rates <- subject$raw_sample_rates
                sample_rates <- sapply(electrodes, function(e) {
                  sample_rates[subject$electrodes == e]
                })
                ravepipeline::lapply_jobs(seq_along(electrodes), 
                  function(ii) {
                    e <- electrodes[[ii]]
                    srate <- sample_rates[[ii]]
                    h5_path <- sprintf(fmt, e)
                    h5_names <- gsub("^/", "", ieegio::io_h5_names(h5_path))
                    sel <- sprintf("raw/%s", blocks) %in% h5_names
                    if (!all(sel)) {
                      stop(sprintf("Cannot find imported block(s): %s (electrode %s)", 
                        blocks[!sel], e))
                    }
                    for (block in blocks) {
                      signal <- ieegio::io_read_h5(h5_path, sprintf("raw/%s", 
                        block), ram = TRUE)
                      if (length(filters$lb) && e %in% channels_to_apply_filters) {
                        signal <- ravetools::notch_filter(s = signal, 
                          sample_rate = srate, lb = filters$lb, 
                          ub = filters$ub, domain = filters$domain)
                      }
                      ieegio::io_write_h5(x = signal, file = h5_path, 
                        name = sprintf("notch/%s", block), chunk = 1024, 
                        replace = TRUE, ctype = "numeric")
                    }
                  }, .globals = list(electrodes = electrodes, 
                    sample_rates = sample_rates, fmt = fmt, blocks = blocks, 
                    channels_to_apply_filters = channels_to_apply_filters, 
                    filters = filters), callback = function(ii) {
                    sprintf("Applying Notch filters|Electrode - %s", 
                      electrodes[[ii]])
                  })
                preproc <- ravecore::RAVEPreprocessSettings$new(subject = subject$subject_id)
                for (e in electrodes) {
                  preproc$data[[as.character(e)]]$notch_filtered <- TRUE
                  preproc$data[[as.character(e)]]$has_wavelet <- FALSE
                }
                preproc$save()
                apply_notch <- list(electrodes = electrodes, 
                  filter_applied = channels_to_apply_filters, 
                  notch_filter_lowerbound = filters$lb, notch_filter_upperbound = filters$ub, 
                  timestamp = strftime(Sys.time(), usetz = TRUE))
                subject$set_default(namespace = "notch_filter", 
                  key = "parameters", value = apply_notch)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(apply_notch)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "apply_notch", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "apply_notch", target_expr = quote({
                {
                  blocks <- subject$preprocess_settings$blocks
                  electrodes <- imported_electrodes
                  filters <- filter_settings
                  fmt <- file.path(subject$preprocess_path, "voltage", 
                    "electrode_%d.h5")
                  sample_rates <- subject$raw_sample_rates
                  sample_rates <- sapply(electrodes, function(e) {
                    sample_rates[subject$electrodes == e]
                  })
                  ravepipeline::lapply_jobs(seq_along(electrodes), 
                    function(ii) {
                      e <- electrodes[[ii]]
                      srate <- sample_rates[[ii]]
                      h5_path <- sprintf(fmt, e)
                      h5_names <- gsub("^/", "", ieegio::io_h5_names(h5_path))
                      sel <- sprintf("raw/%s", blocks) %in% h5_names
                      if (!all(sel)) {
                        stop(sprintf("Cannot find imported block(s): %s (electrode %s)", 
                          blocks[!sel], e))
                      }
                      for (block in blocks) {
                        signal <- ieegio::io_read_h5(h5_path, 
                          sprintf("raw/%s", block), ram = TRUE)
                        if (length(filters$lb) && e %in% channels_to_apply_filters) {
                          signal <- ravetools::notch_filter(s = signal, 
                            sample_rate = srate, lb = filters$lb, 
                            ub = filters$ub, domain = filters$domain)
                        }
                        ieegio::io_write_h5(x = signal, file = h5_path, 
                          name = sprintf("notch/%s", block), 
                          chunk = 1024, replace = TRUE, ctype = "numeric")
                      }
                    }, .globals = list(electrodes = electrodes, 
                      sample_rates = sample_rates, fmt = fmt, 
                      blocks = blocks, channels_to_apply_filters = channels_to_apply_filters, 
                      filters = filters), callback = function(ii) {
                      sprintf("Applying Notch filters|Electrode - %s", 
                        electrodes[[ii]])
                    })
                  preproc <- ravecore::RAVEPreprocessSettings$new(subject = subject$subject_id)
                  for (e in electrodes) {
                    preproc$data[[as.character(e)]]$notch_filtered <- TRUE
                    preproc$data[[as.character(e)]]$has_wavelet <- FALSE
                  }
                  preproc$save()
                  apply_notch <- list(electrodes = electrodes, 
                    filter_applied = channels_to_apply_filters, 
                    notch_filter_lowerbound = filters$lb, notch_filter_upperbound = filters$ub, 
                    timestamp = strftime(Sys.time(), usetz = TRUE))
                  subject$set_default(namespace = "notch_filter", 
                    key = "parameters", value = apply_notch)
                }
                apply_notch
            }), target_depends = c("subject", "imported_electrodes", 
            "filter_settings", "channels_to_apply_filters")), 
        deps = c("subject", "imported_electrodes", "filter_settings", 
        "channels_to_apply_filters"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), generate_diagnostic_plots = targets::tar_target_raw(name = "diagnostic_plots", 
        command = quote({
            .__target_expr__. <- quote({
                diagnostic_plots <- FALSE
                force(subject)
                if (length(diagnostic_plot_path) == 1) {
                  if (startsWith(diagnostic_plot_path, "{")) {
                    diagnostic_plot_path <- glue::glue(diagnostic_plot_path)
                  }
                  ravepipeline::dir_create2(dirname(diagnostic_plot_path))
                } else {
                  diagnostic_plot_path <- NULL
                }
                params <- as.list(diagnostic_plot_params)
                params$path <- diagnostic_plot_path
                diagnostic_plots <- diagnostic_plot_path
                if (!isTRUE(params$dry_run)) {
                  background <- params$background
                  foreground <- params$foreground
                  if (!length(background) == 1) {
                    background <- "white"
                  }
                  if (!length(foreground) == 1) {
                    foreground <- "black"
                  }
                  if (length(params$path) == 1) {
                    grDevices::pdf(file = params$path, width = 12, 
                      height = 7, onefile = TRUE, bg = background, 
                      fg = foreground, useDingbats = FALSE)
                    on.exit({
                      grDevices::dev.off()
                    }, add = TRUE)
                  }
                  winlen <- as.numeric(params$window_length)
                  if (!length(winlen) || is.na(winlen)) {
                    winlen <- "auto"
                  }
                  max_freq <- as.numeric(params$max_frequency)
                  if (!length(max_freq) || is.na(max_freq)) {
                    max_freq <- 300
                  }
                  nbins <- as.numeric(params$histogram_bins)
                  if (!length(nbins) || is.na(nbins)) {
                    nbins <- 50
                  }
                  font_size <- as.numeric(params$font_size)
                  if (!length(font_size) || is.na(font_size)) {
                    font_size <- 2
                  }
                  quiet <- isTRUE(params$quiet)
                  diagnose_notch_filters(subject = subject, electrodes = imported_electrodes, 
                    max_freq = max_freq, winlen = winlen, nbins = nbins, 
                    bg = background, fg = foreground, cex = font_size, 
                    std = 3, lwd = 0.3, quiet = quiet)
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(diagnostic_plots)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "diagnostic_plots", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "diagnostic_plots", target_expr = quote({
                {
                  diagnostic_plots <- FALSE
                  force(subject)
                  if (length(diagnostic_plot_path) == 1) {
                    if (startsWith(diagnostic_plot_path, "{")) {
                      diagnostic_plot_path <- glue::glue(diagnostic_plot_path)
                    }
                    ravepipeline::dir_create2(dirname(diagnostic_plot_path))
                  } else {
                    diagnostic_plot_path <- NULL
                  }
                  params <- as.list(diagnostic_plot_params)
                  params$path <- diagnostic_plot_path
                  diagnostic_plots <- diagnostic_plot_path
                  if (!isTRUE(params$dry_run)) {
                    background <- params$background
                    foreground <- params$foreground
                    if (!length(background) == 1) {
                      background <- "white"
                    }
                    if (!length(foreground) == 1) {
                      foreground <- "black"
                    }
                    if (length(params$path) == 1) {
                      grDevices::pdf(file = params$path, width = 12, 
                        height = 7, onefile = TRUE, bg = background, 
                        fg = foreground, useDingbats = FALSE)
                      on.exit({
                        grDevices::dev.off()
                      }, add = TRUE)
                    }
                    winlen <- as.numeric(params$window_length)
                    if (!length(winlen) || is.na(winlen)) {
                      winlen <- "auto"
                    }
                    max_freq <- as.numeric(params$max_frequency)
                    if (!length(max_freq) || is.na(max_freq)) {
                      max_freq <- 300
                    }
                    nbins <- as.numeric(params$histogram_bins)
                    if (!length(nbins) || is.na(nbins)) {
                      nbins <- 50
                    }
                    font_size <- as.numeric(params$font_size)
                    if (!length(font_size) || is.na(font_size)) {
                      font_size <- 2
                    }
                    quiet <- isTRUE(params$quiet)
                    diagnose_notch_filters(subject = subject, 
                      electrodes = imported_electrodes, max_freq = max_freq, 
                      winlen = winlen, nbins = nbins, bg = background, 
                      fg = foreground, cex = font_size, std = 3, 
                      lwd = 0.3, quiet = quiet)
                  }
                }
                diagnostic_plots
            }), target_depends = c("subject", "diagnostic_plot_path", 
            "diagnostic_plot_params", "imported_electrodes")), 
        deps = c("subject", "diagnostic_plot_path", "diagnostic_plot_params", 
        "imported_electrodes"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"))
