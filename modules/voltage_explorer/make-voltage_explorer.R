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
        }), deps = "settings"), input_condition_groups = targets::tar_target_raw("condition_groups", 
        quote({
            settings[["condition_groups"]]
        }), deps = "settings"), input_analysis_ranges = targets::tar_target_raw("analysis_ranges", 
        quote({
            settings[["analysis_ranges"]]
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
        pattern = NULL, iteration = "list"), clean_analysis_electrodes = targets::tar_target_raw(name = "analysis_electrodes_clean", 
        command = quote({
            .__target_expr__. <- quote({
                available_channels <- repository$electrode_list
                analysis_electrodes_clean <- dipsaus::parse_svec(unlist(analysis_electrodes))
                analysis_electrodes_clean <- analysis_electrodes_clean[analysis_electrodes_clean %in% 
                  available_channels]
                if (!length(analysis_electrodes_clean)) {
                  stop("No channels selected for analysis. Please specify the electrode channels from the following loaded: ", 
                    dipsaus::deparse_svec(available_channels))
                }
                analysis_electrodes_clean <- as.integer(sort(analysis_electrodes_clean))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_electrodes_clean)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "analysis_electrodes_clean", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "analysis_electrodes_clean", target_expr = quote({
                {
                  available_channels <- repository$electrode_list
                  analysis_electrodes_clean <- dipsaus::parse_svec(unlist(analysis_electrodes))
                  analysis_electrodes_clean <- analysis_electrodes_clean[analysis_electrodes_clean %in% 
                    available_channels]
                  if (!length(analysis_electrodes_clean)) {
                    stop("No channels selected for analysis. Please specify the electrode channels from the following loaded: ", 
                      dipsaus::deparse_svec(available_channels))
                  }
                  analysis_electrodes_clean <- as.integer(sort(analysis_electrodes_clean))
                }
                analysis_electrodes_clean
            }), target_depends = c("repository", "analysis_electrodes"
            )), deps = c("repository", "analysis_electrodes"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    diagnose_filters = targets::tar_target_raw(name = "filter_freqz", 
        command = quote({
            .__target_expr__. <- quote({
                sample_rate <- repository$sample_rate
                time_points <- repository$voltage$dimnames$Time
                start_time <- min(time_points, na.rm = TRUE)
                n_timepoints <- length(time_points)
                new_srate <- sample_rate
                configs <- as.list(filter_configurations)
                filter <- list()
                pre_filter_decimate <- 1L
                post_filter_decimate <- 1L
                found_frequency_filter <- FALSE
                freqz_n <- 2^(floor(log2(n_timepoints/2)) + 1)
                xlim <- c(0, 10)
                for (cfg in configs) {
                  type <- cfg$type %||% ""
                  if (type %in% c("detrend", "demean", "baseline")) {
                    next
                  }
                  if (type == "decimate") {
                    by <- max(1L, as.integer(cfg$by %||% 1L))
                    if (!found_frequency_filter) {
                      pre_filter_decimate <- pre_filter_decimate * 
                        by
                      new_srate <- new_srate/by
                      n_timepoints <- ceiling(n_timepoints/by)
                    } else {
                      post_filter_decimate <- post_filter_decimate * 
                        by
                    }
                    next
                  }
                  if (type == "fir") {
                    type <- "fir_kaiser"
                  }
                  if (type == "iir") {
                    type <- "butter"
                  }
                  f <- tryCatch({
                    ravetools::design_filter(sample_rate = new_srate, 
                      filter_order = NA, data_size = n_timepoints, 
                      high_pass_freq = cfg$high_pass_freq %||% 
                        NA, high_pass_trans_freq = cfg$high_pass_trans_freq %||% 
                        NA, low_pass_freq = cfg$low_pass_freq %||% 
                        NA, low_pass_trans_freq = cfg$low_pass_trans_freq %||% 
                        NA, passband_ripple = cfg$passband_ripple %||% 
                        0.1, stopband_attenuation = cfg$stopband_attenuation %||% 
                        40, scale = TRUE, method = type)
                  }, error = function(e) {
                    stop("Could not design filter (type=", type, 
                      "): ", conditionMessage(e))
                    NULL
                  })
                  if (!is.null(f)) {
                    found_frequency_filter <- TRUE
                    freqz <- ravetools::freqz2(b = f$b, a = f$a, 
                      fs = new_srate, n = freqz_n, whole = FALSE)
                    filter[[length(filter) + 1L]] <- list(config = cfg, 
                      type = type, sample_rate = new_srate, b = f$b, 
                      a = f$a, frequency = freqz$w, response = freqz$h)
                    xlim <- range(c(xlim, c(f$parameters$stopband, 
                      f$parameters$passband) * new_srate/2), 
                      na.rm = TRUE)
                  }
                }
                if (length(filter)) {
                  combined_freq <- seq(0, new_srate/2, length.out = freqz_n)
                  combined_response <- rep(1 + (0+0i), length(combined_freq))
                  for (item in filter) {
                    combined_response <- combined_response * 
                      item$response
                  }
                  filter_freqz <- structure(class = c("ravetools-freqz2", 
                    "ravetools-printable"), list(w = combined_freq, 
                    h = combined_response, f = combined_freq, 
                    u = "Hz", srate = new_srate, pre_filter_decimate = pre_filter_decimate, 
                    post_filter_decimate = post_filter_decimate, 
                    original_srate = sample_rate, n = freqz_n, 
                    xlim = xlim))
                } else {
                  filter_freqz <- NULL
                }
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
                  sample_rate <- repository$sample_rate
                  time_points <- repository$voltage$dimnames$Time
                  start_time <- min(time_points, na.rm = TRUE)
                  n_timepoints <- length(time_points)
                  new_srate <- sample_rate
                  configs <- as.list(filter_configurations)
                  filter <- list()
                  pre_filter_decimate <- 1L
                  post_filter_decimate <- 1L
                  found_frequency_filter <- FALSE
                  freqz_n <- 2^(floor(log2(n_timepoints/2)) + 
                    1)
                  xlim <- c(0, 10)
                  for (cfg in configs) {
                    type <- cfg$type %||% ""
                    if (type %in% c("detrend", "demean", "baseline")) {
                      next
                    }
                    if (type == "decimate") {
                      by <- max(1L, as.integer(cfg$by %||% 1L))
                      if (!found_frequency_filter) {
                        pre_filter_decimate <- pre_filter_decimate * 
                          by
                        new_srate <- new_srate/by
                        n_timepoints <- ceiling(n_timepoints/by)
                      } else {
                        post_filter_decimate <- post_filter_decimate * 
                          by
                      }
                      next
                    }
                    if (type == "fir") {
                      type <- "fir_kaiser"
                    }
                    if (type == "iir") {
                      type <- "butter"
                    }
                    f <- tryCatch({
                      ravetools::design_filter(sample_rate = new_srate, 
                        filter_order = NA, data_size = n_timepoints, 
                        high_pass_freq = cfg$high_pass_freq %||% 
                          NA, high_pass_trans_freq = cfg$high_pass_trans_freq %||% 
                          NA, low_pass_freq = cfg$low_pass_freq %||% 
                          NA, low_pass_trans_freq = cfg$low_pass_trans_freq %||% 
                          NA, passband_ripple = cfg$passband_ripple %||% 
                          0.1, stopband_attenuation = cfg$stopband_attenuation %||% 
                          40, scale = TRUE, method = type)
                    }, error = function(e) {
                      stop("Could not design filter (type=", 
                        type, "): ", conditionMessage(e))
                      NULL
                    })
                    if (!is.null(f)) {
                      found_frequency_filter <- TRUE
                      freqz <- ravetools::freqz2(b = f$b, a = f$a, 
                        fs = new_srate, n = freqz_n, whole = FALSE)
                      filter[[length(filter) + 1L]] <- list(config = cfg, 
                        type = type, sample_rate = new_srate, 
                        b = f$b, a = f$a, frequency = freqz$w, 
                        response = freqz$h)
                      xlim <- range(c(xlim, c(f$parameters$stopband, 
                        f$parameters$passband) * new_srate/2), 
                        na.rm = TRUE)
                    }
                  }
                  if (length(filter)) {
                    combined_freq <- seq(0, new_srate/2, length.out = freqz_n)
                    combined_response <- rep(1 + (0+0i), length(combined_freq))
                    for (item in filter) {
                      combined_response <- combined_response * 
                        item$response
                    }
                    filter_freqz <- structure(class = c("ravetools-freqz2", 
                      "ravetools-printable"), list(w = combined_freq, 
                      h = combined_response, f = combined_freq, 
                      u = "Hz", srate = new_srate, pre_filter_decimate = pre_filter_decimate, 
                      post_filter_decimate = post_filter_decimate, 
                      original_srate = sample_rate, n = freqz_n, 
                      xlim = xlim))
                  } else {
                    filter_freqz <- NULL
                  }
                }
                filter_freqz
            }), target_depends = c("repository", "filter_configurations"
            )), deps = c("repository", "filter_configurations"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), prepare_pre_analysis_filters = targets::tar_target_raw(name = "filtered_array", 
        command = quote({
            .__target_expr__. <- quote({
                filtered_array <- prepare_filtered_data(array_type = "filtered_voltage", 
                  repository = repository, filter_configurations = filter_configurations)
                filtered_array <- ravepipeline::RAVEFileArray$new(filtered_array)
                electrodes_to_filter <- repository$electrode_list
                invisible(ravepipeline::lapply_jobs(electrodes_to_filter, 
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
                  }))
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
                  filtered_array <- prepare_filtered_data(array_type = "filtered_voltage", 
                    repository = repository, filter_configurations = filter_configurations)
                  filtered_array <- ravepipeline::RAVEFileArray$new(filtered_array)
                  electrodes_to_filter <- repository$electrode_list
                  invisible(ravepipeline::lapply_jobs(electrodes_to_filter, 
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
                    }))
                  filtered_array$`@impl`
                }
                filtered_array
            }), target_depends = c("repository", "filter_configurations"
            )), deps = c("repository", "filter_configurations"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), get_electrode_coordinate_table = targets::tar_target_raw(name = "analysis_electrode_coordinates", 
        command = quote({
            .__target_expr__. <- quote({
                analysis_electrode_coordinates <- subject$get_electrode_table(electrodes = analysis_electrodes_clean, 
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
                  analysis_electrode_coordinates <- subject$get_electrode_table(electrodes = analysis_electrodes_clean, 
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
            }), target_depends = c("subject", "analysis_electrodes_clean", 
            "repository")), deps = c("subject", "analysis_electrodes_clean", 
        "repository"), cue = targets::tar_cue("thorough"), pattern = NULL, 
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
        pattern = NULL, iteration = "list"), prepare_plot_data_placeholder = targets::tar_target_raw(name = "data_placeholder", 
        command = quote({
            .__target_expr__. <- quote({
                data_placeholder <- condition_groups_clean
                if (inherits(filtered_array, "RAVEFileArray")) {
                  filtered_array_impl <- filtered_array$`@impl`
                } else {
                  filtered_array_impl <- filtered_array
                }
                dnames <- dimnames(filtered_array_impl)
                data_placeholder$sample_rate <- filtered_array_impl$get_header("sample_rate")
                data_placeholder$coord_table <- analysis_electrode_coordinates
                data_placeholder$time_points <- dnames$Time
                attr(data_placeholder, "signature") <- filtered_array_impl$get_header("signature_filters")
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
                  if (inherits(filtered_array, "RAVEFileArray")) {
                    filtered_array_impl <- filtered_array$`@impl`
                  } else {
                    filtered_array_impl <- filtered_array
                  }
                  dnames <- dimnames(filtered_array_impl)
                  data_placeholder$sample_rate <- filtered_array_impl$get_header("sample_rate")
                  data_placeholder$coord_table <- analysis_electrode_coordinates
                  data_placeholder$time_points <- dnames$Time
                  attr(data_placeholder, "signature") <- filtered_array_impl$get_header("signature_filters")
                }
                data_placeholder
            }), target_depends = c("condition_groups_clean", 
            "filtered_array", "analysis_electrode_coordinates"
            )), deps = c("condition_groups_clean", "filtered_array", 
        "analysis_electrode_coordinates"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), calculating_erp_durations = targets::tar_target_raw(name = "crp_results", 
        command = quote({
            .__target_expr__. <- quote({
                if (inherits(filtered_array, "RAVEFileArray")) {
                  filtered_array_impl <- filtered_array$`@impl`
                } else {
                  filtered_array_impl <- filtered_array
                }
                sample_rate <- filtered_array_impl$get_header("sample_rate")
                dnames <- dimnames(filtered_array_impl)
                time_range <- range(dnames$Time)
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
                if (crp_enabled) {
                  cond_groups <- condition_groups_clean$groups
                  crp_results <- ravepipeline::lapply_jobs(dnames$Electrode, 
                    function(electrode) {
                      filtered_array_impl <- filtered_array$`@impl`
                      group_data <- lapply(cond_groups, function(group) {
                        sub_array <- filtered_array_impl[, match(group$trials_included, 
                          dnames$Trial), dnames$Electrode == 
                          electrode, dimnames = NULL, drop = FALSE]
                        dim(sub_array) <- dim(sub_array)[c(1, 
                          2)]
                        crp_result <- ravetools::crp(sub_array, 
                          time = dnames$Time, t_start = crp_time_begin, 
                          t_end = crp_time_end, time_step = crp_time_step, 
                          threshold_quantile = 0.98, artifact_interval = "tR", 
                          remove_artifacts = TRUE)
                        crp_result$projections$S_all <- NULL
                        crp_result$.data$V <- NULL
                        crp_result$parameters$ep <- NULL
                        crp_result$parameters$V_tR <- NULL
                        crp_result$group_index <- group$index
                        crp_result$electrode <- electrode
                        return(crp_result)
                      })
                      return(group_data)
                    }, .globals = list(cond_groups = cond_groups, 
                      filtered_array = ravepipeline::RAVEFileArray$new(filtered_array_impl), 
                      dnames = dnames, crp_time_begin = crp_time_begin, 
                      crp_time_end = crp_time_end, crp_time_step = crp_time_step), 
                    callback = function(e) {
                      sprintf("Calculating ERP duration | %s", 
                        e)
                    })
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
                  if (inherits(filtered_array, "RAVEFileArray")) {
                    filtered_array_impl <- filtered_array$`@impl`
                  } else {
                    filtered_array_impl <- filtered_array
                  }
                  sample_rate <- filtered_array_impl$get_header("sample_rate")
                  dnames <- dimnames(filtered_array_impl)
                  time_range <- range(dnames$Time)
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
                  if (crp_enabled) {
                    cond_groups <- condition_groups_clean$groups
                    crp_results <- ravepipeline::lapply_jobs(dnames$Electrode, 
                      function(electrode) {
                        filtered_array_impl <- filtered_array$`@impl`
                        group_data <- lapply(cond_groups, function(group) {
                          sub_array <- filtered_array_impl[, 
                            match(group$trials_included, dnames$Trial), 
                            dnames$Electrode == electrode, dimnames = NULL, 
                            drop = FALSE]
                          dim(sub_array) <- dim(sub_array)[c(1, 
                            2)]
                          crp_result <- ravetools::crp(sub_array, 
                            time = dnames$Time, t_start = crp_time_begin, 
                            t_end = crp_time_end, time_step = crp_time_step, 
                            threshold_quantile = 0.98, artifact_interval = "tR", 
                            remove_artifacts = TRUE)
                          crp_result$projections$S_all <- NULL
                          crp_result$.data$V <- NULL
                          crp_result$parameters$ep <- NULL
                          crp_result$parameters$V_tR <- NULL
                          crp_result$group_index <- group$index
                          crp_result$electrode <- electrode
                          return(crp_result)
                        })
                        return(group_data)
                      }, .globals = list(cond_groups = cond_groups, 
                        filtered_array = ravepipeline::RAVEFileArray$new(filtered_array_impl), 
                        dnames = dnames, crp_time_begin = crp_time_begin, 
                        crp_time_end = crp_time_end, crp_time_step = crp_time_step), 
                      callback = function(e) {
                        sprintf("Calculating ERP duration | %s", 
                          e)
                      })
                  } else {
                    crp_results <- NULL
                  }
                }
                crp_results
            }), target_depends = c("filtered_array", "condition_groups_clean"
            )), deps = c("filtered_array", "condition_groups_clean"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), prepare_erp_duration_for_viewer = targets::tar_target_raw(name = "erp_results_for_viewer", 
        command = quote({
            .__target_expr__. <- quote({
                erp_results_for_viewer <- crp_results
                if (length(crp_results)) {
                  group_indexes <- condition_groups_clean$group_indexes
                  group_labels <- condition_groups_clean$group_labels
                  crp_tbl <- lapply(crp_results, function(group_results) {
                    group_tbl <- lapply(group_results, function(group_result) {
                      label <- group_labels[group_indexes == 
                        group_result$group_index]
                      data.frame(Electrode = group_result$electrode, 
                        vname = sprintf("%s (%s)", c("t_val", 
                          "resp_strength", "var_explain", "tau"), 
                          label), value = c(group_result$projections$t_value_tR, 
                          mean(abs(group_result$parameters$al_p)), 
                          mean(group_result$parameters$expl_var), 
                          group_result$tau_R))
                    })
                    data.table::rbindlist(group_tbl)
                  })
                  crp_tbl <- data.table::rbindlist(crp_tbl)
                  erp_results_for_viewer <- data.table::dcast(crp_tbl, 
                    Electrode ~ vname, value.var = "value")
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
                    group_indexes <- condition_groups_clean$group_indexes
                    group_labels <- condition_groups_clean$group_labels
                    crp_tbl <- lapply(crp_results, function(group_results) {
                      group_tbl <- lapply(group_results, function(group_result) {
                        label <- group_labels[group_indexes == 
                          group_result$group_index]
                        data.frame(Electrode = group_result$electrode, 
                          vname = sprintf("%s (%s)", c("t_val", 
                            "resp_strength", "var_explain", "tau"), 
                            label), value = c(group_result$projections$t_value_tR, 
                            mean(abs(group_result$parameters$al_p)), 
                            mean(group_result$parameters$expl_var), 
                            group_result$tau_R))
                      })
                      data.table::rbindlist(group_tbl)
                    })
                    crp_tbl <- data.table::rbindlist(crp_tbl)
                    erp_results_for_viewer <- data.table::dcast(crp_tbl, 
                      Electrode ~ vname, value.var = "value")
                  }
                }
                erp_results_for_viewer
            }), target_depends = c("crp_results", "condition_groups_clean"
            )), deps = c("crp_results", "condition_groups_clean"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), prepare_voltage_over_channel_and_condition_by_collapsing_trials = targets::tar_target_raw(name = "data_by_channel_condition", 
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
                      Electrode ~ Electrode %in% analysis_electrodes_clean, 
                      Trial ~ Trial %in% group$trials_included, 
                      drop = FALSE)
                    dimnames(sub_array) <- NULL
                    ravetools::collapse(x = sub_array, keep = c(1L, 
                      3L), average = TRUE)
                  })
                group_data <- simplify2array(group_data)
                data_by_channel_condition <- ravepipeline::pipeline_plot_data(x = data_placeholder, 
                  name = "data_by_channel_condition")
                data_by_channel_condition$data <- group_data
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
                        Electrode ~ Electrode %in% analysis_electrodes_clean, 
                        Trial ~ Trial %in% group$trials_included, 
                        drop = FALSE)
                      dimnames(sub_array) <- NULL
                      ravetools::collapse(x = sub_array, keep = c(1L, 
                        3L), average = TRUE)
                    })
                  group_data <- simplify2array(group_data)
                  data_by_channel_condition <- ravepipeline::pipeline_plot_data(x = data_placeholder, 
                    name = "data_by_channel_condition")
                  data_by_channel_condition$data <- group_data
                }
                data_by_channel_condition
            }), target_depends = c("filtered_array", "data_placeholder", 
            "analysis_electrodes_clean")), deps = c("filtered_array", 
        "data_placeholder", "analysis_electrodes_clean"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), prepare_voltage_collapsed_over_channels_per_condition = targets::tar_target_raw(name = "data_by_trial_channel_condition", 
        command = quote({
            .__target_expr__. <- quote({
                if (inherits(filtered_array, "RAVEFileArray")) {
                  filtered_array_impl <- filtered_array$`@impl`
                } else {
                  filtered_array_impl <- filtered_array
                }
                dnames <- dimnames(filtered_array_impl)
                trial_numbers <- dnames$Trial
                time_points <- data_placeholder$time_points
                sample_rate <- data_placeholder$sample_rate
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
                group_data <- lapply(data_placeholder$groups, 
                  function(group) {
                    sub_array <- subset(filtered_array_impl, 
                      Electrode ~ Electrode %in% analysis_electrodes_clean, 
                      Trial ~ match(group$trials_included, trial_numbers), 
                      drop = FALSE)
                    dimnames(sub_array) <- NULL
                    voltage_by_trial <- ravetools::collapse(x = sub_array, 
                      keep = c(1L, 2L), average = TRUE)
                    if (crp_enabled) {
                      crp_result <- ravetools::crp(voltage_by_trial, 
                        time = time_points, t_start = crp_time_begin, 
                        t_end = crp_time_end, time_step = crp_time_step, 
                        threshold_quantile = 0.98, artifact_interval = "tR", 
                        remove_artifacts = TRUE)
                    } else {
                      crp_result <- NULL
                    }
                    if (length(crp_result$bad_trials)) {
                      bad_trials_index <- crp_result$bad_trials
                      bad_trials <- as.integer(group$trials_included[bad_trials_index])
                      mean_erp <- rowMeans(voltage_by_trial[, 
                        -bad_trials_index, drop = FALSE])
                    } else {
                      bad_trials <- integer(0L)
                      mean_erp <- rowMeans(voltage_by_trial)
                    }
                    return(list(voltage = voltage_by_trial, mean = mean_erp, 
                      bad_trials = bad_trials, crp_result = crp_result))
                  })
                data_by_trial_channel_condition <- ravepipeline::pipeline_plot_data(x = data_placeholder, 
                  name = "data_by_trial_channel_condition")
                data_by_trial_channel_condition$crp_enabled <- crp_enabled
                data_by_trial_channel_condition$data <- group_data
            })
            tryCatch({
                eval(.__target_expr__.)
                return(data_by_trial_channel_condition)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "data_by_trial_channel_condition", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "data_by_trial_channel_condition", 
            target_expr = quote({
                {
                  if (inherits(filtered_array, "RAVEFileArray")) {
                    filtered_array_impl <- filtered_array$`@impl`
                  } else {
                    filtered_array_impl <- filtered_array
                  }
                  dnames <- dimnames(filtered_array_impl)
                  trial_numbers <- dnames$Trial
                  time_points <- data_placeholder$time_points
                  sample_rate <- data_placeholder$sample_rate
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
                  group_data <- lapply(data_placeholder$groups, 
                    function(group) {
                      sub_array <- subset(filtered_array_impl, 
                        Electrode ~ Electrode %in% analysis_electrodes_clean, 
                        Trial ~ match(group$trials_included, 
                          trial_numbers), drop = FALSE)
                      dimnames(sub_array) <- NULL
                      voltage_by_trial <- ravetools::collapse(x = sub_array, 
                        keep = c(1L, 2L), average = TRUE)
                      if (crp_enabled) {
                        crp_result <- ravetools::crp(voltage_by_trial, 
                          time = time_points, t_start = crp_time_begin, 
                          t_end = crp_time_end, time_step = crp_time_step, 
                          threshold_quantile = 0.98, artifact_interval = "tR", 
                          remove_artifacts = TRUE)
                      } else {
                        crp_result <- NULL
                      }
                      if (length(crp_result$bad_trials)) {
                        bad_trials_index <- crp_result$bad_trials
                        bad_trials <- as.integer(group$trials_included[bad_trials_index])
                        mean_erp <- rowMeans(voltage_by_trial[, 
                          -bad_trials_index, drop = FALSE])
                      } else {
                        bad_trials <- integer(0L)
                        mean_erp <- rowMeans(voltage_by_trial)
                      }
                      return(list(voltage = voltage_by_trial, 
                        mean = mean_erp, bad_trials = bad_trials, 
                        crp_result = crp_result))
                    })
                  data_by_trial_channel_condition <- ravepipeline::pipeline_plot_data(x = data_placeholder, 
                    name = "data_by_trial_channel_condition")
                  data_by_trial_channel_condition$crp_enabled <- crp_enabled
                  data_by_trial_channel_condition$data <- group_data
                }
                data_by_trial_channel_condition
            }), target_depends = c("filtered_array", "data_placeholder", 
            "analysis_electrodes_clean")), deps = c("filtered_array", 
        "data_placeholder", "analysis_electrodes_clean"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
