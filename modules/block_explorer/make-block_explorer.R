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
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path",
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings",
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")),
    input_analysis_block = targets::tar_target_raw("analysis_block",
        quote({
            settings[["analysis_block"]]
        }), deps = "settings"), input_pwelch_frequency_limit = targets::tar_target_raw("pwelch_frequency_limit",
        quote({
            settings[["pwelch_frequency_limit"]]
        }), deps = "settings"), input_analysis_electrodes = targets::tar_target_raw("analysis_electrodes",
        quote({
            settings[["analysis_electrodes"]]
        }), deps = "settings"), input_filter_notch = targets::tar_target_raw("filter_notch",
        quote({
            settings[["filter_notch"]]
        }), deps = "settings"), input_loaded_electrodes = targets::tar_target_raw("loaded_electrodes",
        quote({
            settings[["loaded_electrodes"]]
        }), deps = "settings"), input_analysis_time = targets::tar_target_raw("analysis_time",
        quote({
            settings[["analysis_time"]]
        }), deps = "settings"), input_highlight_electrodes = targets::tar_target_raw("highlight_electrodes",
        quote({
            settings[["highlight_electrodes"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_vertical_spacing = targets::tar_target_raw("vertical_spacing",
        quote({
            settings[["vertical_spacing"]]
        }), deps = "settings"), input_filter_bandpass = targets::tar_target_raw("filter_bandpass",
        quote({
            settings[["filter_bandpass"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_block = targets::tar_target_raw("block",
        quote({
            settings[["block"]]
        }), deps = "settings"), input_hide_electrodes = targets::tar_target_raw("hide_electrodes",
        quote({
            settings[["hide_electrodes"]]
        }), deps = "settings"), input_pwelch_params = targets::tar_target_raw("pwelch_params",
        quote({
            settings[["pwelch_params"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject",
        command = quote({
            tryCatch({
                {
                  subject <- RAVESubject$new(project_name = project_name,
                    subject_code = subject_code)
                }
                return(subject)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("subject",
                  e, quote({
                    subject <- RAVESubject$new(project_name = project_name,
                      subject_code = subject_code)
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic("rave-subject"),
        deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), validate_inputs = targets::tar_target_raw(name = "repository",
        command = quote({
            tryCatch({
                {
                  load_electrodes <- unique(as.integer(dipsaus::parse_svec(loaded_electrodes)))
                  electrodes <- as.integer(subject$electrodes)
                  electrodes <- electrodes[subject$preprocess_settings$data_imported]
                  load_electrodes <- load_electrodes[load_electrodes %in%
                    electrodes]
                  root_path <- file.path(subject$preprocess_path,
                    "voltage")
                  if (length(load_electrodes)) {
                    data_files <- file.path(root_path, sprintf("electrode_%d.h5",
                      load_electrodes))
                    load_electrodes <- load_electrodes[file.exists(data_files)]
                  }
                  if (!length(load_electrodes)) {
                    stop("None electrode will be loaded. Please make sure at least one electrode is imported.")
                  }
                  sample_rates <- subject$raw_sample_rates[subject$electrodes %in%
                    load_electrodes]
                  sample_rate <- unique(sample_rates)
                  if (length(sample_rate) == 0) {
                    stop("Cannot find proper sample rate for the selected electrodes")
                  }
                  if (anyNA(sample_rates)) {
                    stop("Some electrodes have missing sampling frequency. Please make sure all selected electrode channels have been imported.")
                  }
                  if (length(sample_rate) > 1) {
                    stop("Multiple sample rates are found: [",
                      paste(sprintf("%.1f", sample_rate), collapse = ", "),
                      "]. Please choose electrode channels with the sample rates.")
                  }
                  blocks <- block[block %in% subject$blocks]
                  if (!length(blocks)) {
                    stop("All the blocks are invalid. Please choose one or more blocks from the following choices: ",
                      paste(subject$blocks, collapse = ", "))
                  }
                  sample_electrode <- load_electrodes[[1]]
                  sample_instance <- raveio::new_electrode(subject,
                    sample_electrode, quiet = TRUE)
                  sample_data <- sample_instance$load_blocks(blocks,
                    type = "raw-voltage", simplify = FALSE)
                  electrodes <- as.integer(subject$electrodes)
                  cache_root <- file.path(subject$cache_path,
                    "rave2", "raw-voltage")
                  progress <- dipsaus::progress2("Loading raw-voltage",
                    max = length(blocks) + 1, shiny_auto_close = TRUE,
                    quiet = !dipsaus::shiny_is_running())
                  loaded_data <- sapply(blocks, function(block) {
                    progress$inc(sprintf("Loading block %s",
                      block))
                    data_length <- length(sample_data[[block]])
                    dm <- c(data_length, length(electrodes))
                    cache_path <- file.path(cache_root, block)
                    arr <- filearray::filearray_load_or_create(filebase = cache_path,
                      dimension = dm, type = "double", mode = "readwrite",
                      partition_size = 1L, symlink_ok = FALSE,
                      verbose = FALSE, electrodes = as.integer(electrodes),
                      block = block)
                    loaded_electrodes <- arr$get_header(key = "loaded_electrodes",
                      default = NULL)
                    toload <- load_electrodes[!load_electrodes %in%
                      loaded_electrodes]
                    if (length(toload)) {
                      dipsaus::lapply_async2(toload, function(e) {
                        inst <- raveio::new_electrode(subject = subject,
                          number = e)
                        dat <- inst$load_blocks(blocks = block,
                          type = "raw-voltage", simplify = TRUE)
                        arr[, electrodes == e] <- dat
                        invisible()
                      }, plan = FALSE, callback = function(e) {
                        sprintf("Loading electrode %d", e)
                      })
                      arr$set_header("loaded_electrodes", sort(c(loaded_electrodes,
                        toload)))
                    }
                    arr$.mode <- "readonly"
                    arr
                  }, simplify = FALSE, USE.NAMES = TRUE)
                  progress$inc("Done")
                  reference_name <- "noref" %OF% subject$reference_names
                  repository <- raveio::prepare_subject_bare0(subject = subject,
                    electrodes = load_electrodes, reference_name = reference_name)
                  repository$data <- loaded_data
                  repository$blocks <- blocks
                  repository$sample_rate <- sample_rate
                  digest_key <- list(subject_id = subject$subject_id,
                    electrodes = load_electrodes, electrode_signal_types = repository$electrode_signal_types,
                    blocks = blocks, sample_rate = sample_rate)
                  repository$signature <- structure(dipsaus::digest(digest_key),
                    contents = names(digest_key))
                  repository$`@remove`("electrode_instances")
                  class(repository) <- c("rave_prepare_raw_voltage",
                    class(repository))
                }
                return(repository)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("repository",
                  e, quote({
                    load_electrodes <- unique(as.integer(dipsaus::parse_svec(loaded_electrodes)))
                    electrodes <- as.integer(subject$electrodes)
                    electrodes <- electrodes[subject$preprocess_settings$data_imported]
                    load_electrodes <- load_electrodes[load_electrodes %in%
                      electrodes]
                    root_path <- file.path(subject$preprocess_path,
                      "voltage")
                    if (length(load_electrodes)) {
                      data_files <- file.path(root_path, sprintf("electrode_%d.h5",
                        load_electrodes))
                      load_electrodes <- load_electrodes[file.exists(data_files)]
                    }
                    if (!length(load_electrodes)) {
                      stop("None electrode will be loaded. Please make sure at least one electrode is imported.")
                    }
                    sample_rates <- subject$raw_sample_rates[subject$electrodes %in%
                      load_electrodes]
                    sample_rate <- unique(sample_rates)
                    if (length(sample_rate) == 0) {
                      stop("Cannot find proper sample rate for the selected electrodes")
                    }
                    if (anyNA(sample_rates)) {
                      stop("Some electrodes have missing sampling frequency. Please make sure all selected electrode channels have been imported.")
                    }
                    if (length(sample_rate) > 1) {
                      stop("Multiple sample rates are found: [",
                        paste(sprintf("%.1f", sample_rate), collapse = ", "),
                        "]. Please choose electrode channels with the sample rates.")
                    }
                    blocks <- block[block %in% subject$blocks]
                    if (!length(blocks)) {
                      stop("All the blocks are invalid. Please choose one or more blocks from the following choices: ",
                        paste(subject$blocks, collapse = ", "))
                    }
                    sample_electrode <- load_electrodes[[1]]
                    sample_instance <- raveio::new_electrode(subject,
                      sample_electrode, quiet = TRUE)
                    sample_data <- sample_instance$load_blocks(blocks,
                      type = "raw-voltage", simplify = FALSE)
                    electrodes <- as.integer(subject$electrodes)
                    cache_root <- file.path(subject$cache_path,
                      "rave2", "raw-voltage")
                    progress <- dipsaus::progress2("Loading raw-voltage",
                      max = length(blocks) + 1, shiny_auto_close = TRUE,
                      quiet = !dipsaus::shiny_is_running())
                    loaded_data <- sapply(blocks, function(block) {
                      progress$inc(sprintf("Loading block %s",
                        block))
                      data_length <- length(sample_data[[block]])
                      dm <- c(data_length, length(electrodes))
                      cache_path <- file.path(cache_root, block)
                      arr <- filearray::filearray_load_or_create(filebase = cache_path,
                        dimension = dm, type = "double", mode = "readwrite",
                        partition_size = 1L, symlink_ok = FALSE,
                        verbose = FALSE, electrodes = as.integer(electrodes),
                        block = block)
                      loaded_electrodes <- arr$get_header(key = "loaded_electrodes",
                        default = NULL)
                      toload <- load_electrodes[!load_electrodes %in%
                        loaded_electrodes]
                      if (length(toload)) {
                        dipsaus::lapply_async2(toload, function(e) {
                          inst <- raveio::new_electrode(subject = subject,
                            number = e)
                          dat <- inst$load_blocks(blocks = block,
                            type = "raw-voltage", simplify = TRUE)
                          arr[, electrodes == e] <- dat
                          invisible()
                        }, plan = FALSE, callback = function(e) {
                          sprintf("Loading electrode %d", e)
                        })
                        arr$set_header("loaded_electrodes", sort(c(loaded_electrodes,
                          toload)))
                      }
                      arr$.mode <- "readonly"
                      arr
                    }, simplify = FALSE, USE.NAMES = TRUE)
                    progress$inc("Done")
                    reference_name <- "noref" %OF% subject$reference_names
                    repository <- raveio::prepare_subject_bare0(subject = subject,
                      electrodes = load_electrodes, reference_name = reference_name)
                    repository$data <- loaded_data
                    repository$blocks <- blocks
                    repository$sample_rate <- sample_rate
                    digest_key <- list(subject_id = subject$subject_id,
                      electrodes = load_electrodes, electrode_signal_types = repository$electrode_signal_types,
                      blocks = blocks, sample_rate = sample_rate)
                    repository$signature <- structure(dipsaus::digest(digest_key),
                      contents = names(digest_key))
                    repository$`@remove`("electrode_instances")
                    class(repository) <- c("rave_prepare_raw_voltage",
                      class(repository))
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("loaded_electrodes", "subject", "block"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), check_analysis_inputs = targets::tar_target_raw(name = "cleaned_inputs",
        command = quote({
            tryCatch({
                {
                  sample_rate <- repository$sample_rate
                  if (length(sample_rate) != 1) {
                    stop("Cannot find sample rate from the repository. This is code error. Please contact the RAVE development team and report this issue")
                  }
                  filter_bandpass <- as.list(filter_bandpass)
                  if (isTRUE(filter_bandpass$enabled)) {
                    filter_range <- as.numeric(filter_bandpass$range)
                    filter_range <- filter_range[!is.na(filter_range) &
                      filter_range >= 0]
                    filter_range <- sort(unique(filter_range))
                    if (length(filter_range)) {
                      if (filter_range[[1]] == 0) {
                        filter_range[[1]] <- 0.001
                      }
                      filter_range <- sort(unique(filter_range))
                    }
                    if (length(filter_range) != 2) {
                      stop("Band-passing filter must have a postive frequency range with lower-bound smaller than the upper-bound")
                    }
                    if (any(filter_range > sample_rate/2)) {
                      stop("Band-passing filter range must not exceed half sample-rate (Nyquist frequency) to avoid aliasing.")
                    }
                    filter_bandpass <- list(enabled = TRUE, range = filter_range)
                  } else {
                    filter_bandpass <- list(enabled = FALSE)
                  }
                  filter_notch <- as.list(filter_notch)
                  if (isTRUE(filter_notch$enabled)) {
                    lower_bounds <- filter_notch$lower_bounds
                    lower_bounds <- lower_bounds[!is.na(lower_bounds) &
                      lower_bounds > 0]
                    upper_bounds <- filter_notch$upper_bounds
                    upper_bounds <- upper_bounds[!is.na(upper_bounds) &
                      upper_bounds > 0]
                    if (!length(lower_bounds) || length(upper_bounds) !=
                      length(lower_bounds)) {
                      stop(sprintf("Notch filter lower-bounds [%s] have unequal/zero lengths compared to the upper-bounds [%s]. Please make sure the frequencies have equal positive lengths.",
                        paste(sprintf("%.1fHz", lower_bounds),
                          collapse = ","), paste(sprintf("%.1fHz",
                          upper_bounds), collapse = ",")))
                    }
                    if (any(lower_bounds >= upper_bounds)) {
                      stop(sprintf("Notch filter lower-bounds [%s] must be smaller than upper-bounds [%s], respectively.",
                        paste(sprintf("%.1fHz", lower_bounds),
                          collapse = ","), paste(sprintf("%.1fHz",
                          upper_bounds), collapse = ",")))
                    }
                    if (any(upper_bounds > sample_rate/2)) {
                      stop("Notch filter upper-bounds [%s] must not exceed the half sample-rate (Nyquist frequency) to avoid aliasing.")
                    }
                    filter_notch <- list(enabled = TRUE, lower_bounds = lower_bounds,
                      upper_bounds = upper_bounds)
                  } else {
                    filter_notch <- list(enabled = FALSE)
                  }
                  cleaned_inputs <- list(filter_bandpass = filter_bandpass,
                    filter_notch = filter_notch, sample_rate = sample_rate)
                }
                return(cleaned_inputs)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("cleaned_inputs",
                  e, quote({
                    sample_rate <- repository$sample_rate
                    if (length(sample_rate) != 1) {
                      stop("Cannot find sample rate from the repository. This is code error. Please contact the RAVE development team and report this issue")
                    }
                    filter_bandpass <- as.list(filter_bandpass)
                    if (isTRUE(filter_bandpass$enabled)) {
                      filter_range <- as.numeric(filter_bandpass$range)
                      filter_range <- filter_range[!is.na(filter_range) &
                        filter_range >= 0]
                      filter_range <- sort(unique(filter_range))
                      if (length(filter_range)) {
                        if (filter_range[[1]] == 0) {
                          filter_range[[1]] <- 0.001
                        }
                        filter_range <- sort(unique(filter_range))
                      }
                      if (length(filter_range) != 2) {
                        stop("Band-passing filter must have a postive frequency range with lower-bound smaller than the upper-bound")
                      }
                      if (any(filter_range > sample_rate/2)) {
                        stop("Band-passing filter range must not exceed half sample-rate (Nyquist frequency) to avoid aliasing.")
                      }
                      filter_bandpass <- list(enabled = TRUE,
                        range = filter_range)
                    } else {
                      filter_bandpass <- list(enabled = FALSE)
                    }
                    filter_notch <- as.list(filter_notch)
                    if (isTRUE(filter_notch$enabled)) {
                      lower_bounds <- filter_notch$lower_bounds
                      lower_bounds <- lower_bounds[!is.na(lower_bounds) &
                        lower_bounds > 0]
                      upper_bounds <- filter_notch$upper_bounds
                      upper_bounds <- upper_bounds[!is.na(upper_bounds) &
                        upper_bounds > 0]
                      if (!length(lower_bounds) || length(upper_bounds) !=
                        length(lower_bounds)) {
                        stop(sprintf("Notch filter lower-bounds [%s] have unequal/zero lengths compared to the upper-bounds [%s]. Please make sure the frequencies have equal positive lengths.",
                          paste(sprintf("%.1fHz", lower_bounds),
                            collapse = ","), paste(sprintf("%.1fHz",
                            upper_bounds), collapse = ",")))
                      }
                      if (any(lower_bounds >= upper_bounds)) {
                        stop(sprintf("Notch filter lower-bounds [%s] must be smaller than upper-bounds [%s], respectively.",
                          paste(sprintf("%.1fHz", lower_bounds),
                            collapse = ","), paste(sprintf("%.1fHz",
                            upper_bounds), collapse = ",")))
                      }
                      if (any(upper_bounds > sample_rate/2)) {
                        stop("Notch filter upper-bounds [%s] must not exceed the half sample-rate (Nyquist frequency) to avoid aliasing.")
                      }
                      filter_notch <- list(enabled = TRUE, lower_bounds = lower_bounds,
                        upper_bounds = upper_bounds)
                    } else {
                      filter_notch <- list(enabled = FALSE)
                    }
                    cleaned_inputs <- list(filter_bandpass = filter_bandpass,
                      filter_notch = filter_notch, sample_rate = sample_rate)
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("repository", "filter_bandpass", "filter_notch"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), check_pwelch_params = targets::tar_target_raw(name = "pwelch_params2",
        command = quote({
            tryCatch({
                {
                  pwelch_params <- as.list(pwelch_params)
                  window_size <- as.integer(pwelch_params$window_size)
                  if (!length(window_size) || !isTRUE(window_size >
                    0)) {
                    stop("Welch-Periodogram window size must be an integer greater than 1")
                  }
                  noverlap <- as.integer(pwelch_params$noverlap)
                  if (!length(noverlap) || is.na(noverlap) ||
                    noverlap < 0) {
                    stop("Welch-Periodogram window overlap size must be a positive integer")
                  }
                  if (noverlap/window_size > 0.95) {
                    ravepipeline::logger("Welch-Periodogram window overlap size exceeds 0.95 x window size. The `pwelch` calculation will be slow.")
                  }
                  pwelch_params2 <- list(window_size = window_size,
                    noverlap = noverlap)
                }
                return(pwelch_params2)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("pwelch_params2",
                  e, quote({
                    pwelch_params <- as.list(pwelch_params)
                    window_size <- as.integer(pwelch_params$window_size)
                    if (!length(window_size) || !isTRUE(window_size >
                      0)) {
                      stop("Welch-Periodogram window size must be an integer greater than 1")
                    }
                    noverlap <- as.integer(pwelch_params$noverlap)
                    if (!length(noverlap) || is.na(noverlap) ||
                      noverlap < 0) {
                      stop("Welch-Periodogram window overlap size must be a positive integer")
                    }
                    if (noverlap/window_size > 0.95) {
                      ravepipeline::logger("Welch-Periodogram window overlap size exceeds 0.95 x window size. The `pwelch` calculation will be slow.")
                    }
                    pwelch_params2 <- list(window_size = window_size,
                      noverlap = noverlap)
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = "pwelch_params", cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), get_analysis_electrodes = targets::tar_target_raw(name = "analysis_electrodes2",
        command = quote({
            tryCatch({
                {
                  analysis_electrodes <- dipsaus::parse_svec(analysis_electrodes)
                  if (!length(analysis_electrodes)) {
                    analysis_electrodes <- repository$electrode_list
                  }
                  analysis_electrodes2 <- analysis_electrodes[analysis_electrodes %in%
                    repository$electrode_list]
                  analysis_electrodes2 <- unique(sort(analysis_electrodes2))
                }
                return(analysis_electrodes2)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("analysis_electrodes2",
                  e, quote({
                    analysis_electrodes <- dipsaus::parse_svec(analysis_electrodes)
                    if (!length(analysis_electrodes)) {
                      analysis_electrodes <- repository$electrode_list
                    }
                    analysis_electrodes2 <- analysis_electrodes[analysis_electrodes %in%
                      repository$electrode_list]
                    analysis_electrodes2 <- unique(sort(analysis_electrodes2))
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("analysis_electrodes", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), filter_signals = targets::tar_target_raw(name = "filtered_data",
        command = quote({
            tryCatch({
                {
                  force(analysis_electrodes2)
                  tmpdir <- ravedash::temp_dir(persist = "app-session")
                  input_data <- repository$data[[analysis_block]]
                  filtered_data <- filearray::filearray_load_or_create(filebase = file.path(tmpdir,
                    "block_explorer", repository$signature, "filtered",
                    analysis_block), dimension = dim(input_data),
                    type = "double", symlink_ok = FALSE, mode = "readwrite",
                    initialize = FALSE, partition_size = 1L,
                    verbose = FALSE, repository_signature = repository$signature,
                    block = analysis_block, filters = cleaned_inputs[c("filter_bandpass",
                      "filter_notch")], on_missing = function(arr) {
                      dimnames(arr) <- list(NULL, Electrode = repository$subject$electrodes)
                    })
                  filtered_electrodes <- filtered_data$get_header("filtered_electrodes",
                    NULL)
                  electrodes_to_filter <- analysis_electrodes2[!analysis_electrodes2 %in%
                    filtered_electrodes]
                  electrodes <- subject$electrodes
                  sample_rate <- repository$sample_rate
                  if (length(electrodes_to_filter)) {
                    dipsaus::lapply_async2(electrodes_to_filter,
                      function(e) {
                        sel <- which(electrodes == e)
                        if (!length(sel)) {
                          stop("cannot find data of the electrode ",
                            e)
                        }
                        sel <- sel[[1]]
                        signal <- input_data[, sel]
                        filter_bandpass <- cleaned_inputs$filter_bandpass
                        if (isTRUE(filter_bandpass$enabled)) {
                          signal <- ravetools::band_pass1(x = signal,
                            sample_rate = sample_rate, lb = filter_bandpass$range[[1]],
                            ub = filter_bandpass$range[[2]])
                        }
                        if (isTRUE(filter_notch$enabled)) {
                          signal <- ravetools::notch_filter(signal,
                            sample_rate = sample_rate, lb = filter_notch$lower_bounds,
                            ub = filter_notch$upper_bounds)
                        }
                        filtered_data[, sel] <- signal
                        return()
                      }, plan = FALSE, callback = function(e) {
                        sprintf("Filter signals | Electrode channel %.0f",
                          e)
                      })
                    filtered_electrodes <- sort(c(filtered_electrodes,
                      electrodes_to_filter))
                    filtered_data$set_header(key = "filtered_electrodes",
                      value = filtered_electrodes)
                  }
                }
                return(filtered_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("filtered_data",
                  e, quote({
                    force(analysis_electrodes2)
                    tmpdir <- ravedash::temp_dir(persist = "app-session")
                    input_data <- repository$data[[analysis_block]]
                    filtered_data <- filearray::filearray_load_or_create(filebase = file.path(tmpdir,
                      "block_explorer", repository$signature,
                      "filtered", analysis_block), dimension = dim(input_data),
                      type = "double", symlink_ok = FALSE, mode = "readwrite",
                      initialize = FALSE, partition_size = 1L,
                      verbose = FALSE, repository_signature = repository$signature,
                      block = analysis_block, filters = cleaned_inputs[c("filter_bandpass",
                        "filter_notch")], on_missing = function(arr) {
                        dimnames(arr) <- list(NULL, Electrode = repository$subject$electrodes)
                      })
                    filtered_electrodes <- filtered_data$get_header("filtered_electrodes",
                      NULL)
                    electrodes_to_filter <- analysis_electrodes2[!analysis_electrodes2 %in%
                      filtered_electrodes]
                    electrodes <- subject$electrodes
                    sample_rate <- repository$sample_rate
                    if (length(electrodes_to_filter)) {
                      dipsaus::lapply_async2(electrodes_to_filter,
                        function(e) {
                          sel <- which(electrodes == e)
                          if (!length(sel)) {
                            stop("cannot find data of the electrode ",
                              e)
                          }
                          sel <- sel[[1]]
                          signal <- input_data[, sel]
                          filter_bandpass <- cleaned_inputs$filter_bandpass
                          if (isTRUE(filter_bandpass$enabled)) {
                            signal <- ravetools::band_pass1(x = signal,
                              sample_rate = sample_rate, lb = filter_bandpass$range[[1]],
                              ub = filter_bandpass$range[[2]])
                          }
                          if (isTRUE(filter_notch$enabled)) {
                            signal <- ravetools::notch_filter(signal,
                              sample_rate = sample_rate, lb = filter_notch$lower_bounds,
                              ub = filter_notch$upper_bounds)
                          }
                          filtered_data[, sel] <- signal
                          return()
                        }, plan = FALSE, callback = function(e) {
                          sprintf("Filter signals | Electrode channel %.0f",
                            e)
                        })
                      filtered_electrodes <- sort(c(filtered_electrodes,
                        electrodes_to_filter))
                      filtered_data$set_header(key = "filtered_electrodes",
                        value = filtered_electrodes)
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("analysis_electrodes2", "repository", "analysis_block",
        "cleaned_inputs", "subject", "filter_notch"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), calculate_pwelch = targets::tar_target_raw(name = "pwelch_data",
        command = quote({
            tryCatch({
                {
                  force(analysis_electrodes2)
                  input_data <- repository$data[[analysis_block]]
                  sample_rate <- repository$sample_rate
                  window_len <- pwelch_params2$window_size
                  noverlap <- pwelch_params2$noverlap
                  x_len <- nrow(input_data)
                  nfft <- max(min(256, x_len), window_len)
                  NN <- floor((nfft + 1)/2)
                  dm <- c(NN, ncol(input_data))
                  freq <- seq(1, sample_rate/2, length.out = NN)
                  tmpdir <- ravedash::temp_dir(persist = "app-session")
                  pwelch_data <- filearray::filearray_load_or_create(filebase = file.path(tmpdir,
                    "block_explorer", repository$signature, "pwelch",
                    analysis_block), dimension = dm, type = "double",
                    symlink_ok = FALSE, mode = "readwrite", initialize = FALSE,
                    partition_size = 1L, verbose = FALSE, repository_signature = repository$signature,
                    block = analysis_block, pwelch_params = pwelch_params2,
                    on_missing = function(arr) {
                      dimnames(arr) <- list(Frequency = freq,
                        Electrode = repository$subject$electrodes)
                    })
                  processed_electrodes <- pwelch_data$get_header("processed_electrodes",
                    NULL)
                  electrodes_to_pwelch <- analysis_electrodes2[!analysis_electrodes2 %in%
                    processed_electrodes]
                  electrodes <- subject$electrodes
                  if (length(electrodes_to_pwelch)) {
                    dipsaus::lapply_async2(electrodes_to_pwelch,
                      function(e) {
                        sel <- which(electrodes == e)
                        if (!length(sel)) {
                          stop("cannot find data of the electrode ",
                            e)
                        }
                        sel <- sel[[1]]
                        signal <- input_data[, sel]
                        pwelch_result <- ravetools::pwelch(x = signal,
                          fs = sample_rate, window = pwelch_params2$window_size,
                          noverlap = pwelch_params2$noverlap)
                        pwelch_data[, sel] <- pwelch_result$spec
                        return()
                      }, plan = FALSE, callback = function(e) {
                        sprintf("Welch-Periodogram | Electrode channel %.0f",
                          e)
                      })
                    processed_electrodes <- sort(c(processed_electrodes,
                      electrodes_to_pwelch))
                    pwelch_data$set_header(key = "processed_electrodes",
                      value = processed_electrodes)
                  }
                }
                return(pwelch_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("pwelch_data",
                  e, quote({
                    force(analysis_electrodes2)
                    input_data <- repository$data[[analysis_block]]
                    sample_rate <- repository$sample_rate
                    window_len <- pwelch_params2$window_size
                    noverlap <- pwelch_params2$noverlap
                    x_len <- nrow(input_data)
                    nfft <- max(min(256, x_len), window_len)
                    NN <- floor((nfft + 1)/2)
                    dm <- c(NN, ncol(input_data))
                    freq <- seq(1, sample_rate/2, length.out = NN)
                    tmpdir <- ravedash::temp_dir(persist = "app-session")
                    pwelch_data <- filearray::filearray_load_or_create(filebase = file.path(tmpdir,
                      "block_explorer", repository$signature,
                      "pwelch", analysis_block), dimension = dm,
                      type = "double", symlink_ok = FALSE, mode = "readwrite",
                      initialize = FALSE, partition_size = 1L,
                      verbose = FALSE, repository_signature = repository$signature,
                      block = analysis_block, pwelch_params = pwelch_params2,
                      on_missing = function(arr) {
                        dimnames(arr) <- list(Frequency = freq,
                          Electrode = repository$subject$electrodes)
                      })
                    processed_electrodes <- pwelch_data$get_header("processed_electrodes",
                      NULL)
                    electrodes_to_pwelch <- analysis_electrodes2[!analysis_electrodes2 %in%
                      processed_electrodes]
                    electrodes <- subject$electrodes
                    if (length(electrodes_to_pwelch)) {
                      dipsaus::lapply_async2(electrodes_to_pwelch,
                        function(e) {
                          sel <- which(electrodes == e)
                          if (!length(sel)) {
                            stop("cannot find data of the electrode ",
                              e)
                          }
                          sel <- sel[[1]]
                          signal <- input_data[, sel]
                          pwelch_result <- ravetools::pwelch(x = signal,
                            fs = sample_rate, window = pwelch_params2$window_size,
                            noverlap = pwelch_params2$noverlap)
                          pwelch_data[, sel] <- pwelch_result$spec
                          return()
                        }, plan = FALSE, callback = function(e) {
                          sprintf("Welch-Periodogram | Electrode channel %.0f",
                            e)
                        })
                      processed_electrodes <- sort(c(processed_electrodes,
                        electrodes_to_pwelch))
                      pwelch_data$set_header(key = "processed_electrodes",
                        value = processed_electrodes)
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("analysis_electrodes2", "repository", "analysis_block",
        "pwelch_params2", "subject"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), returns_hidden_electrodes = targets::tar_target_raw(name = "hide_electrodes2",
        command = quote({
            tryCatch({
                {
                  hide_electrodes2 <- dipsaus::parse_svec(hide_electrodes,
                    sort = TRUE)
                  if (length(hide_electrodes2)) {
                    hide_electrodes2 <- hide_electrodes2[hide_electrodes2 ==
                      round(hide_electrodes2)]
                    hide_electrodes2 <- hide_electrodes2[hide_electrodes2 %in%
                      repository$electrode_list]
                  }
                }
                return(hide_electrodes2)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("hide_electrodes2",
                  e, quote({
                    hide_electrodes2 <- dipsaus::parse_svec(hide_electrodes,
                      sort = TRUE)
                    if (length(hide_electrodes2)) {
                      hide_electrodes2 <- hide_electrodes2[hide_electrodes2 ==
                        round(hide_electrodes2)]
                      hide_electrodes2 <- hide_electrodes2[hide_electrodes2 %in%
                        repository$electrode_list]
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("hide_electrodes", "repository"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), find_highlighted_electrodes = targets::tar_target_raw(name = "highlight_electrodes2",
        command = quote({
            tryCatch({
                {
                  highlight_electrodes2 <- dipsaus::parse_svec(highlight_electrodes,
                    sort = TRUE)
                  if (length(highlight_electrodes2)) {
                    highlight_electrodes2 <- highlight_electrodes2[highlight_electrodes2 ==
                      round(highlight_electrodes2)]
                    highlight_electrodes2 <- highlight_electrodes2[highlight_electrodes2 %in%
                      repository$electrode_list]
                    highlight_electrodes2 <- highlight_electrodes2[!highlight_electrodes2 %in%
                      hide_electrodes2]
                  }
                }
                return(highlight_electrodes2)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("highlight_electrodes2",
                  e, quote({
                    highlight_electrodes2 <- dipsaus::parse_svec(highlight_electrodes,
                      sort = TRUE)
                    if (length(highlight_electrodes2)) {
                      highlight_electrodes2 <- highlight_electrodes2[highlight_electrodes2 ==
                        round(highlight_electrodes2)]
                      highlight_electrodes2 <- highlight_electrodes2[highlight_electrodes2 %in%
                        repository$electrode_list]
                      highlight_electrodes2 <- highlight_electrodes2[!highlight_electrodes2 %in%
                        hide_electrodes2]
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("highlight_electrodes", "repository", "hide_electrodes2"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), calculate_electrode_selection_index = targets::tar_target_raw(name = "sel_electrodes",
        command = quote({
            tryCatch({
                {
                  electrodes <- repository$subject$electrodes
                  sel_electrodes <- (!electrodes %in% hide_electrodes2) &
                    (electrodes %in% repository$electrode_list) &
                    (electrodes %in% analysis_electrodes2)
                }
                return(sel_electrodes)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("sel_electrodes",
                  e, quote({
                    electrodes <- repository$subject$electrodes
                    sel_electrodes <- (!electrodes %in% hide_electrodes2) &
                      (electrodes %in% repository$electrode_list) &
                      (electrodes %in% analysis_electrodes2)
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("repository", "hide_electrodes2", "analysis_electrodes2"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), calculate_electrode_highlight_index = targets::tar_target_raw(name = "sel_highlights",
        command = quote({
            tryCatch({
                {
                  electrodes <- repository$subject$electrodes
                  sel_highlights <- (electrodes %in% highlight_electrodes2) &
                    (electrodes %in% repository$electrode_list) &
                    (electrodes %in% analysis_electrodes2)
                }
                return(sel_highlights)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("sel_highlights",
                  e, quote({
                    electrodes <- repository$subject$electrodes
                    sel_highlights <- (electrodes %in% highlight_electrodes2) &
                      (electrodes %in% repository$electrode_list) &
                      (electrodes %in% analysis_electrodes2)
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("repository", "highlight_electrodes2", "analysis_electrodes2"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), calculate_analysis_time_points = targets::tar_target_raw(name = "analysis_time2",
        command = quote({
            tryCatch({
                {
                  if (length(analysis_time) != 2 || !is.numeric(analysis_time) ||
                    any(is.na(analysis_time)) || length(unique(analysis_time)) !=
                    2) {
                    analysis_time2 <- NULL
                  } else {
                    sample_rate <- repository$sample_rate
                    ntp <- nrow(filtered_data)
                    timepoint_range <- round(analysis_time *
                      sample_rate)
                    timepoint_range[timepoint_range < 1] <- 1
                    timepoint_range[timepoint_range > ntp] <- ntp
                    if (length(timepoint_range) != 2 || !is.numeric(timepoint_range) ||
                      any(is.na(timepoint_range)) || !isTRUE(timepoint_range[1] <
                      timepoint_range[2])) {
                      analysis_time2 <- NULL
                    } else {
                      analysis_time2 <- structure((timepoint_range -
                        1)/sample_rate, timepoint_range = timepoint_range)
                    }
                  }
                }
                return(analysis_time2)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("analysis_time2",
                  e, quote({
                    if (length(analysis_time) != 2 || !is.numeric(analysis_time) ||
                      any(is.na(analysis_time)) || length(unique(analysis_time)) !=
                      2) {
                      analysis_time2 <- NULL
                    } else {
                      sample_rate <- repository$sample_rate
                      ntp <- nrow(filtered_data)
                      timepoint_range <- round(analysis_time *
                        sample_rate)
                      timepoint_range[timepoint_range < 1] <- 1
                      timepoint_range[timepoint_range > ntp] <- ntp
                      if (length(timepoint_range) != 2 || !is.numeric(timepoint_range) ||
                        any(is.na(timepoint_range)) || !isTRUE(timepoint_range[1] <
                        timepoint_range[2])) {
                        analysis_time2 <- NULL
                      } else {
                        analysis_time2 <- structure((timepoint_range -
                          1)/sample_rate, timepoint_range = timepoint_range)
                      }
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("analysis_time", "repository", "filtered_data"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), calculate_channel_labels_and_colors = targets::tar_target_raw(name = "plot_data_label_colors",
        command = quote({
            tryCatch({
                {
                  electrodes <- repository$subject$electrodes
                  cols <- rep(1, length(electrodes))
                  cols[sel_highlights] <- 2
                  display_electrodes <- electrodes[sel_electrodes]
                  display_color <- cols[sel_electrodes]
                  plot_data_label_colors <- list(labels = display_electrodes,
                    colors = display_color)
                }
                return(plot_data_label_colors)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_data_label_colors",
                  e, quote({
                    electrodes <- repository$subject$electrodes
                    cols <- rep(1, length(electrodes))
                    cols[sel_highlights] <- 2
                    display_electrodes <- electrodes[sel_electrodes]
                    display_color <- cols[sel_electrodes]
                    plot_data_label_colors <- list(labels = display_electrodes,
                      colors = display_color)
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("repository", "sel_highlights", "sel_electrodes"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), plot_data_filtered_voltage_overall = targets::tar_target_raw(name = "plot_data_filtered_voltage_overall",
        command = quote({
            tryCatch({
                {
                  if (!any(sel_electrodes)) {
                    plot_data_filtered_voltage_overall <- "No electrode channel to display"
                  } else {
                    ntp <- nrow(filtered_data)
                    nelec <- sum(sel_electrodes)
                    limit <- graphics_matplot_max_points
                    sample_rate <- repository$sample_rate
                    time_range <- c(0, (ntp - 1)/sample_rate)
                    if (ntp * nelec > limit) {
                      dsample <- ntp * nelec/limit
                      tidx <- round(seq(1, ntp, by = dsample))
                      sample_rate <- sample_rate/dsample
                    } else {
                      tidx <- seq_len(ntp)
                    }
                    plot_data <- filtered_data[tidx, sel_electrodes,
                      drop = FALSE, dimnames = FALSE]
                    if (length(vertical_spacing) != 1 || is.na(vertical_spacing) ||
                      vertical_spacing <= 0) {
                      vertical_spacing <- 0.999
                    }
                    if (vertical_spacing <= 1) {
                      if (length(plot_data) > 1e+05) {
                        space <- stats::quantile(plot_data[sample(length(plot_data),
                          1e+05)], vertical_spacing, na.rm = TRUE) *
                          2
                      } else {
                        space <- stats::quantile(plot_data, vertical_spacing,
                          na.rm = TRUE) * 2
                      }
                    } else {
                      space <- vertical_spacing
                    }
                    plot_data_filtered_voltage_overall <- list(data = t(plot_data),
                      sample_rate = sample_rate, spacing = space,
                      labels = plot_data_label_colors$labels,
                      colors = plot_data_label_colors$colors,
                      time_range = time_range)
                  }
                }
                return(plot_data_filtered_voltage_overall)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_data_filtered_voltage_overall",
                  e, quote({
                    if (!any(sel_electrodes)) {
                      plot_data_filtered_voltage_overall <- "No electrode channel to display"
                    } else {
                      ntp <- nrow(filtered_data)
                      nelec <- sum(sel_electrodes)
                      limit <- graphics_matplot_max_points
                      sample_rate <- repository$sample_rate
                      time_range <- c(0, (ntp - 1)/sample_rate)
                      if (ntp * nelec > limit) {
                        dsample <- ntp * nelec/limit
                        tidx <- round(seq(1, ntp, by = dsample))
                        sample_rate <- sample_rate/dsample
                      } else {
                        tidx <- seq_len(ntp)
                      }
                      plot_data <- filtered_data[tidx, sel_electrodes,
                        drop = FALSE, dimnames = FALSE]
                      if (length(vertical_spacing) != 1 || is.na(vertical_spacing) ||
                        vertical_spacing <= 0) {
                        vertical_spacing <- 0.999
                      }
                      if (vertical_spacing <= 1) {
                        if (length(plot_data) > 1e+05) {
                          space <- stats::quantile(plot_data[sample(length(plot_data),
                            1e+05)], vertical_spacing, na.rm = TRUE) *
                            2
                        } else {
                          space <- stats::quantile(plot_data,
                            vertical_spacing, na.rm = TRUE) *
                            2
                        }
                      } else {
                        space <- vertical_spacing
                      }
                      plot_data_filtered_voltage_overall <- list(data = t(plot_data),
                        sample_rate = sample_rate, spacing = space,
                        labels = plot_data_label_colors$labels,
                        colors = plot_data_label_colors$colors,
                        time_range = time_range)
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("sel_electrodes", "filtered_data", "repository",
        "vertical_spacing", "plot_data_label_colors"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), get_absolute_spacing = targets::tar_target_raw(name = "vertical_spacing2",
        command = quote({
            tryCatch({
                {
                  vertical_spacing2 <- vertical_spacing
                  if (is.list(plot_data_filtered_voltage_overall)) {
                    spacing <- plot_data_filtered_voltage_overall$spacing
                    if (length(spacing) == 1 && !is.na(spacing) &&
                      spacing > 1) {
                      vertical_spacing2 <- spacing
                    }
                  }
                }
                return(vertical_spacing2)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("vertical_spacing2",
                  e, quote({
                    vertical_spacing2 <- vertical_spacing
                    if (is.list(plot_data_filtered_voltage_overall)) {
                      spacing <- plot_data_filtered_voltage_overall$spacing
                      if (length(spacing) == 1 && !is.na(spacing) &&
                        spacing > 1) {
                        vertical_spacing2 <- spacing
                      }
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("vertical_spacing", "plot_data_filtered_voltage_overall"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), plot_data_filtered_voltage_subset = targets::tar_target_raw(name = "plot_data_filtered_voltage_subset",
        command = quote({
            tryCatch({
                {
                  if (length(analysis_time2) != 2) {
                    plot_data_filtered_voltage_subset <- "Invalid analysis time range"
                  } else if (!any(sel_electrodes)) {
                    plot_data_filtered_voltage_subset <- "No electrode channel to display"
                  } else {
                    sample_rate <- repository$sample_rate
                    timepoint_range <- attr(analysis_time2, "timepoint_range")
                    ntp <- timepoint_range[2] - timepoint_range[1] +
                      1
                    nelec <- sum(sel_electrodes)
                    limit <- graphics_matplot_max_points
                    if (ntp * nelec > limit) {
                      dsample <- ntp * nelec/limit
                      tidx <- round(seq(timepoint_range[1], timepoint_range[2],
                        by = dsample))
                      sample_rate <- sample_rate/dsample
                    } else {
                      tidx <- seq.int(timepoint_range[1], timepoint_range[2])
                    }
                    plot_data <- filtered_data[tidx, sel_electrodes,
                      drop = FALSE, dimnames = FALSE]
                    plot_data_filtered_voltage_subset <- list(data = t(plot_data),
                      sample_rate = sample_rate, spacing = vertical_spacing2,
                      labels = plot_data_label_colors$labels,
                      colors = plot_data_label_colors$colors,
                      time_range = analysis_time2)
                  }
                }
                return(plot_data_filtered_voltage_subset)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_data_filtered_voltage_subset",
                  e, quote({
                    if (length(analysis_time2) != 2) {
                      plot_data_filtered_voltage_subset <- "Invalid analysis time range"
                    } else if (!any(sel_electrodes)) {
                      plot_data_filtered_voltage_subset <- "No electrode channel to display"
                    } else {
                      sample_rate <- repository$sample_rate
                      timepoint_range <- attr(analysis_time2,
                        "timepoint_range")
                      ntp <- timepoint_range[2] - timepoint_range[1] +
                        1
                      nelec <- sum(sel_electrodes)
                      limit <- graphics_matplot_max_points
                      if (ntp * nelec > limit) {
                        dsample <- ntp * nelec/limit
                        tidx <- round(seq(timepoint_range[1],
                          timepoint_range[2], by = dsample))
                        sample_rate <- sample_rate/dsample
                      } else {
                        tidx <- seq.int(timepoint_range[1], timepoint_range[2])
                      }
                      plot_data <- filtered_data[tidx, sel_electrodes,
                        drop = FALSE, dimnames = FALSE]
                      plot_data_filtered_voltage_subset <- list(data = t(plot_data),
                        sample_rate = sample_rate, spacing = vertical_spacing2,
                        labels = plot_data_label_colors$labels,
                        colors = plot_data_label_colors$colors,
                        time_range = analysis_time2)
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("analysis_time2", "sel_electrodes", "repository",
        "filtered_data", "vertical_spacing2", "plot_data_label_colors"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), plot_data_pwelch = targets::tar_target_raw(name = "plot_data_pwelch",
        command = quote({
            tryCatch({
                {
                  if (!any(sel_electrodes)) {
                    plot_data_pwelch <- "No electrode channel to display"
                  } else {
                    dnames <- dimnames(pwelch_data)
                    freq_range <- c(pwelch_frequency_limit, c(0,
                      300))[c(1, 2)]
                    row_sel <- dnames$Frequency >= freq_range[[1]] &
                      dnames$Frequency <= freq_range[[2]]
                    if (!any(row_sel)) {
                      plot_data_pwelch <- "Frequency range is too small. Please adjust the input in '\n  Welch Periodogram Settings'."
                    } else {
                      display_electrodes_ = dnames$Electrode[sel_electrodes]
                      hightights_ <- dnames$Electrode[sel_highlights]
                      plot_data <- pwelch_data[row_sel, sel_electrodes,
                        drop = FALSE, dimnames = FALSE]
                      plot_data <- 10 * log10(plot_data)
                      freq <- dnames$Frequency[row_sel]
                      freq_sel <- rep(FALSE, length(freq))
                      for (idx in 1:10) {
                        freq_sel <- freq_sel | ((freq >= (idx *
                          50 - 2) & freq <= (idx * 50 + 2)) |
                          (freq >= (idx * 60 - 2) & freq <= (idx *
                            60 + 2)))
                        if (mean(freq_sel) > 0.9) {
                          break
                        }
                      }
                      freq_sel <- !freq_sel
                      tmp <- NULL
                      if (any(freq_sel)) {
                        tmp <- plot_data[freq_sel, ]
                        tmp <- tmp[is.finite(tmp)]
                      }
                      if (!length(tmp)) {
                        tmp <- plot_data
                        tmp <- tmp[is.finite(tmp)]
                      }
                      ylim <- range(tmp, na.rm = FALSE)
                      has_highlight <- any(sel_highlights)
                      mean1 <- rowMeans(plot_data, na.rm = TRUE)
                      if (has_highlight) {
                        mean2 <- rowMeans(plot_data[, display_electrodes_ %in%
                          hightights_, drop = FALSE], na.rm = TRUE)
                      } else {
                        mean2 <- NULL
                      }
                      if (isTRUE(cleaned_inputs$filter_bandpass$enabled)) {
                        filter_range <- cleaned_inputs$filter_bandpass$range
                      } else {
                        filter_range <- NULL
                      }
                      plot_data_pwelch <- list(frequencies = freq,
                        frequency_range = freq_range, data = plot_data,
                        has_highlight = has_highlight, data_range_trim = ylim,
                        mean_overall = mean1, mean_highlighted = mean2,
                        electrodes = plot_data_label_colors$labels,
                        highlights = hightights_, colors = plot_data_label_colors$colors,
                        bandpass_filter_range = filter_range)
                    }
                  }
                }
                return(plot_data_pwelch)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_data_pwelch",
                  e, quote({
                    if (!any(sel_electrodes)) {
                      plot_data_pwelch <- "No electrode channel to display"
                    } else {
                      dnames <- dimnames(pwelch_data)
                      freq_range <- c(pwelch_frequency_limit,
                        c(0, 300))[c(1, 2)]
                      row_sel <- dnames$Frequency >= freq_range[[1]] &
                        dnames$Frequency <= freq_range[[2]]
                      if (!any(row_sel)) {
                        plot_data_pwelch <- "Frequency range is too small. Please adjust the input in '\n  Welch Periodogram Settings'."
                      } else {
                        display_electrodes_ = dnames$Electrode[sel_electrodes]
                        hightights_ <- dnames$Electrode[sel_highlights]
                        plot_data <- pwelch_data[row_sel, sel_electrodes,
                          drop = FALSE, dimnames = FALSE]
                        plot_data <- 10 * log10(plot_data)
                        freq <- dnames$Frequency[row_sel]
                        freq_sel <- rep(FALSE, length(freq))
                        for (idx in 1:10) {
                          freq_sel <- freq_sel | ((freq >= (idx *
                            50 - 2) & freq <= (idx * 50 + 2)) |
                            (freq >= (idx * 60 - 2) & freq <=
                              (idx * 60 + 2)))
                          if (mean(freq_sel) > 0.9) {
                            break
                          }
                        }
                        freq_sel <- !freq_sel
                        tmp <- NULL
                        if (any(freq_sel)) {
                          tmp <- plot_data[freq_sel, ]
                          tmp <- tmp[is.finite(tmp)]
                        }
                        if (!length(tmp)) {
                          tmp <- plot_data
                          tmp <- tmp[is.finite(tmp)]
                        }
                        ylim <- range(tmp, na.rm = FALSE)
                        has_highlight <- any(sel_highlights)
                        mean1 <- rowMeans(plot_data, na.rm = TRUE)
                        if (has_highlight) {
                          mean2 <- rowMeans(plot_data[, display_electrodes_ %in%
                            hightights_, drop = FALSE], na.rm = TRUE)
                        } else {
                          mean2 <- NULL
                        }
                        if (isTRUE(cleaned_inputs$filter_bandpass$enabled)) {
                          filter_range <- cleaned_inputs$filter_bandpass$range
                        } else {
                          filter_range <- NULL
                        }
                        plot_data_pwelch <- list(frequencies = freq,
                          frequency_range = freq_range, data = plot_data,
                          has_highlight = has_highlight, data_range_trim = ylim,
                          mean_overall = mean1, mean_highlighted = mean2,
                          electrodes = plot_data_label_colors$labels,
                          highlights = hightights_, colors = plot_data_label_colors$colors,
                          bandpass_filter_range = filter_range)
                      }
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("sel_electrodes", "pwelch_data", "pwelch_frequency_limit",
        "sel_highlights", "cleaned_inputs", "plot_data_label_colors"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), plot_data_pwelch_subset = targets::tar_target_raw(name = "plot_data_pwelch_subset",
        command = quote({
            tryCatch({
                {
                  if (length(analysis_time2) != 2) {
                    plot_data_pwelch_subset <- ""
                  } else if (!is.list(plot_data_pwelch) || !length(plot_data_pwelch)) {
                    if (is.character(plot_data_pwelch)) {
                      plot_data_pwelch_subset <- plot_data_pwelch
                    } else {
                      plot_data_pwelch_subset <- "Unable to generate subset Welch Periodogram"
                    }
                  } else {
                    timepoint_range <- attr(analysis_time2, "timepoint_range")
                    window_len <- pwelch_params2$window_size
                    noverlap <- pwelch_params2$noverlap
                    row_sel <- seq.int(timepoint_range[1], timepoint_range[2])
                    step <- max(floor(window_len - noverlap +
                      0.99), 1)
                    offset <- seq(1, max(length(row_sel) - window_len +
                      1, 1), by = step)
                    if (!length(offset)) {
                      plot_data_pwelch_subset <- "Selected time-range is too small. Cannot calculate Welch-Periodogram for the subset."
                    } else {
                      indata <- repository$data[[analysis_block]]
                      sample_rate <- repository$sample_rate
                      has_highlight <- plot_data_pwelch$has_highlight
                      indata <- indata[row_sel, sel_electrodes,
                        drop = FALSE, dimnames = FALSE]
                      indata[is.na(indata)] <- 0
                      pwelch_result <- ravetools::pwelch(x = t(indata),
                        fs = sample_rate, window = window_len,
                        noverlap = noverlap)
                      freq_sel <- (pwelch_result$freq >= plot_data_pwelch$frequency_range[[1]]) &
                        (pwelch_result$freq <= plot_data_pwelch$frequency_range[[2]])
                      freq <- pwelch_result$freq[freq_sel]
                      plot_data <- t(pwelch_result$spec[, freq_sel,
                        drop = FALSE])
                      mean1 <- 10 * log10(rowMeans(plot_data))
                      electrodes <- plot_data_pwelch$electrodes
                      highlights <- plot_data_pwelch$highlights
                      if (has_highlight) {
                        mean2 <- 10 * log10(rowMeans(plot_data[,
                          electrodes %in% highlights, drop = FALSE]))
                      } else {
                        mean2 <- NULL
                      }
                      plot_data_pwelch_subset <- list(frequencies = freq,
                        frequency_range = plot_data_pwelch$frequency_range,
                        data = 10 * log10(plot_data), has_highlight = has_highlight,
                        data_range_trim = plot_data_pwelch$data_range_trim,
                        mean_overall = mean1, mean_highlighted = mean2,
                        electrodes = electrodes, highlights = highlights,
                        colors = plot_data_pwelch$colors, bandpass_filter_range = plot_data_pwelch$bandpass_filter_range)
                    }
                  }
                }
                return(plot_data_pwelch_subset)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_data_pwelch_subset",
                  e, quote({
                    if (length(analysis_time2) != 2) {
                      plot_data_pwelch_subset <- ""
                    } else if (!is.list(plot_data_pwelch) ||
                      !length(plot_data_pwelch)) {
                      if (is.character(plot_data_pwelch)) {
                        plot_data_pwelch_subset <- plot_data_pwelch
                      } else {
                        plot_data_pwelch_subset <- "Unable to generate subset Welch Periodogram"
                      }
                    } else {
                      timepoint_range <- attr(analysis_time2,
                        "timepoint_range")
                      window_len <- pwelch_params2$window_size
                      noverlap <- pwelch_params2$noverlap
                      row_sel <- seq.int(timepoint_range[1],
                        timepoint_range[2])
                      step <- max(floor(window_len - noverlap +
                        0.99), 1)
                      offset <- seq(1, max(length(row_sel) -
                        window_len + 1, 1), by = step)
                      if (!length(offset)) {
                        plot_data_pwelch_subset <- "Selected time-range is too small. Cannot calculate Welch-Periodogram for the subset."
                      } else {
                        indata <- repository$data[[analysis_block]]
                        sample_rate <- repository$sample_rate
                        has_highlight <- plot_data_pwelch$has_highlight
                        indata <- indata[row_sel, sel_electrodes,
                          drop = FALSE, dimnames = FALSE]
                        indata[is.na(indata)] <- 0
                        pwelch_result <- ravetools::pwelch(x = t(indata),
                          fs = sample_rate, window = window_len,
                          noverlap = noverlap)
                        freq_sel <- (pwelch_result$freq >= plot_data_pwelch$frequency_range[[1]]) &
                          (pwelch_result$freq <= plot_data_pwelch$frequency_range[[2]])
                        freq <- pwelch_result$freq[freq_sel]
                        plot_data <- t(pwelch_result$spec[, freq_sel,
                          drop = FALSE])
                        mean1 <- 10 * log10(rowMeans(plot_data))
                        electrodes <- plot_data_pwelch$electrodes
                        highlights <- plot_data_pwelch$highlights
                        if (has_highlight) {
                          mean2 <- 10 * log10(rowMeans(plot_data[,
                            electrodes %in% highlights, drop = FALSE]))
                        } else {
                          mean2 <- NULL
                        }
                        plot_data_pwelch_subset <- list(frequencies = freq,
                          frequency_range = plot_data_pwelch$frequency_range,
                          data = 10 * log10(plot_data), has_highlight = has_highlight,
                          data_range_trim = plot_data_pwelch$data_range_trim,
                          mean_overall = mean1, mean_highlighted = mean2,
                          electrodes = electrodes, highlights = highlights,
                          colors = plot_data_pwelch$colors, bandpass_filter_range = plot_data_pwelch$bandpass_filter_range)
                      }
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("analysis_time2", "plot_data_pwelch", "pwelch_params2",
        "repository", "analysis_block", "sel_electrodes"), cue = targets::tar_cue("thorough"),
        pattern = NULL, iteration = "list"), plot_filtered_signals = targets::tar_target_raw(name = "plot_filtered_signals",
        command = quote({
            tryCatch({
                {
                  plot_filtered_signals <- TRUE
                  plot_data <- plot_data_filtered_voltage_overall
                  if (is.list(plot_data) && length(plot_data)) {
                    if (length(unique(plot_data$colors)) > 1) {
                      pal <- c("gray60", "orange")
                    } else {
                      pal <- graphics::par("fg")
                    }
                    ravetools::plot_signals(plot_data$data, sample_rate = plot_data$sample_rate,
                      channel_names = plot_data$labels, col = pal[plot_data$colors],
                      space = plot_data$spacing, space_mode = "absolute",
                      main = sprintf("Filtered signals (%s)",
                        dipsaus::deparse_svec(plot_data$labels)),
                      tck = -0.005, yaxs = "r")
                  } else {
                    plot_filtered_signals <- plot_data
                  }
                }
                return(plot_filtered_signals)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_filtered_signals",
                  e, quote({
                    plot_filtered_signals <- TRUE
                    plot_data <- plot_data_filtered_voltage_overall
                    if (is.list(plot_data) && length(plot_data)) {
                      if (length(unique(plot_data$colors)) >
                        1) {
                        pal <- c("gray60", "orange")
                      } else {
                        pal <- graphics::par("fg")
                      }
                      ravetools::plot_signals(plot_data$data,
                        sample_rate = plot_data$sample_rate,
                        channel_names = plot_data$labels, col = pal[plot_data$colors],
                        space = plot_data$spacing, space_mode = "absolute",
                        main = sprintf("Filtered signals (%s)",
                          dipsaus::deparse_svec(plot_data$labels)),
                        tck = -0.005, yaxs = "r")
                    } else {
                      plot_filtered_signals <- plot_data
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = "plot_data_filtered_voltage_overall", cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), plot_filtered_signals_subset = targets::tar_target_raw(name = "plot_filtered_signals_subset",
        command = quote({
            tryCatch({
                {
                  plot_filtered_signals_subset <- TRUE
                  plot_data <- plot_data_filtered_voltage_subset
                  if (is.list(plot_data) && length(plot_data)) {
                    if (length(unique(plot_data$colors)) > 1) {
                      pal <- c("gray60", "orange")
                    } else {
                      pal <- graphics::par("fg")
                    }
                    time_range <- plot_data$time_range
                    ravetools::plot_signals(plot_data$data, sample_rate = plot_data$sample_rate,
                      time_shift = time_range[[1]], channel_names = plot_data$labels,
                      col = pal[plot_data$colors], space = plot_data$spacing,
                      space_mode = "absolute", main = sprintf("Data slice (%.2f sec)",
                        time_range[[2]] - time_range[[1]]), tck = -0.005,
                      yaxs = "r")
                  } else {
                    plot_filtered_signals_subset <- plot_data
                  }
                }
                return(plot_filtered_signals_subset)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_filtered_signals_subset",
                  e, quote({
                    plot_filtered_signals_subset <- TRUE
                    plot_data <- plot_data_filtered_voltage_subset
                    if (is.list(plot_data) && length(plot_data)) {
                      if (length(unique(plot_data$colors)) >
                        1) {
                        pal <- c("gray60", "orange")
                      } else {
                        pal <- graphics::par("fg")
                      }
                      time_range <- plot_data$time_range
                      ravetools::plot_signals(plot_data$data,
                        sample_rate = plot_data$sample_rate,
                        time_shift = time_range[[1]], channel_names = plot_data$labels,
                        col = pal[plot_data$colors], space = plot_data$spacing,
                        space_mode = "absolute", main = sprintf("Data slice (%.2f sec)",
                          time_range[[2]] - time_range[[1]]),
                        tck = -0.005, yaxs = "r")
                    } else {
                      plot_filtered_signals_subset <- plot_data
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = "plot_data_filtered_voltage_subset", cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), plot_pwelch = targets::tar_target_raw(name = "plot_pwelch",
        command = quote({
            tryCatch({
                {
                  plot_pwelch <- TRUE
                  plot_data <- plot_data_pwelch
                  if (is.list(plot_data) && length(plot_data)) {
                    cex <- 1
                    mar <- c(2.6, 3.8, 2.1, 0.6) * (0.5 + cex/2)
                    mgp <- cex * c(2, 0.5, 0)
                    tck <- -0.02
                    xline <- 1.2 * cex
                    yline <- 2 * cex
                    xaxs <- "i"
                    yaxs <- "i"
                    main <- "Welch periodogram (no filter)"
                    cex_params <- graphics::par("fg", "bg", "mgp",
                      "mar", "mai", "cex.main", "cex.lab", "cex.axis",
                      "cex.sub")
                    graphics::par(mar = mar, mgp = mgp)
                    on.exit({
                      do.call(graphics::par, cex_params)
                    }, add = TRUE, after = FALSE)
                    fg <- cex_params$fg
                    if (length(unique(plot_data$colors)) > 1) {
                      pal <- c("gray60", "orange")
                    } else {
                      pal <- c(fg, fg)
                    }
                    alpha <- 0.7
                    col <- pal[plot_data$colors]
                    ylim <- plot_data$data_range_trim
                    freq <- plot_data$frequencies
                    graphics::matplot(x = freq, y = plot_data$data,
                      col = col, ylim = plot_data$data_range_trim,
                      type = "l", cex = cex, lty = 1, lwd = 0.5,
                      las = 1, axes = FALSE, xaxs = xaxs, yaxs = yaxs,
                      cex.main = cex_params$cex.main * cex, main = main,
                      log = "x", xlab = "", ylab = "")
                    filter_range <- plot_data$bandpass_filter_range
                    if (length(filter_range)) {
                      graphics::abline(v = filter_range, lty = 2)
                      graphics::text(x = mean(filter_range),
                        y = ylim[[2]], labels = "Bandpass filter",
                        adj = c(0.55, 1))
                    }
                    graphics::grid()
                    graphics::lines(x = freq, y = plot_data$mean_overall,
                      col = fg, lty = 1, lwd = 3)
                    if (plot_data$has_highlight) {
                      graphics::lines(x = freq, y = plot_data$mean_highlighted,
                        col = pal[[2]], lty = 1, lwd = 3)
                    }
                    graphics::axis(1, at = pretty(freq), tck = -0.02,
                      cex = cex, cex.main = cex_params$cex.main *
                        cex, cex.lab = cex_params$cex.lab * cex,
                      cex.axis = cex_params$cex.axis * cex)
                    graphics::axis(2, at = pretty(plot_data$data_range_trim),
                      tck = -0.02, cex = cex, cex.main = cex_params$cex.main *
                        cex, cex.lab = cex_params$cex.lab * cex,
                      cex.axis = cex_params$cex.axis * cex)
                    graphics::mtext(side = 2, text = "Power (dB)",
                      line = yline, cex = cex_params$cex.lab *
                        cex)
                    graphics::mtext(side = 1, text = "log(Frequency)",
                      line = xline, cex = cex_params$cex.lab *
                        cex)
                    if (plot_data$has_highlight) {
                      lg_text <- c("Highlighted", "Mean highlighted",
                        "Mean of all")
                      lg_col <- pal[c(2, 2, 1)]
                      lg_lwd <- c(0.5, 3, 3)
                    } else {
                      lg_text <- c("Mean of all")
                      lg_col <- pal[2]
                      lg_lwd <- 3
                    }
                    graphics::legend("topright", lg_text, lty = 1,
                      col = lg_col, lwd = lg_lwd, bty = "n",
                      text.col = lg_col)
                  } else {
                    plot_pwelch <- plot_data
                  }
                }
                return(plot_pwelch)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_pwelch",
                  e, quote({
                    plot_pwelch <- TRUE
                    plot_data <- plot_data_pwelch
                    if (is.list(plot_data) && length(plot_data)) {
                      cex <- 1
                      mar <- c(2.6, 3.8, 2.1, 0.6) * (0.5 + cex/2)
                      mgp <- cex * c(2, 0.5, 0)
                      tck <- -0.02
                      xline <- 1.2 * cex
                      yline <- 2 * cex
                      xaxs <- "i"
                      yaxs <- "i"
                      main <- "Welch periodogram (no filter)"
                      cex_params <- graphics::par("fg", "bg",
                        "mgp", "mar", "mai", "cex.main", "cex.lab",
                        "cex.axis", "cex.sub")
                      graphics::par(mar = mar, mgp = mgp)
                      on.exit({
                        do.call(graphics::par, cex_params)
                      }, add = TRUE, after = FALSE)
                      fg <- cex_params$fg
                      if (length(unique(plot_data$colors)) >
                        1) {
                        pal <- c("gray60", "orange")
                      } else {
                        pal <- c(fg, fg)
                      }
                      alpha <- 0.7
                      col <- pal[plot_data$colors]
                      ylim <- plot_data$data_range_trim
                      freq <- plot_data$frequencies
                      graphics::matplot(x = freq, y = plot_data$data,
                        col = col, ylim = plot_data$data_range_trim,
                        type = "l", cex = cex, lty = 1, lwd = 0.5,
                        las = 1, axes = FALSE, xaxs = xaxs, yaxs = yaxs,
                        cex.main = cex_params$cex.main * cex,
                        main = main, log = "x", xlab = "", ylab = "")
                      filter_range <- plot_data$bandpass_filter_range
                      if (length(filter_range)) {
                        graphics::abline(v = filter_range, lty = 2)
                        graphics::text(x = mean(filter_range),
                          y = ylim[[2]], labels = "Bandpass filter",
                          adj = c(0.55, 1))
                      }
                      graphics::grid()
                      graphics::lines(x = freq, y = plot_data$mean_overall,
                        col = fg, lty = 1, lwd = 3)
                      if (plot_data$has_highlight) {
                        graphics::lines(x = freq, y = plot_data$mean_highlighted,
                          col = pal[[2]], lty = 1, lwd = 3)
                      }
                      graphics::axis(1, at = pretty(freq), tck = -0.02,
                        cex = cex, cex.main = cex_params$cex.main *
                          cex, cex.lab = cex_params$cex.lab *
                          cex, cex.axis = cex_params$cex.axis *
                          cex)
                      graphics::axis(2, at = pretty(plot_data$data_range_trim),
                        tck = -0.02, cex = cex, cex.main = cex_params$cex.main *
                          cex, cex.lab = cex_params$cex.lab *
                          cex, cex.axis = cex_params$cex.axis *
                          cex)
                      graphics::mtext(side = 2, text = "Power (dB)",
                        line = yline, cex = cex_params$cex.lab *
                          cex)
                      graphics::mtext(side = 1, text = "log(Frequency)",
                        line = xline, cex = cex_params$cex.lab *
                          cex)
                      if (plot_data$has_highlight) {
                        lg_text <- c("Highlighted", "Mean highlighted",
                          "Mean of all")
                        lg_col <- pal[c(2, 2, 1)]
                        lg_lwd <- c(0.5, 3, 3)
                      } else {
                        lg_text <- c("Mean of all")
                        lg_col <- pal[2]
                        lg_lwd <- 3
                      }
                      graphics::legend("topright", lg_text, lty = 1,
                        col = lg_col, lwd = lg_lwd, bty = "n",
                        text.col = lg_col)
                    } else {
                      plot_pwelch <- plot_data
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = "plot_data_pwelch", cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), plot_pwelch_subset = targets::tar_target_raw(name = "plot_pwelch_subset",
        command = quote({
            tryCatch({
                {
                  plot_pwelch_subset <- TRUE
                  plot_data0 <- plot_data_pwelch
                  plot_data <- plot_data_pwelch_subset
                  if (is.list(plot_data0) && length(plot_data0) &&
                    is.list(plot_data) && length(plot_data)) {
                    cex <- 1
                    mar <- c(2.6, 3.8, 2.1, 0.6) * (0.5 + cex/2)
                    mgp <- cex * c(2, 0.5, 0)
                    tck <- -0.02
                    xline <- 1.2 * cex
                    yline <- 2 * cex
                    xaxs <- "i"
                    yaxs <- "i"
                    main <- "Welch periodogram (subset slice)"
                    cex_params <- graphics::par("fg", "bg", "mgp",
                      "mar", "mai", "cex.main", "cex.lab", "cex.axis",
                      "cex.sub")
                    graphics::par(mar = mar, mgp = mgp)
                    on.exit({
                      do.call(graphics::par, cex_params)
                    }, add = TRUE, after = FALSE)
                    fg <- cex_params$fg
                    if (length(unique(plot_data$colors)) > 1) {
                      pal <- c("gray60", "orange")
                    } else {
                      pal <- c(fg, fg)
                    }
                    alpha <- 0.7
                    col <- pal[plot_data$colors]
                    ylim <- plot_data$data_range_trim
                    freq <- plot_data$frequencies
                    graphics::matplot(x = freq, y = plot_data$data,
                      col = col, ylim = plot_data$data_range_trim,
                      type = "l", cex = cex, lty = 1, lwd = 0.5,
                      las = 1, axes = FALSE, xaxs = xaxs, yaxs = yaxs,
                      cex.main = cex_params$cex.main * cex, main = main,
                      log = "x", xlab = "", ylab = "")
                    filter_range <- plot_data$bandpass_filter_range
                    if (length(filter_range)) {
                      graphics::abline(v = filter_range, lty = 2)
                      graphics::text(x = mean(filter_range),
                        y = ylim[[2]], labels = "Bandpass filter",
                        adj = c(0.55, 1))
                    }
                    graphics::grid()
                    graphics::lines(x = plot_data0$frequencies,
                      y = plot_data0$mean_overall, col = fg,
                      lty = 3, lwd = 3)
                    graphics::lines(x = freq, y = plot_data$mean_overall,
                      col = fg, lty = 1, lwd = 3)
                    if (plot_data$has_highlight) {
                      graphics::lines(x = plot_data0$frequencies,
                        y = plot_data0$mean_highlighted, col = pal[[2]],
                        lty = 3, lwd = 3)
                      graphics::lines(x = freq, y = plot_data$mean_highlighted,
                        col = pal[[2]], lty = 1, lwd = 3)
                    }
                    graphics::axis(1, at = pretty(freq), tck = -0.02,
                      cex = cex, cex.main = cex_params$cex.main *
                        cex, cex.lab = cex_params$cex.lab * cex,
                      cex.axis = cex_params$cex.axis * cex)
                    graphics::axis(2, at = pretty(plot_data$data_range_trim),
                      tck = -0.02, cex = cex, cex.main = cex_params$cex.main *
                        cex, cex.lab = cex_params$cex.lab * cex,
                      cex.axis = cex_params$cex.axis * cex)
                    graphics::mtext(side = 2, text = "Power (dB)",
                      line = yline, cex = cex_params$cex.lab *
                        cex)
                    graphics::mtext(side = 1, text = "log(Frequency)",
                      line = xline, cex = cex_params$cex.lab *
                        cex)
                    if (plot_data$has_highlight) {
                      lg_text <- c("Mean highlighted (subset)",
                        "Mean of all (subset)", "Mean highlighted",
                        "Mean of all")
                      lg_col <- pal[c(2, 1, 2, 1)]
                      lg_lwd <- c(3, 3, 3, 3)
                      lg_lty <- c(1, 1, 3, 3)
                    } else {
                      lg_text <- c("Mean of all (subset)", "Mean of all")
                      lg_col <- pal[2]
                      lg_lwd <- 3
                      lg_lty <- c(1, 3)
                    }
                    graphics::legend("topright", lg_text, lty = lg_lty,
                      col = lg_col, lwd = lg_lwd, bty = "n",
                      text.col = lg_col, ncol = 2)
                  } else {
                    plot_pwelch_subset <- plot_data
                  }
                }
                return(plot_pwelch_subset)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error("plot_pwelch_subset",
                  e, quote({
                    plot_pwelch_subset <- TRUE
                    plot_data0 <- plot_data_pwelch
                    plot_data <- plot_data_pwelch_subset
                    if (is.list(plot_data0) && length(plot_data0) &&
                      is.list(plot_data) && length(plot_data)) {
                      cex <- 1
                      mar <- c(2.6, 3.8, 2.1, 0.6) * (0.5 + cex/2)
                      mgp <- cex * c(2, 0.5, 0)
                      tck <- -0.02
                      xline <- 1.2 * cex
                      yline <- 2 * cex
                      xaxs <- "i"
                      yaxs <- "i"
                      main <- "Welch periodogram (subset slice)"
                      cex_params <- graphics::par("fg", "bg",
                        "mgp", "mar", "mai", "cex.main", "cex.lab",
                        "cex.axis", "cex.sub")
                      graphics::par(mar = mar, mgp = mgp)
                      on.exit({
                        do.call(graphics::par, cex_params)
                      }, add = TRUE, after = FALSE)
                      fg <- cex_params$fg
                      if (length(unique(plot_data$colors)) >
                        1) {
                        pal <- c("gray60", "orange")
                      } else {
                        pal <- c(fg, fg)
                      }
                      alpha <- 0.7
                      col <- pal[plot_data$colors]
                      ylim <- plot_data$data_range_trim
                      freq <- plot_data$frequencies
                      graphics::matplot(x = freq, y = plot_data$data,
                        col = col, ylim = plot_data$data_range_trim,
                        type = "l", cex = cex, lty = 1, lwd = 0.5,
                        las = 1, axes = FALSE, xaxs = xaxs, yaxs = yaxs,
                        cex.main = cex_params$cex.main * cex,
                        main = main, log = "x", xlab = "", ylab = "")
                      filter_range <- plot_data$bandpass_filter_range
                      if (length(filter_range)) {
                        graphics::abline(v = filter_range, lty = 2)
                        graphics::text(x = mean(filter_range),
                          y = ylim[[2]], labels = "Bandpass filter",
                          adj = c(0.55, 1))
                      }
                      graphics::grid()
                      graphics::lines(x = plot_data0$frequencies,
                        y = plot_data0$mean_overall, col = fg,
                        lty = 3, lwd = 3)
                      graphics::lines(x = freq, y = plot_data$mean_overall,
                        col = fg, lty = 1, lwd = 3)
                      if (plot_data$has_highlight) {
                        graphics::lines(x = plot_data0$frequencies,
                          y = plot_data0$mean_highlighted, col = pal[[2]],
                          lty = 3, lwd = 3)
                        graphics::lines(x = freq, y = plot_data$mean_highlighted,
                          col = pal[[2]], lty = 1, lwd = 3)
                      }
                      graphics::axis(1, at = pretty(freq), tck = -0.02,
                        cex = cex, cex.main = cex_params$cex.main *
                          cex, cex.lab = cex_params$cex.lab *
                          cex, cex.axis = cex_params$cex.axis *
                          cex)
                      graphics::axis(2, at = pretty(plot_data$data_range_trim),
                        tck = -0.02, cex = cex, cex.main = cex_params$cex.main *
                          cex, cex.lab = cex_params$cex.lab *
                          cex, cex.axis = cex_params$cex.axis *
                          cex)
                      graphics::mtext(side = 2, text = "Power (dB)",
                        line = yline, cex = cex_params$cex.lab *
                          cex)
                      graphics::mtext(side = 1, text = "log(Frequency)",
                        line = xline, cex = cex_params$cex.lab *
                          cex)
                      if (plot_data$has_highlight) {
                        lg_text <- c("Mean highlighted (subset)",
                          "Mean of all (subset)", "Mean highlighted",
                          "Mean of all")
                        lg_col <- pal[c(2, 1, 2, 1)]
                        lg_lwd <- c(3, 3, 3, 3)
                        lg_lty <- c(1, 1, 3, 3)
                      } else {
                        lg_text <- c("Mean of all (subset)",
                          "Mean of all")
                        lg_col <- pal[2]
                        lg_lwd <- 3
                        lg_lty <- c(1, 3)
                      }
                      graphics::legend("topright", lg_text, lty = lg_lty,
                        col = lg_col, lwd = lg_lwd, bty = "n",
                        text.col = lg_col, ncol = 2)
                    } else {
                      plot_pwelch_subset <- plot_data
                    }
                  }))
            })
        }), format = asNamespace("raveio")$target_format_dynamic(NULL),
        deps = c("plot_data_pwelch", "plot_data_pwelch_subset"
        ), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"))
