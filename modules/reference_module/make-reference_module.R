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
        }), deps = "settings"), input_electrode_group = targets::tar_target_raw("electrode_group", 
        quote({
            settings[["electrode_group"]]
        }), deps = "settings"), input_changes = targets::tar_target_raw("changes", 
        quote({
            settings[["changes"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- ravecore::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
                if (!all(subject$preprocess_settings$notch_filtered)) {
                  stop("Please run Notch filter module first.")
                }
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
                    subject_code = subject_code)
                  if (!all(subject$preprocess_settings$notch_filtered)) {
                    stop("Please run Notch filter module first.")
                  }
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), obtain_previous_preprocessing = targets::tar_target_raw(name = "preprocessing_history", 
        command = quote({
            .__target_expr__. <- quote({
                previous_pipeline_path <- file.path(subject$pipeline_path, 
                  "reference_module")
                previous_data <- NULL
                if (dir.exists(previous_pipeline_path)) {
                  try(silent = TRUE, {
                    previous_pipeline <- ravepipeline::pipeline(pipeline_name = "reference_module", 
                      paths = subject$pipeline_path, temporary = TRUE)
                    previous_data <- previous_pipeline$read("preprocessing_history")
                    if (is.list(previous_data)) {
                      previous_data <- previous_data$current
                    }
                  })
                }
                current <- list(notch_filtered = subject$notch_filtered, 
                  notch_params = subject$preprocess_settings$notch_params, 
                  has_wavelet = subject$has_wavelet, wavelet_params = subject$preprocess_settings$wavelet_params)
                use_cache <- TRUE
                if (length(previous_data) != length(current)) {
                  use_cache <- FALSE
                } else {
                  for (nm in names(current)) {
                    a <- previous_data[[nm]]
                    b <- current[[nm]]
                    a <- unlist(a)
                    b <- unlist(b)
                    if (length(a) != length(b)) {
                      use_cache <- FALSE
                      break
                    }
                    tryCatch({
                      if (length(a) > 0 && any(a != b)) {
                        use_cache <- FALSE
                      }
                    }, error = function(e) {
                      use_cache <<- FALSE
                    })
                    if (!use_cache) {
                      break
                    }
                  }
                }
                preprocessing_history <- list(current = current, 
                  previous = previous_data, use_cache = use_cache)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(preprocessing_history)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "preprocessing_history", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "preprocessing_history", target_expr = quote({
                {
                  previous_pipeline_path <- file.path(subject$pipeline_path, 
                    "reference_module")
                  previous_data <- NULL
                  if (dir.exists(previous_pipeline_path)) {
                    try(silent = TRUE, {
                      previous_pipeline <- ravepipeline::pipeline(pipeline_name = "reference_module", 
                        paths = subject$pipeline_path, temporary = TRUE)
                      previous_data <- previous_pipeline$read("preprocessing_history")
                      if (is.list(previous_data)) {
                        previous_data <- previous_data$current
                      }
                    })
                  }
                  current <- list(notch_filtered = subject$notch_filtered, 
                    notch_params = subject$preprocess_settings$notch_params, 
                    has_wavelet = subject$has_wavelet, wavelet_params = subject$preprocess_settings$wavelet_params)
                  use_cache <- TRUE
                  if (length(previous_data) != length(current)) {
                    use_cache <- FALSE
                  } else {
                    for (nm in names(current)) {
                      a <- previous_data[[nm]]
                      b <- current[[nm]]
                      a <- unlist(a)
                      b <- unlist(b)
                      if (length(a) != length(b)) {
                        use_cache <- FALSE
                        break
                      }
                      tryCatch({
                        if (length(a) > 0 && any(a != b)) {
                          use_cache <- FALSE
                        }
                      }, error = function(e) {
                        use_cache <<- FALSE
                      })
                      if (!use_cache) {
                        break
                      }
                    }
                  }
                  preprocessing_history <- list(current = current, 
                    previous = previous_data, use_cache = use_cache)
                }
                preprocessing_history
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    load_presets = targets::tar_target_raw(name = "reference_table_initial", 
        command = quote({
            .__target_expr__. <- quote({
                if (isTRUE(reference_name %in% subject$reference_names)) {
                  tryCatch({
                    reference_table_initial <- subject$get_reference(reference_name, 
                      simplify = FALSE)
                    reference_table_initial <- reference_table_initial[, 
                      c("Electrode", "Group", "Reference", "Type")]
                  }, error = function(e) {
                    ravedash::logger("Unable to load reference [", 
                      reference_name, "]", level = "error")
                    ravedash::logger_error_condition(e)
                    stop("Unable to load reference [", reference_name, 
                      "]. Please check your reference file or simply start with a blank reference profile.")
                  })
                } else {
                  electrode_table <- subject$get_electrode_table(reference_name = reference_name)
                  str_electrode_group <- gsub("[0-9]+$", "", 
                    electrode_table$Label)
                  electrode_table$Group <- trimws(str_electrode_group)
                  if (!"LocationType" %in% names(electrode_table)) {
                    electrode_table$LocationType <- "iEEG"
                  } else {
                    electrode_table$LocationType[!electrode_table$LocationType %in% 
                      ravecore::LOCATION_TYPES] <- "iEEG"
                  }
                  if (length(subject$electrode_types) == nrow(electrode_table)) {
                    electrode_table$SignalType <- subject$electrode_types
                  } else if (!"SignalType" %in% names(electrode_table)) {
                    electrode_table$SignalType <- "LFP"
                  } else {
                    electrode_table$SignalType[!electrode_table$SignalType %in% 
                      ravecore::SIGNAL_TYPES] <- "LFP"
                  }
                  splits <- split(electrode_table, electrode_table$Group)
                  subs <- lapply(splits, function(sub) {
                    ltype <- sub$LocationType
                    refs <- rep("noref", nrow(sub))
                    ref_type <- rep("No Reference", nrow(sub))
                    seeg <- which(ltype == "sEEG")
                    if (length(seeg) >= 2) {
                      last_idx <- seeg[[length(seeg)]]
                      refs[seeg[-length(seeg)]] <- sprintf("ref_%d", 
                        sub$Electrode[seeg[-1]])
                      ref_type[seeg[-length(seeg)]] <- "Bipolar Reference"
                      refs[[last_idx]] <- "noref"
                      sub$Group[[last_idx]] <- "Bipolar-last-electrode"
                    }
                    sub$Reference <- refs
                    sub$Type <- ref_type
                    sub[, c("Electrode", "Group", "Reference", 
                      "Type")]
                  })
                  reference_table_initial <- do.call("rbind", 
                    unname(subs))
                }
                unsaved_meta <- file.path(subject$meta_path, 
                  "reference__unsaved.csv")
                utils::write.csv(reference_table_initial, unsaved_meta)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(reference_table_initial)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "reference_table_initial", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "reference_table_initial", target_expr = quote({
                {
                  if (isTRUE(reference_name %in% subject$reference_names)) {
                    tryCatch({
                      reference_table_initial <- subject$get_reference(reference_name, 
                        simplify = FALSE)
                      reference_table_initial <- reference_table_initial[, 
                        c("Electrode", "Group", "Reference", 
                          "Type")]
                    }, error = function(e) {
                      ravedash::logger("Unable to load reference [", 
                        reference_name, "]", level = "error")
                      ravedash::logger_error_condition(e)
                      stop("Unable to load reference [", reference_name, 
                        "]. Please check your reference file or simply start with a blank reference profile.")
                    })
                  } else {
                    electrode_table <- subject$get_electrode_table(reference_name = reference_name)
                    str_electrode_group <- gsub("[0-9]+$", "", 
                      electrode_table$Label)
                    electrode_table$Group <- trimws(str_electrode_group)
                    if (!"LocationType" %in% names(electrode_table)) {
                      electrode_table$LocationType <- "iEEG"
                    } else {
                      electrode_table$LocationType[!electrode_table$LocationType %in% 
                        ravecore::LOCATION_TYPES] <- "iEEG"
                    }
                    if (length(subject$electrode_types) == nrow(electrode_table)) {
                      electrode_table$SignalType <- subject$electrode_types
                    } else if (!"SignalType" %in% names(electrode_table)) {
                      electrode_table$SignalType <- "LFP"
                    } else {
                      electrode_table$SignalType[!electrode_table$SignalType %in% 
                        ravecore::SIGNAL_TYPES] <- "LFP"
                    }
                    splits <- split(electrode_table, electrode_table$Group)
                    subs <- lapply(splits, function(sub) {
                      ltype <- sub$LocationType
                      refs <- rep("noref", nrow(sub))
                      ref_type <- rep("No Reference", nrow(sub))
                      seeg <- which(ltype == "sEEG")
                      if (length(seeg) >= 2) {
                        last_idx <- seeg[[length(seeg)]]
                        refs[seeg[-length(seeg)]] <- sprintf("ref_%d", 
                          sub$Electrode[seeg[-1]])
                        ref_type[seeg[-length(seeg)]] <- "Bipolar Reference"
                        refs[[last_idx]] <- "noref"
                        sub$Group[[last_idx]] <- "Bipolar-last-electrode"
                      }
                      sub$Reference <- refs
                      sub$Type <- ref_type
                      sub[, c("Electrode", "Group", "Reference", 
                        "Type")]
                    })
                    reference_table_initial <- do.call("rbind", 
                      unname(subs))
                  }
                  unsaved_meta <- file.path(subject$meta_path, 
                    "reference__unsaved.csv")
                  utils::write.csv(reference_table_initial, unsaved_meta)
                }
                reference_table_initial
            }), target_depends = c("reference_name", "subject"
            )), deps = c("reference_name", "subject"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), load_voltage_data = targets::tar_target_raw(name = "voltage_data", 
        command = quote({
            .__target_expr__. <- quote({
                electrodes <- subject$electrodes
                is_lfp <- subject$electrode_types %in% "LFP"
                lfp_channels <- electrodes[is_lfp]
                if (!length(lfp_channels)) {
                  stop("There is no LFP/macro channel for this subject")
                }
                blocks <- subject$blocks
                use_cache <- preprocessing_history$use_cache
                has_wavelet <- all(preprocessing_history$current$has_wavelet[is_lfp])
                cache_root <- file.path(subject$cache_path, "rave2", 
                  "voltage")
                ravepipeline::dir_create2(cache_root)
                notch_params <- subject$preprocess_settings$notch_params
                first_e <- electrodes[is_lfp][[1]]
                first_inst <- ravecore::new_electrode(subject = subject, 
                  number = first_e)
                progress <- dipsaus::progress2("Check cache data", 
                  max = length(blocks), shiny_auto_close = TRUE)
                voltage_signals <- structure(names = blocks, 
                  lapply(blocks, function(block) {
                    progress$inc(sprintf("%s", block))
                    if (has_wavelet) {
                      sample_signal <- ieegio::io_read_h5(first_inst$voltage_file, 
                        name = sprintf("/raw/voltage/%s", block), 
                        ram = FALSE)
                    } else {
                      sample_signal <- ieegio::io_read_h5(first_inst$preprocess_file, 
                        name = sprintf("/notch/%s", block), ram = FALSE)
                    }
                    signal_length <- length(sample_signal)
                    block_path <- file.path(cache_root, block)
                    exists <- TRUE
                    arr <- tryCatch({
                      if (!use_cache) {
                        stop("Do not use cache")
                      }
                      filearray::filearray_checkload(filebase = block_path, 
                        mode = "readwrite", symlink_ok = FALSE, 
                        subject_id = subject$subject_id, blocks = blocks, 
                        electrodes = electrodes, electrode_is_lfp = is_lfp, 
                        notch_params = notch_params, sample_rates = subject$raw_sample_rates, 
                        signal_length = as.integer(signal_length), 
                        staged = TRUE)
                    }, error = function(e) {
                      unlink(block_path, recursive = TRUE)
                      arr <- filearray::filearray_create(filebase = block_path, 
                        dimension = c(signal_length, sum(is_lfp)), 
                        type = "double", partition_size = 1L)
                      arr$.header$subject_id <- subject$subject_id
                      arr$.header$blocks <- blocks
                      arr$.header$electrodes <- electrodes
                      arr$.header$electrode_is_lfp <- is_lfp
                      arr$.header$notch_params <- notch_params
                      arr$.header$sample_rates <- subject$raw_sample_rates
                      arr$.header$signal_length <- as.integer(signal_length)
                      dimnames(arr) <- list(NULL, Electrode = lfp_channels)
                      arr$.save_header()
                      exists <<- FALSE
                      arr
                    })
                    list(exists = exists, array = ravepipeline::RAVEFileArray$new(arr))
                  }))
                exists <- vapply(voltage_signals, "[[", FALSE, 
                  "exists")
                missing_blocks <- blocks[!exists]
                subject_id <- subject$subject_id
                if (length(missing_blocks)) {
                  ravepipeline::lapply_jobs(seq_along(lfp_channels), 
                    function(ii) {
                      e <- lfp_channels[[ii]]
                      inst <- ravecore::new_electrode(subject = subject_id, 
                        number = e)
                      if (has_wavelet) {
                        voltage_file <- inst$voltage_file
                        h5_name_format <- "/raw/voltage/%s"
                      } else {
                        voltage_file <- inst$preprocess_file
                        h5_name_format <- "/notch/%s"
                      }
                      for (block in missing_blocks) {
                        s <- ieegio::io_read_h5(voltage_file, 
                          sprintf(h5_name_format, block), ram = TRUE)
                        arr <- voltage_signals[[block]]$array$`@impl`
                        arr[, ii] <- s
                      }
                      return()
                    }, .globals = list(lfp_channels = lfp_channels, 
                      subject_id = subject_id, missing_blocks = missing_blocks, 
                      voltage_signals = voltage_signals, has_wavelet = has_wavelet), 
                    callback = function(ii) {
                      sprintf("Creating cache|Electrode %s", 
                        electrodes[[ii]])
                    })
                }
                voltage_data <- list(data = structure(lapply(voltage_signals, 
                  function(item) {
                    arr <- item$array$`@impl`
                    if (!item$exists) {
                      arr$set_header("staged", TRUE)
                    }
                    arr$.mode <- "readonly"
                    arr
                  }), names = blocks), electrodes = lfp_channels)
                if (!isTRUE(getOption("raveio.debug", FALSE))) {
                  previous_pipeline_path <- file.path(subject$pipeline_path, 
                    "reference_module")
                  ravepipeline::pipeline_fork(dest = previous_pipeline_path, 
                    activate = FALSE)
                }
                voltage_data
            })
            tryCatch({
                eval(.__target_expr__.)
                return(voltage_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "voltage_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "voltage_data", target_expr = quote({
                {
                  electrodes <- subject$electrodes
                  is_lfp <- subject$electrode_types %in% "LFP"
                  lfp_channels <- electrodes[is_lfp]
                  if (!length(lfp_channels)) {
                    stop("There is no LFP/macro channel for this subject")
                  }
                  blocks <- subject$blocks
                  use_cache <- preprocessing_history$use_cache
                  has_wavelet <- all(preprocessing_history$current$has_wavelet[is_lfp])
                  cache_root <- file.path(subject$cache_path, 
                    "rave2", "voltage")
                  ravepipeline::dir_create2(cache_root)
                  notch_params <- subject$preprocess_settings$notch_params
                  first_e <- electrodes[is_lfp][[1]]
                  first_inst <- ravecore::new_electrode(subject = subject, 
                    number = first_e)
                  progress <- dipsaus::progress2("Check cache data", 
                    max = length(blocks), shiny_auto_close = TRUE)
                  voltage_signals <- structure(names = blocks, 
                    lapply(blocks, function(block) {
                      progress$inc(sprintf("%s", block))
                      if (has_wavelet) {
                        sample_signal <- ieegio::io_read_h5(first_inst$voltage_file, 
                          name = sprintf("/raw/voltage/%s", block), 
                          ram = FALSE)
                      } else {
                        sample_signal <- ieegio::io_read_h5(first_inst$preprocess_file, 
                          name = sprintf("/notch/%s", block), 
                          ram = FALSE)
                      }
                      signal_length <- length(sample_signal)
                      block_path <- file.path(cache_root, block)
                      exists <- TRUE
                      arr <- tryCatch({
                        if (!use_cache) {
                          stop("Do not use cache")
                        }
                        filearray::filearray_checkload(filebase = block_path, 
                          mode = "readwrite", symlink_ok = FALSE, 
                          subject_id = subject$subject_id, blocks = blocks, 
                          electrodes = electrodes, electrode_is_lfp = is_lfp, 
                          notch_params = notch_params, sample_rates = subject$raw_sample_rates, 
                          signal_length = as.integer(signal_length), 
                          staged = TRUE)
                      }, error = function(e) {
                        unlink(block_path, recursive = TRUE)
                        arr <- filearray::filearray_create(filebase = block_path, 
                          dimension = c(signal_length, sum(is_lfp)), 
                          type = "double", partition_size = 1L)
                        arr$.header$subject_id <- subject$subject_id
                        arr$.header$blocks <- blocks
                        arr$.header$electrodes <- electrodes
                        arr$.header$electrode_is_lfp <- is_lfp
                        arr$.header$notch_params <- notch_params
                        arr$.header$sample_rates <- subject$raw_sample_rates
                        arr$.header$signal_length <- as.integer(signal_length)
                        dimnames(arr) <- list(NULL, Electrode = lfp_channels)
                        arr$.save_header()
                        exists <<- FALSE
                        arr
                      })
                      list(exists = exists, array = ravepipeline::RAVEFileArray$new(arr))
                    }))
                  exists <- vapply(voltage_signals, "[[", FALSE, 
                    "exists")
                  missing_blocks <- blocks[!exists]
                  subject_id <- subject$subject_id
                  if (length(missing_blocks)) {
                    ravepipeline::lapply_jobs(seq_along(lfp_channels), 
                      function(ii) {
                        e <- lfp_channels[[ii]]
                        inst <- ravecore::new_electrode(subject = subject_id, 
                          number = e)
                        if (has_wavelet) {
                          voltage_file <- inst$voltage_file
                          h5_name_format <- "/raw/voltage/%s"
                        } else {
                          voltage_file <- inst$preprocess_file
                          h5_name_format <- "/notch/%s"
                        }
                        for (block in missing_blocks) {
                          s <- ieegio::io_read_h5(voltage_file, 
                            sprintf(h5_name_format, block), ram = TRUE)
                          arr <- voltage_signals[[block]]$array$`@impl`
                          arr[, ii] <- s
                        }
                        return()
                      }, .globals = list(lfp_channels = lfp_channels, 
                        subject_id = subject_id, missing_blocks = missing_blocks, 
                        voltage_signals = voltage_signals, has_wavelet = has_wavelet), 
                      callback = function(ii) {
                        sprintf("Creating cache|Electrode %s", 
                          electrodes[[ii]])
                      })
                  }
                  voltage_data <- list(data = structure(lapply(voltage_signals, 
                    function(item) {
                      arr <- item$array$`@impl`
                      if (!item$exists) {
                        arr$set_header("staged", TRUE)
                      }
                      arr$.mode <- "readonly"
                      arr
                    }), names = blocks), electrodes = lfp_channels)
                  if (!isTRUE(getOption("raveio.debug", FALSE))) {
                    previous_pipeline_path <- file.path(subject$pipeline_path, 
                      "reference_module")
                    ravepipeline::pipeline_fork(dest = previous_pipeline_path, 
                      activate = FALSE)
                  }
                  voltage_data
                }
                voltage_data
            }), target_depends = c("subject", "preprocessing_history"
            )), deps = c("subject", "preprocessing_history"), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    validate_electrode_groups = targets::tar_target_raw(name = "reference_group", 
        command = quote({
            .__target_expr__. <- quote({
                ngroups <- length(electrode_group)
                group_names <- ""
                electrodes <- NULL
                reference_group <- reference_table_initial
                reference_group$GroupID <- 0
                lfp_channels <- subject$electrodes[subject$electrode_types %in% 
                  c("LFP")]
                not_lfp <- !reference_group$Electrode %in% lfp_channels
                if (any(not_lfp)) {
                  electrodes <- reference_group$Electrode[not_lfp]
                  reference_group$Group[not_lfp] <- ""
                  reference_group$Reference[not_lfp] <- "noref"
                  reference_group$Type[not_lfp] <- "No Reference"
                }
                id <- 1
                for (x in electrode_group) {
                  e <- dipsaus::parse_svec(x$electrodes)
                  if (length(e)) {
                    e <- unique(e)
                    if (length(x$name) != 1 || !is.character(x$name)) {
                      stop("Each electrode group must have a unique name")
                    }
                    x$name <- trimws(x$name)
                    if (x$name %in% group_names) {
                      stop("Electrode groups must have unique names (violation: ", 
                        x$name, ")")
                    }
                    group_names <- c(group_names, x$name)
                    emissing <- e[!e %in% lfp_channels]
                    if (length(emissing)) {
                      stop("Channel ", dipsaus::deparse_svec(emissing), 
                        " are not declared/imported or LFP channels (e.g. microwires, auxiliary, ...). Please remove from group [", 
                        x$name, "]")
                    }
                    edup <- e[e %in% electrodes]
                    if (length(edup)) {
                      stop("Electrodes ", dipsaus::deparse_svec(edup), 
                        " appear in multiple groups. Please fix this issue by ensuring that each of channel only appears in one group at a time.")
                    }
                    electrodes <- c(electrodes, e)
                    sel <- reference_group$Electrode %in% e
                    reference_group$Group[sel] <- x$name
                    reference_group$GroupID[sel] <- id
                  }
                  id <- id + 1
                }
                head(reference_group)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(reference_group)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "reference_group", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "reference_group", target_expr = quote({
                {
                  ngroups <- length(electrode_group)
                  group_names <- ""
                  electrodes <- NULL
                  reference_group <- reference_table_initial
                  reference_group$GroupID <- 0
                  lfp_channels <- subject$electrodes[subject$electrode_types %in% 
                    c("LFP")]
                  not_lfp <- !reference_group$Electrode %in% 
                    lfp_channels
                  if (any(not_lfp)) {
                    electrodes <- reference_group$Electrode[not_lfp]
                    reference_group$Group[not_lfp] <- ""
                    reference_group$Reference[not_lfp] <- "noref"
                    reference_group$Type[not_lfp] <- "No Reference"
                  }
                  id <- 1
                  for (x in electrode_group) {
                    e <- dipsaus::parse_svec(x$electrodes)
                    if (length(e)) {
                      e <- unique(e)
                      if (length(x$name) != 1 || !is.character(x$name)) {
                        stop("Each electrode group must have a unique name")
                      }
                      x$name <- trimws(x$name)
                      if (x$name %in% group_names) {
                        stop("Electrode groups must have unique names (violation: ", 
                          x$name, ")")
                      }
                      group_names <- c(group_names, x$name)
                      emissing <- e[!e %in% lfp_channels]
                      if (length(emissing)) {
                        stop("Channel ", dipsaus::deparse_svec(emissing), 
                          " are not declared/imported or LFP channels (e.g. microwires, auxiliary, ...). Please remove from group [", 
                          x$name, "]")
                      }
                      edup <- e[e %in% electrodes]
                      if (length(edup)) {
                        stop("Electrodes ", dipsaus::deparse_svec(edup), 
                          " appear in multiple groups. Please fix this issue by ensuring that each of channel only appears in one group at a time.")
                      }
                      electrodes <- c(electrodes, e)
                      sel <- reference_group$Electrode %in% e
                      reference_group$Group[sel] <- x$name
                      reference_group$GroupID[sel] <- id
                    }
                    id <- id + 1
                  }
                  head(reference_group)
                }
                reference_group
            }), target_depends = c("electrode_group", "reference_table_initial", 
            "subject")), deps = c("electrode_group", "reference_table_initial", 
        "subject"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), validate_and_apply_changes = targets::tar_target_raw(name = "reference_updated", 
        command = quote({
            .__target_expr__. <- quote({
                reference_choices <- c("No Reference", "Common Average Reference", 
                  "White-matter Reference", "Bipolar Reference")
                for (item in changes) {
                  evec <- dipsaus::parse_svec(item$electrodes)
                  sel <- reference_group$Electrode %in% evec
                  if (!any(sel)) {
                    next
                  }
                  if (length(item$reference_type) != 1) {
                    stop("Reference type is missing for electrode ", 
                      dipsaus::deparse_svec(evec))
                  }
                  reference_group$Type[sel] <- item$reference_type
                  if (item$reference_type == reference_choices[[1]]) {
                    reference_group$Reference[sel] <- "noref"
                  } else if (item$reference_type %in% reference_choices[c(2, 
                    3)]) {
                    if (length(item$reference_signal) != 1) {
                      stop("Reference signal must be the same within the group if the group reference type is common average or white-matter reference")
                    }
                    reference_group$Reference[sel] <- item$reference_signal
                  } else if (item$reference_type == reference_choices[[4]]) {
                    if (length(item$reference_signal) != sum(sel)) {
                      stop("For Bipolar reference, `reference_signal` must have the same size as the number of electrodes")
                    }
                    reference_group$Reference[sel] <- item$reference_signal
                  }
                }
                reference_updated <- reference_group
                unsaved_meta <- file.path(subject$meta_path, 
                  "reference__unsaved.csv")
                utils::write.csv(reference_updated, unsaved_meta)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(reference_updated)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "reference_updated", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "reference_updated", target_expr = quote({
                {
                  reference_choices <- c("No Reference", "Common Average Reference", 
                    "White-matter Reference", "Bipolar Reference")
                  for (item in changes) {
                    evec <- dipsaus::parse_svec(item$electrodes)
                    sel <- reference_group$Electrode %in% evec
                    if (!any(sel)) {
                      next
                    }
                    if (length(item$reference_type) != 1) {
                      stop("Reference type is missing for electrode ", 
                        dipsaus::deparse_svec(evec))
                    }
                    reference_group$Type[sel] <- item$reference_type
                    if (item$reference_type == reference_choices[[1]]) {
                      reference_group$Reference[sel] <- "noref"
                    } else if (item$reference_type %in% reference_choices[c(2, 
                      3)]) {
                      if (length(item$reference_signal) != 1) {
                        stop("Reference signal must be the same within the group if the group reference type is common average or white-matter reference")
                      }
                      reference_group$Reference[sel] <- item$reference_signal
                    } else if (item$reference_type == reference_choices[[4]]) {
                      if (length(item$reference_signal) != sum(sel)) {
                        stop("For Bipolar reference, `reference_signal` must have the same size as the number of electrodes")
                      }
                      reference_group$Reference[sel] <- item$reference_signal
                    }
                  }
                  reference_updated <- reference_group
                  unsaved_meta <- file.path(subject$meta_path, 
                    "reference__unsaved.csv")
                  utils::write.csv(reference_updated, unsaved_meta)
                }
                reference_updated
            }), target_depends = c("changes", "reference_group", 
            "subject")), deps = c("changes", "reference_group", 
        "subject"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
