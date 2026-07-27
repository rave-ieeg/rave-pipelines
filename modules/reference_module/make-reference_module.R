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
        }), deps = "settings"), input_carla_params = targets::tar_target_raw("carla_params", 
        quote({
            settings[["carla_params"]]
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
                    ravepipeline::logger("Unable to load reference [", 
                      reference_name, "]", level = "error")
                    ravepipeline::logger_error_condition(e)
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
                      ravepipeline::logger("Unable to load reference [", 
                        reference_name, "]", level = "error")
                      ravepipeline::logger_error_condition(e)
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
        pattern = NULL, iteration = "list"), load_repository = targets::tar_target_raw(name = "repository_block", 
        command = quote({
            .__target_expr__. <- quote({
                electrodes <- subject$electrodes
                is_lfp <- subject$electrode_types %in% "LFP"
                lfp_channels <- electrodes[is_lfp]
                if (!length(lfp_channels)) {
                  stop("There is no LFP/macro channel for this subject")
                }
                blocks <- subject$blocks
                repository_block <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = subject, 
                  electrodes = lfp_channels, blocks = blocks)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository_block)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "repository_block", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "repository_block", target_expr = quote({
                {
                  electrodes <- subject$electrodes
                  is_lfp <- subject$electrode_types %in% "LFP"
                  lfp_channels <- electrodes[is_lfp]
                  if (!length(lfp_channels)) {
                    stop("There is no LFP/macro channel for this subject")
                  }
                  blocks <- subject$blocks
                  repository_block <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = subject, 
                    electrodes = lfp_channels, blocks = blocks)
                }
                repository_block
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    load_voltage_data = targets::tar_target_raw(name = "voltage_data", 
        command = quote({
            .__target_expr__. <- quote({
                electrodes <- subject$electrodes
                is_lfp <- subject$electrode_types %in% "LFP"
                lfp_channels <- electrodes[is_lfp]
                if (!length(lfp_channels)) {
                  stop("There is no LFP/macro channel for this subject")
                }
                blocks <- subject$blocks
                container <- repository_block$get_container()
                voltage_path <- file.path(pipeline$extdata_path, 
                  "voltage_data")
                if (file.exists(voltage_path)) {
                  unlink(voltage_path, recursive = TRUE)
                }
                ravepipeline::dir_create2(voltage_path)
                voltage_arrays <- lapply(blocks, function(block) {
                  farray <- container[[block]]$LFP$data
                  dir_copy2(path = farray$.filebase, new_path = file.path(voltage_path, 
                    block), hidden_files = FALSE, overwrite = TRUE)
                  farray <- filearray::filearray_load(filebase = file.path(voltage_path, 
                    block), mode = "readonly")
                  ravepipeline::RAVEFileArray$new(farray)
                })
                names(voltage_arrays) <- blocks
                voltage_data <- list(data = voltage_arrays, electrodes = lfp_channels, 
                  repository_signature = repository_block$signature)
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
                  container <- repository_block$get_container()
                  voltage_path <- file.path(pipeline$extdata_path, 
                    "voltage_data")
                  if (file.exists(voltage_path)) {
                    unlink(voltage_path, recursive = TRUE)
                  }
                  ravepipeline::dir_create2(voltage_path)
                  voltage_arrays <- lapply(blocks, function(block) {
                    farray <- container[[block]]$LFP$data
                    dir_copy2(path = farray$.filebase, new_path = file.path(voltage_path, 
                      block), hidden_files = FALSE, overwrite = TRUE)
                    farray <- filearray::filearray_load(filebase = file.path(voltage_path, 
                      block), mode = "readonly")
                    ravepipeline::RAVEFileArray$new(farray)
                  })
                  names(voltage_arrays) <- blocks
                  voltage_data <- list(data = voltage_arrays, 
                    electrodes = lfp_channels, repository_signature = repository_block$signature)
                  if (!isTRUE(getOption("raveio.debug", FALSE))) {
                    previous_pipeline_path <- file.path(subject$pipeline_path, 
                      "reference_module")
                    ravepipeline::pipeline_fork(dest = previous_pipeline_path, 
                      activate = FALSE)
                  }
                  voltage_data
                }
                voltage_data
            }), target_depends = c("subject", "repository_block"
            )), deps = c("subject", "repository_block"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), repository_for_carla = targets::tar_target_raw(name = "repository_epoch", 
        command = quote({
            .__target_expr__. <- quote({
                repository_epoch <- ravecore::prepare_subject_raw_voltage_with_epochs(subject = subject, 
                  electrodes = carla_params$electrodes, epoch_name = carla_params$epoch_name, 
                  time_windows = ravecore::validate_time_window(carla_params$time_window), 
                  quiet = TRUE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository_epoch)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "repository_epoch", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "repository_epoch", target_expr = quote({
                {
                  repository_epoch <- ravecore::prepare_subject_raw_voltage_with_epochs(subject = subject, 
                    electrodes = carla_params$electrodes, epoch_name = carla_params$epoch_name, 
                    time_windows = ravecore::validate_time_window(carla_params$time_window), 
                    quiet = TRUE)
                }
                repository_epoch
            }), target_depends = c("subject", "carla_params")), 
        deps = c("subject", "carla_params"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), estimate_carla = targets::tar_target_raw(name = "carla_fit", 
        command = quote({
            .__target_expr__. <- quote({
                container <- repository_epoch$get_container()
                filebase <- file.path(pipeline$extdata_path, 
                  "carla_fit")
                d <- dim(container$data_list[[1]])
                d[[3]] <- length(repository_epoch$electrode_list)
                if (file.exists(filebase)) {
                  unlink(filebase, recursive = TRUE)
                }
                ravepipeline::dir_create2(pipeline$extdata_path)
                combined_array <- filearray::filearray_create(filebase = filebase, 
                  dimension = d, type = "float", partition_size = 1L, 
                  initialize = FALSE)
                combined_array_wrapper <- ravepipeline::RAVEFileArray$new(combined_array, 
                  temporary = TRUE)
                lapply(seq_along(container$data_list), function(ii) {
                  combined_array[, , ii] <- container$data_list[[ii]][]
                  NULL
                })
                fit <- ravetools::carla(x = combined_array, nboot = carla_params$n_bootstrap %||% 
                  100, sensitive = isTRUE(carla_params$sensitive), 
                  min_size = carla_params$min_size, virtual_reference = isTRUE(carla_params$virtual_reference), 
                  absolute_rank = isTRUE(carla_params$absolute_rank))
                fit$zmin_mean <- NULL
                fit$virtual_channel <- repository_epoch$electrode_list[fit$virtual_channel]
                fit$bad_channels <- repository_epoch$electrode_list[fit$bad_channels]
                fit$channels <- repository_epoch$electrode_list[fit$channels]
                if (isTRUE(getOption("raveio.debug", FALSE))) {
                  mean_voltage <- ravecore::collapse2(combined_array, 
                    keep = c(1, 3), method = "mean")
                  ravetools::plot_signals(t(mean_voltage), sample_rate = repository_epoch$sample_rates$LFP, 
                    col = ifelse(repository_epoch$electrode_list %in% 
                      fit$channels, "red", "gray"), space = 0.995, 
                    space_mode = "quantile", channel_names = repository_epoch$electrode_list, 
                    main = "CARLA: selected (red) vs rejected (gray)")
                }
                if (file.exists(filebase)) {
                  unlink(filebase, recursive = TRUE)
                }
                carla_fit <- fit
            })
            tryCatch({
                eval(.__target_expr__.)
                return(carla_fit)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "carla_fit", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "carla_fit", target_expr = quote({
                {
                  container <- repository_epoch$get_container()
                  filebase <- file.path(pipeline$extdata_path, 
                    "carla_fit")
                  d <- dim(container$data_list[[1]])
                  d[[3]] <- length(repository_epoch$electrode_list)
                  if (file.exists(filebase)) {
                    unlink(filebase, recursive = TRUE)
                  }
                  ravepipeline::dir_create2(pipeline$extdata_path)
                  combined_array <- filearray::filearray_create(filebase = filebase, 
                    dimension = d, type = "float", partition_size = 1L, 
                    initialize = FALSE)
                  combined_array_wrapper <- ravepipeline::RAVEFileArray$new(combined_array, 
                    temporary = TRUE)
                  lapply(seq_along(container$data_list), function(ii) {
                    combined_array[, , ii] <- container$data_list[[ii]][]
                    NULL
                  })
                  fit <- ravetools::carla(x = combined_array, 
                    nboot = carla_params$n_bootstrap %||% 100, 
                    sensitive = isTRUE(carla_params$sensitive), 
                    min_size = carla_params$min_size, virtual_reference = isTRUE(carla_params$virtual_reference), 
                    absolute_rank = isTRUE(carla_params$absolute_rank))
                  fit$zmin_mean <- NULL
                  fit$virtual_channel <- repository_epoch$electrode_list[fit$virtual_channel]
                  fit$bad_channels <- repository_epoch$electrode_list[fit$bad_channels]
                  fit$channels <- repository_epoch$electrode_list[fit$channels]
                  if (isTRUE(getOption("raveio.debug", FALSE))) {
                    mean_voltage <- ravecore::collapse2(combined_array, 
                      keep = c(1, 3), method = "mean")
                    ravetools::plot_signals(t(mean_voltage), 
                      sample_rate = repository_epoch$sample_rates$LFP, 
                      col = ifelse(repository_epoch$electrode_list %in% 
                        fit$channels, "red", "gray"), space = 0.995, 
                      space_mode = "quantile", channel_names = repository_epoch$electrode_list, 
                      main = "CARLA: selected (red) vs rejected (gray)")
                  }
                  if (file.exists(filebase)) {
                    unlink(filebase, recursive = TRUE)
                  }
                  carla_fit <- fit
                }
                carla_fit
            }), target_depends = c("repository_epoch", "carla_params"
            )), deps = c("repository_epoch", "carla_params"), 
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
