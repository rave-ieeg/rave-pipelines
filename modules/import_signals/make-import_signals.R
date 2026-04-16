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
        }), deps = "settings"), input_skip_validation = targets::tar_target_raw("skip_validation", 
        quote({
            settings[["skip_validation"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_microwire_sample_rate = targets::tar_target_raw("microwire_sample_rate", 
        quote({
            settings[["microwire_sample_rate"]]
        }), deps = "settings"), input_microwire_channels = targets::tar_target_raw("microwire_channels", 
        quote({
            settings[["microwire_channels"]]
        }), deps = "settings"), input_lfp_sample_rate = targets::tar_target_raw("lfp_sample_rate", 
        quote({
            settings[["lfp_sample_rate"]]
        }), deps = "settings"), input_lfp_channels = targets::tar_target_raw("lfp_channels", 
        quote({
            settings[["lfp_channels"]]
        }), deps = "settings"), input_import_format = targets::tar_target_raw("import_format", 
        quote({
            settings[["import_format"]]
        }), deps = "settings"), input_import_blocks = targets::tar_target_raw("import_blocks", 
        quote({
            settings[["import_blocks"]]
        }), deps = "settings"), input_force_import = targets::tar_target_raw("force_import", 
        quote({
            settings[["force_import"]]
        }), deps = "settings"), input_compose_setup = targets::tar_target_raw("compose_setup", 
        quote({
            settings[["compose_setup"]]
        }), deps = "settings"), input_auxiliary_sample_rate = targets::tar_target_raw("auxiliary_sample_rate", 
        quote({
            settings[["auxiliary_sample_rate"]]
        }), deps = "settings"), input_auxiliary_channels = targets::tar_target_raw("auxiliary_channels", 
        quote({
            settings[["auxiliary_channels"]]
        }), deps = "settings"), obtain_subject_instance = targets::tar_target_raw(name = "subject", 
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
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), validate_data = targets::tar_target_raw(name = "validation_result", 
        command = quote({
            .__target_expr__. <- quote({
                blocks <- import_blocks
                miss_b <- blocks[!blocks %in% subject$preprocess_settings$all_blocks]
                if (length(miss_b)) {
                  stop("The following block folders are missing: ", 
                    paste(miss_b, collapse = ", "))
                }
                lfp_channels <- dipsaus::parse_svec(lfp_channels)
                microwire_channels <- dipsaus::parse_svec(microwire_channels)
                auxiliary_channels <- dipsaus::parse_svec(auxiliary_channels)
                if (!length(c(lfp_channels, microwire_channels, 
                  auxiliary_channels))) {
                  stop("No channel is set.")
                }
                lfp_sample_rate <- lfp_sample_rate
                microwire_sample_rate <- microwire_sample_rate
                auxiliary_sample_rate <- auxiliary_sample_rate
                if (length(lfp_channels) > 0 && (!length(lfp_sample_rate) || 
                  !isTRUE(lfp_sample_rate > 1))) {
                  stop("LFP sample rate is invalid")
                }
                if (length(microwire_channels) > 0 && (!length(microwire_sample_rate) || 
                  !isTRUE(microwire_sample_rate > 1))) {
                  stop("Microwire sample rate is invalid")
                }
                if (length(auxiliary_channels) > 0 && (!length(auxiliary_sample_rate) || 
                  !isTRUE(auxiliary_sample_rate > 1))) {
                  stop("Auxiliary sample rate is invalid")
                }
                format <- import_format
                if (is.numeric(format) || grepl(" ", import_format)) {
                  format <- ravecore::IMPORT_FORMATS[[format]]
                }
                clean_compose_setup(compose_setup = compose_setup, 
                  electrodes = lfp_channels)
                if (isTRUE(skip_validation)) {
                  ravepipeline::logger("`skip_validation` is on.", 
                    level = "warn")
                  validation_result <- TRUE
                } else {
                  validate_channels <- function(channels) {
                    if (!length(channels)) {
                      return()
                    }
                    validation_result <- switch(format, native_matlab = {
                      ravecore:::validate_raw_h5_mat_per_channel(subject = subject, 
                        blocks = blocks, electrodes = channels, 
                        check_content = TRUE)
                    }, native_matlab2 = {
                      ravecore:::validate_raw_h5_mat_per_block(subject = subject, 
                        blocks = blocks, electrodes = channels, 
                        check_content = TRUE)
                    }, native_edf = {
                      ravecore:::validate_raw_edf(subject = subject, 
                        blocks = blocks, electrodes = channels, 
                        check_content = TRUE)
                    }, native_brainvis = {
                      ravecore:::validate_raw_brainvision(subject = subject, 
                        blocks = blocks, electrodes = channels, 
                        check_content = TRUE)
                    }, native_blackrock = {
                      ravecore:::validate_raw_nevnsx(subject = subject, 
                        blocks = blocks, electrodes = channels, 
                        check_content = TRUE)
                    }, {
                      stop("Unknown format ", format)
                    })
                    if (!isTRUE(validation_result$passed)) {
                      reasons <- validation_result$errors
                      if (!length(reasons)) {
                        stop("rave_import error: unknown reason.")
                      }
                      msg <- sapply(seq_along(reasons), function(ii) {
                        nm <- names(reasons)[[ii]]
                        items <- reasons[[ii]]
                        paste0(ii, " - ", nm, "\n", paste0("    ", 
                          items, collapse = "\n"))
                      })
                      stop("The following issues found when validating subject ", 
                        sQuote(subject$subject_code), " in project ", 
                        sQuote(subject$project_name), ".\n", 
                        msg)
                    }
                    validation_result
                  }
                  validation_result <- list(lfp = validate_channels(lfp_channels), 
                    microwire = validate_channels(microwire_channels), 
                    auxiliary = validate_channels(auxiliary_channels))
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(validation_result)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "validation_result", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "validation_result", target_expr = quote({
                {
                  blocks <- import_blocks
                  miss_b <- blocks[!blocks %in% subject$preprocess_settings$all_blocks]
                  if (length(miss_b)) {
                    stop("The following block folders are missing: ", 
                      paste(miss_b, collapse = ", "))
                  }
                  lfp_channels <- dipsaus::parse_svec(lfp_channels)
                  microwire_channels <- dipsaus::parse_svec(microwire_channels)
                  auxiliary_channels <- dipsaus::parse_svec(auxiliary_channels)
                  if (!length(c(lfp_channels, microwire_channels, 
                    auxiliary_channels))) {
                    stop("No channel is set.")
                  }
                  lfp_sample_rate <- lfp_sample_rate
                  microwire_sample_rate <- microwire_sample_rate
                  auxiliary_sample_rate <- auxiliary_sample_rate
                  if (length(lfp_channels) > 0 && (!length(lfp_sample_rate) || 
                    !isTRUE(lfp_sample_rate > 1))) {
                    stop("LFP sample rate is invalid")
                  }
                  if (length(microwire_channels) > 0 && (!length(microwire_sample_rate) || 
                    !isTRUE(microwire_sample_rate > 1))) {
                    stop("Microwire sample rate is invalid")
                  }
                  if (length(auxiliary_channels) > 0 && (!length(auxiliary_sample_rate) || 
                    !isTRUE(auxiliary_sample_rate > 1))) {
                    stop("Auxiliary sample rate is invalid")
                  }
                  format <- import_format
                  if (is.numeric(format) || grepl(" ", import_format)) {
                    format <- ravecore::IMPORT_FORMATS[[format]]
                  }
                  clean_compose_setup(compose_setup = compose_setup, 
                    electrodes = lfp_channels)
                  if (isTRUE(skip_validation)) {
                    ravepipeline::logger("`skip_validation` is on.", 
                      level = "warn")
                    validation_result <- TRUE
                  } else {
                    validate_channels <- function(channels) {
                      if (!length(channels)) {
                        return()
                      }
                      validation_result <- switch(format, native_matlab = {
                        ravecore:::validate_raw_h5_mat_per_channel(subject = subject, 
                          blocks = blocks, electrodes = channels, 
                          check_content = TRUE)
                      }, native_matlab2 = {
                        ravecore:::validate_raw_h5_mat_per_block(subject = subject, 
                          blocks = blocks, electrodes = channels, 
                          check_content = TRUE)
                      }, native_edf = {
                        ravecore:::validate_raw_edf(subject = subject, 
                          blocks = blocks, electrodes = channels, 
                          check_content = TRUE)
                      }, native_brainvis = {
                        ravecore:::validate_raw_brainvision(subject = subject, 
                          blocks = blocks, electrodes = channels, 
                          check_content = TRUE)
                      }, native_blackrock = {
                        ravecore:::validate_raw_nevnsx(subject = subject, 
                          blocks = blocks, electrodes = channels, 
                          check_content = TRUE)
                      }, {
                        stop("Unknown format ", format)
                      })
                      if (!isTRUE(validation_result$passed)) {
                        reasons <- validation_result$errors
                        if (!length(reasons)) {
                          stop("rave_import error: unknown reason.")
                        }
                        msg <- sapply(seq_along(reasons), function(ii) {
                          nm <- names(reasons)[[ii]]
                          items <- reasons[[ii]]
                          paste0(ii, " - ", nm, "\n", paste0("    ", 
                            items, collapse = "\n"))
                        })
                        stop("The following issues found when validating subject ", 
                          sQuote(subject$subject_code), " in project ", 
                          sQuote(subject$project_name), ".\n", 
                          msg)
                      }
                      validation_result
                    }
                    validation_result <- list(lfp = validate_channels(lfp_channels), 
                      microwire = validate_channels(microwire_channels), 
                      auxiliary = validate_channels(auxiliary_channels))
                  }
                }
                validation_result
            }), target_depends = c("import_blocks", "subject", 
            "lfp_channels", "microwire_channels", "auxiliary_channels", 
            "lfp_sample_rate", "microwire_sample_rate", "auxiliary_sample_rate", 
            "import_format", "compose_setup", "skip_validation"
            )), deps = c("import_blocks", "subject", "lfp_channels", 
        "microwire_channels", "auxiliary_channels", "lfp_sample_rate", 
        "microwire_sample_rate", "auxiliary_sample_rate", "import_format", 
        "compose_setup", "skip_validation"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), check_import_signals = targets::tar_target_raw(name = "preprocess_info", 
        command = quote({
            .__target_expr__. <- quote({
                force(validation_result)
                subject$initialize_paths(include_freesurfer = FALSE)
                blocks <- import_blocks
                format <- import_format
                if (is.numeric(format) || grepl(" ", import_format)) {
                  format <- ravecore::IMPORT_FORMATS[[format]]
                }
                lfp_channels <- dipsaus::parse_svec(lfp_channels)
                microwire_channels <- dipsaus::parse_svec(microwire_channels)
                auxiliary_channels <- dipsaus::parse_svec(auxiliary_channels)
                lfp_sample_rate <- lfp_sample_rate
                microwire_sample_rate <- microwire_sample_rate
                auxiliary_sample_rate <- auxiliary_sample_rate
                lfp_unit <- NA
                microwire_unit <- NA
                auxiliary_unit <- NA
                all_channels <- c(lfp_channels, microwire_channels, 
                  auxiliary_channels)
                if (force_import) {
                  preproc <- subject$preprocess_settings
                  preproc$data$checklevel <- 0L
                  existing <- preproc$electrodes
                  existing <- existing[!existing %in% all_channels]
                  preproc$data$electrodes <- NULL
                  preproc$data$`@remove`(as.character(all_channels))
                  if (!setequal(preproc$blocks, blocks)) {
                    lapply(existing, function(e) {
                      preproc$data[[as.character(e)]]$data_imported <- FALSE
                    })
                    preproc$set_blocks(blocks)
                  }
                  if (length(lfp_channels)) {
                    preproc$set_electrodes(lfp_channels, type = "LFP", 
                      add = TRUE)
                    preproc$set_sample_rates(lfp_sample_rate, 
                      type = "LFP")
                  }
                  if (length(microwire_channels)) {
                    preproc$set_electrodes(microwire_channels, 
                      type = "Spike", add = TRUE)
                    preproc$set_sample_rates(microwire_sample_rate, 
                      type = "Spike")
                  }
                  if (length(auxiliary_channels)) {
                    preproc$set_electrodes(auxiliary_channels, 
                      type = "Auxiliary", add = TRUE)
                    preproc$set_sample_rates(auxiliary_sample_rate, 
                      type = "Auxiliary")
                  }
                  preproc$save()
                }
                rave_import <- switch(format, native_matlab = {
                  ravecore:::import_from_h5_mat_per_channel
                }, native_matlab2 = {
                  ravecore::import_from_h5_mat_per_block
                }, native_edf = {
                  ravecore:::import_from_edf
                }, native_brainvis = {
                  ravecore:::import_from_brainvis
                }, native_blackrock = {
                  ravecore:::import_from_nevnsx
                }, {
                  stop("Unknown format ", format)
                })
                if (length(lfp_channels)) {
                  rave_import(subject = subject, blocks = blocks, 
                    electrodes = lfp_channels, sample_rate = lfp_sample_rate, 
                    data_type = "LFP", add = FALSE, skip_validation = TRUE)
                }
                if (length(microwire_channels)) {
                  rave_import(subject = subject, blocks = blocks, 
                    electrodes = microwire_channels, sample_rate = microwire_sample_rate, 
                    data_type = "Spike", add = TRUE, skip_validation = TRUE)
                }
                if (length(auxiliary_channels)) {
                  rave_import(subject = subject, blocks = blocks, 
                    electrodes = auxiliary_channels, sample_rate = auxiliary_sample_rate, 
                    data_type = "Auxiliary", add = TRUE, skip_validation = TRUE)
                }
                compose_params <- clean_compose_setup(compose_setup = compose_setup, 
                  electrodes = lfp_channels)
                if (length(compose_params)) {
                  ravepipeline::lapply_jobs(compose_params, function(item) {
                    ravecore::compose_channel(subject = subject$subject_id, 
                      number = item$number, from = item$from, 
                      weights = item$weights, normalize = item$normalize, 
                      force = TRUE)
                    return()
                  }, .globals = list(subject = subject), callback = function(item) {
                    sprintf("Composing channel|%s", item$number)
                  })
                }
                module_id <- "import_lfp_native"
                data_types <- NULL
                if (length(lfp_channels)) {
                  data_types <- "LFP"
                }
                if (length(microwire_channels)) {
                  data_types <- c(data_types, "Spike")
                }
                if (length(auxiliary_channels)) {
                  data_types <- c(data_types, "Auxiliary")
                }
                subject$set_default(namespace = module_id, key = "import_parameters", 
                  value = list(project_name = subject$project_name, 
                    subject_code = subject$subject_code, blocks = blocks, 
                    electrodes = dipsaus::deparse_svec(lfp_channels), 
                    lfp_channels = dipsaus::deparse_svec(lfp_channels), 
                    microwire_channels = dipsaus::deparse_svec(microwire_channels), 
                    auxiliary_channels = dipsaus::deparse_svec(auxiliary_channels), 
                    composed_electrodes = compose_params, sample_rate = lfp_sample_rate, 
                    lfp_sample_rate = lfp_sample_rate, microwire_sample_rate = microwire_sample_rate, 
                    auxiliary_sample_rate = auxiliary_sample_rate, 
                    format = format, format_readable = names(ravecore::IMPORT_FORMATS)[ravecore::IMPORT_FORMATS == 
                      format], conversion = lfp_unit, lfp_conversion = lfp_unit, 
                    microwire_conversion = microwire_unit, auxiliary_conversion = auxiliary_unit, 
                    data_type = data_types, add = FALSE, timestamp = strftime(Sys.time(), 
                      usetz = TRUE)))
                preprocess_info <- ravecore::RAVEPreprocessSettings$new(subject$subject_id, 
                  read_only = TRUE)
                subject$get_electrode_table(reference_name = ".fake", 
                  simplify = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(preprocess_info)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "preprocess_info", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "rave-subject", 
            target_export = "preprocess_info", target_expr = quote({
                {
                  force(validation_result)
                  subject$initialize_paths(include_freesurfer = FALSE)
                  blocks <- import_blocks
                  format <- import_format
                  if (is.numeric(format) || grepl(" ", import_format)) {
                    format <- ravecore::IMPORT_FORMATS[[format]]
                  }
                  lfp_channels <- dipsaus::parse_svec(lfp_channels)
                  microwire_channels <- dipsaus::parse_svec(microwire_channels)
                  auxiliary_channels <- dipsaus::parse_svec(auxiliary_channels)
                  lfp_sample_rate <- lfp_sample_rate
                  microwire_sample_rate <- microwire_sample_rate
                  auxiliary_sample_rate <- auxiliary_sample_rate
                  lfp_unit <- NA
                  microwire_unit <- NA
                  auxiliary_unit <- NA
                  all_channels <- c(lfp_channels, microwire_channels, 
                    auxiliary_channels)
                  if (force_import) {
                    preproc <- subject$preprocess_settings
                    preproc$data$checklevel <- 0L
                    existing <- preproc$electrodes
                    existing <- existing[!existing %in% all_channels]
                    preproc$data$electrodes <- NULL
                    preproc$data$`@remove`(as.character(all_channels))
                    if (!setequal(preproc$blocks, blocks)) {
                      lapply(existing, function(e) {
                        preproc$data[[as.character(e)]]$data_imported <- FALSE
                      })
                      preproc$set_blocks(blocks)
                    }
                    if (length(lfp_channels)) {
                      preproc$set_electrodes(lfp_channels, type = "LFP", 
                        add = TRUE)
                      preproc$set_sample_rates(lfp_sample_rate, 
                        type = "LFP")
                    }
                    if (length(microwire_channels)) {
                      preproc$set_electrodes(microwire_channels, 
                        type = "Spike", add = TRUE)
                      preproc$set_sample_rates(microwire_sample_rate, 
                        type = "Spike")
                    }
                    if (length(auxiliary_channels)) {
                      preproc$set_electrodes(auxiliary_channels, 
                        type = "Auxiliary", add = TRUE)
                      preproc$set_sample_rates(auxiliary_sample_rate, 
                        type = "Auxiliary")
                    }
                    preproc$save()
                  }
                  rave_import <- switch(format, native_matlab = {
                    ravecore:::import_from_h5_mat_per_channel
                  }, native_matlab2 = {
                    ravecore::import_from_h5_mat_per_block
                  }, native_edf = {
                    ravecore:::import_from_edf
                  }, native_brainvis = {
                    ravecore:::import_from_brainvis
                  }, native_blackrock = {
                    ravecore:::import_from_nevnsx
                  }, {
                    stop("Unknown format ", format)
                  })
                  if (length(lfp_channels)) {
                    rave_import(subject = subject, blocks = blocks, 
                      electrodes = lfp_channels, sample_rate = lfp_sample_rate, 
                      data_type = "LFP", add = FALSE, skip_validation = TRUE)
                  }
                  if (length(microwire_channels)) {
                    rave_import(subject = subject, blocks = blocks, 
                      electrodes = microwire_channels, sample_rate = microwire_sample_rate, 
                      data_type = "Spike", add = TRUE, skip_validation = TRUE)
                  }
                  if (length(auxiliary_channels)) {
                    rave_import(subject = subject, blocks = blocks, 
                      electrodes = auxiliary_channels, sample_rate = auxiliary_sample_rate, 
                      data_type = "Auxiliary", add = TRUE, skip_validation = TRUE)
                  }
                  compose_params <- clean_compose_setup(compose_setup = compose_setup, 
                    electrodes = lfp_channels)
                  if (length(compose_params)) {
                    ravepipeline::lapply_jobs(compose_params, 
                      function(item) {
                        ravecore::compose_channel(subject = subject$subject_id, 
                          number = item$number, from = item$from, 
                          weights = item$weights, normalize = item$normalize, 
                          force = TRUE)
                        return()
                      }, .globals = list(subject = subject), 
                      callback = function(item) {
                        sprintf("Composing channel|%s", item$number)
                      })
                  }
                  module_id <- "import_lfp_native"
                  data_types <- NULL
                  if (length(lfp_channels)) {
                    data_types <- "LFP"
                  }
                  if (length(microwire_channels)) {
                    data_types <- c(data_types, "Spike")
                  }
                  if (length(auxiliary_channels)) {
                    data_types <- c(data_types, "Auxiliary")
                  }
                  subject$set_default(namespace = module_id, 
                    key = "import_parameters", value = list(project_name = subject$project_name, 
                      subject_code = subject$subject_code, blocks = blocks, 
                      electrodes = dipsaus::deparse_svec(lfp_channels), 
                      lfp_channels = dipsaus::deparse_svec(lfp_channels), 
                      microwire_channels = dipsaus::deparse_svec(microwire_channels), 
                      auxiliary_channels = dipsaus::deparse_svec(auxiliary_channels), 
                      composed_electrodes = compose_params, sample_rate = lfp_sample_rate, 
                      lfp_sample_rate = lfp_sample_rate, microwire_sample_rate = microwire_sample_rate, 
                      auxiliary_sample_rate = auxiliary_sample_rate, 
                      format = format, format_readable = names(ravecore::IMPORT_FORMATS)[ravecore::IMPORT_FORMATS == 
                        format], conversion = lfp_unit, lfp_conversion = lfp_unit, 
                      microwire_conversion = microwire_unit, 
                      auxiliary_conversion = auxiliary_unit, 
                      data_type = data_types, add = FALSE, timestamp = strftime(Sys.time(), 
                        usetz = TRUE)))
                  preprocess_info <- ravecore::RAVEPreprocessSettings$new(subject$subject_id, 
                    read_only = TRUE)
                  subject$get_electrode_table(reference_name = ".fake", 
                    simplify = FALSE)
                }
                preprocess_info
            }), target_depends = c("validation_result", "subject", 
            "import_blocks", "import_format", "lfp_channels", 
            "microwire_channels", "auxiliary_channels", "lfp_sample_rate", 
            "microwire_sample_rate", "auxiliary_sample_rate", 
            "force_import", "compose_setup")), deps = c("validation_result", 
        "subject", "import_blocks", "import_format", "lfp_channels", 
        "microwire_channels", "auxiliary_channels", "lfp_sample_rate", 
        "microwire_sample_rate", "auxiliary_sample_rate", "force_import", 
        "compose_setup"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
