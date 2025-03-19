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
    input_skip_validation = targets::tar_target_raw("skip_validation", 
        quote({
            settings[["skip_validation"]]
        }), deps = "settings"), input_import_setup__subject_code = targets::tar_target_raw("import_setup__subject_code", 
        quote({
            settings[["import_setup__subject_code"]]
        }), deps = "settings"), input_import_setup__project_name = targets::tar_target_raw("import_setup__project_name", 
        quote({
            settings[["import_setup__project_name"]]
        }), deps = "settings"), input_import_channels__unit = targets::tar_target_raw("import_channels__unit", 
        quote({
            settings[["import_channels__unit"]]
        }), deps = "settings"), input_import_channels__sample_rate = targets::tar_target_raw("import_channels__sample_rate", 
        quote({
            settings[["import_channels__sample_rate"]]
        }), deps = "settings"), input_import_channels__microwire_unit = targets::tar_target_raw("import_channels__microwire_unit", 
        quote({
            settings[["import_channels__microwire_unit"]]
        }), deps = "settings"), input_import_channels__microwire_sample_rate = targets::tar_target_raw("import_channels__microwire_sample_rate", 
        quote({
            settings[["import_channels__microwire_sample_rate"]]
        }), deps = "settings"), input_import_channels__microwire_channels = targets::tar_target_raw("import_channels__microwire_channels", 
        quote({
            settings[["import_channels__microwire_channels"]]
        }), deps = "settings"), input_import_channels__lfp_unit = targets::tar_target_raw("import_channels__lfp_unit", 
        quote({
            settings[["import_channels__lfp_unit"]]
        }), deps = "settings"), input_import_channels__lfp_sample_rate = targets::tar_target_raw("import_channels__lfp_sample_rate", 
        quote({
            settings[["import_channels__lfp_sample_rate"]]
        }), deps = "settings"), input_import_channels__lfp_channels = targets::tar_target_raw("import_channels__lfp_channels", 
        quote({
            settings[["import_channels__lfp_channels"]]
        }), deps = "settings"), input_import_channels__electrodes = targets::tar_target_raw("import_channels__electrodes", 
        quote({
            settings[["import_channels__electrodes"]]
        }), deps = "settings"), input_import_channels__electrode_file = targets::tar_target_raw("import_channels__electrode_file", 
        quote({
            settings[["import_channels__electrode_file"]]
        }), deps = "settings"), input_import_channels__compose_upload_file = targets::tar_target_raw("import_channels__compose_upload_file", 
        quote({
            settings[["import_channels__compose_upload_file"]]
        }), deps = "settings"), input_import_channels__compose_upload_example = targets::tar_target_raw("import_channels__compose_upload_example", 
        quote({
            settings[["import_channels__compose_upload_example"]]
        }), deps = "settings"), input_import_channels__compose_upload = targets::tar_target_raw("import_channels__compose_upload", 
        quote({
            settings[["import_channels__compose_upload"]]
        }), deps = "settings"), input_import_channels__compose_setup_message_1 = targets::tar_target_raw("import_channels__compose_setup_message_1", 
        quote({
            settings[["import_channels__compose_setup_message_1"]]
        }), deps = "settings"), input_import_channels__compose_setup = targets::tar_target_raw("import_channels__compose_setup", 
        quote({
            settings[["import_channels__compose_setup"]]
        }), deps = "settings"), input_import_channels__auxiliary_unit = targets::tar_target_raw("import_channels__auxiliary_unit", 
        quote({
            settings[["import_channels__auxiliary_unit"]]
        }), deps = "settings"), input_import_channels__auxiliary_sample_rate = targets::tar_target_raw("import_channels__auxiliary_sample_rate", 
        quote({
            settings[["import_channels__auxiliary_sample_rate"]]
        }), deps = "settings"), input_import_channels__auxiliary_channels = targets::tar_target_raw("import_channels__auxiliary_channels", 
        quote({
            settings[["import_channels__auxiliary_channels"]]
        }), deps = "settings"), input_import_channels__actions_compose = targets::tar_target_raw("import_channels__actions_compose", 
        quote({
            settings[["import_channels__actions_compose"]]
        }), deps = "settings"), input_import_channels__action_compose_do = targets::tar_target_raw("import_channels__action_compose_do", 
        quote({
            settings[["import_channels__action_compose_do"]]
        }), deps = "settings"), input_import_blocks__session_block = targets::tar_target_raw("import_blocks__session_block", 
        quote({
            settings[["import_blocks__session_block"]]
        }), deps = "settings"), input_import_blocks__format = targets::tar_target_raw("import_blocks__format", 
        quote({
            settings[["import_blocks__format"]]
        }), deps = "settings"), input_force_import = targets::tar_target_raw("force_import", 
        quote({
            settings[["force_import"]]
        }), deps = "settings"), input_compose_setup = targets::tar_target_raw("compose_setup", 
        quote({
            settings[["compose_setup"]]
        }), deps = "settings"), obtain_subject_instance = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::RAVESubject$new(project_name = import_setup__project_name, 
                  subject_code = import_setup__subject_code, 
                  strict = FALSE)
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
                  subject <- raveio::RAVESubject$new(project_name = import_setup__project_name, 
                    subject_code = import_setup__subject_code, 
                    strict = FALSE)
                }
                subject
            }), target_depends = c("import_setup__project_name", 
            "import_setup__subject_code")), deps = c("import_setup__project_name", 
        "import_setup__subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), validate_data = targets::tar_target_raw(name = "validation_result", 
        command = quote({
            .__target_expr__. <- quote({
                blocks <- import_blocks__session_block
                miss_b <- blocks[!blocks %in% subject$preprocess_settings$all_blocks]
                if (length(miss_b)) {
                  stop("The following block folders are missing: ", 
                    paste(miss_b, collapse = ", "))
                }
                lfp_channels <- dipsaus::parse_svec(import_channels__lfp_channels)
                microwire_channels <- dipsaus::parse_svec(import_channels__microwire_channels)
                auxiliary_channels <- dipsaus::parse_svec(import_channels__auxiliary_channels)
                if (!length(c(lfp_channels, microwire_channels, 
                  auxiliary_channels))) {
                  stop("No channel is set.")
                }
                lfp_sample_rate <- import_channels__lfp_sample_rate
                microwire_sample_rate <- import_channels__microwire_sample_rate
                auxiliary_sample_rate <- import_channels__auxiliary_sample_rate
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
                format <- import_blocks__format
                if (!is.numeric(format)) {
                  format <- which(startsWith(names(raveio::IMPORT_FORMATS), 
                    trimws(format)))
                }
                clean_compose_setup(compose_setup = compose_setup, 
                  electrodes = lfp_channels)
                if (isTRUE(skip_validation)) {
                  ravedash::logger("`skip_validation` is on.", 
                    level = "warn")
                  validation_result <- TRUE
                } else {
                  validate_channels <- function(channels) {
                    if (!length(channels)) {
                      return()
                    }
                    validation_result <- raveio::validate_raw_file(subject_code = subject$subject_code, 
                      blocks = blocks, electrodes = channels, 
                      format = format)
                    if (!validation_result) {
                      reasons <- attr(validation_result, "reason")
                      if (!is.list(reasons) || !length(reasons)) {
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
                        msg, call. = bquote(raveio::validate_raw_file(subject_code = .(subject$subject_code), 
                          blocks = .(blocks), electrodes = .(channels), 
                          format = .(format))))
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
                  blocks <- import_blocks__session_block
                  miss_b <- blocks[!blocks %in% subject$preprocess_settings$all_blocks]
                  if (length(miss_b)) {
                    stop("The following block folders are missing: ", 
                      paste(miss_b, collapse = ", "))
                  }
                  lfp_channels <- dipsaus::parse_svec(import_channels__lfp_channels)
                  microwire_channels <- dipsaus::parse_svec(import_channels__microwire_channels)
                  auxiliary_channels <- dipsaus::parse_svec(import_channels__auxiliary_channels)
                  if (!length(c(lfp_channels, microwire_channels, 
                    auxiliary_channels))) {
                    stop("No channel is set.")
                  }
                  lfp_sample_rate <- import_channels__lfp_sample_rate
                  microwire_sample_rate <- import_channels__microwire_sample_rate
                  auxiliary_sample_rate <- import_channels__auxiliary_sample_rate
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
                  format <- import_blocks__format
                  if (!is.numeric(format)) {
                    format <- which(startsWith(names(raveio::IMPORT_FORMATS), 
                      trimws(format)))
                  }
                  clean_compose_setup(compose_setup = compose_setup, 
                    electrodes = lfp_channels)
                  if (isTRUE(skip_validation)) {
                    ravedash::logger("`skip_validation` is on.", 
                      level = "warn")
                    validation_result <- TRUE
                  } else {
                    validate_channels <- function(channels) {
                      if (!length(channels)) {
                        return()
                      }
                      validation_result <- raveio::validate_raw_file(subject_code = subject$subject_code, 
                        blocks = blocks, electrodes = channels, 
                        format = format)
                      if (!validation_result) {
                        reasons <- attr(validation_result, "reason")
                        if (!is.list(reasons) || !length(reasons)) {
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
                          msg, call. = bquote(raveio::validate_raw_file(subject_code = .(subject$subject_code), 
                            blocks = .(blocks), electrodes = .(channels), 
                            format = .(format))))
                      }
                      validation_result
                    }
                    validation_result <- list(lfp = validate_channels(lfp_channels), 
                      microwire = validate_channels(microwire_channels), 
                      auxiliary = validate_channels(auxiliary_channels))
                  }
                }
                validation_result
            }), target_depends = c("import_blocks__session_block", 
            "subject", "import_channels__lfp_channels", "import_channels__microwire_channels", 
            "import_channels__auxiliary_channels", "import_channels__lfp_sample_rate", 
            "import_channels__microwire_sample_rate", "import_channels__auxiliary_sample_rate", 
            "import_blocks__format", "compose_setup", "skip_validation"
            )), deps = c("import_blocks__session_block", "subject", 
        "import_channels__lfp_channels", "import_channels__microwire_channels", 
        "import_channels__auxiliary_channels", "import_channels__lfp_sample_rate", 
        "import_channels__microwire_sample_rate", "import_channels__auxiliary_sample_rate", 
        "import_blocks__format", "compose_setup", "skip_validation"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), check_import_signals = targets::tar_target_raw(name = "preprocess_info", 
        command = quote({
            .__target_expr__. <- quote({
                force(validation_result)
                subject$initialize_paths(include_freesurfer = FALSE)
                blocks <- import_blocks__session_block
                format <- import_blocks__format
                if (!is.numeric(format)) {
                  format <- which(startsWith(names(raveio::IMPORT_FORMATS), 
                    trimws(format)))
                }
                lfp_channels <- dipsaus::parse_svec(import_channels__lfp_channels)
                microwire_channels <- dipsaus::parse_svec(import_channels__microwire_channels)
                auxiliary_channels <- dipsaus::parse_svec(import_channels__auxiliary_channels)
                lfp_sample_rate <- import_channels__lfp_sample_rate
                microwire_sample_rate <- import_channels__microwire_sample_rate
                auxiliary_sample_rate <- import_channels__auxiliary_sample_rate
                lfp_unit <- import_channels__lfp_unit
                microwire_unit <- import_channels__microwire_unit
                auxiliary_unit <- import_channels__auxiliary_unit
                if (length(lfp_unit) != 1 || is.na(lfp_unit) || 
                  lfp_unit == "NA" || !isTRUE(lfp_unit %in% c("V", 
                  "mV", "uV"))) {
                  lfp_unit <- NA
                }
                if (length(microwire_unit) != 1 || is.na(microwire_unit) || 
                  microwire_unit == "NA" || !isTRUE(microwire_unit %in% 
                  c("V", "mV", "uV"))) {
                  microwire_unit <- NA
                }
                if (length(auxiliary_unit) != 1 || is.na(auxiliary_unit) || 
                  auxiliary_unit == "NA" || !isTRUE(auxiliary_unit %in% 
                  c("V", "mV", "uV"))) {
                  auxiliary_unit <- NA
                }
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
                if (length(lfp_channels)) {
                  raveio::rave_import(project_name = subject$project_name, 
                    subject_code = subject$subject_code, blocks = blocks, 
                    electrodes = lfp_channels, sample_rate = lfp_sample_rate, 
                    format = format, conversion = lfp_unit, data_type = "LFP", 
                    add = FALSE, skip_validation = TRUE)
                }
                if (length(microwire_channels)) {
                  raveio::rave_import(project_name = subject$project_name, 
                    subject_code = subject$subject_code, blocks = blocks, 
                    electrodes = microwire_channels, sample_rate = microwire_sample_rate, 
                    format = format, conversion = microwire_unit, 
                    data_type = "Spike", add = TRUE, skip_validation = TRUE)
                }
                if (length(auxiliary_channels)) {
                  raveio::rave_import(project_name = subject$project_name, 
                    subject_code = subject$subject_code, blocks = blocks, 
                    electrodes = auxiliary_channels, sample_rate = auxiliary_sample_rate, 
                    format = format, conversion = auxiliary_unit, 
                    data_type = "Auxiliary", add = TRUE, skip_validation = TRUE)
                }
                compose_params <- clean_compose_setup(compose_setup = compose_setup, 
                  electrodes = lfp_channels)
                if (length(compose_params)) {
                  raveio::lapply_async(compose_params, function(item) {
                    raveio::compose_channel(subject = subject$subject_id, 
                      number = item$number, from = item$from, 
                      weights = item$weights, normalize = item$normalize, 
                      force = TRUE)
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
                if (length(lfp_channels)) {
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
                    format = format, format_readable = names(raveio::IMPORT_FORMATS)[[format]], 
                    conversion = lfp_unit, lfp_conversion = lfp_unit, 
                    microwire_conversion = microwire_unit, auxiliary_conversion = auxiliary_unit, 
                    data_type = data_types, add = FALSE, timestamp = strftime(Sys.time(), 
                      usetz = TRUE)))
                preprocess_info <- raveio::RAVEPreprocessSettings$new(subject$subject_id, 
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
                  blocks <- import_blocks__session_block
                  format <- import_blocks__format
                  if (!is.numeric(format)) {
                    format <- which(startsWith(names(raveio::IMPORT_FORMATS), 
                      trimws(format)))
                  }
                  lfp_channels <- dipsaus::parse_svec(import_channels__lfp_channels)
                  microwire_channels <- dipsaus::parse_svec(import_channels__microwire_channels)
                  auxiliary_channels <- dipsaus::parse_svec(import_channels__auxiliary_channels)
                  lfp_sample_rate <- import_channels__lfp_sample_rate
                  microwire_sample_rate <- import_channels__microwire_sample_rate
                  auxiliary_sample_rate <- import_channels__auxiliary_sample_rate
                  lfp_unit <- import_channels__lfp_unit
                  microwire_unit <- import_channels__microwire_unit
                  auxiliary_unit <- import_channels__auxiliary_unit
                  if (length(lfp_unit) != 1 || is.na(lfp_unit) || 
                    lfp_unit == "NA" || !isTRUE(lfp_unit %in% 
                    c("V", "mV", "uV"))) {
                    lfp_unit <- NA
                  }
                  if (length(microwire_unit) != 1 || is.na(microwire_unit) || 
                    microwire_unit == "NA" || !isTRUE(microwire_unit %in% 
                    c("V", "mV", "uV"))) {
                    microwire_unit <- NA
                  }
                  if (length(auxiliary_unit) != 1 || is.na(auxiliary_unit) || 
                    auxiliary_unit == "NA" || !isTRUE(auxiliary_unit %in% 
                    c("V", "mV", "uV"))) {
                    auxiliary_unit <- NA
                  }
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
                  if (length(lfp_channels)) {
                    raveio::rave_import(project_name = subject$project_name, 
                      subject_code = subject$subject_code, blocks = blocks, 
                      electrodes = lfp_channels, sample_rate = lfp_sample_rate, 
                      format = format, conversion = lfp_unit, 
                      data_type = "LFP", add = FALSE, skip_validation = TRUE)
                  }
                  if (length(microwire_channels)) {
                    raveio::rave_import(project_name = subject$project_name, 
                      subject_code = subject$subject_code, blocks = blocks, 
                      electrodes = microwire_channels, sample_rate = microwire_sample_rate, 
                      format = format, conversion = microwire_unit, 
                      data_type = "Spike", add = TRUE, skip_validation = TRUE)
                  }
                  if (length(auxiliary_channels)) {
                    raveio::rave_import(project_name = subject$project_name, 
                      subject_code = subject$subject_code, blocks = blocks, 
                      electrodes = auxiliary_channels, sample_rate = auxiliary_sample_rate, 
                      format = format, conversion = auxiliary_unit, 
                      data_type = "Auxiliary", add = TRUE, skip_validation = TRUE)
                  }
                  compose_params <- clean_compose_setup(compose_setup = compose_setup, 
                    electrodes = lfp_channels)
                  if (length(compose_params)) {
                    raveio::lapply_async(compose_params, function(item) {
                      raveio::compose_channel(subject = subject$subject_id, 
                        number = item$number, from = item$from, 
                        weights = item$weights, normalize = item$normalize, 
                        force = TRUE)
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
                  if (length(lfp_channels)) {
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
                      format = format, format_readable = names(raveio::IMPORT_FORMATS)[[format]], 
                      conversion = lfp_unit, lfp_conversion = lfp_unit, 
                      microwire_conversion = microwire_unit, 
                      auxiliary_conversion = auxiliary_unit, 
                      data_type = data_types, add = FALSE, timestamp = strftime(Sys.time(), 
                        usetz = TRUE)))
                  preprocess_info <- raveio::RAVEPreprocessSettings$new(subject$subject_id, 
                    read_only = TRUE)
                  subject$get_electrode_table(reference_name = ".fake", 
                    simplify = FALSE)
                }
                preprocess_info
            }), target_depends = c("validation_result", "subject", 
            "import_blocks__session_block", "import_blocks__format", 
            "import_channels__lfp_channels", "import_channels__microwire_channels", 
            "import_channels__auxiliary_channels", "import_channels__lfp_sample_rate", 
            "import_channels__microwire_sample_rate", "import_channels__auxiliary_sample_rate", 
            "import_channels__lfp_unit", "import_channels__microwire_unit", 
            "import_channels__auxiliary_unit", "force_import", 
            "compose_setup")), deps = c("validation_result", 
        "subject", "import_blocks__session_block", "import_blocks__format", 
        "import_channels__lfp_channels", "import_channels__microwire_channels", 
        "import_channels__auxiliary_channels", "import_channels__lfp_sample_rate", 
        "import_channels__microwire_sample_rate", "import_channels__auxiliary_sample_rate", 
        "import_channels__lfp_unit", "import_channels__microwire_unit", 
        "import_channels__auxiliary_unit", "force_import", "compose_setup"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
