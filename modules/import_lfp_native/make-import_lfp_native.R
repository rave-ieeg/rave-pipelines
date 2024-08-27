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
targets::tar_option_set(envir = ._._env_._.)
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path",
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings",
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")),
    input_import_channels__compose_setup = targets::tar_target_raw("import_channels__compose_setup",
        quote({
            settings[["import_channels__compose_setup"]]
        }), deps = "settings"), input_import_channels__compose_setup_message_1 = targets::tar_target_raw("import_channels__compose_setup_message_1",
        quote({
            settings[["import_channels__compose_setup_message_1"]]
        }), deps = "settings"), input_import_channels__compose_setup_message_2 = targets::tar_target_raw("import_channels__compose_setup_message_2",
        quote({
            settings[["import_channels__compose_setup_message_2"]]
        }), deps = "settings"), input_skip_validation = targets::tar_target_raw("skip_validation",
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
        }), deps = "settings"), input_import_channels__electrodes = targets::tar_target_raw("import_channels__electrodes",
        quote({
            settings[["import_channels__electrodes"]]
        }), deps = "settings"), input_import_channels__electrode_file = targets::tar_target_raw("import_channels__electrode_file",
        quote({
            settings[["import_channels__electrode_file"]]
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
                asNamespace("raveio")$resolve_pipeline_error(name = "subject",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave-subject",
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
                electrodes <- dipsaus::parse_svec(import_channels__electrodes)
                if (!length(electrodes)) {
                  stop("No electrode is set.")
                }
                sample_rate <- import_channels__sample_rate
                if (!length(sample_rate) || !isTRUE(sample_rate >
                  1)) {
                  stop("Sample rate is invalid")
                }
                format <- import_blocks__format
                if (!is.numeric(format)) {
                  format <- which(startsWith(names(raveio::IMPORT_FORMATS),
                    trimws(format)))
                }
                clean_compose_setup(compose_setup = compose_setup,
                  electrodes = electrodes)
                if (isTRUE(skip_validation)) {
                  ravedash::logger("`skip_validation` is on.",
                    level = "warn")
                  validation_result <- TRUE
                } else {
                  validation_result <- raveio::validate_raw_file(subject_code = subject$subject_code,
                    blocks = blocks, electrodes = electrodes,
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
                      sQuote(subject$project_name), ".\n", msg,
                      call. = bquote(raveio::validate_raw_file(subject_code = .(subject$subject_code),
                        blocks = .(blocks), electrodes = .(electrodes),
                        format = .(format))))
                  }
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(validation_result)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "validation_result",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL,
            target_export = "validation_result", target_expr = quote({
                {
                  blocks <- import_blocks__session_block
                  miss_b <- blocks[!blocks %in% subject$preprocess_settings$all_blocks]
                  if (length(miss_b)) {
                    stop("The following block folders are missing: ",
                      paste(miss_b, collapse = ", "))
                  }
                  electrodes <- dipsaus::parse_svec(import_channels__electrodes)
                  if (!length(electrodes)) {
                    stop("No electrode is set.")
                  }
                  sample_rate <- import_channels__sample_rate
                  if (!length(sample_rate) || !isTRUE(sample_rate >
                    1)) {
                    stop("Sample rate is invalid")
                  }
                  format <- import_blocks__format
                  if (!is.numeric(format)) {
                    format <- which(startsWith(names(raveio::IMPORT_FORMATS),
                      trimws(format)))
                  }
                  clean_compose_setup(compose_setup = compose_setup,
                    electrodes = electrodes)
                  if (isTRUE(skip_validation)) {
                    ravedash::logger("`skip_validation` is on.",
                      level = "warn")
                    validation_result <- TRUE
                  } else {
                    validation_result <- raveio::validate_raw_file(subject_code = subject$subject_code,
                      blocks = blocks, electrodes = electrodes,
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
                          blocks = .(blocks), electrodes = .(electrodes),
                          format = .(format))))
                    }
                  }
                }
                validation_result
            }), target_depends = c("import_blocks__session_block",
            "subject", "import_channels__electrodes", "import_channels__sample_rate",
            "import_blocks__format", "compose_setup", "skip_validation"
            )), deps = c("import_blocks__session_block", "subject",
        "import_channels__electrodes", "import_channels__sample_rate",
        "import_blocks__format", "compose_setup", "skip_validation"
        ), cue = targets::tar_cue("thorough"), pattern = NULL,
        iteration = "list"), check_import_signals = targets::tar_target_raw(name = "preprocess_info",
        command = quote({
            .__target_expr__. <- quote({
                force(validation_result)
                subject$initialize_paths(include_freesurfer = FALSE)
                blocks <- import_blocks__session_block
                electrodes <- dipsaus::parse_svec(import_channels__electrodes)
                sample_rate <- import_channels__sample_rate
                format <- import_blocks__format
                if (!is.numeric(format)) {
                  format <- which(startsWith(names(raveio::IMPORT_FORMATS),
                    trimws(format)))
                }
                physical_unit <- import_channels__unit
                if (length(physical_unit) != 1 || is.na(physical_unit) ||
                  physical_unit == "NA") {
                  physical_unit <- NA
                } else if (!physical_unit %in% c("V", "mV", "uV")) {
                  physical_unit <- NA
                }
                if (force_import) {
                  preproc <- subject$preprocess_settings
                  preproc$data$checklevel <- 0L
                  existing <- preproc$electrodes
                  existing <- existing[!existing %in% electrodes]
                  preproc$data$electrodes <- NULL
                  preproc$data$`@remove`(as.character(electrodes))
                  if (!setequal(preproc$blocks, blocks)) {
                    lapply(existing, function(e) {
                      preproc$data[[as.character(e)]]$data_imported <- FALSE
                    })
                    preproc$set_blocks(blocks)
                  }
                  preproc$set_electrodes(electrodes, type = "LFP",
                    add = TRUE)
                  preproc$set_sample_rates(sample_rate, type = "LFP")
                  preproc$save()
                }
                raveio::rave_import(project_name = subject$project_name,
                  subject_code = subject$subject_code, blocks = blocks,
                  electrodes = electrodes, sample_rate = sample_rate,
                  format = format, conversion = physical_unit,
                  data_type = "LFP", add = FALSE, skip_validation = TRUE)
                compose_params <- clean_compose_setup(compose_setup = compose_setup,
                  electrodes = electrodes)
                if (length(compose_params)) {
                  raveio::lapply_async(compose_params, function(item) {
                    raveio::compose_channel(subject = subject$subject_id,
                      number = item$number, from = item$from,
                      weights = item$weights, normalize = item$normalize,
                      force = TRUE)
                  })
                }
                module_id <- "import_lfp_native"
                subject$set_default(namespace = module_id, key = "import_parameters",
                  value = list(project_name = subject$project_name,
                    subject_code = subject$subject_code, blocks = blocks,
                    electrodes = dipsaus::deparse_svec(electrodes),
                    composed_electrodes = compose_params, sample_rate = sample_rate,
                    format = format, format_readable = names(raveio::IMPORT_FORMATS)[[format]],
                    conversion = physical_unit, data_type = "LFP",
                    add = FALSE, timestamp = strftime(Sys.time(),
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
                asNamespace("raveio")$resolve_pipeline_error(name = "preprocess_info",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave-subject",
            target_export = "preprocess_info", target_expr = quote({
                {
                  force(validation_result)
                  subject$initialize_paths(include_freesurfer = FALSE)
                  blocks <- import_blocks__session_block
                  electrodes <- dipsaus::parse_svec(import_channels__electrodes)
                  sample_rate <- import_channels__sample_rate
                  format <- import_blocks__format
                  if (!is.numeric(format)) {
                    format <- which(startsWith(names(raveio::IMPORT_FORMATS),
                      trimws(format)))
                  }
                  physical_unit <- import_channels__unit
                  if (length(physical_unit) != 1 || is.na(physical_unit) ||
                    physical_unit == "NA") {
                    physical_unit <- NA
                  } else if (!physical_unit %in% c("V", "mV",
                    "uV")) {
                    physical_unit <- NA
                  }
                  if (force_import) {
                    preproc <- subject$preprocess_settings
                    preproc$data$checklevel <- 0L
                    existing <- preproc$electrodes
                    existing <- existing[!existing %in% electrodes]
                    preproc$data$electrodes <- NULL
                    preproc$data$`@remove`(as.character(electrodes))
                    if (!setequal(preproc$blocks, blocks)) {
                      lapply(existing, function(e) {
                        preproc$data[[as.character(e)]]$data_imported <- FALSE
                      })
                      preproc$set_blocks(blocks)
                    }
                    preproc$set_electrodes(electrodes, type = "LFP",
                      add = TRUE)
                    preproc$set_sample_rates(sample_rate, type = "LFP")
                    preproc$save()
                  }
                  raveio::rave_import(project_name = subject$project_name,
                    subject_code = subject$subject_code, blocks = blocks,
                    electrodes = electrodes, sample_rate = sample_rate,
                    format = format, conversion = physical_unit,
                    data_type = "LFP", add = FALSE, skip_validation = TRUE)
                  compose_params <- clean_compose_setup(compose_setup = compose_setup,
                    electrodes = electrodes)
                  if (length(compose_params)) {
                    raveio::lapply_async(compose_params, function(item) {
                      raveio::compose_channel(subject = subject$subject_id,
                        number = item$number, from = item$from,
                        weights = item$weights, normalize = item$normalize,
                        force = TRUE)
                    })
                  }
                  module_id <- "import_lfp_native"
                  subject$set_default(namespace = module_id,
                    key = "import_parameters", value = list(project_name = subject$project_name,
                      subject_code = subject$subject_code, blocks = blocks,
                      electrodes = dipsaus::deparse_svec(electrodes),
                      composed_electrodes = compose_params, sample_rate = sample_rate,
                      format = format, format_readable = names(raveio::IMPORT_FORMATS)[[format]],
                      conversion = physical_unit, data_type = "LFP",
                      add = FALSE, timestamp = strftime(Sys.time(),
                        usetz = TRUE)))
                  preprocess_info <- raveio::RAVEPreprocessSettings$new(subject$subject_id,
                    read_only = TRUE)
                  subject$get_electrode_table(reference_name = ".fake",
                    simplify = FALSE)
                }
                preprocess_info
            }), target_depends = c("validation_result", "subject",
            "import_blocks__session_block", "import_channels__electrodes",
            "import_channels__sample_rate", "import_blocks__format",
            "import_channels__unit", "force_import", "compose_setup"
            )), deps = c("validation_result", "subject", "import_blocks__session_block",
        "import_channels__electrodes", "import_channels__sample_rate",
        "import_blocks__format", "import_channels__unit", "force_import",
        "compose_setup"), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"))
