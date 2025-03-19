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
    input_kernel_table = targets::tar_target_raw("kernel_table",
        quote({
            settings[["kernel_table"]]
        }), deps = "settings"), input_pre_downsample = targets::tar_target_raw("pre_downsample",
        quote({
            settings[["pre_downsample"]]
        }), deps = "settings"), input_precision = targets::tar_target_raw("precision",
        quote({
            settings[["precision"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name",
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code",
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_target_sample_rate = targets::tar_target_raw("target_sample_rate",
        quote({
            settings[["target_sample_rate"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject",
        command = quote({
            .__target_expr__. <- quote({
                stopifnot(grepl("^[a-zA-Z0-9_]{1,}$", project_name))
                stopifnot(grepl("^[a-zA-Z0-9_]{1,}$", subject_code))
                subject <- raveio::RAVESubject$new(project_name = project_name,
                  subject_code = subject_code)
                print(subject)
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
                  stopifnot(grepl("^[a-zA-Z0-9_]{1,}$", project_name))
                  stopifnot(grepl("^[a-zA-Z0-9_]{1,}$", subject_code))
                  subject <- raveio::RAVESubject$new(project_name = project_name,
                    subject_code = subject_code)
                  print(subject)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), clear_cache = targets::tar_target_raw(name = "clear_cache",
        command = quote({
            .__target_expr__. <- quote({
                clear_cache <- raveio::clear_cached_files(subject_code = subject$subject_code)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(clear_cache)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "clear_cache",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL,
            target_export = "clear_cache", target_expr = quote({
                {
                  clear_cache <- raveio::clear_cached_files(subject_code = subject$subject_code)
                }
                clear_cache
            }), target_depends = "subject"), deps = "subject",
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"),
    get_notch_filter_timestamp = targets::tar_target_raw(name = "notch_filtere_stamp",
        command = quote({
            .__target_expr__. <- quote({
                notch_filtere_stamp <- subject$get_default("parameters",
                  namespace = "notch_filter", default_if_missing = Sys.time())
            })
            tryCatch({
                eval(.__target_expr__.)
                return(notch_filtere_stamp)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "notch_filtere_stamp",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL,
            target_export = "notch_filtere_stamp", target_expr = quote({
                {
                  notch_filtere_stamp <- subject$get_default("parameters",
                    namespace = "notch_filter", default_if_missing = Sys.time())
                }
                notch_filtere_stamp
            }), target_depends = "subject"), deps = "subject",
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"),
    check_prerequisite = targets::tar_target_raw(name = "notch_filtered_electrodes",
        command = quote({
            .__target_expr__. <- quote({
                electrodes <- subject$electrodes
                etypes <- subject$electrode_types
                notch_filtered_electrodes <- electrodes[subject$preprocess_settings$notch_filtered &
                  etypes %in% c("LFP", "EKG", "Audio")]
                if (!length(notch_filtered_electrodes)) {
                  stop("There is no electrode available to the wavelet module")
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(notch_filtered_electrodes)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "notch_filtered_electrodes",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL,
            target_export = "notch_filtered_electrodes", target_expr = quote({
                {
                  electrodes <- subject$electrodes
                  etypes <- subject$electrode_types
                  notch_filtered_electrodes <- electrodes[subject$preprocess_settings$notch_filtered &
                    etypes %in% c("LFP", "EKG", "Audio")]
                  if (!length(notch_filtered_electrodes)) {
                    stop("There is no electrode available to the wavelet module")
                  }
                }
                notch_filtered_electrodes
            }), target_depends = "subject"), deps = "subject",
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"),
    get_raw_sample_rates = targets::tar_target_raw(name = "sample_rates",
        command = quote({
            .__target_expr__. <- quote({
                sample_rates <- subject$preprocess_settings$sample_rates
                electrodes <- subject$electrodes
                sample_rates <- sapply(notch_filtered_electrodes,
                  function(e) {
                    re <- sample_rates[electrodes == e]
                    if (!length(re)) {
                      stop("Electrode ", e, " does not have sample rate. The data might not be imported correctly and some configurations are missing.")
                    }
                    re[[1]]
                  })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(sample_rates)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "sample_rates",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL,
            target_export = "sample_rates", target_expr = quote({
                {
                  sample_rates <- subject$preprocess_settings$sample_rates
                  electrodes <- subject$electrodes
                  sample_rates <- sapply(notch_filtered_electrodes,
                    function(e) {
                      re <- sample_rates[electrodes == e]
                      if (!length(re)) {
                        stop("Electrode ", e, " does not have sample rate. The data might not be imported correctly and some configurations are missing.")
                      }
                      re[[1]]
                    })
                }
                sample_rates
            }), target_depends = c("subject", "notch_filtered_electrodes"
            )), deps = c("subject", "notch_filtered_electrodes"
        ), cue = targets::tar_cue("always"), pattern = NULL,
        iteration = "list"), check_kernel = targets::tar_target_raw(name = "kernels",
        command = quote({
            .__target_expr__. <- quote({
                freqs <- unlist(kernel_table$Frequency)
                cycles <- unlist(kernel_table$Cycles)
                if (any(is.na(freqs) | freqs <= 0)) {
                  stop("The wavelet kernel table contains negative `Frequency` value(s)")
                }
                if (length(cycles) != 2) {
                  if (length(freqs) != length(cycles)) {
                    stop("The wavelet kernel table lengths are inconsistent: the lenth of `Cycles` must be either 2 (a range on which wavelet cycles that will be interpolated log-linearly), or the same length as `Frequency`.")
                  }
                }
                if (any(is.na(cycles) | cycles <= 1)) {
                  stop("The wavelet kernel table contains negative or 0 `Cycles` value(s): wavelet cycles must be greater equal than 1 (also integer numbers are strongly recommended)")
                }
                if (length(freqs) != 2 && length(cycles) == 2) {
                  if (cycles[[1]] > cycles[[2]]) {
                    stop("The wavelet kernel table: when `Cycles` is a range, (length of 2), it must be in non-decreasing order.")
                  }
                }
                if (length(precision) != 1 || !precision %in%
                  c("float", "double")) {
                  precision <- "float"
                }
                kernels <- list(freqs = freqs, cycles = cycles,
                  precision = precision)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(kernels)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "kernels",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL,
            target_export = "kernels", target_expr = quote({
                {
                  freqs <- unlist(kernel_table$Frequency)
                  cycles <- unlist(kernel_table$Cycles)
                  if (any(is.na(freqs) | freqs <= 0)) {
                    stop("The wavelet kernel table contains negative `Frequency` value(s)")
                  }
                  if (length(cycles) != 2) {
                    if (length(freqs) != length(cycles)) {
                      stop("The wavelet kernel table lengths are inconsistent: the lenth of `Cycles` must be either 2 (a range on which wavelet cycles that will be interpolated log-linearly), or the same length as `Frequency`.")
                    }
                  }
                  if (any(is.na(cycles) | cycles <= 1)) {
                    stop("The wavelet kernel table contains negative or 0 `Cycles` value(s): wavelet cycles must be greater equal than 1 (also integer numbers are strongly recommended)")
                  }
                  if (length(freqs) != 2 && length(cycles) ==
                    2) {
                    if (cycles[[1]] > cycles[[2]]) {
                      stop("The wavelet kernel table: when `Cycles` is a range, (length of 2), it must be in non-decreasing order.")
                    }
                  }
                  if (length(precision) != 1 || !precision %in%
                    c("float", "double")) {
                    precision <- "float"
                  }
                  kernels <- list(freqs = freqs, cycles = cycles,
                    precision = precision)
                }
                kernels
            }), target_depends = c("kernel_table", "precision"
            )), deps = c("kernel_table", "precision"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"), run_wavelet = targets::tar_target_raw(name = "wavelet_params",
        command = quote({
            .__target_expr__. <- quote({
                wavelet_params <- raveio:::run_wavelet(subject = subject,
                  electrodes = notch_filtered_electrodes, freqs = kernels$freqs,
                  cycles = kernels$cycles, target_sample_rate = target_sample_rate,
                  kernels_precision = kernels$precision, pre_downsample = pre_downsample,
                  verbose = TRUE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(wavelet_params)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "wavelet_params",
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL,
            target_export = "wavelet_params", target_expr = quote({
                {
                  wavelet_params <- raveio:::run_wavelet(subject = subject,
                    electrodes = notch_filtered_electrodes, freqs = kernels$freqs,
                    cycles = kernels$cycles, target_sample_rate = target_sample_rate,
                    kernels_precision = kernels$precision, pre_downsample = pre_downsample,
                    verbose = TRUE)
                }
                wavelet_params
            }), target_depends = c("subject", "notch_filtered_electrodes",
            "kernels", "target_sample_rate", "pre_downsample"
            )), deps = c("subject", "notch_filtered_electrodes",
        "kernels", "target_sample_rate", "pre_downsample"), cue = targets::tar_cue("always"),
        pattern = NULL, iteration = "list"))
