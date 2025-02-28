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
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_random_generator = targets::tar_target_raw("random_generator", 
        quote({
            settings[["random_generator"]]
        }), deps = "settings"), input_sample_size = targets::tar_target_raw("sample_size", 
        quote({
            settings[["sample_size"]]
        }), deps = "settings"), generate_input_data = targets::tar_target_raw(name = "input_data", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("import numpy as np", "", "# sample size as integer", 
                "sample_s = int(sample_size)", "", "if sample_s <= 0:", 
                "  raise Exception(f\"Sample size should be positive\")", 
                "", "# get random number generator", "if random_generator == \"randn\":", 
                "  generator = np.random.randn", "elif random_generator == \"randint\":", 
                "  generator = np.random.randint", "elif random_generator == \"rand\":", 
                "  generator = np.random.rand", "else:", "  raise Exception(f\"Unknown random number generator { generator_name }\")", 
                "", "input_data = {", "  'x' : generator(sample_s),", 
                "  'y' : generator(sample_s)", "}")
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "input_data", paste(e$message, collapse = "\n"), 
                  "input_data", paste(code, collapse = "\n")))
            }
            re <- tryCatch(expr = {
                .env <- environment()
                if (length(c("sample_size", "random_generator"
                ))) {
                  args <- structure(names = c("sample_size", 
                  "random_generator"), lapply(c("sample_size", 
                  "random_generator"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["input_data"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler, error = function(e) {
                  traceback(e)
                  stop(e$message, call. = FALSE)
                })
            return(re)
        }), deps = c("sample_size", "random_generator"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "input_data")), calculate_correlation = targets::tar_target_raw(name = "correlation", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("import numpy as np", "correlation = np.corrcoef(input_data['x'], input_data['y'])", 
                "print(correlation)")
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "correlation", paste(e$message, collapse = "\n"), 
                  "correlation", paste(code, collapse = "\n")))
            }
            re <- tryCatch(expr = {
                .env <- environment()
                if (length("input_data")) {
                  args <- structure(names = "input_data", lapply("input_data", 
                    get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["correlation"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler, error = function(e) {
                  traceback(e)
                  stop(e$message, call. = FALSE)
                })
            return(re)
        }), deps = "input_data", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "correlation")))
