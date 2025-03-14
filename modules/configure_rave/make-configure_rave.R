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
        read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_variable = targets::tar_target_raw("variable", quote({
        settings[["variable"]]
    }), deps = "settings"), get_system_summary = targets::tar_target_raw(name = "system_summary", 
        command = quote({
            .__target_expr__. <- quote({
                lapply(ravemanager:::rave_depends, loadNamespace)
                system_summary <- sessionInfo()
            })
            tryCatch({
                eval(.__target_expr__.)
                return(system_summary)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "system_summary", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "system_summary", target_expr = quote({
                {
                  lapply(ravemanager:::rave_depends, loadNamespace)
                  system_summary <- sessionInfo()
                }
                system_summary
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    get_python_information = targets::tar_target_raw(name = "python_summary", 
        command = quote({
            .__target_expr__. <- quote({
                python_summary <- FALSE
                try({
                  if (dir.exists(rpymat::env_path())) {
                    summary <- rpymat::ensure_rpymat(verbose = FALSE)
                    packages <- reticulate::py_list_packages(rpymat::env_path())
                    python_summary <- list(summary = summary, 
                      packages = packages)
                  }
                }, silent = TRUE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(python_summary)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "python_summary", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "python_summary", target_expr = quote({
                {
                  python_summary <- FALSE
                  try({
                    if (dir.exists(rpymat::env_path())) {
                      summary <- rpymat::ensure_rpymat(verbose = FALSE)
                      packages <- reticulate::py_list_packages(rpymat::env_path())
                      python_summary <- list(summary = summary, 
                        packages = packages)
                    }
                  }, silent = TRUE)
                }
                python_summary
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"))
