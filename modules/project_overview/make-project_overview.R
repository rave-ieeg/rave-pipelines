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
    input_use_cache = targets::tar_target_raw("use_cache", quote({
        settings[["use_cache"]]
    }), deps = "settings"), input_template_subject = targets::tar_target_raw("template_subject", 
        quote({
            settings[["template_subject"]]
        }), deps = "settings"), input_project_names = targets::tar_target_raw("project_names", 
        quote({
            settings[["project_names"]]
        }), deps = "settings"), obtain_project_names = targets::tar_target_raw(name = "project_names_cleaned", 
        command = quote({
            .__target_expr__. <- quote({
                all_projects <- raveio::get_projects(refresh = TRUE)
                if (length(project_names)) {
                  project_names_cleaned <- sort(unique(unlist(project_names)))
                  project_names_cleaned <- project_names_cleaned[project_names_cleaned %in% 
                    all_projects]
                } else {
                  project_names_cleaned <- all_projects
                }
                if (!length(project_names_cleaned)) {
                  stop("No valid project found.")
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(project_names_cleaned)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "project_names_cleaned", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "project_names_cleaned", target_expr = quote({
                {
                  all_projects <- raveio::get_projects(refresh = TRUE)
                  if (length(project_names)) {
                    project_names_cleaned <- sort(unique(unlist(project_names)))
                    project_names_cleaned <- project_names_cleaned[project_names_cleaned %in% 
                      all_projects]
                  } else {
                    project_names_cleaned <- all_projects
                  }
                  if (!length(project_names_cleaned)) {
                    stop("No valid project found.")
                  }
                }
                project_names_cleaned
            }), target_depends = "project_names"), deps = "project_names", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    ensure_template_brain = targets::tar_target_raw(name = "template_info", 
        command = quote({
            .__target_expr__. <- quote({
                template_info <- ensure_brain_template(template_subject = template_subject)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(template_info)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "template_info", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "template_info", target_expr = quote({
                {
                  template_info <- ensure_brain_template(template_subject = template_subject)
                }
                template_info
            }), target_depends = "template_subject"), deps = "template_subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    snapshot_projects = targets::tar_target_raw(name = "snapshot_results", 
        command = quote({
            .__target_expr__. <- quote({
                force(template_info)
                force(use_cache)
                if (nzchar(Sys.getenv("RAVE_PIPELINE_ACTIVE", 
                  unset = ""))) {
                  suppressWarnings({
                    snapshot_results <- lapply(project_names_cleaned, 
                      function(project_name) {
                        snapshot_project(project_name, template = template_subject, 
                          cache_root = "build/cache", use_cache = use_cache)
                      })
                  })
                } else {
                  snapshot_results <- NULL
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_results", target_expr = quote({
                {
                  force(template_info)
                  force(use_cache)
                  if (nzchar(Sys.getenv("RAVE_PIPELINE_ACTIVE", 
                    unset = ""))) {
                    suppressWarnings({
                      snapshot_results <- lapply(project_names_cleaned, 
                        function(project_name) {
                          snapshot_project(project_name, template = template_subject, 
                            cache_root = "build/cache", use_cache = use_cache)
                        })
                    })
                  } else {
                    snapshot_results <- NULL
                  }
                }
                snapshot_results
            }), target_depends = c("template_info", "use_cache", 
            "project_names_cleaned", "template_subject")), deps = c("template_info", 
        "use_cache", "project_names_cleaned", "template_subject"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), generate_project_overview = targets::tar_target_raw(name = "project_overview", 
        command = quote({
            .__target_expr__. <- quote({
                if (nzchar(Sys.getenv("RAVE_PIPELINE_ACTIVE", 
                  unset = ""))) {
                  force(template_info)
                  project_overview <- generate_website(project_names_cleaned)
                } else {
                  project_overview <- NULL
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(project_overview)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "project_overview", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "project_overview", target_expr = quote({
                {
                  if (nzchar(Sys.getenv("RAVE_PIPELINE_ACTIVE", 
                    unset = ""))) {
                    force(template_info)
                    project_overview <- generate_website(project_names_cleaned)
                  } else {
                    project_overview <- NULL
                  }
                }
                project_overview
            }), target_depends = c("template_info", "project_names_cleaned"
            )), deps = c("template_info", "project_names_cleaned"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
