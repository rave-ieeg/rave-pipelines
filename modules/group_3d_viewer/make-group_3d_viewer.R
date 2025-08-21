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
    }), deps = "settings"), input_template_name = targets::tar_target_raw("template_name", 
        quote({
            settings[["template_name"]]
        }), deps = "settings"), input_subject_codes = targets::tar_target_raw("subject_codes", 
        quote({
            settings[["subject_codes"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_mapping_method = targets::tar_target_raw("mapping_method", 
        quote({
            settings[["mapping_method"]]
        }), deps = "settings"), input_electrode_radius = targets::tar_target_raw("electrode_radius", 
        quote({
            settings[["electrode_radius"]]
        }), deps = "settings"), ensure_template_brain = targets::tar_target_raw(name = "template_info", 
        command = quote({
            .__target_expr__. <- quote({
                template_root <- threeBrain::default_template_directory()
                template_path <- file.path(template_root, template_name)
                if (!dir.exists(template_path)) {
                  oldopt <- options(timeout = 3600)
                  on.exit({
                    options(oldopt)
                  })
                  threeBrain::download_template_subject(subject_code = template_name)
                }
                template_path <- normalizePath(template_path, 
                  winslash = "/", mustWork = TRUE)
                template_brain <- threeBrain::threeBrain(path = template_path, 
                  subject_code = template_name)
                volumes <- sort(list.files(file.path(template_path, 
                  "mri"), pattern = "\\.(mgz|nii|nii.gz)$", recursive = FALSE, 
                  include.dirs = FALSE, full.names = FALSE, all.files = FALSE, 
                  ignore.case = TRUE))
                volumes <- gsub("\\.(mgz|nii|nii.gz)$", "", volumes, 
                  ignore.case = TRUE)
                surfaces <- sort(template_brain$available_surfaces)
                annotations <- list.files(file.path(template_path, 
                  "label"), pattern = "\\.(annot|curv|sulc)$", 
                  recursive = FALSE, include.dirs = FALSE, full.names = FALSE, 
                  all.files = FALSE, ignore.case = TRUE)
                annotations <- gsub("\\.(annot|curv|sulc)$", 
                  "", annotations, ignore.case = TRUE)
                annotations <- gsub("^[lr]h\\.", "", annotations, 
                  ignore.case = TRUE)
                annotations <- sprintf("label/%s", sort(unique(annotations)))
                template_info <- list(name = template_name, path = template_path, 
                  volumes = volumes, surfaces = surfaces, annotations = annotations)
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
                  template_root <- threeBrain::default_template_directory()
                  template_path <- file.path(template_root, template_name)
                  if (!dir.exists(template_path)) {
                    oldopt <- options(timeout = 3600)
                    on.exit({
                      options(oldopt)
                    })
                    threeBrain::download_template_subject(subject_code = template_name)
                  }
                  template_path <- normalizePath(template_path, 
                    winslash = "/", mustWork = TRUE)
                  template_brain <- threeBrain::threeBrain(path = template_path, 
                    subject_code = template_name)
                  volumes <- sort(list.files(file.path(template_path, 
                    "mri"), pattern = "\\.(mgz|nii|nii.gz)$", 
                    recursive = FALSE, include.dirs = FALSE, 
                    full.names = FALSE, all.files = FALSE, ignore.case = TRUE))
                  volumes <- gsub("\\.(mgz|nii|nii.gz)$", "", 
                    volumes, ignore.case = TRUE)
                  surfaces <- sort(template_brain$available_surfaces)
                  annotations <- list.files(file.path(template_path, 
                    "label"), pattern = "\\.(annot|curv|sulc)$", 
                    recursive = FALSE, include.dirs = FALSE, 
                    full.names = FALSE, all.files = FALSE, ignore.case = TRUE)
                  annotations <- gsub("\\.(annot|curv|sulc)$", 
                    "", annotations, ignore.case = TRUE)
                  annotations <- gsub("^[lr]h\\.", "", annotations, 
                    ignore.case = TRUE)
                  annotations <- sprintf("label/%s", sort(unique(annotations)))
                  template_info <- list(name = template_name, 
                    path = template_path, volumes = volumes, 
                    surfaces = surfaces, annotations = annotations)
                }
                template_info
            }), target_depends = "template_name"), deps = "template_name", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    filter_subject_codes = targets::tar_target_raw(name = "subject_codes_filtered", 
        command = quote({
            .__target_expr__. <- quote({
                if (length(project_name) != 1) {
                  stop("Invalid RAVE project name. The `project_name` input must have length of one")
                }
                project <- raveio::as_rave_project(project_name)
                all_subject_codes <- project$subjects()
                if (!length(all_subject_codes)) {
                  warning(sprintf("No subject found under the project `%s`", 
                    project_name))
                }
                subject_codes <- unlist(subject_codes)
                subject_codes <- subject_codes[subject_codes %in% 
                  all_subject_codes]
                subject_codes_filtered <- sort(unique(subject_codes))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject_codes_filtered)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "subject_codes_filtered", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "subject_codes_filtered", target_expr = quote({
                {
                  if (length(project_name) != 1) {
                    stop("Invalid RAVE project name. The `project_name` input must have length of one")
                  }
                  project <- raveio::as_rave_project(project_name)
                  all_subject_codes <- project$subjects()
                  if (!length(all_subject_codes)) {
                    warning(sprintf("No subject found under the project `%s`", 
                      project_name))
                  }
                  subject_codes <- unlist(subject_codes)
                  subject_codes <- subject_codes[subject_codes %in% 
                    all_subject_codes]
                  subject_codes_filtered <- sort(unique(subject_codes))
                }
                subject_codes_filtered
            }), target_depends = c("project_name", "subject_codes"
            )), deps = c("project_name", "subject_codes"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), checking_mapping_capabilities = targets::tar_target_raw(name = "mapping_capabilities", 
        command = quote({
            .__target_expr__. <- quote({
                mapping_capabilities <- data.table::rbindlist(lapply(subject_codes_filtered, 
                  function(subject_code) {
                    c(list(Subject = subject_code), mapping_capability(project_name, 
                      subject_code))
                  }))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(mapping_capabilities)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "mapping_capabilities", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "mapping_capabilities", target_expr = quote({
                {
                  mapping_capabilities <- data.table::rbindlist(lapply(subject_codes_filtered, 
                    function(subject_code) {
                      c(list(Subject = subject_code), mapping_capability(project_name, 
                        subject_code))
                    }))
                }
                mapping_capabilities
            }), target_depends = c("subject_codes_filtered", 
            "project_name")), deps = c("subject_codes_filtered", 
        "project_name"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), get_mapped_brain_list = targets::tar_target_raw(name = "brain_list", 
        command = quote({
            .__target_expr__. <- quote({
                force(mapping_capabilities)
                brain_list <- lapply(subject_codes_filtered, 
                  function(subject_code) {
                    capability <- mapping_capabilities[mapping_capabilities$Subject == 
                      subject_code, ]
                    brain <- tryCatch({
                      brain <- get_mapped_brain(project_name = project_name, 
                        subject_code = subject_code, mapping_method = mapping_method, 
                        capability = capability, use_cache = use_cache)
                      raw_table <- brain$electrodes$raw_table
                      if (is.data.frame(raw_table) && nrow(raw_table) > 
                        0 && "LabelPrefix" %in% names(raw_table)) {
                        brain$set_electrode_values(data.frame(Electrode = raw_table$Electrode, 
                          LabelPrefix = raw_table$LabelPrefix))
                      }
                      brain
                    }, error = function(e) {
                      NULL
                    })
                    brain
                  })
                brain_list <- brain_list[!vapply(brain_list, 
                  is.null, FALSE)]
            })
            tryCatch({
                eval(.__target_expr__.)
                return(brain_list)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "brain_list", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "user-defined-r", 
            target_export = "brain_list", target_expr = quote({
                {
                  force(mapping_capabilities)
                  brain_list <- lapply(subject_codes_filtered, 
                    function(subject_code) {
                      capability <- mapping_capabilities[mapping_capabilities$Subject == 
                        subject_code, ]
                      brain <- tryCatch({
                        brain <- get_mapped_brain(project_name = project_name, 
                          subject_code = subject_code, mapping_method = mapping_method, 
                          capability = capability, use_cache = use_cache)
                        raw_table <- brain$electrodes$raw_table
                        if (is.data.frame(raw_table) && nrow(raw_table) > 
                          0 && "LabelPrefix" %in% names(raw_table)) {
                          brain$set_electrode_values(data.frame(Electrode = raw_table$Electrode, 
                            LabelPrefix = raw_table$LabelPrefix))
                        }
                        brain
                      }, error = function(e) {
                        NULL
                      })
                      brain
                    })
                  brain_list <- brain_list[!vapply(brain_list, 
                    is.null, FALSE)]
                }
                brain_list
            }), target_depends = c("mapping_capabilities", "subject_codes_filtered", 
            "project_name", "mapping_method", "use_cache")), 
        deps = c("mapping_capabilities", "subject_codes_filtered", 
        "project_name", "mapping_method", "use_cache"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), generate_template_brain = targets::tar_target_raw(name = "template", 
        command = quote({
            .__target_expr__. <- quote({
                radius <- electrode_radius
                if (length(radius) != 1 || !is.numeric(radius) || 
                  !isTRUE(radius > 0) || !is.finite(radius)) {
                  radius <- 0
                }
                if (radius > 0) {
                  brain_list2 <- lapply(brain_list, function(brain) {
                    raw_table <- brain$electrodes$raw_table
                    if (is.data.frame(raw_table) && nrow(raw_table) > 
                      0) {
                      raw_table$Radius <- radius
                      brain$set_electrodes(raw_table, priority = "sphere")
                    }
                    brain
                  })
                } else {
                  brain_list2 <- brain_list
                }
                template <- threeBrain::merge_brain(.list = brain_list2, 
                  template_subject = template_info$name)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(template)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "template", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "user-defined-r", 
            target_export = "template", target_expr = quote({
                {
                  radius <- electrode_radius
                  if (length(radius) != 1 || !is.numeric(radius) || 
                    !isTRUE(radius > 0) || !is.finite(radius)) {
                    radius <- 0
                  }
                  if (radius > 0) {
                    brain_list2 <- lapply(brain_list, function(brain) {
                      raw_table <- brain$electrodes$raw_table
                      if (is.data.frame(raw_table) && nrow(raw_table) > 
                        0) {
                        raw_table$Radius <- radius
                        brain$set_electrodes(raw_table, priority = "sphere")
                      }
                      brain
                    })
                  } else {
                    brain_list2 <- brain_list
                  }
                  template <- threeBrain::merge_brain(.list = brain_list2, 
                    template_subject = template_info$name)
                }
                template
            }), target_depends = c("electrode_radius", "brain_list", 
            "template_info")), deps = c("electrode_radius", "brain_list", 
        "template_info"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
