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
    input_atlas_name = targets::tar_target_raw("atlas_name", 
        quote({
            settings[["atlas_name"]]
        }), deps = "settings"), input_value_type = targets::tar_target_raw("value_type", 
        quote({
            settings[["value_type"]]
        }), deps = "settings"), `__extern_path_value_table` = targets::tar_target_raw("settings_path._value_table_", 
        "./data/value_table.csv", format = "file"), input_value_table = targets::tar_target_raw("value_table", 
        quote({
            asNamespace("ravepipeline")$pipeline_load_extdata(name = "value_table", 
                format = "csv", error_if_missing = FALSE, default_if_missing = structure(list(), 
                  class = "key_missing"), pipe_dir = ".")
        }), deps = "settings_path._value_table_"), input_value_name = targets::tar_target_raw("value_name", 
        quote({
            settings[["value_name"]]
        }), deps = "settings"), input_template_name = targets::tar_target_raw("template_name", 
        quote({
            settings[["template_name"]]
        }), deps = "settings"), input_subject_codes = targets::tar_target_raw("subject_codes", 
        quote({
            settings[["subject_codes"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_mapping_threshold = targets::tar_target_raw("mapping_threshold", 
        quote({
            settings[["mapping_threshold"]]
        }), deps = "settings"), input_mapping_params = targets::tar_target_raw("mapping_params", 
        quote({
            settings[["mapping_params"]]
        }), deps = "settings"), input_mapping_method = targets::tar_target_raw("mapping_method", 
        quote({
            settings[["mapping_method"]]
        }), deps = "settings"), ensure_template_files = targets::tar_target_raw(name = "template_information", 
        command = quote({
            .__target_expr__. <- quote({
                template_root <- threeBrain::default_template_directory()
                if (length(template_name) != 1) {
                  stop("Input `template_name` must be a string with length one")
                }
                template_path <- file.path(template_root, template_name)
                if (!dir.exists(template_path)) {
                  available_templates <- names(threeBrain::available_templates())
                  if (isTRUE(template_name %in% available_templates)) {
                    opt <- options(timeout = 600)
                    on.exit(options(opt))
                    message("No template is found... Downloading the template. This might take a while")
                    threeBrain::download_template_subject(subject_code = template_name)
                  }
                }
                template_information <- list(name = template_name, 
                  path = normalizePath(template_path, winslash = "/"))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(template_information)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "template_information", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "template_information", target_expr = quote({
                {
                  template_root <- threeBrain::default_template_directory()
                  if (length(template_name) != 1) {
                    stop("Input `template_name` must be a string with length one")
                  }
                  template_path <- file.path(template_root, template_name)
                  if (!dir.exists(template_path)) {
                    available_templates <- names(threeBrain::available_templates())
                    if (isTRUE(template_name %in% available_templates)) {
                      opt <- options(timeout = 600)
                      on.exit(options(opt))
                      message("No template is found... Downloading the template. This might take a while")
                      threeBrain::download_template_subject(subject_code = template_name)
                    }
                  }
                  template_information <- list(name = template_name, 
                    path = normalizePath(template_path, winslash = "/"))
                }
                template_information
            }), target_depends = "template_name"), deps = "template_name", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    find_meta_from_value_file = targets::tar_target_raw(name = "value_table_meta", 
        command = quote({
            .__target_expr__. <- quote({
                if (!is.data.frame(value_table)) {
                  stop("Unable to parse the value table. Please provide a comma-separated value (csv) file with column 'Subject' in it (case-sensitive).")
                }
                value_table_meta <- extract_meta_value_table(value_table, 
                  project_name)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(value_table_meta)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "value_table_meta", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "value_table_meta", target_expr = quote({
                {
                  if (!is.data.frame(value_table)) {
                    stop("Unable to parse the value table. Please provide a comma-separated value (csv) file with column 'Subject' in it (case-sensitive).")
                  }
                  value_table_meta <- extract_meta_value_table(value_table, 
                    project_name)
                }
                value_table_meta
            }), target_depends = c("value_table", "project_name"
            )), deps = c("value_table", "project_name"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), validate_inputs = targets::tar_target_raw(name = "cleaned_inputs", 
        command = quote({
            .__target_expr__. <- quote({
                project <- raveio::as_rave_project(project_name)
                all_subjects <- project$subjects()
                from_subjects <- value_table_meta$subjects
                from_subjects <- from_subjects[from_subjects %in% 
                  all_subjects]
                has_surfaces <- vapply(from_subjects, function(subject) {
                  subject <- raveio::RAVESubject$new(project_name = project$name, 
                    subject_code = subject, strict = FALSE)
                  brain <- raveio::rave_brain(subject)
                  if (is.null(brain)) {
                    return(FALSE)
                  }
                  if (!length(brain$surface_types)) {
                    return(FALSE)
                  }
                  TRUE
                }, FUN.VALUE = FALSE, USE.NAMES = FALSE)
                cleaned_inputs <- list(project_name = project_name, 
                  template = template_information, subject_codes = from_subjects, 
                  has_surfaces = has_surfaces)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(cleaned_inputs)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "cleaned_inputs", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "cleaned_inputs", target_expr = quote({
                {
                  project <- raveio::as_rave_project(project_name)
                  all_subjects <- project$subjects()
                  from_subjects <- value_table_meta$subjects
                  from_subjects <- from_subjects[from_subjects %in% 
                    all_subjects]
                  has_surfaces <- vapply(from_subjects, function(subject) {
                    subject <- raveio::RAVESubject$new(project_name = project$name, 
                      subject_code = subject, strict = FALSE)
                    brain <- raveio::rave_brain(subject)
                    if (is.null(brain)) {
                      return(FALSE)
                    }
                    if (!length(brain$surface_types)) {
                      return(FALSE)
                    }
                    TRUE
                  }, FUN.VALUE = FALSE, USE.NAMES = FALSE)
                  cleaned_inputs <- list(project_name = project_name, 
                    template = template_information, subject_codes = from_subjects, 
                    has_surfaces = has_surfaces)
                }
                cleaned_inputs
            }), target_depends = c("project_name", "value_table_meta", 
            "template_information")), deps = c("project_name", 
        "value_table_meta", "template_information"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), map_to_template = targets::tar_target_raw(name = "mapped_results", 
        command = quote({
            .__target_expr__. <- quote({
                suppressWarnings({
                  raveio::with_future_parallel({
                    flip_hemisphere <- mapping_params$flip_hemisphere
                    mapped_results <- raveio::lapply_async(subject_codes, 
                      function(subject_code) {
                        res <- map_to_template(subject = raveio::RAVESubject$new(project_name = cleaned_inputs$project_name, 
                          subject_code = subject_code), method = mapping_method, 
                          template = cleaned_inputs$template$name, 
                          volumetric_transform = "affine", interpolator = mapping_params$interpolator, 
                          save_to = NULL, flip_hemisphere = isTRUE(subject_code %in% 
                            flip_hemisphere), n_segments = c(mapping_params$width, 
                            mapping_params$height))
                        res$mapped_table$Subject <- subject_code
                        res$mapped_table
                      }, callback = function(subject_code) {
                        sprintf("Mapping subject|%s", subject_code)
                      })
                  })
                  mapped_results <- data.table::rbindlist(mapped_results)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(mapped_results)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "mapped_results", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "mapped_results", target_expr = quote({
                {
                  suppressWarnings({
                    raveio::with_future_parallel({
                      flip_hemisphere <- mapping_params$flip_hemisphere
                      mapped_results <- raveio::lapply_async(subject_codes, 
                        function(subject_code) {
                          res <- map_to_template(subject = raveio::RAVESubject$new(project_name = cleaned_inputs$project_name, 
                            subject_code = subject_code), method = mapping_method, 
                            template = cleaned_inputs$template$name, 
                            volumetric_transform = "affine", 
                            interpolator = mapping_params$interpolator, 
                            save_to = NULL, flip_hemisphere = isTRUE(subject_code %in% 
                              flip_hemisphere), n_segments = c(mapping_params$width, 
                              mapping_params$height))
                          res$mapped_table$Subject <- subject_code
                          res$mapped_table
                        }, callback = function(subject_code) {
                          sprintf("Mapping subject|%s", subject_code)
                        })
                    })
                    mapped_results <- data.table::rbindlist(mapped_results)
                  })
                }
                mapped_results
            }), target_depends = c("mapping_params", "subject_codes", 
            "cleaned_inputs", "mapping_method")), deps = c("mapping_params", 
        "subject_codes", "cleaned_inputs", "mapping_method"), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    generate_atlas = targets::tar_target_raw(name = "mapped_atlas", 
        command = quote({
            .__target_expr__. <- quote({
                if (!isTRUE(value_name %in% names(value_table))) {
                  stop("Invalid value name [", paste(value_name, 
                    collapse = ""), "] for generating atlases.")
                }
                mapped_atlas <- generate_atlas(mapped_results = mapped_results, 
                  value_table = value_table, template_name = cleaned_inputs$template$name, 
                  value_name = value_name, value_type = value_type, 
                  mapping_threshold = mapping_threshold)
                mapped_atlas
            })
            tryCatch({
                eval(.__target_expr__.)
                return(mapped_atlas)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "mapped_atlas", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "mapped_atlas", target_expr = quote({
                {
                  if (!isTRUE(value_name %in% names(value_table))) {
                    stop("Invalid value name [", paste(value_name, 
                      collapse = ""), "] for generating atlases.")
                  }
                  mapped_atlas <- generate_atlas(mapped_results = mapped_results, 
                    value_table = value_table, template_name = cleaned_inputs$template$name, 
                    value_name = value_name, value_type = value_type, 
                    mapping_threshold = mapping_threshold)
                  mapped_atlas
                }
                mapped_atlas
            }), target_depends = c("value_name", "value_table", 
            "mapped_results", "cleaned_inputs", "value_type", 
            "mapping_threshold")), deps = c("value_name", "value_table", 
        "mapped_results", "cleaned_inputs", "value_type", "mapping_threshold"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), save_group_atlas = targets::tar_target_raw(name = "save_atlas", 
        command = quote({
            .__target_expr__. <- quote({
                if (length(atlas_name) != 1 || is.na(atlas_name) || 
                  !nzchar(atlas_name)) {
                  atlas_name <- value_name
                }
                atlas_name <- gsub("[^0-9A-Za-z_+-]", "_", atlas_name)
                label_path <- file.path(threeBrain::default_template_directory(), 
                  mapped_atlas$template_name, "label")
                file_ext <- mapped_atlas$file_ext
                atlas_left <- mapped_atlas$atlas_left
                atlas_right <- mapped_atlas$atlas_right
                if (inherits(atlas_left, "ieegio_surface_contains_measurements")) {
                  file_type <- "measurements"
                } else {
                  file_type <- "annotations"
                }
                file_right <- file.path(label_path, sprintf("rh.%s.%s", 
                  atlas_name, file_ext))
                file_left <- file.path(label_path, sprintf("lh.%s.%s", 
                  atlas_name, file_ext))
                ieegio::write_surface(x = atlas_right, format = "freesurfer", 
                  type = file_type, con = file_right)
                ieegio::write_surface(x = atlas_left, format = "freesurfer", 
                  type = file_type, con = file_left)
                save_atlas <- list(path_left = normalizePath(file_left, 
                  winslash = "/"), path_right = normalizePath(file_right, 
                  winslash = "/"))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(save_atlas)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "save_atlas", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "save_atlas", target_expr = quote({
                {
                  if (length(atlas_name) != 1 || is.na(atlas_name) || 
                    !nzchar(atlas_name)) {
                    atlas_name <- value_name
                  }
                  atlas_name <- gsub("[^0-9A-Za-z_+-]", "_", 
                    atlas_name)
                  label_path <- file.path(threeBrain::default_template_directory(), 
                    mapped_atlas$template_name, "label")
                  file_ext <- mapped_atlas$file_ext
                  atlas_left <- mapped_atlas$atlas_left
                  atlas_right <- mapped_atlas$atlas_right
                  if (inherits(atlas_left, "ieegio_surface_contains_measurements")) {
                    file_type <- "measurements"
                  } else {
                    file_type <- "annotations"
                  }
                  file_right <- file.path(label_path, sprintf("rh.%s.%s", 
                    atlas_name, file_ext))
                  file_left <- file.path(label_path, sprintf("lh.%s.%s", 
                    atlas_name, file_ext))
                  ieegio::write_surface(x = atlas_right, format = "freesurfer", 
                    type = file_type, con = file_right)
                  ieegio::write_surface(x = atlas_left, format = "freesurfer", 
                    type = file_type, con = file_left)
                  save_atlas <- list(path_left = normalizePath(file_left, 
                    winslash = "/"), path_right = normalizePath(file_right, 
                    winslash = "/"))
                }
                save_atlas
            }), target_depends = c("atlas_name", "value_name", 
            "mapped_atlas")), deps = c("atlas_name", "value_name", 
        "mapped_atlas"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
