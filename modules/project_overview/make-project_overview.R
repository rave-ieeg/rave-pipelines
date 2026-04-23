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
    input_template_subject = targets::tar_target_raw("template_subject", 
        quote({
            settings[["template_subject"]]
        }), deps = "settings"), input_subject_codes = targets::tar_target_raw("subject_codes", 
        quote({
            settings[["subject_codes"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_module_filter = targets::tar_target_raw("module_filter", 
        quote({
            settings[["module_filter"]]
        }), deps = "settings"), validate_project = targets::tar_target_raw(name = "project", 
        command = quote({
            .__target_expr__. <- quote({
                all_projects <- ravecore::get_projects(refresh = TRUE)
                if (!length(project_name) || !nzchar(project_name) || 
                  !project_name %in% all_projects) {
                  stop("Invalid project_name: ", sQuote(project_name), 
                    ". Available: ", paste(all_projects, collapse = ", "))
                }
                project <- ravecore::as_rave_project(project_name, 
                  strict = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(project)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "project", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "project", target_expr = quote({
                {
                  all_projects <- ravecore::get_projects(refresh = TRUE)
                  if (!length(project_name) || !nzchar(project_name) || 
                    !project_name %in% all_projects) {
                    stop("Invalid project_name: ", sQuote(project_name), 
                      ". Available: ", paste(all_projects, collapse = ", "))
                  }
                  project <- ravecore::as_rave_project(project_name, 
                    strict = FALSE)
                }
                project
            }), target_depends = "project_name"), deps = "project_name", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    collect_subject_info = targets::tar_target_raw(name = "subject_summary", 
        command = quote({
            .__target_expr__. <- quote({
                available_subjects <- project$subjects()
                if (!length(available_subjects)) {
                  stop("Project ", sQuote(project$name), " has no subjects.")
                }
                subject_summary <- do.call("rbind", lapply(available_subjects, 
                  function(sc) {
                    subject <- ravecore::RAVESubject$new(project_name = project$name, 
                      subject_code = sc, strict = FALSE)
                    preproc <- subject$preprocess_settings
                    data_imported <- all(preproc$data_imported)
                    notch_filtered <- all(preproc$notch_filtered)
                    has_wavelet <- all(preproc$has_wavelet)
                    etable <- tryCatch(subject$meta_data("electrodes"), 
                      error = function(e) data.frame())
                    localized <- nrow(etable) > 0 && all(c("Electrode", 
                      "Coord_x", "Coord_y", "Coord_z", "Label") %in% 
                      names(etable)) && any(etable$Coord_x^2 + 
                      etable$Coord_y^2 + etable$Coord_z^2 > 0, 
                      na.rm = TRUE)
                    n_electrodes <- length(subject$electrodes)
                    blocks <- tryCatch(subject$blocks, error = function(e) character(0))
                    n_epochs <- length(subject$epoch_names)
                    n_references <- length(subject$reference_names)
                    data.frame(Subject = sc, Electrodes = n_electrodes, 
                      Imported = data_imported, Notch = notch_filtered, 
                      Wavelet = has_wavelet, Localized = localized, 
                      Blocks = paste(blocks, collapse = ", "), 
                      Epoch_tables = n_epochs, Reference_tables = n_references, 
                      stringsAsFactors = FALSE)
                  }))
                head(subject_summary)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject_summary)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "subject_summary", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "subject_summary", target_expr = quote({
                {
                  available_subjects <- project$subjects()
                  if (!length(available_subjects)) {
                    stop("Project ", sQuote(project$name), " has no subjects.")
                  }
                  subject_summary <- do.call("rbind", lapply(available_subjects, 
                    function(sc) {
                      subject <- ravecore::RAVESubject$new(project_name = project$name, 
                        subject_code = sc, strict = FALSE)
                      preproc <- subject$preprocess_settings
                      data_imported <- all(preproc$data_imported)
                      notch_filtered <- all(preproc$notch_filtered)
                      has_wavelet <- all(preproc$has_wavelet)
                      etable <- tryCatch(subject$meta_data("electrodes"), 
                        error = function(e) data.frame())
                      localized <- nrow(etable) > 0 && all(c("Electrode", 
                        "Coord_x", "Coord_y", "Coord_z", "Label") %in% 
                        names(etable)) && any(etable$Coord_x^2 + 
                        etable$Coord_y^2 + etable$Coord_z^2 > 
                        0, na.rm = TRUE)
                      n_electrodes <- length(subject$electrodes)
                      blocks <- tryCatch(subject$blocks, error = function(e) character(0))
                      n_epochs <- length(subject$epoch_names)
                      n_references <- length(subject$reference_names)
                      data.frame(Subject = sc, Electrodes = n_electrodes, 
                        Imported = data_imported, Notch = notch_filtered, 
                        Wavelet = has_wavelet, Localized = localized, 
                        Blocks = paste(blocks, collapse = ", "), 
                        Epoch_tables = n_epochs, Reference_tables = n_references, 
                        stringsAsFactors = FALSE)
                    }))
                  head(subject_summary)
                }
                subject_summary
            }), target_depends = "project"), deps = "project", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    ensure_template_brain = targets::tar_target_raw(name = "template_subject_ensured", 
        command = quote({
            .__target_expr__. <- quote({
                template_root <- threeBrain::default_template_directory()
                installed_templates <- list.dirs(template_root, 
                  full.names = FALSE, recursive = FALSE)
                installed_templates <- installed_templates[grepl("^[a-zA-Z]", 
                  installed_templates)]
                installed_templates <- installed_templates[!installed_templates %in% 
                  c("templates", "prototypes")]
                if (!isTRUE(template_subject %in% installed_templates)) {
                  downloadable <- names(threeBrain::available_templates())
                  if (!isTRUE(template_subject %in% downloadable)) {
                    stop("Template brain not available. Choose from: ", 
                      paste(unique(c(downloadable, installed_templates)), 
                        collapse = ", "))
                  }
                  threeBrain::download_template_subject(template_subject)
                }
                template_subject_ensured <- template_subject
            })
            tryCatch({
                eval(.__target_expr__.)
                return(template_subject_ensured)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "template_subject_ensured", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "template_subject_ensured", target_expr = quote({
                {
                  template_root <- threeBrain::default_template_directory()
                  installed_templates <- list.dirs(template_root, 
                    full.names = FALSE, recursive = FALSE)
                  installed_templates <- installed_templates[grepl("^[a-zA-Z]", 
                    installed_templates)]
                  installed_templates <- installed_templates[!installed_templates %in% 
                    c("templates", "prototypes")]
                  if (!isTRUE(template_subject %in% installed_templates)) {
                    downloadable <- names(threeBrain::available_templates())
                    if (!isTRUE(template_subject %in% downloadable)) {
                      stop("Template brain not available. Choose from: ", 
                        paste(unique(c(downloadable, installed_templates)), 
                          collapse = ", "))
                    }
                    threeBrain::download_template_subject(template_subject)
                  }
                  template_subject_ensured <- template_subject
                }
                template_subject_ensured
            }), target_depends = "template_subject"), deps = "template_subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    resolve_subjects = targets::tar_target_raw(name = "resolved_subjects", 
        command = quote({
            .__target_expr__. <- quote({
                if (is.null(subject_codes) || !length(subject_codes)) {
                  resolved_subjects <- subject_summary$Subject
                } else {
                  resolved_subjects <- subject_codes[subject_codes %in% 
                    subject_summary$Subject]
                  if (!length(resolved_subjects)) {
                    stop("None of the requested subjects exist. Available: ", 
                      paste(subject_summary$Subject, collapse = ", "))
                  }
                }
                resolved_subjects <- sort(resolved_subjects)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(resolved_subjects)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "resolved_subjects", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "resolved_subjects", target_expr = quote({
                {
                  if (is.null(subject_codes) || !length(subject_codes)) {
                    resolved_subjects <- subject_summary$Subject
                  } else {
                    resolved_subjects <- subject_codes[subject_codes %in% 
                      subject_summary$Subject]
                    if (!length(resolved_subjects)) {
                      stop("None of the requested subjects exist. Available: ", 
                        paste(subject_summary$Subject, collapse = ", "))
                    }
                  }
                  resolved_subjects <- sort(resolved_subjects)
                }
                resolved_subjects
            }), target_depends = c("subject_codes", "subject_summary"
            )), deps = c("subject_codes", "subject_summary"), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    generate_group_subject_summary = targets::tar_target_raw(name = "snapshot_group_subject_summary", 
        command = quote({
            .__target_expr__. <- quote({
                snapshot_group_subject_summary <- subject_summary[subject_summary$Subject %in% 
                  resolved_subjects, , drop = FALSE]
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_group_subject_summary)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_group_subject_summary", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_group_subject_summary", 
            target_expr = quote({
                {
                  snapshot_group_subject_summary <- subject_summary[subject_summary$Subject %in% 
                    resolved_subjects, , drop = FALSE]
                }
                snapshot_group_subject_summary
            }), target_depends = c("subject_summary", "resolved_subjects"
            )), deps = c("subject_summary", "resolved_subjects"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), generate_group_brain = targets::tar_target_raw(name = "snapshot_group_brain", 
        command = quote({
            .__target_expr__. <- quote({
                pname <- project$name
                brain_list <- lapply(resolved_subjects, function(sc) {
                  subject <- ravecore::new_rave_subject(project_name = pname, 
                    subject_code = sc, strict = FALSE)
                  tryCatch(ravecore::rave_brain(subject), error = function(e) {
                    NULL
                  })
                })
                snapshot_group_brain <- threeBrain::merge_brain(.list = brain_list, 
                  template_surface_types = c("inflated", "sphere.reg"), 
                  template_subject = template_subject_ensured, 
                  electrode_priority = "sphere")
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_group_brain)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_group_brain", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_group_brain", target_expr = quote({
                {
                  pname <- project$name
                  brain_list <- lapply(resolved_subjects, function(sc) {
                    subject <- ravecore::new_rave_subject(project_name = pname, 
                      subject_code = sc, strict = FALSE)
                    tryCatch(ravecore::rave_brain(subject), error = function(e) {
                      NULL
                    })
                  })
                  snapshot_group_brain <- threeBrain::merge_brain(.list = brain_list, 
                    template_surface_types = c("inflated", "sphere.reg"), 
                    template_subject = template_subject_ensured, 
                    electrode_priority = "sphere")
                }
                snapshot_group_brain
            }), target_depends = c("project", "resolved_subjects", 
            "template_subject_ensured")), deps = c("project", 
        "resolved_subjects", "template_subject_ensured"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_group_electrode_coverage = targets::tar_target_raw(name = "snapshot_group_electrode_coverage", 
        command = quote({
            .__target_expr__. <- quote({
                pname <- project$name
                pieces <- lapply(resolved_subjects, function(sc) {
                  subject <- ravecore::new_rave_subject(project_name = pname, 
                    subject_code = sc, strict = FALSE)
                  etable <- tryCatch(subject$meta_data("electrodes"), 
                    error = function(e) NULL)
                  if (!is.data.frame(etable) || !nrow(etable) || 
                    !length(etable$FSLabel)) {
                    return(data.frame(FSLabel = "Unknown", Subject = sc, 
                      stringsAsFactors = FALSE))
                  }
                  if (!length(etable$FSLabel)) {
                    return(NULL)
                  }
                  labels <- etable$FSLabel
                  labels[is.na(labels) | !nzchar(labels)] <- "Unknown"
                  data.frame(FSLabel = labels, Subject = sc, 
                    stringsAsFactors = FALSE)
                })
                pieces <- do.call("rbind", pieces)
                if (!length(pieces)) {
                  snapshot_group_electrode_coverage <- data.frame(FSLabel = character(0), 
                    Total = integer(0), stringsAsFactors = FALSE)
                } else {
                  wide <- reshape2::dcast(pieces, FSLabel ~ Subject, 
                    fun.aggregate = length, value.var = "FSLabel", 
                    fill = 0L)
                  wide <- wide[wide$FSLabel != "Unknown", , drop = FALSE]
                  subject_cols <- setdiff(names(wide), "FSLabel")
                  wide$Total <- rowSums(wide[, subject_cols, 
                    drop = FALSE])
                  wide <- wide[, c("FSLabel", "Total", subject_cols), 
                    drop = FALSE]
                  snapshot_group_electrode_coverage <- wide
                  rownames(snapshot_group_electrode_coverage) <- NULL
                }
                head(snapshot_group_electrode_coverage)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_group_electrode_coverage)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_group_electrode_coverage", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_group_electrode_coverage", 
            target_expr = quote({
                {
                  pname <- project$name
                  pieces <- lapply(resolved_subjects, function(sc) {
                    subject <- ravecore::new_rave_subject(project_name = pname, 
                      subject_code = sc, strict = FALSE)
                    etable <- tryCatch(subject$meta_data("electrodes"), 
                      error = function(e) NULL)
                    if (!is.data.frame(etable) || !nrow(etable) || 
                      !length(etable$FSLabel)) {
                      return(data.frame(FSLabel = "Unknown", 
                        Subject = sc, stringsAsFactors = FALSE))
                    }
                    if (!length(etable$FSLabel)) {
                      return(NULL)
                    }
                    labels <- etable$FSLabel
                    labels[is.na(labels) | !nzchar(labels)] <- "Unknown"
                    data.frame(FSLabel = labels, Subject = sc, 
                      stringsAsFactors = FALSE)
                  })
                  pieces <- do.call("rbind", pieces)
                  if (!length(pieces)) {
                    snapshot_group_electrode_coverage <- data.frame(FSLabel = character(0), 
                      Total = integer(0), stringsAsFactors = FALSE)
                  } else {
                    wide <- reshape2::dcast(pieces, FSLabel ~ 
                      Subject, fun.aggregate = length, value.var = "FSLabel", 
                      fill = 0L)
                    wide <- wide[wide$FSLabel != "Unknown", , 
                      drop = FALSE]
                    subject_cols <- setdiff(names(wide), "FSLabel")
                    wide$Total <- rowSums(wide[, subject_cols, 
                      drop = FALSE])
                    wide <- wide[, c("FSLabel", "Total", subject_cols), 
                      drop = FALSE]
                    snapshot_group_electrode_coverage <- wide
                    rownames(snapshot_group_electrode_coverage) <- NULL
                  }
                  head(snapshot_group_electrode_coverage)
                }
                snapshot_group_electrode_coverage
            }), target_depends = c("project", "resolved_subjects"
            )), deps = c("project", "resolved_subjects"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_subject_module_reports = targets::tar_target_raw(name = "snapshot_subject_module_reports", 
        command = quote({
            .__target_expr__. <- quote({
                pname <- project$name
                report_dir_pattern <- "^report-(.+)_datetime-([0-9]{6}T[0-9]{6})_(.+)$"
                results <- structure(names = resolved_subjects, 
                  lapply(resolved_subjects, function(sc) {
                    subject <- ravecore::new_rave_subject(project_name = pname, 
                      subject_code = sc, strict = FALSE)
                    rpath <- subject$report_path
                    empty <- data.frame(report_name = character(0), 
                      module = character(0), timestamp = character(0), 
                      dir_name = character(0), file_path = character(0), 
                      stringsAsFactors = FALSE)
                    if (!length(rpath) || !dir.exists(rpath)) {
                      return(empty)
                    }
                    dirs <- list.dirs(rpath, recursive = FALSE, 
                      full.names = FALSE)
                    matched <- grepl(report_dir_pattern, dirs)
                    dirs <- dirs[matched]
                    if (!length(dirs)) {
                      return(empty)
                    }
                    parsed <- regmatches(dirs, regexec(report_dir_pattern, 
                      dirs))
                    rname <- vapply(parsed, `[[`, "", 2)
                    tstamp <- vapply(parsed, `[[`, "", 3)
                    module <- vapply(parsed, `[[`, "", 4)
                    fpath <- file.path(rpath, dirs, "report.html")
                    exists <- file.exists(fpath)
                    df <- data.frame(report_name = rname, module = module, 
                      timestamp = tstamp, dir_name = dirs, file_path = fpath, 
                      stringsAsFactors = FALSE)
                    df <- df[exists, , drop = FALSE]
                    if (length(module_filter)) {
                      df <- df[df$module %in% module_filter, 
                        , drop = FALSE]
                    }
                    if (nrow(df)) {
                      df <- df[order(df$timestamp, decreasing = TRUE), 
                        , drop = FALSE]
                      rownames(df) <- NULL
                    }
                    df
                  }))
                snapshot_subject_module_reports <- results
                head(snapshot_subject_module_reports)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_subject_module_reports)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_subject_module_reports", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_subject_module_reports", 
            target_expr = quote({
                {
                  pname <- project$name
                  report_dir_pattern <- "^report-(.+)_datetime-([0-9]{6}T[0-9]{6})_(.+)$"
                  results <- structure(names = resolved_subjects, 
                    lapply(resolved_subjects, function(sc) {
                      subject <- ravecore::new_rave_subject(project_name = pname, 
                        subject_code = sc, strict = FALSE)
                      rpath <- subject$report_path
                      empty <- data.frame(report_name = character(0), 
                        module = character(0), timestamp = character(0), 
                        dir_name = character(0), file_path = character(0), 
                        stringsAsFactors = FALSE)
                      if (!length(rpath) || !dir.exists(rpath)) {
                        return(empty)
                      }
                      dirs <- list.dirs(rpath, recursive = FALSE, 
                        full.names = FALSE)
                      matched <- grepl(report_dir_pattern, dirs)
                      dirs <- dirs[matched]
                      if (!length(dirs)) {
                        return(empty)
                      }
                      parsed <- regmatches(dirs, regexec(report_dir_pattern, 
                        dirs))
                      rname <- vapply(parsed, `[[`, "", 2)
                      tstamp <- vapply(parsed, `[[`, "", 3)
                      module <- vapply(parsed, `[[`, "", 4)
                      fpath <- file.path(rpath, dirs, "report.html")
                      exists <- file.exists(fpath)
                      df <- data.frame(report_name = rname, module = module, 
                        timestamp = tstamp, dir_name = dirs, 
                        file_path = fpath, stringsAsFactors = FALSE)
                      df <- df[exists, , drop = FALSE]
                      if (length(module_filter)) {
                        df <- df[df$module %in% module_filter, 
                          , drop = FALSE]
                      }
                      if (nrow(df)) {
                        df <- df[order(df$timestamp, decreasing = TRUE), 
                          , drop = FALSE]
                        rownames(df) <- NULL
                      }
                      df
                    }))
                  snapshot_subject_module_reports <- results
                  head(snapshot_subject_module_reports)
                }
                snapshot_subject_module_reports
            }), target_depends = c("project", "resolved_subjects", 
            "module_filter")), deps = c("project", "resolved_subjects", 
        "module_filter"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_subject_electrode_tables = targets::tar_target_raw(name = "snapshot_subject_electrode_tables", 
        command = quote({
            .__target_expr__. <- quote({
                pname <- project$name
                snapshot_subject_electrode_tables <- structure(names = resolved_subjects, 
                  lapply(resolved_subjects, function(sc) {
                    subject <- ravecore::new_rave_subject(project_name = pname, 
                      subject_code = sc, strict = FALSE)
                    tryCatch(subject$meta_data("electrodes"), 
                      error = function(e) data.frame())
                  }))
                head(snapshot_subject_electrode_tables)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_subject_electrode_tables)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_subject_electrode_tables", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_subject_electrode_tables", 
            target_expr = quote({
                {
                  pname <- project$name
                  snapshot_subject_electrode_tables <- structure(names = resolved_subjects, 
                    lapply(resolved_subjects, function(sc) {
                      subject <- ravecore::new_rave_subject(project_name = pname, 
                        subject_code = sc, strict = FALSE)
                      tryCatch(subject$meta_data("electrodes"), 
                        error = function(e) data.frame())
                    }))
                  head(snapshot_subject_electrode_tables)
                }
                snapshot_subject_electrode_tables
            }), target_depends = c("project", "resolved_subjects"
            )), deps = c("project", "resolved_subjects"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_subject_meta_summary = targets::tar_target_raw(name = "snapshot_subject_meta_summary", 
        command = quote({
            .__target_expr__. <- quote({
                pname <- project$name
                snapshot_subject_meta_summary <- structure(names = resolved_subjects, 
                  lapply(resolved_subjects, function(sc) {
                    subject <- ravecore::new_rave_subject(project_name = pname, 
                      subject_code = sc, strict = FALSE)
                    epoch_rows <- lapply(subject$epoch_names, 
                      function(en) {
                        etable <- tryCatch(subject$meta_data(meta_type = "epoch", 
                          meta_name = en), error = function(e) NULL)
                        n_trials <- if (is.data.frame(etable)) nrow(etable) else 0L
                        conditions <- if (is.data.frame(etable) && 
                          "Condition" %in% names(etable)) {
                          paste(unique(etable$Condition), collapse = ", ")
                        } else {
                          ""
                        }
                        data.frame(Type = "Epoch", Name = en, 
                          Trials = n_trials, Conditions = conditions, 
                          stringsAsFactors = FALSE)
                      })
                    ref_rows <- lapply(subject$reference_names, 
                      function(rn) {
                        data.frame(Type = "Reference", Name = rn, 
                          Trials = NA_integer_, Conditions = "", 
                          stringsAsFactors = FALSE)
                      })
                    do.call(rbind, c(epoch_rows, ref_rows))
                  }))
                head(snapshot_subject_meta_summary)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_subject_meta_summary)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_subject_meta_summary", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_subject_meta_summary", 
            target_expr = quote({
                {
                  pname <- project$name
                  snapshot_subject_meta_summary <- structure(names = resolved_subjects, 
                    lapply(resolved_subjects, function(sc) {
                      subject <- ravecore::new_rave_subject(project_name = pname, 
                        subject_code = sc, strict = FALSE)
                      epoch_rows <- lapply(subject$epoch_names, 
                        function(en) {
                          etable <- tryCatch(subject$meta_data(meta_type = "epoch", 
                            meta_name = en), error = function(e) NULL)
                          n_trials <- if (is.data.frame(etable)) nrow(etable) else 0L
                          conditions <- if (is.data.frame(etable) && 
                            "Condition" %in% names(etable)) {
                            paste(unique(etable$Condition), collapse = ", ")
                          } else {
                            ""
                          }
                          data.frame(Type = "Epoch", Name = en, 
                            Trials = n_trials, Conditions = conditions, 
                            stringsAsFactors = FALSE)
                        })
                      ref_rows <- lapply(subject$reference_names, 
                        function(rn) {
                          data.frame(Type = "Reference", Name = rn, 
                            Trials = NA_integer_, Conditions = "", 
                            stringsAsFactors = FALSE)
                        })
                      do.call(rbind, c(epoch_rows, ref_rows))
                    }))
                  head(snapshot_subject_meta_summary)
                }
                snapshot_subject_meta_summary
            }), target_depends = c("project", "resolved_subjects"
            )), deps = c("project", "resolved_subjects"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_subject_3d_viewers = targets::tar_target_raw(name = "snapshot_subject_3d_viewers", 
        command = quote({
            .__target_expr__. <- quote({
                pname <- project$name
                snapshot_root <- file.path(project$group_path("project_overview"), 
                  "snapshot", "subjects")
                snapshot_subject_3d_viewers <- structure(names = resolved_subjects, 
                  lapply(resolved_subjects, function(sc) {
                    subject <- ravecore::new_rave_subject(project_name = pname, 
                      subject_code = sc, strict = FALSE)
                    brain <- ravecore::rave_brain(subject)
                    brain
                  }))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_subject_3d_viewers)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_subject_3d_viewers", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_subject_3d_viewers", target_expr = quote({
                {
                  pname <- project$name
                  snapshot_root <- file.path(project$group_path("project_overview"), 
                    "snapshot", "subjects")
                  snapshot_subject_3d_viewers <- structure(names = resolved_subjects, 
                    lapply(resolved_subjects, function(sc) {
                      subject <- ravecore::new_rave_subject(project_name = pname, 
                        subject_code = sc, strict = FALSE)
                      brain <- ravecore::rave_brain(subject)
                      brain
                    }))
                }
                snapshot_subject_3d_viewers
            }), target_depends = c("project", "resolved_subjects"
            )), deps = c("project", "resolved_subjects"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), generate_subject_validation = targets::tar_target_raw(name = "snapshot_subject_validation", 
        command = quote({
            .__target_expr__. <- quote({
                pname <- project$name
                snapshot_subject_validation <- lapply(resolved_subjects, 
                  function(sc) {
                    subject <- ravecore::new_rave_subject(project_name = pname, 
                      subject_code = sc, strict = FALSE)
                    results <- tryCatch(ravecore::validate_subject(subject = subject, 
                      method = "normal", verbose = FALSE), error = function(e) {
                      NULL
                    })
                    if (is.null(results)) {
                      return(data.frame(Subject = sc, Section = "Error", 
                        Check = "validate_subject", Valid = FALSE, 
                        Message = "Validation failed to run", 
                        stringsAsFactors = FALSE))
                    }
                    section_names <- names(results)
                    rows <- lapply(section_names, function(sname) {
                      section <- results[[sname]]
                      if (!inherits(section, "fastmap2")) {
                        return(NULL)
                      }
                      item_names <- names(section)
                      do.call(rbind, lapply(item_names, function(iname) {
                        item <- section[[iname]]
                        if (!is.list(item) || is.null(item$valid)) {
                          return(NULL)
                        }
                        valid <- item$valid
                        msg <- if (isTRUE(valid)) {
                          "OK"
                        } else if (is.na(valid)) {
                          item$message %||% "Skipped"
                        } else {
                          item$message %||% "Failed"
                        }
                        data.frame(Subject = sc, Section = sname, 
                          Check = item$name %||% iname, Valid = isTRUE(valid), 
                          Message = msg, stringsAsFactors = FALSE)
                      }))
                    })
                    do.call(rbind, rows)
                  })
                names(snapshot_subject_validation) <- resolved_subjects
                head(snapshot_subject_validation)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(snapshot_subject_validation)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "snapshot_subject_validation", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "snapshot_subject_validation", target_expr = quote({
                {
                  pname <- project$name
                  snapshot_subject_validation <- lapply(resolved_subjects, 
                    function(sc) {
                      subject <- ravecore::new_rave_subject(project_name = pname, 
                        subject_code = sc, strict = FALSE)
                      results <- tryCatch(ravecore::validate_subject(subject = subject, 
                        method = "normal", verbose = FALSE), 
                        error = function(e) {
                          NULL
                        })
                      if (is.null(results)) {
                        return(data.frame(Subject = sc, Section = "Error", 
                          Check = "validate_subject", Valid = FALSE, 
                          Message = "Validation failed to run", 
                          stringsAsFactors = FALSE))
                      }
                      section_names <- names(results)
                      rows <- lapply(section_names, function(sname) {
                        section <- results[[sname]]
                        if (!inherits(section, "fastmap2")) {
                          return(NULL)
                        }
                        item_names <- names(section)
                        do.call(rbind, lapply(item_names, function(iname) {
                          item <- section[[iname]]
                          if (!is.list(item) || is.null(item$valid)) {
                            return(NULL)
                          }
                          valid <- item$valid
                          msg <- if (isTRUE(valid)) {
                            "OK"
                          } else if (is.na(valid)) {
                            item$message %||% "Skipped"
                          } else {
                            item$message %||% "Failed"
                          }
                          data.frame(Subject = sc, Section = sname, 
                            Check = item$name %||% iname, Valid = isTRUE(valid), 
                            Message = msg, stringsAsFactors = FALSE)
                        }))
                      })
                      do.call(rbind, rows)
                    })
                  names(snapshot_subject_validation) <- resolved_subjects
                  head(snapshot_subject_validation)
                }
                snapshot_subject_validation
            }), target_depends = c("project", "resolved_subjects"
            )), deps = c("project", "resolved_subjects"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
