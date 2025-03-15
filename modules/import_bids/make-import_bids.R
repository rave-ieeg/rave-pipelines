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
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_BIDS_dataset = targets::tar_target_raw("BIDS_dataset", 
        quote({
            settings[["BIDS_dataset"]]
        }), deps = "settings"), input_BIDS_sessions = targets::tar_target_raw("BIDS_sessions", 
        quote({
            settings[["BIDS_sessions"]]
        }), deps = "settings"), input_BIDS_runs = targets::tar_target_raw("BIDS_runs", 
        quote({
            settings[["BIDS_runs"]]
        }), deps = "settings"), input_backup = targets::tar_target_raw("backup", 
        quote({
            settings[["backup"]]
        }), deps = "settings"), input_BIDS_subject = targets::tar_target_raw("BIDS_subject", 
        quote({
            settings[["BIDS_subject"]]
        }), deps = "settings"), input_overwrite = targets::tar_target_raw("overwrite", 
        quote({
            settings[["overwrite"]]
        }), deps = "settings"), get_BIDS_subject_information = targets::tar_target_raw(name = "BIDS_subject_info", 
        command = quote({
            .__target_expr__. <- quote({
                bids_root <- ravepipeline::raveio_getopt("bids_data_dir")
                if (!checkmate::test_character(BIDS_subject, 
                  min.chars = 4, pattern = "^sub-", len = 1L, 
                  any.missing = FALSE)) {
                  stop("BIDS format requires the subject code to start with `sub-`. Please add this prefix to the input.")
                }
                BIDS_subject_path <- file.path(bids_root, BIDS_dataset, 
                  BIDS_subject)
                runs_and_datatypes <- find_bids_runs(subject_path = BIDS_subject_path, 
                  BIDS_subject = BIDS_subject, BIDS_sessions = BIDS_sessions)
                dtypes <- runs_and_datatypes$data_types
                scode <- gsub("sub-", "", BIDS_subject)
                scode <- gsub("-", "_", scode)
                if (!grepl("^[a-zA-Z]", scode)) {
                  scode <- sprintf("subj_%s", scode)
                }
                BIDS_subject_info <- list(subject_code = scode, 
                  path = BIDS_subject_path, runs = runs_and_datatypes$runs, 
                  data_types = dtypes)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(BIDS_subject_info)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "BIDS_subject_info", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "BIDS_subject_info", target_expr = quote({
                {
                  bids_root <- ravepipeline::raveio_getopt("bids_data_dir")
                  if (!checkmate::test_character(BIDS_subject, 
                    min.chars = 4, pattern = "^sub-", len = 1L, 
                    any.missing = FALSE)) {
                    stop("BIDS format requires the subject code to start with `sub-`. Please add this prefix to the input.")
                  }
                  BIDS_subject_path <- file.path(bids_root, BIDS_dataset, 
                    BIDS_subject)
                  runs_and_datatypes <- find_bids_runs(subject_path = BIDS_subject_path, 
                    BIDS_subject = BIDS_subject, BIDS_sessions = BIDS_sessions)
                  dtypes <- runs_and_datatypes$data_types
                  scode <- gsub("sub-", "", BIDS_subject)
                  scode <- gsub("-", "_", scode)
                  if (!grepl("^[a-zA-Z]", scode)) {
                    scode <- sprintf("subj_%s", scode)
                  }
                  BIDS_subject_info <- list(subject_code = scode, 
                    path = BIDS_subject_path, runs = runs_and_datatypes$runs, 
                    data_types = dtypes)
                }
                BIDS_subject_info
            }), target_depends = c("BIDS_subject", "BIDS_dataset", 
            "BIDS_sessions")), deps = c("BIDS_subject", "BIDS_dataset", 
        "BIDS_sessions"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), validate_BIDS_runs = targets::tar_target_raw(name = "migrate_plan", 
        command = quote({
            .__target_expr__. <- quote({
                if (!length(BIDS_runs)) {
                  stop("Please choose at least one run to import")
                }
                runs <- lapply(BIDS_runs, function(run) {
                  if (!is.list(run)) {
                    run_path <- run
                    run_name <- NULL
                  } else {
                    run_path <- run$path
                    run_name <- run$name
                  }
                  if (!checkmate::test_character(run_path, len = 1L, 
                    min.chars = 1L, any.missing = FALSE)) {
                    stop("Invalid choices of BIDS run to import: `BIDS_runs` must be non-empty and exist")
                  }
                  data_files <- file.path(BIDS_subject_info$path, 
                    sprintf("%s_%s", run_path, BIDS_subject_info$data_types))
                  if (!any(file.exists(data_files))) {
                    stop("Run [", run_path, "] has no data in side.")
                  }
                  if (!checkmate::test_character(run_name, len = 1L, 
                    any.missing = FALSE, pattern = "^[a-zA-Z0-9_]{1,}$")) {
                    run_name <- suggust_block_name(run_path)
                  }
                  return(list(path = run_path, name = run_name))
                })
                src_paths <- gsub("[/|\\\\]+", "/", sapply(runs, 
                  "[[", "path"))
                block_names <- sapply(runs, "[[", "name")
                if (any(duplicated(block_names))) {
                  block_names <- sprintf("%s%s", block_names, 
                    seq_along(block_names))
                }
                abs_src_paths <- file.path(BIDS_subject_info$path, 
                  src_paths)
                src_exist <- sapply(abs_src_paths, function(path) {
                  any(file.exists(sprintf("%s_%s", path, BIDS_subject_info$data_types)))
                })
                raw_path <- ravepipeline::raveio_getopt("raw_data_dir")
                dst_paths <- file.path(raw_path, BIDS_subject_info$subject_code, 
                  block_names)
                dst_exist <- dir.exists(dst_paths)
                needs_migrate <- src_exist & (!dst_exist | overwrite)
                migrate_plan <- data.frame(Source = src_paths, 
                  Block = block_names, SourceExist = src_exist, 
                  BlockExist = dst_exist, Planned = needs_migrate, 
                  row.names = NULL)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(migrate_plan)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "migrate_plan", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "migrate_plan", target_expr = quote({
                {
                  if (!length(BIDS_runs)) {
                    stop("Please choose at least one run to import")
                  }
                  runs <- lapply(BIDS_runs, function(run) {
                    if (!is.list(run)) {
                      run_path <- run
                      run_name <- NULL
                    } else {
                      run_path <- run$path
                      run_name <- run$name
                    }
                    if (!checkmate::test_character(run_path, 
                      len = 1L, min.chars = 1L, any.missing = FALSE)) {
                      stop("Invalid choices of BIDS run to import: `BIDS_runs` must be non-empty and exist")
                    }
                    data_files <- file.path(BIDS_subject_info$path, 
                      sprintf("%s_%s", run_path, BIDS_subject_info$data_types))
                    if (!any(file.exists(data_files))) {
                      stop("Run [", run_path, "] has no data in side.")
                    }
                    if (!checkmate::test_character(run_name, 
                      len = 1L, any.missing = FALSE, pattern = "^[a-zA-Z0-9_]{1,}$")) {
                      run_name <- suggust_block_name(run_path)
                    }
                    return(list(path = run_path, name = run_name))
                  })
                  src_paths <- gsub("[/|\\\\]+", "/", sapply(runs, 
                    "[[", "path"))
                  block_names <- sapply(runs, "[[", "name")
                  if (any(duplicated(block_names))) {
                    block_names <- sprintf("%s%s", block_names, 
                      seq_along(block_names))
                  }
                  abs_src_paths <- file.path(BIDS_subject_info$path, 
                    src_paths)
                  src_exist <- sapply(abs_src_paths, function(path) {
                    any(file.exists(sprintf("%s_%s", path, BIDS_subject_info$data_types)))
                  })
                  raw_path <- ravepipeline::raveio_getopt("raw_data_dir")
                  dst_paths <- file.path(raw_path, BIDS_subject_info$subject_code, 
                    block_names)
                  dst_exist <- dir.exists(dst_paths)
                  needs_migrate <- src_exist & (!dst_exist | 
                    overwrite)
                  migrate_plan <- data.frame(Source = src_paths, 
                    Block = block_names, SourceExist = src_exist, 
                    BlockExist = dst_exist, Planned = needs_migrate, 
                    row.names = NULL)
                }
                migrate_plan
            }), target_depends = c("BIDS_runs", "BIDS_subject_info", 
            "overwrite")), deps = c("BIDS_runs", "BIDS_subject_info", 
        "overwrite"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), migrate_to_rave_raw = targets::tar_target_raw(name = "migrate_result", 
        command = quote({
            .__target_expr__. <- quote({
                raw_path <- ravepipeline::raveio_getopt("raw_data_dir")
                subject_code <- BIDS_subject_info$subject_code
                data_types <- BIDS_subject_info$data_types
                bids_root <- BIDS_subject_info$path
                subject_root <- file.path(raw_path, subject_code)
                migrate_result_path <- file.path(subject_root, 
                  "conf-BIDS_import.yaml")
                if (!dir.exists(subject_root)) {
                  raveio::dir_create2(subject_root)
                }
                if (file.exists(migrate_result_path)) {
                  migrate_result <- tryCatch({
                    as.list(raveio::load_yaml(migrate_result_path))
                  }, error = function(e) {
                    list()
                  })
                } else {
                  migrate_result <- list()
                }
                migrate_result$profile <- "BIDS import configuration"
                if (!is.list(migrate_result$migrates)) {
                  migrate_result$migrates <- list()
                }
                migrate_result$subject_code <- list(BIDS = BIDS_subject, 
                  RAVE = subject_code)
                migrate_result$subject_path <- list(BIDS = bids_root, 
                  RAVE = subject_root)
                if (any(migrate_plan$Planned)) {
                  plan_table <- migrate_plan[migrate_plan$Planned, 
                    , drop = FALSE]
                  res <- dipsaus::lapply_async2(x = seq_len(nrow(plan_table)), 
                    FUN = function(ii) {
                      src_prefix <- file.path(bids_root, plan_table$Source[[ii]])
                      src_files <- sprintf("%s_%s", src_prefix, 
                        data_types)
                      fe <- file.exists(src_files)
                      dtypes <- data_types[fe]
                      src_files <- src_files[fe]
                      dst_path <- file.path(subject_root, plan_table$Block[[ii]])
                      if (file.exists(dst_path)) {
                        if (backup) {
                          raveio::backup_file(dst_path, remove = TRUE)
                        }
                      }
                      unlink(dst_path, recursive = TRUE)
                      raveio::dir_create2(dst_path)
                      for (jj in seq_along(dtypes)) {
                        src_abspath <- src_files[[jj]]
                        dst_abspath <- file.path(dst_path, basename(src_abspath))
                        file.copy(from = src_abspath, to = dst_abspath, 
                          overwrite = TRUE, copy.mode = TRUE, 
                          copy.date = TRUE)
                      }
                      list(BIDS_run = plan_table$Source[[ii]], 
                        RAVE_block = plan_table$Block[[ii]], 
                        imported = dtypes, timestamp = strftime(Sys.time(), 
                          usetz = TRUE))
                    }, plan = FALSE, callback = function(ii) {
                      sprintf("BIDS to RAVE|migrating %s", plan_table$Block[[ii]])
                    })
                  migrate_result$migrates <- c(migrate_result$migrates, 
                    res)
                  raveio::save_yaml(migrate_result, file = migrate_result_path)
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(migrate_result)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "migrate_result", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "migrate_result", target_expr = quote({
                {
                  raw_path <- ravepipeline::raveio_getopt("raw_data_dir")
                  subject_code <- BIDS_subject_info$subject_code
                  data_types <- BIDS_subject_info$data_types
                  bids_root <- BIDS_subject_info$path
                  subject_root <- file.path(raw_path, subject_code)
                  migrate_result_path <- file.path(subject_root, 
                    "conf-BIDS_import.yaml")
                  if (!dir.exists(subject_root)) {
                    raveio::dir_create2(subject_root)
                  }
                  if (file.exists(migrate_result_path)) {
                    migrate_result <- tryCatch({
                      as.list(raveio::load_yaml(migrate_result_path))
                    }, error = function(e) {
                      list()
                    })
                  } else {
                    migrate_result <- list()
                  }
                  migrate_result$profile <- "BIDS import configuration"
                  if (!is.list(migrate_result$migrates)) {
                    migrate_result$migrates <- list()
                  }
                  migrate_result$subject_code <- list(BIDS = BIDS_subject, 
                    RAVE = subject_code)
                  migrate_result$subject_path <- list(BIDS = bids_root, 
                    RAVE = subject_root)
                  if (any(migrate_plan$Planned)) {
                    plan_table <- migrate_plan[migrate_plan$Planned, 
                      , drop = FALSE]
                    res <- dipsaus::lapply_async2(x = seq_len(nrow(plan_table)), 
                      FUN = function(ii) {
                        src_prefix <- file.path(bids_root, plan_table$Source[[ii]])
                        src_files <- sprintf("%s_%s", src_prefix, 
                          data_types)
                        fe <- file.exists(src_files)
                        dtypes <- data_types[fe]
                        src_files <- src_files[fe]
                        dst_path <- file.path(subject_root, plan_table$Block[[ii]])
                        if (file.exists(dst_path)) {
                          if (backup) {
                            raveio::backup_file(dst_path, remove = TRUE)
                          }
                        }
                        unlink(dst_path, recursive = TRUE)
                        raveio::dir_create2(dst_path)
                        for (jj in seq_along(dtypes)) {
                          src_abspath <- src_files[[jj]]
                          dst_abspath <- file.path(dst_path, 
                            basename(src_abspath))
                          file.copy(from = src_abspath, to = dst_abspath, 
                            overwrite = TRUE, copy.mode = TRUE, 
                            copy.date = TRUE)
                        }
                        list(BIDS_run = plan_table$Source[[ii]], 
                          RAVE_block = plan_table$Block[[ii]], 
                          imported = dtypes, timestamp = strftime(Sys.time(), 
                            usetz = TRUE))
                      }, plan = FALSE, callback = function(ii) {
                        sprintf("BIDS to RAVE|migrating %s", 
                          plan_table$Block[[ii]])
                      })
                    migrate_result$migrates <- c(migrate_result$migrates, 
                      res)
                    raveio::save_yaml(migrate_result, file = migrate_result_path)
                  }
                }
                migrate_result
            }), target_depends = c("BIDS_subject_info", "BIDS_subject", 
            "migrate_plan", "backup")), deps = c("BIDS_subject_info", 
        "BIDS_subject", "migrate_plan", "backup"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"))
