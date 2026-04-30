# Set pipeline options
targets::tar_option_set(
  packages = c("ravepipeline")
)


ensure_brain_template <- function(template_subject = ravepipeline::raveio_getopt("threeBrain_template_subject")) {
  template_subject <- template_subject[!is.na(template_subject)]
  if(!length(template_subject)) {
    template_subject <- ravepipeline::raveio_getopt("threeBrain_template_subject")
  } else {
    template_subject <- template_subject[[1]]
  }
  template_root <- threeBrain::default_template_directory()
  template_path <- file.path(template_root, template_subject)
  if (!dir.exists(template_path)) {
    oldopt <- options(timeout = 3600)
    on.exit({ options(oldopt) })
    threeBrain::download_template_subject(subject_code = template_subject)
  }
  return(invisible(template_path))
}

get_cache_dir <- function(project_name) {
  file.path(pipeline$pipeline_path, "cache", project_name)
}

get_snapshot_dir <- function(project_name) {
  project <- ravecore::as_rave_project(project_name, strict = FALSE)
  file.path(project$group_path("project_overview"), "snapshot")
}

discover_existing_reports <- function(subject, module_filter = NULL) {
  report_path <- subject$report_path
  if (!dir.exists(report_path)) {
    return(data.frame(
      report_name = character(0),
      module = character(0),
      timestamp = character(0),
      file_path = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Pattern: report-{name}_datetime-{YYMMDDTHHMMSS}_{module}
  all_dirs <- list.dirs(report_path, recursive = FALSE, full.names = FALSE)
  pattern <- "^report-(.+)_datetime-([0-9]{6}T[0-9]{6})_(.+)$"
  matches <- grepl(pattern, all_dirs)
  matched_dirs <- all_dirs[matches]

  if (!length(matched_dirs)) {
    return(data.frame(
      report_name = character(0),
      module = character(0),
      timestamp = character(0),
      file_path = character(0),
      stringsAsFactors = FALSE
    ))
  }

  parsed <- regmatches(matched_dirs, regexec(pattern, matched_dirs))
  report_name <- vapply(parsed, `[[`, "", 2)
  timestamp <- vapply(parsed, `[[`, "", 3)
  module <- vapply(parsed, `[[`, "", 4)
  file_path <- file.path(report_path, matched_dirs, "report.html")

  df <- data.frame(
    report_name = report_name,
    module = module,
    timestamp = timestamp,
    dir_name = matched_dirs,
    file_path = file_path,
    exists = file.exists(file_path),
    stringsAsFactors = FALSE
  )

  # Filter to existing reports
  df <- df[df$exists, , drop = FALSE]

  # Apply module filter
  if (length(module_filter) && !all(is.na(module_filter))) {
    df <- df[df$module %in% module_filter, , drop = FALSE]
  }

  # Sort by timestamp descending (newest first)
  if (nrow(df)) {
    df <- df[order(df$timestamp, decreasing = TRUE), , drop = FALSE]
  }

  df
}
