# Export helpers for project_overview module

consolidate_export <- function(project_name, cache_dir, snapshot_dir,
                                build_dir, subject_codes,
                                subject_sections, rmd_path) {

  # Clean and recreate build directory
  if (dir.exists(build_dir)) {
    unlink(build_dir, recursive = TRUE)
  }
  dir.create(build_dir, recursive = TRUE)

  # 1. Copy cache structure to build
  group_cache <- file.path(cache_dir, "group")
  if (dir.exists(group_cache)) {
    group_build <- file.path(build_dir, "group")
    dir.create(group_build, recursive = TRUE)
    file.copy(
      list.files(group_cache, full.names = TRUE),
      group_build, recursive = TRUE
    )
  }

  # Copy group viewer from snapshot_dir
  group_viewer_src <- file.path(snapshot_dir, "group", "group_viewer.html")
  if (file.exists(group_viewer_src)) {
    group_build <- file.path(build_dir, "group")
    if (!dir.exists(group_build)) dir.create(group_build, recursive = TRUE)
    file.copy(group_viewer_src, file.path(group_build, "group_viewer.html"))
  }

  # Copy per-subject data
  for (sc in subject_codes) {
    subject_cache <- file.path(cache_dir, "subjects", sc)
    subject_build <- file.path(build_dir, "subjects", sc)
    dir.create(subject_build, recursive = TRUE)

    if (dir.exists(subject_cache)) {
      file.copy(
        list.files(subject_cache, full.names = TRUE),
        subject_build, recursive = TRUE
      )
    }

    # Copy viewer from snapshot_dir
    viewer_src <- file.path(snapshot_dir, "subjects", sc, "viewer.html")
    if (file.exists(viewer_src)) {
      file.copy(viewer_src, file.path(subject_build, "viewer.html"))
    }

    # Copy actual report HTML files
    if (isTRUE(subject_sections$module_reports)) {
      manifest_path <- file.path(subject_cache, "reports", "manifest.rds")
      if (file.exists(manifest_path)) {
        manifest <- readRDS(manifest_path)
        if (nrow(manifest)) {
          reports_build <- file.path(subject_build, "reports")
          if (!dir.exists(reports_build)) dir.create(reports_build, recursive = TRUE)

          for (i in seq_len(nrow(manifest))) {
            src_html <- manifest$file_path[[i]]
            if (file.exists(src_html)) {
              report_dir <- file.path(reports_build, manifest$dir_name[[i]])
              dir.create(report_dir, recursive = TRUE)
              file.copy(src_html, file.path(report_dir, "report.html"))
            }
          }
        }
      }
    }

    # Generate static HTML tables
    generate_static_validation(sc, subject_cache, subject_build)
  }

  # Generate static electrode coverage table
  generate_static_coverage(build_dir)

  # Generate static subjects summary
  generate_static_subjects_summary(build_dir, project_name, subject_codes)

  # 2. Render index.html from Rmd template
  if (file.exists(rmd_path)) {
    index_path <- file.path(build_dir, "index.html")
    rmarkdown::render(
      input = rmd_path,
      output_file = index_path,
      params = list(
        project_name = project_name,
        build_dir = build_dir,
        subject_codes = subject_codes,
        subject_sections = subject_sections
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
  }

  # 3. Zip the build directory
  zip_path <- paste0(build_dir, ".zip")
  if (file.exists(zip_path)) {
    unlink(zip_path)
  }

  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(dirname(build_dir))
  utils::zip(
    zipfile = zip_path,
    files = basename(build_dir)
  )

  zip_path
}


generate_static_table <- function(rds_path, output_html, title = "Data Table") {
  if (!file.exists(rds_path)) return(invisible(NULL))

  data <- readRDS(rds_path)
  if (!is.data.frame(data) || !nrow(data)) return(invisible(NULL))

  table_html <- knitr::kable(data, format = "html", escape = FALSE)

  html <- sprintf(
    '<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>%s</title>
  <style>
    body { font-family: sans-serif; margin: 20px; }
    table { border-collapse: collapse; width: 100%%; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f2f2f2; }
    tr:nth-child(even) { background-color: #f9f9f9; }
  </style>
</head>
<body>
  <h2>%s</h2>
  %s
</body>
</html>', title, title, table_html)

  writeLines(html, output_html)
  invisible(output_html)
}


generate_static_validation <- function(subject_code, subject_cache, subject_build) {
  snapshot_path <- file.path(subject_cache, "snapshot.rds")
  if (!file.exists(snapshot_path)) return(invisible(NULL))

  snapshot <- readRDS(snapshot_path)
  validation <- snapshot$validation
  if (is.null(validation)) return(invisible(NULL))

  if (is.list(validation) && !is.data.frame(validation)) {
    validation <- tryCatch(
      as.data.frame(validation, stringsAsFactors = FALSE),
      error = function(e) {
        data.frame(
          Check = names(validation),
          Result = vapply(validation, function(v) paste(v, collapse = "; "), ""),
          stringsAsFactors = FALSE
        )
      }
    )
  }

  if (!is.data.frame(validation) || !nrow(validation)) return(invisible(NULL))

  output_html <- file.path(subject_build, "validation.html")
  generate_static_table(
    rds_path = snapshot_path,
    output_html = output_html,
    title = sprintf("Validation: %s", subject_code)
  )

  # Actually write the validation data directly
  table_html <- knitr::kable(validation, format = "html", escape = FALSE)
  html <- sprintf(
    '<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Validation: %s</title>
  <style>
    body { font-family: sans-serif; margin: 20px; }
    table { border-collapse: collapse; width: 100%%; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f2f2f2; }
    tr:nth-child(even) { background-color: #f9f9f9; }
  </style>
</head>
<body>
  <h2>Validation: %s</h2>
  %s
</body>
</html>', subject_code, subject_code, table_html)

  writeLines(html, output_html)
  invisible(output_html)
}


generate_static_coverage <- function(build_dir) {
  rds_path <- file.path(build_dir, "group", "snapshot.rds")
  if (!file.exists(rds_path)) return(invisible(NULL))

  snapshot <- readRDS(rds_path)
  coverage <- snapshot$electrode_coverage
  if (is.null(coverage) || !nrow(coverage)) return(invisible(NULL))

  output_html <- file.path(build_dir, "group", "electrode_coverage.html")
  table_html <- knitr::kable(coverage, format = "html", escape = FALSE)

  html <- sprintf(
    '<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Electrode Coverage</title>
  <style>
    body { font-family: sans-serif; margin: 20px; }
    table { border-collapse: collapse; width: 100%%; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f2f2f2; }
    tr:nth-child(even) { background-color: #f9f9f9; }
  </style>
</head>
<body>
  <h2>Electrode Coverage</h2>
  %s
</body>
</html>', table_html)

  writeLines(html, output_html)
  invisible(output_html)
}


generate_static_subjects_summary <- function(build_dir, project_name, subject_codes) {
  rows <- lapply(subject_codes, function(sc) {
    snapshot_path <- file.path(build_dir, "subjects", sc, "snapshot.rds")
    if (!file.exists(snapshot_path)) {
      return(data.frame(
        Subject = sc, Electrodes = NA, Epochs = NA,
        `Report Time` = NA, stringsAsFactors = FALSE, check.names = FALSE
      ))
    }
    snapshot <- readRDS(snapshot_path)
    data.frame(
      Subject = sc,
      Electrodes = length(snapshot$electrodes),
      Epochs = length(snapshot$epoch_names),
      `Report Time` = if (length(snapshot$snapshot_date)) strftime(snapshot$snapshot_date, usetz = TRUE) else NA,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })

  df <- do.call(rbind, rows)
  if (is.null(df) || !nrow(df)) return(invisible(NULL))

  output_html <- file.path(build_dir, "group", "subjects_summary.html")
  if (!dir.exists(dirname(output_html))) dir.create(dirname(output_html), recursive = TRUE)

  table_html <- knitr::kable(df, format = "html", escape = FALSE)
  html <- sprintf(
    '<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Subjects Summary - %s</title>
  <style>
    body { font-family: sans-serif; margin: 20px; }
    table { border-collapse: collapse; width: 100%%; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f2f2f2; }
    tr:nth-child(even) { background-color: #f9f9f9; }
  </style>
</head>
<body>
  <h2>Subjects Summary: %s</h2>
  %s
</body>
</html>', project_name, project_name, table_html)

  writeLines(html, output_html)
  invisible(output_html)
}
