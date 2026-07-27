dir_copy2 <- function(path, new_path, hidden_files = FALSE, overwrite = FALSE) {

  path <- normalizePath(path, mustWork = TRUE)
  new_path <- normalizePath(new_path, mustWork = FALSE)

  if (isTRUE(path == new_path)) {
    return()
  }

  if (!dir.exists(new_path)) {
    dir.create(new_path, showWarnings = FALSE, recursive = TRUE)
  }

  file_paths <- list.files(
    path = path,
    all.files = hidden_files,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE
  )

  # check if any existing files
  if (length(file_paths)) {
    if (!overwrite) {
      target_paths <- file.path(new_path, file_paths)
      target_exists <- file.exists(target_paths)
      if (any(target_exists)) {
        stop("One or more files exist and overwrite is disabled. Abort copying the folder.")
      }
    }
  }

  dir_paths <- list.dirs(path = path, full.names = FALSE, recursive = TRUE)
  dir_paths <- dir_paths[!dir_paths %in% c("", ".", "..")]
  if (!hidden_files) {
    dir_paths <- dir_paths[!startsWith(dir_paths, ".")]
  }

  if (length(dir_paths)) {
    lapply(dir_paths, function(p) {
      dir.create(file.path(new_path, p), showWarnings = FALSE, recursive = TRUE)
    })
  }

  if (!length(file_paths)) { return() }

  # copy!
  file.copy(
    file.path(path, file_paths),
    file.path(new_path, file_paths),
    overwrite = overwrite
  )
}
