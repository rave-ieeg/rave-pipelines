#!/usr/bin/env Rscript
# list_source_files.R — List canonical source files in a module
#
# Usage:
#   Rscript list_source_files.R <module_id>
#
# Examples:
#   Rscript list_source_files.R notch_filter
#   Rscript list_source_files.R power_explorer
#
# Only lists source files that are safe to read (privacy protected).
# User data files (settings.yaml, _targets/, etc.) are excluded.

source("scripts/_common.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript list_source_files.R <module_id>")
}

module_id <- args[[1L]]
module_path <- get_module_path(module_id)

files <- list_canonical_files(module_path)

cat(sprintf("## Source files in '%s' (%d files)\n\n", module_id, length(files)))

# Group by directory
dirs <- unique(dirname(files))
dirs <- sort(dirs)

for (d in dirs) {
  if (d == ".") {
    cat("Root:\n")
    root_files <- files[dirname(files) == "."]
    for (f in root_files) {
      cat(sprintf("  %s\n", f))
    }
  } else {
    cat(sprintf("\n%s/:\n", d))
    dir_files <- files[dirname(files) == d]
    for (f in dir_files) {
      cat(sprintf("  %s\n", basename(f)))
    }
  }
}
