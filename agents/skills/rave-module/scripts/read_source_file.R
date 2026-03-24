#!/usr/bin/env Rscript
# read_source_file.R — Read module source file with line numbers
#
# Usage:
#   Rscript read_source_file.R <module_id> --file=<path> [--start=1] [--nlines=200]
#
# Examples:
#   Rscript read_source_file.R notch_filter --file=DESCRIPTION
#   Rscript read_source_file.R notch_filter --file=main.Rmd
#   Rscript read_source_file.R notch_filter --file=R/module_server.R --start=50 --nlines=100
#
# Output format:
#   lineno: source line
#
# Only reads source files that are safe to read (privacy protected).
# User data files (settings.yaml, _targets/, etc.) are blocked.

source("scripts/_common.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript read_source_file.R <module_id> --file=<path> [--start=1] [--nlines=200]")
}

module_id <- args[[1L]]
file_arg <- parse_named_arg(args, "file")
start_arg <- parse_named_arg(args, "start")
nlines_arg <- parse_named_arg(args, "nlines")

if (is.null(file_arg)) {
  stop("--file=<path> is required. Example: --file=R/module_server.R")
}

start <- if (is.null(start_arg)) 1L else as.integer(start_arg)
nlines <- if (is.null(nlines_arg)) 200L else as.integer(nlines_arg)

module_path <- get_module_path(module_id)
result <- read_file_lines(module_path, file_arg, start = start, nlines = nlines)

cat(sprintf("## %s/%s (lines %d-%d of %d)\n\n",
            module_id, file_arg, result$start, result$end, result$total))

if (length(result$lines) == 0L) {
  cat("(no lines in range)\n")
} else {
  numbered <- format_numbered_lines(result$lines, result$start)
  cat(numbered, sep = "\n")
}

if (result$end < result$total) {
  cat(sprintf("\n\n... %d more lines. Use --start=%d to continue.\n",
              result$total - result$end, result$end + 1L))
}
