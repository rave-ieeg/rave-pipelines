#!/usr/bin/env Rscript
# run.R — Execute pipeline targets for a module
#
# Usage:
#   Rscript run.R <module_id> [--targets=t1,t2,t3]
#
# Examples:
#   Rscript run.R notch_filter
#   Rscript run.R notch_filter --targets=apply_notch,diagnostic_plots

source("scripts/_common.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript run.R <module_id> [--targets=t1,t2,t3]")
}

module_id <- args[[1L]]
pipeline <- get_pipeline(module_id)

target_names <- parse_csv_arg(args[-1L], "targets")

if (is.null(target_names)) {
  cat(sprintf("## Running all targets for '%s'\n", module_id))
  pipeline$run(
    scheduler = "none",
    type = "vanilla",
    callr_function = NULL,
    async = FALSE
  )
} else {
  cat(sprintf(
    "## Running targets for '%s': %s\n",
    module_id, paste(target_names, collapse = ", ")
  ))
  pipeline$run(
    names = target_names,
    scheduler = "none",
    type = "vanilla",
    callr_function = NULL,
    async = FALSE
  )
}

cat("\n## Done.\n")
