#!/usr/bin/env Rscript
# get_targets.R — List targets, dependencies, and build status
#
# Usage:
#   Rscript get_targets.R <module_id>
#
# Example:
#   Rscript get_targets.R notch_filter

source("scripts/_common.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript get_targets.R <module_id>")
}

module_id <- args[[1L]]
pipeline <- get_pipeline(module_id)

# Get target names and progress
progress <- tryCatch(
  ravepipeline::pipeline_progress(method = "details"),
  error = function(e) NULL
)
vartable <- tryCatch(
  ravepipeline::pipeline_vartable(),
  error = function(e) NULL
)

cat(sprintf("## Targets for '%s'\n\n", module_id))

if (!is.null(progress) && nrow(progress) > 0L) {
  cat(sprintf("%-35s %s\n", "TARGET", "STATUS"))
  cat(sprintf(
    "%-35s %s\n",
    strrep("-", 35), strrep("-", 12)
  ))
  for (i in seq_len(nrow(progress))) {
    cat(sprintf(
      "%-35s %s\n",
      progress$name[[i]],
      progress$progress[[i]]
    ))
  }
} else if (!is.null(vartable) && nrow(vartable) > 0L) {
  cat(sprintf("%-35s\n", "TARGET"))
  cat(sprintf("%-35s\n", strrep("-", 35)))
  for (i in seq_len(nrow(vartable))) {
    cat(sprintf("%-35s\n", vartable$name[[i]]))
  }
} else {
  cat("No target information available.\n")
  cat("Run the pipeline first or check module configuration.\n")
}
