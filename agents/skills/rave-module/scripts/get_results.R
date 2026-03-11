#!/usr/bin/env Rscript
# get_results.R — Read results from a completed pipeline target
#
# Usage:
#   Rscript get_results.R <module_id> --target=<name>
#
# Example:
#   Rscript get_results.R notch_filter --target=apply_notch

source("scripts/_common.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript get_results.R <module_id> --target=<name>")
}

module_id <- args[[1L]]
target_name <- parse_named_arg(args[-1L], "target")

if (is.null(target_name)) {
  stop("--target=<name> is required. Usage: Rscript get_results.R <module_id> --target=<name>")
}

pipeline <- get_pipeline(module_id)

result <- tryCatch(
  pipeline$read(var_names = target_name),
  error = function(e) {
    stop(sprintf(
      "Failed to read target '%s': %s",
      target_name, conditionMessage(e)
    ))
  }
)

cat(sprintf(
  "## Result for target '%s' in '%s'\n\n",
  target_name, module_id
))

output <- capture.output(
  str(result, max.level = 3L, vec.len = 20L)
)
if (length(output) > 200L) {
  cat(paste(output[1:200], collapse = "\n"))
  cat(sprintf(
    "\n\n... (truncated, %d lines total)\n",
    length(output)
  ))
} else {
  cat(paste(output, collapse = "\n"))
  cat("\n")
}
