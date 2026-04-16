#!/usr/bin/env Rscript
# set_inputs.R — Read or update a module's settings.yaml
#
# Usage:
#   Rscript set_inputs.R <module_id> ['JSON_STRING']
#
# Examples:
#   Rscript set_inputs.R notch_filter
#   Rscript set_inputs.R notch_filter '{"subject_code":"YAB","project_name":"demo"}'
#   Rscript set_inputs.R notch_filter '{"notch_filter_lowerbound":[59,118,178]}'

source("scripts/_common.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript set_inputs.R <module_id> ['JSON_STRING']")
}

module_id <- args[[1L]]
pipeline <- get_pipeline(module_id)
settings <- pipeline$get_settings()

if (length(args) < 2L) {
  cat(jsonlite::toJSON(settings, auto_unbox = TRUE, pretty = TRUE))
} else {
  new_values <- jsonlite::fromJSON(args[[2L]], simplifyVector = TRUE)
  if (!is.list(new_values) || is.null(names(new_values))) {
    stop("JSON argument must be a named object, e.g. '{\"key\": \"value\"}'")
  }
  invalid_keys <- setdiff(names(new_values), names(settings))
  if (length(invalid_keys) > 0L) {
    stop(sprintf(
      "Unknown input(s): %s\nAvailable settings:\n%s",
      paste(invalid_keys, collapse = ", "),
      jsonlite::toJSON(settings, auto_unbox = TRUE, pretty = TRUE)
    ))
  }
  pipeline$set_settings(.list = new_values)
  settings <- pipeline$get_settings()
  cat(jsonlite::toJSON(settings, auto_unbox = TRUE, pretty = TRUE))
}
