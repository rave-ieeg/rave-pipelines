#!/usr/bin/env Rscript
# subject_info.R — Describe a RAVE subject without loading its data
#
# Prints the subject's blocks; its electrodes by signal type, with sample
# rates and which ones are Notch-filtered or have Wavelet power; its epoch
# and reference names (the subject's defaults are marked); and its imaging
# state. Use it to offer the user the real choices for a module's loader.
#
# Usage:
#   Rscript subject_info.R <project/subject>
#
# Example:
#   Rscript subject_info.R demo/DemoSubject

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !grepl("^[^/]+/[^/]+$", args[[1L]])) {
  stop("Usage: Rscript subject_info.R <project/subject>, e.g. demo/DemoSubject")
}

subject <- ravecore::as_rave_subject(args[[1L]], strict = FALSE)
if (!dir.exists(subject$path)) {
  cat(sprintf(paste(
    "There is no subject `%s`. List the subjects of project `%s` with",
    "`Rscript project_info.R %s`.\n"
  ), args[[1L]], subject$project_name, subject$project_name))
  quit(save = "no", status = 1L)
}
print(subject)
