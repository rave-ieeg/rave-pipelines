#!/usr/bin/env Rscript
# project_info.R — List the subjects of a RAVE project (or, without a
# project, the projects)
#
# Usage:
#   Rscript project_info.R [project]
#
# Example:
#   Rscript project_info.R demo

args <- commandArgs(trailingOnly = TRUE)

if (!length(args)) {
  projects <- ravecore::get_projects(refresh = TRUE)
  cat(sprintf("RAVE projects: %s\n",
              if (length(projects)) paste(projects, collapse = ", ") else "none"))
  cat("List a project's subjects with `Rscript project_info.R <project>`.\n")
  quit(save = "no")
}

project <- ravecore::as_rave_project(args[[1L]], strict = FALSE)
if (!dir.exists(project$path)) {
  cat(sprintf(paste(
    "There is no project `%s`. List the projects with",
    "`Rscript project_info.R`.\n"
  ), args[[1L]]))
  quit(save = "no", status = 1L)
}
subjects <- project$subjects()
cat(sprintf("RAVE project <%s>\n  Subjects: %s\n", project$name,
            if (length(subjects)) paste(subjects, collapse = ", ") else "none"))
cat("Describe a subject with `Rscript subject_info.R <project>/<subject>`.\n")
