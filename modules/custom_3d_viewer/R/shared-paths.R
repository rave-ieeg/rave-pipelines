
#' Get 'Neurosynth' website address using 'MNI152' coordinates
#' @param x,y,z numerical values: the right-anterior-superior 'RAS'
#' coordinates in \code{'MNI152'} space
#' @returns 'Neurosynth' website address
#' @export
url_neurosynth <- function(x, y, z) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  z <- as.numeric(z)

  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  z[is.na(z)] <- 0
  sprintf("https://neurosynth.org/locations/?x=%.0f&y=%.0f&z=%.0f", x, y, z)
}

has_fsdir <- function(project_name, subject_code) {
  if(length(subject_code) != 1 || is.na(subject_code) || !nzchar(subject_code)) {
    return(FALSE)
  }
  if(length(project_name) != 1 || is.na(project_name) || !nzchar(project_name)) {
    native_raw_path <- ravepipeline::raveio_getopt("raw_data_dir")
    raw_fs <- file.path(native_raw_path, subject_code, "rave-imaging", "fs")
    if(file.exists(raw_fs)) {
      if(threeBrain::check_freesurfer_path(raw_fs, autoinstall_template = FALSE,
                                           check_volume = FALSE)) {
        return(raw_fs)
      }
    }
    return(FALSE)
  }
  subject <- ravecore::RAVESubject$new(project_name = project_name,
                                       subject_code = subject_code,
                                       strict = FALSE)
  re <- subject$freesurfer_path
  if(length(re) != 1 || is.na(re) || !file.exists(re)) {
    return(FALSE)
  }
  return(TRUE)
}

get_projects_with_scode <- function(subject_code, refresh = TRUE) {
  if(length(subject_code) != 1 || is.na(subject_code) || !nzchar(subject_code)) {
    return(NULL)
  }
  all_projects <- ravecore::get_projects(refresh = refresh)
  all_projects <- all_projects[
    vapply(
      all_projects,
      function(project_name) {
        project <- ravecore::as_rave_project(project_name)
        isTRUE(project$has_subject(subject_code))
      },
      FALSE
    )
  ]
  all_projects
}

get_brain_subject <- function(brain, strict = TRUE) {
  if(length(brain$subject_code) != 1) { return() }

  subject_code <- brain$subject_code
  project_name <- brain$meta$constructor_params$project_name %||% brain$project_name

  if(length(subject_code) != 1 || length(project_name) != 1) { return() }
  ravecore::RAVESubject$new(project_name = project_name,
                            subject_code = subject_code,
                            strict = strict)
}

get_subject_imaging_datapath <- function(
    ..., subject_code, project_name, type = c("uploads", "pipeline"), check = FALSE,
    raw_path = NULL) {

  type <- match.arg(type)
  subject <- ravecore::RAVESubject$new(project_name = project_name,
                                       subject_code = subject_code,
                                       strict = TRUE)
  switch(
    type,
    "pipeline" = {
      .NotYetImplemented()
      root_path <- subject$pipeline_path
      if(check && dir.exists(subject$rave_path)) {
        # subject must exists, otherwise do not create
        ravepipeline::dir_create2(root_path)
      }
      file.path(root_path, ...)
    },
    {
      stopifnot(is.null(raw_path))
      root_path <- file.path(subject$imaging_path, "custom-data")
      if(check) {
        ravepipeline::dir_create2(root_path)
      }
      file.path(root_path, ...)
    }
  )

}
