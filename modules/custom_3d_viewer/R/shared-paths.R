
has_fsdir <- function(project_name, subject_code,
                      rave_path = ravepipeline::raveio_getopt("data_dir"),
                      raw_path = ravepipeline::raveio_getopt("raw_data_dir")) {
  if(length(subject_code) != 1 || is.na(subject_code) || !nzchar(subject_code)) {
    return(FALSE)
  }
  raw_fs <- file.path(raw_path, subject_code, "rave-imaging", "fs")
  if(length(project_name) != 1 || is.na(project_name) || !nzchar(project_name)) {
    if(file.exists(raw_fs)) {
      if(threeBrain::check_freesurfer_path(raw_fs, autoinstall_template = FALSE,
                                           check_volume = FALSE)) {
        return(raw_fs)
      }
    }
    return(FALSE)
  }
  if(!dir.exists(file.path(rave_path, project_name, subject_code))) {
    if(threeBrain::check_freesurfer_path(raw_fs, autoinstall_template = FALSE,
                                         check_volume = FALSE)) {
      return(raw_fs)
    }
    return(FALSE)
  }
  subject <- raveio::RAVESubject$new(project_name = project_name,
                                     subject_code = subject_code,
                                     strict = FALSE)
  re <- subject$freesurfer_path
  if(length(re) != 1 || is.na(re) || !file.exists(re)) {
    return(FALSE)
  }
  return(TRUE)
}

get_projects_with_scode <- function(subject_code, refresh = TRUE,
                                    rave_path = ravepipeline::raveio_getopt("data_dir")) {
  if(length(subject_code) != 1 || is.na(subject_code) || !nzchar(subject_code)) {
    return(NULL)
  }
  all_projects <- raveio::get_projects(refresh = refresh)
  dirs <- file.path(rave_path, all_projects, subject_code)
  all_projects <- all_projects[dir.exists(dirs)]
  all_projects
}

get_subject_imaging_datapath <- function(
    ..., subject_code, project_name, type = c("uploads", "pipeline"), check = FALSE,
    raw_path = ravepipeline::raveio_getopt("raw_data_dir")) {

  type <- match.arg(type)

  switch(
    type,
    "pipeline" = {
      subject <- raveio::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
      root_path <- subject$pipeline_path
      if(check && dir.exists(subject$rave_path)) {
        # subject must exists, otherwise do not create
        raveio::dir_create2(root_path)
      }
      file.path(root_path, ...)
    },
    {
      root_path <- file.path(raw_path, subject_code,
                             "rave-imaging", "custom-data")
      if(check) {
        raveio::dir_create2(root_path)
      }
      file.path(root_path, ...)
    }
  )

}
