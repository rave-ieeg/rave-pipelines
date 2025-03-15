#' File whose name starts with `shared-` will be automatically
#' loaded by the pipeline. Hence you can put global static objects
#' here, for example,
#'   - define functions that may be used multiple times,
#'   - declare variables that can be re-used
#'   - import packages that will be used via `library()`, or `targets::tar_option_set` (see below)
NULL

`%OF%` <- dipsaus::`%OF%`
TEMPLATE_CHOICES = c(
  "Simple property",
  "Multiple properties",
  "Animation"
)

load_brain_from_subject_code <- function(subject_code, project_name = "[Auto]", surface_types = NULL, use_template = FALSE) {

  rave_path <- ravepipeline::raveio_getopt("data_dir")
  raw_path <- ravepipeline::raveio_getopt("raw_data_dir")

  brain <- NULL
  electrode_table <- NULL

  if( !project_name %in% c("[Auto]", "[Upload]", "[None]") ) {
    # user specified
    if(isTRUE(has_fsdir(project_name = project_name, subject_code = subject_code,
                        rave_path = rave_path, raw_path = raw_path))) {
      subject <- raveio::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code)
      # brain <- threeBrain::freesurfer_brain2(
      #   fs_subject_folder = subject$freesurfer_path,
      #   subject_name = subject_code, surface_types = c("pial", surface_types)
      # )
      brain <- raveio::rave_brain(subject, surfaces = c("pial", surface_types))
      brain$electrodes$objects <- list()
    }
    # get electrode table
    electrode_table <- raveio::load_meta2(meta_type = "electrodes",
                                          project_name = project_name,
                                          subject_code = subject_code)
  }

  if(!is.data.frame(electrode_table) && !project_name %in% c("[None]")) {
    electrode_table <- ravepipeline::pipeline_load_extdata("suggested_electrode_table")
  }

  if(is.null(brain)) {
    # auto-search brain
    projects <- get_projects_with_scode(subject_code = subject_code, refresh = TRUE,
                                        rave_path = rave_path)

    for(project in projects) {
      if( has_fsdir(project_name = project, subject_code = subject_code,
                    rave_path = rave_path, raw_path = raw_path) ) {
        subject <- raveio::RAVESubject$new(project_name = project,
                                           subject_code = subject_code)
        brain <- raveio::rave_brain(subject, surfaces = c("pial", surface_types))
        brain$electrodes$objects <- list()
        project_name <- project
        break
      }
    }
  }

  if( use_template ) {
    if(is.null(brain)) {
      template_subject <- unname(getOption('threeBrain.template_subject', 'N27'))
      template_dir <- threeBrain::default_template_directory()
      brain <- threeBrain::threeBrain(
        path = file.path(template_dir, template_subject),
        subject_code = subject_code,
        surface_types = unique(c( "pial", "pial-outer-smoothed", surface_types ))
      )
    } else {
      brain <- threeBrain::merge_brain(brain, template_surface_types = c( "pial", surface_types ))
    }
  } else if(is.null(brain)) {
    stop("Cannot find a valid 3D model for this subject. Please construct the 3D models, or use a MNI template brain.")
  }

  if(!is.null(brain) && is.data.frame(electrode_table) && !"Subject" %in% names(electrode_table)) {
    electrode_table$Subject <- subject_code
    brain$set_electrodes(electrode_table)
  }

  list(
    subject_code = subject_code,
    project_name = project_name,
    brain = brain,
    electrode_table = electrode_table,
    surface_types = brain$surface_types
  )
}

read_xlsx <- function(path, sheet = NULL, ...) {
  if( !rpymat:::env_available() && !dipsaus::package_installed("readxl") ) {
    ravemanager:::install_packages("readxl")
  }
  rpymat::read_xlsx(path = path, sheet = sheet, ...)
}
