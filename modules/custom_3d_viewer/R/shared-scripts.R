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

load_brain_from_subject_code <- function(
    subject_code, project_name = "[Auto]",

    overlay_types = NULL,
    surface_types = NULL,
    annot_types = NULL,

    use_spheres = FALSE,
    override_radius = NA,

    use_template = FALSE
  ) {

  # DIPSAUS DEBUG START
  # subject_code <- "Precision003"
  # list2env(list(project_name = "[Auto]",overlay_types = NULL,
  #               surface_types = NULL,
  #               annot_types = NULL,
  #
  #               use_spheres = FALSE,
  #               override_radius = NA,
  #
  #               use_template = FALSE), envir=.GlobalEnv)


  rave_path <- ravepipeline::raveio_getopt("data_dir")
  raw_path <- ravepipeline::raveio_getopt("raw_data_dir")

  brain <- NULL
  electrode_table <- NULL

  surface_types <- unique(c("pial", surface_types, "sphere.reg"))

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
      brain <- raveio::rave_brain(
        subject = subject,
        surfaces = surface_types,
        overlays = overlay_types,
        annotations = annot_types,
        include_electrodes = FALSE
      )
      # brain$electrodes$objects <- list()
      # brain$electrodes$objects2 <- list()
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
        brain <- raveio::rave_brain(
          subject = subject,
          surfaces = surface_types,
          overlays = overlay_types,
          annotations = annot_types,
          include_electrodes = FALSE
        )
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
        surface_types = surface_types
        # atlas_types = overlay_types,
        # annotation_types = annot_types
      )
      # have to fake the table a bit since we do not use native space
      mni305 <- as.matrix(electrode_table[, c("MNI305_x", "MNI305_y", "MNI305_z")])
      isvalid <- rowSums(tkr_ras ^ 2) > 0
      mni305 <- t(cbind(mni305, 1))
      tkr_ras <- brain$Torig %*% solve(brain$Norig) %*% solve(brain$xfm) %*% mni305
      tkr_ras[, !isvalid] <- 0
      electrode_table$Coord_x <- tkr_ras[1, ]
      electrode_table$Coord_y <- tkr_ras[2, ]
      electrode_table$Coord_z <- tkr_ras[3, ]
    } else {
      brain <- threeBrain::merge_brain(
        brain,
        template_surface_types = surface_types
        # template_atlas_types = overlay_types,
        # template_annotation_types = annot_types
      )
    }
  } else if(is.null(brain)) {
    stop("Cannot find a valid 3D model for this subject. Please construct the 3D models, or use a MNI template brain. For example, run `threeBrain::download_template_subject('cvs_avg35_inMNI152'); threeBrain::set_default_template('cvs_avg35_inMNI152')`")
  }

  if(!is.null(brain) && is.data.frame(electrode_table) && !"Subject" %in% names(electrode_table)) {
    electrode_table$Subject <- subject_code

    if(isTRUE(override_radius > 0)) {
      electrode_table$Radius <- override_radius
    }

    brain$set_electrodes(electrode_table, priority = ifelse(isTRUE(use_spheres), "sphere", "prototype"))
  }

  list(
    brain = brain,
    subject_code = subject_code,
    project_name = project_name,
    electrode_table = electrode_table,
    surface_types = brain$surface_types,
    overlay_types = overlay_types,
    annot_types = annot_types,
    use_spheres = use_spheres,
    override_radius = override_radius
  )
}

read_xlsx <- function(path, sheet = NULL, ...) {
  if( !rpymat:::env_available() && !dipsaus::package_installed("readxl") ) {
    ravemanager:::install_packages("readxl")
  }
  rpymat::read_xlsx(path = path, sheet = sheet, ...)
}
