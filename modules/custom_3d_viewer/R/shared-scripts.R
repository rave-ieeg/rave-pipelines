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
    subject_code, project_name,

    overlay_types = NULL,
    surface_types = NULL,
    annot_types = NULL,

    use_spheres = FALSE,
    override_radius = NA,

    coordinate_sys = NULL,
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


  brain <- NULL
  electrode_table <- NULL

  surface_types <- unique(c("pial", surface_types, "sphere.reg"))

  electrode_table <- ravepipeline::pipeline_load_extdata("suggested_electrode_table")

  include_electrodes <- FALSE
  if(!is.data.frame(electrode_table)) {
    include_electrodes <- TRUE
  }

  subject <- ravecore::RAVESubject$new(
    project_name = project_name,
    subject_code = subject_code
  )

  brain <- ravecore::rave_brain(
    subject = subject,
    surfaces = surface_types,
    overlays = overlay_types,
    annotations = annot_types,
    include_electrodes = include_electrodes
  )
  if( use_template ) {
    if(is.null(brain)) {
      template <- threeBrain::merge_brain(
        template_surface_types = surface_types
        # template_atlas_types = overlay_types,
        # template_annotation_types = annot_types
      )
      brain <- template$template_object
    } else {
      template <- threeBrain::merge_brain(
        brain,
        template_surface_types = surface_types
        # template_atlas_types = overlay_types,
        # template_annotation_types = annot_types
      )
    }
  } else {
    template <- NULL
  }
  if(is.null(brain)) {
    stop("Unable to find the 3D brain models. Have you created one yet? See rave.wiki or the following paper if you haven't: https://doi.org/10.1523/ENEURO.0328-23.2023")
  }

  if(!include_electrodes) {

    if(use_spheres) {
      priority <- "sphere"
    } else {
      priority <- "prototype"
    }

    if(isTRUE(override_radius > 0)) {
      electrode_table$Radius <- override_radius
    }

    coordinate_sys <- paste(coordinate_sys, collapse = "")
    if(isTRUE(coordinate_sys %in% c("tkrRAS", "ScannerRAS", "MNI152", "MNI305"))) {
      brain$set_electrodes(electrode_table, coord_sys = coordinate_sys, priority = priority)
    } else {
      brain$set_electrodes(electrode_table, priority = priority)
    }
  }

  if(use_template && !is.null(template)) {
    brain <- template
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
