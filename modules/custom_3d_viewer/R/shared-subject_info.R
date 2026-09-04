
path_is_valid <- function(path) {
  if (is.character(path) && length(path) == 1 && !is.na(path) && nzchar(path) && file.exists(path)) {
    return(TRUE)
  }
  return(FALSE)
}


subject_imaging_info <- function(
    project_name, subject_code,
    electrode_table = NULL,
    electrode_source_type = c(
      "Subject meta directory - electrodes.csv",
      "File upload - auto",
      "File upload - Scanner RAS",
      "File upload - tk-registered (FreeSurfer) RAS",
      "File upload - MNI152 RAS"
    )
) {

  if (length(electrode_source_type) > 1) {
    electrode_source_type <- match.arg(electrode_source_type)
  }

  stopifnot(
    "Project name must not be empty" = length(project_name) == 1 && nzchar(project_name),
    "Subject code must not be empty" = length(subject_code) == 1 && nzchar(subject_code)
  )

  # DIPSAUS DEBUG START
  # project_name <- "demo"
  # subject_code <- "DemoSubject"
  subject <- ravecore::new_rave_subject(project_name = project_name,
                                        subject_code = subject_code,
                                        strict = FALSE)

  # ---- Electrode coordinate table and system ---------------------------------
  coordinate_sys <- ""

  switch(
    electrode_source_type,
    "File upload - auto" = {
      if (is.data.frame(electrode_table) && all(c("Coord_x", "Coord_y", "Coord_z") %in% names(electrode_table))) {
        electrode_table$x <- electrode_table$Coord_x
        electrode_table$y <- electrode_table$Coord_y
        electrode_table$z <- electrode_table$Coord_z
      }
      coordinate_sys <- "tkrRAS"
    },
    "File upload - Scanner RAS" = {
      coordinate_sys <- "ScannerRAS"
    },
    "File upload - tk-registered (FreeSurfer) RAS" = {
      coordinate_sys <- "tkrRAS"
    },
    "File upload - MNI152 RAS" = {
      coordinate_sys <- "MNI152"
    },
    {
      # Default
      electrode_table <- tryCatch(
        {
          subject$get_electrode_table(warn = FALSE)
        }, error = function(e) {
          electrode_table
        }
      )
    }
  )
  if (is.data.frame(electrode_table)) {

    nms <- names(electrode_table)

    # Ensure column Electrode
    if (!"Electrode" %in% nms) {
      if ("Channel" %in% nms) {
        electrode_table$Electrode <- electrode_table$Channel
      } else {
        electrode_table$Electrode <- seq_len(nrow(electrode_table))
      }
    }
    electrode_table$Electrode <- as.integer(electrode_table$Electrode)

    # Ensure column Label
    if (!"Label" %in% nms) {
      if ("name" %in% nms) {
        electrode_table$Label <- electrode_table$name
      } else {
        electrode_table$Label <- sprintf("Electrode%04d", electrode_table$Electrode)
      }
    }

    # remove these two reserved columns in case they are inconsistent
    electrode_table$Subject <- NULL
    electrode_table$SubjectCode <- NULL

    nms <- names(electrode_table)
    if (
      !all(c("Coord_x", "Coord_y", "Coord_z") %in% nms) &&
      !all(c("x", "y", "z") %in% nms)
    ) {
      electrode_table <- NULL
      coordinate_sys <- ""
    }
  }

  # ---- Subject freesurfer information ---------------------------------
  brain <- ravecore::rave_brain(subject, include_electrodes = FALSE)

  volumes <- unique(c("aparc.DKTatlas+aseg", "aparc.a2009s+aseg", brain$available_atlases))

  surfaces <- c("smoothwm", "inflated", "white", "pial-outer-smooth", brain$available_surfaces)
  surfaces <- unique(surfaces[!tolower(surfaces) %in% c("pial", "pial.t1")])

  streamlines <- unique(c("default/*", brain$available_streamlines))

  annotations <- brain$available_annotations


  list(
    coordinate_table = electrode_table,
    coordinate_sys = coordinate_sys,
    volumes = volumes,
    surfaces = surfaces,
    annotations = annotations,
    streamlines = streamlines
  )
}
