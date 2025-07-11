

subject_fs_path <- function(subject_code) {
  subject <- raveio::RAVESubject$new(project_name = "YAEL", subject_code = subject_code, strict = FALSE)
  fs_path <- normalizePath(file.path(subject$imaging_path, "fs"), winslash = "/", mustWork = FALSE)
  fs_path
}


mapping_capability <- function(project_name, subject_code) {

  subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)

  has_ants <- rpyANTs::ants_available()

  # cache
  meta_root <- subject$meta_path
  surface_mapped_path <- file.path(meta_root, "electrodes_normalization_spherical.csv")
  mni152_mapped_path <- file.path(meta_root, "electrodes_normalization_mni152.csv")

  has_cache_surface <- file.exists(surface_mapped_path)
  has_cache_volumetric <- file.exists(mni152_mapped_path)

  # sphere.reg
  brain <- raveio::rave_brain(subject = subject, include_electrodes = FALSE, surfaces = "sphere.reg")

  has_fs <- !is.null(brain)
  has_sphere <- "sphere.reg" %in% brain$surface_types

  # affine
  has_affine <- has_fs

  # mni152 non-linear
  normalization_path <- file.path(subject$imaging_path, "normalization")
  has_normalization <- dir.exists(normalization_path)

  if(has_normalization && has_ants) {
    tryCatch(
      {
        yael <- raveio::as_yael_process(subject)
        mapping <- NULL
        for(name in c("mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09c")) {
          mapping <- yael$get_template_mapping(name)
          if(!is.null(mapping)) {
            break
          }
        }
        if(is.null(mapping)) {
          has_normalization <- FALSE
        }
      },
      error = function(e) {
        has_normalization <<- FALSE
      }
    )
  }

  capability <- list(
    freesurfer = has_fs,
    ants = has_ants,
    affine = has_affine,
    surface = has_sphere,
    surface_cache = has_cache_surface,
    mni152 = has_normalization,
    mni152_cache = has_cache_volumetric
  )

  return(capability)
}


get_mapped_brain <- function(project_name, subject_code, mapping_method = c("auto", "sphere.reg", "mni152", "affine"), capability = NULL, use_cache = TRUE) {

  mapping_method <- match.arg(mapping_method)

  subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
  brain <- raveio::rave_brain(subject = subject, surfaces = "sphere.reg", include_electrodes = FALSE)

  if(is.null(brain)) { return(NULL) }

  if(!length(capability)) {
    capability <- mapping_capability(project_name = project_name, subject_code = subject_code)
  }

  electrode_table <- tryCatch(
    {
      subject$get_electrode_table()
    },
    error = function(e) {
      return(NULL)
    }
  )
  if(!is.data.frame(electrode_table)) { return(brain) }
  tkr_ras <- as.matrix(electrode_table[, sprintf("Coord_%s", c("x", "y", "z"))])
  has_localization <- rowSums(tkr_ras^2)
  has_localization <- !is.na(has_localization) & has_localization > 0

  if(!any(has_localization)) {
    return(brain)
  }
  if( mapping_method == "auto" ) {
    brain$set_electrodes(electrode_table, priority = "sphere")
    return(brain)
  }

  meta_root <- subject$meta_path

  trim_coordinates <- function(x) {
    x[!has_localization] <- 0
    x
  }

  if( mapping_method == "sphere.reg" ) {
    # check if spherical normalization is available
    surface_mapped_path <- file.path(meta_root, "electrodes_normalization_spherical.csv")
    if(file.exists(surface_mapped_path) && !use_cache) {
      raveio::backup_file(surface_mapped_path, remove = TRUE, quiet = FALSE)
    }

    if(!file.exists(surface_mapped_path)) {

      # needs to calculate the spherical normalization
      if(capability$surface) {
        # the files are available so generate the electrodes_normalization_spherical.csv on the fly
        mapped_table_spherical <- raveio::transform_point_to_template(
          subject = subject,
          # positions = tkr_ras,
          # space = "tkrRAS",
          mapping_method = "surface",
          flip_hemisphere = FALSE,
          verbose = TRUE,
          project_surface = "pial"
        )
        mapped_table <- data.frame(
          SubjectCode = subject$subject_code,
          Electrode = electrode_table$Electrode,
          LabelPrefix = electrode_table$LabelPrefix,
          Label = electrode_table$Label,
          Hemisphere = mapped_table_spherical$Hemisphere,

          # MNI305 as surface coordinate
          MNI305_x = trim_coordinates(mapped_table_spherical$MNI305_x),
          MNI305_y = trim_coordinates(mapped_table_spherical$MNI305_y),
          MNI305_z = trim_coordinates(mapped_table_spherical$MNI305_z),

          # spherical
          DistanceShifted = trim_coordinates(mapped_table_spherical$DistanceShifted),
          Sphere_x = trim_coordinates(mapped_table_spherical$Sphere_x),
          Sphere_y = trim_coordinates(mapped_table_spherical$Sphere_y),
          Sphere_z = trim_coordinates(mapped_table_spherical$Sphere_z),

          # scanner RAS
          T1R = trim_coordinates(electrode_table$T1R),
          T1A = trim_coordinates(electrode_table$T1A),
          T1S = trim_coordinates(electrode_table$T1S),

          # tkr-RAS
          Coord_x = electrode_table$Coord_x,
          Coord_y = electrode_table$Coord_y,
          Coord_z = electrode_table$Coord_z
        )

        radius <- electrode_table$Radius
        if(length(radius)) {
          mapped_table$Radius <- radius
        }

        write.csv(x = mapped_table, file = surface_mapped_path, row.names = FALSE)
      } else {
        mapped_table <- electrode_table
      }
    } else {
      mapped_table <- read.csv(file = surface_mapped_path)
    }

    brain$set_electrodes(mapped_table, priority = "sphere")
    return(brain)
  }

  if( mapping_method == "mni152" ) {

    mni152_mapped_path <- file.path(meta_root, "electrodes_normalization_mni152.csv")
    if(file.exists(mni152_mapped_path) && !use_cache) {
      raveio::backup_file(mni152_mapped_path, remove = TRUE, quiet = FALSE)
    }

    if(!file.exists(mni152_mapped_path)) {

      # needs to calculate the non-linear MNI152 normalization

      if(capability$mni152) {

        # the files are available so generate the electrodes_normalization_spherical.csv on the fly
        mapped_table_volumetric <- raveio::transform_point_to_template(
          subject = subject,
          # positions = tkr_ras,
          # space = "tkrRAS",
          mapping_method = "volumetric",
          flip_hemisphere = FALSE,
          verbose = TRUE,
          project_surface = "pial"
        )

        mapped_table <- data.frame(
          SubjectCode = subject$subject_code,
          Electrode = electrode_table$Electrode,
          LabelPrefix = electrode_table$LabelPrefix,
          Label = electrode_table$Label,
          Hemisphere = electrode_table$Hemisphere,

          # MNI152
          MNI152_x = trim_coordinates(mapped_table_volumetric$MNI152_x),
          MNI152_y = trim_coordinates(mapped_table_volumetric$MNI152_y),
          MNI152_z = trim_coordinates(mapped_table_volumetric$MNI152_z),

          # MNI305 for 3D viewer
          MNI305_x = trim_coordinates(mapped_table_volumetric$MNI305_x),
          MNI305_y = trim_coordinates(mapped_table_volumetric$MNI305_y),
          MNI305_z = trim_coordinates(mapped_table_volumetric$MNI305_z),

          # scanner RAS
          T1R = trim_coordinates(electrode_table$T1R),
          T1A = trim_coordinates(electrode_table$T1A),
          T1S = trim_coordinates(electrode_table$T1S),

          # tkr-RAS
          Coord_x = electrode_table$Coord_x,
          Coord_y = electrode_table$Coord_y,
          Coord_z = electrode_table$Coord_z
        )

        radius <- electrode_table$Radius
        if(length(radius)) {
          mapped_table$Radius <- radius
        }

        write.csv(x = mapped_table, file = mni152_mapped_path, row.names = FALSE)
      } else {
        mapped_table <- electrode_table
      }
    } else {
      mapped_table <- read.csv(file = mni152_mapped_path)
    }

    brain$set_electrodes(mapped_table, priority = "sphere")
    return(brain)
  }

  brain$set_electrodes(electrode_table, priority = "sphere")
  return(brain)

}










