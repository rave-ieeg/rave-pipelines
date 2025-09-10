map_to_template <- function(subject, method = c("high-density"), ...) {

  method <- match.arg(method)

  re <- switch (
    method,
    'high-density' = map_to_template_thinfilm(subject = subject, ...)
  )
  return(re)
}

map_to_template_thinfilm <- function(subject, template = "fsaverage", volumetric_transform = "affine",
                                     interpolator = 0.3, save_to = NULL, flip_hemisphere = FALSE, n_segments = c(16, 16), ...) {
  subject <- ravecore::as_rave_subject(subject, strict = FALSE)
  # Make sure fspath exists
  freesurfer_path <- subject$freesurfer_path
  tryCatch({
    stopifnot(threeBrain::check_freesurfer_path(freesurfer_path, check_volume = TRUE, check_surface = TRUE))
  }, error = function(e) {
    stop("Check FreeSurfer folder failed for subject ", subject$subject_id, ". Please make sure the FreeSurfer folder `fs` exists under folder [", subject$imaging_path, "] and the FreeSurfer pipeline is finished successfully")
  })
  mapped_table <- ravecore::transform_thinfilm_to_mni152(
    subject = subject,
    volumetric_transform = volumetric_transform,
    interpolator = interpolator,
    template_subject = template,
    flip_hemisphere = flip_hemisphere,
    n_segments = n_segments
  )

  # Save template coordinates ----------------------------------------------------
  sub_table <- mapped_table[, c("Electrode", "Label",
                                "MNI305_x", "MNI305_y", "MNI305_z",
                                "MNI152_x", "MNI152_y", "MNI152_z",
                                "Radius")]
  electrode_table <- subject$get_electrode_table()
  electrode_table$MNI305_x <- mapped_table$MNI305_x
  electrode_table$MNI305_y <- mapped_table$MNI305_y
  electrode_table$MNI305_z <- mapped_table$MNI305_z
  electrode_table$MNI152_x <- mapped_table$MNI152_x
  electrode_table$MNI152_y <- mapped_table$MNI152_y
  electrode_table$MNI152_z <- mapped_table$MNI152_z
  electrode_table$Radius <- mapped_table$Radius
  electrode_table$SubjectCode <- subject$subject_code

  if(length(save_to) == 1) {
    if(isTRUE(save_to)) {
      ravecore::save_meta2(
        meta_type = "electrodes",
        project_name = subject$project_name,
        subject_code = subject$subject_code,
        data = electrode_table
      )
    } else if(is.character(save_to)) {
      ravecore:::safe_write_csv(
        x = sub_table,
        file = file.path(subject$meta_path, save_to),
        quiet = FALSE
      )
    }
  }


  # ---- Preview the results -----------------------------------------------------
  # brain <- raveio::rave_brain(subject = subject)
  #
  # # To view the electrodes on template, use "sphere" shape to represent contacts
  # brain$set_electrodes(electrode_table, priority = "sphere")
  # template_brain <- threeBrain::merge_brain(brain, template_subject = template)

  # Plot
  list(
    mapped_table = sub_table,
    subject = subject$subject_id
  )

}
