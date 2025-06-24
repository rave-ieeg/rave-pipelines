
extract_meta_value_table <- function(value_file, project_name) {
  # value_file <- "/Users/dipterix/PennNeurosurgery Dropbox/Dipterix W/BeauchampLabAtPenn/RAVE/RAVE_Talks/2025_Precision_Jun23/Data/group_mask.csv"
  # project_name <- "YAEL"

  project <- raveio::as_rave_project(project_name)
  subjects <- project$subjects()

  # Source data from which the mask/value files will be created
  if(is.data.frame(value_file)) {
    value_table <- value_file
  } else {
    value_table <- data.table::fread(value_file)
  }
  subjects <- sort(unique(value_table$Subject))
  value_names <- names(value_table)
  value_names <- value_names[!tolower(value_names) %in% c("electrode", "time", "subjectcode", "subject")]

  list(
    subjects = subjects,
    value_names = value_names
  )
}

load_mapped_brain <- function(project_name, subject_codes, mapped_results) {
  res <- lapply(subject_codes, function(subject_code) {
    subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
    brain <- raveio::rave_brain(subject)
    if(is.null(brain)) { return() }
    electrode_table <- brain$electrodes$raw_table
    electrode_table$Subject <- subject_code
    merged <- merge(
      electrode_table,
      mapped_results,
      by = c("Subject", "Electrode"),
      all.x = TRUE,
      all.y = FALSE,
      suffixes = c("___no_use", "")
    )
    nms <- names(merged)
    nms <- nms[!endsWith(nms, "___no_use")]
    electrode_table <- as.data.frame(merged)[, nms]
    brain$set_electrodes(electrode_table, priority = "sphere")
    brain
  })
  names(res) <- subject_codes
  dipsaus::drop_nulls(res)
}


generate_atlas <- function(
    mapped_results, value_table, template_name, value_name,
    value_type = c("numerical", "categorical"),
    mapping_threshold = 1) {
  # template_name <- cleaned_inputs$template$name
  # value_name <- "Fun_Channels"

  mapped_results <- as.data.frame(mapped_results)
  value_table <- as.data.frame(value_table)

  value_type <- match.arg(value_type)

  # merge the value table
  combined_table <- merge(
    mapped_results[, c("Subject", "Electrode", "MNI152_x", "MNI152_y", "MNI152_z")],
    value_table,
    by = c("Subject", "Electrode"),
    all = FALSE)

  # ---- Load template surface (pial) models and generate atlas ------------------

  pial_right <- ieegio::read_surface(
    file.path(
      threeBrain::default_template_directory(),
      template_name,
      "surf",
      "rh.pial"
    )
  )
  pial_left <- ieegio::read_surface(
    file.path(
      threeBrain::default_template_directory(),
      template_name,
      "surf",
      "lh.pial"
    )
  )

  # Calculate the coordinates of right-hemisphere surface in the template space
  rh_pial_in_fsaverage <- pial_right$geometry$transforms$ScannerAnat %*% pial_right$geometry$vertices
  lh_pial_in_fsaverage <- pial_left$geometry$transforms$ScannerAnat %*% pial_left$geometry$vertices

  # For each mesh vertex node, find the nearest electrode contact across all subjects
  contact_positions <- as.matrix(combined_table[, paste0("MNI152_", c("x", "y", "z"))])

  # pial_surface <- rh_pial_in_fsaverage
  create_atlas <- function(pial_surface) {
    # ravetools::vcg_kdtree_nearest is a RAVE function that finds nearest `k` points
    # for each contact on the surface
    kdtree <- ravetools::vcg_kdtree_nearest(
      target = contact_positions,
      query = t(pial_surface[1:3, , drop = FALSE]),
      k = 3
    )

    # Remove points that are too far
    dist <- kdtree$distance
    dist[dist > mapping_threshold] <- Inf

    # Set weights based on distance
    weights <- exp(-dist)
    sum_weights <- rowSums(weights)
    sum_weights[sum_weights == 0] <- 1
    weights <- weights / sum_weights

    # Generate template atlas values
    mesh_values <- combined_table[[value_name]][kdtree$index]
    dim(mesh_values) <- dim(kdtree$index)

    if(value_type == "numerical") {
      mesh_values_weighted_avg <- rowSums(mesh_values * weights)
      re <- ieegio::as_ieegio_surface(
        x = NULL,
        measurements = data.frame(value = mesh_values_weighted_avg))
    } else {
      levels <- sort(unique(combined_table[[value_name]]))
      levels <- levels[!levels %in% "Unknown"]
      levels <- c("Unknown", levels)

      best_matching_value <- sapply(seq_len(nrow(weights)), function(ii) {
        x <- weights[ii, ]
        idx <- which.max(x)
        if(x[[idx]] == 0) { return("Unknown") }
        return(mesh_values[ii, idx])
      })
      best_matching_value <- factor(as.character(best_matching_value), levels = levels)

      n_colors <- max(length(threeBrain:::DEFAULT_COLOR_DISCRETE), length(levels))
      colors <- c("#000000", grDevices::colorRampPalette(threeBrain:::DEFAULT_COLOR_DISCRETE)(n_colors - 1))

      re <- ieegio::as_ieegio_surface(
        x = NULL,
        annotation_labels = data.frame(
          Key = seq_along(levels) - 1L,
          Label = levels,
          Color = colors[seq_along(levels)]
        ),
        annotation_values = data.frame(value = as.integer(best_matching_value) - 1L)
      )
    }

    re
  }

  atlas_right <- create_atlas(rh_pial_in_fsaverage)
  atlas_left <- create_atlas(lh_pial_in_fsaverage)
  #
  # s1 <- ieegio::as_ieegio_surface(
  #   t(pial_right$geometry$vertices),
  #   faces = t(pial_left$geometry$faces))
  # s2 <- atlas_right
  # merged <- merge(s1, s2)
  # plot(merged,
  #      method = "r3js",
  #      col = c("gray", viridis::viridis(256)))


  # ---- write as freesurfer curvature file to fsaverage directory ---------------
  file_ext <- ifelse(value_type == "numerical", "curv", "annot")
  file_type <- ifelse(value_type == "numerical", "measurements", "annotations")
  ieegio::write_surface(
    x = atlas_right,
    format = 'freesurfer',
    type = file_type,
    con = file.path(
      threeBrain::default_template_directory(),
      template_name,
      "label",
      sprintf("rh.rave_temporary.%s", file_ext)
    )
  )

  ieegio::write_surface(
    x = atlas_left,
    format = 'freesurfer',
    type = file_type,
    con = file.path(
      threeBrain::default_template_directory(),
      template_name,
      "label",
      sprintf("lh.rave_temporary.%s", file_ext)
    )
  )

  list(
    template_name = template_name,
    value_name = value_name,
    file_ext = file_ext,
    atlas_left = atlas_left,
    atlas_right = atlas_right
  )


}
