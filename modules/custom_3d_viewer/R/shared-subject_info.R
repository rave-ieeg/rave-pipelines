
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

  surfaces <- c("smoothwm", "inflated", "white", "pial-outer-smoothed", brain$available_surfaces)
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

# This is an UI-side function
parse_object_selector <- function(
  object_type = c("Electrode", "Mesh surface", "3D volume", "Streamlines"),
  proxy, loaded_brain_info, 
  object_selector_electrode = NULL,
  object_selector_surface = NULL,
  object_selector_volume = NULL,
  object_selector_streamlines = NULL
) {
  # object_type <- paste(input$object_selector, collapse = "")
  # controllers <- proxy$controllers
  # loaded_brain_info <- component_container$data$loaded_brain_info
  object_type <- match.arg(object_type)

  if (missing(loaded_brain_info)) {
    loaded_brain_info <- pipeline$read("loaded_brain_info")
  }

  controllers <- proxy$controllers
  mouse_event_double_click <- as.list(proxy$mouse_event_double_click)

  switch (
    object_type,
    "Electrode" = {
      electrode <- paste(object_selector_electrode, collapse = "")
      if (!is.na(electrode) && nzchar(electrode)) {
        electrode <- as.integer(electrode)
      } else {
        electrode <- NA
      }
      if (is.na(electrode)) {
        info <- mouse_event_double_click
        # Check highlighted electrode
        if (!isTRUE(info$is_electrode)) {
          return(simpleError("Please click on an electrode from the viewer"))
        }
        electrode <- as.integer(info$electrode_number)
      }
      electrode_table <- loaded_brain_info$electrode_table
      info <- electrode_table[electrode_table$Electrode == electrode, ]
      if (nrow(info) == 0) {
        return(simpleError("Cannot find electrode information from the coordinate table"))
      } else {
        info <- info[1, ]
      }
      return(list(
        type = "electrode",
        format = sprintf("Electrode %s [ch %s]", info$Label, info$Electrode),
        names = info$Label,
        channel = info$Electrode
      ))
    },
    "Mesh surface" = {
      surface_name <- paste(object_selector_surface, collapse = "")
      if (!nzchar(surface_name)) {
        return(simpleError("No surface selected"))
      }
      if (endsWith(surface_name, "[lh]")) {
        hemisphere <- "left"
      } else {
        hemisphere <- "right"
      }

      surface_name <- gsub(" \\[[lr]h\\]$", "", surface_name)

      if (!isTRUE(surface_name %in% loaded_brain_info$brain$surface_types)) {
        return(simpleError(sprintf("Unknown surface selected: %s", surface_name)))
      }

      return(list(
        type = "surface",
        format = sprintf("Surface %s [%s hemisphere]", surface_name, hemisphere),
        names = surface_name,
        hemisphere = hemisphere
      ))

      # surfaces <- unlist(lapply(loaded_brain_info$brain$surface_types, function(surface_type) {
      #   sprintf(c("%s [lh]", "%s [rh]"), surface_type)
      # }))
    },
    "3D volume" = {
      volume_name <- paste(object_selector_volume, collapse = "")
      if (volume_name %in% c("[Current active overlay]", "")) {
        volume_name <- controllers[["Voxel Type"]]
      }
      brain <- loaded_brain_info$brain
      volume <- brain$atlases[brain$atlas_types$name %in% volume_name]
      if (!length(volume)) {
        return(simpleError("No overlay is active"))
      }
      volume <- volume[[1]]

      if (volume$object$color_format == "RGBAFormat") {
        data_type <- "categorical annotation"
      } else {
        data_type <- "continuous measurement"
      }

      return(list(
        type = "volume",
        format = sprintf("Overlay %s [%s]", volume$atlas_type, data_type),
        names = volume$atlas_type
      ))
    },
    "Streamlines" = {
      all_streamline_names <- loaded_brain_info$brain$streamline_types
      streamline_names <- paste(object_selector_streamlines, collapse = "")
      if (streamline_names %in% c("[Current active streamlines]", "")) {
        # Check actives
        streamline_names <- all_streamline_names[vapply(all_streamline_names, function(name) {
          isTRUE(controllers[[sprintf("Show: %s", name)]])
        }, FUN.VALUE = FALSE)]
      } else if (endsWith(streamline_names, "*")) {
        streamline_names <- gsub("*", "", streamline_names, fixed = TRUE)
        streamline_names <- all_streamline_names[startsWith(all_streamline_names, streamline_names)]
      }
      return(list(
        type = "streamlines",
        format = sprintf(
          "Streamlines [n=%d]: %s",
          length(streamline_names),
          utils::capture.output(utils::str(streamline_names, give.head = FALSE))
        ),
        names = streamline_names
      ))
    }
  )
}

# do.call(resolve_object_by_info, info)
resolve_object_by_info <- function(
  brain, type = c("volume", "surface", "electrode", "streamlines"),
  names, ...
) {

  # DIPSAUS DEBUG START
  # brain <- ravecore::rave_brain("YAEL/CIT168", streamlines = "alic/*")
  type <- match.arg(type)
  if (!is.character(names) || !length(names)) {
    return(list())
  }
  args <- list(...)
  switch(
    type,
    "volume" = {
      # return(list(
      #   type = "volume",
      #   format = sprintf("Overlay %s [%s]", volume$atlas_type, data_type),
      #   names = volume$atlas_type
      # ))
      # names <- "aparc_a2009s_aseg"
      names <- names[names %in% brain$atlas_types$name]
      volumes <- brain$atlases[names]
      
      return(
        structure(
          names = names,
          lapply(volumes, function(volume) {
            # volume$object$color_format
            path <- volume$group$group_data$volume_data$absolute_path
            ieegio::as_ieegio_volume(path)
          })
        )
      )
    },
    "electrode" = {
      coord_table <- brain$electrodes$raw_table

      if (length(args$channel)) {
        rows <- coord_table[coord_table$Electrode %in% args$channel, , drop = FALSE]
      } else {
        rows <- coord_table[coord_table$Label %in% names, , drop = FALSE]
      }
      
      return(rows)
    },
    "surface" = {
      # Surface names length is always 1
      # names <- "pial"
      surface_name <- names[1]
      surface_group <- brain$surfaces[[surface_name]]
      if (!length(surface_group)) {
        return(list())
      }
      hemisphere <- args$hemisphere %||% "left"
      surface <- switch(
        hemisphere,
        "left" = {
          data_name <- sprintf("free_vertices_FreeSurfer Left Hemisphere - pial (%s)", brain$subject_code)
          path <- surface_group$group$group_data[[data_name]]$absolute_path
          ieegio::as_ieegio_surface(path)
        },
        "right" = {
          data_name <- sprintf("free_vertices_FreeSurfer Right Hemisphere - pial (%s)", brain$subject_code)
          path <- surface_group$group$group_data[[data_name]]$absolute_path
          ieegio::as_ieegio_surface(path)
        },
        stop("Invalid hemisphere: ", hemisphere)
      )

      return(surface)
    },
    "streamlines" = {
      # Names are explicitly specified, rather than wildcard matching
      names <- names[names %in% brain$streamline_types]
      streamlines <- structure(
        names = names,
        lapply(names, function(name) {
          # name <- names[[1]]
          streamline <- brain$streamlines[[name]]
          paths <- streamline$group$group_data[[streamline$object$data_key]]
          ieegio::as_ieegio_streamlines(paths$absolute_path)
        })
      )
      return(streamlines)
    }
  )
}


resolve_analysis_object <- function(brain, analysis_objects) {
  lapply(analysis_objects, function(info) {
    info$brain <- brain
    info$object <- do.call(resolve_object_by_info, info)
    info$brain <- NULL
    info
  })
}
