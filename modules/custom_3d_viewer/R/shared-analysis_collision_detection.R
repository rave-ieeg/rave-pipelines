group_analysis_objects_for_streamline_collision_detection <- function(objects, loaded_brain_info) {
  # DIPSAUS DEBUG START
  # loaded_brain_info <- pipeline["loaded_brain_info"]
  # analysis_objects <- pipeline['analysis_objects']
  # objects <- resolve_analysis_object(brain = loaded_brain_info$brain, analysis_objects)
  roi_list <- list()
  streamline_list <- list()

  brain <- loaded_brain_info$brain

  for(object in objects) {
    # object <- objects[[2]]
    switch(
      object$type,
      "electrode" = {
        tkr_ras <- c(
          object$object$Coord_x,
          object$object$Coord_y,
          object$object$Coord_z
        )
        scanner_ras <- brain$electrodes$apply_transform_points(matrix(tkr_ras, ncol = 3), "tkrRAS", "scannerRAS")
        roi_list[[length(roi_list) + 1]] <- ieegio::as_ieegio_roi(scanner_ras)
      },
      "streamlines" = {
        streamline_list[names(object$object)] <- lapply(object$object, ieegio::as_ieegio_roi)
      },
      {
        roi_list[[length(roi_list) + 1]] <- ieegio::as_ieegio_roi(object$object)
      }
    )
  }
  if (!length(roi_list)) {
    stop("No ROI volume/surface/electrode found. Analysis must have at least one ROI object and one streamline bundle.")
  }
  if (!length(streamline_list)) {
    stop("No streamlines found. Analysis must have at least one ROI object and one streamline bundle.")
  }

  list(
    roi_list = roi_list,
    streamline_list = streamline_list
  )
}


streamline_collision_detection_analyzer <- create_analysis(
  name = "streamline_collision_detection",
  description = "Streamline collision detection"
)

streamline_collision_detection_analyzer$set_input_ui(
  input_name = "mode_x",
  ui_func = function(inputId, restored_inputs = list()) {
    shiny::selectInput(
      inputId = inputId,
      label = "Mode for ROI (volume/surface/electrode) objects",
      choices = c("auto", "volume", "pointcloud", "surface"),
      selected = restored_inputs$mode_x %||% "auto"
    )
  }
)

streamline_collision_detection_analyzer$set_input_ui(
  input_name = "radius",
  ui_func = function(inputId, restored_inputs = list()) {
    shiny::numericInput(
      inputId = inputId,
      label = "Radius (mm)",
      value = restored_inputs$radius %||% 0,
      min = 0,
      step = 0.1
    )
  }
)

streamline_collision_detection_analyzer$set_collect_inputs_from_pipeline(
  collect_func = function(pipeline_settings) {
    inputs <- as.list(pipeline_settings[[streamline_collision_detection_analyzer$inputs_settings_name]])
    inputs$objects <- pipeline_settings$analysis_objects
    inputs
  }
)

# streamline_collision_detection_analyzer$`@collect_inputs_from_pipeline`(pipeline)


# inputs <- streamline_collision_detection_analyzer$`@collect_inputs_from_shiny`(shiny::MockShinySession$new())

streamline_collision_detection_analyzer$set_store_inputs_to_pipeline(
  store_func = function(inputs, pipeline) {
    pipeline$set_settings(
      analysis_objects = unname(inputs$objects)
    )
    inputs$objects <- NULL
    # analyzer class will handle the rest of inputs
    return(inputs)
  }
)

streamline_collision_detection_analyzer$set_preprocess(
  pipeline_targets = c("loaded_brain_info"),
  preprocess_func = function(value, pipeline_targets) {
  
    loaded_brain_info <- pipeline_targets$loaded_brain_info
    # TODO: check loaded_brain_info
    objects <- value$objects
    types <- vapply(
      objects,
      function(object) {
        paste(object$type, collapse = "")
      },
      FUN.VALUE = ""
    )
    types <- types[nzchar(types)]
    if (!length(types)) {
      stop("No object selected. Analysis must have at least one ROI object (electrode, surface, or volume) and one streamline bundle.")
    }
    if (!any(types != "streamlines")) {
      stop("No ROI volume/surface/electrode found. Analysis must have at least one ROI object and one streamline bundle.")
    }
    if (!any(types == "streamlines")) {
      stop("No streamlines found. Analysis must have at least one ROI object and one streamline bundle.")
    }

    # resolve objects
    objects <- resolve_analysis_object(
      brain = loaded_brain_info$brain,
      analysis_objects = objects
    )
    

    invisible(list(
      objects = objects,

      # value is list of radius and mode_x
      params = as.list(value),
      loaded_brain_info = loaded_brain_info
    ))
  }
)

streamline_collision_detection_analyzer$set_analyze(
  analyze_func = function(value, options = list()) {
    objects <- value$objects

    params <- as.list(value$params)
    if (length(options)) {
      params[names(options)] <- options
    }
      
    
    loaded_brain_info <- value$loaded_brain_info

    mode_x <- params$mode_x %||% "auto"
    radius <- params$radius %||% "0"
    include_interior <- params$include_interior %||% TRUE
    early_stop <- params$early_stop %||% FALSE

    sorted <- group_analysis_objects_for_streamline_collision_detection(
      objects,
      loaded_brain_info = loaded_brain_info
    )
    rois <- sorted$roi_list
    streamlines <- sorted$streamline_list
    streamline_names <- names(sorted$streamline_list)
    rm(sorted)

    # bind all streamlines together to speed up
    # using ieegio:::length.ieegio_streamlines
    line_counts <- sapply(streamlines, length)
    cumsum_line_counts <- cumsum(c(0, line_counts))
    lines <- fastmap::fastqueue()
    lapply(streamlines, function(line) {
      lines$madd(.list = line[])
      return()
    })
    lines <- lines$as_list()
    overlapped <- rep(TRUE, sum(line_counts))

    for (roi in rois) {
      streamlines <- ieegio::as_ieegio_streamlines(lines[overlapped])
      result <- ieegio::detect_roi_overlap(
        x = roi,
        y = streamlines,
        mode_x = mode_x,
        mode_y = "streamlines",
        radius = radius,
        early_stop = early_stop,
        include_interior = include_interior
      )
      overlapped2 <- sapply(result$annotated$data, function(data) {
        data$properties[["Overlap"]]
      })
      sel <- overlapped2 == 0
      if (any(sel)) {
        overlapped[which(overlapped)[sel]] <- FALSE
      }
      if (!any(overlapped)) {
        break
      }
    }

    percentage <- sapply(seq_len(length(cumsum_line_counts) - 1), function(ii) {
      idx <- seq.int(cumsum_line_counts[[ii]] + 1, cumsum_line_counts[[ii + 1]])
      mean(overlapped[idx])
    })

    list(
      collision = data.frame(
        streamline = streamline_names,
        hit_ratio = percentage,
        line_count = line_counts,
        row.names = NULL
      )
    )
  }
)


streamline_collision_detection_analyzer$set_visualize(
  visualize_func = function(value, options = list()) {
    collision_table <- value$collision
    # re <- pipeline[streamline_collision_detection_analyzer$results_target_name]
    # value <- re$results
    re <- DT::datatable(
      collision_table,
      colnames = c(
        "Streamline" = "streamline",
        "Overlap (%)" = "hit_ratio",
        "# of total lines" = "line_count"
      )
    )
    # `columns` refers to the displayed (renamed) column names
    DT::formatPercentage(re, columns = "Overlap (%)", digits = 0)
    # print(collision_table)
  }
)

# Test
# ravepipeline::pipeline_setup_rmd("custom_3d_viewer")
# streamline_collision_detection_analyzer$`@render_input`("radius", pipeline = pipeline)
# inputs <- streamline_collision_detection_analyzer$`@collect_inputs_from_pipeline`(pipeline)
# pipeline$get_settings("analysis_objects")
# value <- streamline_collision_detection_analyzer$`@preprocess_data`(value = inputs, pipeline_targets = pipeline[streamline_collision_detection_analyzer$pipeline_targets, simplify = FALSE])
# value <- streamline_collision_detection_analyzer$`@analyze_data`(value)
# streamline_collision_detection_analyzer$`@visualize_data`(value)
