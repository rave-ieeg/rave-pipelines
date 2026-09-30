# agents/tools/rave_3dviewer.R
#
# Root-level MCP tools: read and change a module's live threeBrain 3D viewer
# through `threeBrain::brain_proxy()`, as a person does with the viewer's
# control panel. Nothing here is specific to one module: a module enables the
# tools in its agents.yaml and names its viewer's output ID in its manual and
# system prompt.
#
# To add a `name`, add an entry to `rave_3dviewer_getters` (with the `args`
# keys it takes) or `rave_3dviewer_setters` (with the `data` keys it takes),
# then describe it in the tool description below and in
# agents/tool-schema.yaml.

rave_3dviewer_quote <- function(x) {
  paste0("`", x, "`", collapse = ", ")
}

# `args` / `data`: a JSON object (a string), or a list decoded already
rave_3dviewer_object <- function(x, what, example) {
  if (is.null(x)) { return(list()) }
  if (is.character(x)) {
    x <- paste(x, collapse = "")
    if (!nzchar(trimws(x))) { return(list()) }
    x <- tryCatch(
      jsonlite::fromJSON(x, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                         simplifyMatrix = FALSE),
      error = function(e) {
        stop(sprintf("`%s` must be a JSON object, e.g. %s (%s)", what, example,
                     conditionMessage(e)), call. = FALSE)
      }
    )
  }
  if (!is.list(x) ||
      (length(x) && (is.null(names(x)) || !all(nzchar(names(x)))))) {
    stop(sprintf("`%s` must be a JSON object, e.g. %s", what, example),
         call. = FALSE)
  }
  x
}

rave_3dviewer_check_keys <- function(x, allowed, what, name) {
  unknown <- setdiff(names(x), allowed)
  if (length(unknown)) {
    stop(sprintf(
      "`%s` does not take %s %s. It takes %s.", name, what,
      rave_3dviewer_quote(unknown),
      if (length(allowed)) rave_3dviewer_quote(allowed) else "none"
    ), call. = FALSE)
  }
}

# Close matches for a mistyped name: names that contain it, then near ones
rave_3dviewer_suggest <- function(x, choices) {
  if (!length(choices)) { return(character(0)) }
  contains <- choices[grepl(x, choices, fixed = TRUE) |
                        grepl(tolower(x), tolower(choices), fixed = TRUE)]
  distance <- utils::adist(tolower(x), tolower(choices))[1, ]
  near <- choices[order(distance)][sort(distance) <= max(2, nchar(x) %/% 3)]
  utils::head(unique(c(contains, near)), 5)
}

rave_3dviewer_unknown_controller <- function(label, choices) {
  hint <- rave_3dviewer_suggest(label, choices)
  stop(sprintf(
    "Unknown controller `%s`.%s See `rave_3dviewer_get(name = \"controller_options\")` for every controller.",
    label,
    if (length(hint)) sprintf(" Did you mean %s?", rave_3dviewer_quote(hint)) else ""
  ), call. = FALSE)
}

rave_3dviewer_vec3 <- function(x, what, nonzero = FALSE) {
  v <- suppressWarnings(as.numeric(unlist(x)))
  if (length(v) != 3 || anyNA(v) || !all(is.finite(v)) ||
      (nonzero && all(v == 0))) {
    stop(sprintf("`%s` must be three finite numbers [x, y, z]%s.", what,
                 if (nonzero) ", not all zero" else ""), call. = FALSE)
  }
  v
}

# The viewer `outputId` of this module, and its proxy. A viewer counts as
# rendered once it has reported its controllers
rave_3dviewer_viewer <- function(session, outputId) {
  outputId <- trimws(paste(outputId, collapse = ""))
  prefix <- session$ns("")
  if (nzchar(prefix) && startsWith(outputId, prefix)) {
    outputId <- substring(outputId, nchar(prefix) + 1L)
  }
  inputs <- shiny::isolate(names(session$input))
  rendered <- sub("_controllers$", "", grep("_controllers$", inputs, value = TRUE))
  if (!isTRUE(outputId %in% rendered)) {
    if (!length(rendered)) {
      stop(paste(
        "No 3D viewer has rendered in this module yet: load the data (script",
        "`load_data`), wait until the viewer shows, then try again."
      ), call. = FALSE)
    }
    stop(sprintf(
      "No rendered 3D viewer `%s` in this module. Rendered viewers (outputId): %s.",
      outputId, rave_3dviewer_quote(rendered)
    ), call. = FALSE)
  }
  list(
    outputId = outputId,
    session = session,
    proxy = threeBrain::brain_proxy(outputId, session = session)
  )
}

rave_3dviewer_input <- function(viewer, name) {
  shiny::isolate(viewer$session$input[[paste0(viewer$outputId, "_", name)]])
}

# Controller specs (type, choices, range) sent by the viewer; empty with a
# threeBrain older than 1.3.0.62, which does not send them
rave_3dviewer_specs <- function(viewer) {
  if (!is.function(viewer$proxy$get_controller_specs)) { return(list()) }
  specs <- viewer$proxy$get_controller_specs()
  if (!is.list(specs)) { return(list()) }
  specs
}

rave_3dviewer_specs_note <- paste(
  "Choices and ranges were not checked: this viewer does not report its",
  "controller specs (threeBrain 1.3.0.62 or newer does)."
)

# Without specs, guess a controller's type from its current value
rave_3dviewer_guess_spec <- function(label, value) {
  type <- if (is.logical(value)) {
    "boolean"
  } else if (is.numeric(value)) {
    "number"
  } else if (is.character(value) && isTRUE(grepl("^#[0-9A-Fa-f]{6}$", value))) {
    "color"
  } else if (is.list(value)) {
    "interval"
  } else {
    "string"
  }
  list(name = label, type = type)
}

rave_3dviewer_names_filter <- function(x, args) {
  if (is.null(args$names)) { return(x) }
  wanted <- as.character(unlist(args$names))
  for (label in wanted) {
    if (!label %in% names(x)) {
      rave_3dviewer_unknown_controller(label, names(x))
    }
  }
  x[wanted]
}

# ---- rave_3dviewer_get ---------------------------------------------------------

rave_3dviewer_crosshair_spaces <- c("scanner", "tkrRAS", "MNI305", "MNI152", "CRS")
rave_3dviewer_slice_controllers <- c("Sagittal (L - R)", "Coronal (P - A)", "Axial (I - S)")

rave_3dviewer_has_slices <- function(viewer) {
  all(rave_3dviewer_slice_controllers %in% names(viewer$proxy$get_controllers()))
}

rave_3dviewer_no_slices <- function() {
  stop(paste(
    "This viewer has no slice panels, so it has no crosshair (its controllers",
    "have no `Sagittal (L - R)`, `Coronal (P - A)`, `Axial (I - S)`)."
  ), call. = FALSE)
}

rave_3dviewer_has_subject <- function(viewer) {
  length(viewer$proxy$isolate("current_subject")$subject_code) == 1
}

rave_3dviewer_event <- function(event) {
  if (!length(event)) { return(NULL) }
  object <- event$object
  vec <- function(x) {
    v <- suppressWarnings(as.numeric(unlist(x)))
    if (length(v) == 3 && !anyNA(v)) v else NULL
  }
  re <- list(
    name = event$name %||% object$name,
    type = event$type %||% object$type %||% event$geom_type,
    subject = event$subject %||% object$subject_code,
    electrode = event$electrode_number %||% object$number,
    hemisphere = object$hemisphere,
    tkrRAS = vec(event$position %||% event$tkr_ras),
    scanner = vec(object$scanner_position %||% event$scan_ras),
    MNI305 = vec(object$MNI305_position),
    MNI152 = vec(object$MNI152_position),
    displayed_data = event$current_clip,
    time = event$current_time,
    vertex_index = event$vertex_index,
    face_index = event$face_index,
    line_index = event$line_index,
    line_length = event$line_length,
    value = event$display,
    threshold = event$threshold,
    underlay = event$underlay
  )
  re[!vapply(re, is.null, FALSE)]
}

rave_3dviewer_getters <- list(

  controllers = list(
    args = "names",
    fun = function(viewer, args) {
      controllers <- viewer$proxy$get_controllers()
      list(
        outputId = viewer$outputId,
        controllers = rave_3dviewer_names_filter(controllers, args)
      )
    }
  ),

  controller_options = list(
    args = "names",
    fun = function(viewer, args) {
      specs <- rave_3dviewer_specs(viewer)
      note <- paste(
        "What `rave_3dviewer_set(name = \"controllers\")` accepts. `choices`",
        "are the values to send (`labels` are what the panel shows). Hidden",
        "controllers apply once a related one is set (e.g. voxel settings",
        "after `Voxel Type`)."
      )
      if (!length(specs)) {
        controllers <- viewer$proxy$get_controllers()
        specs <- structure(
          lapply(names(controllers), function(label) {
            rave_3dviewer_guess_spec(label, controllers[[label]])
          }),
          names = names(controllers)
        )
        note <- c(note, paste(
          "This viewer does not report controller specs (threeBrain 1.3.0.62",
          "or newer does): types are guessed from the values; choices and",
          "ranges are unknown."
        ))
      }
      specs <- specs[!vapply(specs, function(spec) {
        identical(spec$type, "function")
      }, FALSE)]
      options <- lapply(specs, function(spec) {
        entry <- list(type = spec$type, folder = spec$folder)
        if (identical(spec$type, "option")) {
          values <- spec$values %||% spec$choices
          entry$choices <- I(values)
          if (!identical(as.character(values), as.character(spec$choices))) {
            entry$labels <- I(spec$choices)
          }
        }
        for (key in c("min", "max", "step")) {
          entry[[key]] <- spec[[key]]
        }
        if (spec$type %in% c("interval", "linegraph")) { entry$settable <- FALSE }
        if (isTRUE(spec$hidden)) { entry$hidden <- TRUE }
        if (isTRUE(spec$disabled)) { entry$disabled <- TRUE }
        entry
      })
      list(
        outputId = viewer$outputId,
        controllers = rave_3dviewer_names_filter(options, args),
        note = note
      )
    }
  ),

  camera = list(
    args = character(0),
    fun = function(viewer, args) {
      if (is.null(rave_3dviewer_input(viewer, "main_camera"))) {
        return(list(outputId = viewer$outputId, note = paste(
          "The viewer has not reported its camera yet: it reports the camera",
          "only after a person drags or zooms it with the mouse."
        )))
      }
      camera <- viewer$proxy$isolate("main_camera")
      list(
        outputId = viewer$outputId,
        position = camera$position,
        up = camera$up,
        zoom = camera$zoom,
        target = unname(as.numeric(unlist(camera$target))),
        note = paste(
          "As reported when a person last dragged or zoomed the viewer with",
          "the mouse. Changes by `rave_3dviewer_set(name = \"camera\")`, by the",
          "`Camera Position` controller, or by the keyboard are not reported."
        )
      )
    }
  ),

  crosshair = list(
    args = "space",
    fun = function(viewer, args) {
      space <- args$space %||% "scanner"
      if (!isTRUE(space %in% c(rave_3dviewer_crosshair_spaces, "all"))) {
        stop(sprintf("`space` must be one of %s.", rave_3dviewer_quote(
          c(rave_3dviewer_crosshair_spaces, "all"))), call. = FALSE)
      }
      if (!rave_3dviewer_has_slices(viewer)) { rave_3dviewer_no_slices() }
      has_subject <- rave_3dviewer_has_subject(viewer)
      position_in <- function(space) {
        if (space == "tkrRAS") {
          return(unname(viewer$proxy$isolate("plane_position")))
        }
        unname(viewer$proxy$get_crosshair_position(space))
      }
      if (space == "all") {
        spaces <- rave_3dviewer_crosshair_spaces
        if (!has_subject) { spaces <- "tkrRAS" }
        re <- list(
          outputId = viewer$outputId,
          positions = structure(lapply(spaces, position_in), names = spaces),
          note = "CRS: voxel column, row, slice of the subject's MRI"
        )
        if (!has_subject) {
          re$note <- paste(
            "The viewer has not reported its subject yet: only tkrRAS is",
            "available (the other spaces need the subject's transforms)."
          )
        }
        return(re)
      }
      if (space != "tkrRAS" && !has_subject) {
        stop(sprintf(paste(
          "The viewer has not reported its subject yet, so the crosshair",
          "cannot be converted to `%s`; use `space` \"tkrRAS\" or try again",
          "once the viewer has loaded."
        ), space), call. = FALSE)
      }
      list(outputId = viewer$outputId, space = space, position = position_in(space))
    }
  ),

  selected = list(
    args = character(0),
    fun = function(viewer, args) {
      events <- list(
        click = rave_3dviewer_event(rave_3dviewer_input(viewer, "mouse_clicked")),
        double_click = rave_3dviewer_event(rave_3dviewer_input(viewer, "mouse_dblclicked")),
        focus = rave_3dviewer_event(rave_3dviewer_input(viewer, "mouse_focused"))
      )
      none <- names(events)[vapply(events, is.null, FALSE)]
      c(
        list(outputId = viewer$outputId),
        events,
        list(note = paste0(
          "The last object a person clicked, double-clicked, or focused (holding",
          " F and clicking: surfaces, slices, volumes, streamlines too).",
          if (length(none)) sprintf(" Not reported yet: %s.", paste(none, collapse = ", ")) else ""
        ))
      )
    }
  )
)

rave_3dviewer_get <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function(outputId, name, args = list()) {
        getter <- rave_3dviewer_getters[[paste(name, collapse = "")]]
        if (is.null(getter)) {
          stop(sprintf("Unknown `name` `%s`. Use one of %s.", paste(name, collapse = ""),
                       rave_3dviewer_quote(names(rave_3dviewer_getters))), call. = FALSE)
        }
        viewer <- rave_3dviewer_viewer(session, outputId)
        args <- rave_3dviewer_object(args, "args", "{\"space\": \"MNI152\"}")
        rave_3dviewer_check_keys(args, getter$args, "args", name)
        shiny::isolate(getter$fun(viewer, args))
      },
      name = "rave_3dviewer_get",
      description = paste(
        "Read the state of a module's live 3D brain viewer (threeBrain).",
        "`outputId` is the viewer's output ID, named in the module's manual",
        "(e.g. `viewer`); the viewer must be on the page and rendered.",
        "`name`: `controllers` gives the control panel's current values;",
        "`controller_options` gives what each controller accepts (type, and",
        "choices or range) for `rave_3dviewer_set`; `camera` gives the camera",
        "position, up and zoom as last reported after a mouse drag; `crosshair`",
        "gives the slice crosshair position; `selected` gives the last object",
        "clicked, double-clicked or focused (e.g. an electrode's number and",
        "coordinates). `args` (a JSON object): `controllers` and",
        "`controller_options` take {\"names\": [\"Surface Type\", ...]} to pick",
        "some; `crosshair` takes {\"space\": \"scanner\"} (default), \"tkrRAS\",",
        "\"MNI305\", \"MNI152\", \"CRS\" (voxel indices), or \"all\"."
      ),
      arguments = list(
        outputId = ellmer::type_string(
          description = paste(
            "The viewer's output ID in this module, without the module prefix",
            "(e.g. `viewer`)."
          )
        ),
        name = ellmer::type_enum(
          values = names(rave_3dviewer_getters),
          description = "What to read."
        ),
        args = ellmer::type_string(
          description = paste(
            "Optional JSON object of arguments for `name`, e.g.",
            "{\"space\": \"MNI152\"} for `crosshair`, or",
            "{\"names\": [\"Surface Type\"]} for `controllers`."
          ),
          required = FALSE
        )
      )
    )
  }

)

# ---- rave_3dviewer_set ---------------------------------------------------------

# Controllers that reset or depend on others go first (e.g. `Display Data`
# resets `Display Range`; `Voxel Type` re-ranges the voxel controllers)
rave_3dviewer_primary_controllers <- c(
  "Display Data", "Threshold Data", "Voxel Type", "Surface Color Data",
  "Surface Threshold Data", "Left Hemisphere", "Right Hemisphere",
  "View Layout", "Surface Type"
)

# String controllers that take coordinates "x, y, z"
rave_3dviewer_coordinate_controllers <- c(
  "Crosshair tkrRAS", "Crosshair ScanRAS", "Affine MNI152"
)

rave_3dviewer_refused_controllers <- c(
  "Record" = "starts a video recording that the browser downloads: ask the user to click it instead"
)

rave_3dviewer_color <- function(label, value) {
  bad <- function() {
    stop(sprintf(paste(
      "`%s` takes a color: \"#RRGGBB\" or an R color name such as \"black\"."
    ), label), call. = FALSE)
  }
  if (!is.character(value) || length(value) != 1 || is.na(value)) { bad() }
  if (grepl("^#[0-9A-Fa-f]{6}$", value)) { return(value) }
  if (grepl("^#[0-9A-Fa-f]{3}$", value)) {
    digits <- strsplit(substring(value, 2), "")[[1]]
    return(paste0("#", paste(rep(digits, each = 2), collapse = "")))
  }
  if (!tolower(value) %in% grDevices::colors()) { bad() }
  grDevices::rgb(t(grDevices::col2rgb(value)), maxColorValue = 255)
}

# The value to send for one controller, checked against its spec
rave_3dviewer_controller_value <- function(label, value, spec) {
  if (label %in% names(rave_3dviewer_refused_controllers)) {
    stop(sprintf("`%s` %s.", label, rave_3dviewer_refused_controllers[[label]]),
         call. = FALSE)
  }
  type <- spec$type %||% "string"
  if (type == "function") {
    stop(sprintf("`%s` is a button; it cannot be set.", label), call. = FALSE)
  }
  if (type %in% c("interval", "linegraph")) {
    stop(sprintf("`%s` cannot be set through the viewer tools (a %s controller).",
                 label, type), call. = FALSE)
  }
  if (isTRUE(spec$disabled)) {
    stop(sprintf("`%s` is disabled in the viewer right now.", label), call. = FALSE)
  }
  switch(
    type,
    "boolean" = {
      if (!is.logical(value) || length(value) != 1 || is.na(value)) {
        stop(sprintf("`%s` takes true or false.", label), call. = FALSE)
      }
      value
    },
    "number" = {
      if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
        stop(sprintf("`%s` takes a number.", label), call. = FALSE)
      }
      low <- spec$min
      high <- spec$max
      if ((length(low) == 1 && value < low) || (length(high) == 1 && value > high)) {
        stop(sprintf("`%s` must be between %s and %s.", label,
                     format(low %||% -Inf),
                     format(high %||% Inf)), call. = FALSE)
      }
      value
    },
    "option" = {
      values <- spec$values %||% spec$choices
      if (length(value) == 1 && !is.na(value)) {
        if (value %in% values) { return(value) }
        idx <- match(as.character(value), as.character(spec$choices))
        if (!is.na(idx)) { return(values[[idx]]) }
      }
      stop(sprintf("`%s` must be one of %s.", label, rave_3dviewer_quote(values)),
           call. = FALSE)
    },
    "color" = rave_3dviewer_color(label, value),
    "string" = {
      if (is.numeric(value) && length(value) && all(is.finite(value))) {
        if (label %in% rave_3dviewer_coordinate_controllers) {
          if (length(value) != 3) {
            stop(sprintf("`%s` takes three numbers [x, y, z].", label), call. = FALSE)
          }
          return(paste(value, collapse = ", "))
        }
        return(paste(value, collapse = ","))
      }
      if (!is.character(value) || length(value) != 1 || is.na(value)) {
        stop(sprintf("`%s` takes text (or numbers, e.g. [-5, 5] for a range).", label),
             call. = FALSE)
      }
      value
    },
    value
  )
}

rave_3dviewer_setters <- list(

  controllers = list(
    keys = NULL,  # any controller label
    fun = function(viewer, data) {
      if (!length(data)) {
        stop("`controllers` needs at least one controller, e.g. {\"Surface Type\": \"pial\"}.",
             call. = FALSE)
      }
      specs <- rave_3dviewer_specs(viewer)
      note <- character(0)
      if (!length(specs)) {
        controllers <- viewer$proxy$get_controllers()
        specs <- structure(
          lapply(names(controllers), function(label) {
            rave_3dviewer_guess_spec(label, controllers[[label]])
          }),
          names = names(controllers)
        )
        note <- c(note, rave_3dviewer_specs_note)
      }
      sent <- list()
      hidden <- character(0)
      for (label in names(data)) {
        spec <- specs[[label]]
        if (is.null(spec)) { rave_3dviewer_unknown_controller(label, names(specs)) }
        sent[[label]] <- rave_3dviewer_controller_value(label, data[[label]], spec)
        if (isTRUE(spec$hidden)) { hidden <- c(hidden, label) }
      }
      first <- intersect(rave_3dviewer_primary_controllers, names(sent))
      sent <- sent[c(first, setdiff(names(sent), first))]
      viewer$proxy$set_controllers(sent)
      if (length(hidden)) {
        note <- c(note, sprintf(paste(
          "%s %s hidden in the control panel right now: sent anyway; such",
          "controllers apply once a related one is set (e.g. voxel settings",
          "after `Voxel Type`)."
        ), rave_3dviewer_quote(hidden), if (length(hidden) == 1) "is" else "are"))
      }
      list(
        outputId = viewer$outputId,
        sent = sent,
        note = c(note, paste(
          "The viewer applies these after this call returns and reports its",
          "controllers within about 0.5 s: read them back with",
          "`rave_3dviewer_get(name = \"controllers\")`. (`shiny_query_ui`",
          "cannot show the 3D view: its pictures leave the brain blank.)"
        ))
      )
    }
  ),

  camera = list(
    keys = c("position", "up", "zoom"),
    fun = function(viewer, data) {
      if (!length(data)) {
        stop("`camera` needs at least one of `position`, `up`, `zoom`.", call. = FALSE)
      }
      if (!is.null(data$up) && is.null(data$position)) {
        stop("`up` needs `position` too: give both.", call. = FALSE)
      }
      sent <- list()
      if (!is.null(data$zoom)) {
        zoom <- data$zoom
        if (!is.numeric(zoom) || length(zoom) != 1 || !is.finite(zoom) ||
            zoom < 0.5 || zoom > 40) {
          stop("`zoom` must be a number between 0.5 and 40.", call. = FALSE)
        }
      }
      if (!is.null(data$position)) {
        position <- rave_3dviewer_vec3(data$position, "position", nonzero = TRUE)
        up <- c(0, 0, 1)
        if (!is.null(data$up)) {
          up <- rave_3dviewer_vec3(data$up, "up", nonzero = TRUE)
        }
        cosine <- abs(sum(position * up)) / sqrt(sum(position^2) * sum(up^2))
        if (cosine > 0.999) {
          stop(sprintf(paste(
            "`up` [%s] is parallel to `position`: give `up` perpendicular to",
            "the view direction, e.g. [0, 1, 0] for a view from above or below."
          ), paste(up, collapse = ", ")), call. = FALSE)
        }
        viewer$proxy$set_camera(position = position, up = up)
        sent$position <- position / sqrt(sum(position^2)) * 500
        sent$up <- up
      }
      if (!is.null(data$zoom)) {
        viewer$proxy$set_zoom_level(data$zoom)
        sent$zoom <- data$zoom
      }
      list(
        outputId = viewer$outputId,
        sent = sent,
        note = paste(
          "The viewer does not report camera changes made by this tool:",
          "`rave_3dviewer_get(name = \"camera\")` keeps the camera a person",
          "last dragged to, and a module that re-renders the viewer may restore",
          "that camera. Pictures from `shiny_query_ui` leave the 3D view blank,",
          "so tell the user what you changed and ask them to check the view."
        )
      )
    }
  ),

  crosshair = list(
    keys = c("position", "space"),
    fun = function(viewer, data) {
      position <- rave_3dviewer_vec3(data$position, "position")
      space <- data$space %||% "scanner"
      if (!isTRUE(space %in% rave_3dviewer_crosshair_spaces)) {
        stop(sprintf("`space` must be one of %s.",
                     rave_3dviewer_quote(rave_3dviewer_crosshair_spaces)), call. = FALSE)
      }
      if (!rave_3dviewer_has_slices(viewer)) { rave_3dviewer_no_slices() }
      if (space != "tkrRAS" && !rave_3dviewer_has_subject(viewer)) {
        stop(sprintf(paste(
          "The viewer has not reported its subject yet, so `%s` coordinates",
          "cannot be converted; use `space` \"tkrRAS\" or try again once the",
          "viewer has loaded."
        ), space), call. = FALSE)
      }
      viewer$proxy$set_crosshair_position(position, space = space)
      list(
        outputId = viewer$outputId,
        space = space,
        position = position,
        note = paste(
          "The viewer moves the crosshair after this call returns: read it",
          "back with `rave_3dviewer_get(name = \"crosshair\")`."
        )
      )
    }
  )
)

rave_3dviewer_set <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function(outputId, name, data) {
        setter <- rave_3dviewer_setters[[paste(name, collapse = "")]]
        if (is.null(setter)) {
          stop(sprintf("Unknown `name` `%s`. Use one of %s.", paste(name, collapse = ""),
                       rave_3dviewer_quote(names(rave_3dviewer_setters))), call. = FALSE)
        }
        viewer <- rave_3dviewer_viewer(session, outputId)
        data <- rave_3dviewer_object(data, "data", "{\"Surface Type\": \"pial\"}")
        if (!is.null(setter$keys)) {
          rave_3dviewer_check_keys(data, setter$keys, "data", name)
        }
        shiny::isolate(setter$fun(viewer, data))
      },
      name = "rave_3dviewer_set",
      description = paste(
        "Change a module's live 3D brain viewer (threeBrain), as a person does",
        "with its control panel. `outputId` is the viewer's output ID, named in",
        "the module's manual (e.g. `viewer`). `data` is a JSON object for `name`:",
        "`controllers`: {\"<controller>\": value, ...}, e.g. {\"Surface Type\":",
        "\"pial\", \"Left Opacity\": 0.4, \"Background Color\": \"#000000\"};",
        "each value is checked against the controller's type and its choices or",
        "range (see `rave_3dviewer_get(name = \"controller_options\")`), and",
        "several are applied in one call, in the order the viewer needs.",
        "Standard views: {\"Camera Position\": \"left\"} (also right, anterior,",
        "posterior, superior, inferior). `camera`: {\"position\": [x, y, z],",
        "\"up\": [x, y, z], \"zoom\": 1.5}; position is the direction from the",
        "brain center to the camera, zoom is 0.5 to 40; the viewer does not",
        "report this back. `crosshair`: {\"position\": [x, y, z], \"space\":",
        "\"scanner\"} moves the slice crosshair; space is \"scanner\" (default),",
        "\"tkrRAS\", \"MNI305\", \"MNI152\", or \"CRS\". Nothing is saved, and a",
        "module that re-renders its viewer may reset it. Check the result with",
        "`rave_3dviewer_get` (pictures from `shiny_query_ui` leave the 3D view",
        "blank)."
      ),
      arguments = list(
        outputId = ellmer::type_string(
          description = paste(
            "The viewer's output ID in this module, without the module prefix",
            "(e.g. `viewer`)."
          )
        ),
        name = ellmer::type_enum(
          values = names(rave_3dviewer_setters),
          description = "What to change."
        ),
        data = ellmer::type_string(
          description = paste(
            "JSON object for `name`, e.g. {\"Surface Type\": \"pial\"} for",
            "`controllers`, {\"zoom\": 2} for `camera`, or",
            "{\"position\": [-40, 10, 20], \"space\": \"MNI152\"} for `crosshair`."
          )
        )
      )
    )
  }

)
