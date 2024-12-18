library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
._._env_._.$pipeline <- pipeline_from_path(".")
lapply(sort(list.files(
  "R/", ignore.case = TRUE,
  pattern = "^shared-.*\\.R", 
  full.names = TRUE
)), function(f) {
  source(f, local = ._._env_._., chdir = TRUE)
})
targets::tar_option_set(envir = ._._env_._.)
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_selected_template = targets::tar_target_raw("selected_template", 
        quote({
            settings[["selected_template"]]
        }), deps = "settings"), input_shiny_outputId = targets::tar_target_raw("shiny_outputId", 
        quote({
            settings[["shiny_outputId"]]
        }), deps = "settings"), input_main_camera = targets::tar_target_raw("main_camera", 
        quote({
            settings[["main_camera"]]
        }), deps = "settings"), input_controllers = targets::tar_target_raw("controllers", 
        quote({
            settings[["controllers"]]
        }), deps = "settings"), input_uploaded_source = targets::tar_target_raw("uploaded_source", 
        quote({
            settings[["uploaded_source"]]
        }), deps = "settings"), input_use_template = targets::tar_target_raw("use_template", 
        quote({
            settings[["use_template"]]
        }), deps = "settings"), input_surface_types = targets::tar_target_raw("surface_types", 
        quote({
            settings[["surface_types"]]
        }), deps = "settings"), input_baseline_method = targets::tar_target_raw("baseline_method", 
        quote({
            settings[["baseline_method"]]
        }), deps = "settings"), input_baseline = targets::tar_target_raw("baseline", 
        quote({
            settings[["baseline"]]
        }), deps = "settings"), input_type = targets::tar_target_raw("type", 
        quote({
            settings[["type"]]
        }), deps = "settings"), input_intervals = targets::tar_target_raw("intervals", 
        quote({
            settings[["intervals"]]
        }), deps = "settings"), input_reference = targets::tar_target_raw("reference", 
        quote({
            settings[["reference"]]
        }), deps = "settings"), input_electrodes = targets::tar_target_raw("electrodes", 
        quote({
            settings[["electrodes"]]
        }), deps = "settings"), input_epoch = targets::tar_target_raw("epoch", 
        quote({
            settings[["epoch"]]
        }), deps = "settings"), input_save_pipeline = targets::tar_target_raw("save_pipeline", 
        quote({
            settings[["save_pipeline"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_path_to_electrodes = targets::tar_target_raw("path_to_electrodes", 
        quote({
            settings[["path_to_electrodes"]]
        }), deps = "settings"), input_path_to_connectivity_matrix = targets::tar_target_raw("path_to_connectivity_matrix", 
        quote({
            settings[["path_to_connectivity_matrix"]]
        }), deps = "settings"), get_template_details = targets::tar_target_raw(name = "template_details", 
        command = quote({
            .__target_expr__. <- quote({
                template_details <- list(selected_template = selected_template, 
                  selected_surfaces = surface_types)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(template_details)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "template_details", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "template_details", target_expr = quote({
                {
                  template_details <- list(selected_template = selected_template, 
                    selected_surfaces = surface_types)
                }
                template_details
            }), target_depends = c("selected_template", "surface_types"
            )), deps = c("selected_template", "surface_types"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), render_initial_viewer = targets::tar_target_raw(name = "initial_brain_widget", 
        command = quote({
            .__target_expr__. <- quote({
                initial_brain_widget = 1
                return()
                library(dipsaus)
                force(shiny_outputId)
                controllers <- as.list(controllers)
                main_camera <- as.list(main_camera)
                background <- controllers[["Background Color"]]
                if (length(background) != 1) {
                  background <- "#FFFFFF"
                }
                zoom_level <- main_camera$zoom
                if (length(zoom_level) != 1 || zoom_level <= 
                  0) {
                  zoom_level <- 1
                }
                position <- as.numeric(unname(unlist(main_camera$position)))
                up <- as.numeric(unname(unlist(main_camera$up)))
                if (length(position) != 3 || length(up) != 3 || 
                  all(position == 0) || all(up == 0) || any(is.na(position)) || 
                  any(is.na(up))) {
                  position <- c(0, 0, 500)
                  up <- c(0, 1, 0)
                } else {
                  position <- position/sqrt(sum(position^2)) * 
                    500
                  up <- up/sqrt(sum(up^2))
                }
                if (!isTRUE(controllers[["Show Panels"]])) {
                  controllers[["Show Panels"]] <- FALSE
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(initial_brain_widget)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "initial_brain_widget", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "initial_brain_widget", target_expr = quote({
                {
                  initial_brain_widget = 1
                  return()
                  library(dipsaus)
                  force(shiny_outputId)
                  controllers <- as.list(controllers)
                  main_camera <- as.list(main_camera)
                  background <- controllers[["Background Color"]]
                  if (length(background) != 1) {
                    background <- "#FFFFFF"
                  }
                  zoom_level <- main_camera$zoom
                  if (length(zoom_level) != 1 || zoom_level <= 
                    0) {
                    zoom_level <- 1
                  }
                  position <- as.numeric(unname(unlist(main_camera$position)))
                  up <- as.numeric(unname(unlist(main_camera$up)))
                  if (length(position) != 3 || length(up) != 
                    3 || all(position == 0) || all(up == 0) || 
                    any(is.na(position)) || any(is.na(up))) {
                    position <- c(0, 0, 500)
                    up <- c(0, 1, 0)
                  } else {
                    position <- position/sqrt(sum(position^2)) * 
                      500
                    up <- up/sqrt(sum(up^2))
                  }
                  if (!isTRUE(controllers[["Show Panels"]])) {
                    controllers[["Show Panels"]] <- FALSE
                  }
                }
                initial_brain_widget
            }), target_depends = c("shiny_outputId", "controllers", 
            "main_camera")), deps = c("shiny_outputId", "controllers", 
        "main_camera"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), build_viewer_with_electrodes = targets::tar_target_raw(name = "template_brain_with_electrodes", 
        command = quote({
            .__target_expr__. <- quote({
                template_brain_with_electrodes = 1
                return()
            })
            tryCatch({
                eval(.__target_expr__.)
                return(template_brain_with_electrodes)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "template_brain_with_electrodes", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "template_brain_with_electrodes", 
            target_expr = quote({
                {
                  template_brain_with_electrodes = 1
                  return()
                }
                template_brain_with_electrodes
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    render_viewer_with_electrodes = targets::tar_target_raw(name = "brain_widget_electrodes_only", 
        command = quote({
            .__target_expr__. <- quote({
                brain_widget_electrodes_only = 1
                return()
            })
            tryCatch({
                eval(.__target_expr__.)
                return(brain_widget_electrodes_only)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "brain_widget_electrodes_only", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "brain_widget_electrodes_only", target_expr = quote({
                {
                  brain_widget_electrodes_only = 1
                  return()
                }
                brain_widget_electrodes_only
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    render_viewer_with_connections = targets::tar_target_raw(name = "brain_widget_with_data", 
        command = quote({
            .__target_expr__. <- quote({
                brain_widget_with_data = 1
                library(dipsaus)
                force(shiny_outputId)
                controllers <- as.list(controllers)
                main_camera <- as.list(main_camera)
                background <- controllers[["Background Color"]]
                if (length(background) != 1) {
                  background <- "#FFFFFF"
                }
                zoom_level <- main_camera$zoom
                if (length(zoom_level) != 1 || zoom_level <= 
                  0) {
                  zoom_level <- 1
                }
                position <- as.numeric(unname(unlist(main_camera$position)))
                up <- as.numeric(unname(unlist(main_camera$up)))
                if (length(position) != 3 || length(up) != 3 || 
                  all(position == 0) || all(up == 0) || any(is.na(position)) || 
                  any(is.na(up))) {
                  position <- c(0, 0, 500)
                  up <- c(0, 1, 0)
                } else {
                  position <- position/sqrt(sum(position^2)) * 
                    500
                  up <- up/sqrt(sum(up^2))
                }
                if (!isTRUE(controllers[["Show Panels"]])) {
                  controllers[["Show Panels"]] <- FALSE
                }
                if (is.null(path_to_connectivity_matrix) || !file.exists(path_to_connectivity_matrix)) {
                  warning("No electrode table is available")
                  brain_widget_with_data <- NULL
                } else {
                  cmat <- read.csv(path_to_connectivity_matrix, 
                    header = TRUE)
                  df <- data.frame(SubjectCode = loaded_brain$template_subject, 
                    Electrode = template_brain_with_electrodes$electrodes$raw_table$Electrode)
                  df <- cbind(df, cmat)
                  template_brain_with_electrodes$set_electrode_values(df)
                  for (ri in 1:nrow(df)) {
                    varname <- (colnames(df)[-(1:2)])[ri]
                    template_brain_with_electrodes$electrodes$fix_electrode_color(ri, 
                      "black", varname)
                  }
                  brain_widget_with_data <- template_brain_with_electrodes$plot(show_modal = FALSE, 
                    val_ranges = sapply(colnames(cmat), function(nm) c(-1, 
                      1), simplify = FALSE), background = background, 
                    controllers = controllers, start_zoom = zoom_level, 
                    custom_javascript = raveio::glue("\n    // Remove the focus box\n    if( canvas.focus_box ) {\n      canvas.focus_box.visible = false;\n    }\n    \n    // set camera\n    canvas.mainCamera.position.set(\n      {{ position[[1]] }} , \n      {{ position[[2]] }} , \n      {{ position[[3]] }}\n    );\n    canvas.mainCamera.up.set(\n      {{ up[[1]] }} , \n      {{ up[[2]] }} , \n      {{ up[[3]] }}\n    )\n    canvas.mainCamera.updateProjectionMatrix();\n\n    // Let shiny know the viewer is ready\n    if( window.Shiny ) {\n       window.Shiny.setInputValue(\"{{ shiny_outputId }}\", \"{{Sys.time()}}\");\n    }\n\n    // Force render one frame (update the canvas)\n    canvas.needsUpdate = true;\n    ", 
                      .open = "{{", .close = "}}"))
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(brain_widget_with_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "brain_widget_with_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "brain_widget_with_data", target_expr = quote({
                {
                  brain_widget_with_data = 1
                  library(dipsaus)
                  force(shiny_outputId)
                  controllers <- as.list(controllers)
                  main_camera <- as.list(main_camera)
                  background <- controllers[["Background Color"]]
                  if (length(background) != 1) {
                    background <- "#FFFFFF"
                  }
                  zoom_level <- main_camera$zoom
                  if (length(zoom_level) != 1 || zoom_level <= 
                    0) {
                    zoom_level <- 1
                  }
                  position <- as.numeric(unname(unlist(main_camera$position)))
                  up <- as.numeric(unname(unlist(main_camera$up)))
                  if (length(position) != 3 || length(up) != 
                    3 || all(position == 0) || all(up == 0) || 
                    any(is.na(position)) || any(is.na(up))) {
                    position <- c(0, 0, 500)
                    up <- c(0, 1, 0)
                  } else {
                    position <- position/sqrt(sum(position^2)) * 
                      500
                    up <- up/sqrt(sum(up^2))
                  }
                  if (!isTRUE(controllers[["Show Panels"]])) {
                    controllers[["Show Panels"]] <- FALSE
                  }
                  if (is.null(path_to_connectivity_matrix) || 
                    !file.exists(path_to_connectivity_matrix)) {
                    warning("No electrode table is available")
                    brain_widget_with_data <- NULL
                  } else {
                    cmat <- read.csv(path_to_connectivity_matrix, 
                      header = TRUE)
                    df <- data.frame(SubjectCode = loaded_brain$template_subject, 
                      Electrode = template_brain_with_electrodes$electrodes$raw_table$Electrode)
                    df <- cbind(df, cmat)
                    template_brain_with_electrodes$set_electrode_values(df)
                    for (ri in 1:nrow(df)) {
                      varname <- (colnames(df)[-(1:2)])[ri]
                      template_brain_with_electrodes$electrodes$fix_electrode_color(ri, 
                        "black", varname)
                    }
                    brain_widget_with_data <- template_brain_with_electrodes$plot(show_modal = FALSE, 
                      val_ranges = sapply(colnames(cmat), function(nm) c(-1, 
                        1), simplify = FALSE), background = background, 
                      controllers = controllers, start_zoom = zoom_level, 
                      custom_javascript = raveio::glue("\n    // Remove the focus box\n    if( canvas.focus_box ) {\n      canvas.focus_box.visible = false;\n    }\n    \n    // set camera\n    canvas.mainCamera.position.set(\n      {{ position[[1]] }} , \n      {{ position[[2]] }} , \n      {{ position[[3]] }}\n    );\n    canvas.mainCamera.up.set(\n      {{ up[[1]] }} , \n      {{ up[[2]] }} , \n      {{ up[[3]] }}\n    )\n    canvas.mainCamera.updateProjectionMatrix();\n\n    // Let shiny know the viewer is ready\n    if( window.Shiny ) {\n       window.Shiny.setInputValue(\"{{ shiny_outputId }}\", \"{{Sys.time()}}\");\n    }\n\n    // Force render one frame (update the canvas)\n    canvas.needsUpdate = true;\n    ", 
                        .open = "{{", .close = "}}"))
                  }
                }
                brain_widget_with_data
            }), target_depends = c("shiny_outputId", "controllers", 
            "main_camera", "path_to_connectivity_matrix", "template_brain_with_electrodes"
            )), deps = c("shiny_outputId", "controllers", "main_camera", 
        "path_to_connectivity_matrix", "template_brain_with_electrodes"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
