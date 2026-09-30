require(testthat)

# Unit tests for the root-level agent tools in `agents/tools`: what a script
# prints reaches the agent, and `shiny_ui_operate` sends the right messages to
# the browser. No app or browser is needed; modules/*/test-mcp.R drive a live
# app end to end.
source(testthat::test_path(
  "..", "..", "agents", "tools", "module_interactive_script.R"
), local = TRUE)
source(testthat::test_path(
  "..", "..", "agents", "tools", "shiny_ui_operate.R"
), local = TRUE)
source(testthat::test_path(
  "..", "..", "agents", "tools", "rave_3dviewer.R"
), local = TRUE)

# ---- capture_script_output --------------------------------------------------

test_that("capture_script_output returns the value and what the script printed", {
  captured <- capture_script_output({
    cat("to stdout\n")
    message("to message")
    cat("to stderr\n", file = stderr())
    cli::cli_alert_success("kernels completed")
    42
  })
  expect_equal(captured$value, 42)
  expect_null(captured$error)
  expect_match(captured$output, "to stdout")
  expect_match(captured$output, "to message")
  expect_match(captured$output, "to stderr")
  expect_match(captured$output, "kernels completed")
})

test_that("capture_script_output keeps the error and the output before it", {
  captured <- capture_script_output({
    message("before the error")
    stop("boom")
  })
  expect_s3_class(captured$error, "error")
  expect_equal(conditionMessage(captured$error), "boom")
  expect_null(captured$value)
  expect_match(captured$output, "before the error")
})

test_that("capture_script_output records warnings", {
  captured <- suppressWarnings(capture_script_output({
    warning("careful")
    1
  }))
  expect_match(captured$output, "Warning: careful")
})

test_that("capture_script_output restores the console sinks", {
  n_sinks <- sink.number()
  message_sink <- sink.number(type = "message")
  capture_script_output(message("x"))
  capture_script_output(stop("y"))
  capture_script_output(sink(textConnection("leaked", "w", local = TRUE)))
  expect_equal(sink.number(), n_sinks)
  expect_equal(sink.number(type = "message"), message_sink)
})

test_that("capture_script_output strips ANSI codes and keeps the end of long output", {
  captured <- capture_script_output({
    cat("\033[31mred\033[39m\n")
    cat(strrep("a", 50), "\n", sep = "")
    cat("the end\n")
  }, max_chars = 20)
  expect_false(grepl("\033", captured$output, fixed = TRUE))
  expect_lte(nchar(captured$output), 23)
  expect_match(captured$output, "the end$")
})

# ---- shiny_ui_operate -------------------------------------------------------

# A stand-in for a module session: records what the tool sends to the browser.
# `inputs` are input IDs on the page; `values` are inputs with their values
fake_session <- function(module_id = "wavelet_module", inputs = character(),
                         values = list()) {
  session <- new.env()
  session$ns <- shiny::NS(module_id)
  session$input <- c(structure(as.list(seq_along(inputs)), names = inputs), values)
  session$sent <- list()
  session$sendCustomMessage <- function(type, message) {
    session$sent[[length(session$sent) + 1]] <- list(type = type, message = message)
  }
  session$sendModal <- function(type, message) {
    session$sent[[length(session$sent) + 1]] <- list(
      type = paste0("modal-", type), message = message)
  }
  session
}

operate <- function(session, ...) {
  tool <- shiny_ui_operate(session = session)[[1]]
  tool(...)
}

test_that("click on a module input sends shidashi.click with its namespaced ID", {
  session <- fake_session(inputs = "wavelet_confirm_btn2")
  reply <- operate(session, action = "click", target = "wavelet_confirm_btn2")
  expect_length(session$sent, 1)
  expect_equal(session$sent[[1]]$type, "shidashi.click")
  expect_equal(session$sent[[1]]$message$selector, "#wavelet_module-wavelet_confirm_btn2")
  expect_match(reply, "shiny_query_ui")
})

test_that("click also takes the input ID with the module prefix", {
  session <- fake_session(inputs = "wavelet_confirm_btn2")
  operate(session, action = "click", target = "wavelet_module-wavelet_confirm_btn2")
  expect_equal(session$sent[[1]]$message$selector, "#wavelet_module-wavelet_confirm_btn2")
})

test_that("click refuses a module input that is not on the page", {
  session <- fake_session(inputs = "wavelet_do_btn")
  expect_error(
    operate(session, action = "click", target = "wavelet_confirm_btn"),
    "not on the page"
  )
  expect_length(session$sent, 0)
})

test_that("click sends a CSS selector as it is", {
  session <- fake_session()
  operate(session, action = "click", target = ".swal-button")
  expect_equal(session$sent[[1]]$type, "shidashi.click")
  expect_equal(session$sent[[1]]$message$selector, ".swal-button")
})

test_that("click needs a target", {
  expect_error(operate(fake_session(), action = "click"), "target")
})

test_that("dismiss_modal closes the dialog", {
  session <- fake_session()
  operate(session, action = "dismiss_modal")
  expect_equal(session$sent[[1]]$type, "modal-remove")
})

test_that("close_alert2 closes the alert (dipsaus::close_alert2)", {
  session <- fake_session()
  reply <- operate(session, action = "close_alert2")
  expect_length(session$sent, 1)
  expect_equal(session$sent[[1]]$type, "dipsaus-swal-close")
  expect_match(reply, "alert")
})

test_that("show_notification stays until it is closed or removed", {
  session <- fake_session()
  operate(session, action = "show_notification", message = "Please review the dialog",
          title = "From the agent", type = "warning")
  sent <- session$sent[[1]]
  expect_equal(sent$type, "shidashi.show_notification")
  expect_equal(sent$message$body, "Please review the dialog")
  expect_equal(sent$message$title, "From the agent")
  expect_false(sent$message$autohide)
  expect_match(sent$message$class, "bg-warning")
  expect_match(sent$message$class, "wavelet_module-agent_notification")
})

test_that("show_notification needs a message", {
  expect_error(operate(fake_session(), action = "show_notification"), "message")
})

test_that("remove_notification removes notifications by CSS class, or all", {
  session <- fake_session()
  operate(session, action = "remove_notification", target = "wavelet_module-error_notif")
  operate(session, action = "remove_notification")
  expect_equal(session$sent[[1]]$type, "shidashi.clear_notification")
  expect_equal(session$sent[[1]]$message$selector, ".wavelet_module-error_notif.toast")
  expect_equal(session$sent[[2]]$message$selector, ".toast")
})

test_that("an unknown action is an error", {
  expect_error(operate(fake_session(), action = "scroll"), "action")
})

# ---- rave_3dviewer_get / rave_3dviewer_set ----------------------------------

# What a rendered threeBrain viewer (DemoSubject) reports to its module
# session: controller values, controller specs, subject transforms, and the
# last mouse events. Specs arrive keyed by controller name, choices as lists
viewer_spec <- function(name, folder, type, ...) {
  list(name = name, folder = folder, type = type, hidden = FALSE,
       disabled = FALSE, ...)
}
viewer_option <- function(name, folder, choices, values = choices, ...) {
  viewer_spec(name, folder, "option", choices = as.list(choices),
              values = as.list(values), ...)
}
demo_specs <- list(
  "Background Color" = viewer_spec("Background Color", "Default", "color"),
  "Camera Position" = viewer_option(
    "Camera Position", "Default",
    c("[free rotate]", "[lock]", "right", "left", "anterior", "posterior",
      "superior", "inferior")),
  "Reset Canvas" = viewer_spec("Reset Canvas", "Default", "function"),
  "Record" = viewer_spec("Record", "Default", "boolean"),
  "Show Panels" = viewer_spec("Show Panels", "Volume Settings", "boolean"),
  "Sagittal (L - R)" = viewer_spec("Sagittal (L - R)", "Volume Settings", "number",
                                   min = -128, max = 128, step = 0.1),
  "Coronal (P - A)" = viewer_spec("Coronal (P - A)", "Volume Settings", "number",
                                  min = -128, max = 128, step = 0.1),
  "Axial (I - S)" = viewer_spec("Axial (I - S)", "Volume Settings", "number",
                                min = -128, max = 128, step = 0.1),
  "Crosshair tkrRAS" = viewer_spec("Crosshair tkrRAS", "Volume Settings", "string"),
  "Frustum" = viewer_spec("Frustum", "Volume Settings", "interval",
                          min = -15, max = 15, step = 0.1),
  "Voxel Type" = viewer_option("Voxel Type", "Volume Settings",
                               c("aparc_a2009s_aseg", "aparc_aseg", "none")),
  "Voxel Display" = modifyList(
    viewer_option("Voxel Display", "Volume Settings",
                  c("hidden", "normal", "side camera", "main camera", "anat. slices")),
    list(hidden = TRUE)),
  "Surface Type" = viewer_option("Surface Type", "Surface Settings", "pial"),
  "Left Opacity" = viewer_spec("Left Opacity", "Surface Settings", "number",
                               min = 0.1, max = 1, step = 0.0009),
  "Display Data" = viewer_option("Display Data", "Data Visualization",
                                 c("[None]", "Value")),
  "Display Range" = viewer_spec("Display Range", "Data Visualization", "string"),
  "Display Data (Graph)" = viewer_spec("Display Data (Graph)",
                                       "Data Visualization", "linegraph"),
  "Speed" = viewer_option("Speed", "Data Visualization",
                          c("x 0.5", "x 1", "x 2"), c(0.5, 1, 2)),
  "Edit Mode" = modifyList(
    viewer_option("Edit Mode", "Electrode Localization",
                  c("disabled", "CT/volume")),
    list(disabled = TRUE))
)
demo_controllers <- list(
  "Background Color" = "#ffffff", "Camera Position" = "[free rotate]",
  "Record" = FALSE, "Show Panels" = TRUE,
  "Sagittal (L - R)" = -51.2, "Coronal (P - A)" = 56.32, "Axial (I - S)" = 0,
  "Crosshair tkrRAS" = "-51.2, 56.3, 0.0", "Frustum" = list(min = -1, max = 1),
  "Voxel Type" = "none", "Voxel Display" = "normal", "Surface Type" = "pial",
  "Left Opacity" = 1, "Display Data" = "Value",
  "Display Range" = "-12.30,12.30", "Display Data (Graph)" = -1, "Speed" = 1,
  "Edit Mode" = "disabled"
)
matrix_rows <- function(...) lapply(list(...), as.list)
demo_subject <- list(
  subject_code = "DemoSubject",
  Norig = matrix_rows(c(-1, 0, 0, 131.6144714), c(0, 0, 1, -127.5),
                      c(0, -1, 0, 127.5), c(0, 0, 0, 1)),
  Torig = matrix_rows(c(-1, 0, 0, 128), c(0, 0, 1, -128),
                      c(0, -1, 0, 128), c(0, 0, 0, 1)),
  xfm = matrix_rows(c(1.071608, -0.069364, 0.003839, -5.186203),
                    c(-0.033422, 1.293478, 0.133732, -26.001312),
                    c(0.039658, -0.231380, 1.268464, -21.980759),
                    c(0, 0, 0, 1))
)
demo_click <- list(
  object = list(
    name = "DemoSubject, 13 - G13", type = "electrode",
    position = list(-64.27, 8.94, 15.24), subject_code = "DemoSubject",
    hemisphere = "left", number = 13,
    MNI305_position = list(-70.78, -9.79, -7.87),
    MNI152_position = list(-70.72, -9.27, -5.65),
    scanner_position = list(-60.66, 9.44, 14.74)),
  name = "DemoSubject, 13 - G13", geom_type = "electrode",
  position = list(-64.27, 8.94, 15.24), is_electrode = TRUE,
  current_time = -0.7539, time_range = list(-1, 1.01), subject = "DemoSubject",
  current_clip = "Value", color_map = list(lut = as.list(1:64)),
  electrode_number = 13
)

viewer_session <- function(controllers = demo_controllers, specs = demo_specs,
                           subject = demo_subject, extra = list(),
                           outputId = "viewer") {
  values <- list(controllers, specs, subject)
  names(values) <- paste0(outputId, c("_controllers", "_controller_specs",
                                      "_current_subject"))
  values <- values[!vapply(values, is.null, FALSE)]
  fake_session("custom_3d_viewer", values = c(values, extra))
}

# Controller specs (types, choices, ranges) come from threeBrain 1.3.0.62+
skip_without_controller_specs <- function() {
  testthat::skip_if_not(
    "get_controller_specs" %in% names(threeBrain::ViewerProxy$public_methods),
    "needs threeBrain 1.3.0.62 or newer (controller specs)"
  )
}

viewer_get <- function(session, ...) {
  rave_3dviewer_get(session = session)[[1]](...)
}
viewer_set <- function(session, ...) {
  rave_3dviewer_set(session = session)[[1]](...)
}

# What the tool sent to the viewer: list(name, value) per message
sent_to_viewer <- function(session, outputId = "viewer") {
  type <- sprintf("threeBrain-RtoJS-custom_3d_viewer-%s", outputId)
  lapply(Filter(function(x) identical(x$type, type), session$sent),
         function(x) x$message)
}

test_that("the viewer tools are named rave_3dviewer_get and rave_3dviewer_set", {
  session <- viewer_session()
  expect_equal(rave_3dviewer_get(session = session)[[1]]@name, "rave_3dviewer_get")
  expect_equal(rave_3dviewer_set(session = session)[[1]]@name, "rave_3dviewer_set")
})

# -- outputId ---

test_that("outputId names a rendered viewer, with or without the module prefix", {
  session <- viewer_session()
  expect_equal(viewer_get(session, outputId = "viewer", name = "controllers")$outputId,
               "viewer")
  expect_equal(
    viewer_get(session, outputId = "custom_3d_viewer-viewer", name = "controllers")$outputId,
    "viewer")
})

test_that("an unknown outputId is an error that lists the rendered viewers", {
  session <- viewer_session(extra = list(brain_viewer_controllers = list(a = 1)))
  err <- expect_error(viewer_get(session, outputId = "nope", name = "controllers"))
  expect_match(conditionMessage(err), "nope")
  expect_match(conditionMessage(err), "`viewer`")
  expect_match(conditionMessage(err), "`brain_viewer`")
  expect_error(viewer_set(session, outputId = "nope", name = "controllers",
                          data = '{"Show Panels": false}'), "nope")
  expect_length(session$sent, 0)
})

test_that("with no rendered viewer the error says so", {
  session <- fake_session("custom_3d_viewer")
  expect_error(viewer_get(session, outputId = "viewer", name = "camera"),
               "No 3D viewer has rendered")
})

# -- args and data encoding ---

test_that("args must be a JSON object, and each name takes only its own keys", {
  session <- viewer_session()
  expect_error(viewer_get(session, outputId = "viewer", name = "controllers",
                          args = "[1, 2]"), "JSON object")
  expect_error(viewer_get(session, outputId = "viewer", name = "controllers",
                          args = "{not json"), "JSON object")
  err <- expect_error(viewer_get(session, outputId = "viewer", name = "camera",
                                 args = '{"space": "scanner"}'))
  expect_match(conditionMessage(err), "`camera`.*does not take.*`space`")
  expect_error(viewer_get(session, outputId = "viewer", name = "crosshair",
                          args = '{"spaces": "all"}'), "`space`")
})

test_that("args and data may also arrive as decoded lists", {
  session <- viewer_session()
  res <- viewer_get(session, outputId = "viewer", name = "controllers",
                    args = list(names = "Surface Type"))
  expect_equal(names(res$controllers), "Surface Type")
  viewer_set(session, outputId = "viewer", name = "controllers",
             data = list("Show Panels" = FALSE))
  expect_equal(sent_to_viewer(session)[[1]]$value, list("Show Panels" = FALSE))
})

# -- get ---

test_that("get controllers returns every value, or the ones named in args", {
  session <- viewer_session()
  res <- viewer_get(session, outputId = "viewer", name = "controllers")
  expect_equal(res$controllers, demo_controllers)
  res <- viewer_get(session, outputId = "viewer", name = "controllers",
                    args = '{"names": ["Surface Type", "Left Opacity"]}')
  expect_equal(res$controllers, demo_controllers[c("Surface Type", "Left Opacity")])
})

test_that("get controllers suggests close names for an unknown one", {
  session <- viewer_session()
  err <- expect_error(viewer_get(session, outputId = "viewer", name = "controllers",
                                 args = '{"names": ["Surface Typ"]}'))
  expect_match(conditionMessage(err), "Surface Typ")
  expect_match(conditionMessage(err), "`Surface Type`")
})

test_that("get controller_options gives type, choices and ranges, not buttons", {
  skip_without_controller_specs()
  session <- viewer_session()
  res <- viewer_get(session, outputId = "viewer", name = "controller_options")
  opts <- res$controllers
  expect_false("Reset Canvas" %in% names(opts))
  expect_equal(opts[["Voxel Type"]]$type, "option")
  expect_equal(unclass(opts[["Voxel Type"]]$choices),
               c("aparc_a2009s_aseg", "aparc_aseg", "none"))
  expect_equal(opts[["Left Opacity"]]$min, 0.1)
  expect_equal(opts[["Left Opacity"]]$max, 1)
  expect_equal(unclass(opts[["Speed"]]$choices), c(0.5, 1, 2))
  expect_equal(unclass(opts[["Speed"]]$labels), c("x 0.5", "x 1", "x 2"))
  expect_true(opts[["Voxel Display"]]$hidden)
  expect_false(opts[["Frustum"]]$settable)
  expect_false(opts[["Display Data (Graph)"]]$settable)
  expect_true(opts[["Edit Mode"]]$disabled)
  res <- viewer_get(session, outputId = "viewer", name = "controller_options",
                    args = '{"names": ["Surface Type"]}')
  expect_equal(names(res$controllers), "Surface Type")
})

test_that("get camera reports the camera, and says it follows mouse drags only", {
  session <- viewer_session(extra = list(viewer_main_camera = list(
    target = list(x = 0, y = 0, z = 0), position = list(x = 0, y = 500, z = 0),
    up = list(x = 0, y = 0, z = 1), zoom = 2)))
  res <- viewer_get(session, outputId = "viewer", name = "camera")
  expect_equal(res$position, c(0, 500, 0))
  expect_equal(res$up, c(0, 0, 1))
  expect_equal(res$zoom, 2)
  expect_match(res$note, "drag")
  res <- viewer_get(viewer_session(), outputId = "viewer", name = "camera")
  expect_null(res$position)
  expect_match(res$note, "not reported")
})

test_that("get crosshair defaults to scanner space, and `all` gives every space", {
  session <- viewer_session()
  res <- viewer_get(session, outputId = "viewer", name = "crosshair")
  expect_equal(res$space, "scanner")
  expect_equal(res$position, c(-51.2, 56.32, 0) + c(3.6144714, 0.5, -0.5),
               tolerance = 1e-6)
  res <- viewer_get(session, outputId = "viewer", name = "crosshair",
                    args = '{"space": "tkrRAS"}')
  expect_equal(res$position, c(-51.2, 56.32, 0))
  res <- viewer_get(session, outputId = "viewer", name = "crosshair",
                    args = '{"space": "all"}')
  expect_named(res$positions, c("scanner", "tkrRAS", "MNI305", "MNI152", "CRS"))
  expect_equal(res$positions$tkrRAS, c(-51.2, 56.32, 0))
  expect_error(viewer_get(session, outputId = "viewer", name = "crosshair",
                          args = '{"space": "voxel"}'), "space")
})

test_that("get crosshair needs slice panels, and a subject for spaces but tkrRAS", {
  no_slices <- demo_controllers[!grepl("Sagittal|Coronal|Axial", names(demo_controllers))]
  expect_error(viewer_get(viewer_session(controllers = no_slices), outputId = "viewer",
                          name = "crosshair"), "slice panels")
  session <- viewer_session(subject = NULL)
  expect_error(viewer_get(session, outputId = "viewer", name = "crosshair"), "subject")
  res <- viewer_get(session, outputId = "viewer", name = "crosshair",
                    args = '{"space": "tkrRAS"}')
  expect_equal(res$position, c(-51.2, 56.32, 0))
  res <- viewer_get(session, outputId = "viewer", name = "crosshair",
                    args = '{"space": "all"}')
  expect_named(res$positions, "tkrRAS")
  expect_match(res$note, "subject")
})

test_that("get selected reports the last click, double-click and focus", {
  session <- viewer_session(extra = list(
    viewer_mouse_clicked = demo_click,
    viewer_mouse_dblclicked = demo_click,
    viewer_mouse_focused = list(name = "Left Hemisphere - pial (DemoSubject)",
                                geom_type = "hemisphere", subject = "DemoSubject",
                                tkr_ras = list(-40, 10, 20), scan_ras = list(-36.4, 10.5, 19.5),
                                vertex_index = 1234)))
  res <- viewer_get(session, outputId = "viewer", name = "selected")
  expect_equal(res$click$name, "DemoSubject, 13 - G13")
  expect_equal(res$click$electrode, 13)
  expect_equal(res$click$subject, "DemoSubject")
  expect_equal(res$click$tkrRAS, c(-64.27, 8.94, 15.24))
  expect_equal(res$click$MNI152, c(-70.72, -9.27, -5.65))
  expect_equal(res$click$scanner, c(-60.66, 9.44, 14.74))
  expect_equal(res$click$displayed_data, "Value")
  expect_equal(res$click$time, -0.7539)
  expect_null(res$click$color_map)
  expect_equal(res$double_click$electrode, 13)
  expect_equal(res$focus$tkrRAS, c(-40, 10, 20))
  expect_equal(res$focus$vertex_index, 1234)
  res <- viewer_get(viewer_session(), outputId = "viewer", name = "selected")
  expect_null(res$click)
  expect_match(res$note, "click")
})

# -- set controllers ---

test_that("set controllers sends one message, primary controllers first", {
  skip_without_controller_specs()
  session <- viewer_session()
  res <- viewer_set(session, outputId = "viewer", name = "controllers", data = paste0(
    '{"Left Opacity": 0.4, "Display Range": [-5, 5], "Display Data": "Value",',
    ' "Voxel Display": "normal", "Voxel Type": "aparc_aseg"}'))
  sent <- sent_to_viewer(session)
  expect_length(sent, 1)
  expect_equal(sent[[1]]$name, "controllers")
  expect_equal(names(sent[[1]]$value), c("Display Data", "Voxel Type", "Left Opacity",
                                         "Display Range", "Voxel Display"))
  expect_equal(sent[[1]]$value[["Display Range"]], "-5,5")
  expect_equal(res$sent, sent[[1]]$value)
  expect_match(paste(res$note, collapse = " "), "Voxel Display")
})

test_that("set controllers converts values the viewer expects in another form", {
  skip_without_controller_specs()
  session <- viewer_session()
  viewer_set(session, outputId = "viewer", name = "controllers", data = paste0(
    '{"Background Color": "red", "Speed": "x 2", "Crosshair tkrRAS": [1, 2.5, 3]}'))
  value <- sent_to_viewer(session)[[1]]$value
  expect_equal(value[["Background Color"]], "#FF0000")
  expect_equal(value[["Speed"]], 2)
  expect_equal(value[["Crosshair tkrRAS"]], "1, 2.5, 3")
  session <- viewer_session()
  viewer_set(session, outputId = "viewer", name = "controllers",
             data = '{"Background Color": "#336699", "Speed": 0.5}')
  value <- sent_to_viewer(session)[[1]]$value
  expect_equal(value[["Background Color"]], "#336699")
  expect_equal(value[["Speed"]], 0.5)
})

test_that("set controllers rejects bad values and sends nothing", {
  skip_without_controller_specs()
  session <- viewer_session()
  set_one <- function(json) {
    viewer_set(session, outputId = "viewer", name = "controllers", data = json)
  }
  err <- expect_error(set_one('{"Surface Typ": "pial"}'))
  expect_match(conditionMessage(err), "Unknown controller `Surface Typ`")
  expect_match(conditionMessage(err), "`Surface Type`")
  expect_error(set_one('{"Show Panels": "false"}'), "`Show Panels`.*true or false")
  expect_error(set_one('{"Left Opacity": 2}'), "`Left Opacity`.*between 0.1 and 1")
  expect_error(set_one('{"Left Opacity": "0.4"}'), "`Left Opacity`.*number")
  expect_error(set_one('{"Voxel Type": "aseg"}'), "`Voxel Type`.*one of")
  expect_error(set_one('{"Background Color": "not-a-color"}'), "`Background Color`.*color")
  expect_error(set_one('{"Edit Mode": "CT/volume"}'), "`Edit Mode` is disabled")
  expect_error(set_one('{"Reset Canvas": true}'), "`Reset Canvas` is a button")
  expect_error(set_one('{"Frustum": {"min": -2, "max": 2}}'), "`Frustum` cannot be set")
  expect_error(set_one('{"Record": true}'), "`Record`")
  expect_error(set_one('{}'), "at least one")
  expect_error(set_one('"Surface Type"'), "JSON object")
  # one bad value stops the whole call
  expect_error(set_one('{"Show Panels": false, "Left Opacity": 5}'), "Left Opacity")
  expect_length(session$sent, 0)
})

test_that("without controller specs (older threeBrain) set controllers checks types only", {
  session <- viewer_session(specs = NULL)
  res <- viewer_set(session, outputId = "viewer", name = "controllers",
                    data = '{"Show Panels": false, "Left Opacity": 5}')
  expect_equal(sent_to_viewer(session)[[1]]$value,
               list("Show Panels" = FALSE, "Left Opacity" = 5))
  expect_match(paste(res$note, collapse = " "), "not checked")
  expect_error(viewer_set(session, outputId = "viewer", name = "controllers",
                          data = '{"Show Panels": "no"}'), "true or false")
  expect_error(viewer_set(session, outputId = "viewer", name = "controllers",
                          data = '{"Surface Typ": "pial"}'), "Unknown controller")
})

# -- set camera ---

test_that("set camera sends the direction, up vector and zoom", {
  session <- viewer_session()
  res <- viewer_set(session, outputId = "viewer", name = "camera",
                    data = '{"position": [0, 1, 0], "up": [0, 0, 1], "zoom": 1.5}')
  sent <- sent_to_viewer(session)
  expect_equal(vapply(sent, `[[`, "", "name"), c("camera", "zoom_level"))
  expect_equal(sent[[1]]$value$position, c(0, 500, 0))
  expect_equal(sent[[1]]$value$up, c(0, 0, 1))
  expect_equal(sent[[2]]$value, 1.5)
  expect_match(paste(res$note, collapse = " "), "not report")
  session <- viewer_session()
  viewer_set(session, outputId = "viewer", name = "camera", data = '{"zoom": 3}')
  expect_equal(vapply(sent_to_viewer(session), `[[`, "", "name"), "zoom_level")
})

test_that("set camera rejects bad cameras and sends nothing", {
  session <- viewer_session()
  set_camera <- function(json) {
    viewer_set(session, outputId = "viewer", name = "camera", data = json)
  }
  expect_error(set_camera('{"zoom": 100}'), "zoom.*0.5.*40")
  expect_error(set_camera('{"up": [0, 1, 0]}'), "`up`.*`position`")
  expect_error(set_camera('{"position": [0, 0, 500]}'), "parallel.*\\[0, ?1, ?0\\]")
  expect_error(set_camera('{"position": [0, 0, 1], "up": [0, 0, -1]}'), "parallel")
  expect_error(set_camera('{"position": [0, 0, 0]}'), "position")
  expect_error(set_camera('{"position": [1, 2]}'), "position")
  expect_error(set_camera('{}'), "position.*up.*zoom")
  expect_error(set_camera('{"angle": 3}'), "does not take.*`angle`")
  expect_length(session$sent, 0)
})

# -- set crosshair ---

test_that("set crosshair takes scanner coordinates by default", {
  session <- viewer_session()
  res <- viewer_set(session, outputId = "viewer", name = "crosshair",
                    data = '{"position": [-47.5855286, 56.82, -0.5]}')
  sent <- sent_to_viewer(session)
  expect_equal(sent[[1]]$name, "controllers")
  expect_equal(unname(unlist(sent[[1]]$value[c("Sagittal (L - R)", "Coronal (P - A)",
                                               "Axial (I - S)")])),
               c(-51.2, 56.32, 0), tolerance = 1e-6)
  expect_equal(res$space, "scanner")
  session <- viewer_session()
  viewer_set(session, outputId = "viewer", name = "crosshair",
             data = '{"position": [1, 2, 3], "space": "tkrRAS"}')
  expect_equal(unname(unlist(sent_to_viewer(session)[[1]]$value)), c(1, 2, 3))
})

test_that("set crosshair checks the position, the space, the panels and the subject", {
  no_slices <- demo_controllers[!grepl("Sagittal|Coronal|Axial", names(demo_controllers))]
  expect_error(viewer_set(viewer_session(controllers = no_slices), outputId = "viewer",
                          name = "crosshair", data = '{"position": [1, 2, 3]}'),
               "slice panels")
  session <- viewer_session(subject = NULL)
  expect_error(viewer_set(session, outputId = "viewer", name = "crosshair",
                          data = '{"position": [1, 2, 3]}'), "subject")
  viewer_set(session, outputId = "viewer", name = "crosshair",
             data = '{"position": [1, 2, 3], "space": "tkrRAS"}')
  expect_length(sent_to_viewer(session), 1)
  session <- viewer_session()
  expect_error(viewer_set(session, outputId = "viewer", name = "crosshair",
                          data = '{"position": [1, 2]}'), "position")
  expect_error(viewer_set(session, outputId = "viewer", name = "crosshair",
                          data = '{"position": [1, 2, 3], "space": "voxel"}'), "space")
  expect_length(session$sent, 0)
})

test_that("set controllers sends an option's own value, whatever form it came in", {
  skip_without_controller_specs()
  session <- viewer_session()
  viewer_set(session, outputId = "viewer", name = "controllers",
             data = '{"Speed": "0.5", "Background Color": "Red"}')
  value <- sent_to_viewer(session)[[1]]$value
  expect_identical(value[["Speed"]], 0.5)
  expect_equal(value[["Background Color"]], "#FF0000")
})
