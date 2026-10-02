# Manually test the MCP tools: load a brain in the Subject 3D Viewer, color its
# electrodes with an uploaded table, and drive the 3D viewer the way an agent
# would, without clicking in the browser. A browser session with the module
# open is still needed: input updates, clicks, and the viewer's state
# round-trip through it.
#
# Agents take the same path as people:
#   * `shiny_input_update` sets the loader inputs; `load_data` loads the brain
#   * `shiny_input_update` picks the data table; `run_analysis` (the
#     "Re-generate & Visualize" button) regenerates the viewer
#   * `rave_3dviewer_get` / `rave_3dviewer_set` read and change the viewer
#     (controllers, camera, crosshair) with outputId "viewer"
#   * `reset_viewer` is the "Reset controller option" link
#   * Quick analysis: `add_object` ("Add object"), `open_analysis`
#     ("Configure & Run..."), then `shiny_ui_operate` clicks the dialog's "Run"
#
# Writes: `modules/custom_3d_viewer/settings.yaml` (copy it first and restore
# it from the copy; `git checkout` would also drop uncommitted edits) and the
# module's own target store. No subject file is written: the data table is an
# upload that already exists.
#
# Needs threeBrain 1.3.0.62 or newer (controller specs and fresh controller
# values) in the app's R session.

module <- "custom_3d_viewer"
source("agents/skills/build-module-mcp/test-common.R")  # shared MCP test helpers
viewer <- "viewer"   # the viewer's outputId

# test subject and data
project_name   <- "demo"
subject_code   <- "DemoSubject"
volume_types   <- "aparc+aseg"
surface_types  <- "pial-outer-smoothed"
uploaded_table <- "electrode_value-14.fst"   # existing upload (Subject, Electrode, Time, Amplitude)

# Quick analysis (streamline collision detection) needs streamlines, which
# DemoSubject does not have. The user chose YAEL/CIT168 for the Run step;
# set `analysis_subject_code` to NULL to skip it
# The ROI is an electrode: with a volume ROI the analysis itself fails
# (`as_ieegio_streamlines` error in its pipeline target, for people too)
analysis_project_name <- "YAEL"
analysis_subject_code <- "CIT168"
analysis_streamlines  <- "basalganglia/*"   # loader choice and object (a group)
analysis_electrode    <- "1"                # the ROI object

# ---- helpers ----------------------------------------------------------------

viewer_set_ok <- function(name, data) {
  text <- viewer_set(name, data)
  if (isTRUE(attr(text, "is_error"))) {
    stop(sprintf("rave_3dviewer_set `%s` failed: %s", name, text[[1]]))
  }
  invisible(jsonlite::fromJSON(text[[1]]))
}

viewer_set_fails <- function(name, data, pattern) {
  text <- viewer_set(name, data)
  if (!isTRUE(attr(text, "is_error")) || !grepl(pattern, text[[1]])) {
    stop(sprintf("Expected rave_3dviewer_set `%s` to fail with `%s`; got: %s",
                 name, pattern, text[[1]]))
  }
  invisible(text)
}

controllers_now <- function(labels) {
  viewer_get("controllers", list(names = I(labels)))$controllers
}

# Wait until the viewer reports these controller values
wait_controllers <- function(expected, timeout = 20) {
  wait_until(function() {
    now <- tryCatch(controllers_now(names(expected)), error = function(e) NULL)
    !is.null(now) && all(vapply(names(expected), function(label) {
      isTRUE(all.equal(now[[label]], expected[[label]]))
    }, FALSE))
  }, sprintf("controllers %s", jsonlite::toJSON(expected, auto_unbox = TRUE)),
  timeout = timeout)
}

# The viewer counts as ready once it reports its controllers
wait_viewer <- function(timeout = 120) {
  wait_until(function() {
    text <- tool("tool__rave_3dviewer_get", outputId = viewer, name = "controllers",
                 .quiet = TRUE)
    !isTRUE(attr(text, "is_error"))
  }, "the 3D viewer to render", timeout = timeout, interval = 2)
}

# The settings the module saved, read from its settings.yaml
saved_settings <- function() {
  yaml::read_yaml(file.path("modules", module, "settings.yaml"))
}

stopifnot(app_running())
stopifnot(module_open())

# ---- protocol -----------------------------------------------------------------

tools <- mcp("tools/list")$result$tools
tool_names <- vapply(tools, `[[`, "", "name")
names(tools) <- tool_names
hints <- data.frame(
  tool        = tool_names,
  read_only   = vapply(tools, function(t) isTRUE(t$annotations$readOnlyHint), FALSE),
  destructive = vapply(tools, function(t) isTRUE(t$annotations$destructiveHint), FALSE)
)
print(hints)
stopifnot(
  "tool__rave_3dviewer_get is read-only" =
    isTRUE(hints$read_only[hints$tool == "tool__rave_3dviewer_get"]),
  "tool__rave_3dviewer_set is neither read-only nor destructive" =
    identical(unlist(hints[hints$tool == "tool__rave_3dviewer_set", c("read_only", "destructive")],
                     use.names = FALSE), c(FALSE, FALSE)),
  "tool__shiny_ui_operate is listed" = "tool__shiny_ui_operate" %in% tool_names
)

tool("skill_load__rave-module", action = "reference",
     file_name = "references/custom_3d_viewer.md", pattern = "Drive the module")

scripts <- jsonlite::fromJSON(tool("tool__module_interactive_script_list")[[1]])$scripts
stopifnot(all(c("load_data", "run_analysis", "reset_viewer", "add_object",
                "open_analysis") %in% scripts$name))

# ---- load the brain -----------------------------------------------------------

set_input_wait("loader_project_name", project_name)
set_input_wait("loader_subject_code", subject_code)
set_input_wait("loader_electrode_source", "Subject meta directory - electrodes.csv")
set_input_wait("loader_volume_types", volume_types)
set_input_wait("loader_surface_types", surface_types)
set_input_wait("loader_annot_types", I(character(0)))
set_input_wait("loader_streamline_types", I(character(0)))
set_input_wait("loader_use_spheres", FALSE)
set_input_wait("loader_use_template", FALSE)
run_script("load_data")
wait_viewer()
stopifnot(identical(saved_settings()$subject_code, subject_code))

# ---- pick the data table, then regenerate the viewer ---------------------------

set_input_wait("data_source", "Uploads")
set_input_wait("uploaded_source", uploaded_table)
run_script("run_analysis")
wait_until(function() {
  options <- tryCatch(viewer_get("controller_options", list(names = I("Display Data"))),
                      error = function(e) NULL)
  "Amplitude" %in% options$controllers[["Display Data"]]$choices
}, "the viewer to show the uploaded table", timeout = 120, interval = 2)
stopifnot(identical(saved_settings()$uploaded_source, uploaded_table))

# ---- read the viewer ------------------------------------------------------------

str(viewer_get("controllers", list(names = I(c("Display Data", "Surface Type",
                                                "Background Color")))))
options <- viewer_get("controller_options",
                      list(names = I(c("Surface Type", "Voxel Type", "Left Opacity"))))$controllers
str(options)
stopifnot(
  "surface types loaded" = all(c("pial", surface_types) %in% options[["Surface Type"]]$choices),
  "volume loaded" = "aparc_aseg" %in% options[["Voxel Type"]]$choices,
  "opacity range" = identical(c(options[["Left Opacity"]]$min, options[["Left Opacity"]]$max),
                              c(0.1, 1))
)
camera <- viewer_get("camera")
cat("camera:", camera$note, "\n")
crosshair <- viewer_get("crosshair")
stopifnot(identical(crosshair$space, "scanner"), length(crosshair$position) == 3)
all_spaces <- viewer_get("crosshair", list(space = "all"))$positions
stopifnot(identical(names(all_spaces), c("scanner", "tkrRAS", "MNI305", "MNI152", "CRS")))
str(viewer_get("selected"))

# ---- change the viewer ------------------------------------------------------------

view <- list("Surface Type" = surface_types, "Left Opacity" = 0.4,
             "Voxel Type" = "aparc_aseg", "Background Color" = "#000000",
             "Display Data" = "Amplitude", "Display Range" = "-2,2")
viewer_set_ok("controllers", c(view[names(view) != "Display Range"],
                               list("Display Range" = c(-2, 2))))
wait_controllers(view)

viewer_set_ok("camera", list(position = c(-1, 0.3, 0.2), zoom = 1.3))
viewer_set_ok("crosshair", list(position = c(-40, 10, 20), space = "MNI152"))
wait_until(function() {
  position <- viewer_get("crosshair", list(space = "MNI152"))$position
  isTRUE(all(abs(position - c(-40, 10, 20)) < 0.01))
}, "the crosshair to move", timeout = 20)

# ---- errors: nothing is sent ------------------------------------------------------

viewer_set_fails("controllers", list("Left Opacity" = 5), "between 0.1 and 1")
viewer_set_fails("controllers", list("Surface Typ" = "pial"), "Surface Type")
viewer_set_fails("camera", list(zoom = 100), "0.5 and 40")
viewer_set_fails("crosshair", list(position = c(1, 2)), "position")
wrong_id <- tool("tool__rave_3dviewer_get", outputId = "brain", name = "controllers")
stopifnot(isTRUE(attr(wrong_id, "is_error")), grepl("`viewer`", wrong_id[[1]]))
Sys.sleep(1)
stopifnot(isTRUE(all.equal(controllers_now("Left Opacity")[["Left Opacity"]], 0.4)))

# ---- re-generating keeps the synced controllers -------------------------------------

run_script("run_analysis")
wait_until(function() {
  now <- tryCatch(controllers_now(c("Left Opacity", "Voxel Type", "Background Color")),
                  error = function(e) NULL)
  isTRUE(all.equal(now[["Left Opacity"]], 0.4)) && identical(now[["Voxel Type"]], "none")
}, "the regenerated viewer (Left Opacity kept, Voxel Type reset)", timeout = 120, interval = 2)
saved <- saved_settings()$controllers
stopifnot(isTRUE(all.equal(saved[["Left Opacity"]], 0.4)),
          identical(saved[["Background Color"]], "#000000"))

# ---- reset: the script, then the link as a person clicks it -------------------------

run_script("reset_viewer")
wait_controllers(list("Left Opacity" = 1), timeout = 120)
stopifnot(!length(saved_settings()$controllers))

viewer_set_ok("controllers", list("Left Opacity" = 0.6))
wait_controllers(list("Left Opacity" = 0.6))
operate("click", target = "viewer_reset")
wait_controllers(list("Left Opacity" = 1), timeout = 120)

# ---- quick analysis: objects and the dialog (no streamlines needed) -----------------

object_keys <- function() unlist(input_info("object_selector_list")$current_value)

# As agents should: wait until the preview shows the object, then add it; the
# script returns the added object's label
add_object <- function(pattern) {
  wait_until(function() {
    preview <- tool("tool__shiny_output_result", outputId = "object_selector_text",
                    transform_image = FALSE, .quiet = TRUE)
    isTRUE(grepl(pattern, preview[[1]]))
  }, sprintf("the preview to show `%s`", pattern))
  added <- run_script("add_object")$result
  if (!isTRUE(grepl(pattern, added))) {
    stop(sprintf("`add_object` returned `%s`, not the object `%s`",
                 paste(added, collapse = ""), pattern))
  }
  invisible(added)
}
set_input_wait("object_selector_list", I(character(0)),
               check = function(value) !length(unlist(value)))
set_input_wait("object_selector", "Electrode")
set_input_wait("object_selector_electrode", "14")
add_object("ch 14")
wait_until(function() length(object_keys()) == 1, "the electrode in the object list")
set_input_wait("object_selector", "3D volume")
set_input_wait("object_selector_volume", "aparc_aseg")
add_object("Overlay aparc_aseg")
wait_until(function() length(object_keys()) == 2, "the volume in the object list")
keys <- object_keys()

# the dialog opens; Cancel, as a user who declines would
run_script("open_analysis")
wait_until(function() on_page(sprintf("#%s-analysis_param_run", module)), "the analysis dialog")
operate("dismiss_modal")
wait_until(function() !on_page(sprintf("#%s-analysis_param_run", module)),
           "the dialog to close")

# remove the electrode, then clear the list
set_input_wait("object_selector_list", I(keys[2]))
set_input_wait("object_selector_list", I(character(0)),
               check = function(value) !length(unlist(value)))

# ---- quick analysis: run it on a subject with streamlines -----------------------------

if (is.null(analysis_subject_code)) {
  cat("\nQuick analysis skipped: set `analysis_subject_code` to a subject with",
      "streamlines.\n")
} else {
  set_input_wait("loader_project_name", analysis_project_name)
  set_input_wait("loader_subject_code", analysis_subject_code)
  set_input_wait("loader_volume_types", I(character(0)))
  set_input_wait("loader_surface_types", I(character(0)))
  set_input_wait("loader_streamline_types", I(analysis_streamlines))
  run_script("load_data")
  stopifnot(identical(saved_settings()$subject_code, analysis_subject_code))
  # the old viewer's controllers stay until the new one reports: wait for
  # the streamline controllers of the new brain
  group <- sub("/\\*$", "", analysis_streamlines)
  wait_until(function() {
    options <- tryCatch(viewer_get("controller_options"), error = function(e) NULL)
    any(startsWith(names(options$controllers), paste0("Show: ", group, "/")))
  }, "the viewer to show the streamlines", timeout = 300, interval = 3)

  # an ROI electrode and a streamline group
  set_input_wait("object_selector_list", I(character(0)),
                 check = function(value) !length(unlist(value)))
  set_input_wait("object_selector", "Electrode")
  set_input_wait("object_selector_electrode", analysis_electrode)
  add_object(sprintf("\\[ch %s\\]", analysis_electrode))
  set_input_wait("object_selector", "Streamlines")
  set_input_wait("object_selector_streamlines", analysis_streamlines)
  add_object(paste0("Streamlines .*", group, "/"))
  wait_until(function() length(object_keys()) == 2, "two objects in the list")

  run_script("open_analysis")
  wait_until(function() on_page(sprintf("#%s-analysis_param_run", module)),
             "the analysis dialog")
  operate("click", target = "analysis_param_run")   # the dialog's "Run", as a person would
  wait_until(function() !on_page(sprintf("#%s-analysis_param_run", module)),
             "the analysis to finish", timeout = 300, interval = 2)
  result <- tool("tool__shiny_output_result", outputId = "analysis_results",
                 transform_image = FALSE)
  stopifnot(grepl("Overlap", result[[1]]))
  stopifnot(length(saved_settings()$analysis_objects) == 2)
}

cat("\nSubject 3D Viewer workflow passed. Restore",
    "`modules/custom_3d_viewer/settings.yaml` from your copy.\n")
