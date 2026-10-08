# Test the MCP tools of the Electrode Localization module the way an agent
# would, without clicking in the browser. A browser session with the module
# open is still needed: input updates and clicks round-trip through it.
#
# The test writes the subject's staged electrode table
# (meta/electrodes_unsaved.csv, geometry_unsaved.json) by loading and staging.
# It never saves to the subject: it checks that agents' clicks on 'Save to
# subject' are refused. Back up the subject's meta folder and
# `modules/electrode_localization/settings.yaml` first, and restore them after.

module <- "electrode_localization"
source("agents/skills/build-module-mcp/test-common.R")  # shared MCP test helpers

# test subject: four groups of an sEEG electrode model, channels 1-48
project_name <- "YAEL"
subject_code <- "yael_demo_001"
viewer       <- "localization_viewer"
model        <- "SEEG-ADTECH-SD12R-SP05X-000"
full_plan <- lapply(seq_len(4), function(ii) {
  list(label = c("AHL", "LB", "LC", "LD")[[ii]],
       dimension = sprintf("%d-%d", ii * 12 - 11, ii * 12),
       type = model, hemisphere = "auto")
})
short_plan <- full_plan[1]   # leaves out channels 13-48 of the staged table

# ---- helpers ----------------------------------------------------------------

subject <- ravecore::as_rave_subject(sprintf("%s/%s", project_name, subject_code),
                                     strict = FALSE)
staged_file <- file.path(subject$meta_path, "electrodes_unsaved.csv")
saved_file <- file.path(subject$meta_path, "electrodes.csv")

plan_labels <- function(value) {
  vapply(unname(value)[vapply(unname(value), is.list, FALSE)], function(group) {
    paste(group$label, collapse = "")
  }, "")
}
same_plan <- function(plan) {
  function(value) identical(plan_labels(value), vapply(plan, `[[`, "", "label"))
}

stopifnot(app_running(), module_open())
started <- Sys.time()
checks <- c()

# ---- tools and scripts --------------------------------------------------------

offered <- module_tools()
checks["tools: shiny_ui_operate and 3D viewer tools offered"] <-
  all(vapply(c("shiny_ui_operate", "rave_3dviewer_get", "rave_3dviewer_set"),
             function(nm) any(grepl(nm, offered)), FALSE))

# ---- loader -------------------------------------------------------------------

set_input_wait("loader_project_name", project_name)
set_input_wait("loader_subject_code", subject_code)
wait_input("loader_transform_fname", check = function(v) identical(unlist(v), "CT_IJK_to_MR_RAS.txt"),
           timeout = 60)
checks["loader: method filled in from the co-registration"] <-
  identical(unlist(input_info("loader_method")$current_value),
            "CT (IJK) to MR (RAS) transform + Raw CT")
checks["loader: CT filled in"] <-
  identical(unlist(input_info("loader_ct_fname")$current_value), "CT_RAW.nii.gz")
checks["loader: scripts listed"] <- all(c("load_data") %in%
  vapply(list_scripts(.quiet = TRUE)$scripts, `[[`, "", "name"))

# Error case: a plan that leaves out channels of the staged table
md5_staged <- tools::md5sum(staged_file)
set_input_wait("loader_plan", short_plan, check = same_plan(short_plan))
error_text <- run_script_error("load_data")
checks["load error: names the missing channels"] <-
  grepl("13-48", error_text) && grepl("include them in the electrode plan", error_text)
checks["load error: staged table unchanged"] <- unname(tools::md5sum(staged_file) == md5_staged)
Sys.sleep(1)
operate("close_alert2")
Sys.sleep(1)
checks["load error: alert closed"] <- !nzchar(alert_text())

# Load with the full plan
set_input_wait("loader_plan", full_plan, check = same_plan(full_plan))
loaded <- run_script("load_data")
checks["load: result names the subject"] <-
  any(grepl(sprintf("subject=%s/%s", project_name, subject_code), loaded$result, fixed = TRUE))
checks["load: result lists the groups"] <-
  any(grepl("AHL (SEEG-ADTECH-SD12R-SP05X-000, 12 channels)", loaded$result, fixed = TRUE))
wait_until(data_loaded, "data loaded", timeout = 120)

# ---- main page ----------------------------------------------------------------

status <- run_script("localization_status")
checks["status: four groups"] <- sum(grepl("^group [1-4]", status$result)) == 4
checks["status: electrode model control points"] <-
  any(grepl("control points placed 12/12", status$result, fixed = TRUE))

set_input("switch_plan_btn_1", "")
wait_until(function() grepl("scannerRAS", page_text("#electrode_localization-group_table")),
           "group table", timeout = 30)
table_text <- page_text("#electrode_localization-group_table")
checks["table: unified columns"] <- grepl("Label scannerRAS MNI152 FSLabel", table_text, fixed = TRUE)
checks["table: contact labels"] <- grepl("AHL1", table_text, fixed = TRUE)
checks["table: no model positions"] <- !grepl("ModelXYZ", table_text, fixed = TRUE)

# The viewer reports its controllers once it has loaded its images; a group
# switch before that is ignored, so switch again while waiting
controllers <- NULL
for (attempt in seq_len(10)) {
  controllers <- tryCatch(viewer_get("controllers"), error = function(e) NULL)
  if (!is.null(controllers)) break
  set_input(sprintf("switch_plan_btn_%d", 2 - attempt %% 2), "")
  Sys.sleep(3)
}
set_input("switch_plan_btn_1", "")
Sys.sleep(2)
checks["viewer: Edit Mode reported"] <- !is.null(controllers) &&
  any(grepl("Edit Mode", names(unlist(controllers))))

set_input("action_reset_fslabel_btn", "")
Sys.sleep(2)
checks["re-compute: table still shown"] <- grepl("AHL1", page_text("#electrode_localization-group_table"))

# ---- staging and the people-only Save button ---------------------------------------

saved_mtime <- file.mtime(saved_file)
staged <- run_script("run_analysis")
checks["stage: result names the staged file"] <- any(grepl("^staged=", staged$result))
checks["stage: staged table written now"] <- isTRUE(file.mtime(staged_file) >= started)
wait_until(function() dialog_title() == "Electrode table", "Electrode table dialog")

# Fail-safe: never click a Save button that is not marked for people only
if (!on_page("#electrode_localization-save_btn[mcp-agent-disabled='true']")) {
  stop("'Save to subject' is not marked `mcp-agent-disabled`: stopping before ",
       "the click, which would save to the subject")
}
refused <- tool("tool__shiny_ui_operate", action = "click", target = "save_btn")
checks["save: shiny_ui_operate click refused"] <-
  isTRUE(attr(refused, "is_error")) && grepl("disabled for agents", refused[[1]])
refused2 <- tool("tool__shiny_input_update", inputId = "save_btn", value = "")
checks["save: shiny_input_update refused"] <- isTRUE(attr(refused2, "is_error"))
Sys.sleep(3)
checks["save: subject's electrode table untouched"] <- identical(file.mtime(saved_file), saved_mtime)
checks["save: dialog still open"] <- dialog_title() == "Electrode table"

operate("dismiss_modal")
Sys.sleep(1)
checks["dialog closed"] <- dialog_title() == ""

check_all(checks, "electrode_localization MCP test")
