# Test the MCP tools of the Electrode Localization module the way an agent
# would, without clicking in the browser. A browser session with the module
# open is still needed: input updates and clicks round-trip through it.
#
# The test writes the subject's staged electrode table
# (meta/electrodes_unsaved.csv, geometry_unsaved.json) by loading and staging;
# after each plan change it restores the staged table and deletes the backup
# that loading made. It never saves to the subject: it checks that agents'
# clicks on 'Save to subject' are refused. It clicks 'Run YAEL preprocess
# pipeline' only to check the refusal (the subject has a co-registration and
# nothing is dropped). Back up the subject's meta folder and
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
short_plan <- full_plan[1]   # leaves out the placed channels 13-48

# ---- helpers ----------------------------------------------------------------

subject <- ravecore::as_rave_subject(sprintf("%s/%s", project_name, subject_code),
                                     strict = FALSE)
staged_file <- file.path(subject$meta_path, "electrodes_unsaved.csv")
saved_file <- file.path(subject$meta_path, "electrodes.csv")

# label, type, and channels of each group (plans in this test can share labels)
plan_signature <- function(value) {
  vapply(unname(value)[vapply(unname(value), is.list, FALSE)], function(group) {
    paste(group$label, group$type, group$dimension, collapse = "")
  }, "")
}
same_plan <- function(plan) {
  function(value) identical(plan_signature(value), plan_signature(plan))
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

# Drag & drop preprocessing refuses a subject that has a co-registered CT (it
# also refuses without dropped files, so nothing can run here)
coreg_dir <- file.path(subject$imaging_path, "coregistration")
coreg_mtimes <- file.mtime(list.files(coreg_dir, full.names = TRUE))
set_input("loader_preprocess_run", "")
wait_until(function() nzchar(alert_text()), "preprocess refusal alert", timeout = 30)
checks["preprocess: refused, names the co-registered CT"] <-
  grepl("already has a co-registered CT (CT_RAW.nii.gz)", alert_text(), fixed = TRUE)
operate("close_alert2")
Sys.sleep(1)
checks["preprocess: co-registration untouched"] <-
  identical(file.mtime(list.files(coreg_dir, full.names = TRUE)), coreg_mtimes)

# Plan changes warn and back up the staged table, which is restored after each
staged_copy <- tempfile(fileext = ".csv")
file.copy(staged_file, staged_copy)
staged_before <- read.csv(staged_copy)
list_backups <- function() {
  list.files(subject$meta_path, pattern = "^electrodes_unsaved_\\[backup_", full.names = TRUE)
}
backups_before <- list_backups()
restore_staged <- function() {
  file.copy(staged_copy, staged_file, overwrite = TRUE)
  unlink(setdiff(list_backups(), backups_before))
}
coords_of <- function(tbl, chans) {
  unname(as.matrix(tbl[match(chans, tbl$Electrode), c("Coord_x", "Coord_y", "Coord_z")]))
}
toast_text <- function() page_text(".toast-container")
# the warning shows once the module opens (ravedash clears notifications then)
warning_shown <- function() {
  tryCatch({
    wait_until(function() grepl("Electrode plan changed", toast_text(), fixed = TRUE),
               "the 'Electrode plan changed' notification", timeout = 20)
  }, error = function(e) FALSE)
}

# A plan without LB-LD removes their placed channels from the staged table
set_input_wait("loader_plan", short_plan, check = same_plan(short_plan))
loaded <- run_script("load_data")
wait_until(data_loaded, "data loaded", timeout = 120)
staged <- read.csv(staged_file)
new_backups <- setdiff(list_backups(), backups_before)
checks["plan without groups: result warns about channels 13-48"] <-
  any(grepl("^warning=Channels 13-48 are no longer in the plan", loaded$result))
checks["plan without groups: staged table keeps channels 1-12 and their positions"] <-
  identical(sort(staged$Electrode), 1:12) &&
  isTRUE(all.equal(coords_of(staged, 1:12), coords_of(staged_before, 1:12)))
checks["plan without groups: previous staged table backed up"] <-
  length(new_backups) == 1 &&
  unname(tools::md5sum(new_backups) == tools::md5sum(staged_copy))
checks["plan without groups: people see the warning"] <- isTRUE(warning_shown())
restore_staged()

# Planning AHL with another 12-contact model resets AHL only
model_plan <- full_plan
model_plan[[1]]$type <- "SEEG-DIXI-D08-12AM"
set_input_wait("loader_plan", model_plan, check = same_plan(model_plan))
loaded <- run_script("load_data")
wait_until(data_loaded, "data loaded", timeout = 120)
staged <- read.csv(staged_file)
checks["model change: result warns about AHL"] <-
  any(grepl("^warning=AHL \\(channels 1-12\\): electrode model changed", loaded$result))
checks["model change: AHL reset, other groups kept"] <-
  all(coords_of(staged, 1:12) == 0) &&
  isTRUE(all.equal(coords_of(staged, 13:48), coords_of(staged_before, 13:48)))
checks["model change: people see the warning"] <- isTRUE(warning_shown())
restore_staged()

# Load with the full plan
set_input_wait("loader_plan", full_plan, check = same_plan(full_plan))
loaded <- run_script("load_data")
checks["load: result names the subject"] <-
  any(grepl(sprintf("subject=%s/%s", project_name, subject_code), loaded$result, fixed = TRUE))
checks["load: result lists the groups"] <-
  any(grepl("AHL (SEEG-ADTECH-SD12R-SP05X-000, 12 channels)", loaded$result, fixed = TRUE))
wait_until(data_loaded, "data loaded", timeout = 120)
Sys.sleep(3)
checks["load: unchanged plan, no warning"] <-
  !any(grepl("^warning=", loaded$result)) && !grepl("Electrode plan changed", toast_text(), fixed = TRUE)
checks["load: staged positions kept"] <-
  isTRUE(all.equal(coords_of(read.csv(staged_file), 1:48), coords_of(staged_before, 1:48)))

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
