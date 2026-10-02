# Test the MCP tools of the Project Overview module the way an agent would,
# without clicking in the browser. A browser session with the module open is
# still needed: input updates round-trip through it.
#
# Nothing is written into the project. The test writes the module's pipeline
# cache (`modules/project_overview/shared`, gitignored),
# `modules/project_overview/settings.yaml` (copy it first and restore it from
# the copy: `git checkout` would also drop uncommitted edits), and an export
# zip in a temporary folder of the app.

module <- "project_overview"
source("agents/skills/build-module-mcp/test-common.R")  # shared MCP test helpers

# test project and settings
project_name     <- "demo"
subject_codes    <- c("DemoSubject", "YAB")
template_subject <- "cvs_avg35_inMNI152"   # installed: nothing is downloaded
do_export        <- TRUE                   # FALSE: skip the (slow) export

# ---- helpers ----------------------------------------------------------------

pipeline <- ravepipeline::pipeline(module, paths = "modules", temporary = TRUE)

# Text of a table (`output_text`). These tables are registered without a
# download, so the reply is their HTML; a table in a hidden tab comes back
# empty, so show its tab first
table_tab <- c(subjects_summary_table = "Subject Summary",
               module_reports_table = "Module Reports",
               electrode_coverage_table = "Electrode Coverage")
table_text <- function(output_id) {
  set_input_wait("output_cardset", table_tab[[output_id]])
  output_text(output_id)
}
has_word <- function(text, word) {
  grepl(sprintf("\\b%s\\b", word), text, perl = TRUE)
}

stopifnot(app_running())
stopifnot(module_open())

installed <- list.dirs(threeBrain::default_template_directory(),
                       full.names = FALSE, recursive = FALSE)
stopifnot("template installed (no download)" = template_subject %in% installed)

# ---- protocol -----------------------------------------------------------------

reply <- mcp("tools/call", list(name = "shidashi_sessions", arguments = no_args))
sessions <- jsonlite::fromJSON(reply$result$content[[1]]$text,
                               simplifyVector = FALSE)
this_module <- Filter(function(m) identical(m$module_id, module),
                      sessions$open_modules)[[1]]
module_tools <- unlist(this_module$tools)
print(module_tools)
stopifnot(
  "3D viewer tools offered" = all(c("tool__rave_3dviewer_get",
                                    "tool__rave_3dviewer_set") %in% module_tools),
  "shiny_ui_operate offered" = "tool__shiny_ui_operate" %in% module_tools
)
manual <- tool("skill_load__rave-module", action = "reference",
               file_name = "references/project_overview.md", pattern = "Drive the module")
stopifnot("manual loads" = !isTRUE(attr(manual, "is_error")))

listed <- jsonlite::fromJSON(tool("tool__module_interactive_script_list")[[1]],
                             simplifyVector = FALSE)
script_names <- vapply(listed$scripts, `[[`, "", "name")
stopifnot(setequal(script_names, c("load_data", "generate_report", "run_analysis",
                                   "export_status", "pipeline_progress")))
stopifnot("every script has a description" = all(vapply(listed$scripts, function(x) {
  !identical(x$description, x$name)
}, FALSE)))

# ---- load the project -------------------------------------------------------------

set_input_wait("loader_project_name", project_name)
load_start <- Sys.time()
loaded <- run_script("load_data")
result <- as.character(loaded$result)
project <- ravecore::as_rave_project(project_name, strict = FALSE)
all_subjects <- project$subjects()
summary_tbl <- pipeline$read("subject_summary")
yes_no <- function(x) ifelse(x, "yes", "no")
expected <- c(
  sprintf("Loaded project %s: %d subjects", project_name, length(all_subjects)),
  sprintf("%s (%d electrodes; imported %s, notch %s, wavelet %s, localized %s; epochs %d, references %d)",
          summary_tbl$Subject, summary_tbl$Electrodes, yes_no(summary_tbl$Imported),
          yes_no(summary_tbl$Notch), yes_no(summary_tbl$Wavelet),
          yes_no(summary_tbl$Localized), summary_tbl$Epoch_tables,
          summary_tbl$Reference_tables)
)
stopifnot(
  "load_data summary" = identical(result, expected),
  "every subject listed" = setequal(summary_tbl$Subject, all_subjects),
  "settings.yaml written by load_data" = file.mtime(settings_file) >= load_start,
  identical(saved_settings()$project_name, project_name)
)
cat("load_data as expected:", result[[1]], "\n")
Sys.sleep(3)

# ---- configure and build ----------------------------------------------------------

set_input_wait("subject_codes", subject_codes, check = same_set(subject_codes))
set_input_wait("group_viewer", TRUE, check = is_true)
set_input_wait("template_subject", template_subject)
for (id in c("electrode_coverage", "subjects_metadata", "epoch_references",
             "module_reports")) {
  set_input_wait(id, TRUE, check = is_true)
}
set_input_wait("module_filter", list(), check = is_empty)
set_input_wait("native_viewer", FALSE, check = is_false)
set_input_wait("validation", FALSE, check = is_false)
stopifnot("Generate Report button is read-only" = isFALSE(input_info("generate_btn")$writable))

build_start <- Sys.time()
reply <- run_script("generate_report")
settings <- saved_settings()
expected_result <- sprintf(paste(
  "Built for project %s: subjects %s; sections: 3D Group Viewer, Electrode",
  "Coverage, Subjects Summary, Epoch & Reference Tables, Module Reports;",
  "template %s; module filter none (all modules). Read the tables with",
  "`shiny_output_result`."), project_name, paste(sort(subject_codes), collapse = ", "),
  template_subject)
stopifnot(
  "generate_report result" = identical(as.character(reply$result), expected_result),
  "settings.yaml written" = file.mtime(settings_file) >= build_start,
  "subject_codes saved" = setequal(unlist(settings$subject_codes), subject_codes),
  "template saved" = identical(settings$template_subject, template_subject),
  "no module filter saved" = !length(settings$module_filter)
)

# The tables: the same rows as the pipeline targets
summary_text <- ""
wait_until(function() {
  summary_text <<- table_text("subjects_summary_table")
  has_word(summary_text, subject_codes[[1]])
}, "the summary table")
group_summary <- pipeline$read("snapshot_group_subject_summary")
stopifnot(
  "summary target = chosen subjects" = setequal(group_summary$Subject, subject_codes),
  "summary table lists the subjects" = all(vapply(subject_codes, function(sc) {
    has_word(summary_text, sc)
  }, FALSE)),
  "summary table lists no other subject" = !any(vapply(
    setdiff(all_subjects, subject_codes), function(sc) {
      has_word(summary_text, sc)
    }, FALSE))
)
reports <- pipeline$read("snapshot_subject_module_reports")
demo_reports <- reports[["DemoSubject"]]
latest <- demo_reports[!duplicated(demo_reports[, c("module", "report_name")]), ]
# the table shows the parsed creation time, not the folder name
latest_times <- format(as.POSIXct(latest$timestamp, format = "%y%m%dT%H%M%S",
                                  tz = "UTC"), "%Y-%m-%d %H:%M:%S")
reports_text <- ""
wait_until(function() {
  reports_text <<- table_text("module_reports_table")
  grepl("Showing", reports_text, fixed = TRUE)
}, "the module reports table")
stopifnot(
  "DemoSubject has reports" = nrow(demo_reports) > 0,
  "the latest report of each module and name is listed" = all(vapply(
    latest_times, function(d) grepl(d, reports_text, fixed = TRUE), FALSE))
)
coverage <- pipeline$read("snapshot_group_electrode_coverage")
coverage_text <- ""
wait_until(function() {
  coverage_text <<- table_text("electrode_coverage_table")
  grepl("Showing", coverage_text, fixed = TRUE)
}, "the electrode coverage table")
stopifnot(
  "coverage columns are the subjects" = setequal(
    setdiff(names(coverage), c("FSLabel", "Total")), subject_codes),
  "coverage table lists the regions" = all(vapply(utils::head(coverage$FSLabel, 5),
    function(label) grepl(label, coverage_text, fixed = TRUE), FALSE))
)
cat("Tables as expected\n")

# ---- tabs and the viewer ----------------------------------------------------------

set_input_wait("output_cardset", "3D Viewer")
wait_input("brain_viewer_selector", function(value) identical(unlist(value), "Group Brain"))
wait_until(function() {
  text <- tool("tool__rave_3dviewer_get", outputId = "brain_widget",
               name = "controllers", .quiet = TRUE)
  !isTRUE(attr(text, "is_error")) &&
    length(jsonlite::fromJSON(text[[1]])$controllers) > 0
}, "the group viewer's controllers", timeout = 120, interval = 3)
set_input_wait("output_cardset", "Subject Summary")
cat("Tabs and viewer as expected\n")

# ---- guard case: scripts refused while the loader is open ---------------------------

clicked <- tool("tool__shiny_ui_operate", action = "click",
                target = "a.nav-link[rave-action*='toggle_loader']")
wait_until(function() {
  text <- tool("tool__module_interactive_script_run", name = "generate_report",
               .quiet = TRUE)
  isTRUE(attr(text, "is_error")) && grepl("Script not started", text[[1]], fixed = TRUE)
}, "generate_report to be refused while the loader is open")
tool("tool__shiny_ui_operate", action = "click",
     target = "a.nav-link[rave-action*='toggle_loader']")
wait_until(function() {
  listed <- jsonlite::fromJSON(tool("tool__module_interactive_script_list",
                                    .quiet = TRUE)[[1]])
  startsWith(listed$note, "Data loaded")
}, "the loader to close")
cat("Guard case as expected\n")

# ---- export ---------------------------------------------------------------------------

status <- as.character(run_script("export_status")$result)
if (!do_export) {
  cat("do_export is FALSE: skipping the export\n")
} else {
  reply <- run_script("run_analysis")
  wait_until(function() {
    status <<- as.character(run_script("export_status", .quiet = TRUE)$result)
    cat("export_status:", status, "\n")
    !startsWith(status, "Export running") && !startsWith(status, "Export job finished")
  }, "the export", timeout = 900, interval = 10)
  stopifnot("export finished" = startsWith(status, "Export finished"))
  zip_path <- sub("^Export finished: (.+) \\(the dialog.*$", "\\1", status)
  files <- utils::unzip(zip_path, list = TRUE)$Name
  stopifnot(
    "zip exists" = file.exists(zip_path),
    "zip holds the report" = any(grepl("\\.html$", files))
  )
  wait_until(function() grepl("Download packaged report",
                              page_text(".modal.show .modal-title"), fixed = TRUE),
             "the download dialog")
  tool("tool__shiny_ui_operate", action = "dismiss_modal")
  wait_until(function() !nzchar(page_text(".modal.show .modal-title")),
             "the dialog to close")
  cat("Export as expected:", zip_path, "\n")
}

cat("\nAll project_overview MCP checks passed; nothing was written into the project.\n")
