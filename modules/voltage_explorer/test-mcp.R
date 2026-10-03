# Test the MCP tools of the Voltage Explorer module the way an agent would,
# without clicking in the browser. A browser session with the module open is
# still needed: input updates round-trip through it.
#
# The analysis writes the module's pipeline cache (`modules/voltage_explorer/
# shared`, gitignored), `modules/voltage_explorer/settings.yaml`, and the
# module's preferences (plot options and CRP parameters: the keys
# `voltage_explorer.*` of RAVE's global preference store,
# `ravepipeline:::global_preferences("default")`, shared by every session).
# Copy them first and restore them from the copies (`git checkout` would also
# drop uncommitted edits). Unless `do_write` is FALSE, the test then writes one HTML
# report into the subject: `reports/report-univariateVoltage_datetime-<time>_
# voltage_explorer/`.

module <- "voltage_explorer"
source("agents/skills/build-module-mcp/test-common.R")  # shared MCP test helpers

# test subject and settings
project_name      <- "demo"
subject_code      <- "DemoSubject"
epoch_name        <- "auditory_onset"
epoch_window      <- c(-1, 2)          # trial window, seconds
reference_name    <- "default"
loaded_electrodes <- "13-16,24"
band_pass         <- c(1, 30)          # FIR (least squares) band-pass, Hz
baseline_window   <- c(-1, 0)
drift_method      <- "detrend+demean"
analysis_event    <- "Trial Onset"
condition_groups  <- list(
  list(label = "A", conditions = c("drive_a", "known_a", "last_a", "meant_a")),
  list(label = "AV", conditions = c("drive_av", "known_av", "last_av", "meant_av"))
)
crp_window        <- c(0.01, 1)
channel_filter    <- list(list(name = "any:t_proj", operator = "or",
                               criteria = "abs_gte", threshold = "2"))
do_write          <- TRUE              # FALSE: stop before the report

# ---- helpers ----------------------------------------------------------------

# Condition groups (compound input, or saved settings): same labels, same
# conditions (in any order)
same_groups <- function(expected) {
  function(value) {
    value <- unname(value)
    length(value) == length(expected) && all(mapply(function(v, e) {
      identical(as.character(v$label), e$label) &&
        identical(sort(as.character(unlist(v$conditions))), sort(e$conditions))
    }, value, expected))
  }
}

get_subject <- function() {
  ravecore::as_rave_subject(sprintf("%s/%s", project_name, subject_code),
                            strict = FALSE)
}

# The filter and analysis inputs of this test, set in dependency order
set_analysis_inputs <- function() {
  set_input_wait("passing_filter_enabled", TRUE, check = is_true)
  set_input_wait("passing_filter_type", "band_pass")
  set_input_wait("passing_filter_method", "firls")
  set_input_wait("passing_freq1", band_pass[[1]], check = same_number(band_pass[[1]]))
  set_input_wait("passing_freq2", band_pass[[2]], check = same_number(band_pass[[2]]))
  set_input_wait("bandstop_filter_enabled", FALSE, check = is_false)
  set_input_wait("remove_drift_method", drift_method)
  set_input_wait("pre_downsample_factor_auto", TRUE, check = is_true)
  set_input_wait("post_downsample_factor_auto", TRUE, check = is_true)
  set_input_wait("enable_baseline_method", TRUE, check = is_true)
  set_input_wait("baseline_window", baseline_window, check = same_number(baseline_window))
  set_input_wait("analysis_event", analysis_event)
  set_input_wait("condition_groups", condition_groups,
                 check = same_groups(condition_groups))
  set_input_wait("crp_detection_window", crp_window, check = same_number(crp_window))
  set_input_wait("crp_remove_artifacts", TRUE, check = is_true)
}

# The filter list `run_analysis` should save. The automatic decimation factors
# follow the module's rules: before the filters keep 3 x max(cutoff, 300 Hz),
# after them 4 x max(cutoff, 50 Hz)
expected_filters <- function(sample_rate) {
  pre <- max(floor(sample_rate / ceiling(max(c(band_pass, 300)) * 3)), 1)
  post <- max(floor(sample_rate / pre / ceiling(max(c(band_pass, 50)) * 4)), 1)
  list(
    list(type = "detrend"), list(type = "demean"),
    list(type = "decimate", by = pre, auto = TRUE),
    list(type = "firls", high_pass_freq = band_pass[[1]],
         low_pass_freq = band_pass[[2]]),
    list(type = "decimate", by = post, auto = TRUE),
    list(type = "baseline", windows = baseline_window)
  )
}
same_filters <- function(actual, expected) {
  length(actual) == length(expected) && all(mapply(function(a, e) {
    identical(names(a)[order(names(a))], names(e)[order(names(e))]) &&
      all(vapply(names(e), function(nm) {
        isTRUE(all.equal(unlist(a[[nm]]), unlist(e[[nm]]), check.attributes = FALSE))
      }, FALSE))
  }, actual, expected))
}

# Stop unless the saved settings match the inputs of this test
check_saved_settings <- function(since, sample_rate) {
  settings <- saved_settings()
  checks <- c(
    "settings.yaml written by this run" = isTRUE(file.mtime(settings_file) >= since),
    "filter_configurations" = same_filters(settings$filter_configurations,
                                           expected_filters(sample_rate)),
    "analysis_event" = identical(settings$analysis_event, analysis_event),
    "condition groups" = same_groups(condition_groups)(settings$condition_groups),
    "crp_detection_window" = isTRUE(all.equal(
      as.numeric(unlist(settings$crp_detection_window)), crp_window)),
    "crp_remove_artifacts" = isTRUE(settings$crp_remove_artifacts)
  )
  print(data.frame(check = names(checks), passed = unname(checks)),
        row.names = FALSE)
  if (!all(checks)) stop("settings.yaml does not match the inputs")
  invisible(settings)
}

pipeline <- ravepipeline::pipeline(module, paths = "modules", temporary = TRUE)
shared <- pipeline$shared_env()

# The register of report folders
list_reports <- function() {
  sort(list.dirs(get_subject()$report_path, recursive = FALSE))
}

# 3D viewer helpers (tools `rave_3dviewer_get` / `rave_3dviewer_set`)
viewer <- "brain_viewer"

stopifnot(app_running())

tool("switch_module", module_id = "voltage_explorer")
stopifnot(module_open())

test_start <- Sys.time()

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
               file_name = "references/voltage_explorer.md", pattern = "Drive the module")
stopifnot("manual loads" = !isTRUE(attr(manual, "is_error")))

listed <- jsonlite::fromJSON(tool("tool__module_interactive_script_list")[[1]],
                             simplifyVector = FALSE)
script_names <- vapply(listed$scripts, `[[`, "", "name")
stopifnot(setequal(script_names, c(
  "load_data", "run_analysis", "pipeline_progress", "inspect_filters",
  "reset_signal_config", "reset_crp_params", "reset_plot_options",
  "send_to_electrode_selector", "open_report_dialog", "report_status"
)))
stopifnot("every script has a description" = all(vapply(listed$scripts, function(x) {
  !identical(x$description, x$name)
}, FALSE)))

# ---- load the subject -----------------------------------------------------------

set_input_wait("loader_project_name", project_name)
set_input_wait("loader_subject_code", subject_code)
set_input_wait("loader_epoch_name", epoch_name)
set_input_wait("loader_epoch_name__trial_starts", epoch_window[[1]],
               check = same_number(epoch_window[[1]]))
set_input_wait("loader_epoch_name__trial_ends", epoch_window[[2]],
               check = same_number(epoch_window[[2]]))
set_input_wait("loader_reference_name", reference_name)
set_input_wait("loader_electrode_text", loaded_electrodes)

load_start <- Sys.time()
loaded <- run_script("load_data")
subject <- get_subject()
epoch <- subject$get_epoch(epoch_name)$table
sample_rate <- unique(subject$raw_sample_rates[subject$electrode_types == "LFP"])
stopifnot(
  "load_data summary" = startsWith(loaded$result, sprintf(
    "Loaded %s/%s: epoch %s (%d trials, %s to %s s), reference %s, LFP electrodes %s, sample rate %s Hz;",
    project_name, subject_code, epoch_name, nrow(epoch), epoch_window[[1]],
    epoch_window[[2]], reference_name, loaded_electrodes, sample_rate)),
  grepl("drive_a (16)", loaded$result, fixed = TRUE),
  endsWith(loaded$result, "events: Trial Onset")
)
settings <- saved_settings()
stopifnot(
  "settings.yaml written by load_data" = file.mtime(settings_file) >= load_start,
  identical(settings$subject_code, subject_code),
  identical(settings$epoch_choice, epoch_name),
  identical(trimws(settings$loaded_electrodes), loaded_electrodes)
)

# The module initializes its inputs after loading; let that settle
Sys.sleep(3)

# The fixed registration: the detrend select is reachable under its real ID;
# the people-only buttons are read-only
for (id in c("remove_drift_method", "plot_time_start", "by_channel_plot_type",
             "discrete_colormap", "by_cond_channel_selector")) {
  wait_input(id)
}
for (id in c("filter_inspector_btn", "selector_filter_apply", "open_report_modal",
             "signal_config_reset", "crp_params_reset", "plot_options_reset")) {
  stopifnot(isFALSE(input_info(id)$writable))
}

# ---- error case: a band-pass with one cutoff ---------------------------------------

# A known input problem: people get a notification that says what to change,
# not ravedash's "Coding Error", and agents get the same reason
one_cutoff <- "A band-pass filter requires two cutoff frequencies"
notifications <- function() page_text(".toast-container")

set_input_wait("passing_filter_enabled", TRUE, check = is_true)
set_input_wait("passing_filter_type", "band_pass")
set_input_wait("passing_freq1", band_pass[[1]], check = same_number(band_pass[[1]]))
# An empty string clears a numeric input (JSON null would be ignored)
set_input_wait("passing_freq2", "", check = function(value) {
  !length(unlist(value)) || is.na(unlist(value)[[1]])
})
operate("remove_notification")
before <- file.mtime(settings_file)
text <- tool("tool__module_interactive_script_run", name = "run_analysis")
stopifnot(
  "run_analysis fails" = isTRUE(attr(text, "is_error")),
  "the reason" = grepl(one_cutoff, text[[1]], fixed = TRUE),
  "settings.yaml unchanged" = identical(file.mtime(settings_file), before)
)
wait_until(function() grepl(one_cutoff, notifications(), fixed = TRUE),
           "the notification")
stopifnot("no alert" = !nzchar(alert_text()))

# The same for a person clicking 'Run analysis' in the footer
operate("remove_notification")
operate("click", target = ".ravedash-footer button.rave-button-autorecalculate")
wait_until(function() grepl(one_cutoff, notifications(), fixed = TRUE),
           "the notification after a click")
Sys.sleep(1)
stopifnot(
  "no Coding Error" = !grepl("Coding Error", notifications(), fixed = TRUE),
  "settings.yaml unchanged after the click" =
    identical(file.mtime(settings_file), before)
)
operate("remove_notification")
cat("Error case (one cutoff) as expected\n")

# ---- the filter inspector -----------------------------------------------------------

set_analysis_inputs()
reply <- run_script("inspect_filters")
stopifnot(startsWith(as.character(reply$result), "The 'Filter Inspector' dialog is open"))
wait_until(function() {
  plot <- tool("tool__shiny_query_ui", css_selector = sprintf(
    "#%s-filter_inspector_plot", module), .quiet = TRUE)
  any(grepl("^\\[image", plot))
}, "the filter inspector plot")
tool("tool__shiny_ui_operate", action = "dismiss_modal")
wait_until(function() !nzchar(page_text(".modal.show .modal-title")), "the dialog to close")
cat("Filter inspector as expected\n")

# ---- run the analysis -----------------------------------------------------------------

run_start <- Sys.time()
reply <- run_script("run_analysis")
settings <- check_saved_settings(run_start, sample_rate)
erp_tbl <- pipeline$read("erp_results_for_viewer")
groups <- vapply(settings$condition_groups, function(group) {
  sprintf("%s (%s)", group$label, paste(unlist(group$conditions), collapse = ", "))
}, "")
f <- expected_filters(sample_rate)
expected_result <- sprintf(paste(
  "Analysis done: electrodes %s; filters: detrend, demean, decimate by %s,",
  "firls band-pass %s-%s Hz, decimate by %s, baseline %s to %s s; event %s;",
  "groups: %s; CRP window %s to %s s, artifacts removed; metrics per electrode",
  "in output `crp_viewer_table`"),
  dipsaus::deparse_svec(erp_tbl$Electrode), f[[3]]$by, band_pass[[1]],
  band_pass[[2]], f[[5]]$by, baseline_window[[1]], baseline_window[[2]],
  analysis_event, paste(groups, collapse = "; "), crp_window[[1]], crp_window[[2]])
stopifnot(
  "run_analysis result" = identical(as.character(reply$result), expected_result),
  # `data_placeholder` has cue "always": every run rebuilds it (the metrics
  # themselves are rebuilt only when their inputs changed)
  "the pipeline ran" = target_time("data_placeholder") >= run_start,
  "every loaded electrode analyzed" = identical(
    as.integer(erp_tbl$Electrode), as.integer(dipsaus::parse_svec(loaded_electrodes)))
)

# The results table: the same numbers as the pipeline target
table_csv <- utils::read.csv(text = output_data("crp_viewer_table"),
                             check.names = FALSE)
metric_columns <- intersect(names(erp_tbl), names(table_csv))
metric_columns <- setdiff(metric_columns, "Electrode")
stopifnot(
  "table rows" = identical(as.integer(table_csv$Electrode), as.integer(erp_tbl$Electrode)),
  "table has metric columns" = length(metric_columns) > 0,
  "table values" = all(vapply(metric_columns, function(nm) {
    isTRUE(all.equal(as.numeric(table_csv[[nm]]), as.numeric(erp_tbl[[nm]]),
                     tolerance = 1e-6))
  }, FALSE))
)
cat("crp_viewer_table equals erp_results_for_viewer:", length(metric_columns),
    "metric columns\n")

# A registered figure, read while its tab is hidden
figure <- tool("tool__shiny_output_result", outputId = "figure_data_crp_param_snr")
stopifnot("figure comes back as an image" = any(grepl("^\\[image", figure)))

# ---- plot options ---------------------------------------------------------------------

set_input_wait("by_channel_plot_type", "heatmap")
set_input_wait("plot_time_start", -0.2, check = same_number(-0.2))
set_input_wait("plot_time_end", 0.8, check = same_number(0.8))
figure <- tool("tool__shiny_output_result", outputId = "figure_data_by_channel_condition")
stopifnot("heatmap figure comes back as an image" = any(grepl("^\\[image", figure)))
set_input_wait("by_cond_channel_selector", "14")
figure <- tool("tool__shiny_output_result",
               outputId = "figure_data_by_trial_channel_condition_butterfly")
stopifnot("single-channel figure comes back as an image" = any(grepl("^\\[image", figure)))

reply <- run_script("reset_plot_options")
wait_input("by_channel_plot_type", function(value) identical(unlist(value), "multiline"))
wait_input("plot_cex", same_number(1.2))
cat("Plot options and reset as expected\n")

# A cleared or unknown colormap name must not close the session: the colormap
# reactive is a `bindEvent()` trigger of the 3D-viewer observer, evaluated
# outside `safe_observe()`'s protection. The widget itself refuses to stay
# empty (`shidashi` >= 0.2.0.13 puts the previous palette back), so the value
# read back is a palette name either way.
set_input("continuous_colormap", "")          # what an emptied selectize sends
Sys.sleep(2)
stopifnot("session survives an emptied colormap" = module_open())
figure <- tool("tool__shiny_output_result", outputId = "figure_data_crp_param_alpha_prime")
stopifnot("figure draws after the emptied colormap" = any(grepl("^\\[image", figure)))
palette <- unlist(input_info("continuous_colormap")$current_value)
stopifnot("the selector holds a palette name" = length(palette) == 1 && nzchar(palette))
set_input_wait("continuous_colormap", "viridis")
set_input_wait("continuous_colormap", "default")
cat("Emptied colormap selector survived (value read back:", palette, ")\n")

# ---- channel filter -> electrode selector ----------------------------------------------

set_input_wait("crp_channel_filter", channel_filter, check = function(value) {
  length(value) == 1 && identical(value[[1]]$name, channel_filter[[1]]$name) &&
    identical(as.character(value[[1]]$threshold), channel_filter[[1]]$threshold)
})
reply <- run_script("send_to_electrode_selector")
expected_selection <- shared$selector_filter_electrodes(erp_tbl, channel_filter)
expected_text <- dipsaus::deparse_svec(
  expected_selection %||% dipsaus::parse_svec(loaded_electrodes))
wait_input("electrode_text", function(value) identical(unlist(value), expected_text))
stopifnot("result lists the electrodes" =
            grepl(expected_text, as.character(reply$result), fixed = TRUE))
cat("send_to_electrode_selector as expected:", expected_text, "\n")

# ---- 3D viewer ------------------------------------------------------------------------

wait_until(function() {
  controllers <- tryCatch(viewer_get("controllers")$controllers,
                          error = function(e) NULL)
  identical(controllers[["Threshold Data"]], "selector_filter")
}, "the 3D viewer's threshold data", timeout = 60, interval = 2)
cat("3D viewer thresholds on selector_filter\n")

# ---- error case: the filter inspector without filters ---------------------------------

set_input_wait("passing_filter_enabled", FALSE, check = is_false)
set_input_wait("bandstop_filter_enabled", FALSE, check = is_false)
text <- tool("tool__module_interactive_script_run", name = "inspect_filters")
stopifnot(
  "inspect_filters fails" = isTRUE(attr(text, "is_error")),
  "the reason" = grepl("Filter inspector will not launch", text[[1]], fixed = TRUE)
)
Sys.sleep(1)
stopifnot("no dialog" = !nzchar(page_text(".modal.show .modal-title")))
cat("Error case (no filter) as expected\n")
set_analysis_inputs()

# ---- the HTML report (writes into the subject) ----------------------------------------

if (!do_write) {
  cat("\ndo_write is FALSE: stopping before the report.\n")
} else {
  reports_before <- list_reports()
  reply <- run_script("open_report_dialog")
  stopifnot(startsWith(as.character(reply$result), "The report dialog is open"))
  wait_until(function() nzchar(page_text(sprintf("#%s-do_generate_report", module))),
             "the report dialog")
  report_start <- Sys.time()
  clicked <- tool("tool__shiny_ui_operate", action = "click",
                  target = "do_generate_report")
  stopifnot(!isTRUE(attr(clicked, "is_error")))
  status <- ""
  wait_until(function() {
    status <<- as.character(run_script("report_status", .quiet = TRUE)$result)
    cat("report_status:", status, "\n")
    startsWith(status, "Report finished") || startsWith(status, "Report errored") ||
      startsWith(status, "The report job has ended")
  }, "the report", timeout = 600, interval = 10)
  stopifnot("report finished" = startsWith(status, "Report finished"))
  new_reports <- setdiff(list_reports(), reports_before)
  report_file <- sub("^Report finished: ", "", status)
  stopifnot(
    "one new report folder" = length(new_reports) == 1,
    "report.html in it" = identical(normalizePath(dirname(report_file)),
                                    normalizePath(new_reports[[1]])),
    "written by this run" = file.mtime(report_file) >= report_start,
    "folder name" = grepl("^report-univariateVoltage_datetime-.+_voltage_explorer$",
                          basename(new_reports[[1]]))
  )
  cat("Report written:", new_reports[[1]], "\n")
}

cat("\nAll voltage_explorer MCP checks passed.\n")
