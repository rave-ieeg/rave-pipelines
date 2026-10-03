# Test the MCP tools of the Power Clustering module the way an agent would,
# without clicking in the browser. A browser session with the module open is
# still needed: input updates round-trip through it.
#
# The test writes nothing into the subject. It writes the module's pipeline
# cache (`modules/power_clust/shared`, gitignored) and
# `modules/power_clust/settings.yaml`: copy that file first and restore it
# from the copy (`git checkout` would also drop uncommitted edits).

module <- "power_clust"
source("agents/skills/build-module-mcp/test-common.R")  # shared MCP test helpers

# test subject and settings
project_name      <- "demo"
subject_code      <- "DemoSubject"
epoch_name        <- "auditory_onset"
epoch_window      <- c(-1, 2)          # trial window, seconds
reference_name    <- "default"
loaded_electrodes <- "13-16,24"
time_range        <- c(0, 1)           # analysis window, seconds
frequency_range   <- c(70, 150)        # Hz
zeta_threshold    <- 0.5
baseline_window   <- c(-1, 0)
baseline_unit     <- "Decibel"
baseline_scope    <- "Per frequency, trial, and electrode"
condition_groups  <- list(
  list(group_name = "Auditory",
       group_conditions = c("drive_a", "known_a", "last_a", "meant_a"),
       group_start_event = "Trial Onset", group_finish_event = "[Analysis end]"),
  list(group_name = "AudioVisual",
       group_conditions = c("drive_av", "known_av", "last_av", "meant_av"),
       group_start_event = "Trial Onset", group_finish_event = "[Analysis end]")
)
narrow_frequency_range <- c(3, 9)      # contains no wavelet frequency
narrow_time_range      <- c(-1, 0)     # ends at the event: no duration

# ---- helpers ----------------------------------------------------------------

# Condition groups (compound input, or saved settings): same names, same
# conditions (in any order), same events
same_groups <- function(expected) {
  function(value) {
    value <- unname(value)
    length(value) == length(expected) && all(mapply(function(v, e) {
      identical(as.character(v$group_name), e$group_name) &&
        identical(sort(as.character(unlist(v$group_conditions))),
                  sort(e$group_conditions)) &&
        identical(as.character(v$group_start_event), e$group_start_event) &&
        identical(as.character(v$group_finish_event), e$group_finish_event)
    }, value, expected))
  }
}

get_subject <- function() {
  ravecore::as_rave_subject(sprintf("%s/%s", project_name, subject_code),
                            strict = FALSE)
}

# The analysis inputs of this test
set_analysis_inputs <- function() {
  set_input_wait("time_range", time_range, check = same_number(time_range))
  set_input_wait("frequency_range", frequency_range,
                 check = same_number(frequency_range))
  set_input_wait("zeta_threshold", zeta_threshold,
                 check = same_number(zeta_threshold))
  set_input_wait("baseline_choices__windows",
                 list(list(window_interval = baseline_window)),
                 check = same_number(baseline_window))
  set_input_wait("baseline_choices__unit_of_analysis", baseline_unit)
  set_input_wait("baseline_choices__global_baseline_choice", baseline_scope)
  set_input_wait("condition_groups", condition_groups,
                 check = same_groups(condition_groups))
}

# Stop unless the saved settings match the inputs of this test
check_saved_settings <- function(since) {
  settings <- saved_settings()
  checks <- c(
    "settings.yaml written by this run" = isTRUE(file.mtime(settings_file) >= since),
    "analysis_window" = isTRUE(all.equal(
      as.numeric(unlist(settings$analysis_window)), time_range)),
    "frequency_range" = isTRUE(all.equal(
      as.numeric(unlist(settings$frequency_range)), frequency_range)),
    "zeta_threshold" = isTRUE(all.equal(settings$zeta_threshold, zeta_threshold)),
    "baseline windows" = isTRUE(all.equal(
      as.numeric(unlist(settings$baseline__windows)), baseline_window)),
    "baseline unit" = identical(settings$baseline__unit_of_analysis, baseline_unit),
    "baseline scope" = identical(settings$baseline__global_baseline_choice,
                                 baseline_scope),
    "condition groups" = same_groups(condition_groups)(settings$condition_groups)
  )
  print(data.frame(check = names(checks), passed = unname(checks)),
        row.names = FALSE)
  if (!all(checks)) stop("settings.yaml does not match the inputs")
  invisible(settings)
}

# The lines `cluster_summary` (and `run_analysis`) should give, built here
# from the pipeline targets
pipeline <- ravepipeline::pipeline(module, paths = "modules", temporary = TRUE)
expected_cluster_lines <- function(k, k_source) {
  res <- pipeline$read(c("clustering_tree", "combined_group_results",
                         "clustering_index"))
  channels <- res$combined_group_results$electrode_channels
  clusters <- stats::cutree(res$clustering_tree$cluster_object, k = k)
  scores <- res$clustering_index$scores
  c(
    sprintf("Clusters at k=%d (%s) of electrodes %s; groups: %s", k, k_source,
            dipsaus::deparse_svec(channels),
            paste(res$combined_group_results$group_labels, collapse = ", ")),
    vapply(seq_len(k), function(ii) {
      sprintf("cluster %d (n=%d): %s", ii, sum(clusters == ii),
              dipsaus::deparse_svec(channels[clusters == ii]))
    }, ""),
    sprintf("Silhouette score by k: %s; suggested k=%s",
            paste(sprintf("%d=%.3f", scores$k, scores$silhouette), collapse = ", "),
            res$clustering_index$suggested$k)
  )
}
same_lines <- function(actual, expected, what) {
  actual <- as.character(actual)
  if (!identical(actual, expected)) {
    cat("Expected:\n"); writeLines(expected)
    cat("Got:\n"); writeLines(actual)
    stop(what, " differs from the pipeline targets")
  }
  cat(what, "as expected:\n"); writeLines(paste(" ", expected))
}

# The settings line `run_analysis` should start with, from settings.yaml
expected_run_summary <- function() {
  settings <- saved_settings()
  groups <- vapply(settings$condition_groups, function(group) {
    sprintf("%s (%s) [%s to %s]", group$group_name,
            paste(unlist(group$group_conditions), collapse = ", "),
            group$group_start_event, group$group_finish_event)
  }, "")
  sprintf(
    paste(
      "Clustering done: window %s to %s s; frequencies %s-%s Hz; zeta %s;",
      "baseline %s to %s s, %s, %s; groups: %s"
    ),
    time_range[[1]], time_range[[2]], frequency_range[[1]], frequency_range[[2]],
    zeta_threshold, baseline_window[[1]], baseline_window[[2]], baseline_unit,
    baseline_scope, paste(groups, collapse = "; ")
  )
}

# 3D viewer helpers (tools `rave_3dviewer_get` / `rave_3dviewer_set`)
viewer <- "viewer"

stopifnot(app_running())
stopifnot(module_open())

test_start <- Sys.time()

# ---- protocol -----------------------------------------------------------------

app_id <- jsonlite::fromJSON(mcp_url)$app_id
cat("app id:", app_id, "\n")

# The app lists the tools of all modules; check this module's own list
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
               file_name = "references/power_clust.md", pattern = "Drive the module")
stopifnot("manual loads" = !isTRUE(attr(manual, "is_error")))

listed <- jsonlite::fromJSON(tool("tool__module_interactive_script_list")[[1]],
                             simplifyVector = FALSE)
script_names <- vapply(listed$scripts, `[[`, "", "name")
stopifnot(setequal(script_names, c("load_data", "run_analysis",
                                   "cluster_summary", "pipeline_progress")))
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

# On a freshly opened page the module's analysis inputs do not exist before
# the first load. A reused page keeps its results when the same data are
# loaded again (the module skips re-initializing unchanged data)
fresh_page <- !isTRUE(input_info("n_clusters")$exists)

load_start <- Sys.time()
loaded <- run_script("load_data")
subject <- get_subject()
epoch <- subject$get_epoch(epoch_name)$table
frequencies <- utils::read.csv(file.path(subject$meta_path, "frequencies.csv"))$Frequency
stopifnot(
  "load_data summary" = startsWith(loaded$result, sprintf(
    "Loaded %s/%s: epoch %s (%d trials, %s to %s s), reference %s, electrodes %s;",
    project_name, subject_code, epoch_name, nrow(epoch), epoch_window[[1]],
    epoch_window[[2]], reference_name, loaded_electrodes)),
  grepl("drive_a (16)", loaded$result, fixed = TRUE),
  grepl("events: Trial Onset;", loaded$result, fixed = TRUE),
  endsWith(loaded$result, sprintf("frequencies %s-%s Hz (%d)", min(frequencies),
                                  max(frequencies), length(frequencies)))
)
settings <- saved_settings()
stopifnot(
  "settings.yaml written by load_data" = file.mtime(settings_file) >= load_start,
  identical(settings$project_name, project_name),
  identical(settings$subject_code, subject_code),
  identical(settings$epoch_choice, epoch_name),
  isTRUE(all.equal(as.numeric(c(settings$epoch_choice__trial_starts,
                                settings$epoch_choice__trial_ends)), epoch_window)),
  identical(settings$reference_name, reference_name),
  identical(trimws(settings$loaded_electrodes), loaded_electrodes)
)

# The module initializes its inputs after loading; let that settle
Sys.sleep(3)
if (fresh_page) {
  summary_reply <- run_script("cluster_summary")
  stopifnot("no results right after loading" =
              startsWith(as.character(summary_reply$result)[[1]], "No results yet"))
} else {
  cat("Reused page: skipping the check that loading leaves no results\n")
}

# The newly registered inputs exist; the people-only button is read-only
for (id in c("time_range", "frequency_range", "zeta_threshold", "n_clusters",
             "condition_groups", "cluster_tabset", "btn_load_settings")) {
  wait_input(id)
}
stopifnot("Load Settings is read-only" = isFALSE(input_info("btn_load_settings")$writable))

# ---- run the clustering -------------------------------------------------------------

set_analysis_inputs()
run_start <- Sys.time()
reply <- run_script("run_analysis")
result <- as.character(reply$result)
check_saved_settings(run_start)
stopifnot(
  "run_analysis summary" = identical(result[[1]], expected_run_summary()),
  "clustering_index rebuilt by this run" = target_time("clustering_index") >= run_start
)
suggested_k <- pipeline$read("clustering_index")$suggested$k
same_lines(result[-1], expected_cluster_lines(suggested_k, "suggested"),
           "run_analysis clusters")

# The run sets `n_clusters` to the suggested k; `cluster_summary` follows it
wait_input("n_clusters", same_number(suggested_k))
same_lines(run_script("cluster_summary", .quiet = TRUE)$result,
           expected_cluster_lines(suggested_k, "`n_clusters`"), "cluster_summary")

# ---- tabs: the table and the plots of the hidden tabs ----------------------------------

set_input_wait("cluster_tabset", "Clustering table")
clusters <- stats::cutree(pipeline$read("clustering_tree")$cluster_object,
                          k = suggested_k)
channels <- pipeline$read("combined_group_results")$electrode_channels
wait_until(function() {
  table_text <- page_text(sprintf("#%s-cluster_table", module))
  all(vapply(seq_len(suggested_k), function(ii) {
    grepl(dipsaus::deparse_svec(channels[clusters == ii]), table_text, fixed = TRUE)
  }, FALSE))
}, "the clustering table to list the clusters")
set_input_wait("cluster_tabset", "Diagnostic plots")
mean_plot <- tool("tool__shiny_query_ui",
                  css_selector = sprintf("#%s-cluster_mean_plot", module))
stopifnot("mean plot pictured" = any(grepl("^\\[image", mean_plot)))
set_input_wait("cluster_tabset", "Channel time-series")

# ---- another k: cut the same tree, no re-run ------------------------------------------

scored_k <- pipeline$read("clustering_index")$scores$k
other_k <- setdiff(scored_k, suggested_k)
new_k <- if (length(other_k)) other_k[[1]] else 1
set_input_wait("n_clusters", new_k, check = same_number(new_k))
wait_until(function() {
  identical(as.character(run_script("cluster_summary", .quiet = TRUE)$result),
            expected_cluster_lines(new_k, "`n_clusters`"))
}, sprintf("cluster_summary at k=%d", new_k))
cat("cluster_summary follows n_clusters =", new_k, "\n")

# The viewer shows the clusters (its values are reported with a delay)
wait_until(function() {
  controllers <- tryCatch(viewer_get("controllers")$controllers,
                          error = function(e) NULL)
  identical(controllers[["Display Data"]], "Cluster")
}, "the 3D viewer to show display data 'Cluster'", timeout = 60, interval = 2)
cat("3D viewer shows display data 'Cluster'\n")

# ---- error case 1: a frequency band with no wavelet frequency -------------------------

stopifnot(!any(frequencies >= narrow_frequency_range[[1]] &
                 frequencies <= narrow_frequency_range[[2]]))
set_input_wait("frequency_range", narrow_frequency_range,
               check = same_number(narrow_frequency_range))
before <- file.mtime(settings_file)
reply <- run_script("run_analysis")
stopifnot(
  "did not run" = startsWith(as.character(reply$result)[[1]], "Clustering did not run"),
  # the error itself, logged after the code of the run
  "error in output" = grepl("Error in [^\n]*: Frequency range is too narrow",
                            reply$output),
  "settings.yaml unchanged" = identical(file.mtime(settings_file), before)
)
# The 'Error found!' alert stays open until it is closed
wait_until(function() grepl("Frequency range is too narrow", alert_text(), fixed = TRUE),
           "the error alert")
closed <- tool("tool__shiny_ui_operate", action = "close_alert2")
stopifnot(!isTRUE(attr(closed, "is_error")))
wait_until(function() !nzchar(alert_text()), "the alert to close")
cat("Error case 1 as expected\n")

# ---- error case 2: a window that ends at the event (a pipeline step fails) -------------

set_input_wait("frequency_range", frequency_range, check = same_number(frequency_range))
set_input_wait("time_range", narrow_time_range, check = same_number(narrow_time_range))
reply <- run_script("run_analysis")
stopifnot(
  "did not run" = startsWith(as.character(reply$result)[[1]], "Clustering did not run"),
  "error in output" = grepl("Possible issue: Analysis time duration is too narrow",
                            reply$output, fixed = TRUE)
)
progress <- as.character(run_script("pipeline_progress")$result)
stopifnot("pipeline_progress names the error" = any(grepl(
  "errored - Analysis time duration is too narrow", progress, fixed = TRUE)))
wait_until(function() grepl("Error while running pipeline",
                            page_text(".toast-container"), fixed = TRUE),
           "the error toast")
Sys.sleep(1)
stopifnot("no alert" = !nzchar(alert_text()))
summary_reply <- run_script("cluster_summary")
stopifnot("no results after a failed run" =
            startsWith(as.character(summary_reply$result)[[1]], "No results yet"))
cat("Error case 2 as expected\n")

# Leave the module with the test's window
set_input_wait("time_range", time_range, check = same_number(time_range))

cat("\nAll power_clust MCP checks passed; nothing was written into the subject.\n")
