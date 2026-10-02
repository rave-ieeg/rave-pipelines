# Test the MCP tools of the Power Explorer module the way an agent would,
# without clicking in the browser. A browser session with the module open is
# still needed: input updates round-trip through it.
#
# Agents may analyze freely; exporting, saving for group analysis, and
# generating reports write into the subject, so agents confirm those with the
# user first. "Cluster -> electrodes.csv" is for people only: its button is
# registered read-only, it has no script, and the module does not offer
# `shiny_ui_operate`, which clicks any element.
#
# Agents may also flag outlier trials (input `flagged_trials`), but only people
# save them into the epoch (column `ExcludedHint`): "Save Flags to Epoch" has
# no script either.
#
# The analyses (quick, full, two factors, custom ROI, clusters) write only the
# module's pipeline cache (`modules/power_explorer/shared`, gitignored) and
# `modules/power_explorer/settings.yaml`. Unless `do_flags` is FALSE, the
# flagged-trials steps write a test epoch `meta/epoch_pe_flag_test*.csv` into
# the subject and delete it at the end of those steps (delete it by hand if
# the test stops earlier). Unless `do_write` is FALSE, the test then writes
# into the subject:
#   * one export folder `power_explorer/pe_export_<time>/` (two electrodes)
#   * two group-analysis saves `pipelines/power_explorer/power_explorer-
#     mcp_test-<time>/`; the second one replaces the first, which the current
#     ravecore keeps (see the replace step)
#   * one HTML report `reports/report-univariatePower_datetime-<time>_power_explorer/`
# A live run rewrites `modules/power_explorer/settings.yaml`: copy it first
# and restore it from the copy (`git checkout` would also drop uncommitted
# edits).

port   <- as.integer(Sys.getenv("RAVE_TEST_PORT", "17283"))  # port used for testing
module <- "power_explorer"

# test subject and settings
project_name        <- "demo"
subject_code        <- "DemoSubject"
epoch_name          <- "auditory_onset"
epoch_window        <- c(-1, 2)          # trial window, seconds
reference_name      <- "default"
loaded_electrodes   <- "13-16,24"
analysis_electrodes <- "14-16"
baseline_window     <- c(-1, 0)
baseline_scope      <- "Per frequency, trial, and electrode"
baseline_unit       <- "% Change Power"
analysis_windows    <- list(
  list(label = "HighGamma", event = "Trial Onset", time = c(0, 1),
       frequency_dd = "Select one", frequency = c(70, 150))
)
first_groups <- list(
  list(label = "Auditory", conditions = c("drive_a", "known_a", "last_a", "meant_a")),
  list(label = "AudioVisual", conditions = c("drive_av", "known_av", "last_av", "meant_av"))
)
second_groups <- list(
  list(label = "drive_known", conditions = c("drive_a", "drive_av", "known_a", "known_av")),
  list(label = "last_meant", conditions = c("last_a", "last_av", "meant_a", "meant_av"))
)
roi_variable <- "FSLabel"
roi_groups <- list(
  list(label = "STG", conditions = I("ctx_lh_G_temp_sup-Lateral")),
  list(label = "Other", conditions = c("ctx_lh_G_pariet_inf-Supramar",
                                       "ctx_lh_G_temporal_middle"))
)
roi_electrodes    <- c(STG = "13-15", Other = "16,24")  # electrodes of each ROI group
export_electrodes <- c(14, 15)
group_label       <- "mcp_test"
report_graphs     <- "over_time_by_condition"
do_write          <- TRUE            # FALSE: stop before writing into the subject
do_flags          <- TRUE            # FALSE: skip the flagged-trials steps (test epoch)
flag_epoch        <- "pe_flag_test"  # copy of `epoch_name`; marks trials 5 (known_a)
                                     # and 15 (last_av), both in `first_groups`

# testing URL
base    <- sprintf("http://127.0.0.1:%d", port)
mcp_url <- paste0(base, "/mcp")
no_args <- structure(list(), names = character(0))   # sent as {}

# ---- helpers ----------------------------------------------------------------

# Send one JSON-RPC request to the app and return the parsed reply
mcp <- function(method, params = no_args, url = mcp_url) {
  httr2::request(url) |>
    httr2::req_body_json(list(jsonrpc = "2.0", id = 1, method = method,
                              params = params)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(900) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# Call a tool, print its reply (unless `.quiet`), and return the reply text
# invisibly; attribute `is_error` tells whether the call failed.
# Tool arguments go in `...`; use list() for JSON arrays.
tool <- function(.name, ..., .url = mcp_url, .quiet = FALSE) {
  args <- list(...)
  if (!length(args)) args <- no_args
  reply <- mcp("tools/call", list(name = .name, arguments = args), url = .url)
  text <- if (is.null(reply$result)) {
    reply$error$message
  } else {
    vapply(reply$result$content, function(item) {
      if (identical(item$type, "text")) return(item$text)
      sprintf("[%s: %s, %d characters]", item$type,
              if (is.null(item$mimeType)) "?" else item$mimeType,
              nchar(if (is.null(item$data)) "" else item$data))
    }, "")
  }
  is_error <- is.null(reply$result) || isTRUE(reply$result$isError)
  if (!.quiet) {
    cat("\n--", .name, if (is_error) "[isError]", "--\n")
    cat(substr(text, 1, 1500), sep = "\n")
  }
  invisible(structure(text, is_error = is_error))
}

app_running <- function() {
  up <- suppressWarnings(try(readLines(mcp_url, warn = FALSE), silent = TRUE))
  !inherits(up, "try-error")
}

module_open <- function() {
  reply <- mcp("tools/call", list(name = "shidashi_sessions", arguments = no_args))
  sessions <- jsonlite::fromJSON(reply$result$content[[1]]$text)
  module %in% sessions$open_modules$module_id
}

# Registered input: list with `exists`, `writable`, and `current_value`
input_info <- function(id) {
  text <- tool("tool__shiny_input_info", inputIds = list(id), .quiet = TRUE)
  info <- tryCatch(jsonlite::fromJSON(text[[1]], simplifyVector = FALSE),
                   error = function(e) NULL)
  info[[id]]
}

# Wait until an input exists and `check(current_value)` is TRUE
wait_input <- function(id, check = function(value) TRUE, timeout = 30) {
  start <- Sys.time()
  repeat {
    info <- input_info(id)
    if (isTRUE(info$exists) && isTRUE(check(info$current_value))) {
      return(invisible(info$current_value))
    }
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      stop(sprintf("Timed out waiting for input `%s` (current value: %s)", id,
                   jsonlite::toJSON(info$current_value, auto_unbox = TRUE)))
    }
    Sys.sleep(0.5)
  }
}

# Set an input as an agent does (non-strings are sent as JSON text)
set_input <- function(id, value) {
  if (!is.character(value) || length(value) != 1 || inherits(value, "AsIs")) {
    value <- as.character(jsonlite::toJSON(value, auto_unbox = TRUE))
  }
  text <- tool("tool__shiny_input_update", inputId = id, value = value)
  if (isTRUE(attr(text, "is_error"))) {
    stop(sprintf("Cannot set input `%s`: %s", id, text[[1]]))
  }
  invisible(text)
}

# Set an input and wait until the app reports the new value. The update is
# sent again every few seconds (e.g. a select's choices may still be loading)
set_input_wait <- function(id, value, check = NULL, timeout = 30) {
  if (is.null(check)) {
    check <- function(current) {
      identical(as.character(unlist(current)), as.character(unlist(value)))
    }
  }
  wait_input(id, timeout = timeout)
  start <- Sys.time()
  repeat {
    set_input(id, value)
    ok <- tryCatch({
      wait_input(id, check, timeout = 3)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(invisible(TRUE))
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      stop(sprintf("Input `%s` did not change to %s", id,
                   jsonlite::toJSON(value, auto_unbox = TRUE)))
    }
  }
}

# Checks for inputs whose values come back in another shape
same_number <- function(x) {
  function(value) isTRUE(all(as.numeric(unlist(value)) == x))
}
is_true <- function(value) isTRUE(as.logical(unlist(value)))
is_false <- function(value) isFALSE(as.logical(unlist(value)))

# Groups (compound inputs, or saved settings): same labels, same conditions
same_groups <- function(expected) {
  function(value) {
    value <- unname(value)
    length(value) == length(expected) && all(mapply(function(v, e) {
      identical(as.character(v$label), e$label) &&
        identical(sort(as.character(unlist(v$conditions))),
                  sort(as.character(unlist(e$conditions))))
    }, value, expected))
  }
}
# Analysis windows: same label, event, time, and frequency
same_windows <- function(expected) {
  function(value) {
    value <- unname(value)
    length(value) == length(expected) && all(mapply(function(v, e) {
      identical(as.character(v$label), e$label) &&
        identical(as.character(v$event), e$event) &&
        isTRUE(all.equal(as.numeric(unlist(v$time)), e$time)) &&
        isTRUE(all.equal(as.numeric(unlist(v$frequency)), e$frequency))
    }, value, expected))
  }
}

# Run an interactive script; returns its reply: `result`, and `output` (what
# the script printed). Stops if the script fails
run_script <- function(name, .quiet = FALSE) {
  text <- tool("tool__module_interactive_script_run", name = name, .quiet = .quiet)
  if (isTRUE(attr(text, "is_error"))) {
    stop(sprintf("Script `%s` failed: %s", name, text[[1]]))
  }
  invisible(jsonlite::fromJSON(text[[1]]))
}

# Run a script that must fail; returns the error text
run_script_error <- function(name) {
  text <- tool("tool__module_interactive_script_run", name = name)
  if (!isTRUE(attr(text, "is_error"))) {
    stop(sprintf("Script `%s` should have failed", name))
  }
  invisible(text[[1]])
}

wait_until <- function(check, what, timeout = 30, interval = 0.5) {
  start <- Sys.time()
  repeat {
    if (isTRUE(check())) return(invisible(TRUE))
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      stop("Timed out waiting for ", what)
    }
    Sys.sleep(interval)
  }
}

# Text of the element matching `selector`, without tags ("" if none)
page_text <- function(selector) {
  text <- tool("tool__shiny_query_ui", css_selector = selector,
               transform_image = FALSE, .quiet = TRUE)
  if (isTRUE(attr(text, "is_error"))) return("")
  html <- sub("<!-- NOTE:.*$", "", text[[1]])
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", html)))
}

# Title and text of the alert on the page ("" if none)
alert <- ".swal-overlay--show-modal"
alert_text <- function() page_text(paste(alert, ".swal-modal"))

get_subject <- function() {
  ravecore::as_rave_subject(sprintf("%s/%s", project_name, subject_code),
                            strict = FALSE)
}

# The settings the module saved, read from its settings.yaml
settings_file <- file.path("modules", module, "settings.yaml")
saved_settings <- function() yaml::read_yaml(settings_file)

# The analysis inputs of this test, set in dependency order
set_analysis_inputs <- function(quick) {
  set_input_wait("electrode_text", analysis_electrodes)
  set_input_wait("baseline_window", baseline_window,
                 check = same_number(baseline_window))
  set_input_wait("baseline_scope", baseline_scope)
  set_input_wait("baseline_unit", baseline_unit)
  set_input_wait("ui_analysis_settings", analysis_windows,
                 check = same_windows(analysis_windows))
  set_input_wait("condition_variable", "Condition")
  set_input_wait("first_condition_groupings", first_groups,
                 check = same_groups(first_groups))
  set_input_wait("quick_omnibus_only", quick,
                 check = if (quick) is_true else is_false)
}

# Stop unless the saved settings match the inputs of this test
check_saved_settings <- function(since, electrodes = analysis_electrodes,
                                 groups = first_groups,
                                 windows = analysis_windows) {
  settings <- saved_settings()
  checks <- c(
    "settings.yaml written by this run" = isTRUE(file.mtime(settings_file) >= since),
    "analysis_electrodes" = identical(trimws(settings$analysis_electrodes), electrodes),
    "baseline window" = isTRUE(all.equal(
      as.numeric(unlist(settings$baseline_settings$window)), baseline_window)),
    "baseline scope" = identical(settings$baseline_settings$scope, baseline_scope),
    "baseline unit" = identical(settings$baseline_settings$unit_of_analysis,
                                baseline_unit),
    "analysis windows" = same_windows(windows)(settings$analysis_settings),
    "condition variable" = identical(settings$condition_variable, "Condition"),
    "first factor" = same_groups(groups)(settings$first_condition_groupings)
  )
  print(data.frame(check = names(checks), passed = unname(checks)),
        row.names = FALSE)
  if (!all(checks)) stop("settings.yaml does not match the inputs")
  invisible(settings)
}

# The lines script `electrode_statistics` should give, built here from the
# pipeline target (the numbers behind the module's plots and table)
pipeline <- ravepipeline::pipeline(module, paths = "modules", temporary = TRUE)
expected_statistics <- function() {
  stats <- pipeline$read("omnibus_results")$stats
  unit <- saved_settings()$baseline_settings$unit_of_analysis
  lines <- vapply(seq_len(nrow(stats)), function(ii) {
    sprintf("%s: %s", rownames(stats)[[ii]], paste(
      sprintf("%s=%s", colnames(stats), signif(stats[ii, ], 4)),
      collapse = ", "
    ))
  }, "")
  c(sprintf("Statistics (%s) of electrodes %s, one line per statistic (%d):",
            unit, dipsaus::deparse_svec(as.integer(colnames(stats))),
            nrow(stats)), lines)
}
check_statistics <- function(electrodes) {
  actual <- as.character(run_script("electrode_statistics", .quiet = TRUE)$result)
  expected <- expected_statistics()
  if (!identical(actual, expected)) {
    cat("Expected:\n"); writeLines(expected)
    cat("Got:\n"); writeLines(actual)
    stop("`electrode_statistics` differs from target `omnibus_results`")
  }
  stats <- pipeline$read("omnibus_results")$stats
  if (!identical(as.integer(colnames(stats)), as.integer(electrodes))) {
    stop("The statistics are not those of electrodes ",
         dipsaus::deparse_svec(electrodes))
  }
  cat("electrode_statistics as expected:", expected[[1]], "\n")
  writeLines(paste(" ", utils::head(expected[-1], 6)))
  invisible(actual)
}

# 3D viewer helpers (tools `rave_3dviewer_get` / `rave_3dviewer_set`)
viewer <- "brain_viewer"
viewer_get <- function(name, args = NULL, .quiet = TRUE) {
  call_args <- list(.name = "tool__rave_3dviewer_get", outputId = viewer,
                    name = name, .quiet = .quiet)
  if (!is.null(args)) {
    call_args$args <- as.character(jsonlite::toJSON(args, auto_unbox = TRUE))
  }
  text <- do.call(tool, call_args)
  if (isTRUE(attr(text, "is_error"))) {
    stop(sprintf("rave_3dviewer_get `%s` failed: %s", name, text[[1]]))
  }
  jsonlite::fromJSON(text[[1]], simplifyVector = TRUE)
}

# Folders in the subject that the write steps add to
export_root <- function() file.path(get_subject()$path, "power_explorer")
list_exports <- function() {
  sort(list.dirs(export_root(), recursive = FALSE, full.names = TRUE))
}
# Group-analysis saves with this test's label, by folder (the subject's
# `list_pipelines()` keeps only the newest save of each label)
group_saves <- function() {
  root <- file.path(get_subject()$pipeline_path, module)
  dirs <- list.files(root, pattern = sprintf("^%s-%s-", module, group_label),
                     full.names = TRUE)
  dirs <- dirs[vapply(dirs, function(dir) {
    info <- tryCatch(readRDS(file.path(dir, "_fork_info")),
                     error = function(e) NULL)
    identical(info$policy, "group_analysis")
  }, FALSE)]
  sort(normalizePath(dirs))
}
list_reports <- function() {
  sort(list.dirs(get_subject()$report_path, recursive = FALSE))
}

# Values of the export, computed here from ravecore: the mean of the
# baseline-corrected power over the window's time and band, per trial
expected_export <- function(electrode, trials) {
  repo <- ravecore::prepare_subject_power_with_epochs(
    subject = sprintf("%s/%s", project_name, subject_code),
    electrodes = electrode, epoch_name = epoch_name,
    reference_name = reference_name, time_windows = epoch_window)
  ravecore::power_baseline(
    repo, baseline_windows = list(baseline_window), method = "percentage",
    units = c("Trial", "Frequency", "Electrode"), electrodes = electrode)
  baselined <- repo$power$baselined   # Frequency x Time x Trial x Electrode
  dnames <- dimnames(baselined)
  power <- array(baselined[], dim = dim(baselined), dimnames = dnames)
  window <- analysis_windows[[1]]
  fi <- which(as.numeric(dnames$Frequency) >= window$frequency[[1]] &
                as.numeric(dnames$Frequency) <= window$frequency[[2]])
  ti <- which(as.numeric(dnames$Time) >= window$time[[1]] &
                as.numeric(dnames$Time) <= window$time[[2]])
  values <- apply(power[fi, ti, , 1, drop = FALSE], 3, mean)
  names(values) <- dnames$Trial
  values[as.character(trials)]
}

# Compare an export folder with the inputs and with ravecore
check_export <- function(path, start) {
  subject <- get_subject()
  epoch <- subject$get_epoch(epoch_name)$table
  group_of <- unlist(lapply(first_groups, function(g) {
    stats::setNames(rep(g$label, length(g$conditions)), g$conditions)
  }))
  trials <- sort(epoch$Trial[epoch$Condition %in% names(group_of)])
  files <- sort(list.files(path))
  csv_names <- sprintf("%s_%s_e%04d.csv", project_name, subject_code,
                       export_electrodes)
  metadata <- yaml::read_yaml(file.path(path, "metadata.yaml"))
  window <- analysis_windows[[1]]
  checks <- c(
    "written by this run" = isTRUE(file.mtime(path) >= start),
    "files" = identical(files, sort(c(csv_names, "metadata.yaml"))),
    "metadata subject" = identical(metadata$subject, subject_code),
    "metadata project" = identical(metadata$project, project_name),
    "metadata baseline window" = identical(metadata$baseline_window,
                                           paste(baseline_window, collapse = ":")),
    "metadata baseline scope" = identical(metadata$baseline_scope, baseline_scope),
    "metadata unit" = identical(metadata$unit, "Pct_PowerChange"),
    "metadata window" = {
      aw <- metadata$analyis_settings[[1]]
      identical(aw$label, window$label) && identical(aw$event, window$event) &&
        isTRUE(all.equal(as.numeric(unlist(aw$time)), window$time)) &&
        isTRUE(all.equal(as.numeric(unlist(aw$frequency)), window$frequency))
    },
    # tables are saved column by column
    "metadata electrodes" = identical(
      as.integer(unlist(metadata$electrodes$Electrode)),
      as.integer(export_electrodes)),
    "metadata reference" = identical(
      as.integer(unlist(metadata$reference$Electrode)),
      as.integer(export_electrodes))
  )
  for (ii in seq_along(export_electrodes)) {
    e <- export_electrodes[[ii]]
    label <- csv_names[[ii]]
    tbl <- tryCatch(utils::read.csv(file.path(path, label)),
                    error = function(err) data.frame())
    tbl <- tbl[order(tbl$Trial), , drop = FALSE]
    expected <- expected_export(e, trials)
    checks[[paste(label, "rows")]] <- identical(nrow(tbl), length(trials))
    checks[[paste(label, "trials")]] <- identical(as.integer(tbl$Trial),
                                                  as.integer(trials))
    checks[[paste(label, "electrode")]] <- all(tbl$Electrode == e)
    checks[[paste(label, "window")]] <- all(tbl$AnalysisGroup == window$label)
    checks[[paste(label, "conditions")]] <- identical(
      as.character(tbl$OrigTrialLabel),
      as.character(epoch$Condition[match(tbl$Trial, epoch$Trial)]))
    checks[[paste(label, "groups")]] <- identical(
      as.character(tbl$TrialLabel),
      unname(group_of[as.character(tbl$OrigTrialLabel)]))
    checks[[paste(label, "values")]] <- isTRUE(all.equal(
      as.numeric(tbl$Pct_PowerChange), unname(as.numeric(expected)),
      tolerance = 1e-6))
  }
  print(data.frame(check = names(checks), passed = unname(checks)),
        row.names = FALSE)
  if (!all(checks)) stop("The export is not as expected")
  cat("Export is as expected:", path, "\n")
}

stopifnot(app_running())
stopifnot(module_open())

test_start <- Sys.time()
electrodes_csv <- file.path(get_subject()$meta_path, "electrodes.csv")
electrodes_csv_md5 <- tools::md5sum(electrodes_csv)

# The test epoch of the flagged-trials steps: written now, before the loader
# lists the subject's epochs; deleted at the end of those steps
epoch_file <- function(name) {
  file.path(get_subject()$meta_path, sprintf("epoch_%s.csv", name))
}
epoch_csv_md5 <- tools::md5sum(epoch_file(epoch_name))
flag_epoch_files <- function() {
  list.files(get_subject()$meta_path, pattern = sprintf("^epoch_%s", flag_epoch),
             full.names = TRUE)
}
# Write the test epoch: `table` (all trials of `epoch_name` by default) with
# `marked` as its excluded trials
write_flag_epoch <- function(marked, table = get_subject()$get_epoch(epoch_name, as_table = TRUE)) {
  table$ExcludedHint <- table$Trial %in% marked
  utils::write.csv(table, epoch_file(flag_epoch), row.names = FALSE)
}
if (do_flags) {
  stopifnot("no test epoch left from an earlier run" = !length(flag_epoch_files()))
  write_flag_epoch(c(5, 15))
}

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
  "shiny_ui_operate not offered" = !"tool__shiny_ui_operate" %in% module_tools
)

# ---- meta tools ---------------------------------------------------------------

tool("shidashi_sessions")
tool("skill_load__rave-module", action = "reference",
     file_name = "references/power_explorer.md", pattern = "Drive the module")

listed <- jsonlite::fromJSON(tool("tool__module_interactive_script_list")[[1]],
                             simplifyVector = FALSE)
script_names <- vapply(listed$scripts, `[[`, "", "name")
# No script saves flagged trials into the epoch or writes electrodes.csv
stopifnot(setequal(script_names, c(
  "load_data", "run_analysis", "electrode_statistics", "pipeline_progress",
  "assign_roi_levels", "clear_roi_groups", "cluster_to_viewer",
  "cluster_to_roi", "export_electrodes", "generate_report", "report_status",
  "save_for_group_analysis"
)))

# ---- load the subject -----------------------------------------------------------

set_input_wait("loader_project_name", project_name)
if (do_flags) {
  # The open loader lists a subject's epochs when the subject is chosen:
  # choose another subject first, so the list includes the test epoch (this
  # needs a fresh module page, where the loader is open)
  other_subject <- setdiff(ravecore::as_rave_project(project_name)$subjects(),
                           subject_code)[[1]]
  set_input_wait("loader_subject_code", other_subject)
}
set_input_wait("loader_subject_code", subject_code)
set_input_wait("loader_epoch_name", epoch_name)
set_input_wait("loader_epoch_name__trial_starts", epoch_window[[1]],
               check = same_number(epoch_window[[1]]))
set_input_wait("loader_epoch_name__trial_ends", epoch_window[[2]],
               check = same_number(epoch_window[[2]]))
set_input_wait("loader_reference_name", reference_name)
set_input_wait("loader_electrode_text", loaded_electrodes)
info <- input_info("loader_epoch_name__load_single_trial")
stopifnot("single-trial box is read-only" = isFALSE(info$writable),
          "single-trial box is unchecked" = is_false(info$current_value))

load_start <- Sys.time()
loaded <- run_script("load_data")
cat("load_data:", loaded$result, "\n")
n_trials <- nrow(get_subject()$get_epoch(epoch_name)$table)
stopifnot(
  startsWith(loaded$result, sprintf(
    "Loaded %s/%s: epoch %s (%d trials, %s to %s s), reference %s, electrodes %s;",
    project_name, subject_code, epoch_name, n_trials, epoch_window[[1]],
    epoch_window[[2]], reference_name, loaded_electrodes)),
  grepl("drive_a (16)", loaded$result, fixed = TRUE),
  grepl("events: Trial Onset", loaded$result, fixed = TRUE)
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
# Loading clears the statistics of any earlier run
statistics <- as.character(run_script("electrode_statistics")$result)
stopifnot(startsWith(statistics[[1]], "No results yet"))

# ---- quick analysis ---------------------------------------------------------------

set_analysis_inputs(quick = TRUE)
run_start <- Sys.time()
reply <- run_script("run_analysis")
stopifnot(startsWith(reply$result, sprintf(
  "Analysis done (quick): electrodes %s; windows: HighGamma (Trial Onset, 0 to 1 s, 70-150 Hz);",
  analysis_electrodes)))
check_saved_settings(run_start)
check_statistics(dipsaus::parse_svec(analysis_electrodes))

# ---- loading other data clears the results ---------------------------------------------

# Another reference makes new data: the results of the run above no longer
# apply, so the statistics are gone and the plots ask for RAVE!
heatmap_text <- function() page_text("#power_explorer-over_time_by_electrode")
set_input_wait("loader_reference_name", "noref")
reply <- run_script("load_data")
stopifnot(grepl("reference noref", reply$result, fixed = TRUE))
wait_until(function() {
  startsWith(as.character(run_script("electrode_statistics", .quiet = TRUE)$result)[[1]],
             "No results yet")
}, "the statistics to clear")
wait_until(function() grepl("No results available", heatmap_text(), fixed = TRUE),
           "the heatmap to ask for RAVE!")

# Back to the reference of this test
set_input_wait("loader_reference_name", reference_name)
reply <- run_script("load_data")
stopifnot(grepl(sprintf("reference %s", reference_name), reply$result, fixed = TRUE))
Sys.sleep(3)
set_analysis_inputs(quick = FALSE)

# ---- full analysis -------------------------------------------------------------------

set_input_wait("quick_omnibus_only", FALSE, check = is_false)
run_start <- Sys.time()
reply <- run_script("run_analysis")
stopifnot(startsWith(reply$result, "Analysis done (full):"))
check_saved_settings(run_start)
check_statistics(dipsaus::parse_svec(analysis_electrodes))
model <- tool("tool__shiny_output_result", outputId = "by_condition_statistics",
              transform_image = FALSE)
stopifnot(!isTRUE(attr(model, "is_error")),
          grepl("Model Formula", model[[1]], fixed = TRUE),
          grepl("Factor1", model[[1]], fixed = TRUE))
plot <- tool("tool__shiny_output_result", outputId = "over_time_by_condition")
stopifnot(!isTRUE(attr(plot, "is_error")), any(grepl("^\\[image", plot)))

# ---- error cases: nothing runs, nothing is saved, no dialog --------------------------

# Two identical analysis windows
set_input_wait("ui_analysis_settings",
               c(analysis_windows, list(modifyList(analysis_windows[[1]],
                                                   list(label = "Copy")))),
               check = function(value) length(value) == 2)
before <- file.mtime(settings_file)
reply <- run_script("run_analysis")
stopifnot(startsWith(reply$result, "Analysis did not run"),
          grepl("Two or more analysis settings are identical", reply$output,
                fixed = TRUE),
          identical(file.mtime(settings_file), before),
          !nzchar(alert_text()))
set_input_wait("ui_analysis_settings", analysis_windows,
               check = same_windows(analysis_windows))

# A first-factor level left empty by a duplicated condition
duplicated_groups <- list(list(label = "A", conditions = I("drive_a")),
                          list(label = "B", conditions = I("drive_a")))
set_input_wait("first_condition_groupings", duplicated_groups,
               check = same_groups(duplicated_groups))
reply <- run_script("run_analysis")
stopifnot(startsWith(reply$result, "Analysis did not run"),
          grepl("Insufficient Data", reply$output, fixed = TRUE),
          identical(file.mtime(settings_file), before),
          !nzchar(alert_text()))
set_input_wait("first_condition_groupings", first_groups,
               check = same_groups(first_groups))

# A failing pipeline step: no loaded electrode selected. The script fails;
# `pipeline_progress` gives the step's message
set_input_wait("electrode_text", "100")
error <- run_script_error("run_analysis")
progress <- as.character(run_script("pipeline_progress")$result)
stopifnot(any(grepl("requested_electrodes: errored", progress, fixed = TRUE)),
          any(grepl("No electrode selected", progress, fixed = TRUE)))
set_input_wait("electrode_text", analysis_electrodes)

# ---- second factor (a full run, for the contrasts) ---------------------------------------

set_input_wait("quick_omnibus_only", FALSE, check = is_false)
set_input_wait("enable_second_condition_groupings", TRUE, check = is_true)
set_input_wait("second_condition_groupings", second_groups,
               check = same_groups(second_groups))
run_start <- Sys.time()
reply <- run_script("run_analysis")
stopifnot(startsWith(reply$result, "Analysis done (full):"),
          grepl("second factor: drive_known", reply$result, fixed = TRUE))
settings <- check_saved_settings(run_start)
stopifnot(isTRUE(settings$enable_second_condition_groupings),
          same_groups(second_groups)(settings$second_condition_groupings))
statistics <- check_statistics(dipsaus::parse_svec(analysis_electrodes))
stopifnot("Factor1 x Factor2 cells" =
            any(grepl("Auditory", statistics) & grepl("drive_known", statistics)))

# With two factors, the model across electrodes also gives the stratified and
# interaction contrasts, and the module offers them
model_statistics <- pipeline$read("across_electrode_statistics")
stopifnot(
  "fixed effects" = identical(as.character(model_statistics$fixed_effects),
                              c("Factor1", "Factor2")),
  "stratified contrasts" = identical(names(model_statistics$stratified_contrasts),
                                     c("Factor1", "Factor2")),
  "interaction contrasts" = identical(names(model_statistics$itx_contrasts),
                                      "Factor1_Factor2")
)
contrasts_html <- function() {
  text <- tool("tool__shiny_output_result",
               outputId = "by_condition_statistics_contrasts",
               transform_image = FALSE, .quiet = TRUE)
  if (isTRUE(attr(text, "is_error"))) return(NA_character_)
  text[[1]]
}
pairwise_html <- contrasts_html()
stopifnot(!is.na(pairwise_html))
set_input_wait("bcs_choose_contrasts", "Stratified contrasts (more power!)")
wait_until(function() {
  html <- contrasts_html()
  !is.na(html) && !identical(html, pairwise_html)
}, "the stratified contrasts")
stratified_html <- contrasts_html()
set_input_wait("bcs_choose_contrasts", "ITX Contrasts (diff of diff)")
wait_until(function() {
  html <- contrasts_html()
  !is.na(html) && !identical(html, pairwise_html) && !identical(html, stratified_html)
}, "the interaction contrasts")
set_input_wait("bcs_choose_contrasts", "All-possible pairwise")
set_input_wait("enable_second_condition_groupings", FALSE, check = is_false)

# ---- custom ROI --------------------------------------------------------------------------

set_input_wait("quick_omnibus_only", TRUE, check = is_true)
set_input_wait("enable_custom_ROI", TRUE, check = is_true)
set_input_wait("custom_roi_variable", roi_variable)
electrode_table <- get_subject()$get_electrode_table()
roi_values <- unique(electrode_table[[roi_variable]][
  electrode_table$Electrode %in% dipsaus::parse_svec(loaded_electrodes)])
run_script("assign_roi_levels")
wait_input("custom_roi_groupings", function(value) {
  labels <- vapply(unname(value), function(v) as.character(v$label), "")
  setequal(labels, roi_values) && length(labels) == length(roi_values)
})

set_input_wait("custom_roi_type", "Group/Stratify results")
set_input_wait("custom_roi_groupings", roi_groups,
               check = same_groups(roi_groups))
run_start <- Sys.time()
reply <- run_script("run_analysis")
stopifnot(grepl("custom ROI: FSLabel, Group/Stratify results", reply$result,
                fixed = TRUE))
settings <- check_saved_settings(run_start, electrodes = loaded_electrodes)
saved_roi <- unname(settings$custom_roi_groupings)
stopifnot(
  isTRUE(settings$enable_custom_ROI),
  identical(settings$custom_roi_variable, roi_variable),
  identical(settings$custom_roi_type, "Group/Stratify results"),
  same_groups(roi_groups)(saved_roi),
  identical(vapply(saved_roi, function(g) as.character(g$electrodes), ""),
            unname(roi_electrodes[vapply(saved_roi, `[[`, "", "label")]))
)

run_script("clear_roi_groups")
wait_input("custom_roi_groupings", function(value) {
  value <- unname(value)
  length(value) == 1 && identical(as.character(value[[1]]$label), "All levels") &&
    setequal(as.character(unlist(value[[1]]$conditions)), roi_values)
})
set_input_wait("enable_custom_ROI", FALSE, check = is_false)

# ---- clusters -------------------------------------------------------------------------------

reply <- run_script("run_analysis")
stopifnot(grepl("custom ROI: off", reply$result, fixed = TRUE))
set_input_wait("otbe_yaxis_sort", "Activity Correlation")
set_input_wait("otbe_yaxis_cluster_k", 2, check = same_number(2))
plot <- tool("tool__shiny_output_result", outputId = "over_time_by_electrode")
stopifnot(!isTRUE(attr(plot, "is_error")))
run_script("cluster_to_roi")
wait_input("custom_roi_variable",
           function(value) identical(as.character(unlist(value)), "PE_Cluster"))
wait_input("enable_custom_ROI", is_true)
run_script("cluster_to_viewer")
set_input_wait("enable_custom_ROI", FALSE, check = is_false)
set_input_wait("otbe_yaxis_sort", "Electrode #")

# ---- the electrodes.csv button is locked ------------------------------------------------------

info <- input_info("otbe_cluster_to_electrodes_csv")
stopifnot(isTRUE(info$exists), isFALSE(info$writable))
text <- tool("tool__shiny_input_update", inputId = "otbe_cluster_to_electrodes_csv",
             value = "1")
stopifnot(isTRUE(attr(text, "is_error")), grepl("read-only", text[[1]]))
text <- tool("tool__shiny_ui_operate", action = "click",
             target = "otbe_cluster_to_electrodes_csv", `_module` = module)
stopifnot(isTRUE(attr(text, "is_error")))
Sys.sleep(2)
stopifnot(identical(tools::md5sum(electrodes_csv), electrodes_csv_md5))
cat("The electrodes.csv button is locked; electrodes.csv is unchanged\n")

# ---- 3D viewer --------------------------------------------------------------------------------

wait_until(function() {
  !isTRUE(attr(tool("tool__rave_3dviewer_get", outputId = viewer,
                    name = "controllers", .quiet = TRUE), "is_error"))
}, "the 3D viewer to render", timeout = 120, interval = 2)
options <- viewer_get("controller_options",
                      list(names = I("Display Data")))$controllers[["Display Data"]]
current <- viewer_get("controllers",
                      list(names = I("Display Data")))$controllers[["Display Data"]]
cat("Display Data:", current, "; choices:", paste(options$choices, collapse = ", "), "\n")
choice <- setdiff(options$choices, c(current, "[None]", ""))[[1]]
text <- tool("tool__rave_3dviewer_set", outputId = viewer, name = "controllers",
             data = as.character(jsonlite::toJSON(list("Display Data" = choice),
                                                  auto_unbox = TRUE)))
stopifnot(!isTRUE(attr(text, "is_error")))
wait_until(function() {
  identical(viewer_get("controllers", list(names = I("Display Data")))$controllers[["Display Data"]],
            choice)
}, sprintf("'Display Data' to become %s", choice))

stopifnot(identical(tools::md5sum(electrodes_csv), electrodes_csv_md5))

# ---- flagged trials (writes, then deletes, a test epoch!) -------------------------------------

# The test epoch marks trials 5 and 15 (`ExcludedHint`), so they start as the
# flagged trials: the next run leaves them out of the averages and statistics.
# Agents change the list with `flagged_trials`; only people save it into the
# epoch, with the link "Save Flags to Epoch", which runs the module's
# `save_trial_flags_to_epoch()`
if (do_flags) {
  flag_epoch_md5 <- tools::md5sum(epoch_file(flag_epoch))
  is_flagged <- function(text) {
    function(value) identical(as.character(unlist(value)), text)
  }
  load_epoch <- function(name) {
    set_input_wait("loader_epoch_name", name)
    run_script("load_data")$result
  }

  loaded <- load_epoch(flag_epoch)
  stopifnot(grepl("trials marked excluded in the epoch (ExcludedHint): 5,15",
                  loaded, fixed = TRUE))
  wait_input("flagged_trials", is_flagged("5,15"))

  set_analysis_inputs(quick = TRUE)
  reply <- run_script("run_analysis")
  stopifnot(grepl("; flagged trials (left out): 5,15", reply$result, fixed = TRUE))
  omnibus <- pipeline$read("omnibus_results")
  outliers <- omnibus$data_with_outliers
  stopifnot(
    "flagged trials left out of the statistics" =
      !any(as.integer(omnibus$data$Trial) %in% c(5, 15)),
    "flagged trials kept as not clean" =
      setequal(as.integer(outliers$Trial[!outliers$is_clean]), c(5, 15)),
    "flagged trials saved with the settings" =
      setequal(as.integer(unlist(saved_settings()$trial_outliers_list)), c(5, 15))
  )

  # An agent flags one more trial: the field holds the whole list
  set_input_wait("flagged_trials", "5,15,42")
  reply <- run_script("run_analysis")
  stopifnot(grepl("; flagged trials (left out): 5,15,42", reply$result, fixed = TRUE))

  # Text that is not trial numbers is put back, with a warning
  set_input("flagged_trials", "abc")
  wait_until(function() grepl("abc", page_text(".toast-container"), fixed = TRUE),
             "the warning about 'abc'")
  wait_input("flagged_trials", is_flagged("5,15,42"))
  # Trials not in the epoch are dropped; an empty field clears the list
  set_input_wait("flagged_trials", "5,9999", check = is_flagged("5"))
  set_input_wait("flagged_trials", "")
  reply <- run_script("run_analysis")
  stopifnot(grepl("; flagged trials (left out): none", reply$result, fixed = TRUE))
  stopifnot("agents' flags leave the epoch file alone" =
              identical(tools::md5sum(epoch_file(flag_epoch)), flag_epoch_md5))

  # What "Save Flags to Epoch" runs, on the loaded epoch
  repository <- pipeline$read("repository")
  save_flags <- pipeline$shared_env()$save_trial_flags_to_epoch
  marks <- function() {
    tbl <- utils::read.csv(epoch_file(flag_epoch))
    sort(tbl$Trial[as.logical(tbl$ExcludedHint)])
  }
  save_flags(repository, c(42, 6))
  trimmed <- utils::read.csv(epoch_file(paste0(flag_epoch, "_OutlierRemoved")))
  stopifnot(
    "saved: the epoch marks 6 and 42" = identical(marks(), c(6L, 42L)),
    "saved: _OutlierRemoved holds the other trials" =
      nrow(trimmed) == n_trials - 2 && !any(trimmed$OriginalTrial %in% c(6, 42))
  )
  save_flags(repository, integer(0))
  stopifnot(
    "saved nothing: no mark" = length(marks()) == 0,
    "saved nothing: no _OutlierRemoved" =
      !file.exists(epoch_file(paste0(flag_epoch, "_OutlierRemoved")))
  )
  # Saving stops once the epoch's trials changed since loading
  write_flag_epoch(integer(0),
                   table = get_subject()$get_epoch(epoch_name, as_table = TRUE)[-1, ])
  error <- tryCatch({ save_flags(repository, 6); "" }, error = conditionMessage)
  stopifnot(grepl("changed since the data were loaded", error, fixed = TRUE))

  # Loading another epoch drops the flags of this session; an epoch starts
  # from its marks
  write_flag_epoch(c(6, 42))
  set_input_wait("flagged_trials", "5")
  load_epoch(epoch_name)
  wait_input("flagged_trials", is_flagged(""))
  load_epoch(flag_epoch)
  wait_input("flagged_trials", is_flagged("6,42"))

  # Reloading the same epoch (here with another reference) keeps the flags of
  # this session, saved or not. The field shows consecutive trials as a range
  set_input_wait("flagged_trials", "42,6,5", check = is_flagged("5-6,42"))
  set_input_wait("loader_reference_name", "noref")
  run_script("load_data")
  Sys.sleep(3)
  stopifnot("unsaved flags kept on reload" =
              is_flagged("5-6,42")(input_info("flagged_trials")$current_value))

  # Back to the data of this test; delete the test epoch
  set_input_wait("loader_reference_name", reference_name)
  load_epoch(epoch_name)
  wait_input("flagged_trials", is_flagged(""))
  unlink(flag_epoch_files())
  stopifnot(
    "test epoch deleted" = !length(flag_epoch_files()),
    "the epoch of this test is unchanged" =
      identical(tools::md5sum(epoch_file(epoch_name)), epoch_csv_md5)
  )
  Sys.sleep(3)
  set_analysis_inputs(quick = TRUE)
  cat("Flagged trials as expected; the test epoch is deleted\n")
}

if (!do_write) {
  stop("`do_write` is FALSE: stopped before writing into the subject. ",
       "Nothing was written to it, apart from the test epoch of the ",
       "flagged-trials steps, which they deleted.")
}

# ---- export (writes a new folder!) --------------------------------------------------------------

set_input_wait("electrodes_to_export_roi_name", "none")
set_input_wait("frequencies_to_export", "Collapsed, Analysis window(s) only")
set_input_wait("times_to_export", "Collapsed, Analysis window(s) only")
set_input_wait("trials_to_export", "Raw, Only trials used in grouping factors")
exports_before <- list_exports()

# The export runs in quick mode too (it builds `data_for_export` either way)
set_input_wait("quick_omnibus_only", TRUE, check = is_true)

# No electrodes: nothing is written; people see "Export not started"
set_input_wait("electrodes_to_export", "")
reply <- run_script("export_electrodes")
stopifnot(length(reply$result) == 0,
          grepl("Export not started: No electrodes selected for export",
                reply$output, fixed = TRUE),
          identical(list_exports(), exports_before))
wait_until(function() grepl("Export not started", alert_text(), fixed = TRUE),
           "the 'Export not started' alert")

set_input_wait("electrodes_to_export", dipsaus::deparse_svec(export_electrodes))
export_start <- Sys.time()
reply <- run_script("export_electrodes")
path <- normalizePath(reply$result)
stopifnot(identical(setdiff(list_exports(), exports_before), path))
check_export(path, export_start)
wait_until(function() {
  message <- alert_text()
  grepl("Done with exporting!", message, fixed = TRUE) &&
    grepl(basename(path), message, fixed = TRUE)
}, "the 'Done with exporting!' alert with the export folder")
# The export re-ran the analysis on the export electrodes
check_statistics(export_electrodes)

# ---- save for group analysis (writes a pipeline copy!) ----------------------------------------------

# Without a label: nothing is saved; people see "Could not save results"
set_input_wait("replace_existing_group_anlysis_pipeline", "Create New")
set_input_wait("save_pipeline_for_group_analysis_label", "")
saves_before <- group_saves()
reply <- run_script("save_for_group_analysis")
stopifnot(startsWith(reply$result, "Not saved"),
          grepl("Could not save results: Saved results must have a label.",
                reply$output, fixed = TRUE),
          identical(group_saves(), saves_before))

set_input_wait("save_pipeline_for_group_analysis_label", group_label)
save_start <- Sys.time()
reply <- run_script("save_for_group_analysis")
first_save <- normalizePath(reply$result)
fork_time <- as.POSIXct(sub(sprintf("^%s-%s-", module, group_label), "",
                            basename(first_save)), format = "%y%m%dT%H%M%S")
fork_settings <- yaml::read_yaml(file.path(first_save, "settings.yaml"))
stopifnot(
  "the only new save" = identical(setdiff(group_saves(), saves_before), first_save),
  "the save is listed" = basename(first_save) %in%
    get_subject()$list_pipelines(module)$directory,
  "saved by this run" = isTRUE(fork_time >= trunc(save_start, "secs")),
  "saved settings are the module's" = identical(fork_settings, saved_settings()),
  "group analysis data saved" = file.exists(file.path(
    first_save, "shared", "objects", "data_for_group_analysis"))
)
cat("Saved for group analysis:", first_save, "\n")

# Replace it. The module asks `fork_to_subject()` to delete the older saves
# with the label, which it finds with the subject's
# `list_pipelines(all = TRUE)`. That call returns no rows in ravecore at the
# time of writing, and then the older saves are kept
set_input_wait("replace_existing_group_anlysis_pipeline", group_label)
wait_input("save_pipeline_for_group_analysis_label",
           function(value) identical(value, group_label))
Sys.sleep(1.5)   # the fork folder name has a one-second resolution
saves_before <- group_saves()
reply <- run_script("save_for_group_analysis")
second_save <- normalizePath(reply$result)
stopifnot(!identical(second_save, first_save),
          identical(setdiff(group_saves(), saves_before), second_save))
lists_all <- nrow(get_subject()$list_pipelines(module, all = TRUE)) > 0
if (lists_all) {
  stopifnot("the older saves are deleted" = identical(group_saves(), second_save))
  cat("Replaced the save:", second_save, "\n")
} else {
  stopifnot("the older saves are kept" = identical(
    group_saves(), sort(c(saves_before, second_save))))
  cat("NOTE: ravecore's list_pipelines(all = TRUE) returns no rows, so",
      "replacing kept the older saves:", saves_before, "\n")
}

# ---- HTML report (writes a report folder!) --------------------------------------------------------

# A full run first: the report shows the results of all the plots
set_input_wait("quick_omnibus_only", FALSE, check = is_false)
reply <- run_script("run_analysis")
stopifnot(startsWith(reply$result, "Analysis done (full):"))

stopifnot(startsWith(as.character(run_script("report_status")$result),
                     "No report has been scheduled"))
set_input_wait("exp_html_electrodes_to_include", "Aggregate only")
set_input_wait("exp_html_graphs", I(report_graphs))
reports_before <- list_reports()
report_start <- Sys.time()
run_script("generate_report")
status <- NULL
wait_until(function() {
  status <<- as.character(run_script("report_status", .quiet = TRUE)$result)
  cat("report_status:", status, "\n")
  startsWith(status, "Report finished:") || startsWith(status, "Report errored") ||
    startsWith(status, "The report job has ended")
}, "the report", timeout = 900, interval = 10)
stopifnot(startsWith(status, "Report finished:"))
report <- sub("^Report finished: ", "", status)
stopifnot(
  file.exists(report), file.size(report) > 0,
  isTRUE(file.mtime(report) >= report_start),
  identical(setdiff(list_reports(), reports_before), dirname(report))
)

stopifnot(identical(tools::md5sum(electrodes_csv), electrodes_csv_md5))

cat("\nPower Explorer workflow passed. It wrote into", sprintf("%s/%s:", project_name, subject_code),
    "\n  export:", path,
    "\n  group-analysis save:", second_save,
    "\n  report:", dirname(report),
    "\nThe 'Done with exporting!' alert stays open until someone closes it.",
    "\nRestore `modules/power_explorer/settings.yaml` from your copy.\n")
