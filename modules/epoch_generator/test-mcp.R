# modules/epoch_generator/test-mcp.R
#
# MCP test of the Epoch Generator module (`epoch_generator`) on the demo
# subject demo/YAB. It drives the module the way an agent does:
#
# 1. loads the subject;
# 2. block 008, channel 129 (an audio track): absolute value, Above 10,
#    minimum duration 1 s. The onsets must match the sound onsets of the
#    subject's epoch `YABaOutlier` (audio-only and audio-visual trials), and
#    no visual-only trial;
# 3. block 010, channel 130 (a photodiode): raw signal, Below -100, 0.5 s.
#    The onsets must come 0.05-0.4 s before the visual and audio-visual
#    trials of `YABaOutlier`, and match no audio-only trial;
# 4. checks the known errors (as script errors, with no toast for MCP calls),
#    the block guard, zero candidates when nothing crosses, the staging log,
#    the read-only scripts, and unstaging;
# 5. with `do_write`, saves one agent-generated epoch, checks its CSV and
#    description, loads it back, and deletes both files again.
#
# What it writes:
# * `modules/epoch_generator/settings.yaml` (the loader saves the project and
#   subject): copy it before the test and restore it from the copy
#   (`git checkout` would also drop uncommitted edits).
# * With `do_write`: `epoch_agent_generated_<time>.csv` and
#   `epoch_agent_generated_<time>_description.txt` in demo/YAB's meta folder,
#   deleted at the end. The test refuses to start while such files exist,
#   and checks that `epoch_YABaOutlier.csv` is unchanged.
#
# Run from the repository root against a test copy of the app, with the
# module open and visible in a browser:
#   RAVE_TEST_PORT=17299 Rscript modules/epoch_generator/test-mcp.R

module <- "epoch_generator"
source("agents/skills/build-module-mcp/test-common.R")

project_name    <- "demo"
subject_code    <- "YAB"
reference_epoch <- "YABaOutlier"   # 72/72/71/72 trials
do_write        <- TRUE            # FALSE: stop before save_agent_epoch
sample_rate     <- 2000            # Hz; the rate of both trigger files

audio <- list(block = "008", file = "YABDatafile008_ch129.mat",
              varname = "analogTraces", absolute = TRUE, direction = "Above",
              min_duration = 1, threshold = 10, expect = 57)
photo <- list(block = "010", file = "YABDatafile010_ch130.mat",
              varname = "analogTraces", absolute = FALSE, direction = "Below",
              min_duration = 0.5, threshold = -100, expect = 56)

subject   <- ravecore::RAVESubject$new(project_name, subject_code, strict = FALSE)
meta_path <- subject$meta_path
reference <- subject$meta_data(meta_type = "epoch", meta_name = reference_epoch)

# Trial types of `YABaOutlier`: '*_a' audio only, '*_v' visual only, the rest
# both (e.g. '*_av', 'AknownVmeant', 'press_AV')
trial_kind <- function(condition) {
  ifelse(grepl("_a$", condition), "audio",
         ifelse(grepl("_v$", condition), "visual", "both"))
}

# ---- module-specific helpers -------------------------------------------------------

# A script's labelled result ("candidates=57") as a named character vector
labelled <- function(result) {
  x <- as.character(unlist(result))
  structure(sub("^[^=]*=", "", x), names = sub("=.*$", "", x))
}

# The read-only script `epoch_status`
status <- function() {
  reply <- run_script("epoch_status", .quiet = TRUE)
  values <- labelled(reply$result)
  list(
    output = paste(reply$output, collapse = "\n"),
    candidates = as.integer(values[["candidates"]]),
    selected = as.integer(values[["selected"]]),
    staged = as.integer(values[["staged"]]),
    threshold = suppressWarnings(as.numeric(values[["threshold"]]))
  )
}

# Poll `epoch_status` until `check(status)` holds
wait_status <- function(check, what, timeout = 30) {
  last <- NULL
  wait_until(function() {
    last <<- status()
    isTRUE(check(last))
  }, what, timeout = timeout)
  invisible(last)
}

# The candidate onsets (at most 100) from `candidate_times`
candidate_times <- function() {
  result <- unlist(run_script("candidate_times", .quiet = TRUE)$result)
  if (is.character(result)) { return(numeric(0)) }
  as.numeric(result)
}

# Set the threshold as the manual says: clear the field, wait until it is
# empty, set the number, and wait until `epoch_status` reports candidates for it
set_threshold <- function(value) {
  set_input("threshold", "")
  wait_input("threshold", is_empty)
  set_input("threshold", value)
  wait_input("threshold", same_number(value))
  wait_status(function(s) {
    isTRUE(abs(s$threshold - value) < 1e-9) && !grepl("PENDING", s$output)
  }, sprintf("candidates for threshold %s", format(value)))
}

# Stage every candidate: Clear selection, Toggle selection, stage_selected
stage_all <- function(expect) {
  selected_all <- FALSE
  for (attempt in 1:2) {
    set_input("clear_selection", 1)
    wait_status(function(s) s$selected == 0, "no candidate selected")
    set_input("toggle_selection", 1)
    selected_all <- tryCatch({
      wait_status(function(s) s$selected == expect,
                  sprintf("%d candidates selected", expect), timeout = 15)
      TRUE
    }, error = function(e) FALSE)
    if (selected_all) { break }
  }
  if (!selected_all) { stop("Could not select all ", expect, " candidates") }
  run_script("stage_selected")
}

# Choose the block and the trigger file, and load the file
load_file <- function(case) {
  set_input_wait("block", case$block)
  set_input_wait("epoch_file", case$file)
  reply <- run_script("load_epoch_file")
  wait_input("varname", function(v) identical(unlist(v), case$varname))
  reply
}

# Distance from a time to the nearest onset
nearest_gap <- function(t, times) {
  if (!length(times)) { return(Inf) }
  min(abs(times - t))
}

# No alert left open, and no toast with this text (MCP calls get errors, not
# notifications)
nothing_followed <- function(text) {
  !on_page(alert) && !grepl(text, page_text(".toast-container"), fixed = TRUE)
}

# ---- 1. catalog --------------------------------------------------------------------

wait_until(app_running, "the app", timeout = 60)
wait_until(module_open, "the module page", timeout = 60)

tools <- module_tools()
expected_tools <- sprintf("tool__%s", c(
  "shiny_input_info", "shiny_input_update", "shiny_output_info",
  "shiny_query_ui", "shiny_output_result", "module_interactive_script_list",
  "module_interactive_script_inspect", "module_interactive_script_run"
))
scripts <- vapply(list_scripts(.quiet = TRUE)$scripts, function(s) s$name, "")
check_all(c(
  "all eight module tools" = all(expected_tools %in% tools),
  "no shiny_ui_operate" = !"tool__shiny_ui_operate" %in% tools,
  "all scripts" = all(c(
    "load_data", "load_epoch_file", "load_epoch_signal", "stage_selected",
    "unstage_selected", "unstage_all", "discard_epoch", "epoch_channel_info",
    "threshold_scan", "epoch_status", "candidate_times", "save_agent_epoch"
  ) %in% scripts)
), "catalog")

# ---- 2. load -----------------------------------------------------------------------

if (!data_loaded()) {
  refused <- run_script_error("epoch_status")
  check_all(c("scripts refused before load" = grepl("not started", refused, ignore.case = TRUE)),
            "before load")
}
set_input_wait("loader_project_name", project_name)
set_input_wait("loader_subject_code", subject_code)
loaded <- run_script("load_data")
wait_until(data_loaded, "the data to load", timeout = 120)
loaded_values <- labelled(loaded$result)
check_all(c(
  "load_data names the subject" = identical(loaded_values[["subject"]], "demo/YAB"),
  "load_data lists the blocks" = identical(loaded_values[["blocks"]], "008,010,011,012"),
  "load_data lists the epochs" = grepl(reference_epoch, loaded_values[["epochs"]]),
  "settings.yaml has the subject" = identical(saved_settings()$project_name, project_name) &&
    identical(saved_settings()$subject_code, subject_code)
), "load")

# Registered inputs: the buttons that scripts replace are read-only
check_all(c(
  "threshold exists" = isTRUE(input_info("threshold")$exists),
  "toggle_selection writable" = isTRUE(input_info("toggle_selection")$writable),
  "load_epoch_btn read-only" = isFALSE(input_info("load_epoch_btn")$writable),
  "load_signal_btn read-only" = isFALSE(input_info("load_signal_btn")$writable),
  "save_changes read-only" = isFALSE(input_info("save_changes")$writable),
  "undo_changes read-only" = isFALSE(input_info("undo_changes")$writable),
  "undo_all read-only" = isFALSE(input_info("undo_all")$writable),
  "discard_epoch read-only" = isFALSE(input_info("discard_epoch")$writable),
  "download_btn2 read-only" = isFALSE(input_info("download_btn2")$writable)
), "registered inputs")

# ---- 3. error: no file loaded --------------------------------------------------------

no_file <- run_script_error("load_epoch_signal")
check_all(c(
  "error names the problem" = grepl("Please load epoch file first", no_file, fixed = TRUE),
  "nothing followed (no alert, no toast)" = nothing_followed("Please load epoch file first")
), "load_epoch_signal before a file")

# ---- 4. audio: block 008, channel 129 ------------------------------------------------

# A fresh sample-rate estimate: the module fills it in only while the field is
# at most 1
set_input_wait("sample_rate", 1, check = same_number(1))
file_reply <- load_file(audio)
# the duration estimate (the trigger file is as long as the iEEG recording)
wait_input("sample_rate", same_number(sample_rate))
file_values <- labelled(file_reply$result)
check_all(c(
  "type MAT" = identical(file_values[["type"]], "MAT"),
  "variable analogTraces" = identical(file_values[["variable"]], audio$varname),
  "output: ask the user for the rate" = grepl("REQUIRED from the user", file_reply$output)
), "load_epoch_file (audio)")

# Error: a struct is not a signal
set_input_wait("varname", "analogInfos")
not_signal <- run_script_error("load_epoch_signal")
check_all(c(
  "error: not a numeric signal" = grepl("not a numeric signal", not_signal, fixed = TRUE),
  "nothing followed" = nothing_followed("not a numeric signal")
), "load_epoch_signal with a struct")
set_input_wait("varname", audio$varname)

# Another file selected without "Load epoch file": the browser empties the
# variable list, but the module keeps the loaded file (and Shiny keeps the old
# `varname`), so `epoch_status` warns
set_input_wait("epoch_file", "YABDatafile008_ch130.mat")
changed <- status()
check_all(c(
  "warning: the selected file is not the loaded file" =
    grepl("is not the loaded file", changed$output, fixed = TRUE)
), "file changed without loading")
load_file(audio)

signal_reply <- run_script("load_epoch_signal")
# new data clears the threshold field
wait_input("threshold", is_empty)
info_reply <- run_script("epoch_channel_info")
info_values <- labelled(info_reply$result)
check_all(c(
  "646130 samples" = grepl("646130 samples", signal_reply$output, fixed = TRUE),
  "323.065 s at 2000 Hz" = grepl("323.065 s at 2000 Hz", signal_reply$output, fixed = TRUE),
  "the iEEG block lasts as long" = grepl("block 008 lasts 323.065 s", signal_reply$output, fixed = TRUE),
  "no duration mismatch" = !grepl("durations differ", signal_reply$output),
  "info: the likely trigger files of block 008" = grepl(paste0(
    "likely trigger files (the module's guesses): ",
    "YABDatafile008_ch129.mat, YABDatafile008_ch130.mat"
  ), info_reply$output, fixed = TRUE),
  "info: duration estimate 2000" = identical(info_values[["estimate_hz"]], "2000"),
  "info: rate hint in the file" = grepl("analogInfos$SampleRate = 2000", info_reply$output, fixed = TRUE),
  "info: MATLAB rule" = grepl("REQUIRED from the user", info_reply$output, fixed = TRUE)
), "load_epoch_signal and epoch_channel_info (audio)")

set_input_wait("plot_absolute", audio$absolute, check = is_true)
set_input_wait("threshold_direction", audio$direction)
set_input_wait("num_duration", audio$min_duration, check = same_number(audio$min_duration))
scan <- unlist(run_script("threshold_scan")$result)
check_all(c("threshold_scan has a 57 plateau" = sum(grepl(": 57$", scan)) >= 2),
          "threshold_scan (audio)")

set_threshold(audio$threshold)
times_008 <- candidate_times()
ref_008 <- reference[reference$Block == audio$block, ]
kind_008 <- trial_kind(ref_008$Condition)
gap_sound <- vapply(ref_008$Time[kind_008 != "visual"], nearest_gap, 0, times = times_008)
gap_visual <- vapply(ref_008$Time[kind_008 == "visual"], nearest_gap, 0, times = times_008)
check_all(c(
  "57 candidates" = length(times_008) == audio$expect,
  "56 trials with sound" = length(gap_sound) == 56,
  "each trial with sound has an onset within 25 ms" = all(gap_sound < 0.025),
  "no visual-only trial has an onset within 0.1 s" = all(gap_visual > 0.1)
), "audio onsets against YABaOutlier (block 008)")

# Zero candidates when nothing crosses (no NA onset)
zero <- set_threshold(1e6)
check_all(c(
  "0 candidates" = zero$candidates == 0,
  "candidate_times is empty" = !length(candidate_times()),
  "no NA in the report" = !grepl("NA,|, NA", zero$output)
), "threshold above the signal")
set_threshold(audio$threshold)

# Block guard: the candidates belong to block 008
set_input_wait("block", photo$block)
guard <- run_script_error("stage_selected")
check_all(c(
  "error names both blocks" = grepl("found in block 008's signal, but block 010", guard, fixed = TRUE),
  "nothing followed" = nothing_followed("found in block 008")
), "block guard")
set_input_wait("block", audio$block)
wait_status(function(s) s$candidates == audio$expect, "the audio candidates")

stage_008 <- stage_all(audio$expect)
after_008 <- status()
check_all(c(
  "staged 57 in 008" = identical(labelled(stage_008$result)[["staged_in_block"]], "57"),
  "the STOP note" = grepl("STOP:", stage_008$output, fixed = TRUE),
  "no candidates left" = after_008$candidates == 0,
  "stale selection reported" = grepl("Stale table selection", after_008$output, fixed = TRUE)
), "stage all (audio)")
no_candidates <- run_script_error("stage_selected")
nothing_selected <- run_script_error("unstage_selected")
check_all(c(
  "stage_selected: no candidates" = grepl("no candidate onsets", no_candidates, fixed = TRUE),
  "unstage_selected: nothing selected" = grepl("No staged onsets are selected", nothing_selected, fixed = TRUE)
), "known errors after staging")

# ---- 5. photodiode: block 010, channel 130 -------------------------------------------

load_file(photo)
wait_input("sample_rate", same_number(sample_rate))
run_script("load_epoch_signal")
wait_input("threshold", is_empty)
set_input_wait("plot_absolute", photo$absolute, check = is_false)
set_input_wait("threshold_direction", photo$direction)
set_input_wait("num_duration", photo$min_duration, check = same_number(photo$min_duration))
set_threshold(photo$threshold)

times_010 <- candidate_times()
ref_010 <- reference[reference$Block == photo$block, ]
kind_010 <- trial_kind(ref_010$Condition)
lead_visual <- vapply(ref_010$Time[kind_010 != "audio"], function(t) {
  d <- t - times_010
  d <- d[d > 0]
  if (length(d)) min(d) else Inf
}, 0)
gap_audio <- vapply(ref_010$Time[kind_010 == "audio"], nearest_gap, 0, times = times_010)
check_all(c(
  "56 candidates" = length(times_010) == photo$expect,
  "56 trials with video" = length(lead_visual) == 56,
  "each trial with video has an onset 0.05-0.4 s before it" = all(lead_visual > 0.05 & lead_visual < 0.4),
  "no audio-only trial has an onset within 0.5 s" = all(gap_audio > 0.5)
), "photodiode onsets against YABaOutlier (block 010)")

stage_all(photo$expect)

# ---- 6. provenance and unstaging -----------------------------------------------------

log_status <- status()
check_all(c(
  "staged 113" = log_status$staged == 113,
  "log: audio staging" = grepl(paste0(
    "staged 57 in block 008 from 'analogTraces' of 'YABDatafile008_ch129.mat' ",
    "(block 008), 2000 Hz, Above 10 on the absolute signal, minimum duration 1 s"
  ), log_status$output, fixed = TRUE),
  "log: photodiode staging" = grepl(paste0(
    "staged 56 in block 010 from 'analogTraces' of 'YABDatafile010_ch130.mat' ",
    "(block 010), 2000 Hz, Below -100 on the raw signal, minimum duration 0.5 s"
  ), log_status$output, fixed = TRUE)
), "staging log")

unstaged <- run_script("unstage_all")
check_all(c(
  "removed 56 from 010" = identical(labelled(unstaged$result)[["removed"]], "56"),
  "57 left" = identical(labelled(unstaged$result)[["staged_total"]], "57")
), "unstage_all (block 010)")
# Unstaging re-runs the thresholding, so the candidates come back
wait_status(function(s) s$candidates == photo$expect, "the photodiode candidates again")
stage_all(photo$expect)
wait_status(function(s) s$staged == 113, "113 staged onsets")

# ---- 7. save one agent-generated epoch, then delete it -------------------------------

if (!do_write) {
  stop("`do_write` is FALSE: stopped before save_agent_epoch. Nothing was written to the subject.")
}

leftovers <- list.files(meta_path, pattern = "^epoch_agent_generated_")
if (length(leftovers)) {
  stop("Agent-generated epochs already exist in ", meta_path, ": ",
       paste(leftovers, collapse = ", "), ". Check and remove them first.")
}
reference_file <- file.path(meta_path, sprintf("epoch_%s.csv", reference_epoch))
md5_before <- unname(tools::md5sum(reference_file))
started <- Sys.time() - 1

saved <- run_script("save_agent_epoch")
saved_values <- labelled(saved$result)
epoch_name <- saved_values[["epoch"]]
csv_file <- file.path(meta_path, saved_values[["csv"]])
sidecar_file <- file.path(meta_path, saved_values[["description"]])

table_saved <- utils::read.csv(csv_file, colClasses = c(Block = "character"))
expected <- data.frame(
  Block = rep(c("008", "010"), c(57, 56)),
  Time = c(sort(times_008), sort(times_010)),
  Trial = 1:113,
  Condition = "NoCondition"
)
sidecar <- yaml::read_yaml(sidecar_file)
actions_008 <- sidecar$blocks[["008"]]$actions
actions_010 <- sidecar$blocks[["010"]]$actions
staged_008 <- Filter(function(a) identical(a$action, "staged"), actions_008)
staged_010 <- Filter(function(a) identical(a$action, "staged"), actions_010)
last_010 <- staged_010[[length(staged_010)]]
check_all(c(
  "name agent_generated_<yymmddTHHMMSS>" = grepl("^agent_generated_[0-9]{6}T[0-9]{6}$", epoch_name),
  "csv written now" = file.exists(csv_file) && file.mtime(csv_file) >= started,
  "description written now" = file.exists(sidecar_file) && file.mtime(sidecar_file) >= started,
  "csv columns" = identical(names(table_saved), c("Block", "Time", "Trial", "Condition")),
  "csv blocks" = identical(table_saved$Block, expected$Block),
  "csv times" = isTRUE(all.equal(table_saved$Time, expected$Time, tolerance = 0)),
  "csv trials" = identical(as.integer(table_saved$Trial), expected$Trial),
  "csv conditions" = all(table_saved$Condition == "NoCondition"),
  "sidecar names the epoch" = identical(sidecar$epoch_name, epoch_name),
  "sidecar total" = identical(as.integer(sidecar$total_trials), 113L),
  "sidecar 008 trials" = identical(as.integer(sidecar$blocks[["008"]]$trials), 57L),
  "sidecar 008 block duration" = isTRUE(abs(sidecar$blocks[["008"]]$ieeg_block_duration_seconds - 323.065) < 1e-6),
  "sidecar 008 staging" = length(staged_008) == 1 &&
    identical(staged_008[[1]]$file, "008/YABDatafile008_ch129.mat") &&
    identical(staged_008[[1]]$variable, "analogTraces") &&
    isTRUE(staged_008[[1]]$sample_rate == 2000) &&
    isTRUE(staged_008[[1]]$threshold == 10) &&
    identical(staged_008[[1]]$threshold_direction, "Above") &&
    isTRUE(staged_008[[1]]$transform$absolute) &&
    length(staged_008[[1]]$onsets) == 57,
  "sidecar 010 history: staged, unstaged all, staged" = identical(
    vapply(actions_010, function(a) a$action, ""),
    c("staged", "unstaged all", "staged")
  ),
  "sidecar 010 staging" = isTRUE(last_010$threshold == -100) &&
    identical(last_010$threshold_direction, "Below") &&
    isFALSE(last_010$transform$absolute) &&
    isTRUE(last_010$minimum_duration_seconds == 0.5) &&
    length(last_010$onsets) == 56
), "save_agent_epoch")

# The new epoch is listed: loading it puts its 113 onsets back
set_input("load_epoch", epoch_name)
loaded_back <- wait_status(function(s) s$staged == 113 &&
                             grepl(sprintf("loaded epoch '%s'", epoch_name), s$output, fixed = TRUE),
                           "the saved epoch loaded back")

unlink(c(csv_file, sidecar_file))
check_all(c(
  "test files deleted" = !file.exists(csv_file) && !file.exists(sidecar_file),
  "no agent epochs left" = !length(list.files(meta_path, pattern = "^epoch_agent_generated_")),
  "YABaOutlier unchanged" = identical(unname(tools::md5sum(reference_file)), md5_before)
), "clean-up")

# ---- 8. load an existing epoch, then discard -------------------------------------------

set_input("load_epoch", reference_epoch)
reference_loaded <- wait_status(function(s) s$staged == nrow(reference) &&
                                  grepl(sprintf("loaded epoch '%s'", reference_epoch), s$output, fixed = TRUE),
                                "YABaOutlier loaded")
check_all(c(
  "72/72/71/72 staged" = grepl("008: 72, 010: 72, 011: 71, 012: 72", reference_loaded$output, fixed = TRUE)
), "load existing epoch")

# Candidates near staged onsets are dropped by the block of the loaded signal
# (010), even while another block is selected: unstaging block 011 re-runs the
# thresholding, and block 010's photodiode onsets (within 0.5 s of its staged
# YABaOutlier onsets) stay out
computed_at <- function(s) {
  regmatches(s$output, regexpr("computed [0-9:]{8}", s$output))
}
before_unstage <- computed_at(status())
set_input_wait("block", "011")
unstaged_011 <- run_script("unstage_all")
excluded <- wait_status(function(s) {
  s$staged == nrow(reference) - 71 && !identical(computed_at(s), before_unstage)
}, "the thresholding to re-run after unstaging block 011")
check_all(c(
  "removed 71 from 011" = identical(labelled(unstaged_011$result)[["removed"]], "71"),
  "block 010's candidates near its staged onsets stay out" = excluded$candidates == 0
), "exclusion by the loaded signal's block")
set_input_wait("block", photo$block)

discarded <- run_script("discard_epoch")
check_all(c(
  "discarded 216" = identical(labelled(discarded$result)[["removed"]], "216"),
  "nothing staged" = status()$staged == 0
), "discard_epoch")

cat("\nAll epoch_generator MCP checks passed.\n")
