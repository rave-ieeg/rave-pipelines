# Manually test the MCP tools: run the Morlet wavelet on one subject the way an
# agent would, without clicking in the browser. A browser session with the
# module open is still needed: input updates and clicks round-trip through it.
#
# Agents take the same path as people:
#   * `load_data` loads the subject (it must be Notch-filtered)
#   * `shiny_input_update` sets the wavelet inputs
#   * `run_analysis` (the "Run wavelet" button) checks the inputs, builds the
#     kernel table, and opens the confirmation dialog; what goes wrong is in
#     the script's `output`
#   * `shiny_ui_operate` clicks the dialog's "Confirm and run in background"
#     or "Confirm" button, which runs the same code as for people
#   * script `pipeline_progress` and the alert tell when the wavelet is done
#
# The error cases write nothing. Then the wavelet runs twice on the test
# subject, in the background and then in the foreground, with different
# precision so that the saved results tell the runs apart.
#
# WARNING: each run overwrites the subject's wavelet results (power, phase,
# and voltage files, `meta/frequencies.csv`, `meta/reference_noref.csv`,
# common-average references) and re-forks `[subject]/pipelines/wavelet_module`
# after backing it up. Use a subject whose wavelet can be redone. A live run
# also rewrites `modules/wavelet_module/settings.yaml`: copy it first and
# restore it from the copy (`git checkout` would also drop uncommitted edits).

port   <- as.integer(Sys.getenv("RAVE_TEST_PORT", "17283"))  # port used for testing
module <- "wavelet_module"

# test subject and settings
project_name <- "test2"
subject_code <- "DemoSubject"
do_apply     <- TRUE   # FALSE: stop before the first wavelet run (nothing is written to the subject)

target_sample_rate <- 100
pre_downsample     <- 2
freq_range         <- c(2, 200)
freq_step          <- 10          # coarse, so that the runs are quick
cycle_range        <- c(3, 20)

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

# Registered input: list with `exists` and `current_value` (NULL if unknown)
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
  if (!is.character(value) || length(value) != 1) {
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

# Check for numeric inputs, which come back as numbers or strings
same_number <- function(x) {
  function(value) isTRUE(all(as.numeric(unlist(value)) == x))
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

# Stop unless the script printed `pattern`
expect_output <- function(reply, pattern) {
  if (!isTRUE(grepl(pattern, reply$output))) {
    stop(sprintf("Expected `%s` in the script output; got:\n%s",
                 pattern, paste(reply$output, collapse = "\n")))
  }
  invisible(reply)
}

# Operate the page with `shiny_ui_operate`; stops if the tool fails
operate <- function(action, ...) {
  text <- tool("tool__shiny_ui_operate", action = action, ...)
  if (isTRUE(attr(text, "is_error"))) {
    stop(sprintf("shiny_ui_operate `%s` failed: %s", action, text[[1]]))
  }
  invisible(text)
}

# Whether an element matching `selector` is on the page
on_page <- function(selector) {
  text <- tool("tool__shiny_query_ui", css_selector = selector,
               transform_image = FALSE, .quiet = TRUE)
  !isTRUE(attr(text, "is_error"))
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

# The confirmation dialog's "Confirm" button: on the page while it is open
dialog <- "#wavelet_module-wavelet_confirm_btn"

# Title and text of the alert on the page ("" if none)
alert <- ".swal-overlay--show-modal"
alert_text <- function() {
  text <- tool("tool__shiny_query_ui", css_selector = paste(alert, ".swal-modal"),
               transform_image = FALSE, .quiet = TRUE)
  if (isTRUE(attr(text, "is_error"))) return("")
  html <- sub("<!-- NOTE:.*$", "", text[[1]])
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", html)))
}

# The kernel table that the module builds from the three sliders (see
# `kernel_params` in R/module_server.R)
expected_kernel <- function() {
  freqs <- seq(freq_range[[1]], freq_range[[2]], by = freq_step)
  cycles <- round(exp(
    (log(cycle_range[[2]]) - log(cycle_range[[1]])) /
      (log(freq_range[[2]]) - log(freq_range[[1]])) *
      (log(freqs) - log(freq_range[[1]])) + log(cycle_range[[1]])
  ))
  list(frequencies = freqs, cycles = cycles)
}

load_subject <- function() {
  ravecore::as_rave_subject(sprintf("%s/%s", project_name, subject_code),
                            strict = FALSE)
}

# Whether the subject holds a wavelet saved after `start`
saved_since <- function(start) {
  stamp <- load_subject()$preprocess_settings$wavelet_params$timestamp
  length(stamp) == 1 && isTRUE(as.POSIXct(stamp) >= trunc(start, "secs"))
}

# Wait until the run that started at `start` is saved and its final alert
# shows. Prints the pipeline progress on the way
wait_wavelet <- function(start, timeout = 900) {
  t0 <- Sys.time()
  repeat {
    progress <- run_script("pipeline_progress", .quiet = TRUE)$result
    message <- alert_text()
    cat(format(Sys.time(), "%H:%M:%S"), "|", paste(progress, collapse = "; "),
        "| alert:", if (nzchar(message)) message else "(none)", "\n")
    if (grepl("^Errors|Wavelet done, but", message)) {
      stop("The wavelet run reported a problem: ", message)
    }
    if (grepl("Please feel free to close", message) && saved_since(start)) {
      return(invisible(progress))
    }
    if (difftime(Sys.time(), t0, units = "secs") > timeout) {
      stop("Timed out waiting for the wavelet")
    }
    Sys.sleep(5)
  }
}

# Compare the wavelet saved in the subject with what was set
check_saved <- function(start, precision) {
  subject <- load_subject()
  settings <- subject$preprocess_settings
  params <- settings$wavelet_params
  expected <- expected_kernel()
  # Electrodes the module loads (see target `check_prerequisite` in main.Rmd)
  loaded <- settings$notch_filtered &
    subject$electrode_types %in% c("LFP", "EKG", "Audio")
  checks <- c(
    "saved by this run" = isTRUE(as.POSIXct(params$timestamp) >= trunc(start, "secs")),
    "frequencies" = isTRUE(all.equal(as.numeric(params$frequencies), expected$frequencies)),
    "cycles" = isTRUE(all.equal(as.numeric(params$cycle), expected$cycles)),
    "precision" = identical(params$precision, precision),
    "pre-down-sample" = isTRUE(as.numeric(params$pre_downsample) == pre_downsample),
    "power sample rate" = isTRUE(subject$power_sample_rate == target_sample_rate),
    "loaded electrodes have power" = all(settings$has_wavelet[loaded]),
    "pipeline forked into the subject" = isTRUE(
      file.mtime(file.path(subject$pipeline_path, module)) >= trunc(start, "secs"))
  )
  print(data.frame(check = names(checks), passed = unname(checks)), row.names = FALSE)
  if (!all(checks)) {
    stop("The saved wavelet is not as expected")
  }
  cat("Saved wavelet is as expected (", precision, ")\n", sep = "")
}

stopifnot(app_running())
stopifnot(module_open())

# ---- protocol -----------------------------------------------------------------

app_id <- jsonlite::fromJSON(mcp_url)$app_id
cat("app id:", app_id, "\n")

tools <- mcp("tools/list")$result$tools
tool_names <- vapply(tools, `[[`, "", "name")
names(tools) <- tool_names
print(data.frame(
  tool        = tool_names,
  read_only   = vapply(tools, function(t) isTRUE(t$annotations$readOnlyHint), FALSE),
  destructive = vapply(tools, function(t) isTRUE(t$annotations$destructiveHint), FALSE)
))

# ---- meta tools ---------------------------------------------------------------

tool("shidashi_sessions")
tool("skill_load__rave-module", action = "reference",
     file_name = "references/wavelet_module.md", pattern = "Drive the module")

# ---- load data ----------------------------------------------------------------

tool("tool__module_interactive_script_list")

set_input_wait("loader_project_name", project_name)
set_input_wait("loader_subject_code", subject_code)
loaded <- run_script("load_data")
cat("load_data:", loaded$result, "\n")

# ---- configure the wavelet ----------------------------------------------------

# The builtin kernel generator (agents cannot upload a preset table)
set_input_wait("use_preset", "Builtin tool")

# `pre_downsample` choices depend on `target_sample_rate`: set that first
set_input_wait("target_sample_rate", target_sample_rate, check = same_number(target_sample_rate))
set_input_wait("pre_downsample", as.character(pre_downsample))
set_input_wait("freq_range", freq_range)
set_input_wait("freq_step", freq_step)
set_input_wait("cycle_range", cycle_range)

# ---- errors come back in `output`; nothing is written -------------------------

# An invalid input: the power sample rate must be greater than 1
set_input_wait("target_sample_rate", 1, check = same_number(1))
expect_output(run_script("run_analysis"), "invalid inputs")
stopifnot(!on_page(dialog))
# People see the error as a notification, which agents can remove
wait_until(function() on_page(".wavelet_module-error_notif"), "the error notification")
operate("remove_notification", target = "wavelet_module-error_notif")
wait_until(function() !on_page(".wavelet_module-error_notif"),
           "the error notification to go")
set_input_wait("target_sample_rate", target_sample_rate, check = same_number(target_sample_rate))

# The pipeline rejects kernels with a single cycle; `pipeline_progress` says why
set_input_wait("cycle_range", c(1, 20))
expect_output(run_script("run_analysis"), "kernels errored")
progress <- run_script("pipeline_progress")$result
stopifnot(any(grepl("^kernels: errored.*Cycles", progress)))
stopifnot(!on_page(dialog))
set_input_wait("cycle_range", cycle_range)

# ---- the confirmation dialog, and notifications for the user ------------------

expect_output(run_script("run_analysis"), "kernels completed")
wait_until(function() on_page(dialog), "the confirmation dialog")
tool("tool__shiny_query_ui", css_selector = ".modal-body", transform_image = FALSE)
tool("tool__shiny_output_result", outputId = "kernel_table", transform_image = FALSE)
cat("Expected kernel frequencies:", dipsaus::deparse_svec(expected_kernel()$frequencies), "\n")

# Cancel, as a user who declines would
operate("dismiss_modal")
wait_until(function() !on_page(dialog), "the dialog to close")

operate("show_notification", message = "test-mcp.R is about to run the wavelet",
        title = "Agent", type = "warning")
wait_until(function() on_page(".wavelet_module-agent_notification"), "the notification")
operate("remove_notification", target = "wavelet_module-agent_notification")
wait_until(function() !on_page(".wavelet_module-agent_notification"),
           "the notification to go")

if (!do_apply) {
  stop("`do_apply` is FALSE: stopped before the first wavelet run. Nothing was written to the subject.")
}

# ---- run 1: "Run wavelet" and "Confirm and run in background" (writes!) -------

set_input_wait("precision", TRUE)
run1_start <- Sys.time()
operate("click", target = "wavelet_do_btn")        # "Run wavelet", as a person would
wait_until(function() on_page(dialog), "the confirmation dialog")
operate("click", target = "wavelet_confirm_btn2")  # "Confirm and run in background"
wait_wavelet(run1_start)
check_saved(run1_start, precision = "float")
operate("close_alert2")                            # close the "Done!" alert
wait_until(function() !nzchar(alert_text()), "the alert to close")

# ---- run 2: `run_analysis` and "Confirm" (writes!) ----------------------------

set_input_wait("precision", FALSE)
run2_start <- Sys.time()
expect_output(run_script("run_analysis"), "kernels completed")
wait_until(function() on_page(dialog), "the confirmation dialog")
# "Confirm" runs in the app's R process: calls wait until the wavelet is done
operate("click", target = "wavelet_confirm_btn")
wait_wavelet(run2_start)
check_saved(run2_start, precision = "double")
operate("click", target = paste(alert, ".swal-button"))  # its OK button, as a person would
wait_until(function() !nzchar(alert_text()), "the alert to close")

cat("\nWavelet workflow passed. Restore `modules/wavelet_module/settings.yaml`",
    "from your copy.\n")
