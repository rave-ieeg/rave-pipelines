# Manually test the MCP tools of the Data Tools module the way an agent would,
# without clicking in the browser. A browser session with the module open is
# still needed: input updates round-trip through it.
#
# Agents may only validate subjects and export data:
#   * `load_data` loads the subject, even a broken one
#   * `run_analysis` (the "Validate subject" button) checks the subject's
#     files; the read-only script `validation_results` lists the checks
#   * `generate_exports` (the "Generate exports" button) writes a new export
#     folder
#   * the RAVE 1.0 conversion is for people only: its button is registered
#     read-only, and this module does not offer `shiny_ui_operate`, which
#     clicks any element
#
# Nothing converts a subject. The failing checks come from a subject that was
# never imported; nothing is written to it. Then one export writes a new
# folder, `rave/exports/rave-repository/export-<time>`, into the export
# subject (42 MB for demo/DemoSubject); set `do_write <- FALSE` to stop before
# it. A live run also rewrites `modules/compatibility_rave1/settings.yaml`:
# copy it first and restore it from the copy (`git checkout` would also drop
# uncommitted edits).

port   <- as.integer(Sys.getenv("RAVE_TEST_PORT", "17283"))  # port used for testing
module <- "compatibility_rave1"

# test subjects and settings
broken_project   <- "demo"          # never imported: its checks fail; nothing written
broken_subject   <- "KC"
project_name     <- "demo"          # validated, then exported (writes a new folder!)
subject_code     <- "DemoSubject"
do_write         <- TRUE            # FALSE: stop before the export

export_channels  <- c(14, 15)
export_epoch     <- "auditory_onset"
export_reference <- "default"
export_window    <- c(-1, 2)        # seconds around each onset

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

get_subject <- function(project, subject) {
  ravecore::as_rave_subject(sprintf("%s/%s", project, subject), strict = FALSE)
}

# Every file in a subject's folder, with its size and modification time
snapshot <- function(project, subject) {
  root <- get_subject(project, subject)$path
  files <- sort(list.files(root, recursive = TRUE, all.files = TRUE,
                           full.names = TRUE))
  file.info(files)[, c("size", "mtime")]
}

# The reply script `validation_results` should give, computed here from
# ravecore directly
expected_validation <- function(project, subject, method, version) {
  results <- ravecore::validate_subject(
    sprintf("%s/%s", project, subject), method = method, version = version,
    verbose = FALSE)
  keys <- c("paths", "preprocess", "meta", "voltage_data",
            "power_phase_data", "epoch_tables", "reference_tables")
  checks <- character()
  statuses <- character()
  lines <- character()
  for (k in keys) {
    for (nm in names(results[[k]])) {
      item <- results[[k]][[nm]]
      status <- if (isTRUE(item$valid)) {
        "passed"
      } else if (is.na(item$valid)) {
        "skipped"
      } else if (identical(item$severity, "minor")) {
        "minor"
      } else {
        "failed"
      }
      checks <- c(checks, sprintf("%s/%s", k, nm))
      statuses <- c(statuses, status)
      lines <- c(lines, sprintf("[%s] %s/%s: %s - %s", status, k, nm,
                                item$description,
                                paste(item$message, collapse = " ")))
    }
  }
  passed <- statuses == "passed"
  c(
    sprintf("%d checks: %d passed, %d failed, %d minor, %d skipped",
            length(statuses), sum(passed), sum(statuses == "failed"),
            sum(statuses == "minor"), sum(statuses == "skipped")),
    lines[!passed],
    sprintf("Passed: %s",
            if (any(passed)) paste(checks[passed], collapse = ", ") else "none")
  )
}

# Stop unless script `validation_results` gives exactly what ravecore finds
check_validation <- function(project, subject, method, version) {
  actual <- as.character(run_script("validation_results", .quiet = TRUE)$result)
  expected <- expected_validation(project, subject, method, version)
  if (!identical(actual, expected)) {
    cat("Expected:\n")
    writeLines(expected)
    cat("Got:\n")
    writeLines(actual)
    stop("`validation_results` differs from ravecore::validate_subject()")
  }
  cat(sprintf("validation_results (%s, version %s) as expected: %s\n",
              method, version, expected[[1]]))
  writeLines(paste(" ", expected[-1]))
  invisible(actual)
}

# The export folders of the export subject
exports_root <- function() {
  file.path(get_subject(project_name, subject_code)$rave_path,
            "exports", "rave-repository")
}
list_exports <- function() {
  root <- exports_root()
  if (!dir.exists(root)) return(character())
  sort(normalizePath(list.dirs(root, recursive = FALSE, full.names = TRUE)))
}

# Compare an export folder written after `start` with the export inputs
check_export <- function(path, start) {
  subject <- get_subject(project_name, subject_code)
  summary <- yaml::read_yaml(file.path(path, "summary.yaml"))
  trials <- subject$get_epoch(export_epoch)$table$Trial
  frequencies <- utils::read.csv(file.path(subject$meta_path,
                                           "frequencies.csv"))$Frequency
  times <- seq(export_window[[1]], export_window[[2]],
               by = 1 / subject$power_sample_rate)
  stamp <- as.POSIXct(sub("^export-", "", basename(path)),
                      format = "%y%m%dT%H%M%S")
  checks <- c(
    "written by this run" = isTRUE(stamp >= trunc(start, "secs")),
    "project" = identical(summary$project_name, project_name),
    "subject" = identical(summary$subject_code, subject_code),
    "channels" = identical(as.integer(unlist(summary$loaded_electrodes)),
                           as.integer(export_channels)),
    "reference" = identical(summary$reference_name, export_reference),
    "epoch" = identical(summary$epoch_name, export_epoch),
    "window" = isTRUE(all.equal(as.numeric(unlist(summary$time_windows)),
                                export_window)),
    "dimensions" = identical(unlist(summary$power$names),
                             c("Frequency", "Time", "Trial", "Electrode")),
    "tables" = all(file.exists(file.path(
      path, c("electrodes.csv", "reference.csv", "with_epochs/epoch.csv"))))
  )
  for (e in export_channels) {
    file <- file.path(path, "with_epochs", "power", sprintf("ch%04d.mat", e))
    mat <- if (file.exists(file)) R.matlab::readMat(file, fixNames = FALSE) else list()
    label <- basename(file)
    checks[[paste(label, "electrode")]] <-
      identical(as.integer(mat$electrode), as.integer(e))
    checks[[paste(label, "trials")]] <-
      identical(sort(as.integer(mat$trial_number)), sort(as.integer(trials)))
    checks[[paste(label, "times")]] <-
      isTRUE(all.equal(as.numeric(mat$time_in_secs), times))
    checks[[paste(label, "frequencies")]] <-
      isTRUE(all.equal(as.numeric(mat$frequency), as.numeric(frequencies)))
    checks[[paste(label, "data size")]] <-
      identical(as.integer(dim(mat$data))[1:3],
                c(length(frequencies), length(times), length(trials)))
  }
  print(data.frame(check = names(checks), passed = unname(checks)),
        row.names = FALSE)
  if (!all(checks)) {
    stop("The export is not as expected")
  }
  cat("Export is as expected:", path, "\n")
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
     file_name = "references/compatibility_rave1.md", pattern = "Drive the module")

listed <- jsonlite::fromJSON(tool("tool__module_interactive_script_list")[[1]],
                             simplifyVector = FALSE)
script_names <- vapply(listed$scripts, `[[`, "", "name")
stopifnot(setequal(script_names, c("load_data", "run_analysis",
                                   "validation_results", "generate_exports")))

# ---- a subject that was never imported: its checks fail -------------------------

broken_before <- snapshot(broken_project, broken_subject)

set_input_wait("loader_project_name", broken_project)
set_input_wait("loader_subject_code", broken_subject)
loaded <- run_script("load_data")
cat("load_data:", loaded$result, "\n")
stopifnot(startsWith(loaded$result,
                     sprintf("Loaded %s/%s:", broken_project, broken_subject)))

set_input_wait("validation_version", "2")
set_input_wait("validation_mode", "basic")
run_script("run_analysis")
broken_results <- check_validation(broken_project, broken_subject, "basic", 2)
stopifnot(grepl(" [1-9][0-9]* failed", broken_results[[1]]))

# ---- the conversion button is locked --------------------------------------------

# A click would show "Converting in progress", then "Conversion failed"
info <- input_info("compatibility_do")
stopifnot(isTRUE(info$exists), isFALSE(info$writable))
text <- tool("tool__shiny_input_update", inputId = "compatibility_do", value = "1")
stopifnot(isTRUE(attr(text, "is_error")), grepl("read-only", text[[1]]))
text <- tool("tool__shiny_ui_operate", action = "click",
             target = "compatibility_do", `_module` = module)
stopifnot(isTRUE(attr(text, "is_error")))
Sys.sleep(3)
stopifnot(!nzchar(alert_text()))
stopifnot(identical(snapshot(broken_project, broken_subject), broken_before))
cat("The conversion button is locked; nothing was written to",
    sprintf("%s/%s", broken_project, broken_subject), "\n")

# ---- the export subject: validation ---------------------------------------------

set_input_wait("loader_project_name", project_name)
set_input_wait("loader_subject_code", subject_code)
loaded <- run_script("load_data")
cat("load_data:", loaded$result, "\n")
stopifnot(
  startsWith(loaded$result, sprintf("Loaded %s/%s:", project_name, subject_code)),
  grepl(export_epoch, loaded$result, fixed = TRUE),
  grepl(export_reference, loaded$result, fixed = TRUE)
)

# Loading clears the results of the previous subject
wait_until(function() {
  result <- run_script("validation_results", .quiet = TRUE)$result
  startsWith(result[[1]], "No validation results")
}, "the previous validation results to clear")

# Open the card, as an agent does to show the user, then validate
set_input("quickaccess_data_integrity", "1")
set_input_wait("validation_version", "2")
set_input_wait("validation_mode", "normal")
reply <- run_script("run_analysis")
check_validation(project_name, subject_code, "normal", 2)
stopifnot(!nzchar(alert_text()))     # "Validation in progress..." has closed
wait_until(function() {
  grepl("valid: yes", page_text("#compatibility_rave1-validation_check"))
}, "the results in the open card")

# Validate again while the card is collapsed, then open it: the card shows
# the new results
set_input("quickaccess_export", "1")
set_input_wait("validation_version", "1")
run_script("run_analysis")
check_validation(project_name, subject_code, "normal", 1)
set_input("quickaccess_data_integrity", "1")
wait_until(function() {
  grepl("valid: yes", page_text("#compatibility_rave1-validation_check"))
}, "the results after opening the card")

# ---- exports ----------------------------------------------------------------------

set_input("quickaccess_export", "1")

# Raw voltage has no reference: the module sets `noref`
set_input_wait("export_type", "raw-voltage")
wait_input("export_reference", function(value) identical(value, "noref"))

set_input_wait("export_type", "power")
set_input_wait("export_reference", export_reference)
set_input_wait("export_epoch", export_epoch)
set_input_wait("export_electrode", dipsaus::deparse_svec(export_channels))
set_input_wait("export_post", export_window[[2]],
               check = same_number(export_window[[2]]))

# An invalid input: nothing is written, and no alert opens. People see the
# rule under the input
set_input_wait("export_pre", 0.5, check = same_number(0.5))
before <- list_exports()
error <- run_script_error("generate_exports")
stopifnot(grepl("Please correct the inputs before exporting data", error))
stopifnot(identical(list_exports(), before))
Sys.sleep(2)
stopifnot(!nzchar(alert_text()))
message_text <- page_text(".shiny-input-container:has(#compatibility_rave1-export_pre)")
cat("Under `export_pre`:", message_text, "\n")
stopifnot(grepl("Please choose a negative number", message_text))

set_input_wait("export_pre", export_window[[1]],
               check = same_number(export_window[[1]]))

if (!do_write) {
  stop("`do_write` is FALSE: stopped before the export. Nothing was written to the subjects.")
}

# ---- the export (writes a new folder!) --------------------------------------------

start <- Sys.time()
before <- list_exports()
reply <- run_script("generate_exports")
path <- normalizePath(reply$result)
stopifnot(identical(setdiff(list_exports(), before), path))
check_export(path, start)
wait_until(function() {
  message <- alert_text()
  grepl("Success!", message, fixed = TRUE) &&
    grepl(basename(path), message, fixed = TRUE)
}, "the 'Success!' alert with the export path")

cat("\nData Tools workflow passed. It wrote", path,
    "\nThe 'Success!' alert stays open until someone closes it.",
    "\nRestore `modules/compatibility_rave1/settings.yaml` from your copy.\n")
