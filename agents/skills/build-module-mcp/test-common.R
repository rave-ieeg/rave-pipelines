# agents/skills/build-module-mcp/test-common.R
#
# Helpers shared by the modules' `test-mcp.R`: call a running app's MCP tools
# the way an agent does, and check what the module did.
#
# Usage, at the top of a module's test (run from the repository root):
#
#   module <- "power_clust"                 # required: the module ID
#   source("agents/skills/build-module-mcp/test-common.R")
#
# The app's port comes from `port` when it is set before sourcing, otherwise
# from the environment variable `RAVE_TEST_PORT` (default 17283, the
# developer's own app: tests normally run on a test copy, port 17299).
#
# Tool arguments are R values, not JSON text: `httr2` sends them as JSON and
# the app decodes them into the same R values a JSON string would give
# (`simplifyVector = TRUE`, `simplifyDataFrame = FALSE`). A vector of length
# one becomes a scalar: wrap it in `I()` to send a one-element array (e.g.
# `I("drive_a")`); `list()` sends an empty array. Agents themselves send JSON
# strings (`"[0, 1]"`); both reach the module the same way.

if (!exists("module", inherits = FALSE) || !is.character(module)) {
  stop("Set `module` (the module ID) before sourcing test-common.R")
}
if (!exists("port", inherits = FALSE)) {
  port <- as.integer(Sys.getenv("RAVE_TEST_PORT", "17283"))
}
base    <- sprintf("http://127.0.0.1:%d", port)
mcp_url <- paste0(base, "/mcp")
no_args <- structure(list(), names = character(0))   # sent as {}

# ---- MCP requests --------------------------------------------------------------

# Send one JSON-RPC request to the app and return the parsed reply
mcp <- function(method, params = no_args, url = mcp_url, timeout = 900) {
  httr2::request(url) |>
    httr2::req_body_json(list(jsonrpc = "2.0", id = 1, method = method,
                              params = params)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# Call a tool with R-native arguments in `...`, print its reply (unless
# `.quiet`), and return the reply text invisibly: one string per content item
# (an image becomes "[image: <type>, <n> characters]"); attribute `is_error`
# tells whether the call failed
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

# The JSON answer of a tool reply (its first content item) as R values
reply_value <- function(text, ...) {
  jsonlite::fromJSON(text[[1]], ...)
}

# Whether an image came back (e.g. from `shiny_query_ui` or
# `shiny_output_result`)
has_image <- function(text) any(grepl("^\\[image", text))

# ---- the app and the module ------------------------------------------------------

app_running <- function() {
  up <- suppressWarnings(try(readLines(mcp_url, warn = FALSE), silent = TRUE))
  !inherits(up, "try-error")
}

module_open <- function() {
  reply <- mcp("tools/call", list(name = "shidashi_sessions", arguments = no_args))
  sessions <- jsonlite::fromJSON(reply$result$content[[1]]$text)
  module %in% sessions$open_modules$module_id
}

# The tools offered to this module (`tools/list` is app-wide)
module_tools <- function() {
  reply <- mcp("tools/call", list(name = "shidashi_sessions", arguments = no_args))
  sessions <- jsonlite::fromJSON(reply$result$content[[1]]$text,
                                 simplifyVector = FALSE)
  this_module <- Filter(function(m) identical(m$module_id, module),
                        sessions$open_modules)
  if (!length(this_module)) stop("Module `", module, "` is not open")
  unlist(this_module[[1]]$tools)
}

# The module's interactive scripts: list with `note` and `scripts` (each with
# `name` and `description`)
list_scripts <- function(.quiet = FALSE) {
  jsonlite::fromJSON(tool("tool__module_interactive_script_list",
                          .quiet = .quiet)[[1]], simplifyVector = FALSE)
}

# Whether the module reports its data as loaded (and the loader closed)
data_loaded <- function() {
  startsWith(list_scripts(.quiet = TRUE)$note, "Data loaded")
}

# ---- inputs ------------------------------------------------------------------------

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

# Set an input through `shiny_input_update`, with an R-native value (see the
# note at the top); stops if the tool refuses
set_input <- function(id, value) {
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

# Checks for input values that come back in another shape
same_number <- function(x) {
  function(value) isTRUE(all(as.numeric(unlist(value)) == x))
}
same_set <- function(x) {
  function(value) {
    length(unlist(value)) == length(x) && setequal(as.character(unlist(value)), x)
  }
}
is_true <- function(value) isTRUE(as.logical(unlist(value)))
is_false <- function(value) isFALSE(as.logical(unlist(value)))
is_empty <- function(value) !length(unlist(value))

# ---- scripts -----------------------------------------------------------------------

# Run an interactive script; returns its reply: `note`, `result` (its return
# value), and `output` (what it printed). Stops if the script fails
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

# Stop unless the script printed `pattern` (a regular expression)
expect_output <- function(reply, pattern) {
  if (!isTRUE(grepl(pattern, reply$output))) {
    stop(sprintf("Expected `%s` in the script output; got:\n%s",
                 pattern, paste(reply$output, collapse = "\n")))
  }
  invisible(reply)
}

# ---- the page ----------------------------------------------------------------------

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

# Title of the open dialog ("" if none)
dialog_title <- function() page_text(".modal.show .modal-title")

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

# ---- outputs -----------------------------------------------------------------------

# Text of a registered output (`shiny_output_result`), without tags: the
# rendered HTML, plus the data block of downloadable outputs
output_text <- function(output_id, max_chars = 200000L) {
  text <- tool("tool__shiny_output_result", outputId = output_id,
               transform_image = FALSE, max_chars = max_chars, .quiet = TRUE)
  if (isTRUE(attr(text, "is_error"))) stop("Cannot read output ", output_id)
  html <- paste(text[!startsWith(text, "[shidashi]")], collapse = "\n")
  trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", html)))
}

# The data block of a registered output that has a download ("Data of this
# output ...") or is an htmlwidget ("Full data of this widget ..."), without
# its heading line
output_data <- function(output_id, max_chars = 200000L) {
  text <- tool("tool__shiny_output_result", outputId = output_id,
               transform_image = FALSE, max_chars = max_chars, .quiet = TRUE)
  if (isTRUE(attr(text, "is_error"))) stop("Cannot read output ", output_id)
  data_text <- text[grepl("^(Data of this output|Full data of this widget)", text)]
  if (!length(data_text)) stop("Output ", output_id, " returned no data")
  sub("^[^\n]*\n", "", data_text[[1]])
}

# ---- 3D viewer (tools `rave_3dviewer_get` / `rave_3dviewer_set`) -------------------
# `outputId` defaults to the test's variable `viewer`

viewer_get <- function(name, args = NULL, outputId = viewer, .quiet = TRUE) {
  call_args <- list(.name = "tool__rave_3dviewer_get", outputId = outputId,
                    name = name, .quiet = .quiet)
  if (!is.null(args)) call_args$args <- args
  text <- do.call(tool, call_args)
  if (isTRUE(attr(text, "is_error"))) {
    stop(sprintf("rave_3dviewer_get `%s` failed: %s", name, text[[1]]))
  }
  jsonlite::fromJSON(text[[1]], simplifyVector = TRUE)
}

# Returns the reply text (attribute `is_error`)
viewer_set <- function(name, data, outputId = viewer, .quiet = FALSE) {
  tool("tool__rave_3dviewer_set", outputId = outputId, name = name,
       data = data, .quiet = .quiet)
}

# ---- the module's files ------------------------------------------------------------

# The settings the module saved, read from its settings.yaml
settings_file <- file.path("modules", module, "settings.yaml")
saved_settings <- function() yaml::read_yaml(settings_file)

# When a pipeline target was last built (targets metadata); `pipe` defaults
# to the test's variable `pipeline`
target_time <- function(name, pipe = pipeline) {
  meta <- pipe$with_activated(targets::tar_meta(names = dplyr::all_of(name),
                                                fields = "time"))
  as.POSIXct(meta$time[[1]])
}

# Print a named logical vector of checks as a table; stop unless all passed
check_all <- function(checks, what = "checks") {
  print(data.frame(check = names(checks), passed = unname(checks)),
        row.names = FALSE)
  if (!all(checks)) {
    stop(what, " failed: ", paste(names(checks)[!checks], collapse = ", "))
  }
  invisible(TRUE)
}
