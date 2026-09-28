# Manually test the MCP tools: build three references for one subject the way
# an agent would, without clicking in the browser (a browser session with the
# module open is still needed: input updates round-trip through it)
#
#   1. bipolar, skipping the excluded channels
#   2. common average of all channels but the excluded ones
#   3. common average of the channels found by CARLA (excluded ones left out)
#
# Excluded channels stay in the reference table; they are only left out of
# the reference signals. In bipolar groups, the chain skips them (with 17
# excluded, 16 references 18) and they get no reference ("noref"), as does
# the last channel of each group.
#
# Each reference is saved to the subject and read back to check it. A live run
# rewrites `modules/reference_module/settings.yaml`: check `git diff` afterwards.

port   <- as.integer(Sys.getenv("RAVE_TEST_PORT", "17283"))  # port used for testing
module <- "reference_module"

# test subject and settings
project_name <- "test@bids:ds005953"
subject_code <- "01"
excluded_channels <- "17"
save_names   <- c(bipolar = "mcp_bipolar", car = "mcp_car", carla = "mcp_carla")

# Custom groups on purpose (the labels give groups LA 1-12 and LB 13-20), to
# check that groupings other than the default can be set
test_groups <- list(
  list(name = "LA", electrodes = "1-5"),
  list(name = "LB", electrodes = "6-10"),
  list(name = "LC", electrodes = "11-15"),
  list(name = "LD", electrodes = "16-20")
)

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
# sent again every few seconds: e.g. a subject can only be chosen once the
# subject list of the new project has arrived
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

# Run an interactive script; returns its result, stops if it fails
run_script <- function(name) {
  text <- tool("tool__module_interactive_script_run", name = name)
  if (isTRUE(attr(text, "is_error"))) {
    stop(sprintf("Script `%s` failed: %s", name, text[[1]]))
  }
  invisible(jsonlite::fromJSON(text[[1]])$result)
}

data_loaded <- function() {
  text <- tool("tool__module_interactive_script_list", .quiet = TRUE)
  startsWith(jsonlite::fromJSON(text[[1]])$note, "Data loaded")
}

# Set the electrode groups and apply them
set_groups <- function(groups) {
  set_input_wait("electrode_group", groups, check = function(current) {
    as_pairs <- function(x) lapply(unname(x), function(g) c(g$name, g$electrodes))
    identical(as_pairs(current), as_pairs(groups))
  })
  run_script("update_electrode_group")
}

# Choose a group and its reference type. Choosing a group resets
# `reference_type` to the group's current type, so the type is set after
# the group settles
select_group <- function(group, type) {
  set_input_wait("group_name", group)
  Sys.sleep(1)
  set_input_wait("reference_type", type)
  Sys.sleep(0.5)
  wait_input("reference_type", function(v) identical(v, type), timeout = 5)
}

# Open the `Preview & Export` tab, show the table, and save it as `name`
save_as <- function(name) {
  set_input("reference_output_tabset", "Preview & Export")
  wait_input("preview_save_name")
  tool("tool__shiny_query_ui", css_selector = "#reference_module-reference_table_preview",
       transform_image = FALSE)
  set_input_wait("preview_save_name", name)
  run_script("save_reference")
}

# The saved reference table: Reference by Electrode
read_saved <- function(name) {
  subject <- ravecore::as_rave_subject(sprintf("%s/%s", project_name, subject_code))
  path <- file.path(subject$meta_path, sprintf("reference_%s.csv", name))
  tbl <- utils::read.csv(path, stringsAsFactors = FALSE,
                         colClasses = c(Reference = "character"))
  tbl$Reference[is.na(tbl$Reference)] <- ""
  structure(tbl$Reference, names = tbl$Electrode)
}

check_saved <- function(name, expected) {
  actual <- read_saved(name)[names(expected)]
  print(data.frame(Electrode = names(expected), expected = unname(expected),
                   actual = unname(actual)), row.names = FALSE)
  if (!identical(unname(actual), unname(expected))) {
    stop("Reference [", name, "] is not as expected")
  }
  cat("Reference [", name, "] is as expected\n", sep = "")
}

# Expected bipolar references: within each group, each channel references
# the next one and the last gets "noref"; excluded channels get "noref"
expected_bipolar <- function(groups, excluded) {
  refs <- unlist(lapply(groups, function(g) {
    channels <- dipsaus::parse_svec(g$electrodes)
    channels <- channels[!channels %in% excluded]
    if (!length(channels)) return(NULL)
    structure(c(sprintf("ref_%d", channels[-1]), "noref"), names = channels)
  }))
  refs[as.character(excluded)] <- "noref"
  refs[order(as.integer(names(refs)))]
}

stopifnot(app_running())
stopifnot(module_open())

# ---- protocol ---------------------------------------------------------------------

app_id <- jsonlite::fromJSON(mcp_url)$app_id
cat("app id:", app_id, "\n")

tools <- mcp("tools/list")$result$tools
tool_names <- vapply(tools, `[[`, "", "name")
names(tools) <- tool_names
tool_table <- print(data.frame(
  tool        = tool_names,
  read_only   = vapply(tools, function(t) isTRUE(t$annotations$readOnlyHint), FALSE),
  destructive = vapply(tools, function(t) isTRUE(t$annotations$destructiveHint), FALSE)
))

# ---- meta tools -------------------------------------------------------------------

tool("shidashi_sessions")
tool("skill_load__rave-module", action = "reference",
     file_name = "references/reference_module.md", pattern = "Drive the module")

# ---- load data ---------------------------------------------------------------------

tool("tool__module_interactive_script_list")

set_input_wait("loader_project_name", project_name)
set_input_wait("loader_subject_code", subject_code)
set_input_wait("loader_reference_name", "[Blank profile]")

run_script("load_data")
start <- Sys.time()
while (!data_loaded()) {
  if (difftime(Sys.time(), start, units = "secs") > 120) stop("Data not loaded")
  Sys.sleep(1)
}

# All LFP channels, from the groups that the blank profile made (wait for
# them: the input starts with one empty group)
initial_groups <- wait_input("electrode_group", function(v) {
  electrodes <- vapply(v, function(g) paste(g$electrodes, collapse = ""), "")
  length(v) > 0 && all(nzchar(electrodes))
})
lfp_channels <- sort(unique(unlist(lapply(initial_groups, function(g) {
  dipsaus::parse_svec(g$electrodes)
}))))
excluded <- dipsaus::parse_svec(excluded_channels)
good_channels <- dipsaus::deparse_svec(lfp_channels[!lfp_channels %in% excluded])
cat("LFP channels:", dipsaus::deparse_svec(lfp_channels),
    "; without the excluded channels:", good_channels, "\n")

# ---- 1. bipolar ----------------------------------------------------------------------

# The excluded channels move into their own group with no reference, so the
# bipolar chains skip them
bipolar_groups <- c(
  lapply(test_groups, function(g) {
    channels <- dipsaus::parse_svec(g$electrodes)
    list(name = g$name,
         electrodes = dipsaus::deparse_svec(channels[!channels %in% excluded]))
  }),
  list(list(name = "Excluded", electrodes = excluded_channels))
)

set_groups(bipolar_groups)
for (g in bipolar_groups) {
  type <- if (g$name == "Excluded") "No Reference" else "Bipolar Reference"
  select_group(g$name, type)
  run_script("update_group_reference")
}
save_as(save_names[["bipolar"]])

check_saved(save_names[["bipolar"]], expected_bipolar(test_groups, excluded))

# ---- 2. common average of all channels but the excluded ones (one group) --------------

set_groups(list(list(name = "CAR", electrodes = dipsaus::deparse_svec(lfp_channels))))
select_group("CAR", "Common Average Reference")
set_input_wait("reference_channels", "[new reference]")
set_input_wait("reference_channels_new", good_channels)
car_ref <- run_script("generate_reference")
wait_input("reference_channels", function(v) identical(v, car_ref), timeout = 60)
run_script("update_group_reference")
save_as(save_names[["car"]])

# every channel, the excluded ones included, is referenced to the average
check_saved(save_names[["car"]], structure(
  rep(car_ref, length(lfp_channels)), names = lfp_channels))

# ---- 3. common average from CARLA (same signal for every group) ------------------------

set_groups(test_groups)
select_group(test_groups[[1]]$name, "Common Average Reference")
set_input_wait("reference_channels", "[new reference]")
# CARLA candidates: all channels but the excluded ones
set_input_wait("reference_channels_new", good_channels)
carla_channels <- run_script("estimate_carla")
cat("CARLA channels:", carla_channels, "\n")
stopifnot(!any(dipsaus::parse_svec(carla_channels) %in% excluded))
wait_input("reference_channels_new", function(v) identical(v, carla_channels))
carla_ref <- run_script("generate_reference")
wait_input("reference_channels", function(v) identical(v, carla_ref), timeout = 60)
run_script("update_group_reference")

for (g in test_groups[-1]) {
  select_group(g$name, "Common Average Reference")
  set_input_wait("reference_channels", carla_ref)
  run_script("update_group_reference")
}
save_as(save_names[["carla"]])

check_saved(save_names[["carla"]], structure(
  rep(carla_ref, length(lfp_channels)), names = lfp_channels))

cat("\nAll workflows passed. Check `git diff -- modules/reference_module/settings.yaml`.\n")
