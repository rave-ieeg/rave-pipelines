# agents/tools/module_interactive_script.R
#
# Root-level MCP tools: list, inspect, and run the interactive scripts that
# modules register with `server_tools$set_script()` (e.g. `load_data`,
# `run_analysis`)

# Evaluate `expr` and collect what it prints, so a script can report to agents
# by printing. Returns `value` (NULL on error), `error` (the condition, or
# NULL), and `output`: stdout, then the message stream (message(), cli and
# `targets` progress, warnings, and anything written to stderr, such as
# `ravepipeline::logger()`), without ANSI codes, cut to the last `max_chars`
# characters. The console still gets everything: stdout as it is printed, the
# message stream once `expr` finishes.
capture_script_output <- function(expr, max_chars = 3000) {
  stdout_lines <- character()
  message_lines <- character()
  stdout_con <- textConnection("stdout_lines", open = "w", local = TRUE)
  message_con <- textConnection("message_lines", open = "w", local = TRUE)

  n_sinks <- sink.number()
  message_sink <- sink.number(type = "message")
  sink(stdout_con, split = TRUE)
  sink(message_con, type = "message")

  restored <- FALSE
  restore <- function() {
    if (restored) { return() }
    restored <<- TRUE
    if (message_sink == 2L) {
      sink(type = "message")
    } else {
      sink(getConnection(message_sink), type = "message")
    }
    while (sink.number() > n_sinks) { sink() }
    close(stdout_con)
    close(message_con)
  }
  on.exit(restore(), add = TRUE)

  # Messages go into the same buffer as raw stderr, in order; they are
  # muffled so they are not written twice
  write_message <- function(text) {
    text <- paste(text, collapse = "")
    if (!endsWith(text, "\n")) { text <- paste0(text, "\n") }
    cat(text, file = message_con)
  }
  result <- withCallingHandlers(
    tryCatch(
      list(value = expr, error = NULL),
      error = function(e) { list(value = NULL, error = e) }
    ),
    message = function(m) {
      write_message(conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      write_message(paste("Warning:", conditionMessage(w)))
    }
  )
  restore()

  if (length(message_lines)) {
    writeLines(message_lines, con = stderr())
  }

  output <- trimws(cli::ansi_strip(paste(c(stdout_lines, message_lines), collapse = "\n")))
  if (nchar(output) > max_chars) {
    output <- paste0("...", substr(output, nchar(output) - max_chars + 1L, nchar(output)))
  }
  result$output <- output
  result
}

module_interactive_script_list <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function() {
        server_tools <- ravedash::get_default_handlers(session = session)
        # Same check as `module_interactive_script_run`
        data_loaded <- shiny::isolate(
          !ravedash::watch_loader_opened(session = session) &&
            isTRUE(ravedash::watch_data_loaded(session = session))
        )
        script_names <- server_tools$list_scripts()
        scripts <- unname(lapply(script_names, function(name) {
          list(
            name = name,
            description = server_tools$get_script(name)$description
          )
        }))

        if (data_loaded) {
          return(list(
            note = "Data loaded. All scripts are available.",
            scripts = scripts
          ))
        }

        if ("load_data" %in% script_names) {
          return(list(
            note = "Data not yet loaded (or loader screen is active). Only 'load_data' is allowed. Please load data first.",
            scripts = scripts
          ))
        } else {
          return(list(
            note = "Data not yet loaded (or loader screen is active). Please ask the user to load data from the dashboard first.",
            scripts = scripts
          ))
        }
      },
      name = "module_interactive_script_list",
      description = paste(
        "List the interactive scripts registered by the module (names and",
        "descriptions), and whether the data are loaded.",
        "Unlike other tools, interactive scripts act on the live app session,",
        "as if the user clicked the module's buttons, e.g. `load_data` (load",
        "the data chosen in the loader) or `run_analysis` (run the analysis),",
        "if the module has them.",
        "Use `module_interactive_script_inspect` to check the details by name;",
        "and `module_interactive_script_run` to execute script by name."
      ),
      arguments = list()
    )
  }

)

module_interactive_script_inspect <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function(name) {
        server_tools <- ravedash::get_default_handlers(session = session)
        script_names <- server_tools$list_scripts()
        if (!isTRUE(name %in% script_names)) {
          return(sprintf(
            "No interactive script named `%s`. Available scripts: %s",
            name, paste(sprintf("`%s`", script_names), collapse = ", ")
          ))
        }
        # The registry entry also holds an environment and a shiny observer,
        # which cannot be sent to agents: return plain values only
        script <- server_tools$get_script(name)
        list(
          name = name,
          description = script$description,
          requires_loaded_data = !identical(name, "load_data"),
          running = isTRUE(script$running),
          code = paste(deparse(script$expr), collapse = "\n")
        )
      },
      name = "module_interactive_script_inspect",
      description = paste(
        "Inspect an interactive script: its description, whether it needs",
        "the data loaded, whether it is running, and its R code.",
        "Use `module_interactive_script_run` to execute script by name."
      ),
      arguments = list(
        name = ellmer::type_string(
          description = "Interactive script name, must be included from the result returned by tool module_interactive_script_list",
          required = TRUE
        )
      )
    )
  }

)

module_interactive_script_run <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function(name) {
        server_tools <- ravedash::get_default_handlers(session = session)
        script_names <- server_tools$list_scripts()
        if (!isTRUE(name %in% script_names)) {
          stop(sprintf(
            "No interactive script named `%s`. Available scripts: %s",
            name, paste(sprintf("`%s`", script_names), collapse = ", ")
          ))
        }

        if (!identical(name, "load_data")) {
          # load_data is the only special script. All other scripts must have data loaded
          not_ready <- shiny::isolate(
            ravedash::watch_loader_opened(session = session) ||
              !isTRUE(ravedash::watch_data_loaded(session = session))
          )
          if (not_ready) {
            stop(
              "Script not started: the data are not loaded or the data loader is currently opened.",
              "Please run script `load_data` to load the data first."
            )
          }
        }

        # What the script prints goes to the agent as `output`
        captured <- capture_script_output(server_tools$trigger_script(name))
        if (!is.null(captured$error)) {
          ravepipeline::logger_error_condition(captured$error)
          stop(paste(c(
            sprintf("Script `%s` failed: %s", name, conditionMessage(captured$error)),
            if (nzchar(captured$output)) c("Console output:", captured$output)
          ), collapse = "\n"))
        }
        result <- captured$value
        output <- captured$output

        note <- sprintf(
          "Script `%s` finished. The module's UI might be still reacting. Please give it 1 ~ 10 seconds to settle.",
          name
        )
        reply <- list(note = note)
        # Only plain values can be sent back to agents: environments,
        # promises, and large objects are summarized with `str()` instead
        if (is.atomic(result) && length(result) > 0 && length(result) <= 100) {
          reply$result <- result
        } else if (!is.null(result)) {
          reply$result <- "(Result omitted to save the agent context; its `str()` is at the end of `output`)"
          output <- paste(c(
            output, "str(result):",
            utils::capture.output(utils::str(result, max.level = 1, list.len = 10))
          ), collapse = "\n")
        }
        if (nzchar(output)) {
          reply$output <- output
        }
        reply
      },
      name = "module_interactive_script_run",
      description = paste(
        "Execute module interactive script by name, as if the user clicked the",
        "corresponding button in the module (e.g. `load_data` or `run_analysis`).",
        "Most interactive scripts have side-effects (e.g. change the UI inputs,",
        "pipeline settings, or saved results): check what a script does with",
        "`module_interactive_script_inspect` first.",
        "All scripts except `load_data` need the data loaded first.",
        "The reply has the script's return value (`result`) and what it printed",
        "while it ran (`output`: console output, messages, pipeline progress, and",
        "errors a module logs; the last 3000 characters). Read `output`: a script",
        "can print an error and still finish. Anything the module prints after",
        "the script returns (e.g. a dialog opening) is not included."
      ),
      arguments = list(
        name = ellmer::type_string(
          description = "Interactive script name, must be included from the result returned by tool module_interactive_script_list",
          required = TRUE
        )
      )
    )
  }

)
