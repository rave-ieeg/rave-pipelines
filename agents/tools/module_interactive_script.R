# agents/tools/module_interactive_script.R
#
# Root-level MCP tools: list, inspect, and run the interactive scripts that
# modules register with `server_tools$set_script()` (e.g. `load_data`,
# `run_analysis`)

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

        result <- tryCatch(
          server_tools$trigger_script(name),
          error = function(e) {
            ravepipeline::logger_error_condition(e)
            e
          }
        )
        if (inherits(result, "error")) {
          stop(sprintf("Script `%s` failed: %s", name, conditionMessage(result)))
        }

        note <- sprintf(
          "Script `%s` finished. The module's UI might be still reacting. Please give it 1 ~ 10 seconds to settle.",
          name
        )
        # Only plain values can be sent back to agents: environments,
        # promises, and large objects are left out
        if (is.atomic(result) && length(result) > 0 && length(result) <= 100) {
          return(list(note = note, result = result))
        }
        # this will print str to stdout and sent to agent
        str(result)
        return(list(note = note, result = "(Result omitted to save the agent context; see stdout for snapshot)"))
      },
      name = "module_interactive_script_run",
      description = paste(
        "Execute module interactive script by name, as if the user clicked the",
        "corresponding button in the module (e.g. `load_data` or `run_analysis`).",
        "Most interactive scripts have side-effects (e.g. change the UI inputs,",
        "pipeline settings, or saved results): check what a script does with",
        "`module_interactive_script_inspect` first.",
        "All scripts except `load_data` need the data loaded first."
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
