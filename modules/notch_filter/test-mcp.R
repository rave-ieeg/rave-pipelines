# Manually test the MCP tools

module <- "notch_filter"
source("agents/skills/build-module-mcp/test-common.R")  # shared MCP test helpers

stopifnot(app_running())
stopifnot(module_open())

# ---- protocol ---------------------------------------------------------------------

jsonlite::fromJSON(mcp_url)

app_id <- jsonlite::fromJSON(mcp_url)$app_id
cat("app id:", app_id, "\n")
cat("instructions:", mcp("initialize")$result$instructions, "\n")

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
tool("shidashi_tools")
tool("shidashi_call", tool = "skill_load__rave-module",
     arguments = list(action='readme'))

# ---- module inputs -----------------------------------------------------------------

tools$tool__shiny_input_info
input_info <- tool("tool__shiny_input_info")
jsonlite::fromJSON(input_info[[1]])

tool("tool__shiny_input_info", inputIds = "loader_project_name")
tools$tool__shiny_input_update$inputSchema
tool("tool__shiny_input_update", inputId = "loader_project_name", value = "test2")
tool("tool__shiny_input_info", inputIds = "loader_project_name")

tool("tool__shiny_input_info", inputIds = "loader_subject_code")
tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "DemoSubject")
tool("tool__shiny_input_info", inputIds = "loader_subject_code")

# ---- module tools -----------------------------------------------------------------

tool("tool__module_interactive_script_list")
tool("tool__module_interactive_script_inspect", name = "load_data")

tool("tool__module_interactive_script_run", name = "load_data")
tool("tool__module_interactive_script_run", name = "run_analysis")
tool("tool__module_interactive_script_run", name = "apply_notch_filter")

# tool("skill_run__rave-module", file_name = "run.R", args = c("power_explorer", "--targets=repository"))
# tool("skill_run__rave-module", file_name = "get_results.R", args = c("power_explorer", "--target=repository"))
# tool("skill_run__rave-module", file_name = "get_results.R", args = c("power_explorer", "--target=omnibus_results"))

# tool("tool__shiny_input_info")
# tool("tool__shiny_output_info")

# tool("tool__shiny_input_update", inputId = "live_title", value = "Set from R")
# Sys.sleep(1)
# tool("tool__shiny_input_info", inputIds = list("live_title"))

# # one call: the reply is the element's HTML (or an image)
# res <- tool("tool__shiny_query_ui", css_selector = "body", transform_image = FALSE)
# mcp("tools/call", list(name = "tool__shiny_query_ui", arguments = list(css_selector = "body", transform_image = FALSE)), 
#         url = mcp_url)

# # trigger_refresh is listed once per module when modules define it differently
# for (name in grep("trigger_refresh", tool_names, value = TRUE)) tool(name)

# tool("skill_load__greet", action = "readme")
# tool("skill_run__greet", file_name = "greet.R", args = list("R"))

# # ---- choosing the module ----------------------------------------------------------

# tool("tool__hello_world", `_module` = module)             # "(requested)"
# tool("tool__hello_world",                                 # module fixed by the URL
#      .url = sprintf("%s/%s", mcp_url, module))
# tool("tool__hello_world", `_module` = "nope")             # refused
# tool("tool__nope")                                        # unknown tool
