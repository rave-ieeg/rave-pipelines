# agents/tools/trigger_load_data.R
#
# Root-level MCP tool: load data

trigger_load_data <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function() {
        server_tools <- ravedash::get_default_handlers(session = session)

        res <- tryCatch(
          {
            server_tools$trigger_script("load_data")
            "`Load-data` finished."
          },
          error = function(e) {
            ravepipeline::logger_error_condition(e)
            paste(c("`Load-data` triggered but errorred: ", e$message), collapse = "")
          }
        )
        
        return(res)
      },
      name = "trigger_load_data",
      description = "Simulate UI button to trigger load-data: this core tool will drive RAVE to load data from loading screen and prepare the analysis screen.",
      arguments = list()
    )
  }
)
