# agents/tools/trigger_analysis.R
#
# Root-level MCP tool: run analysis

trigger_analysis <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function() {
        # Same gating as the run-analysis button: the analysis saves the UI
        # inputs into the pipeline settings, which are not set up until
        # data are loaded
        not_ready <- shiny::isolate(
          ravedash::watch_loader_opened(session = session) ||
            !isTRUE(ravedash::watch_data_loaded(session = session))
        )
        if (not_ready) {
          return(paste(
            "`Run-analysis` not started: the data are not loaded (the data",
            "loader is open). Load the data first, e.g. with `trigger_load_data`."
          ))
        }

        server_tools <- ravedash::get_default_handlers(session = session)

        res <- tryCatch(
          {
            server_tools$trigger_script("run_analysis")
            "`Run-analysis` triggered and finished. It might take a while (~ 5sec) for outputs to finish rendering."
          },
          error = function(e) {
            ravepipeline::logger_error_condition(e)
            paste(c("`Run-analysis` triggered but errorred: ", e$message), collapse = "")
          }
        )
        
        return(res)
      },
      name = "trigger_analysis",
      description = "Simulate UI button to trigger run-analysis: this core tool will drive RAVE to save user's UI input into pipeline, execute, and display the analyses & visualizations in RAVE dashboard.",
      arguments = list()
    )
  }
)
