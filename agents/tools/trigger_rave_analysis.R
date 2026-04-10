# agents/tools/hello_world.R
#
# Root-level MCP tool: Returns a greeting.
# Used to verify the MCP tunnel works end-to-end.
#
# Enable per module in agents/agent.yaml:
#   tools:
#     root:
#       hello_world: true

trigger_rave_analysis <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function() {
        ravedash::fire_rave_event(
          key = 'run_analysis',
          value = Sys.time(),
          session = session
        )
        return("`Run-analysis` Signal sent. RAVE will start the pipeline and visualize the results soon, typically requires anywhere between a second to a minute.")
      },
      name = "trigger_rave_analysis",
      description = "Simulate UI button to trigger run-analysis: this core tool will drive RAVE to save user's UI input into pipeline, execute, and display the analyses & visualizations in RAVE dashboard.",
      arguments = list()
    )
  }
)
