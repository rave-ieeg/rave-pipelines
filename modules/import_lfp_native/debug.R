##' Let RAVE switch to this module pipeline
##'
Sys.setenv("RAVE_PIPELINE" = "~/Dropbox/projects/rave-pipelines/import-LFP")

##' Debug pipeline: assign variables to global environment
##' so that you can print them directly
##' setting `quick = FALSE` allows you to run the whole pipeline
##' including its parent pipelines. This is useful is the current
##' pipeline is a combination of multiple sub-pipelines
##'
ravepipeline::pipeline_debug(quick = FALSE)
# ravepipeline::pipeline_debug()

##' Visualize pipeline and the relationship between intermediate variables
##' This requires extra package `visNetwork`.
##' Please run `install.packages('visNetwork')` if missing packages
##'
ravepipeline::pipeline_visualize()

##' Test run the pipeline in production mode
##'
# ravepipeline::pipeline_run(type = "async")
ravepipeline::pipeline_run()

##' Check pipeline progress to see status (cached/skipped, built, errored...)
##' Must run `ravepipeline::pipeline_run()` first, otherwise error will occur
##'
ravepipeline::pipeline_progress(method = 'details')


##' Get current variable table. Run `ravepipeline::pipeline_run()` first
##'
ravepipeline::pipeline_vartable()

##' Get intermediate variables
##'
ravepipeline::pipeline_hasname("project_name")
ravepipeline::pipeline_read("project_name")


##' Launch a shiny app to watch the pipeline. Please install extra
##' packages first:
##' `install.packages(c('bs4Dash', 'gt', 'pingr', 'shinybusy'))`
##'
ravepipeline::pipeline_watch()

dipsaus::rs_add_shortcut(1, {
  Sys.setenv("RAVE_PIPELINE" = "/Users/dipterix/Dropbox/projects/rave-pipelines/import-LFP")
  ravepipeline::pipeline_debug(quick = FALSE, env = .GlobalEnv)
})
dipsaus::rs_add_shortcut(2, {
  ravepipeline::pipeline_run()
}, force = FALSE)
