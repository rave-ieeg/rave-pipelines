library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  # load & combine pipelines
  targets <- ravepipeline::load_targets(
    "make-notch_filter.R"
  )

  # reorder for debug use, or simply return `targets`
  targets

})

...targets
