library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  # load & combine pipelines
  targets <- ravepipeline::load_targets(
    "make-voltage_clust.R"
  )

  # reorder for debug use, or simply return `targets`
  targets

})

...targets
