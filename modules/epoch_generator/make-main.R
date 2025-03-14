library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  # load & combine pipelines
  targets <- ravepipeline::load_targets(
    "make-epoch_generator.R"
  )

  # reorder for debug use, or simply return `targets`
  targets

})

...targets
