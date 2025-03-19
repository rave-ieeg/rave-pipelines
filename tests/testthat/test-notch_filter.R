require(testthat)

# please run "tests/test-import_lfp_native.R" first

test_that("Notch filter", {

  pipeline <- ravepipeline::pipeline("notch_filter", paths = file.path(rstudioapi::getActiveProject(),
                                                                      "modules"))

  # pipeline path is correct
  expect_equivalent(
    pipeline$pipeline_path,
    normalizePath(file.path(rstudioapi::getActiveProject(), "modules", "notch_filter"))
  )

  subject2 <- raveio::as_rave_subject("test2/KC", strict = FALSE)
  # skip_if(all(subject2$preprocess_settings$notch_filtered))
  if(getOption("rave.test.fastforward", FALSE)) {
    skip_if(all(subject2$preprocess_settings$notch_filtered))
  }

  tools <- rave::rave_preprocess_tools()
  utils::capture.output({
    tools$load_subject("KC", "test1")
    if(!tools$notch_filtered()) {
      tools$apply_notch(
        bandwidths = list(
          'default' = list(
            centers = c(60,120,180),
            widths = c(1,2,2)
          )
        )
      )
    }

  }, type = "message")

  future::plan('sequential')

  pipeline$set_settings(
    subject_code = "KC",
    project_name = "test2",
    notch_filter_upperbound = c(61, 122, 182),
    notch_filter_lowerbound = c(59, 118, 178)
  )

  # apply notch filters
  if(!all(subject2$preprocess_settings$notch_filtered)) {
    pipeline$run(names = 'apply_notch')
  }

  subject1 <- raveio::as_rave_subject("test1/KC", strict = FALSE)
  subject2 <- raveio::as_rave_subject("test2/KC", strict = FALSE)

  expect_equal(
    subject1$preprocess_settings$notch_filtered,
    subject2$preprocess_settings$notch_filtered
  )
  expect_equivalent(
    subject1$preprocess_settings$notch_params,
    subject2$preprocess_settings$notch_params
  )

  # now check the data
  electrodes <- subject1$electrodes
  blocks <- subject1$blocks
  for(electrode in electrodes) {

    path1 <- file.path(subject1$preprocess_path, "voltage", sprintf("electrode_%d.h5", electrode))
    path2 <- file.path(subject2$preprocess_path, "voltage", sprintf("electrode_%d.h5", electrode))
    for(block in blocks) {
      d1 <- raveio::load_h5(path1, name = sprintf("notch/%s", block))
      d2 <- raveio::load_h5(path2, name = sprintf("notch/%s", block))
      expect_equal(d1[], d2[], label = sprintf("voltage data after notch filters: electrode [%d] block [%s]",
                                               electrode, block))
    }

  }

})
