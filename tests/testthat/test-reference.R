require(testthat)
test_that("reference", {

  pipeline <- ravepipeline::pipeline(
    pipeline_name = "reference_module",
    paths = file.path(rstudioapi::getActiveProject(), "modules"))

  # pipeline path is correct
  expect_equivalent(
    pipeline$pipeline_path,
    normalizePath(file.path(rstudioapi::getActiveProject(), "modules", "reference_module"))
  )

  subject1 <- raveio::as_rave_subject("test1/KC", strict = FALSE)
  subject2 <- raveio::as_rave_subject("test2/KC", strict = FALSE)

  # check if reference data are the same
  ref_path1 <- file.path(subject1$reference_path, "ref_6-7,14-15,22-23,30-31,38-39.h5")
  ref_path2 <- file.path(subject2$reference_path, "ref_6-7,14-15,22-23,30-31,38-39.h5")

  expect_true(file.exists(ref_path1))
  skip_if_not(file.exists(ref_path1))

  if(!file.exists(ref_path2)) {
    # generate reference data
    raveio::generate_reference(subject = subject2$subject_id, electrodes = subject2$electrodes)
  }

  blocks <- subject1$blocks

  # check data
  for(block in blocks) {
    d1 <- raveio::load_h5(ref_path1, name = sprintf("voltage/%s", block))
    d2 <- raveio::load_h5(ref_path2, name = sprintf("voltage/%s", block))
    expect_equal(d1[], d2[], label = sprintf("voltage data in reference: block [%s]", block))

    d1 <- raveio::load_h5(ref_path1, name = sprintf("wavelet/coef/%s", block))
    d2 <- raveio::load_h5(ref_path2, name = sprintf("wavelet/coef/%s", block))
    expect_equal(d1[], d2[],
                 label = sprintf("wavelet data in reference: block [%s]", block),
                 tolerance = 1e-5)

  }

  # check referenced signal
  repo1 <- rave::rave_prepare(subject1$subject_id, 14, "KCaOutlier", c(1, 2), attach = FALSE)
  repo2 <- raveio::prepare_subject_power(subject2, 14, 'default', 'KCaOutlier', c(-1,2))

  p1 <- repo1$module_tools$get_power(referenced = TRUE)
  p2 <- repo2$power$LFP$data_list$e_14

  d1 <- p1$get_data()
  d2 <- aperm(p2[drop = FALSE], c(3,1,2,4))
  expect_equal(d1[], d2[],
               label = sprintf("power data after reference: block [%s]", block),
               tolerance = 1e-5)

})
