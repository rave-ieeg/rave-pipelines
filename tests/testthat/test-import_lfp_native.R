require(testthat)
test_that("import native formats", {

  pipeline <- ravepipeline::pipeline("import_lfp_native", paths = file.path(rstudioapi::getActiveProject(),
                                                                      "modules"))

  # pipeline path is correct
  expect_equivalent(
    pipeline$pipeline_path,
    normalizePath(file.path(rstudioapi::getActiveProject(), "modules", "import_lfp_native"))
  )

  blocks <- c("003", "004")
  electrodes <- dipsaus::parse_svec('6-7,14-15,22-23,30-31,38-39')

  # RAVE 1.0 import
  tools <- rave::rave_preprocess_tools()
  utils::capture.output({
    junk <- utils::capture.output({
      try({
        tools$create_subject("KC", "test1")
        tools$check_load_subject("KC", "test1")
        tools$set_blocks(blocks)
        tools$set_electrodes("6-7,14-15,22-23,30-31,38-39")
        tools$set_sample_rate(2000L)
        tools$save_to_subject()
      }, silent = TRUE)
      tools$check_load_subject("KC", "test1")
      invisible()
    })
  }, type = "message")


  if(!tools$has_raw_cache()) {
    tools$collect_raw_voltage(force = TRUE)
  }
  subject1 <- raveio::as_rave_subject("test1/KC", strict = FALSE)

  # RAVE 2.0 import
  pipeline$set_settings(
    skip_validation = TRUE,
    import_channels__sample_rate = 2000L,
    import_setup__project_name = "test2",
    import_setup__subject_code = "KC",
    force_import = TRUE,
    import_channels__electrodes = "6-7,14-15,22-23,30-31,38-39",
    import_channels__unit = "NA",
    import_channels__electrode_file = "auto",
    import_blocks__format = ".mat/.h5 file per electrode per block",
    import_blocks__session_block = blocks
  )
  pipeline$run('subject')
  subject2 <- raveio::as_rave_subject("test2/KC", strict = FALSE)
  expect_length(subject2$preprocess_settings$data_imported, length(electrodes))

  future::plan('sequential')

  if(getOption("rave.test.fastforward", FALSE)) {
    skip_if(all(subject2$preprocess_settings$data_imported))
  }


  if(!all(subject2$preprocess_settings$data_imported)) {
    pipeline$run()
  }


  subject2 <- raveio::as_rave_subject("test2/KC", strict = FALSE)

  v1 <- raveio::validate_subject(subject = "test1/KC", version = 1, verbose = FALSE)
  v2 <- raveio::validate_subject(subject = "test2/KC", version = 2, verbose = FALSE)

  expect_true(v1$voltage_data$voltage_preprocessing$valid)
  expect_true(v2$voltage_data$voltage_preprocessing$valid)
  for(nm in c("reference_path", "preprocess_path", "meta_path", "rave_path",
              "data_path", "path", "raw_path")) {
    expect_true(v1$paths[[nm]]$valid, label = sprintf("%s exists", nm))
    expect_true(v2$paths[[nm]]$valid, label = sprintf("%s exists", nm))
  }

  # compare data
  for(electrode in electrodes) {

    path1 <- file.path(subject1$preprocess_path, "voltage", sprintf("electrode_%d.h5", electrode))
    path2 <- file.path(subject2$preprocess_path, "voltage", sprintf("electrode_%d.h5", electrode))
    raveio::h5_names(path2)
    for(block in blocks) {
      d1 <- raveio::load_h5(path1, name = sprintf("raw/%s", block))
      d2 <- raveio::load_h5(path2, name = sprintf("raw/%s", block))
      expect_equal(d1[], d2[], label = sprintf("raw voltage: electrode [%d] block [%s]",
                                               electrode, block))
    }

  }


})
