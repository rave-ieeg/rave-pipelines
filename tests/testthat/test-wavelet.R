require(testthat)

# please run the following tests first
#  - tests/test-import_lfp_native.R

test_that("Wavelet", {

  pipeline <- ravepipeline::pipeline(
    pipeline_name = "wavelet_module",
    paths = file.path(rstudioapi::getActiveProject(), "modules")
  )

  # pipeline path is correct
  expect_equivalent(
    pipeline$pipeline_path,
    normalizePath(file.path(rstudioapi::getActiveProject(), "modules", "wavelet_module"))
  )

  subject2 <- raveio::as_rave_subject("test2/KC", strict = FALSE)
  # skip_if(all(subject2$preprocess_settings$notch_filtered))
  if(getOption("rave.test.fastforward", FALSE)) {
    skip_if(all(subject2$preprocess_settings$has_wavelet))
  }

  frequencies <- seq.int(2L, 192L, 10L)
  cycles <- c(2, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 16, 17, 17, 18,
              18, 19, 19, 20)
  electrodes <- subject2$electrodes


  tools <- rave::rave_preprocess_tools()
  utils::capture.output({
    tools$load_subject("KC", "test1")
    if(!tools$waveleted()) {
      tools$apply_wavelet(
        electrodes = electrodes,
        target_srate = 100,
        frequencies = frequencies,
        wave_num = cycles,
        ncores = 1L
      )
    }
  }, type = "message")

  future::plan('sequential')

  pipeline$set_settings(
    project_name = "test2",
    subject_code = "KC",
    kernel_table = list(
      Frequency = frequencies,
      Cycles = cycles),
    precision = "double",
    pre_downsample = 1,
    target_sample_rate = 100
  )

  # apply notch filters
  if(!all(subject2$preprocess_settings$has_wavelet)) {
    pipeline$run(names = 'clear_cache')
    pipeline$run(names = 'wavelet_params')
    pipeline$run(names = 'clear_cache')
  }

  subject1 <- raveio::as_rave_subject("test1/KC", strict = FALSE)
  subject2 <- raveio::as_rave_subject("test2/KC", strict = FALSE)

  expect_equal(
    subject1$preprocess_settings$has_wavelet,
    subject2$preprocess_settings$has_wavelet
  )
  expect_equivalent(
    subject1$preprocess_settings$wavelet_params[c("downsample_to", "frequencies")],
    subject2$preprocess_settings$wavelet_params[c("downsample_to", "frequencies")]
  )

  # calculate cycles and compare
  actual_cycles <- subject2$preprocess_settings$wavelet_params$cycle
  if(length(actual_cycles) > 2) {
    srate <- 2000
    ratio <- (log(max(cycles)) - log(min(cycles))) / (log(max(frequencies)) - log(min(frequencies)))
    wavelet_cycles <- exp((log(frequencies) - log(min(frequencies))) * ratio + log(min(cycles)))
    wavelet_cycles <- round(wavelet_cycles)
    expect_equivalent(actual_cycles, wavelet_cycles)
  }



  # now check the data
  electrodes <- subject1$electrodes
  blocks <- subject1$blocks
  for(electrode in electrodes) {

    path1 <- file.path(subject1$data_path, "power", sprintf("%d.h5", electrode))
    path2 <- file.path(subject2$data_path, "power", sprintf("%d.h5", electrode))
    raveio::h5_names(path2)
    for(block in blocks) {
      d1 <- raveio::load_h5(path1, name = sprintf("raw/power/%s", block))
      d2 <- raveio::load_h5(path2, name = sprintf("raw/power/%s", block))
      expect_equal(d1[], d2[], label = sprintf("wavelet power: electrode [%d] block [%s]",
                                               electrode, block))
    }

    path1 <- file.path(subject1$data_path, "phase", sprintf("%d.h5", electrode))
    path2 <- file.path(subject2$data_path, "phase", sprintf("%d.h5", electrode))
    for(block in blocks) {
      d1 <- raveio::load_h5(path1, name = sprintf("raw/phase/%s", block))
      d2 <- raveio::load_h5(path2, name = sprintf("raw/phase/%s", block))
      expect_equal(d1[], d2[], label = sprintf("wavelet phase: electrode [%d] block [%s]",
                                               electrode, block))
    }
  }


})


test_that("Wavelet quick (float+downsample)", {

  # use test3/KC

  # import data
  subject2 <- raveio::as_rave_subject('test3/KC', strict = FALSE)
  if(!length(subject2$electrodes) || !all(subject2$data_imported)) {

    pipeline <- ravepipeline::pipeline(pipeline_name = "import_lfp_native",
                                 paths = file.path(rstudioapi::getActiveProject(), "modules"))
    pipeline$set_settings(
      skip_validation = TRUE,
      import_channels__sample_rate = 2000L,
      import_setup__project_name = "test3",
      import_setup__subject_code = "KC",
      force_import = TRUE,
      import_channels__electrodes = "6-7,14-15,22-23,30-31,38-39",
      import_channels__unit = "NA",
      import_channels__electrode_file = "auto",
      import_blocks__format = ".mat/.h5 file per electrode per block",
      import_blocks__session_block = blocks
    )
    pipeline$run()

  }

  # notch
  subject2 <- raveio::as_rave_subject('test3/KC', strict = FALSE)
  if(!all(subject2$notch_filtered)) {
    pipeline <- ravepipeline::pipeline(
      pipeline_name = "notch_filter",
      paths = file.path(rstudioapi::getActiveProject(), "modules"))
    pipeline$set_settings(
      subject_code = "KC",
      project_name = "test3",
      notch_filter_upperbound = c(61, 122, 182),
      notch_filter_lowerbound = c(59, 118, 178)
    )

    # apply notch filters
    pipeline$run(names = 'apply_notch')
  }

  # wavelet
  subject2 <- raveio::as_rave_subject('test3/KC', strict = FALSE)
  pipeline <- ravepipeline::pipeline(
    pipeline_name = "wavelet_module",
    paths = file.path(rstudioapi::getActiveProject(), "modules")
  )
  subject1 <- raveio::as_rave_subject("test1/KC", strict = FALSE)
  frequencies <- subject1$preprocess_settings$wavelet_params$frequencies
  cycles <- subject1$preprocess_settings$wavelet_params$cycle
  if(!all(subject2$has_wavelet)) {
    # now try float precision with downsample = 4
    pipeline$set_settings(
      project_name = "test3",
      subject_code = "KC",
      kernel_table = list(
        Frequency = frequencies,
        Cycles = cycles),
      precision = "float",
      pre_downsample = 1,
      target_sample_rate = 100
    )
    pipeline$run(names = 'clear_cache')
    pipeline$run(names = 'wavelet_params')
    pipeline$run(names = 'clear_cache')
  }

  subject1 <- raveio::as_rave_subject("test1/KC", strict = FALSE)
  subject2 <- raveio::as_rave_subject("test3/KC", strict = FALSE)

  expect_equal(
    subject2$preprocess_settings$wavelet_params$precision,
    "float"
  )

  expect_equivalent(
    subject2$preprocess_settings$wavelet_params$pre_downsample,
    1
  )

  expect_equal(
    subject1$preprocess_settings$has_wavelet,
    subject2$preprocess_settings$has_wavelet
  )
  expect_equivalent(
    subject1$preprocess_settings$wavelet_params[c("downsample_to", "frequencies", "cycle")],
    subject2$preprocess_settings$wavelet_params[c("downsample_to", "frequencies", "cycle")]
  )


  # now check the data
  electrodes <- subject1$electrodes
  blocks <- subject1$blocks
  for(electrode in electrodes) {

    path1 <- file.path(subject1$data_path, "power", sprintf("%d.h5", electrode))
    path2 <- file.path(subject2$data_path, "power", sprintf("%d.h5", electrode))
    raveio::h5_names(path2)
    for(block in blocks) {
      d1 <- raveio::load_h5(path1, name = sprintf("raw/power/%s", block))
      d2 <- raveio::load_h5(path2, name = sprintf("raw/power/%s", block))

      # normalize signals
      s1 <- d1[]
      s2 <- d2[]

      expect_equal(dim(s1), dim(s2))
      diff <- log10(s2 / s1)

      expect_lt(max(abs(range(diff))), 1e-4)
    }

    path1 <- file.path(subject1$data_path, "phase", sprintf("%d.h5", electrode))
    path2 <- file.path(subject2$data_path, "phase", sprintf("%d.h5", electrode))
    for(block in blocks) {
      d1 <- raveio::load_h5(path1, name = sprintf("raw/phase/%s", block))
      d2 <- raveio::load_h5(path2, name = sprintf("raw/phase/%s", block))
      # normalize signals
      s1 <- d1[]
      s2 <- d2[]

      expect_equal(dim(s1), dim(s2))
      diff <- s2 - s1

      expect_lt(max(abs(range(diff))), 1e-4)
    }
  }


})
