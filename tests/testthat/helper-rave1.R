# Helpers for the tests that compare RAVE 2.0 pipelines with RAVE 1.0 results:
# test-import_lfp_native.R, test-notch_filter.R, test-wavelet.R, and
# test-reference.R. RAVE 1.0 builds the subject to compare with (test1/KC)
# from the raw data of demo subject KC. `rave` 1.99.0 and newer is a RAVE 2.0
# wrapper without RAVE 1.0's functions, so these tests skip there.

skip_without_rave1 <- function() {
  has_rave1 <- requireNamespace("rave", quietly = TRUE) &&
    all(c("rave_preprocess_tools", "rave_prepare") %in% getNamespaceExports("rave"))
  testthat::skip_if_not(has_rave1, paste(
    "needs RAVE 1.0's `rave::rave_preprocess_tools()` and `rave::rave_prepare()`",
    "(`rave` 1.99.0 and newer does not have them)"
  ))
}

skip_without_kc_raw <- function() {
  kc_raw <- file.path(ravepipeline::raveio_getopt("raw_data_dir"), "KC")
  testthat::skip_if_not(
    dir.exists(kc_raw),
    sprintf("needs the raw data of demo subject KC in %s", kc_raw)
  )
}
