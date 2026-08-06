# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)


# test_dir("tests/testthat", package = NULL, load_package = "none", )
# usethis::use_test("modules/reference_module/")
test_file("tests/testthat/test-import_lfp_native.R")
test_file("tests/testthat/test-notch_filter.R")
test_file("tests/testthat/test-wavelet.R")
test_file("tests/testthat/test-reference.R")
test_file("tests/testthat/test-voltage_explorer-viewer-table.R")
test_file("tests/testthat/test-voltage_explorer-crp-filter.R")
