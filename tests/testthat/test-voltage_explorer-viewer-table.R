require(testthat)

# Unit tests for the pure helpers behind the voltage_explorer "Results Table"
# tab. No subject data or pipeline run required.
source(testthat::test_path(
  "..", "..", "modules", "voltage_explorer", "R", "shared-viewer_table.R"
), local = TRUE)

# Stand-in for `CRP_VIEWER_METRICS` (defined in `shared-common.R`, which needs a
# live pipeline to source). Passed explicitly so the tests do not depend on the
# real registry's contents.
METRICS <- list(
  al_p     = "mean amplitude, uV",
  expl_var = "R2, variance explained",
  SNR      = "canonical vs. residual signal-to-noise",
  t_proj   = "t-statistic on trial projections"
)

test_that("crp_viewer_table_layout groups metric columns by condition", {

  layout <- crp_viewer_table_layout(c(
    "Electrode",
    "al_p (A)", "al_p (AV)", "al_p (V)",
    "SNR (A)", "SNR (AV)", "SNR (V)"
  ), metrics = METRICS)

  expect_equal(layout$conditions, c("A", "AV", "V"))
  expect_equal(names(layout$groups), c("al_p", "SNR"))
  expect_equal(layout$groups$al_p$columns, c("al_p (A)", "al_p (AV)", "al_p (V)"))
  expect_equal(layout$groups$al_p$description, "mean amplitude, uV")

  # metric-major order: the whole `al_p` block, then the whole `SNR` block
  expect_equal(layout$columns, c(
    "al_p (A)", "al_p (AV)", "al_p (V)",
    "SNR (A)", "SNR (AV)", "SNR (V)"
  ))
})

test_that("crp_viewer_table_layout orders groups by the registry, not the input", {

  # `data.table::dcast()` would hand these over alphabetically
  layout <- crp_viewer_table_layout(c(
    "Electrode", "SNR (A)", "al_p (A)", "expl_var (A)", "t_proj (A)"
  ), metrics = METRICS)

  expect_equal(names(layout$groups), c("al_p", "expl_var", "SNR", "t_proj"))
  expect_equal(
    layout$columns,
    c("al_p (A)", "expl_var (A)", "SNR (A)", "t_proj (A)")
  )
})

test_that("crp_viewer_table_layout keeps only registry metrics", {

  # identifier columns and unlisted metrics both drop out, no exclusion list
  layout <- crp_viewer_table_layout(c(
    "Electrode", "Subject", "crp_filter", "Hemisphere",
    "coef (A)", "al_p (A)"
  ), metrics = METRICS)

  expect_equal(names(layout$groups), "al_p")
  expect_equal(layout$columns, "al_p (A)")
})

test_that("crp_viewer_table_layout omits registry metrics with no columns", {

  # `onset` is dropped upstream when CRP onset detection is disabled; it must
  # not come back as an all-NA group
  layout <- crp_viewer_table_layout(
    c("Electrode", "al_p (A)"), metrics = METRICS)

  expect_equal(names(layout$groups), "al_p")
  expect_false("t_proj" %in% names(layout$groups))
})

test_that("crp_viewer_table_layout handles a single condition", {

  layout <- crp_viewer_table_layout(
    c("Electrode", "al_p (A)", "t_proj (A)"), metrics = METRICS)

  expect_equal(layout$conditions, "A")
  expect_equal(names(layout$groups), c("al_p", "t_proj"))
  expect_equal(layout$columns, c("al_p (A)", "t_proj (A)"))
})

test_that("crp_viewer_table_layout keeps groups rectangular when a metric is partial", {

  # a metric with no finite value for one condition is dropped upstream for that
  # condition only, so its group must still span every condition
  layout <- crp_viewer_table_layout(c(
    "Electrode", "al_p (A)", "al_p (V)", "t_proj (A)"
  ), metrics = METRICS)

  expect_equal(layout$conditions, c("A", "V"))
  expect_equal(layout$groups$t_proj$columns, c("t_proj (A)", "t_proj (V)"))
  expect_equal(
    layout$columns,
    c("al_p (A)", "al_p (V)", "t_proj (A)", "t_proj (V)")
  )
})

test_that("crp_viewer_table_layout tolerates spaces in metric and condition names", {

  layout <- crp_viewer_table_layout(
    c("Electrode", "mean amp (A V)"),
    metrics = list(`mean amp` = "test metric")
  )

  expect_equal(names(layout$groups), "mean amp")
  expect_equal(layout$conditions, "A V")
})

test_that("crp_viewer_table_layout returns empty groups when nothing matches", {

  layout <- crp_viewer_table_layout(c("Electrode", "Subject"), metrics = METRICS)

  expect_length(layout$groups, 0L)
  expect_length(layout$columns, 0L)
  expect_length(layout$conditions, 0L)
})

test_that("crp_viewer_table_container spans leading columns and metric groups", {

  layout <- crp_viewer_table_layout(c(
    "Electrode",
    "al_p (A)", "al_p (AV)", "al_p (V)",
    "SNR (A)", "SNR (AV)", "SNR (V)"
  ), metrics = METRICS)
  leading_names <- c("Electrode", "Label", "Filtered")

  html <- as.character(crp_viewer_table_container(layout, leading_names))
  count <- function(pattern) {
    lengths(regmatches(html, gregexpr(pattern, html)))[[1]]
  }

  # leading identifiers occupy both header rows
  expect_equal(count('rowspan="2"'), 3L)
  # each metric spans its three conditions
  expect_equal(count('colspan="3"'), 2L)

  expect_true(grepl(">al_p<", html, fixed = TRUE))
  expect_true(grepl(">AV<", html, fixed = TRUE))

  # the sketch must have one leaf cell per data column
  leaf_count <- length(leading_names) + length(layout$columns)
  expect_equal(count("<th[ >]"), leaf_count + length(layout$groups))
})

test_that("crp_viewer_table_container marks one group start per row per group", {

  layout <- crp_viewer_table_layout(c(
    "Electrode", "al_p (A)", "al_p (AV)", "SNR (A)", "SNR (AV)"
  ), metrics = METRICS)

  html <- as.character(
    crp_viewer_table_container(layout, c("Electrode", "Label", "Filtered")))

  # 2 groups x 2 header rows
  expect_equal(
    lengths(regmatches(html, gregexpr("crp-group-start", html)))[[1]],
    4L
  )
})

test_that("crp_viewer_table_container prints the metric description", {

  layout <- crp_viewer_table_layout(c("Electrode", "al_p (A)"), metrics = METRICS)
  html <- as.character(crp_viewer_table_container(layout, "Electrode"))

  expect_true(grepl("crp-metric-desc", html, fixed = TRUE))
  expect_true(grepl("mean amplitude, uV", html, fixed = TRUE))
})

test_that("crp_viewer_table_container omits an empty description", {

  layout <- crp_viewer_table_layout(
    c("Electrode", "foo (A)"), metrics = list(foo = ""))
  html <- as.character(crp_viewer_table_container(layout, "Electrode"))

  expect_false(grepl("crp-metric-desc", html, fixed = TRUE))
  expect_true(grepl("crp-metric-name", html, fixed = TRUE))
})

test_that("crp_viewer_table_digits switches at a magnitude of 10", {

  expect_equal(crp_viewer_table_digits(c(0.1, 3.4, 9.99)), 2L)
  expect_equal(crp_viewer_table_digits(c(0.1, 3.4, 10)), 1L)
  expect_equal(crp_viewer_table_digits(c(0.1, 3.4, 12.7)), 1L)

  # magnitude, not sign: a large negative counts
  expect_equal(crp_viewer_table_digits(c(-14.2, 0.3)), 1L)
})

test_that("crp_viewer_table_digits falls back to 2 without usable values", {

  expect_equal(crp_viewer_table_digits(numeric(0)), 2L)
  expect_equal(crp_viewer_table_digits(c(NA_real_, NA_real_)), 2L)
  expect_equal(crp_viewer_table_digits(c(NA_real_, Inf)), 2L)
})
