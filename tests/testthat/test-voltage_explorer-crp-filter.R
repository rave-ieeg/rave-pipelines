require(testthat)

# Unit tests for the pure helpers behind the voltage_explorer "Channel filter".
# No subject data or pipeline run required.
source(testthat::test_path(
  "..", "..", "modules", "voltage_explorer", "R", "shared-viewer_table.R"
), local = TRUE)
source(testthat::test_path(
  "..", "..", "modules", "voltage_explorer", "R", "shared-crp_filter_electrodes.R"
), local = TRUE)

# Stand-in for `CRP_VIEWER_METRICS` (defined in `shared-common.R`, which needs a
# live pipeline to source). Passed explicitly so the tests do not depend on the
# real registry's contents.
METRICS <- list(
  al_p     = "mean amplitude, uV",
  expl_var = "R2, variance explained",
  SNR      = "canonical vs. residual signal-to-noise",
  t_proj   = "t-statistic on trial projections",
  onset    = "estimated response onset, s"
)

# Two conditions, five electrodes. `al_p` separates them cleanly:
#   e1 passes in neither, e2 in A only, e3 in AV only, e4 in both,
#   e5 has no value in A at all.
TBL <- data.frame(
  Electrode      = c(1, 2, 3, 4, 5),
  `al_p (A)`     = c(10, 300, 10, 300, NA),
  `al_p (AV)`    = c(10, 10, 300, 300, 300),
  `t_proj (A)`   = c(1, 2, 3, 4, 5),
  `t_proj (AV)`  = c(5, 4, 3, 2, 1),
  crp_filter     = rep("true", 5),
  check.names    = FALSE,
  stringsAsFactors = FALSE
)

filter_row <- function(name, threshold, criteria = "abs_gte", operator = "and") {
  list(name = name, criteria = criteria, threshold = threshold,
       operator = operator)
}


test_that("ALL requires every condition, ANY requires one", {

  expect_equal(
    crp_filter_electrodes(TBL, list(filter_row("all:al_p", "200"))),
    4
  )
  expect_equal(
    crp_filter_electrodes(TBL, list(filter_row("any:al_p", "200"))),
    c(2, 3, 4, 5)
  )
})

test_that("an aggregate matches the per-column chain it replaces", {

  # ANY == the two columns joined by OR
  expect_equal(
    crp_filter_electrodes(TBL, list(filter_row("any:al_p", "200"))),
    crp_filter_electrodes(TBL, list(
      filter_row("al_p (A)", "200"),
      filter_row("al_p (AV)", "200", operator = "or")
    ))
  )

  # ALL == the two columns joined by AND
  expect_equal(
    crp_filter_electrodes(TBL, list(filter_row("all:al_p", "200"))),
    crp_filter_electrodes(TBL, list(
      filter_row("al_p (A)", "200"),
      filter_row("al_p (AV)", "200", operator = "and")
    ))
  )
})

test_that("a missing value fails its own condition only", {

  # e5 is NA in A: rejected by ALL, admitted by ANY on the strength of AV
  expect_false(5 %in% crp_filter_electrodes(TBL, list(filter_row("all:al_p", "200"))))
  expect_true(5 %in% crp_filter_electrodes(TBL, list(filter_row("any:al_p", "200"))))
})

test_that("single-column rows are unchanged", {

  expect_equal(
    crp_filter_electrodes(TBL, list(filter_row("al_p (A)", "200"))),
    c(2, 4)
  )
  # `in` needs two bounds and reads them in either order
  expect_equal(
    crp_filter_electrodes(TBL, list(
      filter_row("t_proj (A)", "4, 2", criteria = "in")
    )),
    c(2, 3, 4)
  )
})

test_that("aggregates chain with the row operator like any other component", {

  # (ALL al_p >= 200) OR (t_proj (A) >= 5)  ->  e4 from the first, e5 from the second
  expect_equal(
    crp_filter_electrodes(TBL, list(
      filter_row("all:al_p", "200"),
      filter_row("t_proj (A)", "5", criteria = "gte", operator = "or")
    )),
    c(4, 5)
  )
})

test_that("unusable components are skipped, not errors", {

  # unknown metric, blank name, aggregate over a metric with no column, and a
  # two-bound criteria given one bound -- all yield "no active filter"
  expect_null(crp_filter_electrodes(TBL, list(filter_row("all:nope", "200"))))
  expect_null(crp_filter_electrodes(TBL, list(filter_row("", "200"))))
  expect_null(crp_filter_electrodes(TBL, list(filter_row("any:onset", "0.1"))))
  expect_null(crp_filter_electrodes(TBL, list(
    filter_row("all:al_p", "200", criteria = "in")
  )))
  expect_null(crp_filter_electrodes(TBL, list(filter_row("all:al_p", ""))))

  # a skipped component leaves the others intact
  expect_equal(
    crp_filter_electrodes(TBL, list(
      filter_row("all:nope", "200"),
      filter_row("all:al_p", "200")
    )),
    4
  )
})


test_that("crp_filter_choices puts the aggregates first", {

  choices <- crp_filter_choices(names(TBL), metrics = METRICS)

  # every "all:", then every "any:", then the individual columns
  expect_equal(choices, c(
    "all:t_proj", "all:al_p", "any:t_proj", "any:al_p",
    "t_proj (A)", "t_proj (AV)", "al_p (A)", "al_p (AV)"
  ))

  # the dropdown shows the values themselves, no separate labels
  expect_null(names(choices))

  # non-metric columns are not filterable
  expect_false(any(c("Electrode", "crp_filter") %in% choices))
})

test_that("crp_filter_choices omits aggregates for a single condition", {

  choices <- crp_filter_choices(
    c("Electrode", "al_p (All Conditions)", "t_proj (All Conditions)"),
    metrics = METRICS
  )

  expect_equal(unname(choices),
               c("t_proj (All Conditions)", "al_p (All Conditions)"))
})

test_that("crp_filter_choices pushes onset to the end", {

  choices <- crp_filter_choices(c(
    "Electrode", "onset (A)", "onset (AV)", "al_p (A)", "al_p (AV)"
  ), metrics = METRICS)

  expect_equal(unname(choices), c(
    "all:al_p", "all:onset", "any:al_p", "any:onset",
    "al_p (A)", "al_p (AV)", "onset (A)", "onset (AV)"
  ))
})
