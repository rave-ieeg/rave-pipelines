# Helpers turning the wide 3D-viewer value table (`erp_results_for_viewer`, see
# `prepare_data_crp_3dviewer_value` in `shared-crp.R`) into a metric-major table
# with a two-row header, so the same metric can be compared across conditions
# side by side. The viewer table names its columns "<metric> (<condition>)",
# e.g. "al_p (A)", "al_p (AV)"; here they are regrouped under an "al_p" header
# spanning the "A"/"AV"/"V" sub-columns.
#
# `CRP_VIEWER_METRICS` (`shared-common.R`) drives everything: which metrics are
# shown, in what order, and the description printed under each metric name.
#
# All functions here are pure (no Shiny, no pipeline access) so they can be unit
# tested on synthetic column names.


# Parse viewer-table column names into a metric-major layout.
#
# `column_names`: character vector of the wide table's column names.
# `metrics`: named list mapping metric name -> description. Its names are the
#   metric whitelist and its order is the display order; columns whose metric is
#   not listed are ignored, which is how `Electrode`, `Subject` and `crp_filter`
#   drop out without an explicit exclusion list.
#
# Returns a list with
#   `conditions`: condition labels, in order of first appearance
#   `groups`    : one entry per metric that has at least one column, ordered by
#                 `metrics`, each `list(metric =, description =, conditions =,
#                 columns =)`. Every group spans the full condition set, so the
#                 header stays rectangular even when a metric is missing for
#                 some condition. The caller is responsible for creating those
#                 missing columns as NA.
#   `columns`   : all grouped column names in display order (metric-major)
crp_viewer_table_layout <- function(column_names, metrics = CRP_VIEWER_METRICS) {

  column_names <- as.character(column_names)

  parsed <- regmatches(
    column_names,
    regexec("^(.+) \\(([^()]*)\\)$", column_names)
  )
  matched <- lengths(parsed) == 3L

  found_metrics <- vapply(parsed[matched], `[[`, character(1L), 2L)
  found_conditions <- vapply(parsed[matched], `[[`, character(1L), 3L)

  known <- found_metrics %in% names(metrics)
  found_metrics <- found_metrics[known]
  found_conditions <- found_conditions[known]

  condition_levels <- unique(found_conditions)
  metric_levels <- intersect(names(metrics), found_metrics)

  groups <- lapply(metric_levels, function(metric) {
    list(
      metric = metric,
      description = as.character(metrics[[metric]]),
      conditions = condition_levels,
      columns = sprintf("%s (%s)", metric, condition_levels)
    )
  })
  names(groups) <- metric_levels

  list(
    conditions = condition_levels,
    groups = groups,
    columns = unlist(lapply(groups, `[[`, "columns"), use.names = FALSE)
  )
}


# Build the two-row `<thead>` sketch that `DT::datatable(container = )` needs:
# leading identifier columns span both rows, each metric spans its conditions
# and prints its description underneath. The first cell of every metric group
# carries `crp-group-start` in both rows, which is what the module CSS turns
# into the vertical rule between groups.
#
# `layout`: result of `crp_viewer_table_layout()`
# `leading_names`: identifier columns shown to the left of the metric groups
#
# The column count of the sketch must match the data frame handed to
# `DT::datatable()` with `rownames = FALSE`, i.e.
# `c(leading_names, layout$columns)`.
crp_viewer_table_container <- function(layout, leading_names) {

  leading_names <- as.character(leading_names)

  htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        lapply(leading_names, function(nm) {
          th(rowspan = 2L, class = "dt-center", nm)
        }),
        lapply(layout$groups, function(group) {
          th(
            colspan = length(group$columns),
            class = "dt-center crp-group-start",
            div(class = "crp-metric-name", group$metric),
            if (nzchar(group$description)) {
              div(class = "crp-metric-desc", group$description)
            }
          )
        })
      ),
      tr(
        lapply(layout$groups, function(group) {
          lapply(seq_along(group$conditions), function(ii) {
            th(
              class = if (ii == 1L) "dt-center crp-group-start" else "dt-center",
              group$conditions[[ii]]
            )
          })
        })
      )
    )
  ))
}


# Decimal places for one metric group: values reaching 10 in magnitude get one
# decimal, everything else two. Applied per group (not per column) so all
# conditions of a metric stay visually aligned. Empty or all-NA input falls
# back to two decimals.
crp_viewer_table_digits <- function(values) {
  values <- abs(as.numeric(values))
  values <- values[is.finite(values)]
  if (length(values) && max(values) >= 10) { 1L } else { 2L }
}
