# The CRP "Channel filter": the dropdown choices, and the electrode selection
# those choices produce.
#
# `erp_results_for_viewer` (`prepare_data_crp_3dviewer_value()`, `shared-crp.R`)
# is wide -- one column per metric x condition group, named
# "<metric> (<condition>)". A filter row names either one of those columns, or
# one of the aggregates "all:<metric>" / "any:<metric>", which stand for every
# condition column of that metric joined by `&` / `|`. Asking for a metric in
# every condition (or in at least one) is the common case, and spelling it out
# one row per condition burns the row budget and has to be rebuilt whenever the
# condition groups change.
#
# The two namespaces cannot collide: a real column always contains " (", and no
# `CRP_VIEWER_METRICS` entry is named "all" or "any". `crp_filter_electrodes()`
# tries an exact column match first regardless.
#
# Both functions are pure (no Shiny, no pipeline access) so they can be unit
# tested on synthetic tables.


# Choices for a filter row's metric dropdown. Every entry reads as what it
# filters on -- "all:al_p", "any:al_p", "al_p (A)" -- so the dropdown needs no
# separate labels. Aggregates come first: they are what most filters want.
crp_filter_choices <- function(column_names, metrics = CRP_VIEWER_METRICS) {

  layout <- crp_viewer_table_layout(column_names, metrics = metrics)

  # `t_proj` leads (the usual first cut), `onset` trails (often absent), the
  # rest keep `CRP_VIEWER_METRICS` order -- `order()` is stable.
  metric_names <- names(layout$groups)
  rank <- ifelse(metric_names == "t_proj", 0L,
                 ifelse(metric_names == "onset", 2L, 1L))

  # Every "all:", then every "any:", then the individual columns -- the two
  # aggregate blocks are each scanned as a unit, so they stay together
  all_of <- character()
  any_of <- character()
  columns <- character()

  for (group in layout$groups[order(rank)]) {

    # `layout` spans every condition of every metric so the results-table header
    # stays rectangular; only the columns actually in the table can be filtered
    cols <- intersect(group$columns, column_names)
    columns <- c(columns, cols)

    # With one condition the aggregates would just repeat that column
    if (length(cols) > 1) {
      all_of <- c(all_of, sprintf("all:%s", group$metric))
      any_of <- c(any_of, sprintf("any:%s", group$metric))
    }
  }

  c(all_of, any_of, columns)
}


# Select electrodes whose CRP metrics satisfy every active filter, for the
# interactive channel filter. The result is written into the analysis-electrode
# selector, which is the single source of `electrode_mask` for every figure.
#
# `erp_tbl`: data.frame with an `Electrode` column plus metric columns.
# `filters`: list of components, each `list(name = <choice>, criteria = <code>,
# threshold = <text "T1" or "T1, T2">, operator = "and"/"or")`. `name` is a
# column of `erp_tbl` or an aggregate from `crp_filter_choices()`. Criteria codes
# (matching the 3D viewer threshold methods, with their boundary conventions):
#   eq v=T1, abs_lt |v|<T1, abs_gte |v|>=T1, lt v<T1, gte v>=T1,
#   in v in [T1,T2], not_in v not in [T1,T2]
# Components combine left-to-right in the order given; each component's operator
# joins it to the running result (the first active component's operator is
# ignored), e.g. c1 AND c2 OR c3 AND c4 == (((c1 & c2) | c3) & c4).
# Components with a blank threshold, unknown name, or insufficient bounds are
# skipped. NA metric values fail the column they are in -- so a channel whose CRP
# failed in one condition is rejected by that condition's `ALL`, while its `ANY`
# can still be carried by another condition. Returns the passing electrode
# numbers, or NULL when there is no usable table or no active filter (-> plot
# all).
crp_filter_electrodes <- function(erp_tbl, filters) {
  if (!is.data.frame(erp_tbl) || !nrow(erp_tbl) || !length(filters)) {
    return(NULL)
  }

  electrodes <- erp_tbl$Electrode
  result <- NULL

  for (filter in filters) {
    criteria <- filter$criteria %||% "abs_gte"

    # A row names one column, or an aggregate over every condition of a metric
    name <- as.character(filter$name %||% "")
    name <- if (length(name)) { name[[1]] } else { "" }
    mode <- "all"
    if (name %in% names(erp_tbl)) {
      columns <- name
    } else {
      parsed <- regmatches(name, regexec("^(all|any):(.+)$", name))[[1]]
      if (length(parsed) != 3L) { next }
      mode <- parsed[[2]]
      columns <- grep(sprintf("^%s \\(", parsed[[3]]), names(erp_tbl),
                      value = TRUE)
    }
    if (!length(columns)) { next }

    bounds <- suppressWarnings(as.numeric(
      strsplit(trimws(as.character(filter$threshold %||% "")), "[,[:space:]]+")[[1]]
    ))
    bounds <- bounds[is.finite(bounds)]
    if (!length(bounds)) { next }

    passed <- NULL
    for (column in columns) {
      v <- suppressWarnings(as.numeric(erp_tbl[[column]]))

      m <- switch(
        criteria,
        "eq"      = v == bounds[[1]],
        "abs_lt"  = abs(v) < bounds[[1]],
        "abs_gte" = abs(v) >= bounds[[1]],
        "lt"      = v < bounds[[1]],
        "gte"     = v >= bounds[[1]],
        "in"      = if (length(bounds) >= 2) v >= min(bounds[1:2]) & v <= max(bounds[1:2]) else NULL,
        "not_in"  = if (length(bounds) >= 2) v < min(bounds[1:2]) | v > max(bounds[1:2]) else NULL,
        NULL
      )
      # Unusable criteria: nothing about it depends on the column, so the whole
      # component is skipped, aggregate or not
      if (is.null(m)) { passed <- NULL; break }

      m[is.na(m)] <- FALSE
      passed <- if (is.null(passed)) {
        m
      } else if (identical(mode, "any")) {
        passed | m
      } else {
        passed & m
      }
    }
    if (is.null(passed)) { next }

    if (is.null(result)) {
      # first active component: operator ignored
      result <- passed
    } else {
      op <- filter$operator %||% "and"
      result <- if (identical(op, "or")) { result | passed } else { result & passed }
    }
  }

  if (is.null(result)) { return(NULL) }
  electrodes[result]
}
