get_spacing <- function(x, space, space_mode = c("quantile", "absolute")) {
  if (length(space_mode) > 1) {
    space_mode <- space_mode[space_mode %in% c("quantile", "absolute")]
    space_mode <- space_mode[[1]]
  } else {
    space_mode <- match.arg(space_mode)
  }

  if (space_mode == "quantile") {
    if (!isTRUE(space > 0 && space < 1)) {
      space <- 1.0
    }
    space <- stats::quantile(abs(unlist(x)),
                             probs = space, na.rm = TRUE)
    space_mode <- "absolute"
  } else {
    space <- abs(space)
    if (!isTRUE(space > 0)) {
      space <- max(abs(unlist(x)), na.rm = TRUE)
    }
  }

  unname(space)
}


# Panel layout for `n` sub-figures. Up to `single_row_max` panels are laid out in a
# single row; beyond that `n2mfrow` picks a grid at the requested aspect ratio. A
# caller-supplied `mfrow` of length 2 always wins.
get_mfrow <- function(n, mfrow = NULL, asp = 3, single_row_max = 4) {
  if (length(mfrow) != 2 || anyNA(mfrow)) {
    if (n > single_row_max) {
      mfrow <- n2mfrow(n, asp = asp)
    } else {
      mfrow <- c(1, n)
    }
  }
  mfrow
}


get_time_range <- function(time_points, time_range = c(NA, NA)) {
  # start_time & end_time are relative to time_shift
  time_shift <- min(time_points, na.rm = TRUE)
  start_time <- time_range[[1]] - time_shift
  if (is.na(start_time)) {
    start_time <- 0
  }
  end_time <- time_range[[2]]
  if (is.na(end_time)) {
    end_time <- max(time_points, na.rm = TRUE)
  }
  end_time <- end_time - time_shift
  duration <- end_time - start_time
  time_range <- c(start_time, end_time) + time_shift

  list(
    time_range = time_range,
    time_shift = time_shift,
    start_time = start_time,
    end_time = end_time,
    duration = duration
  )
}


add_axis_time <- function(time_range, text = "Time (s)", cex = 1) {
  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  xline <- 1.5 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1
  graphics::axis(side = 1L, at = pretty(time_range),
                 las = 1, tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
                 cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)
  graphics::mtext(side = 1, text = text, line = xline,
                  cex = par_opt$cex.lab * cex)
}

add_axis_voltage <- function(value_range, text = bquote("Voltage" ~ (mu * V)), cex = 1) {
  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  yline <- 1 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1
  graphics::axis(
    side = 2L,
    at = c(value_range, 0),
    labels = c(sprintf("%.0f", value_range), "0"),
    las = 1, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex
  )
  graphics::mtext(side = 2L, text, line = 2, cex = cex)
}

# Axis labels with an explicit priority order, for axes too crowded to label in
# full.
#
# `graphics::axis()` already drops labels that would collide, but it always scans
# the axis from its low end upward and keeps whatever it reaches first --
# regardless of the order `at` is given in. Panels that stack channels from the
# top down therefore lose exactly the labels that should have survived, and a
# second `axis()` call cannot see what the first one drew, so separately-drawn
# labels overprint each other.
#
# So the thinning happens here instead: candidates are tried in `rank` order
# (lower wins, ties broken in reading order) and one is kept only when it clears
# every label already accepted. Drawing is one `axis()` call per label -- a call
# carrying a single label has nothing to collide with, so R's own thinning never
# fires and this selection is the only thing that drops anything.
#
# `at`, `labels` as in `graphics::axis()`; `rank` and `col.axis` are recycled over
# them -- one label per call means each can have its own colour. `gap` pads the
# measured label extent. `thin = FALSE` draws every label, overlaps and all.
# `...` is passed to `graphics::axis()` (`pos`, `tick`, ...). Returns the indices
# drawn.
#
# Called through `add_axis_ranked()`, never directly -- how many labels fit
# depends on the device size, so the selection has to be redone on every resize.
draw_axis_ranked <- function(at, labels, rank = 1L, side = 2L, cex = 1,
                             gap = 1.1, thin = TRUE, col.axis = NULL, ...) {
  cex_axis <- graphics::par("cex.axis") * cex

  labels <- as.character(labels)
  candidates <- which(is.finite(at) & !is.na(labels) & nzchar(labels))
  if (!length(candidates)) { return(invisible(integer(0))) }

  if (thin) {
    # Along-axis extent of each label, in user units, measured at the size it
    # will be drawn at. Sides 2/4 are labelled with `las = 1`, so what limits
    # them is text height; sides 1/3 are limited by width.
    extent <- if (side %in% c(2L, 4L)) {
      graphics::strheight(labels, units = "user", cex = cex_axis)
    } else {
      graphics::strwidth(labels, units = "user", cex = cex_axis)
    }
    extent <- extent * gap

    rank <- rep_len(rank, length(at))
    # Reading order: top-down on a vertical axis, left-to-right on a horizontal
    # one. Only used to break ties within a rank.
    along <- if (side %in% c(2L, 4L)) { -at } else { at }
    ordered <- candidates[order(rank[candidates], along[candidates])]

    kept <- integer(0)
    for (ii in ordered) {
      if (!length(kept) ||
          all(abs(at[[ii]] - at[kept]) >= (extent[[ii]] + extent[kept]) / 2)) {
        kept <- c(kept, ii)
      }
    }
    candidates <- sort(kept)
  }

  if (length(col.axis)) {
    col.axis <- rep_len(col.axis, length(at))
  }

  for (ii in candidates) {
    if (length(col.axis)) {
      graphics::axis(side = side, at = at[[ii]], labels = labels[[ii]],
                     las = 1, cex.axis = cex_axis, col.axis = col.axis[[ii]], ...)
    } else {
      graphics::axis(side = side, at = at[[ii]], labels = labels[[ii]],
                     las = 1, cex.axis = cex_axis, ...)
    }
  }

  invisible(candidates)
}

# Draw ranked axis labels, re-thinning them whenever the device is resized.
#
# Resizing a plot does not re-run the code that drew it: R replays the graphics
# engine display list. Base graphics record their `.Internal` calls there, so
# `graphics::axis()` re-runs against the new device size and re-thins itself --
# but a selection made in R beforehand is not on that list, only the labels it
# happened to pick, so those would be replayed frozen at any size. Shiny makes
# this the usual case rather than the exception: `shiny::renderPlot()` defaults
# to `execOnResize = FALSE`, so every resize is a replay.
#
# `grDevices::recordGraphics()` puts the call itself on the display list, along
# with everything it needs to be evaluated again, which is what keeps the
# thinning honest across resizes. `draw_axis_ranked()` -- not this function --
# has to be what gets recorded, or each replay would record itself afresh.
add_axis_ranked <- function(at, labels, rank = 1L, side = 2L, cex = 1,
                            gap = 1.1, thin = TRUE, col.axis = NULL, ...) {
  args <- list(at = at, labels = labels, rank = rank, side = side,
               cex = cex, gap = gap, thin = thin, col.axis = col.axis, ...)
  grDevices::recordGraphics(
    do.call(draw_axis_ranked, args),
    list(args = args, draw_axis_ranked = draw_axis_ranked),
    getNamespace("grDevices")
  )
}

add_vertical_marks <- function(vertical_marks = NULL, col = "#808080", lty = 3, ...) {
  if (length(vertical_marks)) {
    graphics::abline(v = vertical_marks, col = col, lty = lty, ...)
  }
}

add_crp_decorators <- function(crp_result, cex = 1) {
  if (!length(crp_result)) { return() }

  crp_time <- crp_result$parameters$params_times
  crp_mean <- crp_result$parameters$C * mean(crp_result$parameters$al)

  graphics::lines(crp_time, crp_mean, col = "yellow", lwd = 3)

  # `avg_trace_tR` spans [t_start, tau_R]. When onset detection is on, `C` /
  # `params_times` are sliced to [tau_onset, tau_R], so the two axes no longer
  # share a length; rebuild the avg-trace axis from the sampling step, ending at
  # tau_R.
  avg_trace <- crp_result$parameters$avg_trace_tR
  if (length(avg_trace)) {
    ref_time <- crp_result$parameters$params_times_full
    if (length(ref_time) < 2) { ref_time <- crp_time }
    dt <- if (length(ref_time) > 1) stats::median(diff(ref_time)) else NA_real_
    if (isTRUE(is.finite(dt)) && length(avg_trace) == length(crp_time)) {
      avg_time <- crp_time
    } else if (isTRUE(is.finite(dt))) {
      avg_time <- seq(to = crp_result$tau_R, by = dt, length.out = length(avg_trace))
    } else {
      avg_time <- NULL
    }
    if (length(avg_time) == length(avg_trace)) {
      graphics::lines(avg_time, avg_trace, lwd = 1)
    }
  }

  idx <- which.min(abs(crp_time - crp_result$tau_R))

  # points(crp_time[[idx]], crp_mean[[idx]], pch = 1, col = "red")

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  xline <- 1.5 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1

  graphics::axis(
    side = 1,
    at = crp_result$tau_R,
    tick = TRUE,
    pos = crp_mean[[idx]],
    labels = bquote(tau[R] * .(sprintf(
      "=%.2fs", crp_result$tau_R
    ))),
    col = "red", col.axis = "red",
    tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex
  )

}

add_heatmap_legend <- function(vlim, col, title = bquote(mu * "V"), cex = 1,
                               fmt = NULL) {
  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  par_opt$cex.lab <- 1

  vlim <- range(vlim, na.rm = TRUE)

  # Pick a format with enough precision so small (normalized) ranges do not
  # collapse to the same printed value.
  if (is.null(fmt)) {
    span <- diff(vlim)
    if (is.finite(span) && span > 0) {
      digits <- max(0, ceiling(-log10(span)) + 1)
    } else {
      digits <- 0
    }
    fmt <- sprintf("%%.%df", digits)
  }

  legend_z <- seq(vlim[[1]], vlim[[2]], length.out = length(col))

  graphics::image(
    x = 1,
    y = legend_z,
    z = matrix(legend_z, nrow = 1),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = title,
    col = col,
    cex.main = par_opt$cex.main * cex
  )

  graphics::axis(
    side = 2L,
    at = c(vlim, 0),
    labels = c(sprintf(fmt, vlim), "0"),
    las = 1, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex
  )
}

add_axis_trial_number <- function(group, by = 5, cex = 1, vspace = 1) {

  n_trials <- group$n_trials

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  par_opt$cex.lab <- 1
  tck <- -0.005 * (3 + cex)

  if (by > n_trials) {
    at <- c(1, n_trials)
  } else {
    at <- seq(0, by = by, length.out = ceiling(n_trials / by) + 1)
    at <- unique(sort(c(at, 1, n_trials)))

    at <- at[at >= 1 & at <= n_trials]
  }

  graphics::axis(
    side = 2L, at = at * vspace, labels = at, las = 1,
    tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)

}

add_axis_trial_stimuli <- function(group, cex = 1, vspace = 1, lty = 1, col = "#808080") {

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  yline <- 1 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1

  separators <- cumsum(c(0, group$trial_count)) + 0.5

  graphics::axis(
    side = 2L, at = separators * vspace, las = 1,
    labels = rep("", length(group$trial_count) + 1),
    tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)


  graphics::text(
    x = par("usr")[[1]],
    y = (cumsum(group$trial_count) + 0.5 - group$trial_count / 2) * vspace,
    labels = sprintf("%s  ", group$conditions),
    srt = 45, adj = c(1, 0.5),
    cex = 0.85 * cex, xpd = NA)

  if (length(separators) > 1) {
    graphics::abline(h = separators[- c(1)] * vspace,
                     col = col, lty = lty)
  }
}

prepare_par <- function(mfrow = NULL, cex = 1, mar = c(3.1, 3.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1, mgp = cex * c(2, 0.5, 0), tck = -0.005 * (3 + cex),
                        env = parent.frame(), ...) {

  args <- list(
    mar = mar,
    mgp = mgp,
    tck = tck,
    mfrow = mfrow,
    cex = 1,
    ...
  )
  if (!length(mfrow)) {
    args$mfrow <- NULL
    args[[length(args) + 1]] <- "mfrow"
  }

  oldpar <- do.call(graphics::par, args)

  do.call(
    on.exit, list(bquote({
      graphics::par(.(oldpar))
    }), add = TRUE, after = TRUE),
    envir = env
  )

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  par_opt$cex.lab <- 1

  invisible(par_opt)
}


get_filearray_impl <- function(x) {
  if (inherits(x, "RAVEFileArray")) {
    x <- x$`@impl`
  } else {
    x <- filearray::as_filearray(x)
  }
  x
}

recalculate_short_labels <- function(coord_table, electrode_mask = NULL) {
  if (!length(electrode_mask)) { return(coord_table) }

  electrode_mask <- electrode_mask[electrode_mask %in% coord_table$Electrode]
  electrode_mask <- sort(unique(electrode_mask))

  coord_table <- coord_table[coord_table$Electrode %in% electrode_mask, ]
  if (!nrow(coord_table)) {
    return(coord_table)
  }

  # Order by electrode number
  coord_table <- coord_table[order(coord_table$Electrode), ]

  # Add labelprefix
  labels <- coord_table$Label
  label_prefix <- coord_table$LabelPrefix
  label_prefix_lag1 <- c("", label_prefix[-length(label_prefix)])
  is_lead_channel <- label_prefix != label_prefix_lag1
  coord_table$ShortLabel <- ifelse(
    !is_lead_channel,
    gsub("^[a-zA-Z_-]+", "", labels), labels
  )

  # Inner-most channels
  coord_table$LeadChannel <- is_lead_channel

  # Axis-label priority for `add_axis_ranked()`: the first channel of a lead keeps
  # its name however crowded the panel gets, the rest are thinned around it
  coord_table$LabelRank <- ifelse(is_lead_channel, 1L, 2L)

  # Which lead each channel belongs to, so figures can tell leads apart (by
  # alternating the label colour). The leading `TRUE` opens lead 1 at the first
  # channel even when its label prefix is blank, so the alternation cannot invert.
  coord_table$LeadIndex <- cumsum(c(TRUE, is_lead_channel[-1]))

  coord_table
}


# ---- Channel selection -------------------------------------------------------

# Resolve which channels a by-channel figure should draw.
#
# Every `data_*` plot container built from `data_placeholder` carries
#   `$electrodes`     the channel axis of `$data` (its 2nd margin), as electrode
#                     numbers -- NOT necessarily `coord_table$Electrode`, since CRP
#                     runs over every loaded channel while `coord_table` is LFP-only;
#   `$coord_table`    the loaded LFP coordinate table, keyed by `Electrode`;
#   `$electrode_mask` the channels to draw by default.
#
# `electrode_mask` overrides the container default. Matching is by electrode number,
# never by position. An empty mask, or one matching nothing on the axis, falls back
# to every channel, so an over-restrictive selection never yields an empty figure.
resolve_channel_selection <- function(x, electrode_mask = NULL) {

  electrodes <- x$electrodes

  if (!length(electrodes)) {
    # Container predates the explicit axis; the coordinate table is the best guess
    electrodes <- x$coord_table$Electrode
  }

  if (is.null(electrode_mask)) {
    electrode_mask <- x$electrode_mask
  } else {
    electrode_mask <- ravecore:::parse_svec(unlist(electrode_mask))
  }
  electrode_mask <- electrode_mask[electrode_mask %in% electrodes]
  electrode_mask <- sort(unique(electrode_mask))

  if (!length(electrode_mask)) {
    electrode_mask <- electrodes
  }

  # Make sure the short label has proper surgical labels
  coord_table <- recalculate_short_labels(x$coord_table, electrode_mask = electrode_mask)

  index <- which(electrodes %in% electrode_mask)
  kept <- electrodes[index]

  list(
    index = index,
    n = length(index),
    electrodes = kept,
    coord_table = coord_table
  )
}


# Channel tick labels for the annotation styles in `OPTIONS_CHAN_ANNOT`.
channel_names <- function(coord_table, channel_annotation = OPTIONS_CHAN_ANNOT) {
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)
  switch(
    channel_annotation,
    "number" = as.character(coord_table$Electrode),
    "short"  = coord_table$ShortLabel,
    "label"  = coord_table$Label,
    "full"   = sprintf("%s (%s)", coord_table$Electrode, coord_table$Label)
  )
}


# Select electrodes whose CRP metrics satisfy every active filter, for the
# interactive channel filter. The result is written into the analysis-electrode
# selector, which is the single source of `electrode_mask` for every figure.
#
# `erp_tbl`: data.frame with an `Electrode` column plus metric columns.
# `filters`: list of components, each `list(name = <column>, criteria = <code>,
# threshold = <text "T1" or "T1, T2">, operator = "and"/"or")`. Criteria codes
# (matching the 3D viewer threshold methods, with their boundary conventions):
#   eq v=T1, abs_lt |v|<T1, abs_gte |v|>=T1, lt v<T1, gte v>=T1,
#   in v in [T1,T2], not_in v not in [T1,T2]
# Components combine left-to-right in the order given; each component's operator
# joins it to the running result (the first active component's operator is
# ignored), e.g. c1 AND c2 OR c3 AND c4 == (((c1 & c2) | c3) & c4).
# Components with a blank threshold, unknown column, or insufficient bounds are
# skipped. NA metric values fail a filter. Returns the passing electrode numbers,
# or NULL when there is no usable table or no active filter (-> plot all).
crp_filter_electrodes <- function(erp_tbl, filters) {
  if (!is.data.frame(erp_tbl) || !nrow(erp_tbl) || !length(filters)) {
    return(NULL)
  }

  electrodes <- erp_tbl$Electrode
  result <- NULL

  for (filter in filters) {
    column <- filter$name
    criteria <- filter$criteria %||% "abs_gte"
    if (!length(column) || !nzchar(column) || !column %in% names(erp_tbl)) {
      next
    }
    bounds <- suppressWarnings(as.numeric(
      strsplit(trimws(as.character(filter$threshold %||% "")), "[,[:space:]]+")[[1]]
    ))
    bounds <- bounds[is.finite(bounds)]
    if (!length(bounds)) { next }

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
    if (is.null(m)) { next }

    m[is.na(m)] <- FALSE
    if (is.null(result)) {
      # first active component: operator ignored
      result <- m
    } else {
      op <- filter$operator %||% "and"
      result <- if (identical(op, "or")) { result | m } else { result & m }
    }
  }

  if (is.null(result)) { return(NULL) }
  electrodes[result]
}
