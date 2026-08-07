# Trial-level CRP parameters, one panel per condition group.
#
# `x$data$group_data[[ii]]$params` is a trial x channel matrix for condition group
# `ii`; `x$electrodes` is its channel axis and `x$data$parameter_name` names the
# `crp_df` column it was extracted from. The parameter is carried in the object, so
# a single function renders `data_crp_param_alpha_prime`, `_snr`, `_expl_var` and
# anything else `prepare_data_crp_param_by_trial_channel()` produces.
#
# Channels run down the y axis and trials along the x axis, matching every other
# by-electrode figure, so the ranked channel labels of
# `plot_data_crp_by_channel_heatmap()` carry over unchanged.
#
# The trial axis is self-describing: `group_data[[ii]]$group` carries `conditions`,
# `trial_count` and `trials_included`, so `sort_by` needs nothing beyond `x`.
# `params` columns are in `trials_included` order, i.e. concatenated
# condition-by-condition -- that is the `"stimuli"` order, and `"trial"` re-sorts
# it by trial number.
#
# `show_summary` hangs a narrow bar panel off the right of each heatmap, one
# horizontal bar per channel row, collapsing that row's trials to their mean and
# standard error. It is the same summary the heatmap already shows, read across
# instead of down -- the colour scale answers "which trial", the bars answer "how
# big, on average".


# Gap between a heatmap and its summary bars, in inches -- a seam, not a margin:
# the bars annotate the panel they sit against and should read as part of it.
SUMMARY_BAR_GAP <- 0.05


# Axis title for a CRP parameter. Unknown names fall back to the column name.
crp_param_label <- function(parameter_name) {
  label <- CRP_PARAM_LABELS[[as.character(parameter_name)]]
  if (is.null(label)) { label <- as.character(parameter_name) }
  label
}


# Per-channel mean across trials and its standard error. `m` is trial x channel,
# as stored in `group_data[[ii]]$params`. Channels with fewer than two finite
# trials get a zero-width error bar rather than an NA one, so a thin condition
# still draws its bar; channels with no finite trial at all stay NA and are
# skipped by `add_channel_summary_bars()`.
crp_param_channel_stats <- function(m) {
  avg <- colMeans(m, na.rm = TRUE)
  avg[!is.finite(avg)] <- NA_real_

  n <- colSums(!is.na(m))
  se <- apply(m, 2L, stats::sd, na.rm = TRUE) / sqrt(n)
  se[!is.finite(se)] <- 0

  list(mean = unname(avg), se = unname(se))
}


# Summary bars for one heatmap panel: `stats` from `crp_param_channel_stats()`,
# drawn at the channel rows `at` of the heatmap it sits beside.
#
# `ylim` is the heatmap's own y range (`par("usr")`, taken right after the
# `image()` call) held with `yaxs = "i"`, so bar `k` lines up with heatmap row
# `k` whatever the panel height. `xlim` is shared by every panel -- comparing
# conditions is the whole point of the bars, and each would otherwise scale to
# itself. The value axis is dropped: at a ninth of the heatmap's width there is
# no room for it, and the colour bar already states the parameter's scale.
add_channel_summary_bars <- function(stats, at, xlim, ylim, col = "#a6a6a6",
                                     col.se = "#4d4d4d", height = 0.8) {

  graphics::plot.default(
    x = 0, y = 0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "",
    xlim = xlim, ylim = ylim, yaxs = "i"
  )

  avg <- stats$mean
  se <- stats$se
  ok <- which(is.finite(avg))
  if (!length(ok)) { return(invisible()) }

  # Bars grow from zero, so a signed parameter (`al_p`) reads left/right
  graphics::rect(
    xleft = pmin(0, avg[ok]), xright = pmax(0, avg[ok]),
    ybottom = at[ok] - height / 2, ytop = at[ok] + height / 2,
    col = col, border = NA
  )
  graphics::abline(v = 0, col = col.se, lwd = 0.5)

  spread <- ok[se[ok] > 0]
  if (length(spread)) {
    graphics::segments(
      x0 = avg[spread] - se[spread], x1 = avg[spread] + se[spread],
      y0 = at[spread], y1 = at[spread], col = col.se, lwd = 0.75
    )
  }

  invisible()
}


plot_data_crp_param_by_trial_channel_heatmap <- function(
    x, electrode_mask = NULL, sort_by = use_trial_sort_by(),
    space = use_plot_space_resolved()$space,
    space_mode = use_plot_space_resolved()$space_mode,
    channel_annotation = use_channel_annotation_style(), cex = use_cex(),
    mfrow = NULL, col = use_continuous_colormap()$colors,
    show_summary = TRUE, ...) {

  show_summary <- isTRUE(show_summary)
  space_mode <- match.arg(space_mode, choices = OPTIONS_SPACE_MODE)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)
  sort_by <- match.arg(sort_by, choices = OPTIONS_TRIAL_SORT)

  sel <- resolve_channel_selection(x, electrode_mask)
  chan_names <- channel_names(sel$coord_table, channel_annotation)
  n_channels <- sel$n
  axis_style <- crp_channel_axis_style(sel$coord_table)

  group_data <- x$data$group_data
  n_groups <- length(group_data)
  if (!n_groups) { return(invisible()) }

  # Masked trial x channel matrices, shared across the range and limit calculations
  params <- lapply(group_data, function(d) {
    d$params[, sel$index, drop = FALSE]
  })

  if (length(col) == 0) {
    col <- ravepipeline::CONTINUOUS_COLORMAPS("default")
  }
  if (length(col) < 101) {
    col <- grDevices::colorRampPalette(col)(101)
  }
  n_colors <- length(col)

  # Colour scale follows the data, not the parameter: symmetric diverging when the
  # values straddle zero (e.g. the signed fitted amplitude `al_p`), sequential from
  # zero when they do not (`snr`, `expl_var`).
  data_range <- range(unlist(params), na.rm = TRUE)
  diverging <- isTRUE(data_range[[1]] < 0 && data_range[[2]] > 0)

  limit <- get_spacing(params, space = space, space_mode = space_mode)
  vlim <- if (diverging) {
    c(-limit, limit)
  } else if (data_range[[2]] <= 0) {
    col <- col[seq_len(ceiling(n_colors / 2))]
    c(-limit, 0)
  } else {
    col <- col[-seq_len(floor(n_colors / 2))]
    c(0, limit)
  }

  # A rejected trial is NA in `params`, which `image()` would leave unpainted --
  # the same white a near-zero value gets from a diverging colormap. Only worth
  # a colour of its own if there is a rejected trial to mark.
  na_col <- NULL
  if (any(vapply(params, anyNA, FALSE))) {
    na_col <- heatmap_na_color(vlim = vlim, col = col)
  }

  # Summary bars are drawn from the unclamped values -- the heatmap clamps to
  # `vlim` so the colour scale stays readable, but a mean pulled in by clamping
  # would no longer be the mean. One shared x range over every panel's bars and
  # error bars, always including zero since that is where the bars start.
  if (show_summary) {
    bar_stats <- lapply(params, crp_param_channel_stats)
    bar_xlim <- range(c(0, unlist(lapply(bar_stats, function(s) {
      c(s$mean - s$se, s$mean + s$se)
    }))), na.rm = TRUE, finite = TRUE)
    if (!all(is.finite(bar_xlim)) || diff(bar_xlim) <= 0) {
      bar_xlim <- c(0, 1)
    }
  }

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  par_opt <- prepare_par(cex = cex)
  mar <- par_opt$mar

  # The condition names hang below the trial axis at 45 degrees, so what they
  # cost in height is their width projected onto the vertical -- widen the bottom
  # margin to fit the longest one. Only the stimuli ordering draws them.
  if (sort_by == "stimuli") {
    max_bottom_margin <- max(strwidth(x$unique_conditions, units = "inches",
                                      cex = cex * 0.85))
    max_bottom_margin <- (max_bottom_margin + strwidth(" ", units = "inches") * 2) /
      sqrt(2)
    if (par_opt$mai[[1]] < max_bottom_margin) {
      mar[[1]] <- mar[[1]] / par_opt$mai[[1]] * max_bottom_margin + 0.1
    }
  }

  # `add_axis_ranked()` thins side-2 labels by their *height*, so a long channel
  # name is never dropped -- it just runs off the panel. Widen the left margin to
  # the longest one instead ("full" annotation is the case that overflows).
  max_left_margin <- max(strwidth(chan_names, units = "inches",
                                  cex = par_opt$cex.axis * cex), 0, na.rm = TRUE)
  max_left_margin <- max_left_margin + strwidth(" ", units = "inches") * 2
  if (par_opt$mai[[2]] < max_left_margin) {
    mar[[2]] <- mar[[2]] / par_opt$mai[[2]] * max_left_margin + 0.1
  }

  # Reserve the last column for one colour bar per panel row. With the summary
  # on, each panel owns two cells -- heatmap then bars, at 9:1 -- and the cell
  # numbers stay consecutive, so `layout()`'s ascending draw order is the order
  # the loop below draws in: every colour bar first, then each panel's pair.
  n_legend <- mfrow[[1]]
  n_cells <- if (show_summary) { 2L } else { 1L }
  lmat <- matrix(n_legend + seq_len(prod(mfrow) * n_cells),
                 nrow = mfrow[[1]], byrow = TRUE)
  lmat <- cbind(lmat, seq_len(n_legend))
  panel_widths <- if (show_summary) { c(9, 1) } else { 1 }
  graphics::layout(lmat, widths = c(rep(panel_widths, mfrow[[2]]),
                                    graphics::lcm(3)))

  graphics::par(mar = c(mar[[1]], 3.5, mar[[3]], mar[[4]]), cex = 1)
  for (ii in seq_len(mfrow[[1]])) {
    add_heatmap_legend(vlim = vlim, col = col, na_col = na_col,
                       title = crp_param_label(x$data$parameter_name), cex = cex)
  }

  # The bars share the heatmap's top and bottom margins -- that is what makes the
  # two plot regions the same height, and so the rows line up. Between them sits
  # `SUMMARY_BAR_GAP` inches and nothing else: the bars carry no axis, so the
  # heatmap gives up its right margin (which would otherwise be the far side of
  # the figure) and the two panels split the gap. Margins are in text lines, and
  # `mai / mar` is what one line is worth in inches.
  mar_heatmap <- mar
  if (show_summary) {
    gap <- SUMMARY_BAR_GAP / (par_opt$mai[[2]] / par_opt$mar[[2]]) / 2
    mar_heatmap[[4]] <- gap
    mar_bar <- c(mar[[1]], gap, mar[[3]], mar[[4]])
  }

  graphics::par(mar = mar_heatmap, cex = 1)

  # Channel `k` occupies row `n_channels - k + 1`, so the labels are placed
  # against these rows -- channels read top-down, as in every other by-electrode
  # figure
  channel_rows <- rev(seq_len(n_channels))

  for (ii in seq_len(n_groups)) {

    group <- group_data[[ii]]$group

    # trial x channel; clamp so out-of-limit values stay at the ends of the scale
    z <- params[[ii]]
    z[z < vlim[[1]]] <- vlim[[1]]
    z[z > vlim[[2]]] <- vlim[[2]]

    n_trials <- nrow(z)

    # Columns are in `trials_included` order, i.e. grouped by condition
    if (sort_by == "trial") {
      z <- z[order(group$trials_included), , drop = FALSE]
    }

    # `image()` reads `z` as x-major, so trial x channel already gives trials on
    # the x axis; reversing the channel columns puts channel 1 at the top
    z <- z[, rev(seq_len(n_channels)), drop = FALSE]

    graphics::image(
      x = seq_len(n_trials),
      y = seq_len(n_channels),
      z = z,
      axes = FALSE,
      xlab = "",
      ylab = "",
      zlim = vlim,
      main = "",
      adj = 0,
      col = col,
      cex = cex,
      cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex
    )

    # Repaint the cells `image()` skipped, in the colour the legend names. The
    # two images cover disjoint cells, so nothing shows through a seam.
    if (!is.null(na_col) && anyNA(z)) {
      z_na <- array(NA_real_, dim = dim(z))
      z_na[is.na(z)] <- 1
      graphics::image(
        x = seq_len(n_trials),
        y = seq_len(n_channels),
        z = z_na,
        zlim = c(0, 1),
        col = na_col,
        add = TRUE
      )
    }

    # Read the panel's y range while it is current, so the bars can hold it
    heatmap_usr <- graphics::par("usr")

    add_axis_ranked(
      at = channel_rows,
      labels = chan_names,
      rank = axis_style$rank, thin = TRUE, col.axis = axis_style$col,
      side = 2L, cex = cex, tick = TRUE,
      tck = -0.005 * (1 + cex), gap = 1)

    if (sort_by == "trial") {
      add_axis_trial_number(group, cex = cex, side = 1L)
    } else {
      add_axis_trial_stimuli(group, cex = cex, side = 1L)
    }

    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par_opt$cex.main * cex
    )

    if (show_summary) {
      graphics::par(mar = mar_bar)
      add_channel_summary_bars(
        stats = bar_stats[[ii]], at = channel_rows,
        xlim = bar_xlim, ylim = heatmap_usr[c(3, 4)]
      )
      graphics::par(mar = mar_heatmap)
    }

  }

  invisible()
}


# `plot()` is the scripting entry point, so its defaults are stated outright
# rather than read from the preference store: the same call has to produce the
# same figure on any machine. The module UI calls `plot_data_*()` instead, whose
# defaults do follow the user's preferences.
plot.data_crp_param_by_trial_channel <- function(
    x, type = "heatmap",
    space = DEFAULT_PLOT_SPACE / 100, space_mode = OPTIONS_SPACE_MODE,
    sort_by = DEFAULT_TRIAL_SORT,
    channel_annotation = DEFAULT_CHAN_ANNOT, cex = DEFAULT_CEX,
    col = ravepipeline::CONTINUOUS_COLORMAPS(DEFAULT_CONTINUOUS_COLORMAP),
    show_summary = TRUE, ...) {
  type <- match.arg(type, choices = "heatmap")
  plot_data_crp_param_by_trial_channel_heatmap(
    x = x, space = space, space_mode = space_mode, sort_by = sort_by,
    channel_annotation = channel_annotation, cex = cex, col = col,
    show_summary = show_summary, ...
  )
}
