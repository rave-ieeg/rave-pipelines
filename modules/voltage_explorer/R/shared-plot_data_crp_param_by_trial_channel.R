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


# Axis title for a CRP parameter. Unknown names fall back to the column name.
crp_param_label <- function(parameter_name) {
  label <- CRP_PARAM_LABELS[[as.character(parameter_name)]]
  if (is.null(label)) { label <- as.character(parameter_name) }
  label
}


plot_data_crp_param_by_trial_channel_heatmap <- function(
    x, electrode_mask = NULL, sort_by = use_trial_sort_by(),
    space = use_plot_space_resolved()$space,
    space_mode = use_plot_space_resolved()$space_mode,
    channel_annotation = use_channel_annotation_style(), cex = use_cex(),
    mfrow = NULL, col = use_continuous_colormap()$colors, ...) {

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

  # Reserve the last column for one colour bar per panel row
  lmat <- matrix(seq_len(prod(mfrow)), nrow = mfrow[[1]], byrow = TRUE)
  lmat <- cbind(lmat + mfrow[[1]], seq_len(mfrow[[1]]))
  graphics::layout(lmat, widths = c(rep(1, mfrow[[2]]), graphics::lcm(3)))

  graphics::par(mar = c(mar[[1]], 3.5, mar[[3]], mar[[4]]), cex = 1)
  for (ii in seq_len(mfrow[[1]])) {
    add_heatmap_legend(vlim = vlim, col = col,
                       title = crp_param_label(x$data$parameter_name), cex = cex)
  }

  graphics::par(mar = mar, cex = 1)

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
    col = ravepipeline::CONTINUOUS_COLORMAPS(DEFAULT_CONTINUOUS_COLORMAP), ...) {
  type <- match.arg(type, choices = "heatmap")
  plot_data_crp_param_by_trial_channel_heatmap(
    x = x, space = space, space_mode = space_mode, sort_by = sort_by,
    channel_annotation = channel_annotation, cex = cex, col = col, ...
  )
}
