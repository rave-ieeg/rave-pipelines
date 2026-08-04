# CRP canonical response by channel.
#
# `x$data$canonical` is a time x channel x cond_group array of unit-free CRP
# shapes; `$al_mean` is the matching 1 x channel x cond_group array of mean fitted
# amplitudes. Multiplying the two puts the response back into micro-volts, which
# is what `scale_back = TRUE` does -- see `prepare_data_crp_by_channel()`. Left
# unscaled, the values have no unit and only their shape is meaningful.
#
# `$onset` (`tau_onset`) and `$offset` (`tau_R`) are 1 x channel x cond_group and
# get decorated as semi-transparent dots. `x$electrodes` is the channel axis.
#
# Which channels are drawn is decided here, at plot time, by `electrode_mask`.
#
# Three renderings, mirroring `shared-plot_data_by_channel_condition.R`:
#   multiline  one panel per condition group, channels stacked as offset traces
#   heatmap    one panel per condition group, channels as a time x channel image
#   overlay    one panel per channel, condition groups superimposed


# Shared setup for all three renderings: mask the data, apply `scale_back`, pick
# colours, resolve the time axis.
plot_data_crp_by_channel_setup <- function(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col,
    scale_back) {

  sel <- resolve_channel_selection(x, electrode_mask)

  canonical <- x$data$canonical[, sel$index, , drop = FALSE]

  if (scale_back) {
    # A container built before `al_mean` was carried alongside `canonical` would
    # silently collapse the array below and fail several steps later with an
    # unrelated "incorrect number of dimensions"; say what is actually wrong.
    if (!length(x$data$al_mean)) {
      stop("`scale_back = TRUE` needs `al_mean`, which this `data_crp_by_channel` ",
           "does not carry. Rebuild the pipeline (`pipeline$clean()`, then re-run) ",
           "and try again.")
    }
    # `al_mean` is one value per channel per group; `canonical` runs time fastest,
    # so repeating each amplitude across the time axis lines the two up
    al_mean <- x$data$al_mean[, sel$index, , drop = FALSE]
    canonical <- canonical *
      rep(as.vector(al_mean), each = dim(canonical)[[1]])
  } else if (identical(space_mode, "absolute")) {
    # An absolute (uV) spacing means nothing against a unit-free shape
    space <- 1
    space_mode <- "quantile"
  }

  group_indexes <- x$group_indexes
  if (!length(col)) {
    col <- use_discrete_colormap()$colors
  }
  max_group <- max(group_indexes)
  if (length(col) < max_group) {
    col <- rep(col, ceiling(max_group / length(col)))
  }

  # `get_spacing()` returns a half-width; these panels use the full peak-to-peak
  # span, matching `plot_data_by_channel_condition_setup()`
  space <- get_spacing(canonical, space = space, space_mode = space_mode) * 2

  list(
    canonical = canonical,
    onset = x$data$onset[, sel$index, , drop = FALSE],
    offset = x$data$offset[, sel$index, , drop = FALSE],
    selection = sel,
    space = space,
    scale_back = scale_back,
    # Whole micro-volts, or enough decimals to tell unscaled shape values apart
    fmt = if (scale_back) { "%.0f" } else { value_format(c(-1, 1) * space / 2) },
    col = col[group_indexes],
    time_info = get_time_range(x$time_points, time_range = time_range),
    channel_names = channel_names(sel$coord_table, channel_annotation)
  )
}


# Value annotation for the CRP panels: `mu * V` only when the values have been
# scaled back into micro-volts, since an unscaled canonical shape has no unit and
# the number has to stand alone. `values` is one number or a low/high pair.
crp_value_label <- function(values, fmt, scale_back) {
  txt <- sprintf(fmt, values)
  if (length(txt) > 1) {
    if (scale_back) {
      bquote(.(txt[[1]]) ~ "~" ~ .(txt[[2]]) ~ mu * V)
    } else {
      bquote(.(txt[[1]]) ~ "~" ~ .(txt[[2]]))
    }
  } else if (scale_back) {
    bquote(.(txt) ~ mu * V)
  } else {
    bquote(.(txt))
  }
}


# Interpolate a channel's canonical value at a given time, so the onset/offset
# decorations sit on the signal rather than on its baseline.
interp_on_trace <- function(t_val, time_points, y_vals) {
  if (is.na(t_val)) { return(NA_real_) }
  ok <- is.finite(y_vals)
  if (sum(ok) < 2) { return(NA_real_) }
  stats::approx(time_points[ok], y_vals[ok], xout = t_val)$y
}


# Per-channel label priority and the alternating lead colour, shared by the two
# renderings that label a channel axis. Both columns come from
# `recalculate_short_labels()` and are absent for a degenerate (empty) channel
# table, which leaves the colour zero-length -- one colour, no alternation.
crp_channel_axis_style <- function(coord_table) {
  fg <- graphics::par("fg")
  list(
    rank = coord_table$LabelRank %||% 1L,
    col = ifelse(coord_table$LeadIndex %% 2L, fg,
                 grDevices::adjustcolor(fg, alpha.f = 0.5))
  )
}


# One panel per condition group; channels stacked as offset traces.
plot_data_crp_by_channel_multiline <- function(
    x, electrode_mask = NULL,
    space = use_plot_space_resolved()$space,
    space_mode = use_plot_space_resolved()$space_mode, time_range = c(NA, NA),
    channel_annotation = use_channel_annotation_style(), cex = use_cex(),
    crp = use_show_crp_decoration(),
    mfrow = NULL, vertical_marks = 0, col = use_discrete_colormap()$colors,
    flip_y = FALSE, scale_back = use_crp_scale_back(), ...) {

  space_mode <- match.arg(space_mode, choices = OPTIONS_SPACE_MODE)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_crp_by_channel_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col,
    scale_back)

  coord_table <- setup$selection$coord_table
  n_channels <- setup$selection$n
  space <- setup$space
  col <- setup$col
  time_points <- x$time_points
  time_info <- setup$time_info
  time_range <- time_info$time_range
  n_groups <- x$n

  axis_style <- crp_channel_axis_style(coord_table)

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)
  par_opt <- prepare_par(mfrow = mfrow, cex = cex,
                         mar = c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1)

  # Channel `k` is drawn at `space * (n_channels - k + 1)` because the channel
  # axis is reversed below; reversing the baselines keeps every decoration
  # indexed by the original channel number.
  ybase <- rev(space * seq_len(n_channels))

  for (ii in seq_len(n_groups)) {

    c_sub <- array(setup$canonical[, , ii], dim = c(length(time_points), n_channels))

    # Measured before `flip_y` negates the traces: the title reports the data,
    # not the drawing. The scale bar below is what tracks the drawn orientation.
    value_range <- range(c_sub, na.rm = TRUE)

    # Channel reversed because R plots bottom-up while channels read top-down
    signals <- t(c_sub[, rev(seq_len(n_channels)), drop = FALSE])
    if (flip_y) {
      signals <- -signals
    }

    # Blank channel names: `plot_signals` still draws the axis line and a tick
    # per channel, but the labels are left to `add_axis_ranked()` below, which
    # thins them from the top down instead of from the bottom up.
    signal_plot <- ravetools::plot_signals(
      signals = signals,
      sample_rate = x$sample_rate,
      start_time = time_info$start_time,
      duration = time_info$duration,
      time_shift = time_info$time_shift,
      space = space,
      space_mode = "absolute",
      channel_names = rep("", n_channels),
      ylab = "",
      xlim = time_info$time_range,
      main = "",
      adj = 0,
      cex = cex, tck = -0.005 * (1 + cex)
    )

    add_axis_ranked(
      at = ybase,
      labels = setup$channel_names,
      rank = axis_style$rank, thin = TRUE, col.axis = axis_style$col,
      side = 2L, cex = cex, tick = FALSE,
      pos = signal_plot$time_range[[1]] + time_info$time_shift
    )

    add_vertical_marks(vertical_marks, col = "#80808040", lty = 2)

    if (isTRUE(crp)) {
      onset_t <- setup$onset[1, , ii]
      offset_t <- setup$offset[1, , ii]
      trace_y <- function(t_vals) {
        vapply(seq_len(n_channels), function(ch) {
          interp_on_trace(t_vals[[ch]], time_points, c_sub[, ch])
        }, numeric(1))
      }
      sign_y <- if (flip_y) { -1 } else { 1 }
      graphics::points(onset_t, ybase + sign_y * trace_y(onset_t),
                       pch = 20, col = "#00000060")
      graphics::points(offset_t, ybase + sign_y * trace_y(offset_t),
                       pch = 20, col = "#00000060")
    }

    group <- x$groups[[ii]]
    graphics::title(main = bquote(.(group$label) ~ scriptstyle(
      "(" *
        .(crp_value_label(value_range, setup$fmt, scale_back)) ~
        ", n =" ~ .(group$n_trials) *
        ")"
    )), adj = 0, cex.main = par_opt$cex.main * cex, col.main = col[[ii]])

    # Scale bar spanning one `space`, at the right edge
    graphics::arrows(time_range[[2]], 0.5 * space, time_range[[2]], 1.5 * space,
                     code = 3L, angle = 20, length = 0.05, col = "#808080", lwd = 2)

    top_label <- if (flip_y) { -space / 2 } else { space / 2 }
    graphics::text(
      time_range[[2]], space * 1.5,
      cex = cex * 0.8, offset = 0.2, adj = c(1.2, 1.5),
      labels = crp_value_label(top_label, setup$fmt, scale_back)
    )
    graphics::text(
      time_range[[2]], space * 0.5,
      cex = cex * 0.8, offset = 0.2, adj = c(1.2, -0.5),
      labels = crp_value_label(-top_label, setup$fmt, scale_back)
    )
  }

  invisible()
}


# One panel per condition group; channels as a time x channel image.
#
# `col` is the continuous colour ramp for the image, as in
# `plot_data_by_channel_condition_heatmap()` -- not the per-condition palette the
# other two renderings take, which is only used for panel titles here and always
# comes from the discrete colormap preference.
plot_data_crp_by_channel_heatmap <- function(
    x, electrode_mask = NULL,
    space = use_plot_space_resolved()$space,
    space_mode = use_plot_space_resolved()$space_mode, time_range = c(NA, NA),
    channel_annotation = use_channel_annotation_style(), cex = use_cex(),
    crp = use_show_crp_decoration(),
    mfrow = NULL, vertical_marks = 0, col = use_continuous_colormap()$colors,
    flip_y = FALSE, scale_back = use_crp_scale_back(), ...) {

  # CRITICAL: do NOT remove
  # DIPSAUS DEBUG START
  # data_crp_by_channel <- pipeline$run('data_crp_by_channel')
  # list2env(
  #   envir = .GlobalEnv,
  #   list(
  #     x                  = data_crp_by_channel,
  #     electrode_mask     = 13:20,
  #     channel_annotation = use_channel_annotation_style(),
  #     cex                = use_cex(),
  #     crp                = TRUE,
  #     vertical_marks     = 0,
  #     time_range         = c(NA, NA),
  #     space              = use_plot_space(),
  #     space_mode         = ifelse(use_plot_space_is_percentile(), "quantile", "absolute"),
  #     flip_y             = FALSE,
  #     mfrow = NULL,
  #     col = NULL
  #   )
  # )

  space_mode <- match.arg(space_mode, choices = OPTIONS_SPACE_MODE)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_crp_by_channel_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation,
    col = NULL, scale_back)

  coord_table <- setup$selection$coord_table
  n_channels <- setup$selection$n
  title_col <- setup$col
  time_points <- x$time_points
  time_info <- setup$time_info
  time_range <- time_info$time_range
  n_groups <- x$n

  # `setup$space` is the peak-to-peak span the stacked traces need; an image is
  # centred on zero, so halve it back to the amplitude `get_spacing()` returned
  zlim <- c(-1, 1) * setup$space / 2

  # Constant data collapses the spacing to zero; `image()` and the colour bar
  # both need a non-zero span
  if (!isTRUE(diff(zlim) > 0)) { zlim <- c(-1, 1) }

  axis_style <- crp_channel_axis_style(coord_table)

  if (!length(col)) {
    col <- use_continuous_colormap()$colors
  }
  if (length(col) < 101) {
    col <- grDevices::colorRampPalette(col)(101)
  }

  # An image has no y axis to reverse, so `flip_y` flips the colour-to-sign
  # association instead. `add_heatmap_legend()` gets the same ramp, so the bar
  # keeps its true µV labels and shows the reversal.
  if (flip_y) {
    col <- rev(col)
  }

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  par_opt <- prepare_par(cex = cex)
  mar <- par_opt$mar

  # Reserve the last column for one colour bar per panel row
  lmat <- matrix(seq_len(prod(mfrow)), nrow = mfrow[[1]], byrow = TRUE)
  lmat <- cbind(lmat + mfrow[[1]], seq_len(mfrow[[1]]))
  graphics::layout(lmat, widths = c(rep(1, mfrow[[2]]), graphics::lcm(3)))

  graphics::par(mar = c(mar[[1]], 3.5, mar[[3]], mar[[4]]), cex = 1)
  for (ii in seq_len(mfrow[[1]])) {
    # No unit to name when the shapes are left unscaled
    add_heatmap_legend(vlim = zlim, col = col, cex = cex,
                       title = if (scale_back) { bquote(mu * "V") } else { "" })
  }

  graphics::par(mar = mar, cex = 1)

  # Channel `k` occupies row `n_channels - k + 1`, so anything indexed by the
  # original channel number has to be placed against these rows
  channel_rows <- rev(seq_len(n_channels))

  for (ii in seq_len(n_groups)) {

    z <- array(
      setup$canonical[, rev(seq_len(n_channels)), ii],
      dim = c(length(time_points), n_channels)
    )

    value_range <- range(z, na.rm = TRUE)
    z[z < zlim[[1]]] <- zlim[[1]]
    z[z > zlim[[2]]] <- zlim[[2]]

    graphics::image(
      x = time_points,
      y = seq_len(n_channels),
      z = z,
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = time_range, cex = cex,
      zlim = zlim,
      main = "",
      adj = 0,
      col = col
    )

    add_axis_time(time_range = time_range, cex = cex)

    add_axis_ranked(
      at = channel_rows,
      labels = setup$channel_names,
      rank = axis_style$rank, thin = TRUE, col.axis = axis_style$col,
      side = 2L, cex = cex, tick = TRUE,
      tck = -0.005 * (1 + cex), gap = 1
    )

    add_vertical_marks(vertical_marks, col = "black", lty = 1)

    if (isTRUE(crp)) {
      graphics::points(setup$onset[1, , ii], channel_rows, pch = 20, col = "#00000060")
      graphics::points(setup$offset[1, , ii], channel_rows, pch = 20, col = "#00000060")
    }

    group <- x$groups[[ii]]
    graphics::title(main = bquote(.(group$label) ~ scriptstyle(
      "(" *
        .(crp_value_label(value_range, setup$fmt, scale_back)) ~
        ", n =" ~ .(group$n_trials) *
        ")"
    )), adj = 0, cex.main = par_opt$cex.main * cex, col.main = title_col[[ii]])
  }

  invisible()
}


# One panel per channel; condition groups superimposed.
plot_data_crp_by_channel_overlay <- function(
    x, electrode_mask = NULL,
    space = use_plot_space_resolved()$space,
    space_mode = use_plot_space_resolved()$space_mode, time_range = c(NA, NA),
    channel_annotation = use_channel_annotation_style(), cex = use_cex(),
    crp = use_show_crp_decoration(),
    mfrow = NULL, vertical_marks = 0, col = use_discrete_colormap()$colors,
    flip_y = FALSE, scale_back = use_crp_scale_back(), ...) {

  space_mode <- match.arg(space_mode, choices = OPTIONS_SPACE_MODE)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_crp_by_channel_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col,
    scale_back)

  n_channels <- setup$selection$n
  space <- setup$space
  col <- setup$col
  time_points <- x$time_points
  time_range <- setup$time_info$time_range
  n_groups <- x$n

  # Always a grid, however few channels survive the mask
  mfrow <- get_mfrow(n = n_channels, mfrow = mfrow, asp = 2, single_row_max = 0)
  par_opt <- prepare_par(mfrow = mfrow, cex = cex)

  ylim <- if (flip_y) { c(0.5, -0.5) * space } else { c(-0.5, 0.5) * space }

  for (ii in seq_len(n_channels)) {
    data_time_by_group <- array(setup$canonical[, ii, ],
                                dim = dim(setup$canonical)[c(1, 3)])
    data_range <- range(data_time_by_group, na.rm = TRUE)

    graphics::matplot(
      x = time_points, y = data_time_by_group, lty = 1, type = "l", lwd = 1,
      xlab = "", ylab = "", axes = FALSE, xaxs = "i",
      xlim = time_range, ylim = ylim,
      cex = cex, cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex,
      adj = 0, col = col,
      main = bquote(
        .(sprintf("Ch %s", setup$channel_names[[ii]])) ~
          scriptstyle(
            "(" ~ .(crp_value_label(data_range, setup$fmt, scale_back)) ~ ")"
          )
      )
    )

    add_axis_time(time_range = time_range, cex = cex)
    add_axis_voltage(
      value_range = c(-0.5, 0.5) * space, cex = cex, fmt = setup$fmt,
      # Unscaled shapes have no unit, so the axis goes unlabelled
      text = if (scale_back) { bquote("Voltage" ~ (mu * V)) } else { "" }
    )

    add_vertical_marks(vertical_marks, col = "#80808040", lty = 2)
    graphics::abline(h = 0, col = "#80808040", lty = 2)

    # Each group's onset and offset, marked on that group's own trace
    if (isTRUE(crp)) {
      for (jj in seq_len(n_groups)) {
        marks <- c(setup$onset[1, ii, jj], setup$offset[1, ii, jj])
        graphics::points(
          marks,
          vapply(marks, interp_on_trace, numeric(1),
                 time_points = time_points, y_vals = data_time_by_group[, jj]),
          pch = 20, col = col[[jj]]
        )
      }
    }

    graphics::legend(
      "topright",
      sprintf("%s, n=%d", x$group_labels, x$n_trials),
      lty = 1,
      col = col,
      bty = "n",
      cex = cex * 0.8
    )
  }

  invisible()
}


# The rendering for a given type. Used by `plot.data_crp_by_channel()` below and
# by the module server, so the type-to-function map lives in one place.
plot_data_crp_by_channel_fun <- function(
    type = c("multiline", "heatmap", "overlay")) {
  switch(
    match.arg(type),
    "heatmap" = plot_data_crp_by_channel_heatmap,
    "overlay" = plot_data_crp_by_channel_overlay,
    plot_data_crp_by_channel_multiline
  )
}


# `plot()` is the scripting entry point, so its defaults are stated outright
# rather than read from the preference store: the same call has to produce the
# same figure on any machine. The module UI calls `plot_data_*()` instead, whose
# defaults do follow the user's preferences.
plot.data_crp_by_channel <- function(
    x, type = c("multiline", "heatmap", "overlay"),
    space = DEFAULT_PLOT_SPACE / 100, space_mode = OPTIONS_SPACE_MODE,
    channel_annotation = DEFAULT_CHAN_ANNOT, cex = DEFAULT_CEX,
    crp = DEFAULT_SHOW_CRP_DECORATION, scale_back = DEFAULT_CRP_SCALE_BACK,
    col = NULL, ...) {
  type <- match.arg(type)
  if (!length(col)) {
    col <- if (identical(type, "heatmap")) {
      ravepipeline::CONTINUOUS_COLORMAPS(DEFAULT_CONTINUOUS_COLORMAP)
    } else {
      ravepipeline::DISCRETE_COLORMAPS(DEFAULT_DISCRETE_COLORMAP)
    }
  }
  plot_data_crp_by_channel_fun(type)(
    x = x, space = space, space_mode = space_mode,
    channel_annotation = channel_annotation, cex = cex,
    crp = crp, scale_back = scale_back, col = col, ...
  )
}
