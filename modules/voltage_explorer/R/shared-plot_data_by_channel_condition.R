# Mean voltage per channel per condition group, collapsed over trials.
#
# `x$data` is a time x channel x condition-group array covering every loaded LFP
# channel; `x$electrodes` is its channel axis. Which channels are drawn is decided
# here, at plot time, by `electrode_mask` -- see `resolve_channel_selection()`.
#
# Two renderings:
#   multiline  one panel per condition group, channels stacked as offset traces
#   overlay    one panel per channel, condition groups superimposed


# Shared setup for both renderings: mask the data, pick colours, resolve the time
# axis. Returns everything the two functions need in common.
plot_data_by_channel_condition_setup <- function(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col) {

  sel <- resolve_channel_selection(x, electrode_mask)

  # time x channel x group, masked
  data <- x$data[, sel$index, , drop = FALSE]

  # `get_spacing()` returns a half-width; these panels have historically used the
  # full peak-to-peak span, hence the doubling
  space <- get_spacing(data, space = space, space_mode = space_mode) * 2

  group_indexes <- x$group_indexes
  if (!length(col)) {
    col <- use_discrete_colormap()$colors
  }
  max_group <- max(group_indexes)
  if (length(col) < max_group) {
    col <- rep(col, ceiling(max_group / length(col)))
  }

  list(
    data = data,
    selection = sel,
    space = space,
    col = col[group_indexes],
    time_info = get_time_range(x$time_points, time_range = time_range),
    channel_names = channel_names(sel$coord_table, channel_annotation)
  )
}


# One panel per condition group; channels stacked as offset traces.
plot_data_by_channel_condition_multiline <- function(
    x, electrode_mask = NULL,
    space = 1, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    channel_annotation = OPTIONS_CHAN_ANNOT, cex = 1,
    mfrow = NULL, vertical_marks = 0, col = NULL, flip_y = FALSE, ...) {

  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_by_channel_condition_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col)

  coord_table <- setup$selection$coord_table
  n_channels <- setup$selection$n
  space <- setup$space
  col <- setup$col
  time_info <- setup$time_info
  time_range <- time_info$time_range

  # Separators between electrode leads, so stacked traces stay readable
  if (length(coord_table$LeadChannel) && is.logical(coord_table$LeadChannel)) {
    hlines_chs <- which(coord_table$LeadChannel) - 0.5
    hlines_chs <- hlines_chs[hlines_chs >= 1]
  } else if (n_channels > 5) {
    hlines_chs <- seq(0.5, n_channels - 0.5, by = 5)
  } else {
    hlines_chs <- numeric(0L)
  }

  n_groups <- x$n
  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 4)
  prepare_par(mfrow = mfrow, cex = cex,
              mar = c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1)

  for (ii in seq_len(n_groups)) {
    data_time_by_channel <- array(setup$data[, , ii], dim = dim(setup$data)[c(1, 2)])
    group <- x$groups[[ii]]
    ylim <- range(data_time_by_channel, na.rm = TRUE)

    ravetools::plot_signals(
      if (flip_y) { -t(data_time_by_channel) } else { t(data_time_by_channel) },
      sample_rate = x$sample_rate,
      space = space,
      space_mode = "absolute",
      time_shift = time_info$time_shift,
      start_time = time_info$start_time,
      duration = time_info$duration,
      ylab = "",
      channel_names = setup$channel_names,
      main = "",
      adj = 0,
      cex = cex
    )

    graphics::title(main = bquote(.(group$label) ~ scriptstyle(
      "(" *
        .(round(ylim[[1]])) ~ "~" ~ .(round(ylim[[2]])) ~ mu * V ~
        ", n =" ~ .(group$n_trials) *
        ")"
    )), adj = 0, cex.main = par("cex.main") * cex, col.main = col[[ii]])

    add_vertical_marks(vertical_marks, col = "#80808040", lty = 2)

    if (length(hlines_chs)) {
      graphics::abline(h = hlines_chs * space, col = "#80808040", lty = 3)
    }

    # Scale bar spanning one `space`, at the right edge
    graphics::arrows(time_range[[2]], 0.5 * space, time_range[[2]], 1.5 * space,
                     code = 3L, angle = 20, length = 0.05, col = "#808080", lwd = 2)

    top_label <- if (flip_y) { -space / 2 } else { space / 2 }
    graphics::text(
      time_range[[2]], space * 1.5,
      cex = cex * 0.8, offset = 0.2, adj = c(1.2, 1.5),
      labels = bquote(.(sprintf("%.0f", top_label)) ~ mu * V)
    )
    graphics::text(
      time_range[[2]], space * 0.5,
      cex = cex * 0.8, offset = 0.2, adj = c(1.2, -0.5),
      labels = bquote(.(sprintf("%.0f", -top_label)) ~ mu * V)
    )
  }

  invisible()
}


# One panel per channel; condition groups superimposed.
plot_data_by_channel_condition_overlay <- function(
    x, electrode_mask = NULL,
    space = 1, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    channel_annotation = OPTIONS_CHAN_ANNOT, cex = 1,
    mfrow = NULL, vertical_marks = 0, col = NULL, flip_y = FALSE, ...) {

  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_by_channel_condition_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col)

  n_channels <- setup$selection$n
  space <- setup$space
  col <- setup$col
  time_points <- x$time_points
  time_range <- setup$time_info$time_range

  # Always a grid, however few channels survive the mask
  mfrow <- get_mfrow(n = n_channels, mfrow = mfrow, asp = 2, single_row_max = 0)
  par_opt <- prepare_par(mfrow = mfrow, cex = cex)

  ylim <- if (flip_y) { c(0.5, -0.5) * space } else { c(-0.5, 0.5) * space }

  for (ii in seq_len(n_channels)) {
    data_time_by_group <- array(setup$data[, ii, ], dim = dim(setup$data)[c(1, 3)])
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
            "(" ~ .(round(data_range[[1]])) ~ "~" ~ .(round(data_range[[2]])) ~ mu * V ~ ")"
          )
      )
    )

    add_axis_time(time_range = time_range, cex = cex)
    add_axis_voltage(value_range = c(-0.5, 0.5) * space, cex = cex)

    add_vertical_marks(vertical_marks, col = "#80808040", lty = 2)
    graphics::abline(h = 0, col = "#80808040", lty = 2)

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


plot.data_by_channel_condition <- function(x, type = c("multiline", "overlay"), ...) {
  type <- match.arg(type)
  switch(
    type,
    "overlay" = plot_data_by_channel_condition_overlay(x = x, ...),
    plot_data_by_channel_condition_multiline(x = x, ...)
  )
}
