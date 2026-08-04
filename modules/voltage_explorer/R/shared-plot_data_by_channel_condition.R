# Mean voltage per channel per condition group, collapsed over trials.
#
# `x$data` is a time x channel x condition-group array covering every loaded LFP
# channel; `x$electrodes` is its channel axis. Which channels are drawn is decided
# here, at plot time, by `electrode_mask` -- see `resolve_channel_selection()`.
#
# Three renderings:
#   multiline  one panel per condition group, channels stacked as offset traces
#   heatmap    one panel per condition group, channels as a time x channel image
#   overlay    one panel per channel, condition groups superimposed


# Shared setup for all three renderings: mask the data, pick colours, resolve the
# time axis. Returns everything they need in common.
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

  # Critical for dev debugging: do NOT remove
  # DIPSAUS DEBUG START
  # ravepipeline::pipeline_setup_rmd("voltage_explorer")
  # data_by_channel_condition=pipeline$run("data_by_channel_condition")
  # list2env(
  #   envir = .GlobalEnv,
  #   list(
  #     x                  = data_by_channel_condition,
  #     electrode_mask     = 13:20,
  #     channel_annotation = use_channel_annotation_style(),
  #     cex                = use_cex(),
  #     vertical_marks     = 0,
  #     time_range         = c(NA, NA),
  #     space              = use_plot_space(),
  #     space_mode         = ifelse(use_plot_space_is_percentile(), "quantile", "absolute"),
  #     flip_y             = TRUE,
  #     mfrow = NULL,
  #     col = NULL
  #   )
  # )

  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_by_channel_condition_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col)

  coord_table <- setup$selection$coord_table
  n_channels <- setup$selection$n
  space <- setup$space
  col <- setup$col
  time_info <- setup$time_info
  time_range <- time_info$time_range
  # Leads are told apart by alternating the label colour. Both columns come from
  # `recalculate_short_labels()` and are absent for a degenerate (empty) channel
  # table, which leaves `channel_col` zero-length -- one colour, no alternation.
  fg <- graphics::par("fg")
  channel_rank <- coord_table$LabelRank %||% 1L
  channel_col <- ifelse(coord_table$LeadIndex %% 2L, fg,
                        grDevices::adjustcolor(fg, alpha.f = 0.5))

  n_groups <- x$n
  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 4)
  par_opt <- prepare_par(mfrow = mfrow, cex = cex,
                         mar = c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1)

  for (ii in seq_len(n_groups)) {
    # Time by channel, but channel is reversed because R plots from bottom-left
    # to top-right while we want the channels to order from top to bottom
    data_time_by_channel <- array(setup$data[, rev(seq_len(n_channels)), ii], dim = dim(setup$data)[c(1, 2)])

    # Measured before `flip_y` negates the traces: the title reports the data,
    # not the drawing. The scale bar below is what tracks the drawn orientation.
    value_range <- range(data_time_by_channel, na.rm = TRUE)

    if (flip_y) {
      data_time_by_channel <- - data_time_by_channel
    }

    group <- x$groups[[ii]]

    # Blank channel names: `plot_signals` still draws the axis line and a tick
    # per channel, but the labels are left to `add_axis_ranked()` below, which
    # thins them from the top down instead of from the bottom up. Blank (rather
    # than `NULL`) also side-steps its own decimation of long channel lists.
    signal_plot <- ravetools::plot_signals(
      t(data_time_by_channel),
      sample_rate = x$sample_rate,
      space = space,
      space_mode = "absolute",
      time_shift = time_info$time_shift,
      start_time = time_info$start_time,
      duration = time_info$duration,
      ylab = "",
      channel_names = rep("", n_channels),
      main = "",
      adj = 0,
      cex = cex, tck = -0.005 * (1 + cex)
    )

    # `time_range[[1]]` is the start time `plot_signals` settled on after
    # clamping; sharing its axis position keeps every label aligned on one edge.
    add_axis_ranked(
      at = rev(space * seq_len(n_channels)),
      labels = setup$channel_names,
      rank = channel_rank, thin = TRUE, col.axis = channel_col,
      side = 2L, cex = cex, tick = FALSE,
      pos = signal_plot$time_range[[1]] + time_info$time_shift
    )

    graphics::title(main = bquote(.(group$label) ~ scriptstyle(
      "(" *
        .(round(value_range[[1]])) ~ "~" ~ .(round(value_range[[2]])) ~ mu * V ~
        ", n =" ~ .(group$n_trials) *
        ")"
    )), adj = 0, cex.main = par_opt$cex.main * cex, col.main = col[[ii]])

    add_vertical_marks(vertical_marks, col = "#80808040", lty = 2)

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


# One panel per condition group; channels as a time x channel image.
#
# `col` is the continuous colour ramp for the image, as in
# `plot_data_crp_by_channel_heatmap()` -- not the per-condition palette the other
# two renderings take, which is only used for panel titles here and always comes
# from the discrete colormap preference.
plot_data_by_channel_condition_heatmap <- function(
    x, electrode_mask = NULL,
    space = 1, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    channel_annotation = OPTIONS_CHAN_ANNOT, cex = 1,
    mfrow = NULL, vertical_marks = 0, col = NULL, flip_y = FALSE, ...) {

  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_by_channel_condition_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation, col = NULL)

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

  # See `plot_data_by_channel_condition_multiline()`: leads are told apart by
  # alternating the label colour
  fg <- graphics::par("fg")
  channel_rank <- coord_table$LabelRank %||% 1L
  channel_col <- ifelse(coord_table$LeadIndex %% 2L, fg,
                        grDevices::adjustcolor(fg, alpha.f = 0.5))

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
    add_heatmap_legend(vlim = zlim, col = col, cex = cex)
  }

  graphics::par(mar = mar, cex = 1)

  for (ii in seq_len(n_groups)) {
    # Channel reversed so channel 1 lands at the top, as in the other renderings
    z <- array(setup$data[, rev(seq_len(n_channels)), ii],
               dim = c(length(time_points), n_channels))

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
      at = rev(seq_len(n_channels)),
      labels = setup$channel_names,
      rank = channel_rank, thin = TRUE, col.axis = channel_col,
      side = 2L, cex = cex, tick = TRUE,
      tck = -0.005 * (1 + cex), gap = 1
    )

    add_vertical_marks(vertical_marks, col = "black", lty = 1)

    group <- x$groups[[ii]]
    graphics::title(main = bquote(.(group$label) ~ scriptstyle(
      "(" *
        .(round(value_range[[1]])) ~ "~" ~ .(round(value_range[[2]])) ~ mu * V ~
        ", n =" ~ .(group$n_trials) *
        ")"
    )), adj = 0, cex.main = par_opt$cex.main * cex, col.main = title_col[[ii]])
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


plot.data_by_channel_condition <- function(x, type = c("multiline", "heatmap", "overlay"), ...) {
  type <- match.arg(type)
  switch(
    type,
    "heatmap" = plot_data_by_channel_condition_heatmap(x = x, ...),
    "overlay" = plot_data_by_channel_condition_overlay(x = x, ...),
    plot_data_by_channel_condition_multiline(x = x, ...)
  )
}
