# CRP canonical response by channel, one panel per condition group.
#
# `x$data$canonical` is a time x channel x cond_group array; `$onset` (`tau_onset`)
# and `$offset` (`tau_R`) are 1 x channel x cond_group and get decorated as
# semi-transparent black dots. `x$electrodes` is the channel axis -- CRP runs over
# every loaded channel, so it is not necessarily `coord_table$Electrode`.
#
# Which channels are drawn is decided here, at plot time, by `electrode_mask`.


# Shared setup for both renderings.
plot_data_crp_by_channel_setup <- function(
    x, electrode_mask, space, space_mode, time_range, channel_annotation) {

  sel <- resolve_channel_selection(x, electrode_mask)

  canonical <- x$data$canonical[, sel$index, , drop = FALSE]

  list(
    canonical = canonical,
    onset = x$data$onset[, sel$index, , drop = FALSE],
    offset = x$data$offset[, sel$index, , drop = FALSE],
    selection = sel,
    space = get_spacing(canonical, space = space, space_mode = space_mode),
    time_info = get_time_range(x$time_points, time_range = time_range),
    channel_names = channel_names(sel$coord_table, channel_annotation)
  )
}


plot_data_crp_by_channel_heatmap <- function(
    x, electrode_mask = NULL,
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    channel_annotation = OPTIONS_CHAN_ANNOT, cex = 1, crp = TRUE,
    mfrow = NULL, vertical_marks = 0, col = NULL, ...) {

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
  #     space              = 0.99,
  #     space_mode         = "quantile"
  #   )
  # )

  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_crp_by_channel_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation)

  n_channels <- setup$selection$n
  space <- setup$space
  time_points <- x$time_points
  time_info <- setup$time_info
  time_range <- time_info$time_range
  n_groups <- x$n
  coord_table <- setup$selection$coord_table

  # Leads are told apart by alternating the label colour. Both columns come from
  # `recalculate_short_labels()` and are absent for a degenerate (empty) channel
  # table, which leaves `channel_col` zero-length -- one colour, no alternation.
  fg <- graphics::par("fg")
  channel_rank <- coord_table$LabelRank %||% 1L
  channel_col <- ifelse(coord_table$LeadIndex %% 2L, fg,
                        grDevices::adjustcolor(fg, alpha.f = 0.5))

  if (length(col) == 0) {
    pal <- use_continuous_colormap()
    col <- pal$colors
  }
  if (length(col) < 101) {
    col <- grDevices::colorRampPalette(col)(101)
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
    add_heatmap_legend(vlim = c(-space, space), col = col, cex = cex, title = "")
  }

  graphics::par(mar = mar, cex = 1)

  for (ii in seq_len(n_groups)) {

    z <- array(
      setup$canonical[, rev(seq_len(n_channels)), ii],
      dim = c(length(time_points), n_channels)
    )
    z[z < -space] <- -space
    z[z > space] <- space

    graphics::image(
      x = time_points,
      y = seq_len(n_channels),
      z = z,
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = time_range, cex = cex,
      zlim = c(-space, space),
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

    if (isTRUE(crp)) {
      graphics::points(setup$onset[1, , ii], seq_len(n_channels), pch = 20, col = "#00000060")
      graphics::points(setup$offset[1, , ii], seq_len(n_channels), pch = 20, col = "#00000060")
    }

    group <- x$groups[[ii]]
    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par_opt$cex.main * cex
    )

  }

  invisible()
}


plot_data_crp_by_channel_multiline <- function(
    x, electrode_mask = NULL,
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    channel_annotation = OPTIONS_CHAN_ANNOT, cex = 1, crp = TRUE,
    mfrow = NULL, vertical_marks = 0, ...) {

  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  setup <- plot_data_crp_by_channel_setup(
    x, electrode_mask, space, space_mode, time_range, channel_annotation)

  n_channels <- setup$selection$n
  vspace <- setup$space * 2
  time_points <- x$time_points
  time_info <- setup$time_info
  n_groups <- x$n

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)
  par_opt <- prepare_par(mfrow = mfrow, cex = cex)

  # Channel baselines used by plot_signals: trace value at time t is drawn at
  # y = vspace * channel_index + canonical_value(t)
  ybase <- vspace * seq_len(n_channels)

  # Interpolate the canonical value at a given time on a channel's trace, so the
  # onset/offset decorations sit on the signal rather than the baseline.
  interp_on_trace <- function(t_val, y_vals) {
    if (is.na(t_val)) { return(NA_real_) }
    ok <- is.finite(y_vals)
    if (sum(ok) < 2) { return(NA_real_) }
    stats::approx(time_points[ok], y_vals[ok], xout = t_val)$y
  }

  for (ii in seq_len(n_groups)) {

    c_sub <- array(setup$canonical[, , ii], dim = c(length(time_points), n_channels))

    ravetools::plot_signals(
      signals = t(c_sub),
      sample_rate = x$sample_rate,
      start_time = time_info$start_time,
      duration = time_info$duration,
      time_shift = time_info$time_shift,
      space = vspace,
      space_mode = "absolute",
      channel_names = setup$channel_names,
      ylab = "",
      xlim = time_info$time_range,
      cex = cex
    )

    add_vertical_marks(vertical_marks)

    if (isTRUE(crp)) {
      onset_t <- setup$onset[1, , ii]
      offset_t <- setup$offset[1, , ii]
      onset_y <- vapply(seq_len(n_channels), function(ch) {
        interp_on_trace(onset_t[[ch]], c_sub[, ch])
      }, numeric(1))
      offset_y <- vapply(seq_len(n_channels), function(ch) {
        interp_on_trace(offset_t[[ch]], c_sub[, ch])
      }, numeric(1))
      graphics::points(onset_t, ybase + onset_y, pch = 20, col = "#00000060")
      graphics::points(offset_t, ybase + offset_y, pch = 20, col = "#00000060")
    }

    group <- x$groups[[ii]]
    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par_opt$cex.main * cex
    )

  }

  invisible()
}


plot.data_crp_by_channel <- function(x, type = c("multiline", "heatmap"), ...) {
  type <- match.arg(type)
  switch(
    type,
    "heatmap" = plot_data_crp_by_channel_heatmap(x = x, ...),
    plot_data_crp_by_channel_multiline(x = x, ...)
  )
}
