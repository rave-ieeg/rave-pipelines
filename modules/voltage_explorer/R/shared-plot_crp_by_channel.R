# CRP canonical response by channel, one panel per condition group.
# `crp_by_channel$data$canonical` is a time x channel x cond_group array; the
# CRP onset (`tau_onset`) and offset (`tau_R`) for each channel x cond_group are
# decorated as semi-transparent black dots.
crp_channel_names <- function(coord_table, channel_annotation) {
  switch(
    channel_annotation,
    "number" = coord_table$Electrode,
    "short" = coord_table$ShortLabel,
    "label" = coord_table$Label,
    "full" = sprintf("%s (%s)", coord_table$Electrode, coord_table$Label)
  )
}

plot_crp_by_channel_heatmap <- function(
    crp_by_channel, space = 0.995, space_mode = c("quantile", "absolute"),
    time_range = c(NA, NA),
    channel_annotation = c("number", "short", "label", "full"),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, col = NULL, ...) {

  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation)

  group_indexes <- crp_by_channel$group_indexes
  sample_rate <- crp_by_channel$sample_rate
  time_points <- crp_by_channel$time_points
  coord_table <- crp_by_channel$coord_table
  group_labels <- crp_by_channel$group_labels
  n_groups <- crp_by_channel$n

  c_full <- crp_by_channel$data$canonical
  onset <- crp_by_channel$data$onset
  offset <- crp_by_channel$data$offset
  n_channels <- dim(c_full)[[2]]

  space <- get_spacing(c_full, space = space, space_mode = space_mode)

  time_info <- get_time_range(time_points, time_range = time_range)
  time_range <- time_info$time_range

  channel_names <- crp_channel_names(coord_table, channel_annotation)
  is_analysis <- coord_table$Electrode %in% crp_by_channel$analysis_electrodes

  if (length(col) == 0) {
    col <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
             "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
  }
  if (length(col) < 101) {
    col <- grDevices::colorRampPalette(col)(101)
  }

  # Decide the subplots
  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  par_opt <- prepare_par(cex = cex)
  mar <- par_opt$mar
  lmat <- matrix(seq_len(prod(mfrow)), nrow = mfrow[[1]], byrow = TRUE)
  lmat <- cbind(lmat + mfrow[[1]], seq_len(mfrow[[1]]))

  graphics::layout(lmat, widths = c(rep(1, mfrow[[2]]), graphics::lcm(3)))

  graphics::par(mar = c(mar[[1]], 3.5, mar[[3]], mar[[4]]), cex = 1)
  add_heatmap_legend(vlim = c(-space, space), col = col, cex = cex)

  graphics::par(mar = mar, cex = 1)

  for (ii in seq_len(n_groups)) {

    z <- array(c_full[, , ii], dim = c(length(time_points), n_channels))
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

    graphics::axis(
      side = 2L, at = seq_len(n_channels), labels = channel_names, las = 1,
      tck = -0.005 * (3 + cex), cex = cex,
      cex.axis = par_opt$cex.axis * cex)

    # Highlight analysis electrodes with thicker, longer ticks
    if (any(is_analysis)) {
      graphics::axis(
        side = 2L, at = which(is_analysis), labels = FALSE,
        lwd = 0, lwd.ticks = 2.5, tck = -0.005 * (3 + cex),
        cex.axis = par_opt$cex.axis * cex)
    }

    add_vertical_marks(vertical_marks, col = "black", lty = 1)

    if (isTRUE(crp)) {
      graphics::points(onset[1, , ii], seq_len(n_channels), pch = 20, col = "#00000060")
      graphics::points(offset[1, , ii], seq_len(n_channels), pch = 20, col = "#00000060")
    }

    group <- crp_by_channel$groups[[ii]]
    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par_opt$cex.main * cex
    )

  }

}


plot_crp_by_channel_multilines <- function(
    crp_by_channel, space = 0.995, space_mode = c("quantile", "absolute"),
    time_range = c(NA, NA),
    channel_annotation = c("number", "short", "label", "full"),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, ...) {

  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation)

  group_indexes <- crp_by_channel$group_indexes
  sample_rate <- crp_by_channel$sample_rate
  time_points <- crp_by_channel$time_points
  coord_table <- crp_by_channel$coord_table
  group_labels <- crp_by_channel$group_labels
  n_groups <- crp_by_channel$n

  c_full <- crp_by_channel$data$canonical
  onset <- crp_by_channel$data$onset
  offset <- crp_by_channel$data$offset
  n_channels <- dim(c_full)[[2]]

  space <- get_spacing(c_full, space = space, space_mode = space_mode)
  vspace <- space * 2

  time_info <- get_time_range(time_points, time_range = time_range)
  time_range <- time_info$time_range

  channel_names <- crp_channel_names(coord_table, channel_annotation)
  is_analysis <- coord_table$Electrode %in% crp_by_channel$analysis_electrodes
  # Highlight analysis electrodes with a thicker line
  channel_lwd <- ifelse(is_analysis, 2, 1)

  # Decide the subplots
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

    c_sub <- array(c_full[, , ii], dim = c(length(time_points), n_channels))

    ravetools::plot_signals(
      signals = t(c_sub),
      sample_rate = sample_rate,
      start_time = time_info$start_time,
      duration = time_info$duration,
      time_shift = time_info$time_shift,
      space = vspace,
      space_mode = "absolute",
      channel_names = channel_names,
      ylab = "",
      xlim = time_range,
      lwd = channel_lwd,
      cex = cex
    )

    add_vertical_marks(vertical_marks)

    if (isTRUE(crp)) {
      onset_t <- onset[1, , ii]
      offset_t <- offset[1, , ii]
      onset_y <- vapply(seq_len(n_channels), function(ch) {
        interp_on_trace(onset_t[[ch]], c_sub[, ch])
      }, numeric(1))
      offset_y <- vapply(seq_len(n_channels), function(ch) {
        interp_on_trace(offset_t[[ch]], c_sub[, ch])
      }, numeric(1))
      graphics::points(onset_t, ybase + onset_y, pch = 20, col = "#00000060")
      graphics::points(offset_t, ybase + offset_y, pch = 20, col = "#00000060")
    }

    group <- crp_by_channel$groups[[ii]]
    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par_opt$cex.main * cex
    )

  }

}
