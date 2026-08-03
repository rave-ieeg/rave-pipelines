# Single-channel voltage across trials, one panel per condition group.
#
# `x$data[[ii]]$voltage` is a time x trial matrix for condition group `ii`, built
# for the one channel in `x$electrode` -- there is no channel dimension and hence
# no `electrode_mask` here. Use `prepare_data_by_trial_channel_condition()` with a
# different `electrode` to look at another channel.
#
# Three renderings:
#   butterfly  every trial as a faint line, with the mean drawn on top
#   multiline  trials stacked as offset traces
#   heatmap    time x trial image


# Panel title: condition label, trial count, and the channel this was built for.
plot_data_by_trial_channel_condition_title <- function(x, group, cex = 1) {
  label <- group$label
  chan <- x$coord_table$Label[x$coord_table$Electrode %in% x$electrode]
  if (length(x$electrode) == 1) {
    label <- if (length(chan) == 1 && !is.na(chan) && nzchar(chan)) {
      sprintf("%s | Ch %s (%s)", label, x$electrode, chan)
    } else {
      sprintf("%s | Ch %s", label, x$electrode)
    }
  }
  graphics::title(
    main = bquote(.(label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
    adj = 0,
    cex.main = par("cex.main") * cex
  )
}


# Every trial as a faint line, mean on top.
plot_data_by_trial_channel_condition_butterfly <- function(
    x, space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, ...) {

  space_mode <- match.arg(space_mode)

  crp_enabled <- isTRUE(x$crp_enabled & crp)

  time_points <- x$time_points
  n_groups <- x$n
  space <- get_spacing(
    lapply(x$data, "[[", "voltage"),
    space = space,
    space_mode = space_mode
  )
  ylim <- c(-space, space)

  time_info <- get_time_range(time_points, time_range = time_range)
  time_range <- time_info$time_range

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)
  prepare_par(mfrow = mfrow, cex = cex)

  for (ii in seq_len(n_groups)) {

    group <- x$groups[[ii]]
    group_data <- x$data[[ii]]

    col2 <- ifelse(group$trials_included %in% group_data$bad_trials, "#BEBEBE80", "#BEBEBE40")
    graphics::matplot(
      x = time_points,
      y = group_data$voltage,
      type = "l",
      col = col2,
      lty = 1,
      lwd = 0.5,
      ylim = ylim,
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = time_range,
      ...
    )
    add_axis_time(time_range = time_range, cex = cex)
    add_axis_voltage(value_range = c(-space, space), cex = cex)
    add_vertical_marks(vertical_marks)

    graphics::lines(x = time_points, y = group_data$mean, lwd = 1)

    if (crp_enabled) {
      add_crp_decorators(group_data$crp_result)
    }

    plot_data_by_trial_channel_condition_title(x, group, cex = cex)

  }

  invisible()
}


# Trials stacked as offset traces, grouped by stimuli or ordered by trial number.
plot_data_by_trial_channel_condition_multiline <- function(
    x, sort_by = OPTIONS_TRIAL_SORT,
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, ...) {

  space_mode <- match.arg(space_mode)
  sort_by <- match.arg(sort_by, choices = OPTIONS_TRIAL_SORT)

  crp_enabled <- isTRUE(x$crp_enabled & crp)

  n_groups <- x$n
  space <- get_spacing(
    lapply(x$data, "[[", "voltage"),
    space = space,
    space_mode = space_mode
  )

  time_info <- get_time_range(x$time_points, time_range = time_range)
  time_range <- time_info$time_range

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)
  prepare_par(mfrow = mfrow, cex = cex)

  for (ii in seq_len(n_groups)) {

    group <- x$groups[[ii]]
    group_data <- x$data[[ii]]

    col_fg <- par("fg")
    col2 <- ifelse(group$trials_included %in% group_data$bad_trials, "#DF536B80", col_fg)

    data_time_by_trial <- group_data$voltage

    if (sort_by == "trial") {
      odr <- order(group$trials_included)
      data_time_by_trial <- data_time_by_trial[, odr, drop = FALSE]
      col2 <- col2[odr]
    }

    plot.new()
    plot.window(
      xlim = time_range,
      ylim = c(0, space * (2 * ncol(data_time_by_trial) + 2)), yaxs = "i"
    )
    ravetools::plot_signals(
      signals = t(data_time_by_trial),
      sample_rate = x$sample_rate,
      start_time = time_info$start_time,
      duration = time_info$duration,
      time_shift = time_info$time_shift,
      space = space * 2,
      space_mode = "absolute",
      channel_names = NULL,
      ylab = "",
      col = col2,
      new_plot = FALSE
    )

    add_axis_time(time_range = time_range, cex = cex)

    if (sort_by == "trial") {
      add_axis_trial_number(group, vspace = space * 2, cex = cex)
    } else {
      add_axis_trial_stimuli(group, vspace = space * 2, lty = 3, col = "#80808030", cex = cex)
    }

    add_vertical_marks(vertical_marks)
    if (crp_enabled) {
      add_vertical_marks(group_data$crp_result$tau_R)
    }

    plot_data_by_trial_channel_condition_title(x, group, cex = cex)

  }

  invisible()
}


# Time x trial image, one panel per condition group.
plot_data_by_trial_channel_condition_heatmap <- function(
    x, sort_by = OPTIONS_TRIAL_SORT,
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, col = NULL, ...) {

  space_mode <- match.arg(space_mode)
  sort_by <- match.arg(sort_by, choices = OPTIONS_TRIAL_SORT)

  crp_enabled <- isTRUE(x$crp_enabled & crp)

  time_points <- x$time_points
  n_groups <- x$n
  space <- get_spacing(
    lapply(x$data, "[[", "voltage"),
    space = space,
    space_mode = space_mode
  )

  time_info <- get_time_range(time_points, time_range = time_range)
  time_range <- time_info$time_range

  if (length(col) == 0) {
    col <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
             "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
  }
  if (length(col) < 101) {
    col <- grDevices::colorRampPalette(col)(101)
  }

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  # Widen the left margin enough for the longest condition name
  max_left_margin <- max(strwidth(x$unique_conditions, units = "inches", cex = cex * 0.85))
  max_left_margin <- max_left_margin + strwidth(" ", units = "inches") * 2

  par_opt <- prepare_par(cex = cex)
  mar <- par_opt$mar
  if (par_opt$mai[[2]] < max_left_margin) {
    mar[[2]] <- mar[[2]] / par_opt$mai[[2]] * max_left_margin + 0.1
  }

  # Reserve the last column for one colour bar per panel row
  lmat <- matrix(seq_len(prod(mfrow)), nrow = mfrow[[1]], byrow = TRUE)
  lmat <- cbind(lmat + mfrow[[1]], seq_len(mfrow[[1]]))
  graphics::layout(lmat, widths = c(rep(1, mfrow[[2]]), graphics::lcm(3)))

  graphics::par(mar = c(mar[[1]], 3.5, mar[[3]], mar[[4]]), cex = 1)
  for (ii in seq_len(mfrow[[1]])) {
    add_heatmap_legend(vlim = c(-space, space), col = col, cex = cex)
  }

  graphics::par(mar = mar, cex = 1)

  for (ii in seq_len(n_groups)) {

    group <- x$groups[[ii]]
    group_data <- x$data[[ii]]

    data_time_by_trial <- group_data$voltage
    data_time_by_trial[data_time_by_trial < -space] <- -space
    data_time_by_trial[data_time_by_trial > space] <- space

    if (sort_by == "trial") {
      data_time_by_trial <- data_time_by_trial[, order(group$trials_included), drop = FALSE]
    }

    graphics::image(
      x = time_points,
      y = seq_len(group$n_trials),
      z = data_time_by_trial,
      axes = FALSE,
      xlab = "",
      ylab = if (sort_by == "trial") "Trial Number" else "",
      xlim = time_range, cex = cex,
      zlim = c(-space, space),
      main = "",
      cex.main = cex * par_opt$cex.main,
      adj = 0,
      col = col,
      cex.lab = cex * par_opt$cex.lab
    )

    add_axis_time(time_range = time_range, cex = cex)

    if (sort_by == "trial") {
      add_axis_trial_number(group, cex = cex)
    } else {
      add_axis_trial_stimuli(group, cex = cex)
    }

    add_vertical_marks(vertical_marks, col = "black", lty = 1)
    if (crp_enabled) {
      add_vertical_marks(group_data$crp_result$tau_R, lty = 1)
    }

    plot_data_by_trial_channel_condition_title(x, group, cex = cex)

  }

  invisible()
}


plot.data_by_trial_channel_condition <- function(
    x, type = c("butterfly", "multiline", "heatmap"), ...) {
  type <- match.arg(type)
  switch(
    type,
    "heatmap" = plot_data_by_trial_channel_condition_heatmap(x = x, ...),
    "multiline" = plot_data_by_trial_channel_condition_multiline(x = x, ...),
    plot_data_by_trial_channel_condition_butterfly(x = x, ...)
  )
}
