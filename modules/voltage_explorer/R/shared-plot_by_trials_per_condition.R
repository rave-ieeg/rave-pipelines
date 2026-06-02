#' Plot individual trials, groupped by stimuli or ordered by trial number
#' Each figure is a condition group
plot_by_trials_per_condition_heatmap <- function(
    data_by_trial_channel_condition, sort_by = c("stimuli", "trial"),
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, col = NULL, ...) {

  space_mode <- match.arg(space_mode)
  sort_by <- match.arg(sort_by)

  crp_enabled <- isTRUE(data_by_trial_channel_condition$crp_enabled & crp)

  # DIPSAUS DEBUG START
  # sort_by <- "trial"
  # group <- data_by_trial_channel_condition$groups[[1]]
  # ii <- 1
  # space_mode <- "quantile"
  # cex = 1
  # space = 1
  # time_range = c(NA, NA)
  # crp = TRUE
  # mfrow=NULL
  # vertical_marks = 0
  # col = NULL
  # sort_by <- "stimuli"

  group_indexes <- data_by_trial_channel_condition$group_indexes
  sample_rate <- data_by_trial_channel_condition$sample_rate
  time_points <- data_by_trial_channel_condition$time_points
  coord_table <- data_by_trial_channel_condition$coord_table
  group_labels <- data_by_trial_channel_condition$group_labels
  n_groups <- data_by_trial_channel_condition$n
  space <- get_spacing(
    lapply(data_by_trial_channel_condition$data, "[[", "voltage"),
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

  # Decide the subplots
  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  max_left_margin <- max(strwidth(data_by_trial_channel_condition$unique_conditions,
                                  units = "inches", cex = cex * 0.85))
  max_left_margin <- max_left_margin + strwidth(" ", units = "inches") * 2

  par_opt <- prepare_par(cex = cex)
  mar <- par_opt$mar
  lmat <- matrix(seq_len(prod(mfrow)), nrow = mfrow[[1]], byrow = TRUE)
  lmat <- cbind(lmat + mfrow[[1]], seq_len(mfrow[[1]]))

  if (par_opt$mai[[2]] < max_left_margin) {
    mar[[2]] <- mar[[2]] / par_opt$mai[[2]] * max_left_margin + 0.1
  }
  graphics::layout(lmat, widths = c(rep(1, mfrow[[2]]), graphics::lcm(3)))

  graphics::par(mar = c(mar[[1]], 3.5, mar[[3]], mar[[4]]), cex = 1)

  add_heatmap_legend(vlim = c(-space, space), col = col, cex = cex)

  graphics::par(mar = mar, cex = 1)

  for (ii in seq_len(n_groups)) {

    group <- data_by_trial_channel_condition$groups[[ii]]
    group_data <- data_by_trial_channel_condition$data[[ii]]

    # col2 <- ifelse(group$trials_included %in% group_data$bad_trials, "#BEBEBE40", "#BEBEBE80")

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

    add_vertical_marks(vertical_marks)
    if (crp_enabled) {
      add_vertical_marks(group_data$crp_result$tau_R)
    }

    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par("cex.main") * cex,
    )

  }

}


plot_by_trials_per_condition_multilines <- function(
    data_by_trial_channel_condition, sort_by = c("stimuli", "trial"),
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, ...) {

  space_mode <- match.arg(space_mode)
  sort_by <- match.arg(sort_by)

  crp_enabled <- isTRUE(data_by_trial_channel_condition$crp_enabled & crp)

  # DIPSAUS DEBUG START
  # sort_by <- "trial"
  # group <- data_by_trial_channel_condition$groups[[1]]
  # ii <- 1
  # space_mode <- "quantile"
  # cex = 1
  # space = 1
  # time_range = c(NA, NA)
  # crp = TRUE
  # mfrow=NULL
  # vertical_marks = 0
  # col = NULL
  # sort_by <- "stimuli"

  group_indexes <- data_by_trial_channel_condition$group_indexes
  sample_rate <- data_by_trial_channel_condition$sample_rate
  time_points <- data_by_trial_channel_condition$time_points
  coord_table <- data_by_trial_channel_condition$coord_table
  group_labels <- data_by_trial_channel_condition$group_labels
  n_groups <- data_by_trial_channel_condition$n
  space <- get_spacing(
    lapply(data_by_trial_channel_condition$data, "[[", "voltage"),
    space = space,
    space_mode = space_mode
  )

  time_info <- get_time_range(time_points, time_range = time_range)
  time_range <- time_info$time_range

  # Decide the subplots
  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  par_opt <- prepare_par(mfrow = mfrow, cex = cex)

  for (ii in seq_len(n_groups)) {

    group <- data_by_trial_channel_condition$groups[[ii]]
    group_data <- data_by_trial_channel_condition$data[[ii]]

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
      sample_rate = sample_rate,
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

    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par("cex.main") * cex,
    )

  }

}


