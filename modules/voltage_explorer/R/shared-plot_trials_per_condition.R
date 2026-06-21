
# plot trials per condition (collapsing channels)
plot_trials_per_condition <- function(
    data_by_trial_channel_condition,
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    cex = 1, crp = TRUE, mfrow = NULL, vertical_marks = 0, ...) {

  space_mode <- match.arg(space_mode)

  crp_enabled <- isTRUE(data_by_trial_channel_condition$crp_enabled & crp)

  # DIPSAUS DEBUG START
  # sort_by <- "trial"
  # group <- data_by_trial_channel_condition$groups[[1]]
  # ii <- 1
  # space_mode <- "quantile"
  # cex = 1
  # space = 1
  # time_range = c(NA, NA)
  # type = "butterfly"
  # crp = TRUE
  # mfrow=NULL
  # vertical_marks = 0

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
  ylim <- c(-space, space)

  time_info <- get_time_range(time_points, time_range = time_range)
  time_range <- time_info$time_range

  # Decide the subplots
  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  prepare_par(mfrow = mfrow, cex = cex)

  for (ii in seq_len(n_groups)) {

    group <- data_by_trial_channel_condition$groups[[ii]]
    group_data <- data_by_trial_channel_condition$data[[ii]]

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

    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par("cex.main") * cex,
    )

  }

}


