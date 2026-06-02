



# Overall plot: (collapse condition AND channels)
plot_collapse_by_condition <- function(
    data_collapse_by_condition, crp_decoration = TRUE, col = NULL, flip_y = FALSE,
    vertical_marks = 0, time_range = c(NA, NA), cex = 1) {

  oldpar <- graphics::par("mar" = c(5.1, 4.1, 2.1, 2.1), mfrow = c(1, 1), cex = 1)
  on.exit({ graphics::par(oldpar) })

  groups <- data_collapse_by_condition$groups
  group_indexes <- data_collapse_by_condition$group_indexes
  time_points <- data_collapse_by_condition$time_points

  if (is.na(time_range[[1]])) {
    time_range[[1]] <- min(time_points, na.rm = TRUE)
  }
  if (is.na(time_range[[2]])) {
    time_range[[2]] <- max(time_points, na.rm = TRUE)
  }

  max_group_idx <- max(group_indexes)

  if (!length(col)) {
    pal <- use_discrete_palette()
    col <- pal$colors
  }
  if (length(col) < max_group_idx) {
    col <- rep(col, ceiling(max_group_idx / length(col)))
  }
  col <- col[group_indexes]

  mean_erp <- data_collapse_by_condition$data$mean_erp
  data_range <- range(mean_erp, na.rm = TRUE)
  if (flip_y) {
    ylim <- rev(data_range)
  } else {
    ylim <- data_range
  }
  graphics::matplot(
    x = time_points, y = mean_erp, type = "l", lty = 1, col = col, xaxs = "i",
    ylim = ylim, xlab = "", ylab = "", xlim = time_range,
    main = "", adj = 0, axes = FALSE, cex = cex, cex.main = cex * 1.2)

  graphics::mtext(side = 1L, "Time (s)", line = 1.5 + 0.5 * cex, cex = cex)
  graphics::mtext(
    side = 2L,
    bquote("Voltage" ~ (mu * V)),
    line = 2,
    cex = cex
  )

  graphics::axis(1L, pretty(time_range), cex.axis = cex)
  graphics::axis(2L, c(data_range, 0), labels = c(sprintf("%.0f", data_range), "0"), las = 1, cex.axis = cex)

  graphics::abline(v = vertical_marks, h = 0, col = "#80808080", lty = 2)

  if (!isFALSE(crp_decoration) && isTRUE(data_collapse_by_condition$data$crp_enabled)) {
    crp_tau <- data_collapse_by_condition$data$crp_tau
    lapply(seq_along(crp_tau), function(ii) {
      tau <- crp_tau[[ii]]
      if (length(tau) != 3) { return() }
      tau <- tau[[2]]
      idx <- which.min(abs(time_points - tau))
      val <- mean_erp[idx, ii]
      graphics::points(x = tau, y = val, col = col[[ii]], pch = 16)
      segments(x0 = tau, y0 = ylim[[1]], x1 = tau, y1 = val, col = adjustcolor(col[[ii]], alpha.f = 0.5), lty = 3)
      graphics::text(x = tau, y = ylim[[1]] + (ii %% 2) * ifelse(flip_y, -10, 10), col = col[[ii]],
                     labels = bquote(tau[CRP] * "=" * .(sprintf("%.2f", tau)) ~ "s"), cex = 0.8 * cex, adj = c(0.5, 0))
    })
  }

  graphics::legend("topright", data_collapse_by_condition$group_labels, lty = 1, col = col, bty = "n", cex = 0.9 * cex, ncol = 2L)


}


# plot generics
plot.data_by_channel_condition <- function(x, ...) {
  plot_by_channel_condition(data_by_channel_condition = x, ...)
}

plot.data_collapse_by_condition <- function(x, ...) {
  plot_collapse_by_condition(data_collapse_by_condition = x, ...)
}

plot.data_by_trial_channel_condition <- function(
    x, type = c("collapse_trial", "by_trial_line", "by_trial_heatmap"), ...) {
  type <- match.arg(type)

  switch (
    type,
    "by_trial_heatmap" = {
      plot_by_trials_per_condition_heatmap(data_by_trial_channel_condition = x, ...)
    },
    "collapse_trial" = {
      plot_trials_per_condition(data_by_trial_channel_condition = x, ...)
    },
    {
      plot_by_trials_per_condition_multilines(data_by_trial_channel_condition = x, ...)
    }
  )
}
