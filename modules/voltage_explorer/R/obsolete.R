# Retired code, kept for reference only.
#
# The pipeline sources `R/shared-*.R` and nothing else, so anything parked here is
# invisible to the pipeline, to the report, and to `plot()` dispatch via
# `ravepipeline::pipeline_plot_data`. The Shiny app sources every `R/*.R`, so these
# are still *defined* in the app -- dead, but present. Do not add new callers.


# ---- plot_collapse_by_condition ----------------------------------------------
# Retired: plots a `data_collapse_by_condition` container that no producer in this
# module has ever created. The "collapse over channels" figure it drew is the one
# case the electrode-mask refactor deliberately removed -- averaging unrelated
# channels together is not a meaningful summary.
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
    pal <- use_discrete_colormap()
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


# ---- plot.data_collapse_by_condition -----------------------------------------
# Retired: dispatch for the above.
plot.data_collapse_by_condition <- function(x, ...) {
  plot_collapse_by_condition(data_collapse_by_condition = x, ...)
}


# ---- filter_electrodes -------------------------------------------------------
# Retired: this cleaned the old `analysis_electrodes` input into
# `analysis_electrodes_clean`, a target that no longer exists. Channel selection is
# now a plot-time mask; see `resolve_channel_selection()` in `shared-helpers.R`.
filter_electrodes <- function(repository, electrodes, type = "LFP", strict = TRUE) {
  available_electrodes <- repository$electrode_list
  if (missing(electrodes)) {
    electrodes <- available_electrodes
  } else {
    electrodes <- ravecore:::parse_svec(unlist(electrodes))
  }
  # Now filter by type
  available_electrodes2 <- repository$subject$electrodes[repository$subject$electrode_types %in% type]
  available_electrodes <- intersect(available_electrodes, available_electrodes2)

  electrodes <- electrodes[electrodes %in% available_electrodes]
  electrodes <- sort(as.integer(electrodes))

  if (strict && !length(electrodes)) {
    stop(sprintf(
      "No electrode channels selected filtered matching type %s. Please specify the electrodes from the following loaded: %s",
      paste(sprintf("`%s`", type), collapse = ", "),
      ravecore:::deparse_svec(available_electrodes)
    ))
  }

  electrodes
}
