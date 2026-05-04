
plot_by_channel_condition <- function(
    data_by_channel_condition, group_by = c("condition", "channel"),
    space = 1, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    channel_annotation = c("number", "short", "label", "full"), cex = 1,
    mfrow = NULL, vertical_marks = 0) {
  group_by <- match.arg(group_by)
  space_mode <- match.arg(space_mode)
  channel_annotation <- match.arg(channel_annotation)

  # DIPSAUS DEBUG START
  # group_by <- "channel"
  # group <- data_by_channel_condition$groups[[1]]
  # ii <- 1
  # channel_annotation <- "full"
  # cex = 1

  if (space_mode == "quantile") {
    if (!isTRUE(space > 0 && space < 1)) {
      space <- 1.0
    }
    space <- stats::quantile(data_by_channel_condition$data, probs = space, na.rm = TRUE) * 2
    space_mode <- "absolute"
  } else {
    space <- abs(space)
    if (!isTRUE(space > 0)) {
      space <- max(data_by_channel_condition$data, na.rm = TRUE) * 2
    }
  }
  space <- unname(space)

  group_indexes <- data_by_channel_condition$group_indexes
  sample_rate <- data_by_channel_condition$sample_rate
  time_points <- data_by_channel_condition$time_points
  coord_table <- data_by_channel_condition$coord_table
  group_labels <- data_by_channel_condition$group_labels
  n_groups <- data_by_channel_condition$n

  time_shift <- min(time_points, na.rm = TRUE)
  start_time <- time_range[[1]] - time_shift
  if (is.na(start_time)) {
    start_time <- 0
  }
  end_time <- time_range[[2]]
  if (is.na(end_time)) {
    end_time <- max(time_points, na.rm = TRUE)
  }
  end_time <- end_time - time_shift
  duration <- end_time - start_time
  time_range <- c(start_time, end_time) + time_shift

  channel_names <- switch (
    channel_annotation,
    "number" = coord_table$Electrode,
    "short" = coord_table$ShortLabel,
    "label" = coord_table$Label,
    "full" = sprintf("%s (%s)", coord_table$Electrode, coord_table$Label)
  )

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  xline <- 1.5 * cex
  yline <- 1 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1

  switch (
    group_by,
    "condition" = {
      if (length(coord_table$LeadChannel) && is.logical(coord_table$LeadChannel)) {
        hlines_chs <- which(coord_table$LeadChannel) - 0.5
        hlines_chs <- hlines_chs[hlines_chs >= 1]
      } else if (nrow(coord_table) > 5) {
        hlines_chs <- seq(0.5, nrow(coord_table) - 0.5, by = 5)
      } else {
        hlines_chs <- numeric(0L)
      }


      if (length(mfrow) != 2) {
        mfrow <- c(1, n_groups)
      }

      oldpar <- graphics::par(mar = c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1,
                              mfrow = mfrow)
      on.exit({
        graphics::par(oldpar)
      })

      # plot!
      for (ii in seq_along(group_indexes)) {
        data_time_by_channel <- data_by_channel_condition$data[, , ii, drop = FALSE]
        dim(data_time_by_channel) <- dim(data_time_by_channel)[c(1, 2)]
        group <- data_by_channel_condition$groups[[ii]]
        ylim <- range(data_time_by_channel, na.rm = TRUE)

        ravetools::plot_signals(
          t(data_time_by_channel),
          sample_rate = sample_rate,
          space = space,
          space_mode = space_mode,
          time_shift = time_shift,
          start_time = start_time,
          duration = duration,
          ylab = "",
          channel_names = channel_names,
          main = bquote(
            .(group$label) ~ scriptstyle(
              "(" *
                .(round(ylim[[1]])) ~ "~" ~ .(round(ylim[[2]])) ~ mu*V ~
                ", n =" ~ .(group$n_trials) *
              ")"
            )
          ),
          adj = 0,
          cex = cex
        )

        if (length(vertical_marks)) {
          graphics::abline(v = vertical_marks, col = "#80808040", lty = 2)
        }

        if (length(hlines_chs)) {
          graphics::abline(h = hlines_chs * space, col = "#80808040", lty = 3)
        }

        if (ii == 1) {
          graphics::arrows(time_range[[2]], 0.5 * space, time_range[[2]], 1.5 * space,
                           code = 3L, angle = 20, length = 0.05, col = "#808080", lwd = 2)
          graphics::text(
            time_range[[2]], space * 1.5,
            cex = cex * 0.8, pos = 2L, offset = 0.2,
            labels = bquote(.(sprintf("\u00B1%.0f", space / 2)) ~ mu * V)
          )
        }
      }
    },
    "channel" = {
      n_channels <- nrow(coord_table)
      if (length(mfrow) != 2) {
        mfrow <- n2mfrow(n_channels, asp = 2)
      }

      oldpar <- graphics::par(mar = c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1,
                              mgp = cex * c(2, 0.5, 0),
                              cex.lab = 1,
                              mfrow = mfrow)
      on.exit({
        graphics::par(oldpar)
      })


      # plot!
      for (ii in seq_len(n_channels)) {
        data_time_by_group <- data_by_channel_condition$data[, ii, , drop = FALSE]
        dim(data_time_by_group) <- dim(data_time_by_group)[c(1, 3)]
        ylim <- range(data_time_by_group, na.rm = TRUE)

        graphics::matplot(
          x = time_points, y = data_time_by_group, lty = 1, type = "l", lwd = 1,
          xlab = "", ylab = "", axes = FALSE, xaxs = "i",
          xlim = time_range, ylim = c(-0.5, 0.5) * space,
          cex = cex, cex.main = par_opt$cex.main * cex,
          cex.lab = par_opt$cex.lab *  cex, cex.axis = par_opt$cex.axis * cex,
          tck = tck, adj = 0,
          main = bquote(
            .(sprintf("Ch %s", channel_names[[ii]])) ~
              scriptstyle(
                "("~ .(round(ylim[[1]])) ~ "~" ~ .(round(ylim[[2]])) ~ mu*V ~ ")"
              )
          ),
        )


        graphics::mtext(side = 1, text = "Time (s)", line = xline,
                        cex = par_opt$cex.lab * cex)

        graphics::axis(side = 1L, at = pretty(time_range),
                       las = 1, tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
                       cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)

        graphics::axis(
          side = 2L, at = c(-0.5, 0, 0.5) * space, las = 1,
          labels = c(sprintf("%.0f", space * -0.5), "0", sprintf("%.0f", space * 0.5)),
          tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
          cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)


        graphics::abline(v = vertical_marks, col = "#80808040", lty = 2)
        graphics::abline(h = 0, col = "#80808040", lty = 2)

        if (ii %% mfrow[[2]] == 0) {
          # legend
          graphics::legend(
            "topright",
            sprintf("%s, n=%d", group_labels, data_by_channel_condition$n_trials),
            lty = 1,
            col = seq_along(group_labels),
            bty = "n",
            cex = cex * 0.8
          )
        }
      }
    }
  )

  invisible()

}


plot_by_trial_per_condition <- function(
    data_by_trial_per_condition, sort_by = c("stimli", "channel"),
    space = 0.995, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    cex = 1, mfrow = NULL, vertical_marks = 0, col = NULL) {

  sort_by <- match.arg(sort_by)
  space_mode <- match.arg(space_mode)

  # DIPSAUS DEBUG START
  # sort_by <- "stimli"
  # group <- data_by_trial_per_condition$groups[[1]]
  # ii <- 1
  # space_mode <- "quantile"
  # cex = 1
  # space = 1
  # time_range = c(NA, NA)

  if (length(col) == 0) {
    col <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
             "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
  }
  if (length(col) < 101) {
    col <- grDevices::colorRampPalette(col)(101)
  }

  if (space_mode == "quantile") {
    if (!isTRUE(space > 0 && space < 1)) {
      space <- 1.0
    }
    space <- stats::quantile(unlist(data_by_trial_per_condition$data),
                             probs = space, na.rm = TRUE) * 2
    space_mode <- "absolute"
  } else {
    space <- abs(space)
    if (!isTRUE(space > 0)) {
      space <- max(unlist(data_by_trial_per_condition$data), na.rm = TRUE) * 2
    }
  }
  space <- unname(space)

  group_indexes <- data_by_trial_per_condition$group_indexes
  sample_rate <- data_by_trial_per_condition$sample_rate
  time_points <- data_by_trial_per_condition$time_points
  coord_table <- data_by_trial_per_condition$coord_table
  group_labels <- data_by_trial_per_condition$group_labels
  n_groups <- data_by_trial_per_condition$n

  # start_time & end_time are relative to time_shift
  time_shift <- min(time_points, na.rm = TRUE)
  start_time <- time_range[[1]] - time_shift
  if (is.na(start_time)) {
    start_time <- 0
  }
  end_time <- time_range[[2]]
  if (is.na(end_time)) {
    end_time <- max(time_points, na.rm = TRUE)
  }
  end_time <- end_time - time_shift
  duration <- end_time - start_time
  time_range <- c(start_time, end_time) + time_shift

  # Plot options
  if (length(mfrow) != 2) {
    mfrow <- n2mfrow(n_groups, asp = 3)
  }

  max_left_margin <- max(strwidth(data_by_trial_per_condition$unique_conditions,
                                  units = "inches", cex = cex * 0.85))
  max_left_margin <- max_left_margin + strwidth(" ", units = "inches") * 2
  mar <- c(3.1, 2.1, 2.1, 0.2) * (0.25 + cex * 0.75) + 0.1

  oldpar <- graphics::par(mar = mar,
                          mgp = cex * c(2, 0.5, 0),
                          cex.lab = 1,
                          mfrow = mfrow)

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))

  if (par_opt$mai[[2]] < max_left_margin) {
    mar[[2]] <- mar[[2]] / par_opt$mai[[2]] * max_left_margin + 0.1
    graphics::par(mar = mar)
  }
  xline <- 1.5 * cex
  yline <- 1 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1

  on.exit({
    graphics::par(oldpar)
  })


  for (ii in seq_along(group_indexes)) {
    data_time_by_trial <- data_by_trial_per_condition$data[[ii]]
    group <- data_by_trial_per_condition$groups[[ii]]

    data_time_by_trial[data_time_by_trial < -space / 2] <- -space / 2
    data_time_by_trial[data_time_by_trial > space / 2] <- space / 2

    graphics::image(
      x = time_points,
      y = seq_len(group$n_trials),
      z = data_time_by_trial,
      axes = FALSE,
      xlab = "",
      ylab = "",
      xlim = time_range, cex = cex,
      zlim = c(-space, space),
      col = col
    )

    graphics::mtext(side = 1, text = "Time (s)", line = xline,
                    cex = par_opt$cex.lab * cex)

    graphics::axis(side = 1L, at = pretty(time_range),
                   las = 1, tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
                   cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)

    separators <- cumsum(c(0, group$trial_count)) + 0.5
    graphics::axis(
      side = 2L, at = separators, las = 1,
      labels = rep("", length(group$trial_count) + 1),
      tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)

    graphics::axis(
      side = 2L, at = cumsum(group$trial_count) + 0.5 - group$trial_count / 2,
      las = 1, labels = group$conditions,
      tck = 0, cex = cex, cex.axis = 0.85)


    if (length(vertical_marks)) {
      graphics::abline(v = vertical_marks, col = "#808080", lty = 3)
    }

    if (length(separators) > 1) {
      graphics::abline(h = separators[- c(1)],
                       col = "#808080", lty = 1)
    }
  }

}

plot_collapse_by_condition <- function(
    data_collapse_by_condition, crp_decoration = TRUE, col = NULL, flip_y = FALSE,
    vertical_marks = 0, time_range = c(NA, NA)) {

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

  if (is.null(col)) {
    col <- seq_len(max_group_idx)
  } else if (length(col) < max_group_idx) {
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
    main = "Mean response by condition", adj = 0, axes = FALSE)

  graphics::mtext(side = 1L, "Time (s)", line = 2)
  graphics::mtext(side = 2L, bquote("Voltage" ~ (mu*V)), line = 2)

  graphics::axis(1L, pretty(time_range))
  graphics::axis(2L, c(data_range, 0), labels = c(sprintf("%.0f", data_range), "0"), las = 1)

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
                     labels = bquote(tau[CRP] * "=" * .(sprintf("%.2f", tau)) ~ "s"), cex = 0.8, adj = c(0.5, 0))
    })
  }

  graphics::legend("topright", data_collapse_by_condition$group_labels, lty = 1, col = col, bty = "n", cex = 0.9, ncol = 2L)


}


# plot generics
plot.data_by_channel_condition <- function(x, ...) {
  plot_by_channel_condition(data_by_channel_condition = x, ...)
}

plot.data_by_trial_per_condition <- function(x, ...) {
  plot_by_trial_per_condition(data_by_trial_per_condition = x, ...)
}

plot.data_collapse_by_condition <- function(x, ...) {
  plot_collapse_by_condition(data_collapse_by_condition = x, ...)
}
