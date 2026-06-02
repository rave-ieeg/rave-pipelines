# Plot collapse trials (by channels per condition or by condition per channels)
plot_by_channel_condition <- function(
    data_by_channel_condition, group_by = c("condition", "channel"),
    space = 1, space_mode = c("quantile", "absolute"), time_range = c(NA, NA),
    channel_annotation = c("number", "short", "label", "full"), cex = 1,
    mfrow = NULL, vertical_marks = 0, col = NULL, flip_y = FALSE) {
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
    space <- stats::quantile(abs(unlist(data_by_channel_condition$data)), probs = space, na.rm = TRUE) * 2
    space_mode <- "absolute"
  } else {
    space <- abs(space)
    if (!isTRUE(space > 0)) {
      space <- max(abs(unlist(data_by_channel_condition$data)), na.rm = TRUE) * 2
    }
  }
  space <- unname(space)

  group_indexes <- data_by_channel_condition$group_indexes
  sample_rate <- data_by_channel_condition$sample_rate
  time_points <- data_by_channel_condition$time_points
  coord_table <- data_by_channel_condition$coord_table
  group_labels <- data_by_channel_condition$group_labels
  n_groups <- data_by_channel_condition$n

  if (!length(col)) {
    pal <- use_discrete_palette()
    col <- pal$colors
  }
  max_group <- max(group_indexes)
  if (length(col) < max_group) {
    col <- rep(col, ceiling(max_group / length(col)))
  }
  col <- col[group_indexes]

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

  channel_names <- switch(
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

  switch(
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
        if (n_groups > 4) {
          mfrow <- n2mfrow(n_groups, asp = 4)
        } else {
          mfrow <- c(1, n_groups)
        }
      }

      oldpar <- graphics::par(mar = c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1,
                              mfrow = mfrow, cex = 1)
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
          if (flip_y) {
            - t(data_time_by_channel)
          } else {
            t(data_time_by_channel)
          },
          sample_rate = sample_rate,
          space = space,
          space_mode = space_mode,
          time_shift = time_shift,
          start_time = start_time,
          duration = duration,
          ylab = "",
          channel_names = channel_names,
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

        if (length(vertical_marks)) {
          graphics::abline(v = vertical_marks, col = "#80808040", lty = 2)
        }

        if (length(hlines_chs)) {
          graphics::abline(h = hlines_chs * space, col = "#80808040", lty = 3)
        }

        # if (ii == 1) {
        graphics::arrows(time_range[[2]], 0.5 * space, time_range[[2]], 1.5 * space,
                         code = 3L, angle = 20, length = 0.05, col = "#808080", lwd = 2)

        if (flip_y) {
          graphics::text(
            time_range[[2]], space * 1.5,
            cex = cex * 0.8, offset = 0.2, adj = c(1.2, 1.5),
            labels = bquote(.(sprintf("%.0f", - space / 2)) ~ mu * V)
          )

          graphics::text(
            time_range[[2]], space * 0.5,
            cex = cex * 0.8, offset = 0.2, adj = c(1.2, - 0.5),
            labels = bquote(.(sprintf("%.0f", space / 2)) ~ mu * V)
          )
        } else {
          graphics::text(
            time_range[[2]], space * 1.5,
            cex = cex * 0.8, offset = 0.2, adj = c(1.2, 1.5),
            labels = bquote(.(sprintf("%.0f", space / 2)) ~ mu * V)
          )

          graphics::text(
            time_range[[2]], space * 0.5,
            cex = cex * 0.8, offset = 0.2, adj = c(1.2, - 0.5),
            labels = bquote(.(sprintf("%.0f", - space / 2)) ~ mu * V)
          )
        }
        # }
      }
    },
    "channel" = {
      n_channels <- nrow(coord_table)
      if (length(mfrow) != 2) {
        mfrow <- n2mfrow(n_channels, asp = 2)
      }

      oldpar <- graphics::par(mar = c(3.1, 3.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1,
                              mgp = cex * c(2, 0.5, 0),
                              cex.lab = 1, cex = 1,
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
          xlim = time_range, ylim = if (flip_y) {
            c(0.5, -0.5) * space
          } else {
            c(-0.5, 0.5) * space
          },
          cex = cex, cex.main = par_opt$cex.main * cex,
          cex.lab = par_opt$cex.lab *  cex, cex.axis = par_opt$cex.axis * cex,
          tck = tck, adj = 0, col = col,
          main = bquote(
            .(sprintf("Ch %s", channel_names[[ii]])) ~
              scriptstyle(
                "(" ~ .(round(ylim[[1]])) ~ "~" ~ .(round(ylim[[2]])) ~ mu * V ~ ")"
              )
          ),
        )


        graphics::mtext(side = 1, text = "Time (s)", line = xline,
                        cex = par_opt$cex.lab * cex)

        graphics::axis(side = 1L, at = pretty(time_range),
                       las = 1, tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
                       cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)
        graphics::mtext(side = 2, text = bquote("Voltage (" * mu * "V)"), line = yline,
                        cex = par_opt$cex.lab * cex)

        graphics::axis(
          side = 2L, at = c(-0.5, 0, 0.5) * space, las = 1,
          labels = c(sprintf("%.0f", space * -0.5), "0", sprintf("%.0f", space * 0.5)),
          tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
          cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)


        graphics::abline(v = vertical_marks, col = "#80808040", lty = 2)
        graphics::abline(h = 0, col = "#80808040", lty = 2)

        # if (ii %% mfrow[[2]] == 0) {
        # legend
        graphics::legend(
          "topright",
          sprintf("%s, n=%d", group_labels, data_by_channel_condition$n_trials),
          lty = 1,
          col = col,
          bty = "n",
          cex = cex * 0.8
        )
        # }
      }
    }
  )

  invisible()

}
