get_spacing <- function(x, space, space_mode = c("quantile", "absolute")) {
  if (length(space_mode) > 1) {
    space_mode <- space_mode[space_mode %in% c("quantile", "absolute")]
    space_mode <- space_mode[[1]]
  } else {
    space_mode <- match.arg(space_mode)
  }

  if (space_mode == "quantile") {
    if (!isTRUE(space > 0 && space < 1)) {
      space <- 1.0
    }
    space <- stats::quantile(abs(unlist(x)),
                             probs = space, na.rm = TRUE)
    space_mode <- "absolute"
  } else {
    space <- abs(space)
    if (!isTRUE(space > 0)) {
      space <- max(abs(unlist(x)), na.rm = TRUE)
    }
  }

  unname(space)
}


get_mfrow <- function(n, mfrow = NULL, asp = 3) {
  if (length(mfrow) != 2 || anyNA(mfrow)) {
    if (n > 4) {
      mfrow <- n2mfrow(n, asp = 3)
    } else {
      mfrow <- c(1, n)
    }
  }
  mfrow
}


get_time_range <- function(time_points, time_range = c(NA, NA)) {
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

  list(
    time_range = time_range,
    time_shift = time_shift,
    start_time = start_time,
    end_time = end_time,
    duration = duration
  )
}


add_axis_time <- function(time_range, text = "Time (s)", cex = 1) {
  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  xline <- 1.5 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1
  graphics::axis(side = 1L, at = pretty(time_range),
                 las = 1, tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
                 cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)
  graphics::mtext(side = 1, text = text, line = xline,
                  cex = par_opt$cex.lab * cex)
}

add_axis_voltage <- function(value_range, text = bquote("Voltage" ~ (mu * V)), cex = 1) {
  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  yline <- 1 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1
  graphics::axis(
    side = 2L,
    at = c(value_range, 0),
    labels = c(sprintf("%.0f", value_range), "0"),
    las = 1, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex
  )
  graphics::mtext(side = 2L, text, line = 2, cex = cex)
}

add_vertical_marks <- function(vertical_marks = NULL, col = "#808080", lty = 3, ...) {
  if (length(vertical_marks)) {
    graphics::abline(v = vertical_marks, col = col, lty = lty, ...)
  }
}

add_crp_decorators <- function(crp_result, cex = 1) {
  if (!length(crp_result)) { return() }

  crp_time <- crp_result$parameters$params_times
  crp_mean <- crp_result$parameters$C * mean(crp_result$parameters$al)

  graphics::lines(crp_time, crp_mean, col = "yellow", lwd = 3)
  graphics::lines(crp_time, crp_result$parameters$avg_trace_tR, lwd = 1)

  idx <- which.min(abs(crp_time - crp_result$tau_R))

  # points(crp_time[[idx]], crp_mean[[idx]], pch = 1, col = "red")

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  xline <- 1.5 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1

  graphics::axis(
    side = 1,
    at = crp_result$tau_R,
    tick = TRUE,
    pos = crp_mean[[idx]],
    labels = bquote(tau[R] * .(sprintf(
      "=%.2fs", crp_result$tau_R
    ))),
    col = "red", col.axis = "red",
    tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex
  )

}

add_heatmap_legend <- function(vlim, col, title = bquote(mu * "V"), cex = 1,
                               fmt = NULL) {
  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  par_opt$cex.lab <- 1

  vlim <- range(vlim, na.rm = TRUE)

  # Pick a format with enough precision so small (normalized) ranges do not
  # collapse to the same printed value.
  if (is.null(fmt)) {
    span <- diff(vlim)
    if (is.finite(span) && span > 0) {
      digits <- max(0, ceiling(-log10(span)) + 1)
    } else {
      digits <- 0
    }
    fmt <- sprintf("%%.%df", digits)
  }

  legend_z <- seq(vlim[[1]], vlim[[2]], length.out = length(col))

  graphics::image(
    x = 1,
    y = legend_z,
    z = matrix(legend_z, nrow = 1),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = title,
    col = col,
    cex.main = par_opt$cex.main * cex
  )

  graphics::axis(
    side = 2L,
    at = c(vlim, 0),
    labels = c(sprintf(fmt, vlim), "0"),
    las = 1, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex
  )
}

add_axis_trial_number <- function(group, by = 5, cex = 1, vspace = 1) {

  n_trials <- group$n_trials

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  par_opt$cex.lab <- 1
  tck <- -0.005 * (3 + cex)

  if (by > n_trials) {
    at <- c(1, n_trials)
  } else {
    at <- seq(0, by = by, length.out = ceiling(n_trials / by) + 1)
    at <- unique(sort(c(at, 1, n_trials)))

    at <- at[at >= 1 & at <= n_trials]
  }

  graphics::axis(
    side = 2L, at = at * vspace, labels = at, las = 1,
    tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)

}

add_axis_trial_stimuli <- function(group, cex = 1, vspace = 1, lty = 1, col = "#808080") {

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  yline <- 1 * cex
  tck <- -0.005 * (3 + cex)
  par_opt$cex.lab <- 1

  separators <- cumsum(c(0, group$trial_count)) + 0.5

  graphics::axis(
    side = 2L, at = separators * vspace, las = 1,
    labels = rep("", length(group$trial_count) + 1),
    tck = tck, cex = cex, cex.main = par_opt$cex.main * cex,
    cex.lab = par_opt$cex.lab * cex, cex.axis = par_opt$cex.axis * cex)


  graphics::text(
    x = par("usr")[[1]],
    y = (cumsum(group$trial_count) + 0.5 - group$trial_count / 2) * vspace,
    labels = sprintf("%s  ", group$conditions),
    srt = 45, adj = c(1, 0.5),
    cex = 0.85 * cex, xpd = NA)

  if (length(separators) > 1) {
    graphics::abline(h = separators[- c(1)] * vspace,
                     col = col, lty = lty)
  }
}

prepare_par <- function(mfrow = NULL, cex = 1, mar = c(3.1, 3.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1, mgp = cex * c(2, 0.5, 0), tck = -0.005 * (3 + cex),
                        env = parent.frame(), ...) {

  args <- list(
    mar = mar,
    mgp = mgp,
    tck = tck,
    mfrow = mfrow,
    cex = 1,
    ...
  )
  if (!length(mfrow)) {
    args$mfrow <- NULL
    args[[length(args) + 1]] <- "mfrow"
  }

  oldpar <- do.call(graphics::par, args)

  do.call(
    on.exit, list(bquote({
      graphics::par(.(oldpar))
    }), add = TRUE, after = TRUE),
    envir = env
  )

  par_opt <- graphics::par(c("mai", "mar", "mgp", "cex.main",
                             "cex.lab", "cex.axis", "cex.sub"))
  par_opt$cex.lab <- 1

  invisible(par_opt)
}

