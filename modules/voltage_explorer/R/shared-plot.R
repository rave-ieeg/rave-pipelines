group_palette <- c("#FFA500", "#1874CD", "#006400", "#FF4500", "#A52A2A", "#7D26CD",
                   "#FE00FA", "#16FF32", "#FBE426", "#B00068", "#1CFFCE", "#90AD1C",
                   "#2ED9FF", "#DEA0FD", "#F8A19F", "#325A9B", "#C4451C", "#1C8356",
                   "#85660D", "#B10DA1", "#1CBE4F", "#F7E1A0", "#C075A6", "#AAF400",
                   "#BDCDFF", "#822E1C", "#B5EFB5", "#7ED7D1", "#1C7F93", "#3B00FB"
)

pretty_frequency_range <- function(frequency_range) {
  is_na <- is.na(frequency_range)
  if(all(is_na)) { return(NULL) }
  if(any(is_na)) {
    if(is_na[[1]]) {
      return(sprintf("0~%s Hz", frequency_range[[2]]))
    } else {
      return(sprintf("%s Hz~Nyquist", frequency_range[[1]]))
    }
  } else {
    sprintf("%s~%s Hz", frequency_range[[1]], frequency_range[[2]])
  }
}

# ---- Voltage over time per channel -------------------------------------------
# collapsing trials, for each condition group

prepare_voltage_over_time_per_channel <- function(filtered_array, condition_group, channels) {
  # DIPSAUS DEBUG START
  # condition_group = parameter_grid$condition_groups[[1]]
  # channels = analysis_channels_clean

  if(!condition_group$has_trials) { return(NULL) }

  if(missing(channels) || !length(channels)) {
    channels <- filtered_array$get_header("filtered_channels")
  }

  # time x trial
  subarray <- subset(
    filtered_array,
    Trial ~ Trial %in% condition_group$trial_num,
    Electrode ~ Electrode %in% channels,
    drop = FALSE
  )

  # collapse
  data = ravetools::collapse(subarray, keep = c(1L, 3L), average = TRUE)

  # other information
  sample_rate <- filtered_array$get_header("sample_rate")
  frequency_range <- filtered_array$get_header("frequency_range")

  time_points <- dimnames(filtered_array)$Time

  # ravetools::plot_signals(t(data), sample_rate = sample_rate, time_shift = time_points[[1]], channel_names = channels)

  # df <- data.frame(Time = time_points)
  # for(ii in seq_along(channels)) {
  #   name <- sprintf("Ch%d", channels[[ii]])
  #   df[[name]] <- data[, ii]
  # }
  # plt <- plotly::plot_ly(data = df, x = ~Time)
  # for(ii in seq_along(channels)) {
  #   name <- sprintf("Ch%d", channels[[ii]])
  #   plt <- plotly::add_trace(plt, y = as.formula(sprintf("~%s", name)), type = 'scatter', mode = 'lines', name = name)
  # }
  # plotly::add_data(plt, data = df)

  structure(
    list(
      description = "plot_data_voltage_over_time_per_channel",
      condition_group = list(
        id = condition_group$id,
        order = condition_group$order,
        name = condition_group$name
      ),
      sample_rate = sample_rate,
      frequency_range = frequency_range,
      channels = channels,
      time = time_points,
      data = data
    ),
    class = c(
      "voltage_explorer_plot_data_voltage_over_time_per_channel",
      "voltage_explorer_plot_data"
    )
  )
}

# DIPSAUS DEBUG START
# data <- prepare_voltage_over_time_per_channel(
#   filtered_array = filtered_array,
#   condition_group = condition_groups[[1]],
#   channels = analysis_channels_clean
# )
plot_voltage_over_time_per_channel <- function(
    data, ncols = 1, new_plot = TRUE, cex = 1, space = 0.999, ylab = "Channel", ...) {

  if(!length(data)) { return() }

  ncols %?<-% 1
  new_plot %?<-% TRUE
  cex %?<-% 1
  ylab %?<-% "Channel"
  space %?<-% 0.999

  # data$condition_group

  channels <- data$channels
  nchannels <- length(channels)
  nr <- ceiling(nchannels / ncols)

  sample_rate <- data$sample_rate
  time_start <- min(data$time, na.rm = TRUE)

  mar <- c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1
  if( new_plot ) {
    oldpar <- graphics::par(mfrow = c(1, ncols), mar = mar)
  } else {
    oldpar <- graphics::par(mar = mar)
  }
  # on.exit({ do.call(graphics::par, oldpar) })

  if( space > 1 ) {
    space_abs <- space
  } else {
    space_abs <- quantile(abs(data$data), space) * 2
  }

  # top-to bottom style
  for(ii in seq_len(ncols)) {
    idx <- seq_len(nr) + (ii - 1) * nr
    idx <- rev(idx[idx <= nchannels])
    if(length(idx)) {
      info <- ravetools::plot_signals(
        signals = t(data$data[, idx, drop = FALSE]),
        sample_rate = sample_rate, time_shift = time_start,
        channel_names = channels[idx], ylab = ifelse(ii == 1, ylab, ""),
        # ...,
        cex = cex, space = space_abs, space_mode = "absolute"
      )
      if( ii == 1 ) {
        title_text <- paste(
          c(data$condition_group$name, pretty_frequency_range(data$frequency_range)),
          collapse = ", ")
        graphics::title(title_text, adj = 0, line = 0.5)
      }
      graphics::abline(v = 0, col = "#808080", lty = 3)
      # graphics::abline(v = time_start, lty = 1)
      # graphics::abline(h = info$space * seq_along(idx), col = "#808080aa", lty = 3)
      graphics::grid(nx = 0, ny = length(idx) * 2, lty = 3, col = "#808080AA", lwd = 0.5)
    }
  }

}

# ---- Voltage over time per trial ---------------------------------------------
# collapse channels if nchannels > 0

prepare_voltage_over_time_per_trial <- function(
    filtered_array, condition_group, channels) {

  # DIPSAUS DEBUG START
  # condition_group = parameter_grid$condition_groups[[1]]
  # channels = analysis_channels_clean[[2]]

  if(!condition_group$has_trials) { return(NULL) }

  filtered_channels <- filtered_array$get_header("filtered_channels")
  channels_ <- channels
  channels <- as.integer(channels)
  channels <- channels[channels %in% filtered_channels]
  if(!length(channels)) {
    stop(sprintf("Channel [%s] not loaded or filtered", paste(channels_, collapse = ", ")))
  }

  trial_num <- as.integer(dimnames(filtered_array)$Trial)
  trial_sel <- trial_num %in% condition_group$trial_num
  trial_num <- trial_num[trial_sel]

  # time x trial
  subarray <- subset(
    filtered_array,
    Trial ~ trial_sel,
    Electrode ~ Electrode %in% channels,
    drop = FALSE
  )
  if(length(channels) > 1) {
    subarray <- ravetools::collapse(subarray, keep = c(1L, 2L), average = TRUE)
  }
  dim(subarray) <- c(dim(filtered_array)[1], length(trial_num))
  peak_val <- max(abs(range(subarray, na.rm = TRUE)))

  epoch_table <- condition_group$epoch
  cond <- sapply(trial_num, function(n) {
    epoch_table$Condition[epoch_table$Trial == n][[1]]
  })

  # other information
  sample_rate <- filtered_array$get_header("sample_rate")
  frequency_range <- filtered_array$get_header("frequency_range")
  time_points <- dimnames(filtered_array)$Time

  # image(x = time_points, z = subarray, zlim = peak_val * c(-1, 1))
  # ravetools::plot_signals(
  #   t(subarray),
  #   sample_rate = sample_rate,
  #   time_shift = time_points[[1]],
  #   channel_names = trial_num
  # )

  structure(
    list(
      description = "plot_data_voltage_over_time_per_trial",
      condition_group = list(
        id = condition_group$id,
        order = condition_group$order,
        name = condition_group$name
      ),
      condition_label = cond,
      trial_number = trial_num,
      sample_rate = sample_rate,
      frequency_range = frequency_range,
      channels = channels,
      time = time_points,
      data = subarray,
      data_absmax = peak_val
    ),
    class = c(
      "voltage_explorer_plot_data_voltage_over_time_per_trial",
      "voltage_explorer_plot_data"
    )
  )

}


# DIPSAUS DEBUG START
# data <- prepare_voltage_over_time_per_trial(
#   filtered_array = filtered_array,
#   condition_group = condition_groups[[1]],
#   channels = analysis_channels_clean
# )
plot_voltage_over_time_per_trial <- function(
    data, clamp = NA,
    type = c("heatmap", "line"),
    sort_trial_by = c("condition", "trial_number")) {

  if(!length(data)) { return(invisible()) }
  sort_trial_by <- match.arg(sort_trial_by)
  type <- match.arg(type)

  # debug use
  sort_trial_by %?<-% "condition"
  type %?<-% "heatmap"
  clamp %?<-% NA

  # sort trials
  if(sort_trial_by == "condition") {
    o <- order(data$condition_label, -data$trial_number)
    y_ticks <- data$condition_label[o]
  } else {
    o <- order(-data$trial_number)
    y_ticks <- data$trial_number[o]
  }

  signals <- data$data[, o, drop = FALSE]

  clamp <- clamp[!is.na(clamp)]
  if(length(clamp) && is.numeric(clamp)) {
    clamp <- max(abs(clamp))
  } else {
    clamp <- data$data_absmax
  }

  start_time <- min(data$time, na.rm = TRUE)

  if( type == "heatmap" ) {
    signals[signals < -clamp] <- -clamp
    signals[signals > clamp] <- clamp

    graphics::image(
      z = signals, x = data$time, y = seq_along(y_ticks), zlim = c(-clamp, clamp),
      xlab = "", ylab = "", axes = FALSE,
      col = colorRampPalette(
        c(
          "#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#ffffff",
          "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
      )(101)
    )

    graphics::mtext(side = 1, text = 'Time (s)', line = 2)
    graphics::axis(side = 1, at = pretty(data$time))

    spacing <- 1

  } else {

    info <- ravetools::plot_signals(signals = t(signals), sample_rate = data$sample_rate, space = 0.999, time_shift = start_time, ylab = "", tck = 0, channel_names = rep("", ncol(signals)))

    spacing <- info$space

  }

  cex <- 1
  xline <- 1.5 * cex
  tck <- -0.005 * (3 + cex)
  par_opt <- graphics::par()

  if(sort_trial_by == "condition") {
    y_tick_counts <- table(y_ticks)
    y_tick_sep <- c(1, cumsum(y_tick_counts)) * spacing
    y_tick_at <- unname(cumsum(y_tick_counts) - y_tick_counts / 2) * spacing
    # graphics::axis(side = 2, at = y_tick_sep, labels = rep("", length(y_tick_sep)), line = NA)
    graphics::axis(
      side = 2, at = y_tick_sep, labels = rep("", length(y_tick_sep)),
      pos = start_time, las = 1, tck = tck,
      cex = cex, cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex,
      cex.axis = par_opt$cex.axis * cex
    )

    graphics::text(x = start_time,
         y = y_tick_at,
         labels = sprintf("%s ", names(y_tick_counts)),
         xpd = NA,
         ## Rotate the labels by 35 degrees.
         srt = 25, adj = c(1, 0.5),
         cex = 1.2)

  } else {
    idx <- pretty(seq_along(y_ticks))
    idx[idx < 1] <- 1
    idx[idx > length(y_ticks)] <- length(y_ticks)
    idx <- unique(idx)
    # graphics::axis(side = 2, at = idx * spacing, labels = y_ticks[idx], las = 1)
    graphics::axis(
      side = 2, at = idx * spacing, labels = y_ticks[idx],
      pos = start_time, las = 1, tck = tck,
      cex = cex, cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex,
      cex.axis = par_opt$cex.axis * cex
    )
  }

  graphics::title(
    main = paste(
      collapse = ", ",
      c(
        pretty_frequency_range(data$frequency_range),
        sprintf("Ch %s", dipsaus::deparse_svec(data$channels))
      )
    ),
    adj = 0,
    line = 0.5
  )

}


# ---- Voltage over time per condition per channel -----------------------------

prepare_voltage_over_time_per_cond_channel <- function(
    filtered_array, condition_groups, channels, hilbert = FALSE,
    baseline_unit = c("trial", "global", "none")) {
  # DIPSAUS DEBUG START
  # condition_groups = parameter_grid$condition_groups
  # channels = analysis_channels_clean
  # hilbert <- TRUE

  # if(!condition_group$has_trials) { return(NULL) }

  if( hilbert ) {
    hilbert <- TRUE
    baseline_unit <- match.arg(baseline_unit)
    unit <- switch(
      baseline_unit,
      "none" = "Amplitude",
      { "% Change of Amplitude" }
    )
  } else {
    baseline_unit <- "none"
    unit <- "Voltage"
  }


  filtered_channels <- filtered_array$get_header("filtered_channels")
  channels_ <- channels
  channels <- sort(dipsaus::parse_svec(channels))
  channels <- channels[channels %in% filtered_channels]
  if(!length(channels)) {
    stop(sprintf("Channel [%s] not loaded or filtered", paste(channels_, collapse = ", ")))
  }

  # other information
  sample_rate <- filtered_array$get_header("sample_rate")
  frequency_range <- filtered_array$get_header("frequency_range")
  time_points <- dimnames(filtered_array)$Time

  is_baseline <- rep(FALSE, length(time_points))
  if( baseline_unit != "none" ) {
    filter_configurations <- filtered_array$get_header("filter_configurations")
    lapply(filter_configurations, function(config) {
      if(identical(config$type, "baseline")) {
        sel <- is_baseline
        for(window in config$windows) {
          sel <- sel | (time_points >= window[[1]] & time_points <= window[[2]])
        }
        is_baseline <<- sel
      }
      return()
    })
  }



  condition_groups <- condition_groups[vapply(condition_groups, "[[", FALSE, "has_trials")]

  collapsed_arrays <- lapply(condition_groups, function(condition_group) {
    # condition_group <- condition_groups[[1]]
    # time x trial x channel
    subarray <- subset(
      filtered_array,
      Trial ~ Trial %in% condition_group$trial_num,
      Electrode ~ Electrode %in% channels,
      drop = FALSE
    )
    if( hilbert ) {
      dm <- dim(subarray)
      dim(subarray) <- c(dm[[1]], prod(dm[-1]))
      subarray <- abs(gsignal::hilbert(subarray))
      if( baseline_unit == "trial" ) {
        baseline_mean <- colMeans(subarray[is_baseline, , drop = FALSE])
        # zero signal
        baseline_mean[baseline_mean == 0] <- 1
        subarray <- t(t(subarray) * (100 / baseline_mean) - 100)
      } else if ( baseline_unit == "global" ) {
        baseline_mean <- colMeans(subarray[is_baseline, , drop = FALSE])
        baseline_mean <- baseline_mean[baseline_mean != 0]
        if(length(baseline_mean)) {
          baseline_mean <- mean(baseline_mean, na.rm = TRUE)
        } else {
          baseline_mean <- 1
        }
        subarray <- subarray * (100 / baseline_mean) - 100
      }
      dim(subarray) <- dm
    }
    ravetools::collapse(subarray, keep = c(1L, 3L), average = TRUE)
  })

  # time x cond x channel
  collapsed_arrays <- aperm(simplify2array(collapsed_arrays, higher = TRUE), c(1, 3, 2))

  peak_val <- max(abs(range(collapsed_arrays, na.rm = TRUE)))


  # condition group info
  condition_order <- sapply(condition_groups, "[[", "order")
  condition_id <- sapply(condition_groups, "[[", "id")
  condition_name <- sapply(condition_groups, "[[", "name")
  condition_ntrials <- sapply(condition_groups, function(cond) { length(cond$trial_num) })

  structure(
    list(
      description = "plot_data_voltage_over_time_per_cond_channel",
      condition_groups = list(
        id = condition_id,
        order = condition_order,
        name = condition_name,
        n_trials = condition_ntrials
      ),
      sample_rate = sample_rate,
      channels = channels,
      time = time_points,
      frequency_range = frequency_range,
      data = collapsed_arrays,
      unit = unit,
      hilbert = hilbert,
      baseline_unit = baseline_unit,
      data_absmax = peak_val
    ),
    class = c(
      "voltage_explorer_plot_data_voltage_over_time_per_cond_channel",
      "voltage_explorer_plot_data"
    )
  )

}


# DIPSAUS DEBUG START
# data <- prepare_voltage_over_time_per_cond_channel(
#   filtered_array = filtered_array,
#   condition_groups = condition_groups,
#   channels = analysis_channels_clean,
#   hilbert = TRUE, baseline_unit  = 'global'
# )
plot_voltage_over_time_per_cond_channel <- function(
    data, clamp = NA) {

  if(!length(data)) { return(invisible()) }

  n_channels <- length(data$channels)
  n_conditions <- length(data$condition_groups$order)

  col <- group_palette[data$condition_groups$order]

  if(!isTRUE(is.finite(clamp))) {
    clamp <- data$data_absmax
  } else {
    clamp <- max(abs(clamp), na.rm = TRUE)
  }

  if( data$hilbert && data$baseline_unit == "none" ) {
    ylim <- range(pretty(c(0, clamp)))
  } else {
    ylim <- range(pretty(c(-clamp, clamp)))
  }

  oldpar <- graphics::par(
    mfrow = n2mfrow(n_channels, asp = 3),
    mar = c(3.3, 3.8, 2.1, 0.5)
  )
  on.exit({ do.call(graphics::par, oldpar) })

  for(ii in seq_len(n_channels)) {
    channel <- data$channels[[ii]]
    channel_data <- data$data[, , ii, drop = FALSE]
    dim(channel_data) <- dim(channel_data)[c(1, 2)]
    graphics::matplot(
      x = data$time, y = channel_data, type = "l", lty = 1, col = col, axes = FALSE,
      xlab = "", ylab = "", ylim = ylim, xaxs = "i")

    graphics::abline(v = 0, col = "#808080", lty = 3)
    graphics::abline(h = 0, col = "#808080AA", lty = 3)

    graphics::axis(1, at = pretty(data$time))
    if( startsWith(data$unit, "%") ) {
      graphics::axis(2, at = pretty(ylim), las = 1, labels = sprintf("%s%%", pretty(ylim)))
    } else {
      graphics::axis(2, at = pretty(ylim), las = 1)
    }


    graphics::mtext(side = 1, text = "Time (s)", line = 1.8)
    graphics::mtext(side = 2, text = data$unit, line = 2.8)
    graphics::title(main = paste(
      collapse = ", ",
      c(
        sprintf("Ch %s", channel),
        pretty_frequency_range(data$frequency_range)
      )
    ), line = 0.5, adj = 0)

    graphics::legend(
      "topright",
      sprintf("%s (N=%d)", data$condition_groups$name, data$condition_groups$n_trials),
      lty = 1, col = col, bty = "n"
    )
  }

}



