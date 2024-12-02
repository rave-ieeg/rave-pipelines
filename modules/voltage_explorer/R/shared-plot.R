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

#' @description
#' Prepare data for plotting ERP over time for each channel, collapsed
#' over condition group
#' @param filtered_array filtered array, `filearray` object
#' @param condition_group condition group, element of parameter grid
#' @param channels channels for analysis
collapse_cond <- function(filtered_array, condition_group, analysis_setting) {
  # DIPSAUS DEBUG START
  # condition_group = parameter_grid$condition_groups[[1]]
  # analysis_setting = parameter_grid$analysis_settings[[1]]

  if(!length(condition_group$trial_numbers)) { return(NULL) }

  subarray <- subset_filtered_array(filtered_array, condition_group, analysis_setting)

  # collapse
  data = ravetools::collapse(subarray, keep = c(1L, 3L), average = TRUE)

  # other information
  filter_configurations <- filtered_array$get_header("filter_configurations")
  sample_rate <- filtered_array$get_header("sample_rate")
  frequency_range <- filtered_array$get_header("frequency_range")

  time_points <- dimnames(filtered_array)$Time

  if(length(filter_configurations)) {
    physical_unit <- filter_configurations[[length(filter_configurations)]]$physical_unit
  } else {
    physical_unit <- DEFAULT_VOLTAGE_UNIT
  }

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
      # condition_group = condition_group,
      # analysis_setting = analysis_setting,
      sample_rate = sample_rate,
      frequency_range = frequency_range,
      channels = channels,
      time = time_points,
      physical_unit = physical_unit,
      data = data
    ),
    class = c(
      "voltage_explorer_plot_data_voltage_over_time_per_channel",
      "voltage_explorer_plot_data"
    )
  )
}

graphics_title_height <- function(text = "", units = "inches") {
  graphics::strheight(text, units = units, cex = graphics::par('cex.main'))
}

# DIPSAUS DEBUG START
# data <- prepare_voltage_over_time_per_channel(
#   filtered_array = filtered_array,
#   condition_group = condition_groups[[1]],
#   channels = analysis_channels_clean
# )

plot_voltage_over_time_per_channel <- function(
    data, ncols = 1, new_plot = TRUE, cex = 1, space = 0.999, ylab = NULL,
    ...) {

  if(!length(data)) { return() }

  ncols %?<-% 2
  new_plot %?<-% TRUE
  cex %?<-% 1
  ylab %?<-% "Channel"
  space %?<-% 0.999

  # data$condition_group
  physical_unit <- paste(data$physical_unit, collapse = "")
  if(!nzchar(physical_unit)) {
    physical_unit <- DEFAULT_VOLTAGE_UNIT
  }

  channels <- data$channels
  nchannels <- length(channels)
  nr <- ceiling(nchannels / ncols)

  sample_rate <- data$sample_rate
  time_start <- min(data$time, na.rm = TRUE)

  mar <- c(3.1, 2.1, 2.1, 0.8) * (0.25 + cex * 0.75) + 0.1
  title_height <- graphics_title_height()
  oldpar <- graphics::par(c("mar", "mfrow"))

  freq_text <- pretty_frequency_range(data$frequency_range)
  if(length(freq_text)) {
    freq_text <- sprintf("freq=%s, ", freq_text)
  } else {
    freq_text <- ""
  }
  drange <- range(data$data, na.rm = TRUE)
  title_text <- sprintf(
    "%s (%srange=%s, unit=%s)",
    paste(data$condition_group$name, collapse = ""),
    freq_text,
    paste(sprintf("%.0f", drange), collapse = "~"),
    physical_unit
  )

  if( new_plot ) {
    graphics::layout(
      matrix(c(rep(1, ncols), seq_len(ncols) + 1), nrow = 2, byrow = TRUE),
      heights = c(graphics::lcm(title_height * 5.1), 1)
    )
    on.exit({ do.call(graphics::par, oldpar) })
    # oldpar <- graphics::par(mfrow = c(1, ncols), mar = mar)
    graphics::par(mar = c(0, mar[[2]], 0, mar[[4]]))
    graphics::plot.new()
    graphics::title(title_text, adj = 0, line = -1)
  }


  if( space > 1 ) {
    space_abs <- space
  } else {
    space_abs <- quantile(abs(data$data), space) * 2
  }

  time_end <- max(data$time)

  # top-to bottom style
  for(ii in seq_len(ncols)) {
    idx <- seq_len(nr) + (ii - 1) * nr
    idx <- rev(idx[idx <= nchannels])
    if(length(idx)) {
      info <- ravetools::plot_signals(
        signals = t(data$data[, idx, drop = FALSE]),
        sample_rate = sample_rate, time_shift = time_start,
        channel_names = channels[idx], ylab = ifelse(ii == 1, ylab, ""),
        ...,
        cex = cex, space = space_abs, space_mode = "absolute"
      )
      # if( ii == 1 ) {
      #   freq_text <- pretty_frequency_range(data$frequency_range)
      #   if(length(freq_text)) {
      #     freq_text <- sprintf("freq=%s, ", freq_text)
      #   } else {
      #     freq_text <- ""
      #   }
      #   title_text <- sprintf(
      #     "%s (%sunit=%s)",
      #     paste(data$condition_group$name, collapse = ""),
      #     freq_text,
      #     physical_unit
      #   )
      #   graphics::title(title_text, adj = 0, line = 0.5)
      # }
      graphics::abline(v = 0, col = "#808080", lty = 3)
      # graphics::abline(v = time_start, lty = 1)
      # graphics::abline(h = info$space * seq_along(idx), col = "#808080aa", lty = 3)
      graphics::grid(nx = 0, ny = length(idx) * 2, lty = 3, col = "#808080AA", lwd = 0.5)

      graphics::arrows(
        x0 = time_end, x1 = time_end, y0 = info$space * (nr - 0.5),
        y1 = info$space * c(nr + 0.5), col = "#80808080", code = 3,
        length = title_height, angle = 90, lty = 2
      )
      graphics::text(
        sprintf("\u00B1%.1f", info$space),
        x = time_end, y = info$space * c(nr + 0.5) + graphics::strheight("", "user"),
        col = "#80808080", adj = c(1, 0.5)
      )
    }
  }


  invisible(list(
    title = title_text
  ))

}

# ---- Voltage over time per trial ---------------------------------------------
# collapse channels if nchannels > 0

prepare_voltage_over_time_per_trial <- function(
    filtered_array, condition_group, channels) {

  # DIPSAUS DEBUG START
  # condition_group = parameter_grid$condition_groups[[1]]
  # channels = analysis_channels_clean[[2]]

  if(!length(condition_group$trial_numbers)) { return(NULL) }

  filter_configurations <- filtered_array$get_header("filter_configurations")
  if(length(filter_configurations)) {
    physical_unit <- filter_configurations[[length(filter_configurations)]]$physical_unit
  } else {
    physical_unit <- DEFAULT_VOLTAGE_UNIT
  }

  filtered_channels <- filtered_array$get_header("filtered_channels")
  channels_ <- channels
  channels <- as.integer(channels)
  channels <- channels[channels %in% filtered_channels]
  if(!length(channels)) {
    stop(sprintf("Channel [%s] not loaded or filtered", paste(channels_, collapse = ", ")))
  }

  trial_num <- as.integer(dimnames(filtered_array)$Trial)
  trial_sel <- trial_num %in% condition_group$trial_numbers
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

  cond <- condition_group$trial_condition

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
        order = condition_group$group_order,
        name = condition_group$label
      ),
      condition_label = cond,
      trial_number = trial_num,
      sample_rate = sample_rate,
      frequency_range = frequency_range,
      channels = channels,
      time = time_points,
      physical_unit = physical_unit,
      data_absmax = peak_val,
      data = subarray
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

    oldpar <- graphics::par(
      mar = c(3.3, 3.8, 2.1, 0.5)
    )
    on.exit({ do.call(graphics::par, oldpar) })
    info <- ravetools::plot_signals(signals = t(signals), sample_rate = data$sample_rate, space = 0.999, time_shift = start_time, ylab = "", tck = 0, channel_names = rep("", ncol(signals)))

    spacing <- info$space

  }
  graphics::abline(v = 0, lty = 3)

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
    filtered_array, condition_groups, channels) {
  # DIPSAUS DEBUG START
  # condition_groups = parameter_grid$condition_groups
  # channels = analysis_channels_clean

  # if(!condition_group$has_trials) { return(NULL) }

  # other information
  filter_configurations <- filtered_array$get_header("filter_configurations")
  if(length(filter_configurations)) {
    physical_unit <- filter_configurations[[length(filter_configurations)]]$physical_unit
    hilbert <- "hilbert" %in% vapply(filter_configurations, "[[", "", "type")
  } else {
    physical_unit <- DEFAULT_VOLTAGE_UNIT
    hilbert <- FALSE
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

  condition_groups <- condition_groups[vapply(condition_groups, function(condition_group) {
    length(condition_group$trial_numbers) > 0
  }, FALSE)]

  collapsed_arrays <- lapply(condition_groups, function(condition_group) {
    # condition_group <- condition_groups[[1]]
    # time x trial x channel
    subarray <- subset(
      filtered_array,
      Trial ~ Trial %in% condition_group$trial_numbers,
      Electrode ~ Electrode %in% channels,
      drop = FALSE
    )
    ravetools::collapse(subarray, keep = c(1L, 3L), average = TRUE)
  })

  # time x cond x channel
  collapsed_arrays <- aperm(simplify2array(collapsed_arrays, higher = TRUE), c(1, 3, 2))

  peak_val <- max(abs(range(collapsed_arrays, na.rm = TRUE)))


  # condition group info
  condition_order <- sapply(condition_groups, "[[", "group_order")
  condition_id <- sapply(condition_groups, "[[", "id")
  condition_name <- sapply(condition_groups, "[[", "label")
  condition_ntrials <- sapply(condition_groups, function(cond) { length(cond$trial_numbers) })

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
      physical_unit = physical_unit,
      data_absmax = peak_val,
      data = collapsed_arrays
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
plot_voltage_over_time_per_cond <- function(
    data, channels = NULL, clamp = NA, engine = c("base", "plotly"))
{

  engine <- match.arg(engine)

  clamp %?<-% NA
  channels %?<-% NULL
  engine %?<-% "base"

  if(!length(data)) { return(invisible()) }

  if(!length(channels)) {
    channels <- data$channels
  } else {
    channels <- channels[channels %in% data$channels]
  }
  if(!length(channels)) { stop("No channel selected to collapse.") }

  n_conditions <- length(data$condition_groups$order)
  col <- group_palette[data$condition_groups$order]

  idx <- which(data$channels %in% channels)
  if(length(idx) > 1) {
    cond_over_time <- ravetools::collapse(data$data[,,idx], keep = c(1, 2), average = TRUE)
  } else {
    cond_over_time <- data$data[,,idx, drop = TRUE]
  }

  if(!isTRUE(is.finite(clamp))) {
    clamp <- max(abs(range(cond_over_time, na.rm = TRUE)))
  } else {
    clamp <- max(abs(clamp), na.rm = TRUE)
  }
  ylim <- range(pretty(c(-clamp, clamp)))

  title_text <- paste(
    collapse = ", ",
    c(
      sprintf("Ch %s", dipsaus::deparse_svec(data$channels[idx])),
      pretty_frequency_range(data$frequency_range)
    )
  )

  switch(
    engine,
    "plotly" = {
      # construct plotly
      p <- plotly::plot_ly(data = data.frame(Time = data$time), x = ~Time)

      # Add each column as a separate trace
      for (i in seq_along(data$condition_groups$name)) {
        p <- plotly::add_trace(
          p,
          y = cond_over_time[, i],
          type = 'scatter',
          mode = 'lines',
          name = sprintf("%s (N=%d)", data$condition_groups$name[[i]], data$condition_groups$n_trials[[i]]),
          line = list(color = col[i], width = 1),
          hovertemplate = paste0("<b>", data$condition_groups$name[[i]], "</b>: %{y:.1f}<extra></extra>")
        )
      }

      # Customize layout
      p <- plotly::layout(
        p,
        title = list(
          text = title_text,
          x = 0.05
        ),
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = data$physical_unit),
        hovermode = "x unified"
      )
      p
    },
    {
      oldpar <- graphics::par(
        mar = c(3.3, 3.8, 2.1, 0.5)
      )
      on.exit({ do.call(graphics::par, oldpar) })

      graphics::matplot(
        x = data$time, y = cond_over_time, type = "l", lty = 1, col = col, axes = FALSE,
        xlab = "", ylab = "", ylim = ylim, xaxs = "i")

      graphics::abline(v = 0, col = "#808080", lty = 3)
      graphics::abline(h = 0, col = "#808080AA", lty = 3)

      graphics::axis(1, at = pretty(data$time))
      graphics::axis(2, at = pretty(ylim), las = 1)

      graphics::mtext(side = 1, text = "Time (s)", line = 1.8)
      graphics::mtext(side = 2, text = data$physical_unit, line = 2.8)
      graphics::title(main = title_text, line = 0.5, adj = 0)

      graphics::legend(
        "topright",
        sprintf("%s (N=%d)", data$condition_groups$name, data$condition_groups$n_trials),
        lty = 1, col = col, bty = "n"
      )
    }
  )
}

plot_voltage_over_time_per_cond_channel <- function(
    data, clamp = NA, engine = c("base", "plotly"))
{
  engine <- match.arg(engine)

  clamp %?<-% NA
  engine %?<-% "base"

  if(!length(data)) { return(invisible()) }

  n_channels <- length(data$channels)
  n_conditions <- length(data$condition_groups$order)

  col <- group_palette[data$condition_groups$order]

  if(!isTRUE(is.finite(clamp))) {
    clamp <- data$data_absmax
  } else {
    clamp <- max(abs(clamp), na.rm = TRUE)
  }

  ylim <- range(pretty(c(-clamp, clamp)))

  switch(
    engine,
    "plotly" = {
      plots <- lapply(seq_len(n_channels), function(ii) {
        channel <- data$channels[[ii]]
        channel_data <- data$data[, , ii, drop = FALSE]
        dim(channel_data) <- dim(channel_data)[c(1, 2)]
        title_text <- paste(
          collapse = ", ",
          c(
            sprintf("Ch %s", channel),
            pretty_frequency_range(data$frequency_range)
          )
        )

        p <- plotly::plot_ly(data.frame(Time = data$time), x = ~ Time)

        # Add each column as a separate trace
        for (i in seq_along(data$condition_groups$name)) {
          p <- plotly::add_trace(
            p,
            y = channel_data[, i],
            type = 'scatter',
            mode = 'lines',
            name = sprintf("%s (N=%d)", data$condition_groups$name[[i]], data$condition_groups$n_trials[[i]]),
            line = list(color = col[i], width = 1),
            hovertemplate = paste0("<b>", data$condition_groups$name[[i]], "</b>: %{y:.1f}<extra></extra>"),
            showlegend = ii == 1
          )
        }

        # Customize layout
        p <- plotly::layout(
          p,
          annotations = list(
            text = title_text,
            x = 0.05,
            y = 1,
            showarrow = FALSE,
            yref = "paper",
            xref = "paper",
            xanchor = "left",
            yanchor = "top"
            # font = list(size = 15)
          ),
          xaxis = list(
            title = "Time (s)",
            zerolinewidth = 1,
            zerolinecolor = "rgba(0, 0, 0, 0.3)",
            range = c(0, 0.5)
            # color = "rgba(0, 0, 0, 0.5)",      # Semi-transparent color for x-axis text and ticks
            # linecolor = "rgba(0, 0, 0, 0.3)"  # Semi-transparent line for x-axis line
            # tickcolor = "rgba(0, 0, 0, 0.3)"   # Semi-transparent color for x-axis ticks
          ),
          yaxis = list(
            title = data$physical_unit,
            zerolinewidth = 1,
            zerolinecolor = "rgba(0, 0, 0, 0.3)",
            range = ylim
          ),
          hovermode = "x unified"
        )
        p
      })

      # Arrange subplots in a vertical layout with shared x-axis
      final_plot <- plotly::subplot(
        plots,
        nrows = n_channels,
        shareX = TRUE,
        shareY = TRUE
        # titleX = TRUE
      )
      final_plot <- plotly::layout(
        final_plot,
        title = list(
          text = "Customized Traces in Different Subplots",
          x = 0.05,  # Adjust slightly to align with y-axis line
          xanchor = "left"  # Keep alignment from left
        )
      )
      final_plot
    }, {
      oldpar <- graphics::par(
        mfrow = n2mfrow(n_channels, asp = 3),
        mar = c(3.3, 3.8, 2.1, 0.5)
      )
      on.exit({ graphics::par(oldpar) })

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
        graphics::axis(2, at = pretty(ylim), las = 1)

        graphics::mtext(side = 1, text = "Time (s)", line = 1.8)
        graphics::mtext(side = 2, text = data$physical_unit, line = 2.8)
        title_text <- paste(
          collapse = ", ",
          c(
            sprintf("Ch %s", channel),
            pretty_frequency_range(data$frequency_range)
          )
        )
        graphics::title(main = title_text, line = 0.5, adj = 0)

        graphics::legend(
          "topright",
          sprintf("%s (N=%d)", data$condition_groups$name, data$condition_groups$n_trials),
          lty = 1, col = col, bty = "n"
        )
      }
    }
  )


}



