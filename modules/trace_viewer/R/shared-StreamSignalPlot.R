`%OF%` <- dipsaus::`%OF%`

StreamSignalPlot <- R6::R6Class(
  classname = "StreamSignalPlot",
  portable = FALSE,
  private = list(
    # plotly instance
    .impl = NULL,
    # for shiny output and proxy
    .output_id = character(),


    .start_time = numeric(),
    .sample_rates = numeric(),
    .data = NULL,    # list of channel signals
    .channel_names = character(),
    .channel_gap = numeric(),
    .channel_gap_needs_update = FALSE,
    .channel_color = character(),
    .channel_lwd = numeric(),
    .channel_needs_update = logical(),


    .title = character(),
    .xlab = character(),
    .ylab = character(),
    .ytick_size = numeric(),
    .axes_needs_update = FALSE,


    .annotations = NULL,
    .annotations_needs_update = FALSE,


    prepare_annotations = function() {
      annot_table <- private$.annotations
      max_duration <- self$max_duration
      start_time <- self$start_time
      end_time <- start_time + max_duration
      font_size <- self$channel_ticksize

      if(!is.data.frame(annot_table) || nrow(annot_table) == 0) {
        return(list(
          margin_top = font_size * 6 + 16,
          vlines = list(),
          annots = list(),
          ranges = start_time + c(0, max_duration)
        ))
      }
      # annot_table <- data.frame(time = runif(10) * 2, label = letters[1:10], color = 1:10)
      n_annots <- nrow(annot_table)

      annot_colors <- annot_table$color
      if(length(annot_colors)) {
        annot_colors[annot_colors == "#FFFFFF00"] <- graphics::par("fg")
      } else {
        annot_colors <- rep(graphics::par("fg"), n_annots)
      }
      annot_colors <- grDevices::col2rgb(annot_colors, alpha = FALSE)
      annot_colors <- sprintf("rgb(%d,%d,%d)", annot_colors[1, ],
                              annot_colors[2, ], annot_colors[3, ])

      events <- lapply(seq_len(n_annots), function(ii) {
        t0 <- annot_table$time[[ii]]
        if(t0 > end_time || t0 < start_time) { return(NULL) }

        label <- annot_table$label[[ii]]

        list(
          shape = list(
            type  = "line",
            # build shapes against x2 so they ignore the main zoom
            xref  = "x2",       # IMPORTANT: use the full-range axis
            yref  = "paper",
            x0    = t0, x1 = t0,
            y0    = 0,  y1 = 1,
            line  = list(color = annot_colors[[ii]], width = 1),
            layer = "below"     # draw above traces
          ),
          annot = list(
            xref = "x2", yref = "paper",
            x = t0, y = 1,                # top edge of plotting area
            xanchor = "center", yanchor = "bottom",
            yshift = 2,                   # tiny offset into the margin
            text = label,
            showarrow = FALSE,
            font = list(size = font_size),
            align = "center",
            bgcolor = "rgba(255,255,255,0.6)",  # subtle background to improve legibility
            bordercolor = "rgba(0,0,0,0.2)",
            borderwidth = 0.5
          ),
          time = t0
        )
      })
      events <- events[!vapply(events, is.null, FALSE)]

      if(!length(events)) {
        return(list(
          margin_top = font_size * 6 + 16,
          vlines = list(),
          annots = list(),
          ranges = start_time + c(0, max_duration)
        ))
      }

      return(list(
        margin_top = font_size * 6 + 16,
        vlines = lapply(events, "[[", "shape"),
        annots = lapply(events, "[[", "annot"),
        ranges = start_time + c(0, max_duration)
      ))
    }

  ),
  active = list(

    channel_names = function(v) {
      if(!missing(v)) {
        v <- as.character(v)
        if(length(v) != length(private$.channel_names)) {
          stop("Inconsistent number of channels to visualize.")
        }
        if(anyDuplicated(v) || anyNA(v)) {
          stop("Channel names cannot be duplicated nor N/A")
        }
        if(!identical(private$.channel_names, v)) {
          private$.channel_needs_update[private$.channel_names != v] <- TRUE
          private$.channel_names <- v
          self$needs_update <- TRUE
        }
      }
      private$.channel_names
    },

    channel_colors = function(v) {
      if(!missing(v)) {
        v[!is.na(v)] <- grDevices::adjustcolor(v[!is.na(v)])
        n_channels <- length(private$.channel_names)
        if(length(v) == 1) {
          v <- rep(v, n_channels)
        } else if(length(v) != n_channels) {
          stop("Inconsistent number of colors.")
        }
        if(!identical(private$.channel_color, v)) {
          private$.channel_needs_update[private$.channel_color != v] <- TRUE
          private$.channel_color <- v
          self$needs_update <- TRUE
        }
      }
      private$.channel_color
    },

    channel_linewidth = function(v) {
      if(!missing(v)) {
        v <- as.double(v)
        v[!is.finite(v)] <- 1
        n_channels <- length(private$.channel_names)
        if(length(v) == 1) {
          v <- rep(v, n_channels)
        } else if(length(v) != n_channels) {
          stop("Inconsistent number of linewidths.")
        }
        if(!identical(private$.channel_lwd, v)) {
          private$.channel_needs_update[private$.channel_lwd != v] <- TRUE
          private$.channel_lwd <- v
          self$needs_update <- TRUE
        }
      }
      private$.channel_lwd
    },

    channel_gap = function(v) {
      gap <- private$.channel_gap
      if(!missing(v)) {
        v <- unname(as.numeric(v)[[1]])
        stopifnot(is.finite(v))
        if(!isTRUE(gap == v)) {
          private$.channel_gap <- v
          gap <- v
          private$.channel_gap_needs_update <- TRUE
          private$.channel_needs_update[] <- TRUE
          self$needs_update <- TRUE
        }
      } else if( gap <= 0 ) {
        # automatic
        gap <- quantile(abs(unlist(private$.data)), na.rm = TRUE, probs = 0.999) * 2
      }
      gap
    },

    channel_ticksize = function(v) {
      if(!missing(v)) {
        v <- as.integer(v)[[1]]
        stopifnot(isTRUE(v > 0))
        if(private$.ytick_size != v) {
          private$.ytick_size <- v
          private$.axes_needs_update <- TRUE
          self$needs_update <- TRUE
        }
      }
      private$.ytick_size
    },

    start_time = function(v) {
      if(!missing(v)) {
        v <- as.numeric(v)[[1]]
        stopifnot(is.finite(v))
        if(private$.start_time != v) {
          private$.start_time <- v
          private$.channel_needs_update[] <- TRUE
          private$.annotations_needs_update <- TRUE
          self$needs_update <- TRUE
        }
      }
      private$.start_time
    },

    sample_rates = function(v) {
      if(!missing(v)) {
        v <- as.double(v)
        if(anyNA(v) || any(v <= 0)) {
          stop("Sample rates cannot be N/A or negative")
        }
        n_channels <- length(private$.channel_names)
        if(length(v) == 1) {
          v <- rep(v, n_channels)
        } else if(length(v) != n_channels) {
          stop("Inconsistent number of sample rates.")
        }
        if(!identical(private$.sample_rates, v)) {
          private$.sample_rates <- v
          private$.channel_needs_update[] <- TRUE
          self$needs_update <- TRUE
        }
      }
      private$.sample_rates
    },

    durations = function() {
      n_timepoints <- vapply(private$.data, length, 0L)
      n_timepoints / private$.sample_rates
    },

    max_duration = function() {
      max(self$durations)
    },

    title = function(v){
      if(!missing(v)) {
        v <- paste(as.character(v), collapse = " ")
        if(!identical(private$.xlab, v)) {
          private$.title <- v
          private$.axes_needs_update <- TRUE
          self$needs_update <- TRUE
        }
      }
      private$.title
    },

    xlab = function(v) {
      if(!missing(v)) {
        v <- paste(as.character(v), collapse = " ")
        if(!identical(private$.xlab, v)) {
          private$.xlab <- v
          private$.axes_needs_update <- TRUE
          self$needs_update <- TRUE
        }
      }
      private$.xlab
    },

    ylab = function(v) {
      if(!missing(v)) {
        v <- paste(as.character(v), collapse = " ")
        if(!identical(private$.ylab, v)) {
          private$.ylab <- v
          private$.axes_needs_update <- TRUE
          self$needs_update <- TRUE
        }
      }
      private$.ylab
    },

    annotations = function(v) {
      if(!missing(v)) {
        if( length(v) ) {
          # time, event, group, color
          if(!is.data.frame(v)) {
            stop("Annotations must be NULL or a data frame")
          }
          if(!all(c("time", "label") %in% names(v)) || !is.numeric(v$time)) {
            stop("Annotation must have column `time` (numeric) and `label` (character)")
          }
          v <- v[!is.na(v$time), ]
          if(nrow(v)) {
            v$label <- as.character(v$label)
            if(length(v$color)) {
              v$color <- grDevices::adjustcolor(v$color)
            }
          } else {
            v <- NULL
          }
        } else {
          v <- NULL
        }
        private$.annotations <- v
        private$.annotations_needs_update <- TRUE
        self$needs_update <- TRUE
      }
      private$.annotations
    }

  ),
  public = list(

    #' @field needs_update integer flag indicating whether the plot needs to
    #' be updated: 0 means no update is needed; 1 means underlying data is
    #' updated; 2 means
    needs_update = FALSE,

    #' @field MAX_POINTS maximum number of points to plot before down-sampling
    #' kicks in
    MAX_POINTS = 500000,

    initialize = function(n_channels, sample_rates,
                          start_time = 0,
                          channel_names = sprintf("ch%04d", seq_len(n_channels)),
                          channel_gap = 0, title = "",
                          xlab = "Time (s)", ylab = "Channel", ytick_size = 8) {
      if(!ravepipeline:::package_installed("plotly")) {
        stop("Signal plot with streaming data requires installing package plotly. Please run `install.packages('plotly')`")
      }

      if(missing(n_channels)) {
        n_channels <- length(channel_names)
      } else {
        stopifnot(length(channel_names) == n_channels)
      }
      if(anyDuplicated(channel_names)) {
        stop("Channel names must be unique")
      }

      # DIPSAUS DEBUG START
      # n_channels <- 10
      # sample_rate = 100
      # start_times <- 0
      # channel_names = sprintf("ch%04d", seq_len(n_channels))
      # channel_gap <- 0.0
      # xlab = "Time (s)"
      # ylab = "Channel"
      # private <- list()

      stopifnot(all(sample_rates > 0))
      if(length(sample_rates) == 1) {
        sample_rates <- rep(sample_rates, n_channels)
      } else if(length(sample_rates) != n_channels) {
        stop("Channel `sample_rates` must have length of 1 or the same size as the total channel count")
      }


      private$.start_time <- start_time
      private$.sample_rates <- sample_rates
      private$.channel_names <- channel_names
      private$.channel_color <- rep(NA, n_channels)
      private$.channel_lwd <- rep(1, n_channels)

      private$.data <- lapply(seq_len(n_channels), function(ch) {
        rep(0.0, 2)
      })

      # automatic gap according to the data median
      self$channel_gap <- channel_gap
      self$title <- title
      self$xlab <- xlab
      self$ylab <- ylab
      private$.ytick_size <- ytick_size

      private$.impl <- plotly::plot_ly(type = "scattergl", mode = "lines")
      self$needs_update <- TRUE
      private$.channel_needs_update <- rep(TRUE, n_channels)
    },

    get_channel_info = function(n) {
      n_channels <- length(private$.channel_names)
      if(is.numeric(n)) {
        stopifnot(isTRUE(n >= 1 && n <= n_channels))
        list(
          index = n,
          name = private$.channel_names[[n]]
        )
      } else {
        idx <- which(private$.channel_names == n)
        if(!length(idx)) {
          stop("Cannot find channel by name ", paste(n, collapse = ""))
        }
        list(
          index = idx[[1]],
          name = n
        )
      }
    },

    set_channel_data = function(n, data) {
      ch_info <- self$get_channel_info(n)
      private$.data[[ch_info$index]] <- data
      private$.channel_needs_update[[ch_info$index]] <- TRUE
      private$.annotations_needs_update <- TRUE
      self$needs_update <- TRUE
      invisible(self)
    },

    proxy_update_annotations = function(proxy) {
      if(!private$.annotations_needs_update) { return() }
      # annotations
      event_decor <- private$prepare_annotations()

      plotly::plotlyProxyInvoke(
        proxy, "relayout",
        list(
          "shapes" = I(event_decor$vlines),
          "annotations" = I(event_decor$annots),
          "margin.t" = event_decor$margin_top,

          "xaxis2.range" = I(event_decor$ranges)
          # "xaxis2.range" = I(c(start_time, start_time + duration)),
        )
      )
      private$.annotations_needs_update <- FALSE
    },

    proxy_update_channels = function(proxy) {

      channels_to_update <- which(private$.channel_needs_update)
      if(!length(channels_to_update)) { return() }

      channel_gap <- self$channel_gap
      channel_names <- self$channel_names
      n_channels <- length(channel_names)
      sample_rates <- self$sample_rates
      start_time <- self$start_time

      # estimate down-sampling
      total_timepoints <- vapply(private$.data, length, 0L)
      n_timepoints <- max(total_timepoints)
      duration <- max(total_timepoints / sample_rates)
      ratio <- ceiling(sum(total_timepoints) / self$MAX_POINTS)

      # underlying time and data
      plot_data <- lapply(channels_to_update, function(channel_ii) {
        s <- private$.data[[channel_ii]]
        if(ratio > 1 && length(s) > 20) {
          s <- ravetools::decimate(s, ratio)
          tm <- (seq_along(s) - 1L) * (ratio / sample_rates[[channel_ii]])
        } else {
          tm <- (seq_along(s) - 1L) / sample_rates[[channel_ii]]
        }

        list(time = tm + start_time, data = s)
        # s + channel_gap * (n_channels + 1 - ii)
      })

      # data to plot
      data <- lapply(seq_along(channels_to_update), function(ii) {
        channel_ii <- channels_to_update[[ii]]
        plot_data[[ii]]$data + channel_gap * (n_channels + 1 - channel_ii)
      })

      # time
      time <- lapply(seq_along(channels_to_update), function(ii) {
        plot_data[[ii]]$time
      })

      # hover-text
      text <- lapply(seq_along(channels_to_update), function(ii) {
        item <- plot_data[[ii]]
        channel_ii <- channels_to_update[[ii]]
        sprintf("%.2f [%s, t=%.3f]", item$data, channel_names[[channel_ii]], item$time)
      })

      # color
      colors <- private$.channel_color[channels_to_update]
      colors[is.na(colors)] <- graphics::par("fg")
      colors <- as.list(grDevices::adjustcolor(colors))

      # line widths
      linewidths <- as.list(private$.channel_lwd[channels_to_update])

      # push new y values into first trace
      plotly::plotlyProxyInvoke(
        proxy, "restyle",
        list(
          x = I(time),
          y = I(data),
          text = I(text),
          "line.color" = I(colors),
          "line.width" = I(linewidths)
        ),
        as.list(channels_to_update)
      )

      relayout_list <- list(
        "xaxis.range" = I(c(start_time, start_time + duration)),
        "yaxis.tickvals" = rev(seq_len(n_channels)) * channel_gap,
        "yaxis.ticktext" = channel_names
      )
      if(private$.channel_gap_needs_update || !isTRUE(private$.channel_gap > 0)) {
        relayout_list[["yaxis.range"]] <- c(0, channel_gap * (n_channels + 1))
        private$.channel_gap_needs_update <- FALSE
      }
      plotly::plotlyProxyInvoke(proxy, "relayout", relayout_list)

      private$.channel_needs_update[] <- FALSE
      return()
    },

    proxy_update_axes = function(proxy) {

      if(!private$.axes_needs_update) { return() }

      plotly::plotlyProxyInvoke(
        proxy, "relayout",
        list(
          "title.text" = self$title,
          "xaxis.title.text" = self$xlab,
          "yaxis.title.text" = self$ylab,
          "yaxis.tickfont.size" = self$channel_ticksize
        )
      )
      private$.axes_needs_update <- FALSE
    },

    update = function(proxy) {
      if(!self$needs_update) { return(invisible(self)) }

      self$proxy_update_annotations(proxy)
      self$proxy_update_channels(proxy)
      self$proxy_update_axes(proxy)

      invisible(self)
    },

    render = function() {
      if(self$needs_update) {
        # prepare initial plot data
        channel_names <- private$.channel_names
        sample_rates <- private$.sample_rates
        channel_gap <- self$channel_gap

        start_time <- private$.start_time
        n_channels <- length(channel_names)

        # estimate down-sampling
        total_timepoints <- vapply(private$.data, length, 0L)
        max_duration <- max(total_timepoints / sample_rates)
        n_timepoints <- max(total_timepoints)
        ratio <- ceiling(sum(total_timepoints) / self$MAX_POINTS)

        colors <- private$.channel_color
        colors[is.na(colors)] <- graphics::par("fg")
        colors <- grDevices::adjustcolor(colors)

        plot_data <- lapply(seq_len(n_channels), function(ii) {
          channel_name <- channel_names[[ii]]
          channel_data <- private$.data[[ii]]
          if(ratio > 1 && length(channel_data) > 20) {
            channel_data <- ravetools::decimate(channel_data, ratio)
            time <- (seq_along(channel_data) - 1) * (ratio / sample_rates[[ii]])
          } else {
            time <- (seq_along(channel_data) - 1) / sample_rates[[ii]]
          }
          list(
            Time = time + start_time,
            Data = channel_data,
            y = channel_data + channel_gap * (n_channels + 1 - ii),
            Name = channel_name,
            Color = colors[[ii]],
            Linewidth = private$.channel_lwd[[ii]]
          )
        })
        plot_data <- data.table::rbindlist(plot_data, use.names = TRUE)
        impl <- plotly::plot_ly(type = "scattergl", mode = "lines")
        impl <- plotly::add_trace(
          impl,
          data = plot_data,
          x = ~ Time,
          y = ~ y,
          split = ~ Name,
          line = list(
            color = ~ Color,
            width = ~ Linewidth
          ),
          text = ~ sprintf("%.2f [%s, t=%.3f]", Data, Name, Time),  # keep original values
          hoverinfo = "text"  # show custom text and time only
        )

        event_decor <- private$prepare_annotations()

        impl <- plotly::layout(
          impl,
          showlegend = FALSE,

          shapes = event_decor$vlines,
          annotations = event_decor$annots,
          margin = list(t = event_decor$margin_top),

          title = list(
            text = self$title,
            x = 0,
            xanchor = "left",
            xref = "paper",
            y = 0.98,
            yanchor = "center",
            font = list(size = 16)
          ),

          xaxis = list(
            title = private$.xlab,
            range = start_time + c(0, max_duration)
          ),
          # hidden full-range axis to anchor vlines
          xaxis2 = list(
            overlaying = "x",  # share the same plotting domain
            matches    = "x",
            visible    = FALSE,
            # range      = event_decor$ranges,
            range = start_time + c(0, max_duration),
            fixedrange = FALSE
          ),
          yaxis = list(
            title = private$.ylab,
            tickmode = "array",
            tickvals = rev(seq_len(n_channels)) * channel_gap,
            ticktext = channel_names,
            tickfont = list(size = private$.ytick_size),
            range = c(0, channel_gap * (n_channels + 1)),   # tight range
            automargin = TRUE
          )
        )
        private$.impl <- impl
        self$needs_update <- FALSE
      }
      private$.impl
    }

  )
)
