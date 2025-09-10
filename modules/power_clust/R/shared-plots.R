diagnose_cluster <- function(cluster_result, k, combined_group_results, col_label = threeBrain:::DEFAULT_COLOR_DISCRETE) {
  # cluster_result <- clustering_tree
  # k <- 2
  average_responses <- combined_group_results$combined_average_responses
  result <- calc_final_cut_tree(cluster_result, k, cvi = FALSE)

  channel_order <- order(result$cluster)

  channel_color <- col_label[result$cluster]

  channels <- combined_group_results$electrode_channels
  channels_reordered <- channels[channel_order]
  channel_color_reordered <- channel_color[channel_order]
  sample_rate <- combined_group_results$sample_rate

  myCol <- grDevices::colorRampPalette(
    (c(
      "#053061",
      "#2166ac",
      "#4393c3",
      "#92c5de",
      "#d1e5f0",
      "#ffffff",
      "#fddbc7",
      "#f4a582",
      "#d6604d",
      "#b2182b",
      "#67001f"
    ))
  )(101)

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(oldpar) })

  graphics::layout(
    mat = matrix(c(1, 2), nrow = 1),
    widths = c(1, graphics::lcm(3))
  )

  graphics::par(mar = c(4.1, 4.1, 4.1, 0.1))

  zlim <- range(average_responses, na.rm = TRUE)
  zlim <- max(abs(zlim)) * c(-1, 1)
  graphics::image(
    z = average_responses[, channel_order, drop = FALSE],
    x = seq_len(nrow(average_responses)),
    y = seq_along(channels),
    axes = FALSE,
    xlab = "Time (s)",
    ylab = "Channel",
    col = myCol,
    zlim = zlim,
    main = sprintf("Cluster k=%d", result$k)
  )

  group_n_time_points <- combined_group_results$group_n_time_points
  group_start_offset <- combined_group_results$group_start_offset

  group_finish <- cumsum(group_n_time_points)
  group_separator <- c(1, group_finish)
  group_start <- group_separator[-length(group_separator)]
  group_center <- group_finish - group_n_time_points / 2
  axis(1L, at = group_separator, labels = rep("", length(group_separator)), tick = TRUE)

  # start_events <- combined_group_results$group_event_starts
  # start_events[tolower(start_events) %in% c("", "trial onset", "trial_onset")] <- "TrialOnset"
  group_start_offset_labels <- sprintf("%.2f s", group_start_offset)
  group_start_offset_labels[group_start_offset == 0] <- "0 s"
  axis(1L, at = group_start, labels = group_start_offset_labels, tick = FALSE, hadj = -0.1, cex.axis = 0.8, line = -1)

  durations <- sprintf("%.2f s", group_start_offset + group_n_time_points / combined_group_results$sample_rate)
  axis(1L, at = group_finish, labels = durations, tick = FALSE, hadj = 1.1, cex.axis = 0.8, line = -1)

  axis(
    3L,
    at = group_center,
    labels = combined_group_results$group_labels,
    tick = FALSE, line = -0.7
  )
  axis(
    1L,
    at = group_center,
    labels = sprintf("Group %s", combined_group_results$group_indexes),
    tick = FALSE, line = 0
  )
  abline(v = group_separator)

  abline(h = cumsum(table(result$cluster)) + 0.5)

  channel_y_at <- seq_along(channels)
  channel_y_label <- channels_reordered
  for(cls in unique(channels_reordered)) {
    sel <- channel_y_label == cls
    axis(
      2L,
      at = channel_y_at[sel],
      labels = channel_y_label[sel],
      las = 1,
      tick = FALSE,
      col.axis = channel_color_reordered[sel][[1]]
    )
  }


  par(mar = c(4.1, 2.6, 4.1, 0.1))
  legend_z <- seq(zlim[[1]], zlim[[2]], length.out = 101)
  graphics::image(
    z = matrix(legend_z, nrow = 1),
    x = 0, y = legend_z,
    axes = FALSE, xlab = "", ylab = "",
    col = myCol, zlim = zlim
  )

  axis(2L, at = c(zlim, 0), labels = rep("", 3), tick = TRUE, tcl = -0.3)
  axis(2L, at = c(zlim, 0), las = 1, labels = c(sprintf("%.1f", zlim), "0"), cex.axis = 0.8, tick = FALSE, line = -0.4)

  invisible()

}


rect_hclust2 <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = threeBrain:::DEFAULT_COLOR_DISCRETE,  cluster = NULL)
{
  if (length(h) > 1L || length(k) > 1L)
    stop("'k' and 'h' must be a scalar")
  if (!is.null(h)) {
    if (!is.null(k))
      stop("specify exactly one of 'k' and 'h'")
    k <- min(which(rev(tree$height) < h))
    k <- max(k, 2)
  }
  else if (is.null(k))
    stop("specify exactly one of 'k' and 'h'")
  if (k < 2 || k > length(tree$height))
    stop(gettextf("k must be between 2 and %d", length(tree$height)),
         domain = NA)
  if (is.null(cluster)) {
    cluster <- cutree(tree, k = k)
  }

  clustab <- table(cluster)[unique(cluster[tree$order])]
  cluster_order <- as.integer(names(clustab))
  m <- c(0, cumsum(clustab))

  if (!is.null(x)) {
    if (!is.null(which)) {
      stop("specify exactly one of 'which' and 'x'")
    }
    which <- x
    for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
  } else if (is.null(which)) {
    which <- 1L:k
  }
  if (any(which > k)) {
    stop(gettextf("all elements of 'which' must be between 1 and %d",
                  k), domain = NA)
  }
  # border <- rep_len(border, length(which))
  retval <- list()
  for (n in seq_along(which)) {
    actual_cluster_id <- cluster_order[[which[n]]]
    color <- border[actual_cluster_id]
    rect(
      xleft = m[which[n]] + 0.66,
      ybottom = par("usr")[3L],
      xright = m[which[n] +1] + 0.33,
      ytop = mean(rev(tree$height)[(k - 1):k]),
      border = color,
      col = adjustcolor(color, alpha.f = 0.3),
    )
    retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
  }
  invisible(retval)
}
