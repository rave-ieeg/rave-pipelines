# Trial-level CRP parameters, one panel per condition group.
#
# `x$data$group_data[[ii]]$params` is a trial x channel matrix for condition group
# `ii`; `x$electrodes` is its channel axis and `x$data$parameter_name` names the
# `crp_df` column it was extracted from. The parameter is carried in the object, so
# a single function renders `data_crp_param_alpha_prime`, `_snr`, `_expl_var` and
# anything else `prepare_data_crp_param_by_trial_channel()` produces.


# Axis title for a CRP parameter. Unknown names fall back to the column name.
crp_param_label <- function(parameter_name) {
  label <- CRP_PARAM_LABELS[[as.character(parameter_name)]]
  if (is.null(label)) { label <- as.character(parameter_name) }
  label
}


plot_data_crp_param_by_trial_channel_heatmap <- function(
    x, electrode_mask = NULL,
    space = use_plot_space_resolved()$space,
    space_mode = use_plot_space_resolved()$space_mode,
    channel_annotation = use_channel_annotation_style(), cex = use_cex(),
    mfrow = NULL, col = use_continuous_colormap()$colors, ...) {

  space_mode <- match.arg(space_mode, choices = OPTIONS_SPACE_MODE)
  channel_annotation <- match.arg(channel_annotation, choices = OPTIONS_CHAN_ANNOT)

  sel <- resolve_channel_selection(x, electrode_mask)
  chan_names <- channel_names(sel$coord_table, channel_annotation)
  n_channels <- sel$n

  group_data <- x$data$group_data
  n_groups <- length(group_data)
  if (!n_groups) { return(invisible()) }

  # Masked trial x channel matrices, shared across the range and limit calculations
  params <- lapply(group_data, function(d) {
    d$params[, sel$index, drop = FALSE]
  })

  # Colour scale follows the data, not the parameter: symmetric diverging when the
  # values straddle zero (e.g. the signed fitted amplitude `al_p`), sequential from
  # zero when they do not (`snr`, `expl_var`).
  data_range <- range(unlist(params), na.rm = TRUE)
  diverging <- isTRUE(data_range[[1]] < 0 && data_range[[2]] > 0)

  limit <- get_spacing(params, space = space, space_mode = space_mode)
  vlim <- if (diverging) {
    c(-limit, limit)
  } else if (data_range[[2]] <= 0) {
    c(-limit, 0)
  } else {
    c(0, limit)
  }

  if (length(col) == 0) {
    col <- if (diverging) {
      c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
        "#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
    } else {
      c("#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
    }
  }
  if (length(col) < 101) {
    col <- grDevices::colorRampPalette(col)(101)
  }

  mfrow <- get_mfrow(n = n_groups, mfrow = mfrow, asp = 3)

  par_opt <- prepare_par(cex = cex)
  mar <- par_opt$mar

  # Reserve the last column for one colour bar per panel row
  lmat <- matrix(seq_len(prod(mfrow)), nrow = mfrow[[1]], byrow = TRUE)
  lmat <- cbind(lmat + mfrow[[1]], seq_len(mfrow[[1]]))
  graphics::layout(lmat, widths = c(rep(1, mfrow[[2]]), graphics::lcm(3)))

  graphics::par(mar = c(mar[[1]], 3.5, mar[[3]], mar[[4]]), cex = 1)
  for (ii in seq_len(mfrow[[1]])) {
    add_heatmap_legend(vlim = vlim, col = col,
                       title = crp_param_label(x$data$parameter_name), cex = cex)
  }

  graphics::par(mar = mar, cex = 1)

  for (ii in seq_len(n_groups)) {

    group <- group_data[[ii]]$group

    # trial x channel; clamp so out-of-limit values stay at the ends of the scale
    z <- params[[ii]]
    z[z < vlim[[1]]] <- vlim[[1]]
    z[z > vlim[[2]]] <- vlim[[2]]

    n_trials <- nrow(z)

    graphics::image(
      x = seq_len(n_channels),
      y = seq_len(n_trials),
      z = t(z),
      axes = FALSE,
      xlab = "",
      ylab = "",
      zlim = vlim,
      main = "",
      adj = 0,
      col = col,
      cex = cex,
      cex.main = par_opt$cex.main * cex,
      cex.lab = par_opt$cex.lab * cex
    )

    graphics::axis(
      side = 1L, at = seq_len(n_channels), labels = chan_names, las = 2,
      tck = -0.005 * (3 + cex), cex = cex,
      cex.axis = par_opt$cex.axis * cex)

    add_axis_trial_number(group, cex = cex)

    graphics::title(
      main = bquote(.(group$label) ~ scriptstyle("(n =" ~ .(group$n_trials) * ")")),
      adj = 0,
      cex.main = par_opt$cex.main * cex
    )

  }

  invisible()
}


# `plot()` is the scripting entry point, so its defaults are stated outright
# rather than read from the preference store: the same call has to produce the
# same figure on any machine. The module UI calls `plot_data_*()` instead, whose
# defaults do follow the user's preferences.
plot.data_crp_param_by_trial_channel <- function(
    x, type = "heatmap",
    space = DEFAULT_PLOT_SPACE / 100, space_mode = OPTIONS_SPACE_MODE,
    channel_annotation = DEFAULT_CHAN_ANNOT, cex = DEFAULT_CEX,
    col = ravepipeline::CONTINUOUS_COLORMAPS(DEFAULT_CONTINUOUS_COLORMAP), ...) {
  type <- match.arg(type, choices = "heatmap")
  plot_data_crp_param_by_trial_channel_heatmap(
    x = x, space = space, space_mode = space_mode,
    channel_annotation = channel_annotation, cex = cex, col = col, ...
  )
}
