`%>%` <- magrittr::`%>%`
`%<>%` <- magrittr::`%<>%`
`%?<-%` <- dipsaus::`%?<-%`

rave_cex.main <- 1.5
rave_cex.axis <- 1.3
# putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
# the left margin to compensate
rave_cex.lab <- 1.4
rave_axis_tcl = -0.3

rave_color_ramp_palette <-
  colorRampPalette(c('navy', 'white', 'red'),
                   interpolate = 'linear',
                   space = 'Lab')
rave_color_ramp_dark_palette <-
  colorRampPalette(c('#13547a', 'black', '#ff758c'),
                   interpolate = 'linear',
                   space = 'Lab')

..dark_blue_to_red <-
  rev(
    c(
      "#67001f",
      "#b2182b",
      "#d6604d",
      "#f4a582",
      "#fddbc7",
      "#ffffff",
      "#d1e5f0",
      "#92c5de",
      "#4393c3",
      "#2166ac",
      "#053061"
    )
  )
..light_blue_to_light_red <-
  c(..dark_blue_to_red[5:1], 'black', ..dark_blue_to_red[11:7])
..light_blue_to_gray_to_light_red <-
  c(..dark_blue_to_red[5:1], '#1E1E1E', ..dark_blue_to_red[11:7])

rave_color_ramp_palette <-
  colorRampPalette(..dark_blue_to_red, interpolate = 'linear', space = 'Lab')
rave_heat_map_colors <- rave_color_ramp_palette(1001)

rave_color_ramp_dark_palette <-
  colorRampPalette(..light_blue_to_light_red,
                   interpolate = 'linear',
                   space = 'Lab')
rave_color_ramp_gray_palette <-
  colorRampPalette(..light_blue_to_gray_to_light_red,
                   interpolate = 'linear',
                   space = 'Lab')

rave_heat_map_dark_colors <- rave_color_ramp_dark_palette(1001)
rave_heat_map_gray_colors <- rave_color_ramp_gray_palette(1001)

# put this here for legacy, but we need to exterminate these references
crp <- rave_heat_map_colors

group_colors <-
  c('orange',
    'dodgerblue3',
    'darkgreen',
    'orangered',
    'brown',
    'purple3')


..get_nearest_i <- function(from,to) {
  sapply(from, function(.x) which.min(abs(.x-to)))
}
..get_nearest <- ..get_nearest_i

..get_nearest_val <- function(from,to) {
  to[..get_nearest_i(from,to)]
}

#
axis_label_decorator <- function(plot_data, col, Xmap=force, Ymap=force, label_alignment=TRUE, label_alignment.line = 2, ...) {
  # here we are assuming that everything in plot_data
  # is of the same x/y type

  # we  need to check if we've been give a list of things to plot (as is common for line plots),
  # or a single thing (as is common for heatmaps)
  # test if there are plot variables at the highest level, if so, then we are in the latter condition
  pd <- plot_data
  if(is.null(pd[['has_trials']])) {
    ii = which(get_list_elements(pd, 'has_trials'))[1]
    pd <- pd[[ii]]
  }

  # if(!is.null(pd$trial_alignment) && label_alignment) {
  #     rave_axis(1, at=Xmap(0), labels = pd$trial_alignment,
  #               mgpx=c(1,label_alignment.line,1), lwd=0, tcl=0, ...)
  # }
  #
  rave_axis_labels(xlab=attr(pd$data, 'xlab'), ylab=attr(pd$data, 'ylab'), ...)
}

#' @author John Magnotti
#' @title RAVE custom image plotter
#' @param mat z-matrix
#' @param x,y z and y axis
#' @param col vector of colors, color palette
#' @param zlim value to trim zmat
#' @param log which axis will be in log scale
#' @param useRaster,... passed to image()
#' @param clip_to_zlim whether to clip mat
#' @param add logical, whether to overlay current plot to an existing image
#' @description The idea here is to to separate the plotting of the heatmap from all the accoutrements that are done in the decorators.
#' We are just plotting image(mat) Rather Than t(mat) as you might expect. The Rave_calculators know this so we can save a few transposes along the way.
make_image <- function(mat, x, y, zlim, col, log='', useRaster=TRUE, clip_to_zlim=TRUE, add=TRUE) {
  #xlab='Time (s)', ylab='Frequency (Hz)', zlim, log='', useRaster=TRUE, PANEL.FIRST=NULL, PANEL.LAST=NULL, ...) {
  # zmat %<>% clip_x(lim=zlim)

  if(missing(zlim)) {
    zlim <- c(-1,1)*max(abs(mat))
  } else {
    # if zlim is missing, then the zlim will be set symmetrically based on the range
    # of the data (in the 'if' block above), so we only have to worry about clipping if the range is passed in
    if(clip_to_zlim) {
      mat %<>% clip_x(zlim)
    }
  }

  col %?<-% if(par('bg') == 'black') {
    rave_heat_map_dark_colors
  } else if(par('bg') == '#1E1E1E') {
    rave_heat_map_gray_colors
  }else {
    rave_heat_map_colors
  }

  image(x=x, y=y, z=mat, zlim=zlim, col=col, useRaster=useRaster, log=log,
        add=add, axes=F, xlab='', ylab='', main='')

  # return the clipped zmat
  invisible(mat)
}
# for compatibility
# draw_img <- make_image

##RUTABAGA
median_ticks <- function(k, .floor=1) c(.floor, ceiling(k/2), k)

get_foreground_color <- function() {
  switch(par('bg'),
         'white' = 'black',
         'black' = 'white',
         '#1E1E1E' = 'gray70',
         'gray' = '#A5A5A5',
         'black'
  )
}

#works by side effect to change the palette used by the current graphics device
# and set the RAVE theme to light or dark
set_palette_helper <- function(results, plot_options, ...) {
  results %?<-% build_results_object(plot_options)

  .bg <- results$get_value('background_plot_color_hint', ifNotFound = 'white')
  .bg %?<-% 'white'

  # session = shiny::getDefaultReactiveDomain()
  if(tolower(.bg) %in%  c('white')) {
    theme = set_rave_theme('light')
  }else{
    theme = set_rave_theme('dark')
  }

  # setting the background color here triggers a cascade of color changes
  if(tolower(.bg) == 'gray') {
    par('bg'=rave_colors$DARK_GRAY)
  } else {
    par('bg'=.bg)
  }

  pal <- get_palette(results$get_value('color_palette'))

  if(results$get_value('invert_colors_in_palette', FALSE)) {
    pal %<>% invert_palette
  }

  if(results$get_value('reverse_colors_in_palette', FALSE)) {
    pal %<>% rev
  }

  set_palette(pal)

  par(col=get_foreground_color())

  invisible()
}

shiny_is_running <- function() {
  return(shiny::isRunning())

  # cls <- class(getDefaultReactiveDomain())
  # any(cls %in% c('ShinySession', 'session_proxy'))
}


# by default we use plot_title_options variable in results to see what to put in the title string
# callers can override this behavior by specifically dis-allowing certain options
# currently you can't force something to be TRUE if a user doesn't allow it, but we can think about
# this. If that's the case, all the allow_* would be NULL by default, and setting them to TRUE would override
# user preference. This seems rude at best, but for certain plots maybe they really require something to
# be put in the title?
title_decorator <- function(plot_data, plot_title_options,
                            allow_sid=TRUE, allow_enum=TRUE, allow_freq=TRUE,
                            allow_cond=TRUE, allow_sample_size=TRUE, ..., plot=TRUE) {
  title_string = ''

  # if(missing(plot_title_options)) {
  #     plot_title_options = build_plot_options()$plot_title_options
  # }

  # if we have multiple data, just take the first
  # the way to guess this is to check for the existence of a variable that we should have...
  if(is.null(plot_data[['name']])) {
    plot_data = plot_data[[1]]
  }

  # wraps do_on_inclusion to make the following lines easier to understand
  add_if_selected <- function(id, expr) {
    do_on_inclusion(id, expr, plot_title_options)
  }

  if(allow_cond)
    add_if_selected('Condition', {
      .name <- plot_data[['name']]
      if(isTRUE(nchar(.name) > 0)) {
        .name <- '' %&% .name
      }
      title_string = .name
    })

  # we could write this as a simple m/sapply if the variable names had a clear relationship to one another
  if(allow_sid)
    add_if_selected('Subject ID', {
      conditional_sep(title_string) = plot_data$subject_code
    })

  if(allow_enum)
    add_if_selected('Electrode #', {
      el <- dipsaus::deparse_svec(plot_data$electrodes, max_lag=1)
      # print('EL: ' %&% el)
      conditional_sep(title_string) = 'E' %&% el
    })

  if(allow_freq)
    add_if_selected('Frequency Range', {
      conditional_sep(title_string) = 'Freq ' %&% paste0(plot_data$frequency_window, collapse=':')
    })

  if(allow_sample_size)
    add_if_selected('Sample Size', {
      if(!is.null(plot_data$N))
        conditional_sep(title_string) = 'N=' %&% plot_data$N
    })

  # rave_title is an "additive" instead of replacement call, so rendering an empty string won't hurt anything,
  # but let's save a few needless function calls
  if(nchar(title_string) > 0 && plot) {
    rave_title(title_string)
  }

  invisible(title_string)
}

#' Function to get builtin color palettes
#' @param pname palette name
#' @param get_palette_names whether to get palette names
#' @param get_palettes ignored
#' @export
get_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
  # Some of these are from:
  # https://colorhunt.co/
  .palettes <- list(
    'OrBlGrRdBrPr' = c("orange", "dodgerblue3", "darkgreen", "orangered", "brown",
                       "purple3"),
    'Dark IV' = c('#11144c', '#3a9679', '#fabc60', '#e16262'),
    'Pastel IV' = c('#7fe7cc', '#dfe38e', '#efca8c', '#f17e7e'),
    'Twilight IV' = c('#e7eaf6', '#a2a8d3', '#38598b', '#113f67'),
    'Blues then Orange IV' = c('#070d59', '#1f3c88', '#5893d4', '#f7b633'),
    'Bright IV' = c('#ff62a5', '#ffe5ae', '#6b76ff', '#dee0d9')
  )

  if(missing(pname)) {
    if(get_palette_names)
      return (names(.palettes))

    return (.palettes)
  }

  pal <- .palettes[[pname]]
  if(is.null(pal)) {
    warning("Invalid palette requested: ", pname, ". Returning random palette")
    pal <- .palettes[[sample(seq_along(.palettes), 1)]]
  }

  return (pal)
}

# show power over time with MSE by condition
time_series_plot1 <- function(plot_data, PANEL.FIRST=NULL, PANEL.LAST=NULL, plot_time_range=NULL,
                              do_update_ylim=TRUE, axes=TRUE) {
  # check the plottable range, to make sure we're only plotting what the user has requested
  plot_time_range %?<-% get_data_range(plot_data, name = "x")


  for(ii in seq_along(plot_data)) {
    if (! all(plot_data[[ii]]$x %within% plot_time_range) ) {
      attrs = attributes(plot_data[[ii]]$data)

      ind <- plot_data[[ii]]$x %within% plot_time_range
      plot_data[[ii]]$x <- plot_data[[ii]]$x[ind]
      plot_data[[ii]]$data <- plot_data[[ii]]$data[ind,,drop=FALSE]

      attrs$dim = attributes(plot_data[[ii]]$data)$dim
      attributes(plot_data[[ii]]$data) = attrs

      # we need to update the range of the plots
      if(do_update_ylim) {
        plot_data[[ii]]$range <- .fast_range(plus_minus(plot_data[[ii]]$data))
      }
    }
  }

  xlim <- pretty(plot_time_range)#get_list_elements(plot_data, 'x') %>% unlist)
  ylim <- pretty(get_data_range(plot_data) %>% unlist, min.n=2, n=4)

  # dipsaus::cat2('MAR: ', paste0(par('mar'),collapse = ' '), level = 'INFO')
  # rutabaga::plot_clean(xlim, ylim)
  rutabaga::plot_clean(plot_data$x, ylim) #modified by LCR on Sept 21, 2022


  if(isTRUE(is.function(PANEL.FIRST))) PANEL.FIRST(plot_data)

  # draw the axes AFTER the first paneling, should this go in PANEL.FIRST?
  # it's weird because the PANEL.FIRST is responsible for labeling the axes, so why not for drawing them?
  # the counter is that we need the xlim and ylim to make the plot. So it's easy to just draw the labels here
  axes %<>% rep_len(4)
  # if(axes[1]) rave_axis(1, at=pretty(plot_data[[1]]$x), tcl=0, lwd=1) #modified by LCR on Sept 21, 2022
  # if(axes[2]) rave_axis(2, at=pretty(ylim), tcl=0, lwd=1) #modified by LCR on Sept 21, 2022
  if(axes[4]) rave_axis(4, at=pretty(ylim), tcl=0, lwd=1) #modified by LCR on Sept 21, 2022

  abline(h=0, col='gray70')

  # draw each time series
  for(ii in seq_along(plot_data)) {
    with(plot_data[[ii]], {
      if(has_trials) {
        ebar_polygon(x, data[,1], data[,2], add_line = TRUE, col=ii)
      }
    })
  }

  # if someone wants to add "top-level" decorations, now is the time
  if(isTRUE(is.function(PANEL.LAST))) PANEL.LAST(plot_data)

  invisible(plot_data)
}

set_font_scaling <- function(plot_options, FONT_SCALING = c('shiny', 'Rutabaga', 'R')) {
  FONT_SCALING = match.arg(FONT_SCALING)

  font_opts = switch(FONT_SCALING,
                     # here mostly making things bigger
                     'shiny' = list(
                       cex.main = 1.5,
                       cex.axis = 1.3,
                       # putting this to 1.4 because 1.5 causes some clipping of the axis(2) label,
                       # we could also try to increase the left margin to compensate
                       cex.lab = 1.4,
                       # ticks are still outward, but shorter than normal
                       tcl = -0.3
                     ),
                     list(
                       cex.main = 1.2,
                       cex.axis = 1,
                       cex.lab = 1,
                       tcl = -0.5
                     )
  )

  if (FONT_SCALING == 'Rutabaga') {
    # same as R, but shorter ticks are a must!
    font_opts$tcl = -0.3
  }

  # check if we're using a fast_map
  if(is.function(plot_options[['mset']])) {
    plot_options$mset(.list = font_opts)
  } else {
    plot_options[names(font_opts)] = font_opts
  }

  return(plot_options)
}

build_plot_options <- function(..., FONT_SCALING=c('shiny', 'Rutabaga', 'R')) {
  # this works
  options <- fastmap::fastmap()

  #this does not work
  # options <- dipsaus::fastmap2()
  # options$mset(a=2, b=3)

  options$mset(
    plot_time_range = c(-Inf,Inf),
    draw_decorator_labels = FALSE,
    plot_title_options = c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range',
                           'Sample Size', 'Baseline Window', 'Analysis Window'),

    background_plot_color_hint = 'white',

    color_palette = 'Beautiful Field',
    invert_colors_in_palette = FALSE,
    reverse_colors_in_palette = FALSE,

    heatmap_color_palette = get_heatmap_palette(get_palette_names = TRUE)[1],
    heatmap_number_color_values = 101,
    invert_colors_in_heatmap_palette = FALSE,
    reverse_colors_in_heatmap_palette = FALSE,

    show_outliers_on_plots = TRUE,

    log_scale = FALSE,
    max_zlim = 0,
    percentile_range = TRUE,
    sort_trials_by_type = 'Trial Number'
  )

  options %<>% set_font_scaling(FONT_SCALING)


  # any named arguments will override the defaults
  v = list(...)
  # options[names(v)] = v
  if(length(v))    options$mset(.list=v)

  return(options)
}

build_group_contrast_labels <- function(group_names) {
  apply(utils::combn(length(group_names),2), 2,
        function(x) paste(group_names[x],collapse='.vs.'))
}
# helper to reduce redundancies in searching then evaluating
do_on_inclusion <- function(needle, expr, haystack) {
  if(needle %in% haystack) {
    eval(expr)
  }
}

# helper that calls out to sub-decorators based on user-selected options
#
time_series_decorator <- function(plot_data, plot_options, do_not_shade = c(), ...) {

  if(missing(plot_options)) {
    plot_options <- build_plot_options()
  }

  .plot_options <- plot_options$plot_title_options
  ddl = plot_options$draw_decorator_labels

  do_tsd <- function(plot_data, label_hint=label_hint) {
    # plot title
    title_decorator(plot_data, plot_title_options = .plot_options,
                    allow_sample_size=FALSE, allow_cond = FALSE)

    # axis labels
    axis_label_decorator(plot_data)

    windows = c('Analysis')
    if(is.null(plot_data[[1]]$trial_alignment)) {
      plot_data[[1]]$trial_alignment = 'Trial Onset'
    }
    if(plot_data[[1]]$trial_alignment == 'Trial Onset') {
      windows %<>% c("Baseline")
    }

    sapply(windows, function(nm) {
      if(paste(nm, 'Window') %in% .plot_options ) {
        full_name <- tolower(nm) %&% '_window'
        if(!ddl) {
          nm <- FALSE
        }
        if(!(full_name %in% do_not_shade)) {
          window_decorator(plot_data[[1]][[full_name]],
                           type='shaded', shade.col = rave_colors[[full_name]], text = nm,
                           label_placement_offset = ifelse(
                             nm == 'Baseline', 0.9, 0.8)
          )
        }
      }
    })

    # legend options, translate the names into fields available in each element of plot_data
    legend_include = c('name', 'N')[c('Condition', 'Sample Size') %in% .plot_options]
    legend_decorator(plot_data, include = legend_include)
  }

  if(missing(plot_data)) {
    return (do_tsd)
  }

  do_tsd(plot_data)
}

# this decorator takes care of checking if has_data==TRUE and only shows labels for which there is data
# I guess there could be an option to include N==0 labels...
legend_decorator <- function(plot_data, include=c('name', 'N'), location='topleft') {

  valid.names <- c('name', 'N')

  if(length(include) < 1 || !any(include %in% valid.names)) {
    return (invisible(plot_data))
  }

  ii <- which(plot_data %>% get_list_elements('has_trials'))
  nms <- plot_data %>% get_list_elements('name', drop_nulls = FALSE) %>% extract(ii)
  ns <- plot_data %>% get_list_elements('N', drop_nulls = FALSE) %>% extract(ii)

  if('name' %in% include) {
    legend_text = nms
  } else if ('N' %in% include) {
    legend_text = paste0('n=',ns)
  }

  # handle the case where both are included. a little duplication, but clearer
  if (all(valid.names %in% include)) {
    legend_text = paste0(nms, ' (n=', ns, ')')
  }

  .cex = rave_cex.lab*get_cex_for_multifigure()
  if(plotting_to_file()) {
    .cex = 1*get_cex_for_multifigure()
  }

  legend(location, legend=legend_text, ncol=ceiling(length(ii)/3),
         inset=c(.025,.075), bty='n',
         text.col=ii, cex=.cex)

  invisible()
}

#'
#'@export
#'
get_heatmap_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
  # Some of these are from:
  # http://colorbrewer2.org

  .heatmap_palettes <- list(
    BlueWhiteRed = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff",
                     "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061") %>% rev,
    BlueGrayRed = rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#b4b4b4", "#92c5de", "#4393c3", "#2166ac", "#053061")),
    Spectral = c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf',
                 '#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2') %>% rev,
    BrownWhiteGreen = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5',
                        '#c7eae5','#80cdc1','#35978f','#01665e','#003c30'),
    PinkWhiteGreen =c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#f7f7f7',
                      '#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
    PurpleWhiteGreen = c('#40004b','#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7',
                         '#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b'),
    OrangeWhitePurple = c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7',
                          '#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'),
    BlackWhiteRed = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#ffffff',
                      '#e0e0e0','#bababa','#878787','#4d4d4d','#1a1a1a') %>% rev,
    BlueYellowRed = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf',
                      '#e0f3f8','#abd9e9','#74add1','#4575b4','#313695') %>% rev,
    GreenYellowRed = c('#a50026','#d73027','#f46d43','#fdae61','#fee08b','#ffffbf',
                       '#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837') %>% rev
  )
  if(missing(pname)) {
    if(get_palette_names)
      return (names(.heatmap_palettes))

    return (.heatmap_palettes)
  }

  pal <- .heatmap_palettes[[pname]]
  if(is.null(pal)) {
    cat2("Invalid palette requested: ", pname, ". Returning random palette",
         level="WARNING")
    pname = sample(names(.heatmap_palettes), 1)
    pal <- .heatmap_palettes[[pname]]
  }
  attr(pal, 'name') = pname

  return (pal)
}

heat_map_axes <- function(x, y, xlab, ylab, xax=TRUE, yax=TRUE, yntick=6) {
  if(xax) {
    .px <- pretty(x)
    if(missing(xlab)) {
      xlab <- .px
    } else {
      xlab <- xlab[..get_nearest(.px, x)]
    }

    rave_axis(1, at=.px, labels = xlab, tcl=0, lwd=0)
  }
  if(yax) {
    .qy <- quantile(y, 0:(yntick-1)/(yntick-1)) %>% round
    if(missing(ylab)) {
      ylab <- .qy
    } else {
      ylab <- ylab[..get_nearest(.qy, y)]
    }

    rave_axis(2, at=.qy, labels = ylab, tcl=0, lwd=0)
  }
}
#' Create a easy layout for multiple plots sharing the same x,y and legend
#' @description Provide easy ways to set plot layouts
#' @param K number of plots to be made
#' @param nrows number of rows for the plot, default 1
#' @param legend expression for generating legend, see "?legend"
#' @param legend_size legend width/height, default is lcm(3)
#' @param legend_side 1 - bottom, 2 - left, 3 - top, 4 - right. Default is 4
#' @param s_margin margins within each plots see "?par" for "mar"
#' @param b_margin margins for the whole plot see "?par" for "oma"
#' @param l_margin legend margin
easy_layout <- function(K, nrows = 1, legend,
                        legend_size = lcm(3), legend_side = 4,
                        s_margin = par('mar'), b_margin = par('oma'),
                        l_margin, parent_env = parent.frame()){
  #TODO RUTABAGA
  if(missing( l_margin )){
    l_margin = local({
      mar = s_margin;
      mar[legend_side] = 0;
      mar[(legend_side + 2) %% 4] = 0.5;
      mar
    })
  }

  # calculate nrow and ncols
  ncols = ceiling(K / nrows)
  K = nrows * ncols
  mat = matrix(seq_len(K) + 1, nrow = nrows, byrow = TRUE)


  switch (as.character(legend_side),
          '1' = {
            mat = rbind(mat, 1)
            layout(mat, heights = c(rep(1, nrows), legend_size))
          },
          '2' = {
            mat = cbind(1, mat)
            layout(mat, widths = c(legend_size, rep(1, ncols)))
          },
          '3' = {
            mat = rbind(1, mat)
            layout(mat, heights = c(legend_size, rep(1, nrows)))
          },
          {
            mat = cbind(mat, 1)
            layout(mat, widths = c(rep(1, ncols), legend_size))
          }
  )
  oma = par('oma')
  mar = par('mar')
  re = list(
    oma = oma,
    mar = mar
  )

  par(oma = b_margin)

  # draw legend first!
  expr = eval(substitute(legend), parent_env)

  par(mar = l_margin)
  eval(expr, envir = new.env(parent = parent_env))

  par(mar = s_margin)
}

#TODO RUTABAGA
# Try to guess decimal points of the number and returns string
pretty_num <- function(x, digits = 3, roundup = 5, decimal.mark = '.', ...){
  ss = base::prettyNum(x, ...)
  s = unlist(strsplit(ss, ''))
  sel = s == decimal.mark
  if(sum(sel)){
    e = which(sel) + digits
    l = length(s)
    if(l > e && s[e+1] >= roundup){
      s[e] = as.character(as.integer(s[e]) + 1L)
    }
    end = min(e, l)
    paste(s[1:end], collapse = '')
  }else{
    ss
  }
}

build_group_names <- function(groups) {
  gnames = sapply(groups, `[[`, 'group_name')
  gnames[gnames == ""] = paste0('rave_group_', LETTERS[which(gnames=='')])
  return(gnames)
}


phase_rave_color_bar <- function(zlim, actual_lim, clrs=rave_heat_map_colors, ylab='Mean % Signal Change',
                                 mar=c(5.1, 5.1, 2, 2), sym = T, ticks = NULL, digits = 1, ...) {
  max_zlim = max(abs(zlim))
  if(sym || length(zlim) == 1){
    draw_zlim = c(-1,1) * max_zlim
  }else{
    draw_zlim = zlim
  }


  cbar <- matrix(seq(-max_zlim, max_zlim, length=length(rave_heat_map_colors))) %>% t

  # this is from -1 to 1
  x = cbar / max_zlim
  sel = x %within% draw_zlim
  par(mar=mar)
  image(cbar[, sel, drop = F],
        col=clrs[sel], axes=F, ylab=ylab, main='',
        cex.main=rave_cex.main, cex.lab=rave_cex.lab, cex.axis=rave_cex.axis, ...)

  rave_title(sprintf(
    '[%s]',
    paste(sapply(actual_lim, pretty_num, digits = digits), collapse = ':')
  ))

  labels = c(draw_zlim, -max_zlim, max_zlim, 0, ticks)
  at = labels / max_zlim
  unique = !duplicated(at)

  start = min(x[sel])
  end = max(x[sel]) - start
  rave_axis(2, at= (at[unique] - start) / end, labels = sprintf(
    sprintf('%%.%df', digits), labels[unique]), tcl=0.3)
  box()

  invisible(zlim)
}


# thos function is aware of shiny status and always returns 1.0 if shiny is not running
get_cex_for_multifigure <- function() {
  cex_multiplier <- 1
  if (shiny::isRunning()) {
    if (any(par('mfrow') > 2)) {
      cex_multiplier = 1 / 0.66
    } else if (all(par('mfrow') == 2)) {
      cex_multiplier <- 1 / 0.88
    }
  }
  return (cex_multiplier)
}


# Internal use, not exported
rave_axis <-
  function(side,
           at,
           tcl = rave_axis_tcl,
           labels = at,
           las = 1,
           cex.axis = rave_cex.axis,
           cex.lab = rave_cex.lab,
           mgpy = c(3, .6, 0),
           mgpx = c(3, .75, 0),
           col,
           col.axis,
           ...) {
    # if the color isn't specified, then we are free to set the color to what we want.
    # let's set it to be black, unless the background color is black, then we'll do white
    col %?<-% get_foreground_color()
    col.axis %?<-% col

    ## overrule the font scaling if we're writing to a PDF... eventually we'll make this better
    if (shiny_is_running() && plotting_to_file()) {
      cex.axis = 1
      cex.lab = 1
      mgpy = c(3, .5, 0)
      mgpx = c(3, .4, 0)

      print("detected plotting to file")
    }

    rutabaga::ruta_axis(
      side = side,
      at = at,
      tcl = tcl,
      labels = labels,
      las = las,
      cex.axis = cex.axis * get_cex_for_multifigure(),
      cex.lab = cex.lab * get_cex_for_multifigure(),
      mgpy = mgpy,
      mgpx = mgpx,
      col = col,
      col.axis = col.axis,
      ...
    )
  }

plus_minus <- #rutabaga::plus_minus
  function (x, d) {
    if (missing(d)) {
      x <- as.matrix(x)
      d <- x[, 2]
      x <- x[, 1]
    }

    if (any(is.na(d)))
      d[is.na(d)] = 0

    c(x - d, x + d)
  }

rave_axis_labels <-
  function(xlab = NULL,
           ylab = NULL,
           col = NULL,
           cex.lab = rave_cex.lab,
           line = NA,
           ...) {
    col %?<-% get_foreground_color()

    ## overrule the font scaling if we're writing to a PDF... eventually we'll make this better
    if (plotting_to_file()) {
      if (cex.lab > 1)
        cex.lab = 1

      if (!is.null(ylab)) {
        title(
          xlab = NULL,
          ylab = ylab,
          cex.lab = cex.lab * get_cex_for_multifigure(),
          col.lab = col,
          line = 2.5,
          ...
        )
      }

      if (!is.null(xlab)) {
        title(
          xlab = xlab,
          ylab = NULL,
          cex.lab = cex.lab * get_cex_for_multifigure(),
          col.lab = col,
          line = 1.5,
          ...
        )
      }

      return()
    }

    title(
      xlab = xlab,
      ylab = ylab,
      cex.lab = cex.lab * get_cex_for_multifigure(),
      col.lab = col,
      line = line,
      ...
    )
  }

plotting_to_file <- function() {
  i = get0(
    ".Devices",
    envir = baseenv(),
    ifnotfound = list("-1"),
    inherits = FALSE
  )

  if ("-1" == i[[1]]) {
    return (FALSE)
  }

  if (length(i) < dev.cur()) {
    return (FALSE)
  }

  i = i[[dev.cur()]]

  return (isTRUE(
    'dipsaus_dev_name' %in% names(attributes(i)) ||
      i %in% c('pdf', 'png', 'jpeg')
  ))
}

rave_title <- function(main,
                       cex = rave_cex.main,
                       col,
                       font = 1, ...) {
  if (missing(col)) {
    col = if (par('bg') == 'black') {
      'white'
    } else if (par('bg') == '#1E1E1E') {
      'gray70'
    } else {
      'black'
    }
  }

  title(main = list(
    main,
    cex = cex,
    col = col,
    font = font
  ), ...)
}

# allow color cycling
get_color <- function(ii) {
  group_colors[(ii - 1) %% length(group_colors) + 1]
}

















# ----- Added by Zhengjia ------------

plot_ITPC_heatmap <- function(
    x,
    analysis_time = NULL,
    analysis_frequency = NULL,
    zmax = 0,
    mar = c(5.1, 5.1, 2, 2),
    lmar = c(5.1, 5.1, 2, 2),
    nrows = (x$ngroups >= 4) + 1,
    legend_size = ifelse(x$ngroups > 1, lcm(3.5), lcm(4.5)),
    useRaster = TRUE
) {
  # x <- itpc_plot_data
  # mar = c(5.1, 5.1, 2, 2)
  # lmar = c(5.1, 5.1, 2, 2)
  # zmax <- 0

  if( zmax <= 0 ) {
    zmax <- x$itpc_max
  }
  zmax <- max(0.001, zmax)

  easy_layout(
    x$ngroups,
    nrows = nrows,
    s_margin = mar,
    legend = {
      phase_rave_color_bar(
        zlim = c(0, zmax),
        actual_lim = x$itpc_max,
        ylab = 'Inter-Trial Coherence',
        sym = FALSE,
        ticks = median_ticks(zmax, .floor = 0),
        mar = lmar
      )
    },
    legend_size = legend_size
  )

  lapply(x$group_data, function(g){
    make_image(
      mat = g$itpc,
      x = x$time,
      y = x$frequency,
      # xlab='Time (s)', ylab='Frequency (Hz)',
      zlim = c(-zmax , zmax),
      # main = x$name,
      useRaster = useRaster,
      add = FALSE
    )

    heat_map_axes(x$time, x$frequency)
    if(length(analysis_time)) {
      abline(v = analysis_time, lwd = 3, lty = 2)

      itpc_sub <- g$itpc[..get_nearest_i(analysis_time, x$time), ]
      itpc_sub <- itpc_sub / x$itpc_max
      nfreq <- length(itpc_sub)
      xsize <- (x$time_range[[2]] - x$time_range[[1]]) / 10
      # points(
      #   x = as.vector(rbind(analysis_time - xsize * itpc_sub, analysis_time)),
      #   y = rep(x$frequency, each = 2), #c(rev(x$frequency), x$frequency),
      #   type = "S"
      # )

      poly_left0 <- analysis_time - xsize * itpc_sub
      poly_left <- c(analysis_time, poly_left0[[1]], poly_left0,
                     poly_left0[[length(poly_left0)]], analysis_time)
      polygon(
        x = poly_left,
        y = c(-100, -100, x$frequency, max(x$frequency) * 2, max(x$frequency) * 2),
        col = dipsaus::col2hexStr(get_color(g$group_index), alpha = 0.4)
      )

      idx0 <- NULL
      if(length(analysis_frequency) == 2) {
        idx0 <- which(x$frequency %within% analysis_frequency)

        if(length(idx0)) {
          abline(h = analysis_frequency, lty = 3)
        }
      } else if (length(analysis_frequency) == 1) {
        idx0 <- ..get_nearest_i(analysis_frequency, x$frequency)
      }
      if(!length(idx0)) {
        idx0 <- seq_along(itpc_sub)
      }
      idx <- idx0[which.max(itpc_sub[idx0])]
      segments(
        x0 = poly_left0[[idx]],
        x1 = analysis_time,
        y0 = x$frequency[[idx]],
        y1 = x$frequency[[idx]]
      )

    }
    if(length(analysis_frequency)) {
      analysis_frequency
    }
  })

  invisible()
}

generate_plot_data_itpc_plot_heatmap <- function(
    phase_repository, requested_electrodes, group_cleaned,
    inter_electrode_phase_coherence
) {
  itpc_max <- 0
  itpc_plot_data <- list(
    electrodes = requested_electrodes,
    time_range = range(phase_repository$phase$dimnames$Time),
    ngroups = length(group_cleaned),
    time = phase_repository$phase$dimnames$Time,
    frequency = phase_repository$phase$dimnames$Frequency,
    group_names = sapply(group_cleaned, "[[", "name"),
    group_indices = sapply(group_cleaned, "[[", "group_index"),

    group_data = lapply(group_cleaned, function(g) {

      # subset phase (freq x time x trial)
      # remember to use drop=TRUE for matrix
      trial_selection <- phase_repository$epoch$trials %in% g$trial_number
      phase <- inter_electrode_phase_coherence[,, trial_selection, drop = FALSE]

      # get ITPC (time x freq)
      itpc <- Mod(ravetools::collapse(phase, keep = c(2L, 1L), average = TRUE))

      # change `itpc_max`
      value_max <- max(itpc)
      itpc_max <<- max(value_max, itpc_max)

      c(
        g,
        list(
          itpc = itpc,
          max = value_max
        )
      )

    })
  )
  itpc_plot_data$itpc_max <- itpc_max

  return( itpc_plot_data )
}


itpc_erp_time_plot = function(
    plot_data_erp_over_time, plot_data_itpc_plot_heatmap,
    merge_plots = FALSE,
    erp_baseline = c(-1,0),
    erp_analysiswindow = c(0,1),
    erp_window_names = c("Baseline", "Analysis"),
    phase_frequency = 2,
    mar = c(4, 4, 2, 3) + 0.1,
    smooth_erp = 2,
    index_time_range = c(0, 1.25),
    ...){

  # list2env(list(merge_plots = FALSE,
  #               erp_baseline = c(-1,0),
  #               erp_analysiswindow = c(0,1),
  #               erp_window_names = c("Baseline", "Analysis"),
  #               phase_frequency = 2,
  #               mar = c(4, 4, 2, 3) + 0.1,
  #               index_time_range = c(0, 1.25),
  #               smooth_erp = 2), envir=.GlobalEnv)


  plot_data_erp_over_time$ngroups
  plot_data_itpc_plot_heatmap$itpc_max # actual_lim

  yaxis_at <- pretty(plot_data_erp_over_time$erp_mean_se_range, min.n=2, n=4)
  ngroups <- plot_data_erp_over_time$ngroups
  col <- dipsaus::col2hexStr(get_color(plot_data_erp_over_time$group_indices))
  group_names <- plot_data_erp_over_time$group_names
  frequency <- plot_data_itpc_plot_heatmap$frequency
  idx <- ..get_nearest_i(phase_frequency, frequency)
  itpc_frequency <- frequency[ idx ]


  new_plot <- function(which) {
    par(mar = mar)
    rutabaga::plot_clean(plot_data_erp_over_time$time, c(0, 1),
                         xlab = 'Time (s)', ylab = 'Inter-Trial Coherence')
    rave_title(sprintf(
      "E: %s, F: %.1fHz",
      dipsaus::deparse_svec(plot_data_erp_over_time$electrodes),
      itpc_frequency
    ), adj = 0)
    rave_axis(1, at=pretty(plot_data_erp_over_time$time))
    rave_axis(2, at=pretty(seq(0, 1, length.out = 11)))

    legend_col <- col[ which ]
    legend(x = max(plot_data_erp_over_time$time), y = 1,
           legend = group_names[ which ],
           lty = 1, col = legend_col, cex = 1, bty = 'n', ncol = 2,
           bg = dipsaus::col2hexStr(par("bg"), 0.67),
           text.col = legend_col, xjust = 1)
  }

  if(merge_plots){
    new_plot(seq_len(ngroups))
  }else{
    nrows = (ngroups >= 4) + 1
    ncols = ceiling(ngroups / nrows)
    layout( matrix(seq_len(nrows * ncols), nrow = nrows, byrow = TRUE) )
  }

  lapply(seq_len(ngroups), function(ii){
    # ii = 1
    group_erp <- plot_data_erp_over_time$group_data[[ ii ]]
    group_phase <- plot_data_itpc_plot_heatmap$group_data[[ ii ]]

    erp_mean <- group_erp$mean_se[1, ]
    erp_meanse <- group_erp$mean_se[2, ]

    if( !isFALSE(smooth_erp) && smooth_erp >= 0) {

      # smooth ERP with SD window to be smooth_erp

      weight_window <- mov_window(smooth_erp, length(group_erp$trial_number))

      # ravetools::convolve_signal has zero padding
      erp_mean <- filter(group_erp$mean_se[1, ], weight_window, circular = TRUE)
      erp_meanse <- erp_meanse * 0
    }

    # Do plot
    if(!merge_plots) {
      new_plot(ii)
    }

    # plot phase
    phase <- group_phase$itpc[, idx]

    lines(
      x = plot_data_itpc_plot_heatmap$time,
      y = phase,
      col = col[[ ii ]]
    )

    par(new=TRUE)
    plot(c(0, 1), c(0, 1), type = "n", axes = FALSE,
         ylab = "", xlab = "",
         xlim = range(plot_data_erp_over_time$time),
         ylim = rev(range(yaxis_at)), main = "")
    rutabaga::ebar_polygon(
      plot_data_erp_over_time$time,
      erp_mean,
      erp_meanse,
      add_line = TRUE, col = 'gray70')
    rave_axis(4, yaxis_at, tcl=0, lwd=1)

    # index min erp and max ITPC
    erp_selection <- which(plot_data_erp_over_time$time %within% index_time_range)
    phase_selection <- which(plot_data_itpc_plot_heatmap$time %within% index_time_range)

    erp_min_idx <- erp_selection[which.min(erp_mean[erp_selection])]
    itpc_max_idx <- phase_selection[which.max(phase[phase_selection])]

    # mark

    abline(v = plot_data_erp_over_time$time[erp_min_idx], lwd = 3, lty = 2, col = "gray70")
    abline(v = plot_data_itpc_plot_heatmap$time[itpc_max_idx], lwd = 3, lty = 2, col = col[[ ii ]])

  })

  invisible()
}

