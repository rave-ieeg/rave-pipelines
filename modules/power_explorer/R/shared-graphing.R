
get_line_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
  # from: http://colorbrewer2.org/ or in R or from unknown

  .palettes <- list(
    'Beautiful Field' = c("orange", "dodgerblue3", "darkgreen", "orangered", "brown",  "purple3"),
    'J5' = c("#407899","#deba6f", "#65743a", "#de6449", "#4B296B"),
    'Okabe-Ito' = c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                    "#D55E00", "#CC79A7", "gray60"),
    'Okabe-Ito 2' = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                      "#D55E00", "#CC79A7", "gray60"),
    'Black+tan' = c('#1A1A19', '#A25D34', '#353634', '#D0A380'),
    'Accent' = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#656565'),
    'Dark2' = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#656565'),
    'R4' = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#F5C710", "gray62"),
    'R4Permed' = c("#2297E6", "#F5C710", "#61D04F", "#DF536B", "#CD0BBC"),
    'Paired' = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00'),
    'Pastel1' = c('#fbb4ae','#b3cde3','#ccebc5','#decbe4','#fed9a6','#ffffcc','#e5d8bd','#fddaec'),
    'Pastel2' = c('#b3e2cd','#fdcdac','#cbd5e8','#f4cae4','#e6f5c9','#fff2ae','#f1e2cc','#cccccc'),
    'Set1' = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf'),
    'Set2' = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3'),
    'Set3' = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')
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

apply_current_theme <- function(...) {
  theme <- ravedash::current_shiny_theme()

  par('bg'=theme$background, 'fg'=theme$foreground, 'col'=theme$foreground,
      'col.axis' = theme$foreground)

  grDevices::palette(get_line_palette(
    pe_graphics_settings_cache$get('line_color_palette'))
  )
}


build_palettes_and_ranges_for_omnibus_data <- function(omnidata) {

  nms <- names(omnidata)[grepl("^[m\\ |t\\ |p\\ |F\\ |p_fdr]", names(omnidata))]

  # this happens for the movie player
  if(length(nms) == 0) {
    nms = names(omnidata)[!names(omnidata) %in% c('Electrode', 'Time')]
  }

  val_ranges = sapply(nms, function(d) {
    if (startsWith(d, 'p ') | startsWith(d, 'p_fdr'))
      return(c(-.2, .2))
    c(-1, 1) * ceiling(max(abs(omnidata[[d]])))
  }, simplify = FALSE, USE.NAMES = TRUE)

  .colors = pe_graphics_settings_cache$get('heatmap_color_palette')
  if(is.list(.colors)) .colors = .colors[[1]]

  pal = ravebuiltins::expand_heatmap(.colors, ncolors=128)
  pval_pal = ravebuiltins::expand_heatmap(
    rev(tail(.colors, ceiling(length(.colors)/2))),
    ncolors=128, bias=10)
  pals = list(pal)

  pals[seq_along(nms)] = pals
  # names(pals) = fix_name_for_js(names(by_electrode_results))
  names(pals) = nms

  pals[startsWith(names(pals), 'p(')] = list(pval_pal)
  pals[startsWith(names(pals), 'p_fdr')] = list(pval_pal)
  pals[names(pals) %in% c('p')] = list(pval_pal)

  return(
    list(
      palettes = pals,
      val_ranges = val_ranges
    )
  )
}

draw_many_heat_maps <- function (hmaps,
                                 max_zlim = 0, percentile_range = FALSE, log_scale = FALSE,
                                 show_color_bar = TRUE, useRaster = TRUE, PANEL.FIRST = NULL,
                                 PANEL.LAST = NULL, PANEL.COLOR_BAR = NULL,
                                 axes = c(TRUE, TRUE), plot_time_range = NULL, special_case_first_plot = FALSE,
                                 max_columns = 3, decorate_all_plots = FALSE, center_multipanel_title = FALSE,
                                 ignore_time_range = NULL,
                                 marginal_text_fields = c("Subject", "Electrodes"), extra_plot_parameters = NULL,
                                 do_layout = TRUE, byrow=TRUE, ...)
{

  #pe_graphics_settings_cache is defined above
  rave_cex.axis <- pe_graphics_settings_cache$get('rave_cex.axis', missing_default = 1.3)
  rave_cex.main <- pe_graphics_settings_cache$get('rave_cex.main', missing_default = 1.5)


  # how many of the maps actually have usable data (use the range argument)
  has_data <- which(!sapply(hmaps, function(x) {
    if(!is.null(x$range)) {
      return(any(is.nan(x$range)))
    }
    return(range(x$data))
  }))

  # ravedash::logger('has_data:', has_data, level='warning')

  if (do_layout) {
    orig.pars <- layout_heat_maps(length(has_data), max_col = max_columns,
                                  layout_color_bar = show_color_bar, byrow = byrow)

    ## if you want us to do layout, then I assume you want us to setup colors too
    apply_current_theme()
  }
  #some people were passing in NULL for max_zlim
  max_zlim %?<-% 0

  # if (any(par("mfrow") > 1)) {
  #   default_mar <- c(5.1, 4.1, 4.1, 2.1)
  #   if (all(par("mar") == default_mar)) {
  #     mar2 %?<-% 4.5
  #     par(mar = 0.1 + c(4, mar2, 5, 2))
  #   }
  #   if (!decorate_all_plots && do_layout) {
  #     par(oma = pmax(c(0,0,2,0), par('oma')))
  #   }
  # } else {
  #   if (all(par("mar") == default_mar)) {
  #     par(mar = 0.1 + c(par("mar")[1], mar2, 5, 2))
  #   }
  # }

  actual_lim = rutabaga::get_data_range(hmaps)
  if (max_zlim <= 0) {
    max_zlim <- max(abs(actual_lim), na.rm = TRUE)
  } else if (percentile_range) {
    if (max_zlim >= 100) {
      max_zlim = (max_zlim/100) * max(abs(actual_lim),
                                      na.rm = TRUE)
    } else {
      if (!is.numeric(ignore_time_range)) {
        max_zlim <- quantile(unlist(lapply(hmaps, getElement,
                                           "data")), probs = max_zlim/100, na.rm = TRUE)
      }
      else {
        ind <- !(hmaps[[1]]$x %within% ignore_time_range)
        max_zlim <- quantile(unlist(lapply(hmaps, function(h) h$data[ind,
        ])), probs = max_zlim/100, na.rm = TRUE)
      }
    }
    if (max_zlim > 100) {
      max_zlim %<>% round(-1)
    }
    else if (max_zlim > 10) {
      max_zlim %<>% round_to_nearest(5)
    }
  }
  log_scale <- if (isTRUE(log_scale)) {
    "y"
  }
  else {
    ""
  }
  for (mi in seq_along(has_data)) {
    # mi=1
    map = hmaps[[has_data[mi]]]
    plot_time_range %?<-% range(map$x)
    if (!all(map$x %within% plot_time_range)) {
      .attr = attributes(map$data)
      ind <- map$x %within% plot_time_range
      map$x <- map$x[ind]
      map$data <- map$data[ind, , drop = FALSE]
      .attr$dim = attr(map$data, "dim")
      attributes(map$data) <- .attr
    }
    x <- seq_along(map$x)
    y <- seq_along(map$y)
    mto = NULL
    if (!decorate_all_plots && length(has_data) > 1) {
      mto = list(allow_sid = FALSE, allow_enum = FALSE, allow_freq = FALSE, allow_sample_size = FALSE)
      my_r = floor((mi - 1)/par("mfrow")[2]) + 1
      my_c = 1 + (mi - 1) %% par("mfrow")[2]
      ncol_wo_cbar <- par("mfrow")[2]
      if (show_color_bar) {
        if (my_c == par("mfrow")[2]) {
          my_c = 1
          my_r = my_r + 1
        }
        ncol_wo_cbar = ncol_wo_cbar - 1
      }
      if (mi + ncol_wo_cbar <= length(has_data)) {
        map$xlab <- " "
      }
      if (my_c != 1) {
        map$ylab <- " "
      }
    }
    dy <- 0
    pc_args = list()
    if (log_scale == "y") {
      dy <- (y[2] - y[1])/2 + min(y)
      pc_args[c("xlim", "ylim", "cex.lab", "log")] = list(x,
                                                          y + dy, rave_cex.axis * get_cex_for_multifigure(),
                                                          "y")
    } else {
      pad = c(-0.5, 0.5)
      pc_args[c("xlim", "ylim")] = list(range(x) +
                                          pad, range(y) + pad)
    }
    if (!is.null(extra_plot_parameters)) {
      pc_args[names(extra_plot_parameters)] = extra_plot_parameters
    }

    do.call(rutabaga::plot_clean, pc_args)
    if (is.function(PANEL.FIRST)) {
      PANEL.FIRST(map)
    }
    make_image(map$data, x = x, y = y, log = ifelse(log_scale,
                                                    "y", ""), zlim = c(-1, 1) * max_zlim)


    ### draw the axis labels (no drawn axes/ticks on the spectrogram: lwd=0, tcl=0)
    axes %<>% rep_len(2)

    ### how many x and y ticks do we need?
    if (axes[1]) {
      xticks <- ..get_nearest_i(pretty(map$x), map$x)
      rave_axis(1, at = xticks, labels = map$x[xticks],
                tcl = 0, lwd = 0, mgpx = c(0, 0.75, 0))
    }

    if(axes[2]) {
      if (length(map$y) <= 5) {
        yticks <- seq_along(map$y)
      }
      else if (diff(range(diff(sort(map$y)))) > 2) {
        tck = map$y[c(1, round(length(map$y) * (1/3)),
                      round((2/3) * length(map$y)), 1 * length(map$y))]
        yticks <- ..get_nearest_i(tck, map$y)
      }
      else {
        yticks <- ..get_nearest_i(pretty(map$y), map$y)
      }

      rave_axis(2, at = yticks, labels = map$y[yticks],
                tcl = 0, lwd = 0, mgpy = c(0, -1/4, 0))
    }


    # call the last decorator
    if (is.function(PANEL.LAST)) {
      PANEL.LAST(
        data=map,
        Xmap = function(x) ..get_nearest_i(x, map$x),
        Ymap = function(y) ..get_nearest_i(y, map$y),
        more_title_options = mto
      )
    }
  }

  if (show_color_bar) {
    .mar <- c(par("mar")[1], 5.1, 5, 1)
    if (is.function(PANEL.COLOR_BAR)) {
      .mar[3] = 5
    }
    .ylab = ""
    .ylab <- hmaps[[has_data[1]]]$zlab
    rave_color_bar(max_zlim, actual_lim, ylab = .ylab, mar = .mar)
    if (is.function(PANEL.COLOR_BAR)) {
      PANEL.COLOR_BAR(hmaps)
    }
  }

  # if (!decorate_all_plots &&
  #     any(par("mfrow")[1] > 1, par("mfrow")[2] > 2)
  # ) {
  #   if (center_multipanel_title) {
  #     xpos <- ifelse(show_color_bar, 0.475, 0.5)
  #     adj <- 0.5
  #   }
  #   else {
  #     xpos = 0
  #     adj <- 0
  #   }

  # tokens = c('Subject' = hmaps[[1]]$subject_code,
  #            'Electrodes' = "E" %&%
  #              dipsaus::deparse_svec(
  #                hmaps[[1]]$electrodes)
  #              ,
  #            'Frequency' = "F" %&% str_collapse(unique(
  #              sapply(meta_data$analysis, function(a) str_collapse(a$frequency, ':'))
  #            )) %&% "Hz"
  # )
  #
  # tokens = tokens[(names(tokens) %in% marginal_text_fields)]
  # mtext(text = paste(tokens, collapse = " "), line = 0,
  #       at = xpos, adj = adj, outer = TRUE, font = 1, cex = rave_cex.main *
  #         0.8)
  # }

  invisible(hmaps)
}

layout_heat_maps <- function(k, max_col, ratio=4, byrow=TRUE,
                             layout_color_bar=TRUE, colorbar_cm=3.5) {
  opars <- par(no.readonly = TRUE)

  # colorbar_cm <- 3.5
  if(plotting_to_file()) {
    colorbar_cm <- 3
  }

  nr <- ceiling(k / max_col)
  max_col = min(k, max_col)
  m <- 1:k
  mat <- matrix(c(m, rep.int(0, nr*max_col - k)), byrow = byrow,
                nrow=nr, ncol = max_col)
  widths <- rep(ratio, max_col)
  if(layout_color_bar) {
    mat <- cbind(mat, k+1)
    widths <- c(widths, lcm(colorbar_cm))
  }
  layout(mat, widths=widths)
}

plotting_to_file <- function() {
  i = get0(".Devices", envir = baseenv(),
           ifnotfound = list("-1"), inherits = FALSE)

  if("-1" == i[[1]]) {
    return (FALSE)
  }

  if(length(i) < dev.cur()){
    return (FALSE)
  }

  i = i[[dev.cur()]]

  return (
    isTRUE('dipsaus_dev_name' %in% names(attributes(i)) || i %in% c('pdf', 'png', 'jpeg'))
  )
}

#' @title RAVE custom image plotter
#' @param mat z-matrix
#' @param x,y z and y axis
#' @param col vector of colors, color palette
#' @param zlim value to trim zmat
#' @param log which axis will be in log scale
#' @param useRaster passed to image()
#' @param clip_to_zlim whether to clip mat
#' @param add logical, whether to overlay current plot to an existing image
#' @description The idea here is to to separate the plotting of the heatmap from all the accoutrements that are done in the decorators.
#' We are just plotting image(mat) Rather Than t(mat) as you might expect. The built-in calculators know this so we can save a few transposes along the way.
make_image <- function(mat, x, y, zlim, col=NULL, log='', useRaster=TRUE, clip_to_zlim=TRUE, add=TRUE) {

  if(missing(zlim)) {
    zlim <- c(-1,1)*max(abs(mat))
  } else {
    # if zlim is missing, then the zlim will be set symmetrically based on the range
    # of the data (in the 'if' block above), so we only have to worry about clipping if the range is passed in
    if(clip_to_zlim) {
      # mat %<>% clip_x(zlim)
      mat <- rutabaga::clip_x(mat, zlim)
    }
  }

  col %?<-% get_currently_active_heatmap()

  if(!('matrix' %in% class(mat))) {
    stop('mat is not a matrix... check it out: make_image_mat')
    # assign('make_image_mat', mat, globalenv())
  }

  if(is.na(log)) {
    log = ''
  }

  # with(make_image_mat, {

  image(x=x, y=y, z=mat, zlim=zlim, col=col, useRaster=useRaster, log=log,
        add=add, axes=F, xlab='', ylab='', main='')
  # })

  # return the clipped zmat
  invisible(mat)
}

get_heatmap_palette <- function(pname, get_palettes=FALSE, get_palette_names=FALSE) {
  # Some of these are from:
  # http://colorbrewer2.org

  .heatmap_palettes <- list(
    BlueWhiteRed = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#ffffff",
                     "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061") %>% rev,
    BlueGrayRed = rev(c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#b4b4b4",
                        "#92c5de", "#4393c3", "#2166ac", "#053061")),
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

get_currently_active_heatmap <- function() {
  if(!('current_heatmap_palette' %in% pe_graphics_settings_cache$keys())) {
    pe_graphics_settings_cache$set(
      key = 'current_heatmap_palette',
      value = ravebuiltins::expand_heatmap(ravebuiltins::get_heatmap_palette('BlueWhiteRed'), ncolors = 101),
      signature = paste0('BlueWhiteRed', 101)
    )
  }

  pe_graphics_settings_cache$get('current_heatmap_palette')
}

..get_nearest_i <- function(from,to, lower_only=FALSE) {

  if(lower_only) {
    res <- sapply(from, function(.x) {
      to2 = to[to<= .x]
      which.min(abs(.x-to2))
    })
  } else {
    res <- sapply(from, function(.x) {
      which.min(abs(.x-to))
    })
  }

  return(res)
}

..get_nearest <- function(from, to) {
  approxfun(to, seq_along(to))(from)
}

..get_nearest_val <- function(from,to) {
  to[..get_nearest_i(from,to)]
}

`%near%` <- function(x, y, eps=1e-4) {
  abs(x-y) < eps
}

rave_axis <- function(side, at, tcl, labels=at, las=1, cex.axis,
                      cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), col, col.ticks, col.axis, ...) {

  # if the color isn't specified, then we are free to set the color to what we want.
  # set it to the fg color, same for lines and for ticks
  col %?<-% par('fg') # axis line color
  col.ticks %?<-% col # axis ticks color
  col.axis %?<-% col  # axis labels color

  tcl %?<-% pe_graphics_settings_cache$get('rave_axis_tcl')
  cex.axis %?<-% pe_graphics_settings_cache$get('rave_cex.axis')
  cex.lab %?<-% pe_graphics_settings_cache$get('rave_cex.lab')


  ## overrule the font scaling if we're writing to a PDF... eventually we'll make this better
  if(shiny::isRunning() && plotting_to_file()) {
    cex.axis = 1
    cex.lab = 1
    mgpy = c(3,.5,0)
    mgpx = c(3,.4,0)

    dipsaus::cat2("detected plotting to file")
  }

  rutabaga::ruta_axis(
    side = side,
    at = at,
    tcl = tcl,
    labels = labels,
    las = las,
    cex.axis = cex.axis*get_cex_for_multifigure(),
    cex.lab = cex.lab*get_cex_for_multifigure(),
    mgpy = mgpy,
    mgpx = mgpx,
    col=col, col.ticks=col.ticks, col.axis=col.axis,
    ...
  )
}

#' Function make a title for a plot, checks par('bg') to handle dark mode
#' @seealso title
#' @param cex the character expansion for the title (default is rave_cex.main)
#' @param font the font type (default = 1, plain)
#' @export
rave_title <- function(main, cex.main, col, font=1, adj=0.5, ...) {
  col %?<-% par('fg')

  cex.main %?<-% pe_graphics_settings_cache$get('rave_cex.main')

  if(plotting_to_file()) {
    cex.main = 1
  }

  title(main=list(main, cex=cex.main*get_cex_for_multifigure(), col=col, font=font), adj=adj)
}

rave_axis_labels <- function(xlab=NULL, ylab=NULL, col=NULL, cex.lab,
                             xline=1.5, yline=2.75, push_X=0, push_Y=0, ...) {
  col %?<-% par('col')

  cex.lab %?<-% pe_graphics_settings_cache$get('rave_cex.lab')

  ## overrule the font scaling if we're writing to a PDF
  # ... eventually we'll make this better and read from pe_graphics_settings_cache
  if(plotting_to_file()) {
    if (cex.lab > 1) cex.lab = 1
  }

  if(!is.null(ylab)) {
    title(xlab=NULL, ylab=ylab,
          cex.lab=cex.lab*get_cex_for_multifigure(),
          col.lab=col, line=yline+push_Y, ...)
  }

  if(!is.null(xlab)) {
    title(xlab=xlab, ylab=NULL,
          cex.lab=cex.lab*get_cex_for_multifigure(),
          col.lab=col, line=xline+push_X, ...)
  }

}

get_cex_for_multifigure <- function() {
  cex_multiplier <- 1
  if(shiny::isRunning()) {
    if(any(par('mfrow') > 2)) {
      cex_multiplier = 1/0.66
    } else if (all(par('mfrow') == 2)) {
      cex_multiplier <- 1/0.88
    }
  }
  return (cex_multiplier)
}

get_foreground_color <- function() {
  stop('do not use get foreground color')
  # switch(par('bg'),
  #        'white' = 'black',
  #        'black' = 'white',
  #        '#1E1E1E' = 'gray70',
  #        'gray' = '#A5A5A5',
  #        par('col')
  # )
}

setup_palette <- function() {
  pe_graphics_settings_cache$get('color_palette')
}

rave_color_bar <- function(zlim, actual_lim, clrs, ylab, ylab.line=2,
                           mar=c(5.1, 5.1, 2, 2), adjust_for_nrow=TRUE, horizontal=FALSE, ...) {
  # print('drawing color bar')
  clrs %?<-% get_currently_active_heatmap()
  cbar <- matrix(seq(-zlim, zlim, length=length(clrs))) %>% t

  if(horizontal) cbar %<>% t

  ylim = 0:1
  if(adjust_for_nrow && (par('mfrow')[1] > 1)) {
    nr = par('mfrow')[1]
    ylim = 1/nr * c(-1,1) + ylim
  }

  orig.pty <- par('pty')
  on.exit(par(pty=orig.pty))

  par(mar=mar, pty='m')
  image(cbar, useRaster = FALSE, ylim=ylim,
        col=clrs, axes=F, ylab='', main='',
        col.lab = par('fg'))

  # check if any other graphics params were requested, direct them to the proper function
  more = list(...)

  ral.args = list(ylab=ylab, yline=ylab.line)
  if('cex.lab' %in% names(more)) ral.args$cex.lab = more$cex.lab

  if(horizontal) {
    rave_title(ral.args$ylab)
  } else {
    do.call(rave_axis_labels, ral.args)
  }

  ra.args = list(side=2, at=0:1, labels=pretty_round(c(-zlim,zlim), allow_negative_round = TRUE), tcl=0)
  if('cex.axis' %in% more) ra.args$cex.axis = more$cex.axis

  if(horizontal) ra.args$side = 1
  do.call(rave_axis, ra.args)

  ra.args[c('at', 'labels', 'tcl')] = list(0.5, 0, 0.3)
  do.call(rave_axis, ra.args)

  # this won't work if we're tweaking ylim based on nrow
  # the source for box is compiled, but this seems to work. The key is the xpd=T to make sure
  # we don't get clipping
  if(!horizontal) {
    rect(par('usr')[1], 0, par('usr')[2], 1,
         lwd=1, xpd=TRUE)
  } else {
    box()
  }

  invisible(zlim)
}

### decorators ---

draw_box <- function(x,y, ...)  {
  segments(x0=x[1], x1=x[2], y0=y[1], ...)
  segments(x0=x[2], y0=y[1], y1=y[2], ...)
  segments(x0=x[1], x1=x[2], y0=y[2], ...)
  segments(x0=x[1], y0=y[1], y1=y[2], ...)
}


time_frequency_decorator <- function(data, condition_data, baseline_settings, Xmap=force, Ymap=force,
                                     analysis_window_type = c('box', 'line', 'shade'), ...) {

  analysis_window_type = match.arg(analysis_window_type)

  shd2 <- function(data, condition_data, baseline_settings, Xmap, Ymap, ...) {
    # put the name of the trial grouping at the top of the spectrogram
    rave_title(data$name)

    # label the x and y axes
    rave_axis_labels(data$xlab, data$ylab)

    render_vertical <- function(wi, txt, col=adjustcolor(par('fg'), offset=rep(.3, 4)),
                                lty=2, adjust_text=c(0,0)) {
      abline(v=Xmap(wi), lty=lty, col=col)

      # cat(paste("RV: wi=", str_collapse(wi), " =>: ", str_collapse(Xmap(wi)), '\npar usr: ',
      #             str_collapse(par('usr')), '\n'))

      mtext(side=3, at=Xmap(wi[1])+adjust_text[1], adj=0,
            # 1.0*par('usr')[4]+adjust_text[1],
            text = txt, col=col,
            cex = pe_graphics_settings_cache$get('rave_cex.lab')
      )
    }

    # label the baseline windows
    sapply(baseline_settings$window, render_vertical, 'Baseline')

    # label the analysis window(s)
    render_window <- function(as, adjust_text=c(0,0), ...) {

      if(analysis_window_type == 'line') {
        render_vertical(wi = as$time, txt = as$label, col=par('fg'), adjust_text = adjust_text, ...)
      } else {
        x <- Xmap(as$time); y <- Ymap(as$frequency)

        draw_box(x,y,lty=2,lwd=2, col=par('fg'), ...)
        mtext(side=3, at = x[1]+adjust_text[1], adj=0,
              # y[1] + (diff(y)*1) + adjust_text[2],
              text = as$label,
              col=par('fg'), #xpd=T, pos=4,
              cex = pe_graphics_settings_cache$get('rave_cex.lab')
        )
      }
    }

    if('time' %in% names(data$settings)) {
      # this means there is only one set of settings
      render_window(data$settings)
    } else {
      sapply(data$settings, render_window)
    }
  }

  if(missing(data) || is.null(data)) {
    return (shd2)
  }

  shd2(data, condition_data, baseline_settings, Xmap, Ymap)

}

spectrogram_heatmap_decorator <- function(plot_data, plot_options, Xmap=force, Ymap=force, btype='line', atype='box',
                                          title_options=list(allow_freq=FALSE), ...) {
  to <- force(title_options)
  plot_options = list(
    plot_title_options = pe_graphics_settings_cache$get('plot_title_options'),
    draw_decorator_labels = pe_graphics_settings_cache$get('draw_decorator_labels')
  )

  # the idea of more title options is that the caller might "know better" than the creator about
  # 'which' plot is being decorated and can selective (dis/en)able certain text
  shd <- function(plot_data, Xmap=Xmap, Ymap=Ymap, more_title_options, ...) {
    .args = list('plot_data' = plot_data, 'plot_title_options' = plot_options$plot_title_options)

    if(length(title_options) > 0) {
      .args[names(title_options)] = title_options
    }

    if(!missing(more_title_options) && is.list(more_title_options)) {
      .args[names(more_title_options)] <- more_title_options
    }

    if(!is.null(title_options)) {
      do.call(title_decorator, args=.args)
    }

    rave_axis_labels(xlab=plot_data$xlab, ylab=plot_data$ylab)

    # check if the analysis and baseline windows are fully contained within the plotting window. If not, switch baseline type
    # to just be a label. This can, of course, be turned off using the regular options.
    if(!all(plot_data$baseline_window %within% plot_data$x)) {
      btype = 'label'
    }

    if(!all(plot_data$analysis_window %within% plot_data$x)) {
      atype = 'label'
    }

    # would be nice to have a TRIAL_ONSET or something here, rather than a string...
    if(!is.null(plot_data$trial_alignment) && plot_data$trial_alignment != 'Trial Onset') {
      btype = 'n'
    }

    windows <- list(
      'Baseline'=list(
        window = if(btype == 'label') {
          plot_data$baseline_window
        }else {
          Xmap(plot_data$baseline_window)
        },
        type=btype
      ),
      'Analysis'=list(
        window = if(atype=='box') {
          list(x=Xmap(plot_data$analysis_window),
               y=Ymap(plot_data$frequency_window))
        } else if(atype == 'label') {
          plot_data$analysis_window
        }else {
          Xmap(plot_data$analysis_window)
        },
        type=atype
      )
    )

    if(isTRUE(plot_data$enable_frequency2)) {

      windows[['F2 Analysis']] = list(
        window = if(atype=='box') {
          list(x=Xmap(plot_data$analysis_window2),
               y=Ymap(plot_data$frequency_window2))
        } else if(atype == 'label') {
          plot_data$analysis_window2
        }else {
          Xmap(plot_data$analysis_window2)
        },
        type=atype
      )

      if('Analysis Window' %in% plot_options$plot_title_options) {
        plot_options$plot_title_options %<>% c('F2 Analysis Window')
        # print('adding F2 AW')
      }
    }
    # print(str_collapse(plot_options$plot_title_options))

    lapply(names(windows), function(nm) {
      if(paste(nm, 'Window') %in% plot_options$plot_title_options & windows[[nm]]$type != 'n') {
        # cat('trying:\t', str_collapse(c(atype, btype, nm)), '\n')

        lpo <- 0.9
        if(all('label' == c(btype,atype), nm %in% c('F2 Analysis', 'Analysis'))){
          lpo <- 0.8
        } else if(nm == 'F2 Analysis') {
          lpo = 0.75
        }

        with(windows[[nm]],
             window_decorator(
               window=window, type=type,
               text=ifelse(plot_options$draw_decorator_labels, nm, ''))
        )
      }
    })
    # if(results$get_value('draw_decorator_labels')) {
    #     rave_axis(1, at=Xmap(0), labels = plot_data$trial_alignment, mgpx=c(1,2,1), lwd=0, tcl=0)
    # }

    invisible(plot_data)
  }

  if(missing(plot_data)) {
    # dipsaus::cat2('returning decorator')
    return (shd)
  }

  shd(plot_data, Xmap, Ymap)
}

axis_label_decorator <- function(plot_data, col, Xmap=force, Ymap=force, label_alignment=TRUE, label_alignment.line = 2, ...) {
  stop('please do not use axis_label_decorator anymore')
}

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
    # dipsaus::cat2('dput on title:\t', dput(title_string), '\nlen: ', length(title_string), ', nchar=', nchar(title_string), pal = list("Blue" = "dodgerblue3"), level = "Blue")

    rave_title(title_string)
  }

  invisible(title_string)
}

# helper to reduce redundancies in searching then evaluating
do_on_inclusion <- function(needle, expr, haystack) {
  if(needle %in% haystack) {
    eval(expr)
  }
}
###

round_to_nearest <- function(x, val=10) {
  val*round(x/val)
}


fill_poly <- function(x, y, fill.color=1, fill.alpha=0.6, y0=0, do_stroke=TRUE, stroke.lwd=2, stroke.color=fill.color) {
  polygon( c(x, rev(x)), c(rep(y0, length(y)), rev(y)), border=NA, col=adjustcolor(fill.color, fill.alpha))
  #overdraw the data line (this may do weird things if col already has transparency in it)
  if(isTRUE(do_stroke)) lines(x, y, col=stroke.color, lwd=stroke.lwd)
}

is.color <- function(x) {
  if(is.null(x) || is.function(x) || is.nan(x)) return (FALSE)

  res <- tryCatch({
    col2rgb(x)
  }, error = function(...) {
    FALSE
  })
  if(isFALSE(res)) return(FALSE)


  return(TRUE)
}

render_analysis_window <- function(settings, lty=2, do_label=TRUE, text.color=par('fg'),
                                   x, y, line.color=par('fg'), shade.color, stroke.color, shade.alpha=0.6) {
  # settings <- dd$settings
  wi = settings$time

  # handle possibly-overlapping multiple time window labels
  if(do_label) {
    if(is.list(wi)) {

    } else {
      mtext(side=3, at=wi[1], adj=0,
            text = settings$label, col=text.color,
            cex = pe_graphics_settings_cache$get('rave_cex.lab')
      )
    }
  }

  if(is.numeric(lty)) abline(v=wi, lty=2, col=line.color)

  if(!missing(y) && !missing(x)) {
    ind <- x %within% settings$time
    fill_poly(x=x[ind], y=y[ind], fill.color = shade.color, fill.alpha=shade.alpha, stroke.color=stroke.color)
  }
}

plot_over_time_by_condition <- function(over_time_by_condition_data,
                                        combine_conditions=FALSE,
                                        combine_events=FALSE,
                                        condition_switch=NULL) {

  if(is.character(condition_switch)){
    if (condition_switch=='Separate all')  {
      combine_events=FALSE
      combine_conditions=FALSE
    } else if (condition_switch == 'Combine conditions') {
      combine_events = FALSE
      combine_conditions=TRUE
    } else if (condition_switch == 'Combine events')  {
      combine_events = TRUE
      combine_conditions=FALSE
    } else if (condition_switch == 'Combine all') {
      combine_events = TRUE
      combine_conditions=TRUE
    }
  }

  apply_current_theme()

  n.groups <- length(over_time_by_condition_data)
  n.events <- length(over_time_by_condition_data[[1]])

  xlim <- sapply(over_time_by_condition_data, function(otd) {
    range(sapply(otd, function(dd) {
      dd$x
    }))
  }) %>% range
  ylim <- sapply(over_time_by_condition_data, function(otd) {
    range(sapply(otd, function(dd) {
      rutabaga::plus_minus(dd$data)
    }))
  }) %>% pretty %>% range

  draw_axis_labels <- function() {
    nr = par('mfrow')[1]
    nc = par('mfrow')[2]

    x.dividers = seq_len(nc) * 1/nc
    x.midpoints = x.dividers - 0.5*(1/nc)

    y.dividers = seq_len(nr) * 1/nr
    y.midpoints = y.dividers - 0.5*(1/nr)

    rave_cex.axis <- pe_graphics_settings_cache$get('rave_cex.axis')
    mtext(outer = TRUE, side=1, 'Time (s)', cex=rave_cex.axis, at=x.midpoints)
    mtext(outer = TRUE, side=2, over_time_by_condition_data[[1]][[1]]$ylab, at=y.midpoints,
          cex=rave_cex.axis, line=1)
  }

  decorate_plot <- function(graph_data, graph_num,
                            window_label= FALSE, window_type = c('lines', 'fill'), axes=TRUE) {
    # draw the analysis window behind the data
    sh = pe_graphics_settings_cache$get('analysis_window.shade.color')
    if (sh == 'match') sh = graph_num

    st = pe_graphics_settings_cache$get('analysis_window.stroke.color')
    if (st == 'match') st = graph_num

    if('fill' %in% window_type) {
      render_analysis_window(graph_data$settings, do_label=window_label,
                             x=graph_data$x, y=graph_data$data[,1],
                             shade.color = sh,
                             stroke.color = st)
    } else {
      render_analysis_window(graph_data$settings, do_label=window_label,
                             shade.color = sh,
                             stroke.color = st)
    }

    axes = rep_len(axes, 2)
    if(axes[1]) rave_axis(1, at=axTicks(1))
    if(axes[2]) rave_axis(2, at=axTicks(2))

    if(par('usr')[3] < 0) abline(h=0, col='lightgray')
  }

  # many situations merge into a 1,1 plot type, so have an function for that
  plot_all_in_one <- function() {
    par(mfrow=c(1,1), oma=c(2, 2.25, 0, 0), mar=c(2,2,2,1))
    rutabaga::plot_clean(xlim, ylim)
    draw_axis_labels()
    dy = .075*diff(par('usr')[3:4])

    for(ii in seq_along(over_time_by_condition_data)) {
      for (jj in seq_along(over_time_by_condition_data[[ii]])) {
        dd <- over_time_by_condition_data[[ii]][[jj]]
        graph_num = jj + (n.events) * (ii-1)

        # put the decorations behind the data
        decorate_plot(graph_data = dd, graph_num = graph_num,
                      window_type = 'lines', axes=(graph_num==1),
                      # label windows every time there is a new event
                      window_label = (ii==1)
        )

        rutabaga::ebar_polygon(dd$x, dd$data[,1], dd$data[,2], col=graph_num)

        yy = max(axTicks(2)) - dy*graph_num

        text(x=axTicks(1)[1], y=yy,
             labels =paste(dd$time_window_label, dd$data_label, sep=', '), adj=c(0,0),
             cex = pe_graphics_settings_cache$get('rave_cex.lab', 1),
             col=graph_num
        )
      }
    }
  }

  plot_combined_over_conditions <- function(nr,nc) {
    # we have at least 2 events
    par(mfrow=c(nr,nc), oma=c(2, 2.25, 0, 0), mar=c(2,2,2,1))

    #loop over events
    for(ei in seq_along(over_time_by_condition_data[[1]])) {
      rutabaga::plot_clean(xlim, ylim)
      draw_axis_labels()
      with(over_time_by_condition_data[[1]][[ei]],
           rave_title(time_window_label)
      )

      if(ei==1) {
        nms <- names(over_time_by_condition_data)
        dy = .075*diff(par('usr')[3:4])
        yy = max(axTicks(2)) - dy*seq_along(nms)

        text(x=axTicks(1)[1], y=yy,
             labels =nms, adj=c(0,0),
             cex = pe_graphics_settings_cache$get('rave_cex.lab', 1),
             col=seq_along(nms)
        )
      }

      for(ii in seq_along(over_time_by_condition_data)) {
        dd <- over_time_by_condition_data[[ii]][[ei]]
        graph_num = ii

        # put the decorations behind the data
        decorate_plot(graph_data = dd, graph_num = graph_num,
                      window_type = 'lines', axes=(graph_num==1))

        rutabaga::ebar_polygon(dd$x, dd$data[,1], dd$data[,2], col=graph_num)
      }

    }
  }

  plot_combined_over_events <- function(nr,nc) {
    par(mfrow=c(nr,nc), oma=c(2, 2.25, 0, 0), mar=c(2,2,2,1))
    for(ii in seq_along(over_time_by_condition_data)) {
      rutabaga::plot_clean(xlim, ylim)
      draw_axis_labels()
      rave_title(over_time_by_condition_data[[ii]][[1]]$data_label)

      if(ii==1) {
        nms <- names(over_time_by_condition_data[[1]])
        dy = .075*diff(par('usr')[3:4])
        yy = max(axTicks(2)) - dy*seq_along(nms)

        text(x=axTicks(1)[1], y=yy,
             labels =nms, adj=c(0,0),
             cex = pe_graphics_settings_cache$get('rave_cex.lab', 1),
             col=seq_along(nms)
        )
      }

      for(jj in seq_along(over_time_by_condition_data[[ii]])) {
        dd <- over_time_by_condition_data[[ii]][[jj]]
        graph_num = jj

        # put the decorations behind the data
        decorate_plot(graph_data = dd, graph_num = graph_num,
                      window_type = 'lines', axes=(graph_num==1))

        rutabaga::ebar_polygon(dd$x, dd$data[,1], dd$data[,2], col=graph_num)
      }
    }
    # show a warning if the events have a different origin
  }

  MAX_COL = pe_graphics_settings_cache$get('max_columns_in_figure', 3)


  determine_layout <- function(n.panels) {
    nr = ceiling(n.panels / MAX_COL)

    nc = min(n.panels, MAX_COL)

    c(nr,nc)
  }

  if(isTRUE(combine_conditions) && isTRUE(combine_events)){
    # put everybody in a single plot
    plot_all_in_one()
  } else if (isTRUE(combine_conditions)){
    # we're combining conditions (but not events), so the number of
    # unique graphs is the number of unique events, second-level dimension
    layout = determine_layout(length(over_time_by_condition_data[[1]]))

    if(prod(layout)==1) {
      plot_all_in_one()
    } else {
      plot_combined_over_conditions(nr=layout[1], nc=layout[2])
    }

  } else if (isTRUE(combine_events)){
    # here we are combining events but not combining conditions
    layout <- determine_layout(length(over_time_by_condition_data))

    if(prod(layout) == 1) {
      plot_all_in_one()
    } else {
      plot_combined_over_events(nr=layout[1], nc=layout[2])
    }
  } else {
    # separate everything
    k = n.groups * n.events

    # determining our own layout to make sure it looks nice
    nc = k %% n.groups
    if(nc ==0) nc = n.groups
    nr = ceiling(k / n.groups)

    # prefer (1,X) to (X,1)
    if(nr > 1 && nc ==1) {
      nc = nr
      nr = 1
    }

    par(mfcol=c(nr,nc), oma=c(2, 2.25, 0, 0), mar=c(2,2,2,1))
    for(ii in seq_along(over_time_by_condition_data)) {
      for(jj in seq_along(over_time_by_condition_data[[ii]])) {
        rutabaga::plot_clean(xlim, ylim)
        draw_axis_labels()
        dd <- over_time_by_condition_data[[ii]][[jj]]
        rave_title(paste(sep=' | ', dd$data_label, dd$time_window_label))

        graph_num = jj + (ii-1)*n.events

        # put the decorations behind the data
        decorate_plot(graph_data = dd, graph_num = graph_num)

        rutabaga::ebar_polygon(dd$x, dd$data[,1], dd$data[,2], col=graph_num)
      }
    }
  }
}

plot_per_electrode_statistics <- function(stats, requested_stat, show0=c('smart', 'auto', 'always'),
                                          which_plots=c('all', 'm', 't', 'p'), draw_threshold = NULL,
                                          label_electrodes = NULL, label_type=c('number', 'name', 'color', 'showcase')) {

  if(missing(requested_stat) || is.null(requested_stat) || nchar(requested_stat) == 0) {
    requested_stat = 'overall'
  }
  # make sure this matches which_plots above
  plot.types = c('m', 't', 'p')

  theme <- ravedash::current_shiny_theme()
  par('bg'=theme$background, 'fg'=theme$foreground, 'col'=theme$foreground)

  electrode_numbers <- as.integer(colnames(stats))

  ### the first thing to check is if we are in 'showcase' mode
  # if so, then chop off electrodes from the stats block
  which_to_label = integer(0)
  if(!is.null(label_electrodes)) {
    which_to_label = which(electrode_numbers %in% label_electrodes)
  }

  label_type = match.arg(label_type)

  if(length(which_to_label) > 0 && label_type == 'showcase') {
    stats = stats[,which_to_label,drop=FALSE]
    electrode_numbers <- as.integer(colnames(stats))
  }

  show0 = match.arg(show0)

  which_plots = match.arg(which_plots)

  tmp_ind = which(
    stringr::str_detect(tolower(rownames(stats)),
                        tolower(requested_stat))
  )

  row_ind <- 1:3
  if(length(tmp_ind)>1) {
    row_ind = seq(from=tmp_ind[1], to=2+tmp_ind[1])
  }

  get_non_consec <- function(ee) {
    res <- which(diff(ee)>1)
    sort(c(res, res+1))
  }

  el_axis <- function() {
    xat <- if(length(electrode_numbers) <= 5) {
      seq_along(electrode_numbers)
    } else {
      unique(c(1, get_non_consec(electrode_numbers), length(electrode_numbers)))
    }
    rave_axis(1, at=xat, labels=electrode_numbers[xat], tcl=0)
  }

  do_draw_threshold <- function(th) {
    abline(h=th, lty=2, col=pe_graphics_settings_cache$get('champions_tunic'))
  }

  ylabels <- list('p(' = 'p-value', 't(' = 't-score',
                  'm(' = 'mean', 'p_fdr(' = 'fdr p-value')

  if('all' == which_plots) {
    par(mfrow=c(1,3),mar=c(4.1,5.1,4.1,2.1))
  } else {
    row_ind = row_ind[which(which_plots == plot.types)]
  }

  for(ii in row_ind) {
    cn <- rownames(stats)[ii]
    ylab <- ylabels[[substr(cn, 1,2)]]
    if(is.null(ylab)) {
      ## check if we have a speciality p-value
      if(startsWith(cn, 'p_fdr(')) {
        ylab = ylabels[["p_fdr("]]
      }
    }

    yy <- if(ylab %in% c('p-value', 'fdr p-value')) {
      -log10(stats[ii, ])
    } else {
      stats[ii, ]
    }

    ylim = yy
    if(show0 == 'always') {
      ylim = c(0, yy)
    } else if(show0 == 'smart') {
      if(any(stringr::str_detect(cn, ' - '), stringr::str_detect(ylab, '(p-value|t-score)'))) {
        ylim = c(0,yy)
      }
    }
    rutabaga::plot_clean(seq_along(electrode_numbers), pretty(ylim))
    el_axis()

    if(stringr::str_detect(ylab, 'p-value')) {
      for(ax in axTicks(2)) {
        rave_axis(2, at=ax, labels=bquote(10**.(-ax)), tcl=0, cex.axis = 1)
      }

      rave_axis(2, at=axTicks(2), labels = FALSE)

      ## draw threshold if needed
      if(!is.null(draw_threshold)) {
        do_draw_threshold(-log10(draw_threshold))
      }

    } else {
      # because we're not drawing a box, it's awkward to have points below (above)
      # the extreme ticks
      planned_ticks = axTicks(2)
      k = length(planned_ticks)
      if(k %% 2 == 1) {
        planned_ticks = planned_ticks[c(1, (k+1)/2, k)]
      }
      rave_axis(2, at=planned_ticks)

      ## draw threshold if needed
      if(!is.null(draw_threshold)) {
        do_draw_threshold(draw_threshold)
      }

    }
    rave_axis_labels(xlab='Electrode #', ylab=ylab, push_X = 0.75)
    rave_title(cn)

    # if(0 %within% range(ylim)) {
    .x <- seq_along(electrode_numbers)
    points(.x, yy, pch=19,
           col=adjustcolor(par('col'), offset=rep(.35, 4)),
           type='h', lwd=0.5)
    # }
    points(.x, yy, pch=19,
           col= adjustcolor(par('col'), offset=rep(.25, 4))
    )

    if(length(which_to_label) > 0) {
      if(label_type == 'number') {
        text(.x[which_to_label], yy[which_to_label], labels=electrode_numbers[which_to_label],
             pos=ifelse(yy[which_to_label]<0, 1, 3), font=2, xpd=TRUE,
             cex = pe_graphics_settings_cache$get('rave_cex.lab'),
             xpd=TRUE)

      } else if (label_type == 'color') {
        # plotting at 1.01 to make sure we cover existing circle
        points(.x[which_to_label], yy[which_to_label], col=pe_graphics_settings_cache$get('champions_tunic'), pch=19, cex=1.01)
      } else if(label_type == 'name') {
        electrode_names = electrode_numbers

        if('electrode_labels' %in% names(attributes(stats))) {
          electrode_names = attr(stats, 'electrode_labels')
        }

        text(.x[which_to_label], yy[which_to_label], labels=electrode_names[which_to_label],
             pos=ifelse(yy[which_to_label]<0, 1, 3), font=2, xpd=TRUE,
             cex = pe_graphics_settings_cache$get('rave_cex.lab'),
             xpd=TRUE)
      }
    }
  }
}



plot_grouped_data <- function(mat, xvar, yvar='y', gvar=NULL, ...,
                              types = c('jitter points', 'means', 'ebar polygons'),
                              layout=c('grouped', 'overlay'), draw0=TRUE, draw0.col=NULL,
                              ylim=NULL, col=NULL, do_axes=TRUE,
                              names.pos = c('none', 'bottom', 'top'),
                              plot_options = NULL, jitter_seed=NULL, cex_multifigure_scale=TRUE) {

  apply_current_theme()

  # here we need to know about grouping var, x-axis
  if(is.null(plot_options)) {
    plot_options <- list()
  }

  plot_options$pch %?<-% 16

  if(!is.numeric(plot_options$pt.cex)) plot_options$pt.cex <- 1

  if(cex_multifigure_scale) plot_options$pt.cex = plot_options$pt.cex*get_cex_for_multifigure()

  plot_options$outlier_pch %?<-% 1
  plot_options$pt.alpha %?<-% 100

  draw0.col %?<-% par('fg')

  # treat like percent for UI, but alpha.f wants proportion
  plot_options$pt.alpha = plot_options$pt.alpha / 100

  types %?<-% c('jitter points', 'means', 'ebar polygons')

  names.pos = match.arg(names.pos)

  # removed (layout := 'stacked') because I don't know if iEEG data
  # should ever be stacked ?
  if(missing(xvar) && !is.null(gvar)) {
    xvar <- gvar
    gvar <- 'none'
  }

  if(is.null(gvar) || gvar == xvar) {
    gvar <- 'none'
  }

  # layout = 'grouped'
  layout = match.arg(layout)

  # first step is to aggregate the data to get the means and potentially SE
  keys <- xvar
  if(gvar=='none') gvar=NULL

  if(!is.null(gvar)) {
    keys <- c(gvar, keys)
  }
  raw <- data.table::data.table(mat)

  if(!is.null(raw$is_clean) && any(!raw$is_clean)) {
    # get the aggregate data for plotting means/se
    clean <- raw[raw$is_clean, ]
    agg <- clean[ , list(y=mean(get(yvar)), se=rutabaga:::se(get(yvar)), n=.N), keyby=keys]

  } else {
    # if is_clean is null, we need to create it and set them to TRUE.
    raw$is_clean = TRUE

    # get the aggregate data for plotting means/se
    agg <- raw[ , list(y=mean(get(yvar)), se=rutabaga:::se(get(yvar)), n=.N), keyby=keys]
  }

  # convert y into a matrix for plotting
  if(is.null(gvar)) {
    means <- matrix(agg$y, ncol=1)
    names.arg = levels(as.factor(agg[[xvar]]))

    # assign('cc', list('a'=agg, 'x'=xvar), envir = globalenv())

    if('overlay' == layout) {
      names.arg=NA
    }
  } else {
    means <- matrix(agg$y, ncol=nlevels(as.factor(agg[[gvar]])))
    names.arg = levels(as.factor(agg[[gvar]]))
  }

  # color is based on rows of means
  # col <-  seq_len(nrow(means))
  col %?<-% seq_len(nrow(means))

  # in case we have a caller-supplied col but it's the wrong length
  if(length(col) < nrow(means)) {
    col %<>% rep(length.out=nrow(means))
  }

  bp_clean <- function(...) {
    my_args <- list(axes=F, ylab='', xlab='', border=NA,
                    beside = layout=='grouped',
                    # cex.axis=1*get_cex_for_multifigure(),
                    cex.names = pe_graphics_settings_cache$get('rave_cex.lab')*get_cex_for_multifigure()
    )
    your_args <- list(...)
    if(length(your_args) > 0) {
      my_args[names(your_args)] <- your_args
    }

    do.call(barplot, my_args)
  }

  # we want to use barplot to create the window because it has nice xlim and ylim padding
  # but if we're adding points / ebars we need to consider that at creation time
  # ylim=NULL
  tmp_y <- ylim
  if(is.null(tmp_y)) {
    tmp_y <- if(any(c('ebar polygons', 'ebars') %in% types)) {
      range(rutabaga::plus_minus(agg$y, agg$se))
    } else {
      range(agg$y)
    }

    if (any(c('points', 'jitter points', 'connect points',
              'densities', 'density polygons', 'rugs') %in% types)) {
      tmp_y %<>% range(raw[[yvar]]*1.1)
    }

    # ensure start at 0 if doing true barplot
    if(any(c('bars', 'borders') %in% types)) {
      tmp_y %<>% range(0)
    }
    tmp_y <- range(pretty(tmp_y), tmp_y)
  }

  # first create the empty plot. the trick here is that we're
  # giving barplot data with the same dimension of means but with the appropriate
  # range
  ylim %?<-% range(tmp_y, pretty(tmp_y))

  # NB: heights could end up a 1x1 matrix w/o the appropriate range
  bars.x <- bp_clean(array(tmp_y, dim=dim(means)), col=par('bg'), names.arg=names.arg, ylim=ylim, ...)

  if(!is.matrix(bars.x)) {
    if(layout == 'grouped') {
      warning("layout is grouped but I don't have grouped coordinates... bail!")
      return(FALSE)
    }
    bars.x %<>% matrix(nrow = nrow(means), ncol=ncol(means), byrow=T)
  }

  if(draw0) abline(h=0, col=draw0.col)

  # in case you're looping over agg, you'll want this
  long_col <- rep(col, times=ncol(bars.x))
  # for jittering and other l/r movement
  r <- if(length(bars.x) == 1) {
    1/3
  } else if(nrow(bars.x) == 1) {
    (1/3) * min(diff(c(bars.x)))
  } else {
    if ('overlay' == layout && ncol(bars.x) > 1) {
      (1 / 3) * min(diff(t(bars.x)))
    } else {
      (1/3) * min(diff(bars.x))
    }
  }
  if(any(r==0, is.infinite(r))) {r <- 1/3}

  #keep the data together so we can check outlier status later
  points_list <- split(raw, by=keys)# %>% lapply(`[[`, yvar)

  # we need to make sure the points_list respects the factor ordering
  if(is.null(gvar)) {
    ord <- levels(as.factor(mat[[xvar]]))
  } else {
    # sapply(keys, function(k) is.factor(mat[k]))
    # class(mat$Factor1)

    sapply(keys, function(k) is.factor(mat[[k]]))
    mat[keys] <- lapply(mat[keys], as.factor)

    ord <- stringr::str_replace_all(levels(do.call(`:`, mat[,keys])), ':', '.')
  }
  points_list <- points_list[ord]


  # helper function to determine location of x-position for points
  .get_x <- if('jitter points' %in% types) {
    # here we shrink r so that the points stay w/n r after plotting (non-zero point size)
    function(pl, bx) density_jitter(pl$y, around = bx, max.r = 0.7*r, seed = jitter_seed)
  } else {
    function(pl, bx) rep(bx, length(pl$y))
  }

  points_list.x <- mapply(.get_x, points_list, bars.x, SIMPLIFY = FALSE)

  ### BARS
  if (any(c('bars', 'borders') %in% types)) {
    # I think we should be using long_col here
    bar.col = adjustcolor(long_col, .7)
    bar.border = long_col
    if(!('bars' %in% types)) {
      bar.col = rep(NA, length(bar.col))
    }
    if(!('borders' %in% types)) {
      bar.border = rep(NA, length(bar.border))
    }

    # if type is overlay, then we need to draw these on top of each other
    if('overlay' == layout) {
      mapply(function(row, col, border) {
        bp_clean(height=means[row, ], col=col, border=border, add=TRUE
                 # ,space=0.5
        )
      }, seq_len(nrow(means)), bar.col, bar.border)
    } else {
      # stacked and grouped are handled cleanly by bp_clean
      xp <- bp_clean(height = means, col=bar.col, border=bar.border, add=TRUE,
                     plot=FALSE)

      if(length(xp)==1) {
        half_window <- r[1]*diff(par('usr')[1:2])
        polygon(c(xp[1] - half_window, xp[1] + half_window,
                  xp[1] + half_window, xp[1] - half_window),
                y = c(0,0,means[1], means[1]),
                border=bar.border, col=bar.col)
      } else { #if(ncol(xp)==1) {
        half_window <- r[1]*unique(diff(xp[,1]))[1]
        sapply(seq_along(xp), function(xi) {

          polygon(c(xp[xi] - half_window, xp[xi] + half_window,
                    xp[xi] + half_window, xp[xi] - half_window),
                  y = c(0,0,means[xi], means[xi]),
                  border=bar.border[xi], col=bar.col[xi])
        })

      }
    }
  }

  ## DENSITIES

  if('density polygons' %in% types) {
    for(ii in seq_along(points_list)) {
      dx <- density(points_list[[ii]]$y, n=64)
      dx$y <- r*(dx$y/max(dx$y))
      polygon(x = c(bars.x[ii] + dx$y, rev(bars.x[ii] -dx$y), bars.x[ii]+ dx$y[1]),
              c(dx$x, rev(dx$x), dx$x[1]),
              border = NA, col=adjustcolor(long_col[ii], .3), lwd=1.5)
    }
  }

  if('densities' %in% types) {
    for(ii in seq_along(points_list)) {
      dx <- density(points_list[[ii]]$y, n=64)
      dx$y <- r*(dx$y/max(dx$y))
      for(k in c(-1,1)) lines(bars.x[ii] + k*dx$y, dx$x,
                              col=adjustcolor(long_col[ii], .8), lwd=1.5)
    }
  }

  if('connect points' %in% types) {
    # need to determine which points to connect.

    # connect points based on how many
    if(layout=='grouped') {

      ymat <- do.call(cbind, points_list)
      xmat <- do.call(cbind, points_list.x)

      col_ind <- matrix(seq_along(points_list), nrow=nrow(means))

      apply(col_ind, 2, function(ci) {
        for(ii in seq_len(nrow(col_ind)-1)) {
          segments(
            xmat[,ci[ii]], ymat[,ci[ii]],
            xmat[,ci[ii+1]], ymat[,ci[ii+1]],
            lwd=0.75, col=adjustcolor('lightgray', .7)
          )
        }
      })
    } else {
      # when we're overlaying, we connect by gvar (assuming there is one?)
      ymat <- do.call(cbind, points_list)
      xmat <- do.call(cbind, points_list.x)

      ind <- matrix(seq_along(points_list), nrow=nrow(means))

      # as with connecting means, if there is not a grouping variable, just connect what you can
      # transpose the indices so it's like we're going over groups
      if(ncol(ind) == 1) {
        ind %<>% t
      }

      apply(ind, 1, function(ri) {
        for(ii in 1:nrow(xmat)) {
          lines(xmat[ii,ri], ymat[ii,ri], col='lightgray', lwd=0.75)
        }
      })
    }
  }

  if('rugs' %in% types) {
    warning('rugs not implemented')
    # mapply(function(x, pts, clr) {
    #     d <- diff(grconvertY(1:2, from='lines', to='user'))
    #
    #     if(any(duplicated(y))) {
    #         y %<>% jitter
    #     }
    #
    #     lapply(pts$y, function(y) segments(x0=x-d, x+d, y0=y, col=clr))
    #
    # }, bars.x, points_list, col)
  }

  if(any(c('points', 'jitter points') %in% types)) {
    # go through each row in agg and select out the appropriate points from raw
    # we need to make sure mat is sorted before we do this

    if(length(points_list) == length(bars.x)){
      points_list.x %?<-% mapply(.get_x, points_list, bars.x, SIMPLIFY = FALSE)

      mapply(function(x, pl, clr) {
        points(x, pl$y, col=adjustcolor(clr, alpha.f = plot_options$pt.alpha),
               pch=ifelse(pl$is_clean, plot_options$pch, plot_options$outlier_pch),
               cex = plot_options$pt.cex)
      }, points_list.x, points_list, long_col)

    } else {
      warning("couldn't line up points with bars... sorry!")
    }
  }

  # now do error bars if possible
  if('ebars' %in% types) {
    # check if we're horizontal or not
    # if(horizontal) {
    #   ebars.x(...)
    #} else {

    rutabaga::ebars(bars.x, agg$y, sem = agg$se, code=0, col=col, lwd=2)
    #}
  }

  if('ebar polygons' %in% types) {
    for(ii in 1:nrow(agg)) {
      rutabaga::do_poly(
        rutabaga::plus_minus(x = bars.x[ii], d = r),
        # bars.x[ii] %>% plus_minus(r),
        rutabaga::plus_minus(x = agg$y[ii], d = agg$se[ii]),
        # y = agg$y[ii] %>% plus_minus(agg$se[ii]),
        col = long_col[ii], alpha = .3*255)
    }
  }

  if ('connect means' %in% types) {
    # I think if we are grouped, then we don't allow the lines to go across the groups
    if(layout == 'grouped') {
      for(ii in seq_len(ncol(means))) {
        lines(x=bars.x[,ii], means[,ii], col='black', type='l', lwd=2)
      }
    } else {
      # for overlay, we go across the groups
      for(ii in seq_len(nrow(means))) {
        lines(x=bars.x[ii,], means[ii,], col=col[ii], type='l', lwd=2)
      }

      # but if there are no groups.... then go over whatever we can?
      # I think this will make people happy; it's confusing to add an option
      # and have it do nothing. If it does the wrong thing we can "change" it. If
      # it does nothing, it looks broken and we have to "fix"
      if(ncol(means)==1) {
        lines(x=bars.x[,1], means[,1], col='black', type='l', lwd=2)
      }

    }
  }

  if('means' %in% types) {
    segments(bars.x - r, x1=bars.x + r, y0 = means, col=long_col, lwd=2, lend=1)
  }

  if(do_axes) {
    rave_axis(2, at=axTicks(2))
  }

  xlevels = length(unique(raw[[xvar]]))
  if(names.pos != 'none') {
    y = if(names.pos == 'top') {
      par('usr')[4] + 0.05 * diff(par('usr')[3:4])
    } else {
      par('usr')[3] - 0.025 * diff(par('usr')[3:4])
    }

    lbls = levels(as.factor(raw[[xvar]]))
    if(xvar == 'Factor1Factor2') {
      lbls = stringr::str_replace_all(lbls, '\\.', '\n')
    }
    text(c(bars.x)[seq_len(xlevels)], y=rep(y, xlevels),
         labels=lbls,
         col = col, xpd=TRUE,
         cex = pe_graphics_settings_cache$get('rave_cex.lab')*get_cex_for_multifigure())
  }


  return(invisible(list(x=points_list.x, y=points_list, bars.x=bars.x)))
}

density_jitter <- function(x, around=0, max.r=.2, n=length(x), seed=NULL) {
  dx <- density(x, from=min(x), to=max(x), n=n)

  # careful here, switching from x to y (i.e., y = f(x))
  y <- approxfun(dx)(x)

  R.utils::withSeed({
    runif(length(x),
          min = -max.r * (y/max(y)),
          max = max.r * (y/max(y))
    ) + around

  }, seed = seed)
}

build_heatmap_analysis_window_decorator <- function(...,  type=c('line', 'box'),
                                                    lwd=2, lty=2,
                                                    show_top_label=FALSE) {
  force(lwd); force(lty); force(show_top_label)
  type = match.arg(type)

  # make sure we have enough space to write into
  par('oma' = pmax(c(1,0,0,0), par('oma')))

  hawd <- function(data, Xmap, Ymap, ...) {
    # label analysis event
    mtext(data$analysis_event, side = 1, at=Xmap(0),
          line=2.5, cex=(7/8)*get_cex_for_multifigure(), col = par('fg'))

    # label analysis window
    xx <- Xmap(data$analysis_window)
    if(type == 'box') {
      yy <- Ymap(data$analysis_frequency)

      polygon(c(xx,rev(xx)), rep(yy, each=2),
              lty=lty, lwd=lwd,border='black')

    } else {
      abline(v=xx, lty=2, col=par('fg'), xpd=FALSE, lwd=2)
      if(show_top_label)
        mtext(data$analysis_group, side=3, at=xx[1], adj = -.25, line=0,
              cex=get_cex_for_multifigure()*9/8)
    }
  }

  return (hawd)
}

build_axis_label_decorator <- function(..., doX=TRUE, doY=TRUE, push_X=0, push_Y=0) {
  doX = force(doX)
  doY = force(doY)

  push_X %<>% force
  push_Y %<>% force

  o.mar <- par('mar')
  if(doX) {
    o.mar[1] <- max(o.mar[1], 5.1+push_X)
  }
  if(doY) {
    o.mar[2] <- max(o.mar[2], 5.1+push_Y/2)
  }
  par('mar' = o.mar)

  ald <- function(data, ...) {
    print(push_X)
    print(push_Y)
    if(doX)
      rave_axis_labels(xlab=data$xlab, push_X = push_X)

    if(doY)
      rave_axis_labels(ylab=data$ylab, push_Y=push_Y)
  }

  return(ald)
}


stack_decorators <- function(...) {
  decorators <- lapply(list(...), match.fun)

  return(function(...) {
    lapply(decorators, function(DD) {
      DD(...)
    })
  })
}


build_heatmap_condition_label_decorator <- function(all_maps, ...) {
  o.mar <- par('mar')

  ## make sure we have sufficient left-margin
  max_char_count <- sapply(all_maps, function(m) {
    nchar(unique(as.character(m$y)))
  }) %>% unlist %>% max

  mar2 <- 5.1 + max(0, (max_char_count - 5) * 0.75)
  new_mar <- o.mar
  new_mar[2] = max(mar2, o.mar[2])

  par('mar' = new_mar)

  # function that does the decorating
  hcld <- function(data, Xmap, Ymap, ...) {

    diff_cond = rle(data$y)
    len <- diff_cond$lengths
    cs_len <- cumsum(len)

    # drop the last one because we don't need a line at the top
    cs_len = cs_len[-length(cs_len)]

    if(length(cs_len) > 0) {
      abline(h=cs_len+0.5)
    }
    midpoints <- len/2 + c(0, cs_len)

    mtext(diff_cond$values, side = 2,
          at = midpoints,
          cex = 0.8*get_cex_for_multifigure(), las=1, line = 0.5)

  }

  return (hcld)
}

plot_over_time_by_electrode <- function(by_electrode_tf_data) {
  decorators <- stack_decorators(
    build_heatmap_analysis_window_decorator(),
    build_axis_label_decorator(push_X=3),
    build_title_decorator()
  )

  draw_many_heat_maps(
    hmaps = by_electrode_tf_data,
    PANEL.LAST = decorators
  )
}

build_title_decorator <- function(to_include=c('analysis_group',
                                               'condition_group'), sep = ' | ') {
  force(to_include)
  force(sep)

  btd <- function(data, ...) {
    # grab the items from what's available in data
    ind <- which(to_include %in% names(data))

    if(length(ind) < 1) {
      # found nothing, do nothing
      return()
    }

    if(length(ind) == 1) {
      # one item, no separators needed
      str = data[[to_include[[ind]]]]

    } else {
      sep = rep_len(sep, length.out = length(to_include)-1)

      # grab the first item
      str = data[[to_include[[ind[1]]]]]

      # loop from 2:N
      for(ii in seq_along(ind)[-1]) {

        #grab the next item
        nm <- to_include[[ind[ii]]]
        val <- data[[nm]]

        # just in case we have vector values, collapse them here
        val %<>% paste0(collapse=' ')

        # finally combine using requested separator
        str %<>% paste(val, sep=sep[ind[ii-1]])
      }
    }

    # title!
    rave_title(str)
  }

  return (btd)
}

plot_by_frequency_correlation <- function(by_frequency_correlation_data) {

  decorators <- stack_decorators(
    build_axis_label_decorator(push_X = 1.5),
    build_title_decorator()
  )

  par(pty='s')

  draw_many_heat_maps(
    by_frequency_correlation_data,
    PANEL.LAST = decorators
  )
}

plot_by_frequency_over_time <- function(by_frequency_over_time_data) {
  decorators <- stack_decorators(
    build_heatmap_analysis_window_decorator(type = 'box'),
    build_axis_label_decorator(push_X = 3),
    build_title_decorator()
  )

draw_many_heat_maps(by_frequency_over_time_data, show_color_bar = FALSE)
  draw_many_heat_maps(
    by_frequency_over_time_data,
    PANEL.LAST = decorators
  )
}

plot_over_time_by_trial <- function(over_time_by_trial_data) {
  apply_current_theme()

  decorators <- stack_decorators(
    build_title_decorator(),
    build_heatmap_analysis_window_decorator(),
    build_heatmap_condition_label_decorator(over_time_by_trial_data)
  )

  draw_many_heat_maps(hmaps = over_time_by_trial_data,
                      axes = c(T,F), max_zlim = 95, percentile_range = TRUE,
                      PANEL.LAST = decorators
  )

}


# chdir = TRUE, current working directory is this folder
pipeline <- raveio::pipeline(pipeline_name = "power_explorer", paths = "../..")
pe_graphics_settings_cache <- dipsaus::rds_map(file.path(pipeline$preference_path, "graphics"))

default_pegs <- {list(
  rave_cex.main = 1.5,
  rave_cex.axis = 1.3,
  # putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
  # the (outer) left margin to compensate
  rave_cex.lab = 1.4,
  rave_axis_tcl = -0.3,
  plot_time_range = c(-Inf,Inf),
  draw_decorator_labels = FALSE,
  plot_title_options = c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range',
                         'Sample Size', 'Baseline Window', 'Analysis Window'),


  ## this is now managed through ravedash theme
  background_plot_color_hint = 'white',
  champions_tunic = '#009edd',

  line_color_palette = 'Beautiful Field',
  invert_colors_in_palette = FALSE,
  reverse_colors_in_palette = FALSE,

  analysis_window.shade.color = 'gray70',
  analysis_window.stroke.color = 'match',

  heatmap_color_palette = get_heatmap_palette(get_palette_names = TRUE)[[1]],
  heatmap_number_color_values = 101,
  invert_colors_in_heatmap_palette = FALSE,
  reverse_colors_in_heatmap_palette = FALSE,

  show_outliers_on_plots = TRUE,

  log_scale = FALSE,
  max_zlim = 0,
  percentile_range = TRUE,
  sort_trials_by_type = 'Trial Number',

  max_columns_in_figure = 3

)}

nm <- names(default_pegs) [!pe_graphics_settings_cache$has(names(default_pegs))]
if(length(nm)) {
  pe_graphics_settings_cache$mset(.list = default_pegs[nm])
}


