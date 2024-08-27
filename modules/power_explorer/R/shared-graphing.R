

draw_many_heat_maps <- function (hmaps, meta_data,
                                 max_zlim = 0, percentile_range = FALSE, log_scale = FALSE,
                                 show_color_bar = TRUE, useRaster = TRUE, wide = FALSE, PANEL.FIRST = NULL,
                                 PANEL.LAST = NULL, PANEL.COLOR_BAR = NULL, axes = c(TRUE,
                                                                                     TRUE), plot_time_range = NULL, special_case_first_plot = FALSE,
                                 max_columns = 2, decorate_all_plots = FALSE, center_multipanel_title = FALSE,
                                 ignore_time_range = NULL, marginal_text_fields = c("Subject",
                                                                                    "Electrodes"), extra_plot_parameters = NULL,
                                 do_layout = TRUE, ...)
{

    #pe_graphics_settings_cache is defined in aa.R
    rave_cex.axis <- pe_graphics_settings_cache$get('rave_cex.axis', missing_default = 1.3)
    rave_cex.main <- pe_graphics_settings_cache$get('rave_cex.main', missing_default = 1.5)


    # how many of the maps actually have usable data (use the range argument)
    has_data <- which(!sapply(hmaps, function(x) {
        if(!is.null(x$range)) {
            return(any(is.nan(x$range)))
        }
        return(range(x$data))
    }))

    ravedash::logger('has_data:', has_data, level='warning')

    if (do_layout) {
        orig.pars <- layout_heat_maps(length(has_data), max_col = max_columns,
                                      layout_color_bar = show_color_bar)
    }
    #some people were passing in NULL for max_zlim
    max_zlim %?<-% 0

    if (wide) {
        max_char_count = max(sapply(hmaps, function(h) ifelse(any(is.nan(h$range)),
                                                              max(nchar(h$conditions)), 1)))
        par(mar = c(par("mar")[1], 5.1 + max(0, (max_char_count -
                                                     5) * 0.95), 2, 2))
    }
    else {
        if (any(par("mfrow") > 1)) {
            default_mar <- c(5.1, 4.1, 4.1, 2.1)
            if (all(par("mar") == default_mar)) {
                par(mar = 0.1 + c(4, 4.5, 2, 0))
            }
            if (!decorate_all_plots && do_layout) {
                par(oma = c(0, 0, 2, 0))
            }
        }
        else {
            if (all(par("mar") == default_mar)) {
                par(mar = c(par("mar")[1], par("mar")[2], 2,
                            2))
            }
        }
    }
    actual_lim = get_data_range(hmaps)
    if (max_zlim <= 0) {
        max_zlim <- max(abs(actual_lim), na.rm = TRUE)
    }
    else if (percentile_range) {
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

        ### how many x and y ticks do we need?
        xticks <- ..get_nearest_i(pretty(map$x), map$x)
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

        ### draw the axis labels (no drawn axes/ticks on the spectrogram: lwd=0, tcl=0)
        axes %<>% rep_len(2)
        if (axes[1])
            rave_axis(1, at = xticks, labels = map$x[xticks],
                      tcl = 0, lwd = 0, mgpx = c(0, 0.5, 0))
        if (axes[2])
            rave_axis(2, at = yticks, labels = map$y[yticks],
                      tcl = 0, lwd = 0, mgpy = c(0, 0.5, 0))


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
        .mar <- c(par("mar")[1], 3.5, 2, 1)
        if (is.function(PANEL.COLOR_BAR)) {
            .mar[3] = 4
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

layout_heat_maps <- function(k, max_col, ratio=4, layout_color_bar=TRUE, colorbar_cm=3.5) {
    opars <- par(no.readonly = TRUE)

  # colorbar_cm <- 3.5
  if(plotting_to_file()) {
    colorbar_cm <- 3
  }

    nr <- ceiling(k / max_col)
    max_col = min(k, max_col)
    m <- 1:k
    mat <- matrix(c(m, rep.int(0, nr*max_col - k)), byrow = TRUE,
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

set_currently_active_heatmap <- function( pal_name, n_colors = 101 ) {
  pal_name <- pal_name %OF% get_heatmap_palette(get_palette_names = TRUE)

  pal <- ravebuiltins::expand_heatmap(
    ravebuiltins::get_heatmap_palette(pal_name),
    ncolors = 101
  )

  pe_graphics_settings_cache$set(key='heatmap_color_palette',
                                 signature = pal_name,
                                 value = pal_name)
  pe_graphics_settings_cache$set(
    key = 'current_heatmap_palette',
    value = pal,
    signature = paste0(pal_name, n_colors)
  )
}

set_currently_active_heatmap <- function( pal_name, n_colors = 101 ) {
  pal_name <- pal_name %OF% get_heatmap_palette(get_palette_names = TRUE)

  pal <- ravebuiltins::expand_heatmap(
    ravebuiltins::get_heatmap_palette(pal_name),
    ncolors = 101
  )

  pe_graphics_settings_cache$set(key='heatmap_color_palette',
    signature = pal_name,
    value = pal_name)
  pe_graphics_settings_cache$set(
    key = 'current_heatmap_palette',
    value = pal,
    signature = paste0(pal_name, n_colors)
  )
}

..get_nearest_i <- function(from,to, condition=c('equal', 'less', 'greater')) {

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


find_index_of_nearest <- ..get_nearest_i


map_indices_within <- function(from, to) {
  mapply(find_index_of_nearest,
    from, condition=list('greater', 'less'),
    MoreArgs = list(to=to)
  )
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
                      cex.lab, mgpy=c(3, .6, 0), mgpx=c(3, .75, 0), col, col.axis, ...) {

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

        print("detected plotting to file")
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

rave_axis_labels <- function(xlab=NULL, ylab=NULL, col=NULL, cex.lab, line=NA, ...) {
    col %?<-% get_foreground_color()

  cex.lab %?<-% pe_graphics_settings_cache$get('rave_cex.lab')

  ## overrule the font scaling if we're writing to a PDF
  # ... eventually we'll make this better and read from pe_graphics_settings_cache
  if(plotting_to_file()) {
    if (cex.lab > 1) cex.lab = 1
  }

        if(!is.null(ylab)) {
            title(xlab=NULL, ylab=ylab,
                  cex.lab=cex.lab*get_cex_for_multifigure(),
                  col.lab=col, line=2.75, ...)
        }

        if(!is.null(xlab)) {
            title(xlab=xlab, ylab=NULL,
                  cex.lab=cex.lab*get_cex_for_multifigure(),
                  col.lab=col, line=1.5, ...)
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

rave_color_bar <- function(zlim, actual_lim, clrs, ylab, ylab.line=1.5,
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
          col.lab = get_foreground_color())

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


time_frequency_decorator <- function(data, condition_data, analysis_settings, baseline_settings, Xmap=force, Ymap=force,
                                     analysis_window_type = c('box', 'line', 'shade'), ...) {

  analysis_window_type = match.arg(analysis_window_type)

  shd2 <- function(data, condition_data, baseline_settings, Xmap, Ymap, ...) {
    # put the name of the trial grouping at the top of the spectrogram
    rave_title(data$name)

    # label the x and y axes
    rave_axis_labels(data$xlab, data$ylab)


        render_vertical <- function(wi, txt, col='gray30', lty=2) {
            abline(v=Xmap(wi), lty=lty, col=col)

            text(Xmap(wi[1]), par('usr')[4], txt, col=col, xpd=T, pos=4,
                 cex = pe_graphics_settings_cache$get('rave_cex.lab') * get_cex_for_multifigure()
            )
        }

    # label the baseline windows
    sapply(baseline_settings$window, render_vertical, 'Baseline')

    # label the analysis window(s)
    render_window <- function(as, adjust_text=c(0,0), ...) {

            if(analysis_window_type == 'line') {
                render_vertical(as$time, as$label, col='black')
            } else {
                draw_box(x,y,lty=2,lwd=2, col='black')
                text(x[1], y[1] + diff(y)*0.90, as$label, col='black', xpd=T, pos=4,
                     cex = pe_graphics_settings_cache$get('rave_cex.lab') * get_cex_for_multifigure()
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
