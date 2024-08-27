#' File whose name starts with `shared-` will be automatically
#' loaded by the pipeline. Hence you can put global static objects
#' here, for example,
#'   - define functions that may be used multiple times,
#'   - declare variables that can be re-used
#'   - import packages that will be used via `library()`, or `targets::tar_option_set` (see below)
NULL

library(raveio)

extdata_path <- normalizePath("./data", mustWork = FALSE)

`%within%` <- function(a, b){
  (a >= min(b)) & (a <= max(b))
}


clip_x <- function(x, lim) {
  if(length(lim) == 1){
    lim = c(lim, -lim)
  }
  x[x<min(lim)] <- min(lim)
  x[x>max(lim)] <- max(lim)

  x
}

mov_window=function(avewidth, ntrials, mvavg_type = "Gaussian"){

  DEFAULT_DECFACTOR = 1 # decimate by this factor by default
  DEFAULT_SDEV  = 1/7 # smooth trials with this window size by default if Gaussian window
  DEFAULT_AVEWIDTH  = 1 # smooth trials with this window size by default
  decfactor = 0;

  if(decfactor == 0) {
    decfactor = DEFAULT_DECFACTOR
  } else if (decfactor > ntrials) {
    decfactor = ntrials
  }

  #Create moving average window
  if (mvavg_type == 'Gaussian') {
    #construct Gaussian window to weight trials
    if (avewidth == 0) {
      avewidth = DEFAULT_SDEV
    } else if (avewidth < 1) {
      stop('Variable `avewidth` must be a positive integer')
    }
    wt_wind = exp(-0.5 * ((-3 * avewidth):(3 * avewidth) / avewidth) ^ 2)
    wt_wind = t(wt_wind)
    wt_wind = wt_wind / (sum(wt_wind)) # normalize to unit sum
    avewidth = length(wt_wind)

    if (avewidth > ntrials) {
      avewidth = floor((ntrials - 1) / 6)
      if (avewidth == 0) {
        avewidth = DEFAULT_SDEV
        #should be a window with one time point (smallest possible)
      }
      wt_wind = exp(-0.5 * ((-3 * avewidth):(3 * avewidth) / avewidth) ^ 2)
      wt_wind = t(wt_wind)
      wt_wind = wt_wind / (sum(wt_wind))
      warning(sprintf('`avewidth` is too big for this number of trials. Changing avewidth to maximum possible size: %.0f', avewidth))
      avewidth = length(wt_wind)
    }
  } else{
    #construct rectangular "boxcar" window to equally weight trials within window
    if (avewidth == 0) {
      avewidth = DEFAULT_AVEWIDTH
    } else if (avewidth < 1) {
      # help erpimage
      stop('Variable `avewidth` cannot be < 1.')
    } else if (avewidth > ntrials) {
      warning('Setting variable avewidth to max %d.\n', ntrials)
      avewidth = ntrials
    }
    wt_wind = c(rep(1, avewidth)) / avewidth  # check if its correct
  }
  return(wt_wind)
}
