ALLOWED_FILTER_TYPES <- c(
  "demean", "detrend", "decimate", "fir_kaiser", "firls", "fir_remez",
  "butter", "cheby1", "cheby2", "ellip", "fir", "iir", "baseline", "hilbert"
)
DEFAULT_VOLTAGE_UNIT <- "MicroVolt"

#' Apply filters to matrix of signals
#' @param signals vector or matrix where each column is a time-series
#' @param type one of the followings `demean`, `detrend`, `decimate`,
#' or "method" in `ravetools::design_filter`, or `fir` (short for `fir_kaiser`),
#' and `iir` for `butter`
#' @param ... passed to the methods. For `decimate`, use `by` (integer) indicate
#' the decimate ratio. For other methods, see `?ravetools::design_filter`
apply_filter <- function(signals, type = ALLOWED_FILTER_TYPES, ...) {
  type <- match.arg(type)

  if( type == "fir" ) {
    type <- "fir_kaiser"
  } else if ( type == "iir" ) {
    type <- "butter"
  }

  if(!is.matrix(signals)) {
    signals <- matrix(signals, ncol = 1L)
    is_vector <- TRUE
  } else {
    is_vector <- FALSE
  }

  dimnames(signals) <- NULL

  args <- list(...)
  switch(
    type,
    "demean" = {
      signals <- gsignal::detrend(signals, p = 0)
    },
    "detrend" = {
      signals <- gsignal::detrend(signals, p = 1)
    },
    "hilbert" = {
      signals <- abs(gsignal::hilbert(signals))
    },
    "decimate" = {
      checkmate::assert_integerish(args$by, lower = 1, any.missing = FALSE, len = 1L, null.ok = FALSE, .var.name = "by")
      if(args$by > 1) {
        signals <- apply(signals, 2L, ravetools::decimate, q = args$by)
      }
    },
    "baseline" = {
      checkmate::assert_numeric(args$sample_rate, lower = 0.1, any.missing = FALSE, len = 1L, finite = TRUE,
                                null.ok = FALSE, .var.name = "sample_rate")
      checkmate::assert_numeric(args$start_time, any.missing = FALSE, len = 1L, finite = TRUE,
                                null.ok = FALSE, .var.name = "sample_rate")
      windows <- raveio::validate_time_window(args$windows)

      slen <- nrow(signals)
      time <- args$start_time + seq(0, by = 1 / args$sample_rate, length.out = slen)
      sel <- rep(FALSE, slen)
      for(window in windows) {
        sel <- sel | (time >= window[[1]] & time <= window[[2]])
      }
      if(length(args$physical_unit) && startsWith(args$physical_unit, "Ampl")) {
        # hilbert then baseline -> amplitude % change
        baseline <- colMeans(signals[sel, , drop = FALSE])
        baseline[is.na(baseline)] <- 0
        tmp <- t(signals) - baseline
        baseline[baseline == 0] <- 100
        signals <- t(tmp / baseline * 100)
      } else {
        signals <- t(t(signals) - colMeans(signals[sel, , drop = FALSE]))
      }
    },
    {
      checkmate::assert_numeric(args$sample_rate, lower = 0.1, any.missing = FALSE, len = 1L, finite = TRUE,
                                null.ok = FALSE, .var.name = "sample_rate")

      ntimepoints <- nrow(signals)
      args$data_size <- ntimepoints
      args$filter_order %?<-% NA
      args$high_pass_freq  %?<-% NA
      args$high_pass_trans_freq %?<-% NA
      args$low_pass_freq %?<-% NA
      args$low_pass_trans_freq %?<-% NA
      args$passband_ripple %?<-% 0.1
      args$stopband_attenuation %?<-% 40
      args$scale %?<-% TRUE

      # if( isTRUE(args$high_pass_freq < 2) ) {
      #   if(!isTRUE(args$high_pass_trans_freq < args$high_pass_freq)) {
      #     args$high_pass_trans_freq <- args$high_pass_freq - 0.0001
      #   }
      # }


      filter <- ravetools::design_filter(
        sample_rate = args$sample_rate,
        filter_order = args$filter_order,
        data_size = args$data_size,
        high_pass_freq = args$high_pass_freq,
        high_pass_trans_freq = args$high_pass_trans_freq,
        low_pass_freq = args$low_pass_freq,
        low_pass_trans_freq = args$low_pass_trans_freq,
        # passband_ripple = args$passband_ripple,
        stopband_attenuation = args$stopband_attenuation,
        scale = args$scale,
        method = type
      )

      signals <- ravetools::filtfilt(b = filter$b, a = filter$a, x = signals)

    }
  )

  if( is_vector ) {
    drop( signals )
  } else {
    signals
  }
}


assert_filter_config <- function(config, ..., disallow_types = NULL) {
  # c(
  #   "demean", "detrend", "decimate", "fir_kaiser", "firls", "fir_remez",
  #   "butter", "cheby1", "cheby2", "ellip", "fir", "iir"
  # )
  # config <- list(type = "fir", low_pass_freq = 30, low_pass_trans_freq = 3)

  config <- c(as.list(config), list(...))

  if(length(config$type) != 1) {
    stop("Unknown filter type. Please specify the `filter$type`.")
  }

  type <- match.arg(config$type, choices = ALLOWED_FILTER_TYPES)

  if( type %in% disallow_types ) {
    stop("Filter ", type, " is disallowed at this stage.")
  }

  ravedash::logger("Checking pre-analysis filter {type}", level = "debug", use_glue = TRUE)

  if( type == "fir" ) {
    type <- "fir_kaiser"
  } else if ( type == "iir" ) {
    type <- "butter"
  }
  config$type <- type

  switch(
    type,
    "demean" = {
      # signals <- gsignal::detrend(signals, p = 0)
    },
    "detrend" = {
      # signals <- gsignal::detrend(signals, p = 1)
    },
    "decimate" = {
      checkmate::assert_integerish(config$by, lower = 1, any.missing = FALSE, len = 1L, null.ok = FALSE, .var.name = "filter$by")
    },
    "baseline" = {
      checkmate::assert_numeric(config$sample_rate, lower = 0.1, any.missing = FALSE, len = 1L, finite = TRUE,
                                null.ok = FALSE, .var.name = "sample_rate")
      checkmate::assert_numeric(config$start_time, any.missing = FALSE, len = 1L, finite = TRUE,
                                null.ok = FALSE, .var.name = "sample_rate")
      config$windows <- raveio::validate_time_window(config$windows)
      if(identical(config$physical_unit, "Amplitude")) {
        config$physical_unit <- "Amplitude % Change"
      }
    },
    "hilbert" = {
      # signals <- gsignal::hilbert(signals)
      config$physical_unit <- "Amplitude"
    },
    {
      checkmate::assert_numeric(config$sample_rate, lower = 0.1, any.missing = FALSE, len = 1L,
                                null.ok = FALSE, .var.name = "filter$sample_rate")
      high_pass_freq %?<-% c(config$high_pass_freq, NA)[[1]]
      high_pass_trans_freq %?<-% c(config$high_pass_trans_freq, NA)[[1]]
      low_pass_freq %?<-% c(config$low_pass_freq, NA)[[1]]
      low_pass_trans_freq %?<-% c(config$low_pass_trans_freq, NA)[[1]]
      # passband_ripple %?<-% 0.1
      # stopband_attenuation %?<-% 40

      nyquist <- config$sample_rate / 2

      checkmate::assert_numeric(
        c(high_pass_freq, high_pass_trans_freq, low_pass_freq, low_pass_trans_freq),
        finite = TRUE, all.missing = FALSE, any.missing = TRUE
      )

      checkmate::assert_numeric(
        high_pass_freq - high_pass_trans_freq,
        lower = 0.0, upper = nyquist, any.missing = TRUE, len = 1L, null.ok = FALSE,
        .var.name = "high-pass frequency")

      checkmate::assert_numeric(
        low_pass_freq + low_pass_trans_freq,
        lower = 0.0, upper = nyquist, any.missing = TRUE, len = 1L, null.ok = FALSE,
        .var.name = "low-pass frequency")

    }
  )

  config

}


apply_filters_to_signals <- function(signals, filter_configs) {
  # assuming signals is a filearray
  signals_dim <- dim(signals)
  niter <- signals_dim[[2]]

  # res <- raveio::lapply_async(seq_len(niter), function(ii) {
  #   slice <- signals[, ii, ]
  #   for(config in filter_configs) {
  #     call <- as.call(c(
  #       list(quote(apply_filter), signals = quote(slice), sample_rate = sample_rate),
  #       config
  #     ))
  #     signals <- eval(call)
  #   }
  #   as.numeric(signals)
  # }, callback = I)

  signals <- signals[drop = TRUE]
  for(config in filter_configs) {
    call <- as.call(c(
      list(quote(apply_filter), signals = quote(signals)), #, sample_rate = sample_rate, start_time = start_time),
      config
    ))
    # print(call)
    signals <- eval(call)
  }
  signals
}


prepare_filtered_data <- function(data_path, repository, filter_configurations) {
  sample_rate <- repository$sample_rate
  time_points <- repository$voltage$dimnames$Time

  start_time <- min(time_points, na.rm = TRUE)
  n_timepoints <- length(time_points)
  new_srate <- sample_rate

  # sanitize and fill in the filters
  configs <- as.list(filter_configurations)

  high_pass <- NA
  low_pass <- NA

  physical_unit <- DEFAULT_VOLTAGE_UNIT

  for(ii in seq_along(configs)) {
    conf <- configs[[ ii ]]
    conf <- assert_filter_config(
      config = conf,
      sample_rate = new_srate,
      start_time = start_time,
      physical_unit = physical_unit
    )
    if(length(conf$physical_unit)) {
      physical_unit <- conf$physical_unit
    }
    if(identical(conf$type, "decimate")) {
      new_srate <- new_srate / conf$by
      n_timepoints <- ceiling(n_timepoints / conf$by)
    } else {
      if( isTRUE(is.finite(conf$high_pass_freq)) ) {
        if( isTRUE(is.finite(conf$low_pass_freq)) ) {
          # either band-pass or band-stop
          if( isTRUE( conf$low_pass_freq > conf$high_pass_freq ) ) {
            # band-pass
            if( !isTRUE(high_pass >= conf$high_pass_freq) ) {
              high_pass <- conf$high_pass_freq
            }
            if( !isTRUE(low_pass <= conf$low_pass_freq) ) {
              low_pass <- conf$low_pass_freq
            }
          }
        } else if( !isTRUE(high_pass >= conf$high_pass_freq) ) {
          high_pass <- conf$high_pass_freq
        }
      } else if( isTRUE(is.finite(conf$low_pass_freq)) ) {
        if( !isTRUE(low_pass <= conf$low_pass_freq) ) {
          low_pass <- conf$low_pass_freq
        }
      }
    }
    configs[[ ii ]] <- conf
  }

  # calculate array dimension
  new_dim <- repository$voltage$dim
  new_dim <- c(n_timepoints, new_dim[[2]], new_dim[[3]])


  # create a temporary filearray and store in the data/ dir (no need to cache
  # globally)
  # no data is written during this process

  array_type <- "pre_analysis_filtered_voltage"

  new_sig <- structure(
    list(
      array_type,
      repository$signature,
      filter_configurations = configs
    )
  )

  pre_analysis_filter_array <- filearray::filearray_load_or_create(
    filebase = file.path('data', array_type, fsep = "/"),
    mode = "readonly",
    type = "float",
    symlink_ok = FALSE,
    partition_size = 1L,
    dimension = new_dim,
    # signatures
    array_type = array_type,

    signature_filters = new_sig,
    signature_repository = repository$signature,

    initialize = FALSE,
    on_missing = function(arr) {
      # no data initialized so no filtered channels
      arr$set_header(key = "filtered_channels", value = c(), save = FALSE)

      # set filter configurations
      arr$set_header(key = "filter_configurations", value = configs, save = FALSE)

      # set epoch table
      arr$set_header(key = "epoch_table", value = repository$epoch_table, save = FALSE)

      # Get new time-points in case the filter decimates the signals
      new_time_points <- start_time + seq(0, by = 1 / new_srate, length.out = n_timepoints)

      # new dimnames
      new_dnames <- list(
        Time = new_time_points,
        Trial = repository$voltage$dimnames$Trial,
        Electrode = repository$voltage$dimnames$Electrode
      )

      arr$set_header(key = "sample_rate", value = new_srate, save = FALSE)

      # expected frequency range
      arr$set_header(key = "frequency_range", value = as.double(c(high_pass, low_pass)), save = FALSE)

      # this will save the header in bulk
      dimnames(arr) <- new_dnames
      arr
    }
  )

  pre_analysis_filter_array
}
