`%OF%` <- ravepipeline:::`%OF%`

diagnose_notch_filters <- function(
  subject, electrodes, blocks,
  max_freq = 300, winlen = "auto", nbins= 50,
  bg = "white", fg = "black",
  quiet = FALSE, ...
  # cex = 3,
  # std = 3,
  # lwd = 0.3,
  # mar = c(5.2, 5.4, 4.1, 2.1),
  # mai = c(0.6, 0.8, 0.4, 0.1),
) {
  subject <- ravecore::as_rave_subject(subject, strict = FALSE)
  # subject <- local_data$subject
  sample_rates <- subject$preprocess_settings$sample_rates
  all_electrodes <- subject$electrodes
  sample_rates <- subject$raw_sample_rates
  sample_rates <- sapply(electrodes, function(e){
    sample_rates[all_electrodes == e]
  })
  etypes <- subject$electrode_types
  etypes <- sapply(electrodes, function(e){
    etypes[all_electrodes == e]
  })

  if(missing(blocks)) {
    blocks <- subject$preprocess_settings$blocks
  }

  progress <- dipsaus::progress2("Generating diagnostic plots", max = length(electrodes) * 2, quiet = quiet, shiny_auto_close = TRUE)

  old_theme <- graphics::par(c("fg", "bg", "col.axis", "col.lab", "col.main", "col.sub"))
  graphics::par(
    fg = fg,
    bg = bg,
    col.axis = fg,
    col.lab = fg,
    col.main = fg,
    col.sub = fg
  )
  on.exit({
    do.call(graphics::par, old_theme)
  }, add = TRUE)

  FUN <- function(ii) {

    progress$inc(sprintf("Electrode %s (start)", electrodes[[ii]]))

    e <- electrodes[[ii]]
    etype <- etypes[[ii]]
    srate <- sample_rates[[ii]]


    if(isTRUE(winlen == "auto")) {
      winlen <- 2
    }

    if( winlen >= min(srate, 1000)) {
      # winlen should be in seconds, not timepoints
      winlen <- winlen / srate
    }
    if( winlen <= 0.05 ) {
      winlen <- 0.05
    }
    winlen_npts <- floor(winlen * srate)

    for(block in blocks){
      # get signal
      h5file <- file.path(subject$preprocess_path, "voltage", sprintf("electrode_%s.h5", e))
      h5name_raw <- sprintf("raw/%s", block)
      h5name_notch <- sprintf("notch/%s", block)
      has_notch <- h5name_notch %in% gsub("^/", "", ieegio::io_h5_names(h5file))

      raw_signal <- ieegio::io_read_h5(h5file, name = h5name_raw, ram = TRUE)

      if(has_notch) {
        notch_signal <- ieegio::io_read_h5(h5file, name = h5name_notch, ram = TRUE)

        ravetools::diagnose_channel(
          raw_signal,
          notch_signal,
          # sc = ravetools::decimate(raw_signal, ceiling(length(raw_signal) / 5000)),
          srate = srate,
          max_freq = max_freq,
          try_compress = TRUE,
          window = winlen_npts,
          name = c("Original", "Filtered"),
          col = c(fg, "red"),
          nclass = nbins,
          main = sprintf("%s, Block %s, Electrode %s (%s)",
                         subject$subject_id, block, e, etype)
        )
      } else {
        notch_signal <- raw_signal

        ravetools::diagnose_channel(
          raw_signal,
          notch_signal,
          # sc = ravetools::decimate(raw_signal, ceiling(length(raw_signal) / 5000)),
          srate = srate,
          max_freq = max_freq,
          try_compress = TRUE,
          window = winlen_npts,
          name = c("Original", ""),
          col = c(fg, NA),
          nclass = nbins,
          main = sprintf("%s, Block %s, Electrode %s (%s)",
                         subject$subject_id, block, e, etype),
          ...
        )
      }
    }

    progress$inc(sprintf("Electrode %s (end)", electrodes[[ii]]))

  }

  if(length(electrodes) == 1){
    return(FUN(1))
  } else {
    lapply(seq_along(electrodes), FUN)
  }



}
