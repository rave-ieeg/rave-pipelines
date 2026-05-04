calculate_spectrogram <- function(
    repository, channel, condition_groupings,
    baseline_window = c(-0.5, 0),
    frequencies = exp(seq(0, log(200), length.out = 50)),
    cycles = NULL,
    spectrogram_sample_rate = 100
) {

  # list2env(envir = .GlobalEnv, x = list(
  #   baseline_window = c(-0.5, 0),
  #   frequencies = exp(seq(0, log(200), length.out = 50)),
  #   spectrogram_sample_rate = 100,
  #   channel = 14
  # ))

  do.call(stopifnot, structure(
    names = c(
      "Repository must be a RAVE voltage repository with epochs",
      sprintf("Requested channel length must be one instead of %s", dipsaus::deparse_svec(channel)),
      sprintf("Requested channel is not loaded: %s", channel)
    ),
    list(
      inherits(repository, "RAVESubjectEpochVoltageRepository"),
      length(channel) == 1,
      isTRUE(channel %in% repository$electrode_list)
    )
  ))

  if (is.list(condition_groupings$groups)) {
    condition_groupings_clean <- ravecore::validate_condition_groupings(
      condition_groupings = condition_groupings$groups, epoch = repository$epoch)
  } else {
    condition_groupings_clean <- ravecore::validate_condition_groupings(
      condition_groupings = condition_groupings, epoch = repository$epoch)
  }

  # Hard-coded for now
  baseline_window <- ravecore::validate_time_window(baseline_window)

  # Get globals
  time_points <- repository$voltage$dimnames$Time
  n_time_points <- length(time_points)
  time_range <- range(time_points, na.rm = TRUE)
  voltage_sample_rate <- repository$sample_rate

  # Calculate baseline time indices
  baseline_indexpoints <- lapply(baseline_window, function(window) {
    which(time_points >= window[[1]] & time_points <= window[[2]])
  })
  baseline_indexpoints <- sort(unlist(baseline_indexpoints))
  if (length(baseline_indexpoints) == 0) {
    tmp <- sapply(unlist(baseline_window), function(time) {
      dist <- abs(time_points - time)
      idx <- which.min(dist)
      c(idx, dist[[idx]])
    })
    baseline_indexpoints <- tmp[1, which.min(tmp[2, ])]
  }


  # Calculate new time index
  new_time_indx <- round(seq(1, length(time_points), by = voltage_sample_rate / spectrogram_sample_rate))
  new_time_indx <- round(new_time_indx)
  n_new_timepoints <- length(new_time_indx)

  # Wavelet kernels in frequency domain
  freq_range <- range(frequencies)
  if (length(cycles) == 1) {
    cycles <- data.frame(Frequency = frequencies, Cycles = rep(cycles, length(frequencies)))
  } else if (length(cycles) == 2 && length(frequencies) != 2) {
    cycles <- ravetools::wavelet_cycles_suggest(
      freqs = frequencies,
      frequency_range = range(frequencies),
      cycle_range = cycles
    )
  } else {
    max_lower_cycle <- ceiling((time_range[[2]] - time_range[[1]]) / 3 / min(frequencies))
    cycles <- ravetools::wavelet_cycles_suggest(
      freqs = frequencies,
      frequency_range = c(1, 200),
      cycle_range = c(max_lower_cycle, 20)
    )
  }

  n_freqs <- nrow(cycles)
  kernels <- ravetools::wavelet_kernels(
    freqs = cycles$Frequency,
    wave_num = cycles$Cycles,
    srate = voltage_sample_rate
  )

  # Used in step 4
  ind1 <- seq_len(ceiling(n_time_points / 2))
  ind2 <- seq(ceiling(n_time_points / 2) + 1, n_time_points)
  ind_new <- c(ind2, ind1)

  ind_phase <- ind_new[new_time_indx]
  ind_power_baseline <- ind_new[baseline_indexpoints]

  container <- repository$get_container(electrodes = channel)

  channel_data <- container$data_list[[sprintf("e_%d", channel)]]

  group_data <- lapply(condition_groupings_clean$groups, function(group) {
    # group <- condition_groupings$groups[[1]]
    sub_array <- subset(
      channel_data,
      Trial ~ Trial %in% group$trials_included,
      drop = FALSE
    )
    dimnames(sub_array) <- NULL
    dim(sub_array) <- dim(sub_array)[c(1, 2)]    # time x trial

    # fft_data <- stats::mvfft(sub_array)
    fft_data <- ravetools:::mvfftw_r2c(sub_array, HermConj = 1L)

    coef <- lapply(seq_along(kernels$kernels), function(ii) {
      w <- kernels$kernels[[ii]]
      wave_len <- length(w)

      # 1. Center-pad the time-domain wavelet to ntpts
      n_pre  <- ceiling(n_time_points / 2) - floor(wave_len / 2)
      n_post <- n_time_points - n_pre - wave_len
      x <- c(rep(0 + 0i, n_pre), w, rep(0 + 0i, n_post))

      # 2. FFT the wavelet and conjugate -> frequency-domain kernel
      fft_kernel <- Conj(stats::fft(x))

      # 3. Multiply in frequency domain and IFFT
      # tmp <- stats::mvfft(fft_kernel * fft_data, inverse = TRUE) / n_time_points
      tmp <- ravetools:::mvfftw_c2c(fft_kernel * fft_data, inverse = 1L) / n_time_points

      # 4. Circular shift by n_time_points/2 to align time axis
      # ind <- seq_len(ceiling(n_time_points / 2))

      # time x trial
      # tmp <- rbind(tmp[-ind, , drop = FALSE], tmp[ind, , drop = FALSE]) / sqrt(voltage_sample_rate / 2)

      # ITPC
      # phase <- exp(1i * Arg(tmp[new_time_indx, , drop = FALSE]))
      phase <- exp(1i * Arg(tmp[ind_phase, , drop = FALSE]))
      itpc <- Mod(rowMeans(phase))

      # baselined power in dB
      power <- ravetools::baseline_array(
        x = Mod(tmp)^2,
        along_dim = 1,
        unit_dims = 2,

        # Baseline on recalculated indices
        baseline_indexpoints = ind_power_baseline,
        method = "decibel"
      )

      # smooth average
      avg_power <- ravetools::resample(
        # Re-index wavelet after calculating the mean to save time
        x = rowMeans(power)[ind_new],
        q = voltage_sample_rate,
        p = spectrogram_sample_rate
      )

      if (length(avg_power) < length(new_time_indx)) {
        avg_power <- c(avg_power[[length(avg_power)]],
                       rep(NA, length(new_time_indx) - length(avg_power)))
      }
      avg_power <- avg_power[seq_along(new_time_indx)]

      list(
        itpc = itpc,
        power = avg_power
      )

    })

    sample_ret <- double(length(new_time_indx))

    # time x freq
    itpc <- vapply(coef, "[[", sample_ret, "itpc")
    power <- vapply(coef, "[[", sample_ret, "power")

    list(
      itpc = itpc,
      power = power
    )

  })


  ret <- condition_groupings_clean

  # Sample rate
  ret$sample_rate <- spectrogram_sample_rate

  # electrode coordinate table
  ret$coord_table <- repository$electrode_table[repository$electrode_table$Electrode == channel, ]

  # time points
  ret$time_points <- (seq_len(n_new_timepoints) - 1) / spectrogram_sample_rate + time_range[[1]]

  ret$frequencies <- cycles$Frequency
  ret$wavelet_cycles <- cycles$Frequency

  # time x frequency x condition
  itpc <- vapply(group_data, "[[", FUN.VALUE = group_data[[1]]$itpc, "itpc")
  power <- vapply(group_data, "[[", FUN.VALUE = group_data[[1]]$power, "power")
  ret$data <- list(
    itpc = itpc,
    power = power
  )
  ret
}
