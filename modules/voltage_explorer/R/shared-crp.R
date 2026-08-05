# CRP analysis

# Clean up and validate CRP settings
prepare_crp_settings <- function(
    filtered_array, crp_detection_window,
    crp_time_step = 5, crp_threshold_quantile = 98,
    crp_onset_border = FALSE) {
  filtered_array_impl <- get_filearray_impl(filtered_array)

  sample_rate <- filtered_array_impl$get_header("sample_rate")
  time_range <- filtered_array_impl$get_header("valid_time_range")

  # CRP detection window (t_start, t_end); either entry may be NA (or missing):
  # t_start falls back to 0.01, t_end falls back to the max available time.
  crp_detection_window <- suppressWarnings(as.numeric(unlist(crp_detection_window)))
  crp_detection_window <- crp_detection_window[seq_len(2)]
  crp_time_begin <- crp_detection_window[[1]]
  if (!isTRUE(is.finite(crp_time_begin))) { crp_time_begin <- 0.01 }
  if (crp_time_begin > time_range[[2]]) {
    crp_time_begin <- 0
  }
  crp_time_end <- crp_detection_window[[2]]
  if (!isTRUE(is.finite(crp_time_end))) { crp_time_end <- time_range[[2]] }

  # CRP sweeping step (in samples)
  crp_time_step <- as.integer(crp_time_step)
  if (!isTRUE(crp_time_step >= 1)) { crp_time_step <- 5L }

  # CRP duration-uncertainty threshold (percent -> fraction)
  crp_threshold_quantile <- as.numeric(crp_threshold_quantile)
  if (!isTRUE(crp_threshold_quantile >= 1 && crp_threshold_quantile <= 100)) {
    crp_threshold_quantile <- 98
  }

  # Onset detection: border controls whether to estimate onset and how far back
  # the scan may reach (see `ravetools::crp`)
  detect_onset <- !(identical(crp_onset_border, "disabled") || isFALSE(crp_onset_border))
  # "earliest_possible" -> earliest available time (clamped into [min(time), t_end]
  # by `crp`); "disabled" leaves it NULL since onset is not estimated.
  onset_search_start <- switch(
    as.character(crp_onset_border),
    "event_onset" = 0,
    "t_start" = crp_time_begin,
    "earliest_possible" = time_range[[1]],
    NULL
  )

  # CRP needs at least 11 time-points within the detection window
  crp_enabled <- isTRUE((crp_time_end - crp_time_begin) * sample_rate > 10)

  # Shared arguments for `ravetools::crp()`; reused by every crp() call below.
  # Each call supplies its own `x` and `time` via do.call().
  crp_settings <- list(
    enabled = crp_enabled,
    args = list(
      t_start = crp_time_begin,
      t_end = crp_time_end,
      time_step = crp_time_step,
      threshold_quantile = crp_threshold_quantile / 100,
      artifact_interval = "tR",
      remove_artifacts = TRUE,
      detect_onset = detect_onset,
      onset_search_start = onset_search_start
    )
  )
}


# Run CRP on single electrode
# crp_settings is output from prepare_crp_settings
run_crp_on_one_electrode <- function(electrode, aligned_array, crp_settings, condition_groups_clean) {

  stopifnot(length(electrode) == 1)

  if (!isTRUE(crp_settings$enabled)) { return() }

  aligned_array_impl <- get_filearray_impl(aligned_array)
  dnames <- dimnames(aligned_array_impl)

  if (!any(dnames$Electrode == electrode)) {
    return(NULL)
  }

  cond_groups <- condition_groups_clean$groups

  group_data <- lapply(cond_groups, function(group) {
    # group <- cond_groups[[1]]
    # electrode <- dnames$Electrode[[1]]
    sub_array <- aligned_array_impl[, match(group$trials_included, dnames$Trial),
                                    dnames$Electrode == electrode,
                                    dimnames = NULL, drop = FALSE]
    dm <- dim(sub_array)
    dim(sub_array) <- dm[c(1, 2)]

    crp_result <- do.call(ravetools::crp, c(
      list(x = sub_array, time = dnames$Time),
      crp_settings$args
    ))

    # Trim data size as we only need parameterized data
    crp_result$projections$S_all <- NULL
    crp_result$.data$V <- NULL
    crp_result$parameters$ep <- NULL
    crp_result$parameters$V_tR <- NULL

    # These can be calculated in the future to also delete
    crp_result$projections$stat_indices <- NULL
    crp_result$projections$avg_trace_input <- NULL

    # No need to store time stamps (n_timepoints filled time_range)
    crp_result$.data$n_timepoints <- length(crp_result$.data$time)
    crp_result$.data$time <- NULL

    # Add back some electrode and condition group information
    crp_result$group_index <- group$index
    crp_result$group_label <- group$label
    crp_result$electrode <- electrode

    # ~ 30 KB per channel for 1-3s signals, so 3MB for 100 channels... not good but not bad
    # lobstr::obj_size(crp_result)

    return(crp_result)

    # data.frame(
    #   index = group$index,
    #   Electrode = electrode,
    #   # tau = crp_result$tau_R,
    #   Time = crp_result$parameters$params_times,
    #   C = crp_result$parameters$C
    # )
  })

  group_data
}

# Run CRP on all electrodes
run_crp_on_all <- function(aligned_array, crp_settings, condition_groups_clean) {
  if (!isTRUE(crp_settings$enabled)) { return(NULL) }

  aligned_array_impl <- get_filearray_impl(aligned_array)
  dnames <- dimnames(aligned_array_impl)

  electrodes <- dnames$Electrode

  do_crp <- function() {
    ravepipeline::lapply_jobs(
      x = electrodes,
      fun = function(electrode) {
        # tryCatch({
          run_crp_on_one_electrode(
            electrode = electrode,
            aligned_array = aligned_array,
            crp_settings = crp_settings,
            condition_groups_clean = condition_groups_clean
          )
        # }, error = function(e) {
        #   NULL
        # })
      },
      .globals = list(
        run_crp_on_one_electrode = run_crp_on_one_electrode,
        get_filearray_impl = get_filearray_impl,
        aligned_array = ravepipeline::RAVEFileArray$new(aligned_array_impl),
        crp_settings = crp_settings,
        condition_groups_clean = condition_groups_clean
      ), callback = function(e) {
        sprintf("Calculating ERP duration | %s", e)
      }
    )
  }

  if (isTRUE(getOption("rave.debug"))) {
    crp_results <- ravepipeline::with_rave_parallel({
      do_crp()
    })
  } else {
    crp_results <- do_crp()
  }


}

# `run_crp_on_all` returns a list, why not using data.table to contain the results
crp_results_to_df <- function(crp_results) {

  result_list <- lapply(crp_results, function(group_result) {

    if (!length(group_result)) { return() }

    lapply(group_result, function(crp_result) {

      if (!length(crp_result)) { return() }

      # crp_result <- crp_results[[1]][[1]]

      list(
        electrode = crp_result$electrode,
        group_index = crp_result$group_index,
        group_label = crp_result$group_label,

        bad_trials = list(crp_result$bad_trials),

        tau_R = crp_result$tau_R,
        tau_R_lower = crp_result$tau_R_lower,
        tau_R_upper = crp_result$tau_R_upper,
        t_value_tR = crp_result$projections$t_value_tR,

        tau_onset = crp_result$tau_onset,

        C = list(crp_result$parameters$C),
        C_full = list(crp_result$parameters$C_full),


        al = list(crp_result$parameters$al),
        al_mean = mean(crp_result$parameters$al),

        al_p = list(crp_result$parameters$al_p),
        al_p_mean = mean(crp_result$parameters$al_p),

        # mean(crp_result$parameters$al / crp_result$parameters$epep_root)
        snr = list(crp_result$parameters$Vsnr),
        snr_mean = mean(crp_result$parameters$Vsnr),

        # R^2
        expl_var = list(crp_result$parameters$expl_var),
        expl_var_mean = mean(crp_result$parameters$expl_var)
      )

    })
  })

  result_list <- unlist(unname(result_list), recursive = FALSE, use.names = FALSE)
  summary <- data.table::rbindlist(result_list, fill = TRUE)

  summary <- summary[order(summary$electrode, summary$group_index), ]

  summary

}

# Extract the summary data for viewers
prepare_data_crp_3dviewer_value <- function(crp_df) {

  if (length(crp_df)) {
    sub_table <- crp_df[, c(
      "electrode", "group_label", "tau_R", "t_value_tR", "tau_onset",
      "al_mean", "al_p_mean", "snr_mean", "expl_var_mean")]

    names(sub_table) <- c(
      "electrode", "group_label", "tau", "statistics", "onset",
      "coef", "coef_normalized", "snr", "expl_var")

    sub_table <- data.table::melt(sub_table, c("electrode", "group_label"), value.name = "value")
    sub_table <- sub_table[complete.cases(sub_table), ]
    sub_table$variable <- as.character(sub_table$variable)
    value_ranges0 <- lapply(split(sub_table, sub_table$variable), function(sub) {
      absmax <- max(abs(range(sub$value, na.rm = TRUE)))
      structure(names = sub$variable[[1]], list(c(-absmax, absmax)))
    })
    value_ranges0 <- unlist(unname(value_ranges0), recursive = FALSE, use.names = TRUE)

    value_ranges <- list()
    for (label in unique(sub_table$group_label)) {
      value_ranges[sprintf("%s (%s)", names(value_ranges0), label)] <- value_ranges0
    }

    sub_table <- data.table::data.table(
      Electrode = sub_table$electrode,
      vname = sprintf("%s (%s)", sub_table$variable, sub_table$group_label),
      value = sub_table$value
    )

    erp_results_for_viewer <- data.table::dcast(
      sub_table, Electrode ~ vname, value.var = "value")

    attr(erp_results_for_viewer, "value_ranges") <- value_ranges
    # erp_results_for_viewer$Subject <- subject$subject_code
  } else {
    erp_results_for_viewer <- NULL
  }
  erp_results_for_viewer
}

# Extract trial-level CRP parameters (trial x channel per condition).
#
# `name` picks the `crp_df` column to extract and is baked into the result as
# `$data$parameter_name`, so one plot function renders every parameter.
prepare_data_crp_param_by_trial_channel <- function(crp_df, name, data_placeholder) {

  if (!length(crp_df)) {
    return(NULL)
  }

  if (!name %in% names(crp_df)) {
    stop("No column called ", sQuote(name), " in the CRP result table.")
  }

  # The channel axis of every group's `params` matrix; `crp_df` is sorted by
  # electrode, so each group's rows come out in this order
  electrodes <- sort(unique(crp_df$electrode))

  # name = "al"
  crp_params <- lapply(data_placeholder$groups, function(group) {
    # group <- data_placeholder$groups[[1]]
    crp_sub <- crp_df[crp_df$group_index == group$index, ]

    n_trials <- group$n_trials
    samp <- rep(NA_real_, n_trials)

    # trial x electrode
    params <- vapply(seq_len(nrow(crp_sub)), function(ii) {
      bad_trials <- crp_sub$bad_trials[[ii]]
      if (length(bad_trials)) {
        samp[-bad_trials] <- crp_sub[[name]][[ii]]
      } else {
        samp <- crp_sub[[name]][[ii]]
      }
      samp
    }, samp)

    list(
      group = group,
      params = params,
      range = range(params, na.rm = TRUE)
    )
  })

  names(crp_params) <- data_placeholder$group_labels

  data_crp_params <- ravepipeline::pipeline_plot_data(
    x = data_placeholder, name = "data_crp_param_by_trial_channel",
    pipe_dir = pipeline$pipeline_path
  )

  data_crp_params$electrodes <- electrodes
  data_crp_params$data <- list(
    parameter_name = name,
    group_data = crp_params
  )
  data_crp_params
}

# extract CRP canonical shapes for each channel per condition
prepare_data_crp_by_channel <- function(crp_df, data_placeholder) {

  time_points <- data_placeholder$time_points
  samp <- rep(NA_real_, length(time_points))

  crp_df <- crp_df[order(crp_df$electrode, crp_df$group_index, decreasing = FALSE), ]

  c_full <- lapply(split(crp_df, crp_df$group_index), function(sub_cond) {
    # time x channel
    simplify2array(sub_cond$C_full)
  })
  # Time x channel x group
  c_full <- simplify2array(c_full)

  # `drop = FALSE` throughout: with a single condition group the `[, -1]` subset
  # collapses to a plain vector, and the `dim<-` below then fails with
  # "dims [product 1] do not match the length of object".

  # Mean fitted amplitude, the factor that puts `C_full` back into micro-volts.
  # Applied at plot time, not here, so callers can choose (see `scale_back`).
  al_mean <- data.table::dcast(
    crp_df,
    electrode ~ group_index,
    value.var = "al_mean",
    fill = NA_real_
  )
  al_mean <- as.matrix(al_mean[order(al_mean$electrode), ])[, -1, drop = FALSE]
  dim(al_mean) <- c(1, dim(al_mean))

  # TODO: check my order and dimensions
  # offset
  tau_R <- data.table::dcast(
    crp_df,
    electrode ~ group_index,
    value.var = "tau_R",
    fill = NA_real_
  )
  tau_R <- as.matrix(tau_R[order(tau_R$electrode), ])[, -1, drop = FALSE]
  dim(tau_R) <- c(1, dim(tau_R))

  if ("tau_onset" %in% names(crp_df)) {
    tau_onset <- data.table::dcast(
      crp_df,
      electrode ~ group_index,
      value.var = "tau_onset",
      fill = NA_real_
    )
    tau_onset <- as.matrix(tau_onset[order(tau_onset$electrode), ])[, -1, drop = FALSE]
    dim(tau_onset) <- c(1, dim(tau_onset))
  } else {
    tau_onset <- array(NA_real_, dim(tau_R))
  }

  crp_by_channel <- ravepipeline::pipeline_plot_data(
    x = data_placeholder, name = "data_crp_by_channel",
    pipe_dir = pipeline$pipeline_path
  )

  # CRP runs on every loaded channel, so this axis is a superset of the LFP-only
  # `coord_table`; the plots resolve `electrode_mask` against it by number
  crp_by_channel$electrodes <- sort(unique(crp_df$electrode))

  # `canonical` is the unit-free CRP shape; multiplying by `al_mean` puts it back
  # into micro-volts. They are reported separately so the plots can offer that as
  # a choice rather than baking it in.
  crp_by_channel$data <- list(
    canonical = c_full,
    al_mean = al_mean,
    onset = tau_onset,
    offset = tau_R
  )

  crp_by_channel
}












