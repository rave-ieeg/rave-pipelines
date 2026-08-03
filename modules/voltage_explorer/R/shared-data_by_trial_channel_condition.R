prepare_data_by_trial_channel_condition <- function(electrode, aligned_array, data_placeholder, crp_settings) {

  aligned_array_impl <- get_filearray_impl(aligned_array)

  dnames <- dimnames(aligned_array_impl)
  trial_numbers <- dnames$Trial

  time_points <- data_placeholder$time_points
  # loaded_electrodes_clean <- data_placeholder$loaded_lectrodes

  group_data <- lapply(data_placeholder$groups, function(group) {
    # group <- data_placeholder$groups[[1]]
    sub_array <- subset(
      aligned_array_impl,
      Electrode ~ Electrode %in% electrode,
      Trial ~ match(group$trials_included, trial_numbers),
      drop = FALSE
    )
    # Drop dimnames to speed up
    dimnames(sub_array) <- NULL

    # Collapse over trial (the trial is sorted by stimuli names)
    voltage_by_trial <- ravetools::collapse(
      x = sub_array,
      keep = c(1L, 2L),
      average = TRUE
    )

    crp_result <- NULL

    if (isTRUE(crp_settings$enabled)) {
      try({
        crp_result <- do.call(ravetools::crp, c(
          list(x = voltage_by_trial, time = time_points),
          crp_settings$args
        ))
      }, silent = TRUE)
    }

    # Mean and se of mean
    if (length(crp_result$bad_trials)) {
      bad_trials_index <- crp_result$bad_trials
      bad_trials <- as.integer(group$trials_included[bad_trials_index])
      mean_erp <- rowMeans(voltage_by_trial[, - bad_trials_index, drop = FALSE])
    } else {
      bad_trials <- integer(0L)
      mean_erp <- rowMeans(voltage_by_trial)
    }

    return(list(
      # time x trial
      voltage = voltage_by_trial,
      mean = mean_erp,
      bad_trials = bad_trials,
      crp_result = crp_result
    ))
    # return(voltage_by_trial)
  })

  data_by_trial_channel_condition <- ravepipeline::pipeline_plot_data(
    x = data_placeholder, name = "data_by_trial_channel_condition",
    pipe_dir = pipeline$pipeline_path
  )

  data_by_trial_channel_condition$crp_enabled <- isTRUE(crp_settings$enabled)
  data_by_trial_channel_condition$data <- group_data
  data_by_trial_channel_condition
}



# data_by_trial_channel_condition <- prepare_data_by_trial_channel_condition(
#   electrode = 14,
#   aligned_array = aligned_array,
#   data_placeholder = data_placeholder,
#   crp_settings = crp_settings
# )
# plot(data_by_trial_channel_condition)

