prepare_data_by_channel_condition <- function(aligned_array, data_placeholder) {

  aligned_array_impl <- get_filearray_impl(aligned_array)
  loaded_electrodes_clean <- data_placeholder$loaded_lectrodes

  group_data <- lapply(data_placeholder$groups, function(group) {
    # group <- data_placeholder$groups[[1]]
    sub_array <- subset(
      aligned_array_impl,
      Electrode ~ Electrode %in% loaded_electrodes_clean,
      Trial ~ Trial %in% group$trials_included,
      drop = FALSE
    )
    # Drop dimnames to speed up
    dimnames(sub_array) <- NULL

    # # Collapse over trial
    ravetools::collapse(x = sub_array,
                        keep = c(1L, 3L),
                        average = TRUE)
  })

  # Time x Channel x Group
  group_data <- simplify2array(group_data)

  data_by_channel_condition <- ravepipeline::pipeline_plot_data(
    x = data_placeholder, name = "data_by_channel_condition",
    pipe_dir = pipeline$pipeline_path
  )
  data_by_channel_condition$data <- group_data
  data_by_channel_condition
}
