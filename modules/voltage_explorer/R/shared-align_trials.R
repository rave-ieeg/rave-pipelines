# I might need to move this into `ravecore` package since the function is very commonly used
# Update: ravecore::realign_trials is done

align_trials <- function(filtered_array, analysis_event_colname, repository) {
  filtered_array_impl <- get_filearray_impl(filtered_array)

  epoch_table <- filtered_array_impl$get_header("epoch_table")
  event_time <- epoch_table[[analysis_event_colname]]
  onset_time <- epoch_table$Time

  delta <- event_time - onset_time
  delta[is.na(delta)] <- 0

  if (all(delta == 0)) {
    # No need to shift array, return filtered array
    return(ravepipeline::RAVEFileArray$new(filtered_array_impl))
  }

  sample_rate <- filtered_array_impl$get_header("sample_rate")

  # No need to cache this file because the repository, analysis event, and filters
  # together determines the cache key from the pipeline level
  filebase <- file.path(pipeline$pipeline_path, "shared", "user", "trials_aligned", fsep = "/")
  if (file.exists(filebase)) {
    unlink(filebase, recursive = TRUE)
  }


  aligned_array_impl <- ravecore::realign_trials(
    x = filtered_array_impl,
    event = analysis_event_colname,
    epoch = repository$epoch,
    sample_rate = sample_rate,
    .filebase = filebase,
    strict = TRUE
  )

  # aligned_array_impl$.mode <- "readonly"
  aligned_array <- ravepipeline::RAVEFileArray$new(aligned_array_impl)

  return(aligned_array)
}
