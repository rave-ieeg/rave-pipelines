# Implement `rave_unserialize` and `rave_serialize` for custom serialization
# Use these two functions as dispatcher
rave_unserialize <- function(data_path, target_export) {
  return(switch(
    target_export,
    "baselined_power" = {
      unserialize_baselined_power(data_path)
    },
    "pluriform_power" = {
      unserialize_pluriform_power(data_path)
    },
    {
      readRDS(file = data_path)
    }
  ))
}

rave_serialize <- function(object, data_path, target_export) {
  if(file.exists(data_path)) {
    unlink(data_path, recursive = TRUE)
  }
  switch(
    target_export,
    "baselined_power" = {
      # baselined_power has its special path
      return(serialize_baselined_power(object, data_path))
    },
    "pluriform_power" = {
      serialize_pluriform_power(object, data_path)
    },
    {
      saveRDS(object = object, file = data_path)
    }
  )
  return(data_path)
}

# ---- global variables
# this file is independently loaded (shared-xxx.R are not available)
`%within%` <- rutabaga::`%within%`

# ---- target-based implementation

serialize_pluriform_power <- function(object, data_path) {

  if(file.exists(data_path)) {
    unlink(data_path, recursive = TRUE, force = TRUE)
  }
  # make sure data_path exists
  data_path <- raveio::dir_create2(data_path)

  trimmed <- structure(
    names = names(object),
    lapply(seq_along(object), function(ii) {

      sub <- object[[ ii ]]
      structure(
        lapply(seq_along(sub), function(jj) {

          item <- sub[[ jj ]]
          item_path <- raveio::dir_create2(file.path(data_path, ii, jj))

          # convert to filearray and fst
          fst::write_fst(item$data$events, file.path(item_path, "events"))
          filearray::as_filearray(item$data$data,
                                  filebase = file.path(item_path, "data"),
                                  type = "float")

          filearray::as_filearray(item$data$shifted_data,
                                  filebase = file.path(item_path, "shifted_data"),
                                  type = "float")

          item[c("settings", "outliers")]

        }),
        names = names(sub)
      )

    })
  )

  # save trimmed information
  saveRDS(trimmed, file = file.path(data_path, "bootstrap_data"))

}

unserialize_pluriform_power <- function(data_path) {

  bootstrap_path <- file.path(data_path, "bootstrap_data")
  if(!file.exists(bootstrap_path)) { return(NULL) }
  object <- readRDS(bootstrap_path)

  structure(
    names = names(object),
    lapply(seq_along(object), function(ii) {

      sub <- object[[ ii ]]
      structure(
        lapply(seq_along(sub), function(jj) {

          item <- sub[[ jj ]]
          item_path <- file.path(data_path, ii, jj)

          # convert to filearray and fst
          item$data <- list(

            events = fst::read_fst(file.path(item_path, "events"), as.data.table = FALSE),
            data = filearray::filearray_load(file.path(item_path, "data")),
            shifted_data = filearray::filearray_load(file.path(item_path, "shifted_data"))

          )

          if(length(item$outliers)) {
            ravedash::logger('Handling outliers...')
            item$data$clean_data <- subset(item$data$data, drop = FALSE, Trial ~ !(Trial %in% item$outliers))
            item$data$shifted_clean_data <- subset(item$data$shifted_data, drop = FALSE, Trial ~ !(Trial %in% item$outliers))
          } else {
            item$data$clean_data <- item$data$data
            item$data$shifted_clean_data <- item$data$shifted_data
          }

          fi <- as.numeric(dimnames(item$data$shifted_data)$Frequency) %within% unlist(item$settings$frequency)

          item$data$shifted_data_Fsub <- item$data$shifted_data[fi,,,, drop = FALSE]
          item$data$shifted_clean_data_Fsub <- item$data$shifted_clean_data[fi,,,, drop = FALSE]

          item

        }),
        names = names(sub)
      )

    })
  )

}

unserialize_baselined_power <- function(data_path) {
  filearray::filearray_load(data_path, mode = "readonly")
}

serialize_baselined_power <- function(object, data_path) {
  # ignore data_path and use thhe filearray meta path
  normalizePath(object$.filebase, winslash = "/")
}

