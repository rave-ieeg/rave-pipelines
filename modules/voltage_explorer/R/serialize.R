# Implement `rave_unserialize` and `rave_serialize` for custom serialization
# Use these two functions as dispatcher
rave_unserialize <- function(data_path, target_export) {
  return(switch(
    target_export,
    "baselined_voltage" = {
      unserialize_filearray(data_path)
    },
    "analysis_params" = {
      unserialize_constrained(data_path)
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
    "baselined_voltage" = {
      # baselined_voltage has its special path
      return(serialize_filearray(object, data_path))
    },
    "analysis_params" = {
      return(serialize_constrained(object, data_path))
    },
    {
      saveRDS(object = object, file = data_path)
    }
  )
  return(data_path)
}

# ---- global variables
# this file is independently loaded (shared-xxx.R are not available)
# Please make sure this file can run standalone

# ---- target-based implementation
unserialize_filearray <- function(data_path) {

  filebase <- readLines(data_path, n = 1)

  filearray::filearray_load(filebase, mode = "readonly")
}

serialize_filearray <- function(object, data_path) {
  # used by unserializer to obtain the filebase
  writeLines(object$.filebase, data_path)

  # ignore data_path and use the filearray filebase for signature calculation
  normalizePath(object$.filebase, winslash = "/")
}

# ---- RAVEVariable, RAVEVariableCollections --------------------
unserialize_constrained <- function(data_path) {

  raveio <- asNamespace("raveio")
  env <- new.env(parent = raveio)

  # relative to this file
  source("./R/shared-class.R", local = env)

  config <- readRDS(data_path)
  raveio$restore_list(config, env = env)

}

serialize_constrained <- function(object, data_path) {

  raveio <- asNamespace("raveio")

  saveRDS(raveio$store_list(object), data_path)

  normalizePath(data_path, winslash = "/")

}

