
rave_serialize <- function(object, path, target_export) {


  brain_list_to_list <- function(object) {
    object <- lapply(object, function(brain) {
      if(is.null(brain)) { return(NULL) }

      list(
        subject_code = brain$subject_code,
        electrode_table = brain$electrodes$raw_table,
        value_table = brain$electrodes$value_table
      )

    })

    object <- object[!vapply(object, is.null, FALSE)]
    object
  }

  switch (
    target_export,
    'brain_list' = {
      object <- brain_list_to_list(object)
    },

    'template' = {
      # object <- template
      object <- list(
        template_data = list(
          name = object$template_object$subject_code,
          path = object$template_object$base_path
        ),
        subject_data = brain_list_to_list(object$objects)
      )
    }
  )

  saveRDS(object = object, file = path)

}

rave_unserialize <- function(path, target_export) {

  object <- readRDS(path)

  list_to_brain_list <- function(object) {
    brain_list <- lapply(as.list(object), function(brain_data) {
      # brain_data <- object[[1]]
      if(length(brain_data$subject_code) != 1) { return() }

      subject <- ravecore::RAVESubject$new(project_name = "YAEL", subject_code = brain_data$subject_code, strict = FALSE)
      brain <- ravecore::rave_brain(subject = subject, surfaces = "sphere.reg", annotations = NULL, overlays = NULL, include_electrodes = FALSE)
      if(is.null(brain)) { return(brain) }

      if(is.data.frame(brain_data$electrode_table) && nrow(brain_data$electrode_table) > 0) {
        brain$set_electrodes(brain_data$electrode_table, priority = "sphere")
      }
      if(is.data.frame(brain_data$value_table) && nrow(brain_data$value_table) > 0) {
        brain$set_electrode_values(brain_data$value_table)
      }
      brain
    })
    return(brain_list)
  }

  switch (
    target_export,
    'brain_list' = {
      object <- list_to_brain_list(object)
    },

    'template' = {

      brain_list <- list_to_brain_list(object$subject_data)
      object <- threeBrain::merge_brain(
        .list = brain_list,
        template_surface_types = c("pial", "sphere.reg"),
        template_subject = object$template_data$name
      )
    }
  )

  object
}
