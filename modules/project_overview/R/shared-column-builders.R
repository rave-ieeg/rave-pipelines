read_subject_snapshot <- function(subject_code, cache_dir) {
  if (is.character(subject_code)) {
    code <- subject_code
  } else {
    # Accept subject object for backward compatibility with column builders
    code <- subject_code$subject_code
  }
  snapshot_path <- file.path(cache_dir, "subjects", code, "snapshot.rds")
  if (!file.exists(snapshot_path)) {
    stop("Snapshot not found for subject: ", code,
         " (expected at: ", snapshot_path, ")")
  }
  readRDS(snapshot_path)
}

read_group_snapshot <- function(cache_dir) {
  snapshot_path <- file.path(cache_dir, "group", "snapshot.rds")
  if (!file.exists(snapshot_path)) {
    return(NULL)
  }
  readRDS(snapshot_path)
}

make_subjects_columns <- function(cache_dir) {

  read_snapshot <- function(subject) {
    read_subject_snapshot(subject, cache_dir = cache_dir)
  }

  list(
    subject = function(subject) {
      subject$subject_code
    },
    reference = function(subject) {
      snapshot <- read_snapshot(subject)

      ref_names <- snapshot$reference_names
      filtered_ref_names <- ref_names[ref_names != "_unsaved"]
      reference_lines <- lapply(
        filtered_ref_names,
        function(name) {
          ref <- snapshot$reference_tables[[name]]
          if(!nrow(ref)) { return(NULL) }
          noref_count <- sum(ref$Type %in% c("No Reference"), na.rm = TRUE)
          bad_channels <- sum(ref$Type %in% c(""), na.rm = TRUE)

          glue::glue(
            "<span class=\"nowrap\">{name}: [Noref={noref_count}, Excluded={bad_channels}]</span>"
          )
        }
      )
      paste(unlist(reference_lines), collapse = "<br />")
    },
    electrodes = function(subject) {
      snapshot <- read_snapshot(subject)
      length(snapshot$electrodes)
    },
    "recording blocks" = function(subject) {
      snapshot <- read_snapshot(subject)
      epoch_names <- snapshot$epoch_names

      blocks <- list()
      for (epoch_name in epoch_names) {
        tryCatch(
          {
            epoch_data <- snapshot$epoch_tables[epoch_name]
            blocks <- unique(c(blocks, epoch_data$Block))
          },
          error = function(e) {
            # silently skip
          }
        )
      }
      if (length(blocks) == 0) {
        return("-")
      }

      blocks_str <- paste(blocks, collapse = ", ")
      paste(length(blocks), "blocks:<br />", blocks_str)
    },
    epoch = function(subject) {
      snapshot <- read_snapshot(subject)
      epoch_names <- snapshot$epoch_names

      if (length(epoch_names) == 0) {
        return("No epochs found")
      }

      paste(
        lapply(
          epoch_names,
          function(epoch_name) {
            epoch_table <- snapshot$epoch_tables[[epoch_name]]
            if (is.null(epoch_table)) {
              return(glue::glue("<strong>{epoch_name}</strong>: No data"))
            }
            trials_count <- nrow(epoch_table)
            glue::glue("<strong>{epoch_name}</strong>: {trials_count} trials")
          }
        ),
        collapse = "<br />"
      )
    },
    "3D Viewer" = function(subject) {
      snapshot <- read_snapshot(subject)
      if (!is.null(snapshot$viewer_path) && file.exists(snapshot$viewer_path)) {
        return("Available")
      }
      "No viewer"
    },
    "Report Time" = function(subject) {
      snapshot <- read_snapshot(subject)
      snapshot_date <- snapshot$snapshot_date
      if(!length(snapshot_date)) {
        return(NA)
      }
      strftime(snapshot_date, usetz = TRUE)
    }
  )
}


snapshot_subject <- function(subject_id, cache_dir = NULL, snapshot_dir = NULL, use_cache = TRUE) {

  subject <- ravecore::as_rave_subject(subject_id, strict = FALSE)
  subject_code <- subject$subject_code

  if(length(cache_dir) == 1) {
    subject_cache <- file.path(cache_dir, "subjects", subject_code)
    ravepipeline::dir_create2(subject_cache)
    snapshot_path <- file.path(subject_cache, "snapshot.rds")
  } else {
    subject_cache <- NULL
    snapshot_path <- NULL
  }

  # Viewer goes to snapshot_dir (project group_path) for standalone_report access
  if(length(snapshot_dir) == 1) {
    viewer_dir <- file.path(snapshot_dir, "subjects", subject_code)
    ravepipeline::dir_create2(viewer_dir)
    viewer_path <- file.path(viewer_dir, "viewer.html")
  } else {
    viewer_path <- NULL
  }

  if(length(viewer_path) && (!use_cache || !file.exists(viewer_path))) {
    brain <- ravecore::rave_brain(subject = subject, surfaces = c("pial", "inflated", "white"))
    threeBrain::save_brain(brain$plot(), path = viewer_path, title = subject$subject_id)
  }

  if(use_cache && length(snapshot_path) && file.exists(snapshot_path)) {
    return(readRDS(snapshot_path))
  }

  # Gather information
  electrodes <- subject$electrodes

  # preprocessing data
  channel_info <- lapply(electrodes, subject$preprocess_settings$electrode_info)
  channel_info <- data.table::rbindlist(channel_info)

  # electrode coordinates
  electrode_coordinates <- subject$meta_data("electrodes")

  # epoch information
  epoch_names <- subject$epoch_names
  epoch_tables <- structure(
    names = epoch_names,
    lapply(epoch_names, function(epoch_name) {
      subject$meta_data(meta_type = "epoch", meta_name = epoch_name)
    })
  )

  # reference information
  reference_names <- subject$reference_names
  reference_tables <- structure(
    names = reference_names,
    lapply(reference_names, function(reference_name) {
      subject$meta_data(meta_type = "references", meta_name = reference_name)
    })
  )

  # subject validation
  validation <- ravecore::validate_subject(subject = subject, method = "normal", verbose = FALSE)

  re <- list(
    subject = subject$subject_id,
    subject_code = subject_code,
    project_name = subject$project_name,
    electrodes = electrodes,
    channel_info = channel_info,
    electrode_coordinates = electrode_coordinates,
    epoch_names = epoch_names,
    epoch_tables = epoch_tables,
    reference_names = reference_names,
    reference_tables = reference_tables,
    validation = validation,
    viewer_path = viewer_path,
    snapshot_date = Sys.time()
  )

  if(length(snapshot_path) == 1) {
    saveRDS(re, file = snapshot_path)
  }
  re

}

list_subjects <- function(project_name) {
  project <- ravecore::as_rave_project(project_name, strict = FALSE)
  project$subjects()
}


snapshot_project <- function(project_name, subject_codes = NULL, template = "fsaverage",
                             cache_dir = NULL, snapshot_dir = NULL, use_cache = TRUE,
                             sections = list()) {

  if (is.null(subject_codes) || !length(subject_codes)) {
    project <- ravecore::as_rave_project(project_name, strict = FALSE)
    subject_codes <- project$subjects()
  }
  subject_ids <- sprintf("%s/%s", project_name, subject_codes)

  force(cache_dir)
  force(snapshot_dir)
  force(use_cache)

  # Snapshot each subject
  ravepipeline::lapply_jobs(subject_ids, function(subject_id) {
    snapshot_subject(subject_id, cache_dir = cache_dir,
                     snapshot_dir = snapshot_dir, use_cache = use_cache)
    return()
  }, .globals = list(
    cache_dir = cache_dir,
    snapshot_dir = snapshot_dir,
    snapshot_subject = snapshot_subject,
    use_cache = use_cache
  ), callback = function(subject_id) {
    sprintf('Creating snapshot|%s', subject_id)
  })

  # Generate group-level viewer
  group_viewer <- isTRUE(sections$group_viewer)
  if (group_viewer && length(snapshot_dir) == 1 && !is.na(snapshot_dir)) {
    group_viewer_path <- file.path(snapshot_dir, "group", "group_viewer.html")
    ravepipeline::dir_create2(dirname(group_viewer_path))

    if (!use_cache || !file.exists(group_viewer_path)) {
      brain_instances <- lapply(subject_ids, function(subject_id) {
        ravecore::rave_brain(subject_id)
      })
      template_brain <- threeBrain::merge_brain(.list = brain_instances,
                                                 template_surface_types = "inflated")
      template_viewer <- template_brain$plot()
      threeBrain::save_brain(template_viewer, path = group_viewer_path)
    }
  }

  # Build group snapshot with electrode coverage data
  if (length(cache_dir) == 1) {
    group_cache <- file.path(cache_dir, "group")
    ravepipeline::dir_create2(group_cache)

    # Electrode coverage: collect electrode coordinates from all subjects
    all_coords <- lapply(subject_ids, function(subject_id) {
      subject <- ravecore::as_rave_subject(subject_id, strict = FALSE)
      snapshot <- tryCatch(
        read_subject_snapshot(subject$subject_code, cache_dir),
        error = function(e) NULL
      )
      if (is.null(snapshot)) return(NULL)
      coords <- snapshot$electrode_coordinates
      if (is.null(coords) || !nrow(coords)) return(NULL)
      coords$Subject <- subject$subject_code
      coords
    })
    all_coords <- do.call(rbind, Filter(Negate(is.null), all_coords))

    group_snapshot <- list(
      project_name = project_name,
      subject_codes = subject_codes,
      electrode_coverage = all_coords,
      snapshot_date = Sys.time()
    )
    saveRDS(group_snapshot, file.path(group_cache, "snapshot.rds"))
  }

  subject_ids
}


