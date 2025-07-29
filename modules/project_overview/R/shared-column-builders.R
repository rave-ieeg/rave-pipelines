cache_root <- "build/cache"

dir_exists <- function(path) {
  length(path) == 1 && !is.na(path) && dir.exists(path)
}

# pipeline <- raveio::pipeline("project_overview", paths = file.path(rstudioapi::getActiveProject(), "modules"))
# pipeline$run("snapshot_results")
# cache_root <- file.path(rstudioapi::getActiveProject(), "modules", "project_overview", "build/cache")

subjects_columns <- list(
  subject = function(subject) {
    subject$subject_code
  },
  reference = function(subject) {
    snapshot <- read_subject_snapshot(subject)

    ref_names <- snapshot$reference_names
    filtered_ref_names <- ref_names[ref_names != "_unsaved"]
    reference_lines <- lapply(
      filtered_ref_names,
      function(name) {
        ref <- snapshot$reference_tables[[name]]
        if(!nrow(ref)) { return(NULL) }
        # bip_count <- sum(ref$Type == "Bipolar Reference", na.rm = TRUE)
        # car_count <- sum(ref$Type %in% c("Common Average Reference", "White-Matter Reference"), na.rm = TRUE)
        noref_count <- sum(ref$Type %in% c("No Reference"), na.rm = TRUE)
        bad_channels <- sum(ref$Type %in% c(""), na.rm = TRUE)
        other_count <- nrow(ref) - noref_count - bad_channels

        glue::glue(
          "<span class=\"nowrap\">{name}: [Noref={noref_count}, Excluded={bad_channels}]</span>"
        )
      }
    )
    paste(unlist(reference_lines), collapse = "<br />")
  },
  electrodes = function(subject) {
    snapshot <- read_subject_snapshot(subject)

    # electrodes_meta_file <- file.path(
    #   subject$meta_path,
    #   "electrodes.csv"
    # )
    #
    # if (file.exists(electrodes_meta_file)) {
    #   electrodes_meta_data <- tryCatch(
    #     {
    #       read.csv(
    #         electrodes_meta_file,
    #         stringsAsFactors = FALSE
    #       )
    #     },
    #     error = function(e) {
    #       print_error(paste0("Error reading electrodes meta file: ", e$message))
    #       data.frame()
    #     }
    #   )
    #   electrodes_meta_count <- nrow(electrodes_meta_data)
    # } else {
    #   electrodes_meta_count <- 0
    # }

    length(snapshot$electrodes)
    # paste(
    #   length(snapshot$electrodes),
    #   "electrodes with data<br/>",
    #   electrodes_meta_count,
    #   "electrodes in meta file"
    # )
  },
  "recording blocks" = function(subject) {
    snapshot <- read_subject_snapshot(subject)
    epoch_names <- snapshot$epoch_names

    blocks <- list()
    for (epoch_name in epoch_names) {
      tryCatch(
        {
          epoch_data <- snapshot$epoch_tables[epoch_name]
          blocks <- unique(c(blocks, epoch_data$Block))
        },
        error = function(e) {
          print_error(
            glue::glue("Error processing epoch '{epoch_name}': {e$message}")
          )
        }
      )
    }
    if (length(blocks) == 0) {
      return("-")
    }

    blocks_str <- paste(blocks, collapse = ", ")
    paste(
      length(blocks),
      "blocks:<br />",
      blocks_str
    )
  },
  epoch = function(subject) {
    snapshot <- read_subject_snapshot(subject)
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

          cache_file <- get_subject_cache_file(
            paste0("epoch_", epoch_name, ".html"),
            subject
          )

          table_html <- knitr::kable(epoch_table, format = "html")

          html <- glue::glue(
            "<!DOCTYPE html>
              <html>
              <head>
                <meta charset=\"UTF-8\">
                <meta name=\"rave-subject\" content=\"{subject$subject_code}\">
                <meta name=\"rave-data-type\" content=\"epoch\">
                <title>Table View</title>
                <style>
                  table {{ border-collapse: collapse; width: 100%; }}
                  th, td {{ border: 1px solid #ddd; padding: 8px; }}
                  th {{ background-color: #f2f2f2; }}
                </style>
              </head>
              <body>
                {table_html}
              </body>
            </html>"
          )
          writeLines(html, cache_file$write_path)

          cache_file$link_tag(
            glue::glue(
              "<strong>{epoch_name}</strong>: {trials_count} trials"
            )
          )
        }
      ),
      collapse = "<br />"
    )
  },
  "3D Viewer" = function(subject) {
    snapshot <- read_subject_snapshot(subject)

    if (!is.null(snapshot$viewer_path)) {
      cache_file <- get_subject_cache_file("viewer.html", subject)
      return(cache_file$link_tag("viewer"))
    }

    "No viewer"
  },
  "Report Time" = function(subject) {
    snapshot <- read_subject_snapshot(subject)

    snapshot_date <- snapshot$snapshot_date
    if(!length(snapshot_date)) {
      return(NA)
    }
    strftime(snapshot_date, usetz = TRUE)
  }
)


snapshot_subject <- function(subject_id, cache_folder = NULL, use_cache = TRUE)  {

  if(length(cache_folder) == 1) {
    raveio::dir_create2(cache_folder)
    snapshot_path <- file.path(cache_folder, "snapshot.rds")
    viewer_path <- file.path(cache_folder, "viewer.html")
  } else {
    snapshot_path <- NULL
    viewer_path <- NULL
  }

  subject <- raveio::as_rave_subject(subject_id, strict = FALSE)


  if(length(viewer_path) && (!use_cache || !file.exists(viewer_path))) {
    brain <- raveio::rave_brain(subject = subject, surfaces = c("pial", "inflated", "white"))
    threeBrain::save_brain(brain$plot(), path = viewer_path, title = subject$subject_id)
  }

  if(use_cache && length(snapshot_path) && file.exists(snapshot_path)) {
    return(readRDS(snapshot_path))
  }

  subject <- raveio::as_rave_subject(subject_id, strict = FALSE)

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

  # subject validation, no parallel (parallel at subject level)
  raveio::with_future_parallel(
    max_workers = 1L,
    {
      validation <- raveio::validate_subject(subject = subject, method = "normal", verbose = FALSE)
    }
  )

  re <- list(
    subject = subject$subject_id,
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

list_subjects <- function(project_names) {
  unlist(
    lapply(project_names, function(project_name) {
      project <- raveio::as_rave_project(project_name, strict = FALSE)
      sprintf("%s/%s", project_name, project$subjects())
    })
  )
}


snapshot_project <- function(project_name, template = "fsaverage", cache_root = NULL, use_cache = TRUE) {
  subject_ids <- list_subjects(project_name)
  force(cache_root)
  force(use_cache)

  raveio::lapply_async(subject_ids, function(subject_id) {
    if(length(cache_root) == 1) {
      subject <- raveio::as_rave_subject(subject_id, strict = FALSE)
      cache_folder_subject <- file.path(cache_root, subject$project_name, subject$subject_code)
    } else {
      cache_folder_subject <- NULL
    }
    snapshot_subject(subject_id, cache_folder = cache_folder_subject, use_cache = use_cache)
    return()
  }, callback = function(subject_id) {
    sprintf('Creating snapshot|%s', subject_id)
  })

  if(length(cache_root) == 1 && !is.na(cache_root)) {
    group_viewer_path <- file.path(cache_root, project_name, sprintf("group_viewer-%s.html", template))

    if(!use_cache || !file.exists(group_viewer_path)) {
      brain_instances <- lapply(subject_ids, function(subject_id) {
        raveio::rave_brain(subject_id)
      })
      template_brain <- threeBrain::merge_brain(.list = brain_instances, template_surface_types = "inflated")
      template_viewer <- template_brain$plot()
      threeBrain::save_brain(template_viewer, path = group_viewer_path)
    }
  }
  subject_ids
}



