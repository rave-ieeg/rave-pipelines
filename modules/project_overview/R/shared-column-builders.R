subjects_columns <- list(
  subject = function(subject) {
    subject$subject_code
  },
  reference = function(subject) {
    ref_names <- subject$reference_names
    filtered_ref_names <- ref_names[ref_names != "_unsaved"]
    reference_lines <- lapply(
      filtered_ref_names,
      function(name) {
        ref <- subject$get_reference(name)
        bip_count <- sum(ref$Type == "Bipolar Reference", na.rm = TRUE)
        car_count <- sum(ref$Type == "Common Average Reference", na.rm = TRUE)
        other_count <- nrow(ref) - bip_count - car_count

        glue::glue(
          "<span class=\"nowrap\">{name}: (CAR {car_count}, Bipolar {bip_count}, Other {other_count})</span>"
        )
      }
    )
    paste(reference_lines, collapse = "<br />")
  },
  electrodes = function(subject) {
    electrodes_meta_file <- file.path(
      subject$meta_path,
      "electrodes.csv"
    )

    if (file.exists(electrodes_meta_file)) {
      electrodes_meta_data <- tryCatch(
        {
          read.csv(
            electrodes_meta_file,
            stringsAsFactors = FALSE
          )
        },
        error = function(e) {
          print_error(paste0("Error reading electrodes meta file: ", e$message))
          data.frame()
        }
      )
      electrodes_meta_count <- nrow(electrodes_meta_data)
    } else {
      electrodes_meta_count <- 0
    }

    paste(
      length(subject$electrodes),
      "electrodes with data<br/>",
      electrodes_meta_count,
      "electrodes in meta file"
    )
  },
  "recording blocks" = function(subject) {
    epoch_names <- subject$epoch_names

    blocks <- list()
    for (epoch_name in epoch_names) {
      tryCatch(
        {
          epoch_data <- subject$get_epoch(epoch_name)$table
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
    epoch_names <- subject$epoch_names

    if (length(epoch_names) == 0) {
      return("No epochs found")
    }

    paste(
      lapply(
        epoch_names,
        function(epoch_name) {
          epoch <- NULL
          tryCatch(
            {
              lalala <- epoch_name
              epoch <- subject$get_epoch(lalala)
            },
            error = function(e) {
              print_error(
                glue::glue("Error retrieving epoch '{epoch_name}': {e$message}")
              )
            }
          )
          if (is.null(epoch)) {
            return(glue::glue("<strong>{epoch_name}</strong>: No data"))
          }
          trials_count <- epoch$n_trials

          cache_file <- get_subject_cache_file(
            paste0("epoch_", epoch_name, ".html"),
            subject
          )

          if (!cache_file$exists) {
            table_data <- subject$get_epoch(epoch_name)$table
            table_html <- knitr::kable(table_data, format = "html")

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
          }

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
    cache_file <- get_subject_cache_file("viewer.html", subject)

    if (!cache_file$exists) {
      brain <- raveio::rave_brain(subject)
      if (is.null(brain)) {
        return("-")
      }
      brain$set_electrode_values()
      controllers <- list()
      if ("LabelPrefix" %in% names(brain$electrodes$raw_table)) {
        controllers[["Display Data"]] <- "LabelPrefix"
      }
      viewer <- brain$plot(controllers = controllers)

      threeBrain::save_brain(
        viewer,
        title = subject$subject_id, path = cache_file$write_path,
      )
    }

    cache_file$link_tag("viewer")
  }
)
