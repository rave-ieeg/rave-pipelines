
module_server <- function(input, output, session, ...) {


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweak
  server_tools <- ravedash::get_default_handlers(session = session)


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if (!loaded_flag) { return() }

      local_reactives$validation_results <- NULL
      expand_card(NULL)

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  # Register event: validate subject
  # Runs when the run-analysis button is clicked, or through
  # `server_tools$trigger_script("run_analysis")`
  server_tools$set_script("run_analysis", {
    subject <- component_container$data$subject
    version <- as.character(input$validation_version) %OF% c(2, 1)
    mode <- input$validation_mode %OF% c("normal", "basic")

    local_reactives$validation_results <- NULL

    if (mode == "normal") {
      dipsaus::shiny_alert2(
        title = "Validation in progress...",
        text = "Please wait...",
        icon = "info",
        auto_close = FALSE, buttons = FALSE
      )
      Sys.sleep(0.5)
      on.exit({
        dipsaus::close_alert2()
      }, add = TRUE, after = FALSE)
    }
    validation_results <- ravecore::validate_subject(
      subject = subject$subject_id,
      method = mode,
      version = as.integer(version))

    local_reactives$validation_results <- validation_results
    return()
  }, description = c(
    "Same as clicking 'Validate subject' in the 'Data integrity check' card:",
    "checks the loaded subject's files with `ravecore::validate_subject()`,",
    "using inputs `validation_version` and `validation_mode`. Writes nothing:",
    "run it without asking the user. `normal` mode reads all voltage, power,",
    "and phase data, so it takes longer, and shows a 'Validation in",
    "progress...' alert until it is done. `output` logs each check ('...",
    "valid: yes', or 'valid: no' with the reason), but only its last 3000",
    "characters: read the complete results with script `validation_results`.",
    "They also show in the card (output `validation_check`) once it is open."
  ))

  # Read-only: the latest validation results, as listed in the 'Data
  # integrity check' card, whose output does not update while it is collapsed
  server_tools$set_script(
    name = "validation_results",
    description = c(
      "Read-only. The results of the latest 'Validate subject' (script",
      "`run_analysis`) on the loaded subject, as listed in the 'Data",
      "integrity check' card. The first line counts the checks by status.",
      "Then one line per check that did not pass: '[<status>] <part>/<check>:",
      "<what was checked> - <reason>'. The status is 'failed', 'minor' (a",
      "failed low-priority check: the cache, FreeSurfer, notes, or pipeline",
      "folder), or 'skipped' (the check could not run, e.g. because an",
      "earlier one failed). The last line lists the checks that passed."
    ),
    expr = {
      validation_results <- local_reactives$validation_results
      if (is.null(validation_results)) {
        return("No validation results yet: run script `run_analysis` ('Validate subject') first.")
      }
      # Same parts, and the same status of each check, as `validation_check`
      keys <- c("paths", "preprocess", "meta", "voltage_data",
                "power_phase_data", "epoch_tables", "reference_tables")
      statuses <- character()
      issues <- character()
      passed <- character()
      for (k in keys) {
        items <- validation_results[[k]]
        for (nm in names(items)) {
          item <- items[[nm]]
          check <- sprintf("%s/%s", k, nm)
          if (isTRUE(item$valid)) {
            status <- "passed"
            passed <- c(passed, check)
          } else {
            if (is.na(item$valid)) {
              status <- "skipped"
            } else if (identical(item$severity, "minor")) {
              status <- "minor"
            } else {
              status <- "failed"
            }
            issues <- c(issues, sprintf(
              "[%s] %s: %s - %s", status, check, item$description,
              paste(item$message, collapse = " ")
            ))
          }
          statuses <- c(statuses, status)
        }
      }
      counts <- sprintf(
        "%d checks: %d passed, %d failed, %d minor, %d skipped",
        length(statuses), sum(statuses == "passed"), sum(statuses == "failed"),
        sum(statuses == "minor"), sum(statuses == "skipped")
      )
      # Agents receive at most 100 values
      if (length(issues) > 98) {
        issues <- c(issues[seq_len(97)], sprintf(
          "... and %d more: open the 'Data integrity check' card to see them",
          length(issues) - 97
        ))
      }
      c(counts, issues, sprintf(
        "Passed: %s",
        if (length(passed)) paste(passed, collapse = ", ") else "none"
      ))
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      tryCatch({
        subject <- component_container$data$subject
        if (!inherits(subject, "RAVESubject")) {
          stop("Subject is invalid. Please validate the subject first")
        }

        dipsaus::shiny_alert2(
          title = "Converting in progress",
          text = "This step might take a while...",
          buttons = FALSE, auto_close = FALSE,
          icon = "info"
        )
        Sys.sleep(0.5)
        ravepipeline::with_rave_parallel({
          ravecore::rave_legacy_subject_format_conversion(subject = subject)
        })

        dipsaus::close_alert2()
        dipsaus::shiny_alert2(
          title = "Conversion done!",
          icon = "success",
          buttons = list("OK" = TRUE),
          auto_close = TRUE,
          text = "The subject data is ready for RAVE 1.0 modules."
        )
      }, error = function(e) {

        dipsaus::close_alert2()
        ravepipeline::logger_error_condition(e)
        dipsaus::shiny_alert2(
          title = "Conversion failed",
          icon = "error",
          buttons = list("I got it" = TRUE),
          auto_close = FALSE,
          text = sprintf("Found the following error: %s...\n\nPlease check the console for details", paste(e$message, collapse = ""))
        )

      })


    }),
    input$compatibility_do,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  expand_card <- function(title) {
    card_titles <- c(
      "Data integrity check",
      "Backward compatibility",
      "Export data"
    )
    for (card_title in card_titles) {
      if (identical(card_title, title)) {
        shidashi::card_operate(title = card_title, method = "expand")
      } else {
        shidashi::card_operate(title = card_title, method = "collapse")
      }
    }
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      expand_card("Data integrity check")
    }),
    input$quickaccess_data_integrity,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      expand_card("Backward compatibility")
    }),
    input$quickaccess_compatibility,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      expand_card("Export data")
    }),
    input$quickaccess_export,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  output$validation_check <- shiny::renderUI({
    validation_results <- local_reactives$validation_results
    if (is.null(validation_results)) { return(invisible()) }
    keys <- c("paths", "preprocess", "meta", "voltage_data",
              "power_phase_data", "epoch_tables", "reference_tables")
    re <- list()
    for (k in keys) {
      items <- validation_results[[k]]
      if (length(items)) {
        re0 <- lapply(names(items), function(nm) {
          item <- items[[nm]]
          s <- utils::capture.output({
            print(item, use_logger = FALSE)
          })

          if (isTRUE(item$valid)) {
            cls <- "hljs-comment"
          } else if (is.na(item$valid)) {
            cls <- "hljs-literal"
          } else {
            if (identical(item$severity, "minor")) {
              cls <- "hljs-literal"
            } else {
              cls <- "hljs-keyword"
            }
          }
          shiny::tags$code(class = cls, paste(s, collapse = "\n"))
        })
        re <- c(re, re0)
      }
    }
    re <- shiny::pre(
      class = "pre-compact bg-gray-90",
      re
    )
    re
  })

  export_validator <- local({
    sv <- shinyvalidate::InputValidator$new(session = session)
    sv$add_rule("export_type", function(value) {
      if (!length(value)) { return() }
      subject <- component_container$data$subject
      if (inherits(subject, "RAVESubject")) {
        switch(
          value,
          "power" = {
            if (!any(subject$preprocess_settings$has_wavelet)) {
              return("Please make sure that Wavelet has been applied")
            }
          },
          "voltage" = {
            if (!any(subject$preprocess_settings$notch_filtered)) {
              return("Please make sure the Notch filters have been applied")
            }
          }
        )
      }
      return()
    })
    sv$add_rule("export_epoch", function(value) {
      subject <- component_container$data$subject
      if (inherits(subject, "RAVESubject")) {
        if (!isTRUE(value %in% subject$epoch_names)) {
          return("Please choose a valid epoch")
        }
      }
      return()
    })
    sv$add_rule("export_pre", function(value) {
      if (!isTRUE(value < 0)) {
        return("Please choose a negative number")
      }
      return()
    })
    sv$add_rule("export_post", function(value) {
      if (!isTRUE(value > 0)) {
        return("Please choose a positive number")
      }
      return()
    })
    sv$add_rule("export_reference", function(value) {
      if (identical(input$export_type, "raw-voltage")) {
        return()
      }
      subject <- component_container$data$subject
      if (inherits(subject, "RAVESubject")) {
        if (!isTRUE(value %in% subject$reference_names)) {
          return("Please choose a valid reference")
        }
      }
      return()
    })
    sv$add_rule("export_electrode", function(value) {
      value <- trimws(value)
      if (nzchar(value)) {
        value <- dipsaus::parse_svec(value)
        value <- value[value > 0]
        subject <- component_container$data$subject
        if (inherits(subject, "RAVESubject")) {
          if (identical(input$export_type, "raw-voltage") ||
             !length(input$export_reference)) {
            valid_elec <- subject$electrodes
          } else {
            valid_elec <- subject$valid_electrodes(input$export_reference)
          }
          value <- value[value %in% valid_elec]
        }

        if (!length(value)) {
          return("No valid electrode channels chosen")
        }
      }

      return()
    })
    sv$disable()
    sv
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if (!loaded_flag) {
        export_validator$disable()
        return()
      }
      export_validator$enable()

      subject <- component_container$data$subject
      if (!inherits(subject, "RAVESubject")) {
        stop("Subject is invalid. Please validate the subject first")
      }

      export_type <- input$export_type

      switch(
        export_type,
        "raw-voltage" = {
          shiny::updateSelectInput(
            session = session,
            inputId = "export_reference",
            choices = "noref",
            selected = "noref"
          )
        },
        {
          shiny::updateSelectInput(
            session = session,
            inputId = "export_reference",
            choices = subject$reference_names,
            selected = shiny::isolate(input$export_reference) %OF% subject$reference_names
          )
        }
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "export_epoch",
        choices = subject$epoch_names,
        selected = shiny::isolate(input$export_epoch) %OF% subject$epoch_names
      )

    }, error_wrapper = "notification"),
    input$export_type,
    ravedash::watch_data_loaded(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  export_repository <- function(zip = FALSE) {
    loaded_flag <- ravedash::watch_data_loaded()
    if (!loaded_flag) { return() }

    subject <- component_container$data$subject
    if (!inherits(subject, "RAVESubject")) {
      stop("Subject is invalid. Please reload the subject.")
    }

    export_validator$enable()

    if (!export_validator$is_valid()) {
      stop("Please correct the inputs before exporting data")
    }

    export_type <- input$export_type
    export_electrode <- dipsaus::parse_svec(input$export_electrode)
    export_electrode <- export_electrode[export_electrode %in% subject$electrodes]
    if (!length(export_electrode)) {
      export_electrode <- subject$electrodes
    }
    export_epoch <- input$export_epoch
    export_reference <- input$export_reference
    export_window <- c(input$export_pre, input$export_post)

    ravedash::shiny_alert2(
      title = "Exporting repository...",
      text = "Please do not close this alert while exporting data. This pop-up will be closed when data is ready...",
      icon = "info",
      auto_close = FALSE,
      buttons = "Close (will not stop exporting)"
    )

    on.exit({
      Sys.sleep(0.5)
      ravedash::close_alert2()
    }, add = TRUE, after = TRUE)

    path <- ravepipeline::with_rave_parallel({
      repository <- switch(
        export_type,
        "raw-voltage" = {
          ravecore::prepare_subject_raw_voltage_with_epochs(
            subject = subject,
            electrodes = export_electrode,
            epoch_name = export_epoch,
            time_windows = export_window,
            quiet = TRUE
          )
        },
        "voltage" = {
          ravecore::prepare_subject_voltage_with_epochs(
            subject = subject,
            electrodes = export_electrode,
            epoch_name = export_epoch,
            time_windows = export_window,
            reference_name = export_reference,
            quiet = TRUE
          )
        },
        "power" = {
          ravecore::prepare_subject_power_with_epochs(
            subject = subject,
            electrodes = export_electrode,
            epoch_name = export_epoch,
            time_windows = export_window,
            reference_name = export_reference
          )
        },
        {
          stop("Unsupported repository format")
        }
      )

      export_folder <- repository$export_matlab()
      if (zip) {
        wd <- getwd()
        setwd(dirname(export_folder))
        on.exit({ setwd(wd) }, add = TRUE, after = FALSE)
        fname <- basename(export_folder)
        utils::zip(zipfile = sprintf("%s.zip", fname), files = fname)
        setwd(wd)

        path <- normalizePath(file.path(export_folder, sprintf("%s.zip", fname)),
                              winslash = "/")
      } else {
        path <- export_folder
      }

      path
    })

    path
  }

  # Runs when 'Generate exports' is clicked, or through
  # `server_tools$trigger_script("generate_exports")` (e.g. from MCP tools)
  server_tools$set_script(
    name = "generate_exports",
    description = c(
      "Same as clicking 'Generate exports' in the 'Export data' card. ALWAYS",
      "confirm the export settings with the user before running it. Reads",
      "inputs `export_type`, `export_electrode`, `export_reference`,",
      "`export_epoch`, `export_pre`, and `export_post` (check them with",
      "`shiny_input_info`). If one breaks its rule (see the input",
      "descriptions), it fails with 'Please correct the inputs before",
      "exporting data' and writes nothing. Otherwise it cuts the data into",
      "trials around each onset of the epoch, applies the reference, and",
      "writes a new folder, never overwriting one:",
      "`<subject>/rave/exports/rave-repository/export-<yymmddTHHMMSS>`, with",
      "`summary.yaml`, `electrodes.csv`, `reference.csv`,",
      "`with_epochs/epoch.csv`, and one MATLAB file per channel,",
      "`with_epochs/<power|voltage|raw_voltage>/ch<NNNN>.mat`. Large exports",
      "take a while. Returns the folder path. People then see a 'Success!'",
      "alert with the path, which stays until the user closes it."
    ),
    expr = {

      path <- export_repository(zip = FALSE)

      dipsaus::close_alert2()
      Sys.sleep(0.5)
      dipsaus::shiny_alert2(
        title = "Success!",
        text = sprintf("Data has been exported to the following path: \n\n%s", path),
        icon = "success",
        buttons = "Confirm"
      )

      # For agents: the export folder
      path
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      server_tools$trigger_script("generate_exports")
    }, error_wrapper = "notification"),
    input$export_do,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$export_download_do <- shiny::downloadHandler(
    filename = "rave-repository-export.zip",
    contentType = "application/zip",
    content = function(con) {
      ravedash::with_error_alert({
        path <- export_repository(zip = TRUE)
        file.rename(path, con)
      })

      return(con)
    }
  )


}
