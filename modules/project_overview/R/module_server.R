
module_server <- function(input, output, session, ...){

  # Local reactive values
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL,
    pipeline_ready = FALSE
  )

  # Local non-reactive values
  local_data <- dipsaus::fastmap2()
  local_data$config_panel_visible <- FALSE

  server_tools <- get_default_handlers(session = session)

  shiny::bindEvent(
    ravedash::safe_observe({
      if (local_data$config_panel_visible) {
        shidashi::add_class(
          selector = sprintf("#%s", ns("viewer_config_panel")),
          class = "soft-hidden",
          session = session
        )
      } else {
        shidashi::remove_class(
          selector = sprintf("#%s", ns("viewer_config_panel")),
          class = "soft-hidden",
          session = session
        )
      }
      local_data$config_panel_visible <- !local_data$config_panel_visible
      
    }),
    input$output_cardset_config,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ----- Populate subjects & module filter when module body loads -----

  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if (!isTRUE(loaded_flag)) { return() }

      # subject_summary is already built by the loader pipeline$run("subject_summary")
      subject_summary <- tryCatch(
        pipeline$read("subject_summary"),
        error = function(e) NULL
      )
      if (is.null(subject_summary) || !nrow(subject_summary)) { return() }

      subjects <- subject_summary$Subject

      saved_subjects <- pipeline$get_settings("subject_codes")
      selected <- if (length(saved_subjects)) {
        saved_subjects[saved_subjects %in% subjects]
      } else {
        NULL
      }

      shiny::updateSelectInput(
        session = session,
        inputId = "subject_codes",
        choices = subjects,
        selected = selected
      )

      # Scan for available module names from reports across subjects
      project_name <- pipeline$get_settings("project_name", default = "")

      # If project changed, invalidate pipeline cache
      if (!identical(local_data$project_name, project_name)) {
        local_data$project_name <- project_name
        local_reactives$pipeline_ready <- FALSE
      }

      all_modules <- unique(unlist(lapply(subjects, function(sc) {
        subject <- tryCatch(
          ravecore::new_rave_subject(
            project_name = project_name, subject_code = sc, strict = FALSE),
          error = function(e) NULL
        )
        if (is.null(subject)) { return(character(0)) }
        reports <- discover_existing_reports(subject)
        reports$module
      })))

      saved_filter <- pipeline$get_settings("module_filter")
      selected_filter <- if (length(saved_filter)) {
        saved_filter[saved_filter %in% all_modules]
      } else {
        NULL
      }

      shiny::updateSelectInput(
        session = session,
        inputId = "module_filter",
        choices = sort(all_modules),
        selected = selected_filter
      )
    }),
    ravedash::watch_data_loaded(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  # ----- Generate Report button -----

  run_pipeline <- function() {

    subject_codes <- input$subject_codes
    module_filter <- input$module_filter

    # Only these are pipeline settings
    pipeline$set_settings(
      subject_codes = if (length(subject_codes)) subject_codes else NULL,
      template_subject = input$template_subject,
      module_filter = if (length(module_filter)) module_filter else NULL
    )

    # Determine which targets to build based on checked sections
    targets <- c("resolved_subjects")

    # Group-level
    if (isTRUE(input$group_viewer)) {
      targets <- c(targets, "snapshot_group_brain")
    }
    if (isTRUE(input$electrode_coverage)) {
      targets <- c(targets, "snapshot_group_electrode_coverage")
    }
    if (isTRUE(input$subjects_metadata)) {
      targets <- c(targets, "snapshot_group_subject_summary")
    }
    if (isTRUE(input$epoch_references)) {
      targets <- c(targets, "snapshot_subject_meta_summary")
    }

    # Subject-level
    if (isTRUE(input$module_reports)) {
      targets <- c(targets, "snapshot_subject_module_reports")
    }
    if (isTRUE(input$native_viewer)) {
      targets <- c(targets, "snapshot_subject_3d_viewers")
    }
    if (isTRUE(input$validation)) {
      targets <- c(targets, "snapshot_subject_validation")
    }

    dipsaus::shiny_alert2(
      title = "Generating report...",
      text = ravedash::be_patient_text(),
      icon = "info",
      session = session, 
      buttons = FALSE
    )

    on.exit({
      Sys.sleep(0.5)
      dipsaus::close_alert2()
    })

    pipeline$run(
      as_promise = FALSE,
      names = targets,
      return_values = FALSE
    )

    local_reactives$pipeline_ready <- Sys.time()
    ravepipeline::logger("Pipeline targets built successfully")
    # Update viewer selector choices
    resolved <- tryCatch(pipeline$read("resolved_subjects"), error = function(e) NULL)
    viewer_choices <- c("Group Brain", resolved)
    selected_viewer <- shiny::isolate(input$brain_viewer_selector)
    if (!length(selected_viewer) || !selected_viewer %in% viewer_choices) {
      selected_viewer <- "Group Brain"
    }
    shiny::updateSelectInput(
      session = session,
      inputId = "brain_viewer_selector",
      choices = viewer_choices,
      selected = selected_viewer
    )

  }

  shiny::bindEvent(
    ravedash::safe_observe({

      run_pipeline()

    }, error_wrapper = "alert"),
    input$generate_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ----- Helper: read pipeline target safely -----

  read_target <- function(target_name) {
    tryCatch(pipeline$read(target_name), error = function(e) NULL)
  }


  # ===== OUTPUT CARDSET =====

  # --- Subject Summary table (group-level) ---
  shidashi::register_output(
  DT::renderDataTable({
    pipeline_ready <- local_reactives$pipeline_ready
    shiny::validate(shiny::need(!isFALSE(pipeline_ready), "Click 'Generate Report' to build and view results."))

    summary_tbl <- read_target("snapshot_group_subject_summary")

    if (!is.data.frame(summary_tbl) || !nrow(summary_tbl)) {
      return(DT::datatable(
        data.frame(Message = "No summary data. Build target first."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    DT::datatable(
      summary_tbl,
      escape = FALSE,
      selection = "none",
      class = "compact stripe",
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 50,
        scrollX = TRUE
      )
    )
  }),
  outputId = "subjects_summary_table",
  download_type = "no-download",
  description = "Summary table of all subjects in the project"
  )

  # --- Module Reports table (merged across subjects) ---
  shidashi::register_output(
  DT::renderDataTable({
    pipeline_ready <- local_reactives$pipeline_ready
    shiny::validate(shiny::need(!isFALSE(pipeline_ready), "Click 'Generate Report' to build and view results."))
    latest_only <- isTRUE(input$reports_latest_only)

    reports <- read_target("snapshot_subject_module_reports")

    if (is.null(reports) || !length(reports)) {
      return(DT::datatable(
        data.frame(Message = "No reports found. Build 'Module Reports' target first."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    project_name <- pipeline$get_settings("project_name", default = "")

    rows <- lapply(names(reports), function(sc) {
      manifest <- reports[[sc]]
      if (!is.data.frame(manifest) || !nrow(manifest)) { return(NULL) }

      manifest$Link <- vapply(seq_len(nrow(manifest)), function(i) {
        url <- sprintf(
          "?type=widget&module=standalone_report&project_name=%s&subject_code=%s&report_filename=%s",
          project_name, sc, manifest$dir_name[[i]]
        )
        sprintf('<a href="%s" target="_blank">Open</a>', url)
      }, "")

      # Parse timestamp: YYMMDDTHHMMSS -> YYYY-MM-DD HH:MM:SS
      ts_parsed <- vapply(manifest$timestamp, function(ts) {
        tryCatch({
          fmt <- as.POSIXct(ts, format = "%y%m%dT%H%M%S", tz = "UTC")
          format(fmt, "%Y-%m-%d %H:%M:%S")
        }, error = function(e) ts)
      }, "")

      data.frame(
        Subject = sc,
        Module = manifest$module,
        Report = manifest$report_name,
        Created = ts_parsed,
        View = manifest$Link,
        .sort_ts = manifest$timestamp,
        stringsAsFactors = FALSE
      )
    })
    combined <- do.call(rbind, rows)

    if (is.null(combined) || !nrow(combined)) {
      return(DT::datatable(
        data.frame(Message = "No reports found across subjects."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    # Sort descending by timestamp
    combined <- combined[order(combined$.sort_ts, decreasing = TRUE), , drop = FALSE]

    # Keep only most recent per Subject + Module + Report
    if (latest_only) {
      combined <- combined[!duplicated(combined[, c("Subject", "Module", "Report")]), , drop = FALSE]
    }

    combined$.sort_ts <- NULL
    rownames(combined) <- NULL
    names(combined)[names(combined) == "Created"] <- "Created (YYYY-mm-dd)"

    DT::datatable(
      combined,
      escape = FALSE,
      selection = "none",
      class = "compact stripe",
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  }),
  outputId = "module_reports_table",
  download_type = "no-download",
  description = "Table of all module reports across subjects with links to open them"
  )

  # --- Electrode Coverage table (group-level, sorted by Total desc) ---
  shidashi::register_output(
  DT::renderDataTable({
    pipeline_ready <- local_reactives$pipeline_ready
    shiny::validate(shiny::need(!isFALSE(pipeline_ready), "Click 'Generate Report' to build and view results."))

    coverage <- read_target("snapshot_group_electrode_coverage")

    if (!is.data.frame(coverage) || !nrow(coverage)) {
      return(DT::datatable(
        data.frame(Message = "No electrode coverage data. Build target first."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    # Find the Total column index (0-based) for default sorting
    total_col_idx <- match("Total", names(coverage)) - 1L
    sort_order <- if (!is.na(total_col_idx)) {
      list(list(total_col_idx, "desc"))
    } else {
      list()
    }

    DT::datatable(
      coverage,
      selection = "none",
      class = "compact stripe",
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = sort_order
      )
    )
  }),
  outputId = "electrode_coverage_table",
  download_type = "no-download",
  description = "Electrode coverage counts per brain region across all subjects"
  )

  # --- 3D Viewer (Group brain or per-subject native brain) ---
  shidashi::register_output(
  threeBrain::renderBrain({
    pipeline_ready <- local_reactives$pipeline_ready
    shiny::validate(shiny::need(!isFALSE(pipeline_ready), "Click 'Generate Report' to build and view results."))
    selector <- input$brain_viewer_selector
    if (!length(selector)) { return(NULL) }

    if (selector == "Group Brain") {
      brain <- read_target("snapshot_group_brain")
    } else {
      viewers <- read_target("snapshot_subject_3d_viewers")
      brain <- if (!is.null(viewers)) viewers[[selector]] else NULL
    }
    if (is.null(brain)) { return(NULL) }
    brain$plot()
  }),
  outputId = "brain_widget",
  download_type = "threeBrain",
  description = "Interactive 3D brain viewer showing group brain or a selected subject's native brain"
  )

  # --- Epoch & References table (merged across subjects) ---
  shidashi::register_output(
  DT::renderDataTable({
    pipeline_ready <- local_reactives$pipeline_ready
    shiny::validate(shiny::need(!isFALSE(pipeline_ready), "Click 'Generate Report' to build and view results."))

    meta_summary <- read_target("snapshot_subject_meta_summary")

    if (is.null(meta_summary) || !length(meta_summary)) {
      return(DT::datatable(
        data.frame(Message = "No epoch/reference data. Build target first."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    rows <- lapply(names(meta_summary), function(sc) {
      tbl <- meta_summary[[sc]]
      if (!is.data.frame(tbl) || !nrow(tbl)) { return(NULL) }
      tbl$Subject <- sc
      tbl
    })
    combined <- do.call(rbind, rows)

    if (is.null(combined) || !nrow(combined)) {
      return(DT::datatable(
        data.frame(Message = "No epoch/reference data found."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    col_order <- c("Subject", setdiff(names(combined), "Subject"))
    combined <- combined[, col_order, drop = FALSE]
    rownames(combined) <- NULL

    DT::datatable(
      combined,
      selection = "none",
      class = "compact stripe",
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  }),
  outputId = "epoch_reference_table",
  download_type = "no-download",
  description = "Epoch and reference file summary across all subjects"
  )

  # --- Validation table (merged across subjects) ---
  shidashi::register_output(
  DT::renderDataTable({
    pipeline_ready <- local_reactives$pipeline_ready
    shiny::validate(shiny::need(!isFALSE(pipeline_ready), "Click 'Generate Report' to build and view results."))

    validations <- read_target("snapshot_subject_validation")

    if (is.null(validations) || !length(validations)) {
      return(DT::datatable(
        data.frame(Message = "No validation data. Build 'Validation' target first."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    rows <- lapply(names(validations), function(sc) {
      tbl <- validations[[sc]]
      if (!is.data.frame(tbl) || !nrow(tbl)) { return(NULL) }
      tbl$Subject <- sc
      tbl
    })
    combined <- do.call(rbind, rows)

    if (is.null(combined) || !nrow(combined)) {
      return(DT::datatable(
        data.frame(Message = "No validation data found."),
        selection = "none", class = "compact stripe",
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    col_order <- c("Subject", setdiff(names(combined), "Subject"))
    combined <- combined[, col_order, drop = FALSE]
    rownames(combined) <- NULL

    DT::datatable(
      combined,
      selection = "none",
      class = "compact stripe",
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 50,
        scrollX = TRUE
      )
    )
  }),
  outputId = "validation_table",
  download_type = "no-download",
  description = "Per-subject data validation results"
  )


  # ===== EXPORT =====

  shiny::bindEvent(
    ravedash::safe_observe({

      ravedash::show_notification(
        message = "Building HTML report for export...",
        title = "Exporting",
        type = "info",
        autohide = FALSE,
        session = session,
        class = ns("export_report")
      )

      # Make sure the report is up-to-date before exporting
      run_pipeline()

      ignore_targets <- c(
        if (!isTRUE(input$subjects_metadata))  "snapshot_group_subject_summary",
        if (!isTRUE(input$module_reports))      "snapshot_subject_module_reports",
        if (!isTRUE(input$electrode_coverage))  "snapshot_group_electrode_coverage",
        if (!isTRUE(input$group_viewer))        "snapshot_group_brain",
        if (!isTRUE(input$native_viewer))       "snapshot_subject_3d_viewers",
        if (!isTRUE(input$epoch_references))    "snapshot_subject_meta_summary",
        if (!isTRUE(input$validation))          "snapshot_subject_validation"
      )

      local_data$export_tmp_dir <- tempfile(pattern = "projectSnapshot_export_")

      job_id <- pipeline$generate_report(
        "projectSnapshot",
        params = list(ignore_targets = ignore_targets),
        output_format = "html_document",
        output_dir = local_data$export_tmp_dir,
        callback = function(output_dir) {
          # Zip the report directory; return path for onFulfilled
          zip_path <- paste0(output_dir, ".zip")
          if (file.exists(zip_path)) unlink(zip_path)

          cwd <- getwd()
          on.exit(setwd(cwd))
          setwd(dirname(output_dir))
          utils::zip(
            zipfile = zip_path,
            files = basename(output_dir)
          )

          return(zip_path)
        }
      )

      job_promise <- ravepipeline::as.promise(job_id)

      promises::then(
        job_promise,
        onFulfilled = function(report_path) {

          zip_path <- attr(report_path, "callback_result")

          ravedash::clear_notifications(class = ns("export_report"), session = session)

          local_data$export_zip_path <- zip_path

          shiny::showModal(
            session = session,
            shiny::modalDialog(
              title = "Download packaged report",
              easyClose = FALSE,
              size = "m",
              footer = shiny::tagList(
                shiny::modalButton("Close"),
                shiny::downloadButton(
                  outputId = ns("download_btn"),
                  label = "Download",
                  class = "btn-primary",
                  icon = ravedash::shiny_icons$download
                )
              ),
              shiny::p("The HTML report has been built and packaged.")
            )
          )
        },
        onRejected = function(e) {
          ravedash::clear_notifications(class = ns("export_report"), session = session)
          ravedash::error_notification(e, session = session)
        }
      )

      return()
    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$download_btn <- shiny::downloadHandler(
    filename = function() {
      project_name <- pipeline$get_settings("project_name", default = "report")
      sprintf("%s_overview.zip", project_name)
    },
    contentType = "application/zip",
    content = function(con) {
      zip_path <- local_data$export_zip_path
      if (length(zip_path) && file.exists(zip_path)) {
        file.copy(zip_path, con)
      }
      shiny::removeModal(session = session)
      if (
        length(local_data$export_tmp_dir) == 1 &&
          file.exists(local_data$export_tmp_dir)
      ) {
        unlink(local_data$export_tmp_dir, recursive = TRUE)
      }
      return(con)
    }
  )

}
