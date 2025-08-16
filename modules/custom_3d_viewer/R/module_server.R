
module_server <- function(input, output, session, ...){



  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_inputs = NULL,
    update_outputs = NULL,
    viewer_selection = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- ravedash::get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  # server_tools$run_analysis_onchange(
  #   component_container$get_input_ids(c(
  #     "electrode_text", "baseline_choices",
  #     "analysis_ranges", "condition_groups"
  #   ))
  # )

  # Register event: main pipeline need to run

  regenerate_viewer <- function() {
    ravedash::with_error_notification({

      shidashi::clear_notifications()
      dipsaus::shiny_alert2(
        title = "Updating 3D Viewer",
        text = "Loading data...",
        auto_close = FALSE,
        buttons = FALSE,
        icon = "info"
      )
      on.exit({
        dipsaus::close_alert2()
      }, add = TRUE, after = FALSE)
      Sys.sleep(0.5)

      results <- pipeline$run(
        as_promise = FALSE,
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL,
        names = c(
          "path_datatable",
          "brain_widget"
        )
      )

      data_table <- NULL
      if(
        length(results$path_datatable) == 1 &&
        file.exists(results$path_datatable) &&
        grepl("\\.fst$", results$path_datatable, ignore.case = TRUE)
      ) {
        data_table <- fst::fst(results$path_datatable)
      }
      local_reactives$data_table <- data_table
      local_reactives$brain_widget <- results$brain_widget
      local_reactives$update_inputs <- Sys.time()
      local_reactives$update_outputs <- Sys.time()
      local_reactives$viewer_selection <- NULL

    })
  }

  shiny::bindEvent(
    ravedash::safe_observe({

      data_source <- input$data_source
      settings <- list()
      if(identical(data_source, "Uploads")) {
        settings <- list(
          uploaded_source = input$uploaded_source
        )
      } else if(identical(data_source, "Saved pipelines/modules")) {
        settings <- list(
          data_source_project = input$data_source_project,
          data_source_pipeline = input$data_source_pipeline,
          data_source_pipeline_target = input$data_source_pipeline_target
        )
      }
      pipeline$set_settings(
        data_source = input$data_source,
        controllers = list(),
        main_camera = list(),
        shiny_outputId = ns("viewer_ready"),
        .list = settings
      )
      regenerate_viewer()
    }),
    input$viewer_reset,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      synced_controllers <- c(
        "Display Coordinates", "Show Panels", "Slice Brightness", "Slice Mode",
        "Coronal (P - A)", "Axial (I - S)", "Sagittal (L - R)",
        "Overlay Coronal", "Overlay Axial", "Overlay Sagittal",
        "Frustum Near", "Frustum Far",

        # "Voxel Display", "Voxel Opacity",
        "Voxel Min", "Voxel Max", "Voxel Label",

        "Surface Material", "Surface Type", "Clipping Plane",
        "Left Hemisphere", "Right Hemisphere",
        "Left Opacity", "Right Opacity",
        "Left Mesh Clipping", "Right Mesh Clipping",
        "Surface Color", "Blend Factor", "Sigma", "Decay", "Range Limit",

        "Surface Mapping", "Volume Mapping", "Visibility",
        "Electrode Shape", "Outlines", "Text Scale", "Text Visibility",

        "Display Data", "Display Range",
        "Threshold Data", "Threshold Range", "Threshold Method",
        "Additional Data",

        "Show Legend", "Show Time", "Highlight Box", "Info Text", "Time"
      )

      controllers <- pipeline$get_settings("controllers", default = list())

      proxy_controllers <- as.list(proxy$controllers)
      background <- proxy_controllers[["Background Color"]]
      if(length(background) != 1) {
        theme <- shiny::isolate(ravedash::current_shiny_theme())
        background <- theme$background
      }
      controllers[["Background Color"]] <- dipsaus::col2hexStr(background)

      # voxel_type <- proxy_controllers[["Voxel Display"]]
      # if(isTRUE(voxel_type %in% brain$atlas_types)) {
      #   controllers[["Voxel Display"]] <- voxel_type
      #   controllers[["Voxel Opacity"]] <- proxy_controllers[["Voxel Opacity"]]
      # }

      proxy_controllers <- proxy_controllers[
        names(proxy_controllers) %in% synced_controllers]

      for(nm in names(proxy_controllers)) {
        controllers[[nm]] <- proxy_controllers[[nm]]
      }

      main_camera <- pipeline$get_settings("main_camera", default = list())
      proxy_main_camera <- proxy$main_camera
      if(all(c("position", "zoom", "up") %in% names(proxy_main_camera))) {
        main_camera <- proxy_main_camera
      }

      data_source <- input$data_source
      settings <- list()
      if(identical(data_source, "Uploads")) {
        settings <- list(
          uploaded_source = input$uploaded_source
        )
      } else if(identical(data_source, "Saved pipelines/modules")) {
        settings <- list(
          data_source_project = input$data_source_project,
          data_source_pipeline = input$data_source_pipeline,
          data_source_pipeline_target = input$data_source_pipeline_target
        )
      }

      pipeline$set_settings(
        data_source = input$data_source,
        controllers = controllers,
        main_camera = main_camera,
        shiny_outputId = ns("viewer_ready"),
        .list = settings
      )

      regenerate_viewer()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }



      loaded_data <- pipeline$read("loaded_brain")

      if(!is.list(loaded_data)) {
        ravedash::logger("Data read from the pipeline, but it is not a list. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Data read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_data <- component_container$data$loaded_brain
      if(is.list(old_data)){

        if(
          identical(old_data$project_name, loaded_data$project_name) &&
          identical(old_data$subject_code, loaded_data$subject_code) &&
          identical(old_data$electrode_table, loaded_data$electrode_table) &&
          setequal(old_data$surface_types, loaded_data$surface_types)
        ) {
          ravedash::logger("The loaded data remain unchanged, skip initialization", level = "debug", use_glue = TRUE)
          return()
        }

      }


      # TODO: reset UIs to default
      shidashi::clear_notifications()
      shiny::removeModal()

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$loaded_brain <- loaded_data
      local_reactives$brain_widget <- pipeline$read("initial_brain_widget")
      component_container$initialize_with_new_data()

      # TODO Reset outputs

      local_reactives$update_outputs <- Sys.time()
      local_reactives$viewer_selection <- NULL


    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  update_upload_source <- function(selected = NULL) {
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }

    loaded_brain <- component_container$data$loaded_brain
    subject <- get_brain_subject(loaded_brain)
    if(is.null(subject)) { return() }

    root_path <- get_subject_imaging_datapath(
      subject_code = subject$subject_code,
      project_name = subject$project_name,
      type = "uploads"
    )

    candidates <- NULL
    if(dir.exists(root_path)) {
      candidates <- list.files(
        path = root_path, pattern = "\\.(csv|fst)$", ignore.case = TRUE,
        full.names = FALSE, all.files = FALSE, recursive = TRUE,
        include.dirs = FALSE)
      tryCatch({
        if(length(candidates)) {
          o <- order(file.mtime(file.path(root_path, candidates)),
                     decreasing = TRUE)
          candidates <- candidates[o]
        }
      }, error = function(e){})
    }

    selected <- c(
      selected,
      shiny::isolate(input$uploaded_source),
      pipeline$get_settings("uploaded_source")
    ) %OF% c(candidates, "[New Uploads]")

    pipeline$set_settings(
      data_source = "Uploads",
      uploaded_source = selected
    )

    shiny::updateSelectInput(
      session = session,
      inputId = "uploaded_source",
      choices = c("[New Uploads]", candidates),
      selected = selected
    )

  }

  shiny::bindEvent(
    ravedash::safe_observe({

      loaded_brain <- component_container$data$loaded_brain
      if(length(loaded_brain$subject_code) != 1) {
        return()
      }

      subject <- get_brain_subject(loaded_brain)
      if(is.null(subject)) { return() }


      info <- input$uploaded_file
      if(!grepl("\\.(csv|fst|xls[x]{0,1})$", info$name, ignore.case = TRUE)) {
        ravedash::error_notification(list(message = "Unsupported file format: only [.csv] and [.fst] files are supported."))
        return()
      }

      format <- "csv"
      if( grepl("\\.fst$", info$name, ignore.case = TRUE) ) {
        format <- "fst"
      } else if ( grepl("\\.xls[x]{0,1}$", info$name, ignore.case = TRUE) ) {
        format <- "xlsx"
      }

      res <- ravedash::with_error_notification({
        header <- switch(
          format,
          "csv" = {
            utils::read.csv(info$datapath, header = TRUE)
          },
          "fst" = {
            ieegio::io_read_fst(info$datapath, method = "proxy")
          },
          "xlsx" = {
            read_xlsx(info$datapath)
          }
        )
        if(!"Electrode" %in% names(header)) {
          stop('The uploaded data file must contain named headers, and one of the headers must be "Electrode".')
        }

        # save to somewhere?
        root_path <- get_subject_imaging_datapath(
          subject_code = subject$subject_code,
          project_name = subject$project_name,
          type = "uploads"
        )

        ravepipeline::dir_create2(root_path)
        new_name <- gsub("\\.(csv|xls[x]{0,1})$", ".fst", info$name)
        fpath <- file.path(root_path, new_name)

        if(format != "fst") {
          ieegio::io_write_fst(header, fpath)
        } else {
          file.copy(from = info$datapath, to = fpath, overwrite = TRUE)
        }
        update_upload_source(selected = new_name)

        ravedash::show_notification(
          title = "Upload done!",
          type = "success",
          message = shiny::p(
            'Please make sure you click on "Re-generate the viewer" link or ',
            shiny::actionLink(
              inputId = ns("run_analysis_from_notification"),
              label = "here",
              icon = ravedash::shiny_icons$refresh,
              style = "color: #f0f0f0"
            ),
            " to update viewer."
          ),
          autohide = FALSE, close = TRUE,
          icon = ravedash::shiny_icons$upload
        )
      })

    }),
    input$uploaded_file,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      regenerate_viewer()
    }),
    input$run_analysis_from_notification,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      selected <- shiny::isolate(input$download_template_type) %OF% TEMPLATE_CHOICES

      shiny::showModal(shiny::modalDialog(
        title = "Example: electrode value table",
        size = "l",
        easyClose = FALSE,
        footer = shiny::modalButton("Dismiss"),
        shiny::fluidRow(
          shiny::column(
            width = 12L,
            shiny::selectInput(
              inputId = ns("download_template_type"),
              label = shiny::tagList(
                shiny::span("Select a template type "),
                shiny::downloadLink(
                  outputId = ns("download_template_do"),
                  label = "(download this template)",
                )
              ),
              choices = TEMPLATE_CHOICES,
              selected = selected
            )
          ),
          shiny::column(
            width = 12L,
            DT::DTOutput(outputId = ns("download_template_example_output"))
          )
        )
      ))
    }),
    input$download_template_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  template_example <- shiny::reactive({
    tryCatch({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      if(!isTRUE(input$download_template_type %in% TEMPLATE_CHOICES)) {
        return()
      }

      # loaded_data <- pipeline$read("loaded_brain")
      loaded_data <- component_container$data$loaded_brain
      subject_code <- loaded_data$subject_code
      electrode_table <- loaded_data$electrode_table
      if(!is.data.frame(electrode_table)) { return(NULL) }
      if(length(subject_code) != 1) {
        subject_code <- "N27"
      }

      electrodes <- electrode_table$Electrode
      nelec <- length(electrodes)
      if(nelec == 0) { return() }
      tbl <- switch(
        input$download_template_type,
        "Simple property" = {
          data.frame(
            Subject = subject_code,
            Electrode = electrodes,
            tValue = round(rnorm(nelec), 2)
          )
        },
        "Multiple properties" = {
          re <- data.frame(
            Subject = subject_code,
            Electrode = electrodes,
            MyDiscreteValue = sample(LETTERS, size = nelec, replace = TRUE),
            MyContinuousValue = round(rnorm(nelec), 2)
          )
          re$MyContinuousValue[sample(nelec, min(5, max(nelec - 4, 1)))] <- NA
          re
        },
        "Animation" = {
          data.frame(
            Subject = subject_code,
            Electrode = rep(electrodes, each = 3),
            Time = rep(c(0.1, 0.2, 0.5), nelec),
            Amplitude = round(rnorm(nelec * 3), 2)
          )
        },
        { NULL }
      )
    }, error = function(e) { NULL })
  })

  output$download_template_do <- shiny::downloadHandler(
    filename = "electrode_value.csv",
    content = function(con) {
      template <- template_example()
      if(!is.data.frame(template)) {
        stop("No template is available. Electrode table is not detected.")
      }
      utils::write.csv(x = template, file = con, row.names = FALSE)
    }
  )

  output$download_template_example_output <- DT::renderDT({
    template <- template_example()
    shiny::validate(
      shiny::need(
        is.data.frame(template),
        message = "No example is available. Electrode table is not detected..."
      )
    )
    caption <- switch(
      input$download_template_type,
      "Simple property" = "Example with a single value property: column names must contain 'Electrode' (electrode channel) and the variable name.",
      "Multiple properties" = {
        template$MyContinuousValue <- sprintf("%.2f", template$MyContinuousValue)
        "Example with continuous and discrete variables. Use NA to represent missing values. Column names must contain 'Electrode' (electrode channel) and the variable names."
      },
      "Animation" = "Example for animation. Column names must contain 'Electrode' (electrode channel), Time (time in seconds), and at least one variable name.",
      { NULL }
    )

    DT::datatable(
      template,
      class = "stripe compact",
      selection = "none",
      filter = "none",
      caption = caption,
      options = list(
        columnDefs = list(
          list(className = 'dt-right', targets = "_all")
        )
      )
    )
  })

  update_pipeline_source <- function(selected = NULL) {
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }

    loaded_brain <- component_container$data$loaded_brain
    if(!length(loaded_brain$subject_code) == 1) { return() }

    subject_code <- loaded_brain$subject_code
    candidates <- get_projects_with_scode(subject_code = subject_code, refresh = TRUE)

    selected <- c(
      selected,
      shiny::isolate(input$data_source_project),
      pipeline$get_settings("data_source_project")
    ) %OF% candidates

    pipeline$set_settings(
      data_source = "Saved pipelines/modules",
      data_source_project = selected
    )

    shiny::updateSelectInput(
      session = session,
      inputId = "data_source_project",
      choices = candidates,
      selected = selected
    )

    local_reactives$data_source_pipeline_needsupdate <- Sys.time()
  }

  shiny::bindEvent(
    ravedash::safe_observe({

      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      loaded_brain <- component_container$data$loaded_brain
      if(length(loaded_brain$subject_code) != 1) {
        return()
      }

      project_name <- input$data_source_project
      if(length(project_name) != 1 || project_name %in% c("", ".", "/")) { return() }
      subject_code <- loaded_brain$subject_code

      root_path <- get_subject_imaging_datapath(
        subject_code = loaded_brain$subject_code,
        project_name = project_name,
        type = "pipeline"
      )

      avaialble_pipelines <- list.dirs(root_path, full.names = FALSE, recursive = FALSE)
      avaialble_pipelines <- unlist(lapply(avaialble_pipelines, function(pn) {
        re <- list.dirs(file.path(root_path, pn), full.names = FALSE, recursive = FALSE)
        if(length(re)) {
          re <- sprintf("%s/%s", pn, re)
        }
        re
      }))
      candidates <- as.character(avaialble_pipelines)
      if(!length(candidates)) {
        label <- "(No saved pipeline found)"
      } else {
        label <- "Select a saved pipeline"
      }

      selected <- c(
        shiny::isolate(input$data_source_pipeline),
        pipeline$get_settings("data_source_pipeline")
      ) %OF% candidates

      shiny::updateSelectInput(
        session = session,
        inputId = "data_source_pipeline",
        label = label,
        choices = candidates,
        selected = selected
      )
      local_reactives$data_source_pipeline_target_needsupdate <- Sys.time()

    }),
    input$data_source_project,
    local_reactives$data_source_pipeline_needsupdate,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      loaded_brain <- component_container$data$loaded_brain
      if(length(loaded_brain$subject_code) != 1) {
        return()
      }

      project_name <- input$data_source_project
      if(length(project_name) != 1 || project_name %in% c("", "/", ".")) { return() }

      pipepath <- input$data_source_pipeline
      if(length(pipepath) != 1 || pipepath %in% c(".", "")) { return() }

      subject_code <- loaded_brain$subject_code

      pipepath <- get_subject_imaging_datapath(
        pipepath,
        subject_code = loaded_brain$subject_code,
        project_name = project_name,
        type = "pipeline"
      )

      if(length(pipepath) != 1 || is.na(pipepath) || !dir.exists(pipepath)) {
        return()
      }

      tryCatch({
        candidates <- unname(ravepipeline::pipeline_target_names(pipepath))
        selected <- c(
          shiny::isolate(input$data_source_pipeline_target),
          pipeline$get_settings("data_source_pipeline_target")
        ) %OF% candidates

        shiny::updateSelectInput(
          session = session,
          inputId = "data_source_pipeline_target",
          choices = candidates,
          selected = selected
        )
      }, error = function(e) {
        ravedash::error_notification(e)
        shiny::updateSelectInput(
          session = session,
          inputId = "data_source_pipeline_target",
          choices = character()
        )
      })

    }, priority = -1),
    input$data_source_pipeline,
    local_reactives$data_source_pipeline_target_needsupdate,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      # loaded_brain <- pipeline$read("loaded_brain")
      loaded_brain <- component_container$data$loaded_brain


      if( identical(input$data_source, "Uploads") ) {
        update_upload_source()
      } else if( identical(input$data_source, "Saved pipelines/modules") ) {
        # get_projects_with_scode()
        update_pipeline_source()
      }
    }),
    input$data_source,
    local_reactives$update_inputs,
    ravedash::watch_data_loaded(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )




  # Register outputs
  ravedash::register_output(
    outputId = "viewer",
    render_function = threeBrain::renderBrain({
      shiny::validate(
        shiny::need(
          length(local_reactives$update_outputs) &&
            !isFALSE(local_reactives$update_outputs),
          message = "Please run the module first"
        )
      )

      local_reactives$brain_widget
    }),
    output_type = "threeBrain"
  )


  # Brain proxy
  proxy <- threeBrain::brain_proxy("viewer")

  # set background
  shiny::bindEvent(
    ravedash::safe_observe({

      theme <- ravedash::current_shiny_theme()
      proxy$set_background(col = theme$background)
    }),
    ravedash::current_shiny_theme()
  )

  get_proxy_data <- shiny::throttle(shiny::reactive({

    list(
      main_camera = proxy$main_camera,
      surface_type = proxy$surface_type,
      display_variable = proxy$display_variable,
      plane_position = proxy$plane_position,
      controllers = proxy$controllers
    )
  }), millis = 1000)

  output$viewer_status <- shiny::renderUI({


    proxy_data <- get_proxy_data()
    main_camera <- proxy_data$main_camera
    viewer_selection <- local_reactives$viewer_selection
    controllers <- proxy_data$controllers


    mni305 <- controllers[["Intersect MNI305"]]
    if(length(mni305) == 1) {
      mni152 <- suppressWarnings({
        mni305 <- as.numeric(strsplit(mni305, ",")[[1]])
        mni305 <- mni305[!is.na(mni305)]
        if(length(mni305) != 3) {
          mni152 <- ""
        } else {
          mni152 <- ravecore::MNI305_to_MNI152 %*% c(mni305, 1)
          mni152 <- round(mni152[1:3])
          mni152 <- shiny::span(
            shiny::a(
              sprintf("MNI152: %s", paste(sprintf("%.0f", mni152), collapse = ",")),
              target = "_blank",
              href = url_neurosynth(mni152[1], mni152[2], mni152[3])
            )
          )
        }
        mni152
      })
    } else {
      mni152 <- ""
    }

    # name = current_clip,
    # subject = subject,
    # electrode = electrode_number,
    # data = selected_data
    selection_info <- NULL
    if(is.list(viewer_selection)) {

      value <- viewer_selection$data$value
      value_info <- "No value selected"
      if(length(value)) {
        if(is.numeric(value)) {
          if(length(value) == 1) {
            value_info <- sprintf("%.4g", value)
          } else {
            value_info <- shiny::tagList(
              sprintf("%d numerical values in total.", length(value)), shiny::br(),
              shiny::tags$small("(Please double click this text to view the plots)",
                                class = "text-muted")
            )
          }
        } else {
          value <- unique(value)
          if(length(value) == 1) {
            value_info <- as.character(value)
          } else {
            value_info <- shiny::tagList(
              sprintf("%d unique factors.", length(unique(value))), shiny::br(),
              shiny::tags$small("(Please double click this text to view the plots)",
                                class = "text-muted")
            )
          }
        }

      }

      electrode_mni <- unlist(viewer_selection$raw$object$MNI305_position)
      electrode_mni <- as.numeric(electrode_mni)
      electrode_mni <- electrode_mni[!is.na(electrode_mni)]
      if( length(electrode_mni) == 3 ) {
        electrode_mni <- ravecore::MNI305_to_MNI152 %*% c(electrode_mni, 1)
        electrode_mni <- shiny::tags$small(
          shiny::a(
            sprintf("[MNI152: %s]", paste(sprintf("%.0f", electrode_mni[1:3]),
                                         collapse = ",")),
            target = "_blank",
            href = url_neurosynth(
              electrode_mni[[1]], electrode_mni[[2]],
              electrode_mni[[3]])
          )
        )
      } else {
        electrode_mni <- NULL
      }

      selection_info <- shiny::tagList(
        shiny::tags$dt(class = "col-sm-12", shiny::hr()),
        # shiny::tags$dd(class = "col-sm-7"),

        shiny::tags$dt(class = "col-sm-4", "Electrode: "),
        shiny::tags$dd(class = "col-sm-8 code", viewer_selection$electrode, electrode_mni),

        shiny::tags$dt(class = "col-sm-4", "Data selected: "),
        shiny::tags$dd(class = "col-sm-8 code", viewer_selection$name),

        shiny::tags$dt(class = "col-sm-4", "Value: "),
        shiny::tags$dd(class = "col-sm-8 code", value_info)
      )
    }

    shiny::tags$dl(
      class = "row",
      shiny::tags$dt(class = "col-sm-4", "Surface Type: "),
      shiny::tags$dd(class = "col-sm-8 code", paste(proxy_data$surface_type, collapse = "")),

      shiny::tags$dt(class = "col-sm-4", "Anat. Clip Plane: "),
      shiny::tags$dd(class = "col-sm-8 code", mni152),

      shiny::tags$dt(class = "col-sm-4", "Camera Zoom: "),
      shiny::tags$dd(class = "col-sm-8 code", paste(sprintf("%.2f", unlist(main_camera$zoom)), collapse = ", ")),
      selection_info
    )

  })

  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::flip(inputId = "flip_viewer_wrapper")
    }),
    input$flip_viewer_status,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      info <- as.list(proxy$mouse_event_double_click)
      if(!isTRUE(info$is_electrode)) {
        return()
      }
      current_clip <- info$current_clip
      current_time <- info$current_time
      time_range <- unlist(input$time_range)
      subject <- info$subject
      electrode_number <- as.integer(info$electrode_number)

      selected_data <- NULL

      data_table <- local_reactives$data_table
      if(inherits(data_table, "fst_table")) {
        nms <- names(data_table)
        if("Electrode" %in% nms) {
          if(current_clip %in% nms) {
            sel <- data_table$Electrode %in% electrode_number
            if(any(sel)) {
              data <- data_table[[current_clip]][sel]

              if('Time' %in% nms) {
                time <- data_table$Time[sel]
              } else {
                time <- NULL
                current_time <- NULL
                time_range <- NULL
              }

              selected_data <- list(
                time = time,
                value = data,
                current_time = current_time,
                time_range = time_range
              )
            }
          }
        }
      }

      local_reactives$viewer_selection <- list(
        name = current_clip,
        subject = subject,
        electrode = electrode_number,
        data = selected_data,
        raw = info
      )

    }),
    proxy$mouse_event_double_click,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )

  ravedash::register_output(
    outputId = "viewer_selected_data",
    render_function = shiny::renderPlot({
      viewer_selection <- local_reactives$viewer_selection
      shiny::validate(
        shiny::need(
          expr = {
            is.list(viewer_selection) &&
              is.list(viewer_selection$data)
          },
          message = "Please double-click on one of the electrode"
        ),
        shiny::need(
          expr = length(viewer_selection$data$time) > 1,
          message = "No data can be visualized"
        )
      )

      time <- viewer_selection$data$time
      timestr <- sprintf("%.2f", time)
      data <- data.frame(time = as.numeric(timestr),
                         value = viewer_selection$data$value,
                         stringsAsFactors = TRUE)

      use_factor <- is.factor(data$value)

      plot_data <- as.data.frame(t(sapply(split(data, timestr), function(sub) {
        if( use_factor ) {
          c(sub$time[[1]], cumsum(table(sub$value)))
        } else {
          c(sub$time[[1]], nrow(sub), dipsaus::mean_se(sub$value))
        }
      })))

      if( use_factor ) {
        nms <- c("..Time", levels(data$value))
      } else {
        nms <- c("Time", "n", "mean", "se")
      }

      names(plot_data) <- nms

      plot_data <- plot_data[order(plot_data[[1]]), ]

      vname <- paste(viewer_selection$name, collapse = "")

      if( use_factor ) {

        x <- plot_data$..Time
        y <- as.matrix(plot_data)
        y[,1] <- 0

        plot(
          x = range(x, na.rm = TRUE),
          y = range(y, na.rm = TRUE),
          xlab = "Time", type = 'n', ylab = sprintf("%s (count)", vname),
          main = "Accumulated count over time"
        )

        graphics::grid()

        idxlist <- seq_len(length(nms) - 1)
        for(ii in idxlist) {
          graphics::polygon(
            x = c(x, rev(x)),
            y = c(y[, ii], rev(y[, ii + 1])),
            border = NA,
            col = dipsaus::col2hexStr(ii, alpha = 0.4)
          )
        }
        graphics::matlines(x = x, y = y[,-1], lty = 1, col = idxlist)
        ytext <- y[nrow(y), -1]
        xtext <- x[[length(x)]]

        graphics::text(
          labels = nms[-1],
          x = xtext, y = ytext,
          adj = c(1, 1)
        )

      } else {
        x <- plot_data$Time
        y <- plot_data$mean
        se <- plot_data$se
        se[is.na(se)] <- 0
        n <- plot_data$n
        n[is.na(n)] <- 0
        max_n <- max(n, na.rm = TRUE)
        plot(
          x = range(x, na.rm = TRUE),
          y = range(y + se, y - se, na.rm = TRUE),
          xlab = "Time", type = 'n', ylab = vname,
          main = ifelse(
            max_n > 1,
            sprintf("Mean value over time (max count: %.0f)", max_n),
            "Value over time")
        )
        graphics::grid()
        if(max_n > 1) {
          graphics::polygon(
            x = c(x, rev(x)),
            y = c(y - se, rev(y + se)),
            border = NA,
            col = dipsaus::col2hexStr("orange", alpha = 0.4)
          )
        }
        graphics::lines(x, y)
      }

      graphics::abline(v = viewer_selection$data$current_time, col = "gray60")
    }),
    output_opts = list(
      click = shiny::clickOpts(
        id = ns("viewer_selected_data_click"),
        clip = TRUE
      )
    )
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      info <- as.list(input$viewer_selected_data_click)
      time <- info$x
      if(length(time) != 1 || is.na(time) || !is.numeric(time)) {
        return()
      }
      proxy$set_controllers(list(Time = time))

      # set time
      shiny::isolate({
        if(is.list(local_reactives$viewer_selection$data)) {
          local_reactives$viewer_selection$data$current_time <- time
        }
      })
    }),
    input$viewer_selected_data_click,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
