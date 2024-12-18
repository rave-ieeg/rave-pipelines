
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
  server_tools <- get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  # server_tools$run_analysis_onchange(
  #   component_container$get_input_ids(c(
  #     "electrode_text", "baseline_choices",
  #     "analysis_ranges", "condition_groups"
  #   ))
  # )

  # Register event: main pipeline need to run
  regenerate_viewer_electrodes_only <- function() {
    shiny::validate(shiny::need(label = 'Need electrode table',
                                !is.null(local_data$electrode_table)
    ))

    local_reactives$update_3dviewer = Sys.time()

  }

  refresh_reactives <- function() {
    local_reactives$update_inputs <- Sys.time()
    local_reactives$update_outputs <- Sys.time()
    local_reactives$viewer_selection <- NULL

  }



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

      # regenerate_viewer()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()

      ravedash::logger("Reading data loaded flag...", level = "debug")

      if(!loaded_flag){ return() }

      ravedash::logger("Data read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      # old_data <- component_container$data$loaded_brain
      # if('multi-rave-brain' %in% class(loaded_brain)){
      #   if(
      #     identical(old_data$template_subject, loaded_brain$template_subject) &&
      #     # identical(old_data$subject_code, loaded_data$subject_code) &&
      #     # identical(old_data$electrode_table, loaded_data$electrode_table) &&
      #     setequal(old_data$surface_types, loaded_brain$surface_types)
      #   ) {
      #     ravedash::logger("The loaded data remain unchanged, skip initialization", level = "debug", use_glue = TRUE)
      #     return()
      #   }
      # }

      ravedash::logger("Trying to initialize", level = "debug", use_glue = TRUE)

      # TODO: reset UIs to default
      shidashi::clear_notifications()
      shiny::removeModal()

      # Reset preset UI & data
      # component_container$reset_data()
      # component_container$data$loaded_brain <- loaded_brain
      template_details <- pipeline$read("template_details")

      local_reactives$selected_template <- template_details$selected_template
      local_reactives$selected_surfaces <- template_details$selected_surfaces

      # component_container$initialize_with_new_data()

      # TODO Reset outputs
      local_reactives$update_outputs <- Sys.time()
      local_reactives$update_3dviewer <- Sys.time()

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )



  read_data_file <- function(info){
    ret <- NULL

    # if(!grepl("\\.(csv|fst|xls[x]{0,1})$", info$name, ignore.case = TRUE)) {
    if(!grepl("\\.(csv|xls[x]{0,1})$", info$name, ignore.case = TRUE)) {
      ravedash::error_notification(list(message = "Unsupported file format: only [.csv] and [.xls(x)] files are supported."))
      return(ret)
    }

    format <- "csv"
    if( grepl("\\.fst$", info$name, ignore.case = TRUE) ) {
      format <- "fst"
    } else if ( grepl("\\.xls[x]{0,1}$", info$name, ignore.case = TRUE) ) {
      format <- "xlsx"
    }

    ret <- ravedash::with_error_notification({
      switch(
        format,
        "csv" = {
          utils::read.csv(info$datapath, header = TRUE)
        },
        "fst" = {
          raveio::load_fst(info$datapath)
        },
        "xlsx" = {
          read_xlsx(info$datapath)
        }
      )
    })

    return (ret)
  }

  shiny::bindEvent(
    ravedash::safe_observe({

      info <- input$uploaded_electrode_table
      fin <- NULL
      fin <- read_data_file(info)

      if(!is.null(fin)) {
        shiny::validate(shiny::need(
          expr = all(c('SubjectCode', 'Electrode', 'Label', 'x', 'y', 'z') %in% names(fin)),
          message= 'The uploaded data file must contain named headers: SubjectCode, Electrode, Label, x, y, z'
        ))

        #TODO validation checks here
        local_data$electrode_table = fin
        local_reactives$update_3dviewer <- Sys.time()
      }
    }),
    input$uploaded_electrode_table,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      info <- input$uploaded_data_file
      fin <- NULL
      fin <- read_data_file(info)

      if(!is.null(fin)) {
        shiny::validate(shiny::need(
          expr = all(c('Electrode') %in% names(fin)),
          message= 'The uploaded data file must contain an Electrode column'
        ))

        #TODO validation checks here
        local_data$data_file = fin

        if(is.null(local_data$data_file$SubjectCode)) {
          local_data$data_file$SubjectCode = local_reactives$selected_template
        }

        local_reactives$update_3dviewer <- Sys.time()
      }
    }),
    input$uploaded_data_file,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )




  shiny::bindEvent(
    ravedash::safe_observe({

      info <- input$uploaded_connectivity_matrix
      fin <- NULL
      fin <- read_data_file(info)

      if(!is.null(fin)) {
        #TODO validation checks here
        local_data$connectivity_matrix <- fin
        local_reactives$update_3dviewer <- Sys.time()
      }

    }),
    input$uploaded_connectivity_matrix,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  ravedash::register_output(
    outputId = "viewer",
    output_type = "threeBrain",
    render_function = threeBrain::renderBrain({
      force(local_reactives$update_3dviewer)

      loaded_brain <- threeBrain::merge_brain(
        template_subject = local_reactives$selected_template,
        template_surface_types = unique(c("pial", local_reactives$selected_surfaces))
      )

      brain <- loaded_brain$template_object

      if(is.null(brain)) {
        return(threeBrain::threejs_brain(title = "No 3D model found"))
      }

      ## check if we electrodes to show
      if(!is.null(local_data$electrode_table)) {
        brain$set_electrodes(local_data$electrode_table, coord_sys = "MNI152", priority = 'sphere')
      }

      ## do we have "regular" data file?
      datafile <- NULL

      ## check if we have data to add
      if(!is.null(local_data$connectivity_matrix)) {

        #TODO have a way to control which electrodes have been stimulated etc
        # rather than enforcing square matrices

        dfA <- data.frame(
          SubjectCode = local_reactives$selected_template,
          Electrode = brain$electrodes$raw_table$Electrode
        )
        extra_cols <- ncol(dfA)

        dfA <- cbind(dfA, local_data$connectivity_matrix)

        if(!is.null(local_data$data_file)) {
          df <- merge(dfA, local_data$data_file, by = 'Electrode')
        } else {
          df <- dfA
        }
        brain$set_electrode_values(df)

        # fix the color of the stimulated electrode
        for(ri in 1:nrow(dfA)) {
          varname <- (colnames(dfA)[-(seq_along(extra_cols))])[ri]
          brain$electrodes$fix_electrode_color(ri, "black", varname)
        }
      } else {
        if(!is.null(local_data$data_file)) {
          brain$set_electrode_values(local_data$data_file)
        }
      }

      brain$render(outputId = "brain_viewer", session = session,
                   # palettes=res$palettes, value_ranges=res$val_ranges,
                   control_display = FALSE, side_display=FALSE,
                   timestamp=FALSE)
    })
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
          mni152 <- raveio::MNI305_to_MNI152 %*% c(mni305, 1)
          mni152 <- round(mni152[1:3])
          mni152 <- shiny::span(
            shiny::a(
              sprintf("MNI152: %s", paste(sprintf("%.0f", mni152), collapse = ",")),
              target = "_blank",
              href = raveio::url_neurosynth(mni152[1], mni152[2], mni152[3])
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
        electrode_mni <- raveio::MNI305_to_MNI152 %*% c(electrode_mni, 1)
        electrode_mni <- shiny::tags$small(
          shiny::a(
            sprintf("[MNI152: %s]", paste(sprintf("%.0f", electrode_mni[1:3]),
                                          collapse = ",")),
            target = "_blank",
            href = raveio::url_neurosynth(
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
