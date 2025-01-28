
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
        # local_reactives$update_outputs <- Sys.time()
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

        nms <- names(fin)

        nms <- nms[!(nms %in% c('Electrode', 'SubjectCode', 'x', 'y', 'z'))]

        is_numeric <- sapply(fin[nms], function(x) {
          all(is.numeric(x[!is.na(x)]))
        })

        nms <- nms[is_numeric]

        # update available variable choices
        shiny::updateSelectInput(inputId='by_electrode_variable_selector',
                                 choices=nms, selected = nms[1])

        local_reactives$update_3dviewer <- Sys.time()
        local_reactives$update_outputs <- Sys.time()
      }
    }),
    input$uploaded_data_file,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({

      local_data$selected_variable = input$by_electrode_variable_selector
      local_reactives$update_outputs = runif(1)

    }), input$by_electrode_variable_selector, ignoreNULL=TRUE, ignoreInit = TRUE
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

  ravedash::register_output(
    outputId = "by_electrode",
    render_function = shiny::renderPlot({

      force(local_reactives$update_outputs)

      shiny::validate(shiny::need(!is.null(local_data$data_file), message = "No data file loaded"))

      selected_variable = local_data$selected_variable


      # print(str(local_data$data_file))

      # print(str(selected_variable))

      # write.csv(local_data$data_file, file='~/Desktop/tt.csv')

      # local_data <- list(data_file=read.csv('~/Desktop/tt.csv'))

      shiny::validate(
        shiny::need(!is.null(selected_variable), message = "No selected variable"),
        shiny::need(!is.null(local_data$data_file[[selected_variable]]), message = "Selected variable not available")
        )

      df <- subset(local_data$data_file,
                   !is.na(local_data$data_file[[selected_variable]]) &
                     nzchar(local_data$data_file[[selected_variable]])
                   )

      print(df)

      Y <- df[[selected_variable]]
      el <- df$Electrode
      .x <- seq_along(el)

      rutabaga::plot_clean(.x, Y, xlab = 'Electrode #', ylab=selected_variable)
      points(.x, Y, pch=16)
      points(.x, Y, type='h')
      rutabaga::ruta_axis(2, at=axTicks(2))
      rutabaga::ruta_axis(1, at=axTicks(1), labels = el[axTicks(1)])
    })
  )


}
