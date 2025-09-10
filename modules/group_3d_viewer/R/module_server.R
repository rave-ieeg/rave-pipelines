
module_server <- function(input, output, session, ...){

  `%OF%` <- dipsaus::`%OF%`


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  local_data$has_template <- FALSE
  local_data$template_needs_update <- FALSE

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

  # Brain proxy
  brain_proxy <- threeBrain::brain_proxy(outputId = "viewer3d", session = session)

  gen_capability_ui <- function(mapping_capabilities, i) {
    if(!is.data.frame(mapping_capabilities) || nrow(mapping_capabilities) < i) { return(NULL) }
    # i <- 1
    capabilities <- as.list(mapping_capabilities[i, ])
    subject_code <- capabilities$Subject

    if( capabilities$mni152 ) {
      if(capabilities$ants) {
        volume_capability <- "yes"
      } else {
        volume_capability <- "yes (if Python is configured)"
      }
    } else {
      volume_capability <- "no"
    }

    cache <- NULL
    if(capabilities$surface_cache) {
      cache <- "surface"
    }
    if(capabilities$mni152_cache) {
      cache <- c(cache, "volume")
    }
    if(!length(cache)) {
      cache <- "none"
    } else {
      cache <- paste(cache, collapse = ", ")
    }


    ui <- shiny::tags$dl(
      shiny::tags$dt(subject_code, " ", shiny::tags$small(sprintf("(cache: %s)", cache))),
      shiny::tags$dd(
        sprintf(
          "Non-linear surface map: %s; non-linear MNI152 map: %s; affine map: %s",
          ifelse(isTRUE(capabilities$surface), "yes", "no"),
          volume_capability,
          ifelse(isTRUE(capabilities$freesurfer), "yes", "no")
        )
      )
    )
    ui
  }

  generate_viewer <- function() {
    # input$generate_viewer_btn
    # validate
    subject_codes <- input$subject_codes
    # mapping_method <- input$mapping_method %OF% c("auto", "sphere.reg", "MNI152 (non-linear)", "Affine")
    # if( identical(mapping_method, "MNI152 (non-linear)") ) {
    #   mapping_method <- "mni152"
    # }
    # mapping_method <- tolower(mapping_method)

    # electrode_radius <- as.numeric(input$electrode_radius)
    # if(!isTRUE(electrode_radius > 0)) {
    #   electrode_radius <- NA
    # }

    # Collect input data
    pipeline$set_settings(
      subject_codes = subject_codes
      # mapping_method = mapping_method,
      # electrode_radius = electrode_radius
    )

    ravedash::shiny_alert2(
      title = "Verifying capabilities...",
      text = "Collecting information",
      icon = "info",
      auto_close = FALSE,
      buttons = FALSE
    )

    result <- tryCatch(
      {
        local_reactives$mapping_capabilities <- pipeline$run(
          names = "mapping_capabilities",
          return_values = TRUE, type = "vanilla", scheduler = "none")

        TRUE
      },
      error = function(e) {
        e
      }
    )

    Sys.sleep(0.5)
    ravedash::close_alert2(session = session)

    mapping_capabilities <- shiny::isolate(local_reactives$mapping_capabilities)

    if(!isTRUE(result) && !is.data.frame(mapping_capabilities)) {
      ravedash::shiny_alert2(
        title = "Errors",
        text = paste(
          "Found an error while checking the mapping capabilities:\n\n",
          paste(e$message, collapse = "\n")
        ),
        icon = "error",
        danger_mode = TRUE,
        auto_close = FALSE
      )
      return()
    }

    shiny::showModal(
      shiny::modalDialog(
        title = "Mapping options",
        size = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          dipsaus::actionButtonStyled(inputId = ns("make_mapping_btn"), label = "Calculate mapping")
        ),

        shiny::fluidRow(
          shiny::column(
            width = 12L,
            shiny::selectInput(
              input = ns("mapping_method"),
              label = "Mapping method",
              choices = c("auto", "non-linear surface normalization", "non-linear volumetric normalization"),
              selected = input$mapping_method %OF% c("auto", "non-linear surface normalization", "non-linear volumetric normalization"),
              width = "100%"
            ),
            shiny::checkboxInput(
              inputId = ns("clear_cache"),
              label = "Clear existing cache and force re-calculating normalization",
              value = FALSE, width = "100%"
            )
          ),
          shiny::column(
            width = 12L,
            shiny::p(
              "Please check the following capability list to see if desired normalization is available to each subject. ",
              "Some mappings might not be available, depending on the preprocessing. ",
              "Click on the links to check the procedures needed to enable different transform types."
            ),
            shiny::tags$ul(
              lapply(seq_len(nrow(mapping_capabilities)), function(i) {
                gen_capability_ui(mapping_capabilities, i)
              })
            )
          )
        ) # .row
      )
    )
  }

  update_viewer <- function() {
    if(!isTRUE(local_data$template_needs_update)) { return() }

    # Update the viewer with
    template <- pipeline$read("template")

    radius <- input$electrode_radius
    if(isTRUE(radius > 0)) {
      # Needs to overwrite the electrode radius
      lapply(template$objects, function(brain) {
        electrode_table <- brain$electrodes$raw_table
        if(is.data.frame(electrode_table)) {
          electrode_table$Radius <- radius
          brain$set_electrodes(electrode_table, priority = "sphere")
        }
        value_table <- brain$electrodes$value_table
        if(is.data.frame(value_table)) {
          brain$set_electrode_values(value_table)
        }
        brain
      })
    }

    data_uploader <- input$data_uploader
    if(length(data_uploader)) {
      value_table <- utils::read.csv(data_uploader$datapath[[1]])
      template$set_electrode_values(value_table)
    }

    additional_volumes <- input$additional_volumes
    additional_surfaces <- input$additional_surfaces
    additional_annots <- input$additional_annots

    missing_volumes <- additional_volumes[!additional_volumes %in% template$template_object$atlas_types]
    missing_surfaces <- additional_surfaces[!additional_surfaces %in% template$template_object$surface_types]

    if(length(c(missing_volumes, missing_surfaces))) {
      temporary_template <- threeBrain::merge_brain(
        template_subject = local_data$template_name,
        template_surface_types = unique(c('pial', "sphere.reg", input$additional_surfaces)),
        template_atlas_types = additional_volumes,
        template_annotation_types = additional_annots
      )
      template$template_object <- temporary_template$template_object
    } else {
      lapply(additional_annots, function(annot) {
        template$template_object$add_annotation(annot)
      })
    }

    local_data$template <- template
    local_data$has_template <- TRUE
    local_reactives$update_outputs <- Sys.time()

  }

  shiny::bindEvent(
    ravedash::safe_observe({

      local_data$template_needs_update <- TRUE
      generate_viewer()

    }),
    input$generate_viewer_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      local_data$template_needs_update <- TRUE
      if(!local_data$has_template) {
        generate_viewer()
        return()
      }

      update_viewer()


    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  value_table_example <- shiny::reactive({
    tryCatch({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      if(!isTRUE(input$download_sample_value_table_type %in% SAMPLE_VALUE_CHOICES)) {
        return()
      }
      subject_codes <- input$subject_codes
      if(!length(subject_codes)) {
        subject_codes <- sprintf("Subject%03d", 1:3)
      }

      subject_codes <- sort(sample(subject_codes, 10, replace = TRUE))
      electrodes <- unname(unlist(lapply(table(subject_codes), seq_len)))
      nelec <- 10
      tbl <- switch(
        input$download_sample_value_table_type,
        "Simple property" = {
          data.frame(
            Subject = subject_codes,
            Electrode = electrodes,
            tValue = round(rnorm(nelec), 2)
          )
        },
        "Multiple properties" = {
          re <- data.frame(
            Subject = subject_codes,
            Electrode = electrodes,
            MyDiscreteValue = sample(LETTERS, size = nelec, replace = TRUE),
            MyContinuousValue = round(rnorm(nelec), 2)
          )
          re$MyContinuousValue[sample(nelec, min(5, max(nelec - 4, 1)))] <- NA
          re
        },
        "Animation" = {
          data.frame(
            Subject = subject_codes,
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
      sample_value_table <- value_table_example()
      if(!is.data.frame(sample_value_table)) {
        stop("No sample value table is available. Electrode table is not detected.")
      }
      utils::write.csv(x = sample_value_table, file = con, row.names = FALSE)
    }
  )

  output$download_sample_value_table_output <- DT::renderDT({
    sample_value_table <- value_table_example()
    shiny::validate(
      shiny::need(
        is.data.frame(sample_value_table),
        message = "No example is available. Electrode table is not detected..."
      )
    )
    caption <- switch(
      input$download_sample_value_table_type,
      "Simple property" = "Example with a single value property: column names must contain 'Electrode' (electrode channel) and the variable name.",
      "Multiple properties" = {
        sample_value_table$MyContinuousValue <- sprintf("%.2f", sample_value_table$MyContinuousValue)
        "Example with continuous and discrete variables. Use NA to represent missing values. Column names must contain 'Electrode' (electrode channel) and the variable names."
      },
      "Animation" = "Example for animation. Column names must contain 'Electrode' (electrode channel), Time (time in seconds), and at least one variable name.",
      { NULL }
    )

    DT::datatable(
      sample_value_table,
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

  shiny::bindEvent(
    ravedash::safe_observe({
      mapping_method <- input$mapping_method %OF% c("auto", "non-linear surface normalization", "non-linear volumetric normalization")
      mapping_method <- switch (
        mapping_method,
        "non-linear surface normalization" = "sphere.reg",
        "non-linear volumetric normalization" = "mni152",
        "auto"
      )
      use_cache <- !isTRUE(input$clear_cache)

      pipeline$set_settings(
        use_cache = use_cache,
        mapping_method = mapping_method
      )

      ravedash::shiny_alert2(
        title = "Mapping to template...",
        text = "Generating group brain",
        icon = "info",
        auto_close = FALSE,
        buttons = FALSE
      )

      on.exit({
        Sys.sleep(0.5)
        ravedash::close_alert2(session = session)
      })

      pipeline$run(
        names = "template",
        return_values = FALSE, type = "vanilla", scheduler = "none")

      shiny::removeModal(session = session)

      local_reactives$update_outputs <- Sys.time()
      local_data$template <- pipeline$read("template")
      local_data$has_template <- TRUE
      update_viewer()

    }),
    input$make_mapping_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      selected <- shiny::isolate(input$download_template_type) %OF% SAMPLE_VALUE_CHOICES

      shiny::showModal(shiny::modalDialog(
        title = "Example: group electrode value table",
        size = "l",
        easyClose = FALSE,
        footer = shiny::modalButton("Dismiss"),
        shiny::fluidRow(
          shiny::column(
            width = 12L,
            shiny::selectInput(
              inputId = ns("download_sample_value_table_type"),
              label = shiny::tagList(
                shiny::span("Select a template type "),
                shiny::downloadLink(
                  outputId = ns("download_template_do"),
                  label = "(download this template)",
                )
              ),
              choices = SAMPLE_VALUE_CHOICES,
              selected = selected
            )
          ),
          shiny::column(
            width = 12L,
            DT::DTOutput(outputId = ns("download_sample_value_table_output"))
          )
        )
      ))
    }),
    input$data_uploader_example_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )



  output$make_mapping_capabilities_table <- shiny::renderTable(
    striped = TRUE, hover = FALSE, spacing = "xs", width = "100%",
    expr = {
      mapping_capabilities <- local_reactives$mapping_capabilities
      shiny::validate(
        shiny::need(
          is.data.frame(mapping_capabilities) &&
            nrow(mapping_capabilities) > 0, "")
      )
      tbl <- data.frame(
        freesurfer = ifelse(mapping_capabilities$freesurfer, "yes", "no"),
        surface = ifelse(mapping_capabilities$surface, "yes", "no"),
        mni152 = ifelse(mapping_capabilities$mni152, "yes", "no"),
        cache = paste(
          ifelse(mapping_capabilities$surface_cache, "surface", ""),
          ifelse(mapping_capabilities$mni152_cache, "volume", ""), sep = " "
        )
      )
      names(tbl) <- c(
        "FreeSurfer", "Surface Normalization", "Non-linear MNI152", "Cache"
      )
      tbl
    }
  )


  # check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      project_name <- pipeline$get_settings("project_name")
      template_info <- pipeline$read("template_info")
      if(
        identical(project_name, local_data$project_name) &&
        identical(template_info$name, local_data$template_name)
      ) {
        return()
      }
      ravepipeline::logger("Initializing the module UI", level = "debug")

      local_data$project_name <- project_name
      local_data$template_name <- template_info$name
      local_data$has_template <- FALSE
      local_data$template <- NULL
      local_reactives$mapping_capabilities <- NULL

      local_reactives$update_outputs <- FALSE

      subject_codes_filtered <- as.character(pipeline$read("subject_codes_filtered"))

      project <- ravecore::as_rave_project(project_name)
      all_subject_codes <- project$subjects()

      # update subject_codes
      shiny::updateSelectInput(
        session = session,
        inputId = "subject_codes",
        choices = all_subject_codes,
        selected = as.character(subject_codes_filtered)
      )

      # update object list
      additional_volumes <- as.character(template_info$volumes)
      shiny::updateSelectInput(
        session = session,
        inputId = "additional_volumes",
        choices = additional_volumes,
        selected = as.character(input$additional_volumes)
      )

      additional_surfaces <- as.character(template_info$surfaces)
      shiny::updateSelectInput(
        session = session,
        inputId = "additional_surfaces",
        choices = additional_surfaces,
        selected = input$additional_surfaces
      )

      additional_annots <- as.character(template_info$annotations)
      shiny::updateSelectInput(
        session = session,
        inputId = "additional_annots",
        choices = additional_annots,
        selected = input$additional_annots
      )

      later::later(
        delay = 0.5,
        function() {
          # update subject_codes
          shiny::updateSelectInput(
            session = session,
            inputId = "subject_codes",
            selected = as.character(subject_codes_filtered)
          )
        }
      )

      # Reset preset UI & data
      component_container$reset_data()

      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Mapping method",
          size = "l",
          easyClose = TRUE,

          shiny::div(
            shiny::p(
              "RAVE supports multiple template mapping methods:"),
            shiny::tags$ul(
              shiny::tags$li(
                "`auto`: by default, RAVE automatically determines the mapping ",
                "from the existing information without additional computation. ",
                "Choose the other options if you would like to ensure the mapping."
              ),
              shiny::tags$li(
                "`sphere.reg`, or surface normalization: This method uses ",
                shiny::a(
                  href = 'https://surfer.nmr.mgh.harvard.edu/fswiki/SurfaceRegAndTemplates',
                  target = "_blank", "FreeSurfer spherical normalization"), " ",
                "to align the gyrification information. (FreeSurfer recon-all ",
                "folder required)"
              ),
              shiny::tags$li(
                "`MNI152 (non-linear)`, or volumetric normalization: RAVE ",
                "uses symmtric differomorphic registration to ",
                "normalize the underlying T1w image to the MNI152(b) template. ",
                "The internal implementation requires ",
                shiny::a(
                  href = "https://rave.wiki/posts/installation/installation.html#optional-but-recommended-install-isolated-python-environment",
                  target = "_blank", "Python for RAVE. "),
                "If you haven't run the YAEL normalization during the image preprocess ",
                "(module MRI/CT Preprocessing), then the normalization script will be ",
                "provided. (RAVE does not run the script as it will take ~5 min per subject)"
              ),
              shiny::tags$li(
                "`Affine`: naive template mapping using affine transform. This transform ",
                "is available automatically with the FreeSurfer surface reconstruction."
              )
            )
          )
        )
      )
    }),
    input$mapping_method_helper,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )




  # Register outputs
  ravedash::register_output(
    outputId = "viewer3d",
    render_function = threeBrain::renderBrain({
      shiny::validate(
        shiny::need(
          length(local_reactives$update_outputs) &&
            !isFALSE(local_reactives$update_outputs),
          message = "Please run the module first"
        )
      )

      template <- local_data$template
      mapping_method <- pipeline$read("mapping_method")

      controllers <- list(
        "visibility" = "all visible"
      )

      switch (
        mapping_method,
        "sphere.reg" = {
          controllers[["Surface Mapping"]] <- "sphere.reg"
          controllers[["Volume Mapping"]] <- "sphere.reg"
          controllers[["Projection Threshold"]] <- 30
        },
        "mni152" = {
          controllers[["Surface Mapping"]] <- "mni305"
          controllers[["Volume Mapping"]] <- "mni305"
          controllers[["Projection Threshold"]] <- 0
        },
        {
          controllers[["Surface Mapping"]] <- "sphere.reg"
          controllers[["Volume Mapping"]] <- "mni305"
          controllers[["Projection Threshold"]] <- 30
        }
      )

      # value_name <- shiny::isolate(input$value_name)
      # if(length(value_name) == 1) {
      #   controllers[['Display Data']] <- value_name
      # }
      # controllers = list("Display Data" = value_name)

      # if(length(local_data$mapped_atlas)) {
      #   mapped_atlas <- local_data$mapped_atlas
      #
      #   annot_name <- sprintf("label/rave_temporary.%s", mapped_atlas$file_ext)
      #   template_brain$template_object$add_annotation(
      #     annotation = annot_name, template_subject = NA)
      #
      #   controllers[["Vertex Data"]] <- annot_name
      #
      # } else {
      #   controllers[["Vertex Data"]] <- "[none]"
      # }

      template$render(
        outputId = "viewer3d",
        session = session,
        show_modal = FALSE,
        controllers = controllers
      )


    }),
    output_type = "threeBrain"
  )




}
