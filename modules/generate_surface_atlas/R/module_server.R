`%OF%` <- dipsaus::`%OF%`

module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Brain proxy
  brain_proxy <- threeBrain::brain_proxy(outputId = "viewer3d", session = session)

  # Load brain
  refresh_template_brain <- function(update_outputs = TRUE) {
    mapped_results <- pipeline$read("mapped_results")
    cleaned_inputs <- pipeline$read("cleaned_inputs")
    project_name <- cleaned_inputs$project_name
    subject_codes <- pipeline$get_settings("subject_codes")

    # create list of brain
    brain_list <- load_mapped_brain(project_name, subject_codes, mapped_results)

    template_brain <- threeBrain::merge_brain(
      .list = as.list(brain_list),
      template_subject = cleaned_inputs$template$name
    )

    value_names <- names(local_data$value_table)
    if("Electrode" %in% value_names) {
      template_brain$set_electrode_values(local_data$value_table)
    }

    local_data$mapped_results <- mapped_results
    local_data$brain_list <- brain_list
    local_data$template_brain <- template_brain

    if( update_outputs ) {
      local_reactives$update_outputs <- Sys.time()
    }
  }

  # run pipeline
  run_pipeline <- function(target = c("mapped_results", "mapped_atlas", "save_atlas")) {
    target <- match.arg(target)

    mapping_method <- switch (
      paste(input$mapping_method, collapse = ""),
      "high-density ECoG" = "high-density",
      {
        "high-density"
      }
    )

    settings <- list(
      subject_codes = input$subject_codes,
      mapping_method = mapping_method,
      mapping_params = list(
        width = input$mapping_param_micro_ecog_width,
        height = input$mapping_param_micro_ecog_height,
        interpolator = input$mapping_param_micro_ecog_interpolator,
        flip_hemisphere = input$mapping_param_micro_ecog_flip_hemisphere
      )
    )

    if( target != "mapped_results" ) {
      value_name <- input$value_name
      if(!length(value_name) || value_name %in% c('[None]', "[Subject]")) {
        stop("Value name cannot be empty, '[None]', or '[Subject]'")
      }
      mapping_threshold <- input$mapping_threshold
      if(length(mapping_threshold) != 1 || !is.finite(mapping_threshold)) {
        mapping_threshold <- 1
      }

      settings$value_name <- value_name
      settings$value_type <- input$value_type %OF% c("categorical", "numerical")
      settings$mapping_threshold <- mapping_threshold
    }

    if( target == "save_atlas" ) {
      atlas_name <- input$atlas_name
      if(length(atlas_name) != 1 || is.na(atlas_name) || !nzchar(atlas_name)) {
        atlas_name <- ""
      }
      settings$atlas_name <- atlas_name
    }

    pipeline$set_settings(.list = settings)

    ravedash::shiny_alert2(
      title = "Running",
      text = "Mapping to template...",
      icon = "info",
      auto_close = FALSE,
      buttons = FALSE
    )

    on.exit({
      Sys.sleep(0.5)
      ravedash::close_alert2(session = session)
    })

    pipeline$run(
      scheduler = "none",
      type = "smart",
      names = target,
      return_values = FALSE
    )

    if( target != "mapped_results" ) {
      local_data$mapped_atlas <- pipeline$read("mapped_atlas")
    }

    refresh_template_brain(update_outputs = target %in% c("mapped_results", "mapped_atlas"))

    if( target == "save_atlas" ) {

      shiny::removeModal(session = session)

      save_atlas_paths <- pipeline$read('save_atlas')
      folder_name <- dirname(save_atlas_paths[[1]])
      save_atlas_names <- paste(sQuote(basename(unlist(save_atlas_paths))), collapse = "\n")
      ravedash::show_notification(title = "Done exporting", type = "success", autohide = FALSE, close = TRUE, message = paste(
        collapse = "",
        c(
          "The atlas files have been created at path: [", folder_name,
          "]. The file names are: ", save_atlas_names, "."
        )
      ))
    }
  }

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      value_name <- input$value_name

      if(value_name %in% c("[None]", "[Subject]")) {
        ravedash::shiny_alert2(
          title = "Invalid data value name",
          icon = "error",
          danger_mode = TRUE,
          auto_close = TRUE,
          buttons = "OK",
          text = sprintf(
            "Invalid data name `%s`. Please choose a valid data from input `Data name` before exporting.",
            value_name
          )
        )
        return()
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "Widget: export group atlas",
          size = "m",
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            dipsaus::actionButtonStyled(
              inputId = ns("btn_export"),
              label = "Export",
              icon = ravedash::shiny_icons$download
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12L,

              shiny::textInput(
                inputId = ns("atlas_name"),
                label = "Atlas name",
                value = "",
                width = "100%",
                placeholder = value_name
              ),
              shiny::p(shiny::tags$small(
                sprintf(
                  "leave it blank to use the default name: `%s`. The atlas name must only contain letters, digits, under_scores",
                  value_name
                )
              ))
            ),
            shiny::column(
              width = 12L,
              shiny::checkboxGroupInput(
                inputId = ns("export_options"),
                label = "Additional options",
                choices = c(
                  "Overwrite existing files (if exists)"
                )
              )
            )
          )
        )
      )

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  # check whether the loaded data is valid and update/initialize inputs
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      cleaned_inputs <- pipeline$read("cleaned_inputs")
      settings <- pipeline$get_settings()
      value_table <- settings$value_table

      if(
        identical(local_data$cleaned_inputs, cleaned_inputs) &&
        identical(local_data$value_table, value_table)
      ) {
        ravepipeline::logger("The loader reported no change, skip initialization", level = "debug", use_glue = TRUE)
        return()
      }

      # reset UIs to default
      shiny::updateSelectInput(
        session = session,
        inputId = "subject_codes",
        choices = cleaned_inputs$subject_codes,
        selected = settings$mapping_method
      )

      shiny::updateNumericInput(
        session = session,
        inputId = "mapping_param_micro_ecog_width",
        value = c(settings$mapping_params$width, 16)[[1]]
      )

      shiny::updateNumericInput(
        session = session,
        inputId = "mapping_param_micro_ecog_height",
        value = c(settings$mapping_params$height, 16)[[1]]
      )

      shiny::updateSliderInput(
        session = session,
        inputId = "mapping_param_micro_ecog_interpolator",
        value = c(settings$mapping_params$interpolator, 0.3)[[1]]
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "mapping_param_micro_ecog_flip_hemisphere",
        choices = cleaned_inputs$subject_codes,
        selected = as.character(settings$mapping_params$flip_hemisphere)
      )

      value_names <- names(value_table)
      value_names <- value_names[!tolower(value_names) %in% c("subject", "time", "electrode")]
      value_names <- c("[None]", "[Subject]", value_names)

      value_name <- settings$value_name %OF% value_names
      shiny::updateSelectInput(
        session = session,
        inputId = "value_name",
        choices = value_names,
        selected = value_name
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "value_type",
        selected = settings$value_type %OF% c("categorical", "numerical")
      )

      mapping_threshold <- settings$mapping_threshold
      if(!isTRUE(mapping_threshold > 0)) {
        mapping_threshold <- 1
      }
      shiny::updateNumericInput(
        session = session,
        inputId = "mapping_threshold",
        value = mapping_threshold
      )

      # Make sure selectInput values are updated
      later::later(function() {
        shiny::updateSelectInput(
          session = session,
          inputId = "subject_codes",
          selected = cleaned_inputs$subject_codes
        )

        shiny::updateSelectInput(
          session = session,
          inputId = "mapping_param_micro_ecog_flip_hemisphere",
          selected = as.character(settings$mapping_params$flip_hemisphere)
        )

        shiny::updateSelectInput(
          session = session,
          inputId = "value_name",
          selected = value_name
        )
      }, delay = 0.5)


      # Reset preset UI & data
      component_container$reset_data()
      component_container$initialize_with_new_data()

      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")

      local_data$cleaned_inputs <- cleaned_inputs
      local_data$value_table <- value_table


      # Clean memory cache
      local_data$mapped_results <- NULL
      local_data$brain_list <- NULL
      local_data$template_brain <- NULL
      local_data$mapped_atlas <- NULL

      local_reactives$update_outputs <- FALSE

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      if(!length(local_reactives$update_outputs) ||
         isFALSE(local_reactives$update_outputs)) {
        return()
      }
      value_name <- input$value_name
      if(length(value_name) != 1) { return() }
      brain_proxy$set_controllers(list('Display Data' = value_name))
    }),
    input$value_name,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      run_pipeline(target = "mapped_atlas")
    }, error_wrapper = "alert"),
    input$generate_atlas,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      run_pipeline(target = "mapped_results")
    }, error_wrapper = "alert"),
    input$map_to_template,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      export_options <- input$export_options
      overwrite <- "Overwrite existing files (if exists)" %in% export_options
      atlas_name <- input$atlas_name
      if(length(atlas_name) != 1 || is.na(atlas_name) || !nzchar(atlas_name)) {
        atlas_name <- input$value_name
      }
      cleaned_inputs <- pipeline$read("cleaned_inputs")
      template_path <- file.path(cleaned_inputs$template$path, "label")
      value_type <- input$value_type
      if(identical(value_type, "numerical")) {
        file_ext <- "curv"
      } else {
        file_ext <- "annot"
      }
      expected_paths <- file.path(
        template_path,
        sprintf("%sh.%s.%s", c("l", "r"), atlas_name, file_ext))

      if(any(file.exists(expected_paths))) {
        if(!overwrite) {
          stop(
            "Atlas files with name `", atlas_name, "` have already existed ",
            "under the template folder: \n  ", normalizePath(template_path),
            "\nPlease choose another name."
          )
        }
        # for(f in expected_paths) {
        #   raveio::backup_file(f, remove = FALSE, quiet = TRUE)
        # }
      }
      run_pipeline(target = "save_atlas")
    }, error_wrapper = "alert"),
    input$btn_export,
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

      template_brain <- local_data$template_brain
      controllers <- list(
        "visibility" = "all visible"
      )

      value_name <- shiny::isolate(input$value_name)
      if(length(value_name) == 1) {
        controllers[['Display Data']] <- value_name
      }
      controllers = list("Display Data" = value_name)

      if(length(local_data$mapped_atlas)) {
        mapped_atlas <- local_data$mapped_atlas

        annot_name <- sprintf("label/rave_temporary.%s", mapped_atlas$file_ext)
        template_brain$template_object$add_annotation(
          annotation = annot_name, template_subject = NA)

        controllers[["Vertex Data"]] <- annot_name

      } else {
        controllers[["Vertex Data"]] <- "[none]"
      }

      template_brain$render(
        outputId = "viewer3d",
        session = session,
        show_modal = FALSE,
        controllers = controllers
      )


    }),
    output_type = "threeBrain"
  )

}
