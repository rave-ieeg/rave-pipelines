
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  # ravedash::module_server_common(module_id = module_id, check_data_loaded = check_data_loaded, )
  server_tools <- ravedash::get_default_handlers(session = session)
  server_registry <- ravedash::register_rave_session()
  report_wizard <- ravedash::create_report_wizard(pipeline = pipeline, session = session)


  error_notification <- function(e) {
    if(!inherits(e, "condition")) {
      e <- simpleError(message = e$message)
    }
    ravepipeline::logger_error_condition(e)
    shidashi::show_notification(
      message = e$message,
      title = "Error found!",
      type = "danger",
      close = TRUE,
      autohide = TRUE, delay = 30000,
      class = ns("error_notif"),
      collapse = "\n"
    )
  }

  get_preview <- shiny::reactive({
    table <- local_reactives$table_preview
    if(!is.data.frame(table)) { return(NULL) }

    sel <- table$Coord_x != 0 | table$Coord_y != 0 | table$Coord_z != 0
    tkrRAS <- sprintf("%.0f,%.0f,%.0f", table$Coord_x, table$Coord_y, table$Coord_z)
    t1RAS <- sprintf("%.0f,%.0f,%.0f", table$T1R, table$T1A, table$T1S)
    mni305 <- sprintf("%.0f,%.0f,%.0f", table$MNI305_x, table$MNI305_y, table$MNI305_z)
    mni152 <- sprintf("%.0f,%.0f,%.0f", table$MNI152_x, table$MNI152_y, table$MNI152_z)
    prototype <- table$Prototype
    if(!length(prototype)) {
      prototype <- ""
    }

    return(data.frame(
      row.names = table$Electrode,
      Label = table$Label,
      Dimension = table$Dimension,
      LocationType = table$LocationType,
      Prototype = prototype,
      FSIndex = table$FSIndex,
      FSLabel = table$FSLabel,
      tkrRAS = tkrRAS,
      T1RAS = t1RAS,
      MNI305 = mni305,
      MNI152 = mni152
    ))

  })


  finalize_electrode_table <- function() {

    # final_results <- pipeline$read('localization_result_final')
    # subject <- component_container$data$subject
    # electrode_table <- final_results$electrode_table
    # electrode_table$SubjectCode <- subject$subject_code
    # ct_table <- final_results$ct_table
    #
    # raveio::save_meta2(
    #   data = electrode_table,
    #   meta_type = "electrodes",
    #   project_name = subject$project_name,
    #   subject_code = subject$subject_code
    # )
    # brain <- component_container$data$brain
    # if(length(final_results$prototype_definitions)) {
    #   proto_defs <- final_results$prototype_definitions
    #   for(nm in names(proto_defs)) {
    #     target_path <- file.path(brain$base_path, "RAVE", "geometry", sprintf("%s.json", nm))
    #     writeLines(proto_defs[[nm]], target_path)
    #   }
    # }
    # ct_tablepath <- file.path(subject$meta_path, "electrodes_in_ct.csv")
    # if(is.data.frame(ct_table) && nrow(ct_table)) {
    #   utils::write.csv(ct_table, file = ct_tablepath, row.names = FALSE)
    # } else if(file.exists(ct_tablepath)) {
    #   unlink(ct_tablepath)
    # }
    #
    # # backup unsaved.csv as it's not useful anymore
    # unlink(file.path(subject$meta_path, "electrodes_unsaved.csv"))
    # unlink(file.path(subject$meta_path, "geometry_unsaved.json"))
    #
    # # also save it to subject custom-data path so users can view the results with colors
    # custom_path <- file.path(subject$preprocess_settings$raw_path,
    #                          "rave-imaging", "custom-data")
    # custom_path <- raveio::dir_create2(custom_path)
    # raveio::save_fst(electrode_table, path = file.path(custom_path, sprintf("%s-electrodes.fst", subject$project_name)))
    #
    # # Save BIDS-compatible
    # bids <- raveio::convert_electrode_table_to_bids(subject)
    #
    # # sub-<label>[_ses-<label>][_acq-<label>][_space-<label>]_coordsystem.json
    # bids_prefix <- sprintf("sub-%s_space-%s", subject$subject_code, bids$meta$iEEGCoordinateSystem)
    # utils::write.table(
    #   x = bids$table,
    #   file = file.path(subject$meta_path, sprintf("%s_electrodes.tsv", bids_prefix)),
    #   sep = "\t",
    #   na = "n/a",
    #   row.names = FALSE
    # )
    # raveio::save_json(
    #   x = bids$meta,
    #   serialize = FALSE,
    #   auto_unbox = TRUE,
    #   con = file.path(
    #     subject$meta_path,
    #     sprintf("%s_coordsystem.json", bids_prefix)
    #   )
    # )



  }

  shiny::bindEvent(
    ravedash::safe_observe(error_wrapper = "notification", {

      # finalize_electrode_table()
      ravepipeline::logger("Check and save electrode table to subject.", level = "trace")

      ravedash::clear_notifications()

      dipsaus::shiny_alert2(
        title = "Please wait...",
        text = "Finalizing the electrode table...",
        icon = "info", auto_close = FALSE, buttons = FALSE
      )

      postprocess_opt <- input$postprocess_opt

      result_final <- pipeline$run(
        as_promise = FALSE,
        scheduler = "none",
        type = "callr",
        callr_function = NULL,
        async = FALSE,
        names = "localization_result_final"
      )

      electrode_table <- result_final$electrode_table
      needs_update <- FALSE

      if( "nonlinear_surface_mapping" %in% postprocess_opt ) {
        tryCatch(
          {
            surface_mapping <- pipeline$run(
              as_promise = FALSE,
              scheduler = "none",
              type = "callr",
              callr_function = NULL,
              async = FALSE,
              names = "postprocess_surface_mapping"
            )
            electrode_table$Sphere_x <- surface_mapping$Sphere_x
            electrode_table$Sphere_y <- surface_mapping$Sphere_y
            electrode_table$Sphere_z <- surface_mapping$Sphere_z
            electrode_table$DistanceShifted <- surface_mapping$DistanceShifted
            needs_update <- TRUE
          },
          error = function(e) {
            ravedash::show_notification(
              title = "[WARN] Skipped: surface mapping",
              message = paste(e$message, collapse = ""),
              type = "warning",
              autohide = FALSE
            )
          }
        )
      }

      if( "nonlinear_volumetric_mapping" %in% postprocess_opt ) {
        tryCatch(
          {
            volume_mapping <- pipeline$run(
              as_promise = FALSE,
              scheduler = "none",
              type = "callr",
              callr_function = NULL,
              async = FALSE,
              names = "postprocess_ants"
            )
            electrode_table$MNI152_x <- volume_mapping$MNI152_x
            electrode_table$MNI152_y <- volume_mapping$MNI152_y
            electrode_table$MNI152_z <- volume_mapping$MNI152_z
            electrode_table$MNI305_x <- volume_mapping$MNI305_x
            electrode_table$MNI305_y <- volume_mapping$MNI305_y
            electrode_table$MNI305_z <- volume_mapping$MNI305_z
            needs_update <- TRUE
          },
          error = function(e) {
            ravedash::show_notification(
              title = "[WARN] Skipped: non-linear MNI",
              message = paste(e$message, collapse = ""),
              type = "warning",
              autohide = FALSE
            )
          }
        )
      }

      subject <- pipeline$read("subject")
      if( needs_update ) {
        ravecore::save_meta2(
          data = electrode_table,
          meta_type = "electrodes",
          project_name = subject$project_name,
          subject_code = subject$subject_code
        )
      }

      # generate reports
      report_wizard$generate(subject, "electrodeview")

      Sys.sleep(0.5)
      dipsaus::close_alert2()

      dipsaus::shiny_alert2(
        title = "Success!",
        icon = 'success',
        text = "Electrode table has been exported to subject > rave > meta > electrodes.csv",
        auto_close = TRUE, buttons = list("OK" = TRUE),
        on_close = function(...) {
          shiny::removeModal()
        }
      )

    }),
    input$save_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$electrode_table_preview <- DT::renderDataTable({
    df <- get_preview()
    shiny::validate(
      shiny::need(is.data.frame(df), message = "No preview generated.")
    )
    DT::datatable(
      data = df,
      selection ="none",
      rownames = TRUE,
      class = "compact stripe",
      filter = "none",
      editable = "none",
      options = list(
        ordering = FALSE,
        bFilter = 0,
        paging = FALSE,
        keys = TRUE
      )
    )
  })

  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      # Collect input data
      local_reactives$table_preview <- NULL

      brain <- component_container$data$brain
      # geom_names <- names(brain$electrodes$geometries)
      # geometry_definitions <- structure(names = geom_names, lapply(geom_names, function(gname) {
      #   geometry <- brain$electrodes$geometries[[gname]]
      #   if(!inherits(geometry, "ElectrodePrototype")) {
      #     return(NULL)
      #   }
      #   geometry$as_json(flattern = TRUE)
      # }))
      localization_list <- lapply(local_data$plan_list, function(group_info) {
        group_table <- group_info$group_table
        prototype <- group_info$prototype
        if(!is.null(prototype)) {
          attr(group_table, "prototype") <- prototype$as_list(flattern = TRUE)
        }
        group_table
      })
      pipeline$set_settings(
        localization_list = localization_list
      )

      results <- pipeline$run(
        as_promise = FALSE,
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL,
        async = FALSE,
        names = c("localization_result_initial")
      )

      ravepipeline::logger("Fulfilled: ", pipeline$pipeline_name, " - localization_result_initial", level = 'debug')

      morph_mri_exists <- pipeline$read("morph_mri_exists")
      table_preview <- pipeline$read("localization_result_initial")
      local_reactives$table_preview <- table_preview$electrode_table

      shidashi::clear_notifications(class = "pipeline-error")

      ravedash::show_notification(
        message = "Current localization is staged (temporarily saved). Next time, the localization will start from here.",
        title = "Staged!",
        type = "info",
        icon = ravedash::shiny_icons$save,
        delay = 2000
      )

      shiny::showModal(shiny::modalDialog(
        title = "Electrode table",
        easyClose = FALSE,
        size = "xl",
        shiny::div(
          class = "overflow-auto max-height-vh70",
          DT::dataTableOutput(ns("electrode_table_preview"), width = "auto")
        ),
        footer = shiny::tagList(
          shiny::column(
            width = 12L,

            shiny::div(
              shiny::checkboxGroupInput(
                inputId = ns("postprocess_opt"),
                label = "Post-process options:",
                inline = FALSE,
                choiceNames = c(
                  "Surface mapping to inflated brain & fsaverage",
                  "Non-linear MNI152 coordinates"
                ),
                choiceValues = c(
                  "nonlinear_surface_mapping",
                  "nonlinear_volumetric_mapping"
                )
              )
            ),

            shiny::div(
              class = "float-right",
              shiny::modalButton("Dismiss"),
              dipsaus::actionButtonStyled(ns("save_btn"), "Save to subject")
            )
          )

        )
      ))

      return()

    }, error_wrapper = "notification"),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  reload_plan <- function(){
    # subject <- component_container$data$subject
    # if(is.null(subject)) {
    #   local_data$plan_list <- NULL
    # }
    # plan_file <- file.path(subject$meta_path, "electrodes_unsaved.csv")
    # if(!file.exists(plan_file)) {
    #   local_data$plan_list <- NULL
    # }
    #
    # local_data$plan_list <- read_plan_list( plan_file, strict = TRUE, instantiate = TRUE )
    # table <- raveio::safe_read_csv(plan_file)
    # if(!"Interpolation" %in% names(table)) {
    #   table$Interpolation <- "default"
    # }
    # if(!"Prototype" %in% names(table)) {
    #   table$Prototype <- ""
    # }
    #
    # plan_list <- split(table, ~ LabelPrefix + Dimension + LocationType)
    # plan_list <- plan_list[vapply(plan_list, function(x){ nrow(x) > 0 }, FALSE)]
    # # calculate layout & add geometries
    # brain <- component_container$data$brain
    # plan_list <- structure(
    #   lapply(plan_list, function(sub) {
    #     if(!grepl("^[0-9,x. ]+$", sub$Interpolation[[1]])) {
    #       sub$Interpolation <- as.character(max(tryCatch({
    #         dim <- as.integer(dipsaus::parse_svec(sub$Dimension[[1]], sep = "[,x]", unique = FALSE))
    #         dim <- dim[!is.na(dim)]
    #         if(length(dim) > 1) {
    #           dim[[1]] - 2L
    #         } else {
    #           nrow(sub) - 2L
    #         }
    #       }, error = function(e){
    #         nrow(sub) - 2L
    #       }), 1))
    #     }
    #
    #     if( nrow(sub) > 0 && !identical(sub$Prototype[[1]], "") ) {
    #       # try to load existing geometry
    #       prototype <- brain$electrodes$add_geometry(
    #         label_prefix = sub$LabelPrefix[[1]],
    #         prototype_name = sub$Prototype[[1]]
    #       )
    #       if(!is.null(prototype)) {
    #         channel_numbers <- table$Electrode[table]
    #         prototype$set_contact_channels(sub$Electrode, sub$ContactOrder)
    #       }
    #     }
    #     sub
    #   }),
    #   names = names(plan_list)
    # )
    #
    # local_data$plan_list <- plan_list[order(vapply(plan_list, function(x){ as.integer(min(x$Electrode)) }, FUN.VALUE = 1L))]
    # local_data$plan_list
  }


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      subject <- pipeline$read("subject")
      subject <- ravecore::as_rave_subject(subject$subject_id, strict = FALSE)
      plan_file <- file.path(subject$meta_path, "electrodes_unsaved.csv")
      if(!file.exists(plan_file)) {
        # stop("Cannot find `electrodes_unsaved.csv`. This file should have been generated by previous pipeline code.")
        return()
      }

      ct_exists <- pipeline$read('ct_exists')
      brain <- pipeline$read('brain')
      fslut <- pipeline$read('fslut')

      ravepipeline::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # Reset preset UI & data
      component_container$reset_data()
      local_data$plan_list <- NULL
      local_data$last_group_id <- NULL

      component_container$data$subject <- subject
      component_container$data$ct_exists <- ct_exists
      component_container$data$brain <- brain
      component_container$data$fslut <- fslut

      # load plan table
      # reload_plan()
      local_data$plan_list <- read_plan_list( plan_file, brain = brain, strict = TRUE, instantiate = TRUE )
      lapply(local_data$plan_list, function(group_info) {
        prototype <- group_info$prototype
        if(!is.null(prototype)) {
          proto <- brain$electrodes$add_geometry(
            label_prefix = group_info$label_prefix,
            prototype_name = group_info$prototype_name
          )
          prototype$copy(proto)
          prototype$name <- group_info$prototype_name
        }
      })


      component_container$initialize_with_new_data()
      local_reactives$table_output <- NULL
      local_reactives$refresh <- Sys.time()

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )





  # Register outputs
  output$label_selectors_wrapper <- shiny::bindEvent(
    shiny::renderUI({

      if(!isTRUE(ravedash::watch_data_loaded())) { return() }

      shiny::validate(shiny::need(length(local_data$plan_list) > 0, message = "Cannot find localization plan list. Please reload this subject."))

      local_reactives$active_plan <- NULL
      lapply(seq_along(local_data$plan_list), function(ii) {
        group_info <- local_data$plan_list[[ ii ]]
        if(!length(group_info)) { return(NULL) }
        blabel <- group_info$button_label
        dipsaus::actionButtonStyled(
          ns(sprintf("switch_plan_btn_%d", ii)),
          label = blabel,
          type = "default",
          class = "margin-5 btn-xs"
        )
      })
    }),
    local_reactives$refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  lapply(seq_len(300), function(ii){
    shiny::bindEvent(
      ravedash::safe_observe({
        plan_list <- local_data$plan_list
        if(ii > length(plan_list)) { return() }

        group_info <- plan_list[[ii]]
        nm <- group_info$button_label
        ravepipeline::logger("Switching to localization plan: {nm}", use_glue = TRUE, level = "trace")

        old_plan <- shiny::isolate(local_reactives$active_plan)
        local_reactives$active_plan <- ii

        # update button style
        if(length(old_plan)) {
          dipsaus::updateActionButtonStyled(session, inputId = sprintf("switch_plan_btn_%s", old_plan), type = "default")
        }

        dipsaus::updateActionButtonStyled(session, inputId = sprintf("switch_plan_btn_%d", ii), type = "primary")
      }),
      input[[sprintf("switch_plan_btn_%d", ii)]],
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
  })

  # output$localization_viewer <-
  ravedash::register_output(
    outputId = "localization_viewer",
    output_type = "threeBrain",
    render_function = threeBrain::renderBrain({
      local_reactives$refresh

      subject <- component_container$data$subject
      shiny::validate(
        shiny::need(
          isTRUE(ravedash::watch_data_loaded()) && !is.null(subject),
          message = "Waiting for the data..."
        )
      )

      theme <- shiny::isolate(ravedash::current_shiny_theme())

      brain <- component_container$data$brain
      brain$electrodes$objects <- list()
      # brain <- threeBrain::freesurfer_brain2(
      #   fs_subject_folder = subject$freesurfer_path,
      #   subject_name = subject$subject_code
      # )

      localize_data <- pipeline$read("localize_data")

      control_presets <- c("localization", "animation", "display_highlights")
      controllers <- list()
      controllers[["Highlight Box"]] <- FALSE
      controllers[["Overlay Coronal"]] <- TRUE
      controllers[["Overlay Axial"]] <- TRUE
      controllers[["Overlay Sagittal"]] <- TRUE
      controllers[["Show Panels"]] <- FALSE
      controllers[["Show Time"]] <- FALSE
      controllers[["Left Hemisphere"]] <- "hidden"
      controllers[["Right Hemisphere"]] <- "hidden"
      controllers[["Left Opacity"]] <- 1.0
      controllers[["Right Opacity"]] <- 1.0
      # controllers[["Electrode Shape"]] <- "prototype"

      dipsaus::shiny_alert2(
        title = "Finalizing...",
        text = "Generating 3D viewer, rendering voxel data...",
        auto_close = FALSE,
        danger_mode = FALSE,
        icon = "info",
        buttons = FALSE
      )
      on.exit({
        dipsaus::close_alert2()
      }, add = TRUE)

      if(!is.null(localize_data$ct_header) && is.list(localize_data$ct_header)) {
        class(localize_data$ct_header) <- "threeBrain.nii"
      }

      viewer <- brain$localize(
        ct_path = localize_data$ct_path,
        transform_space = localize_data$transform_space,
        transform_matrix = localize_data$transform_matrix,
        mri_path = localize_data$mri_path,
        show_modal = FALSE,
        controllers = controllers
      )

      viewer
    })
  )

  brain_proxy <- threeBrain::brain_proxy("localization_viewer")

  ravedash::safe_observe({
    theme <- ravedash::current_shiny_theme()
    brain_proxy$set_background(dipsaus::col2hexStr(theme$background))
  })

  snapshot_group <- function(group_id) {
    if(length(group_id) != 1 || !is.numeric(group_id)) { return() }
    if(group_id < 1 || group_id > length(local_data$plan_list)) { return() }
    group_info <- local_data$plan_list[[group_id]]
    if(!length(group_info)) { return() }

    # list(
    #   dimension = dimension,
    #   type = pname,
    #   hemisphere = hemisphere,
    #   min_channel = min(channel_number, na.rm = TRUE)
    #   prototype <- prototype
    #   prototype_name <- toupper(pname)
    #   geometry_table <- prototype$control_points
    #   batch_size <- nrow(geometry_table)
    #   group_table <- sub
    #   label_prefix <- re$label
    #   interpolation_string <- interpolation_string
    #   button_label
    # )

    group_info$group_id <- group_id

    if( length(group_info$prototype) ) {
      group_info$geometry_table <- group_info$prototype$control_points
    } else {
      group_info$geometry_table <- NULL
    }

    # print(re[c('group_id', 'batch_size', 'label_prefix', 'prototype_name', 'geometry_table', 'interpolation_string')])
    return(group_info)
  }

  show_group <- function(group_id, reset_labels = FALSE, reset = FALSE) {
    if(missing(group_id)) {
      group_id <- local_reactives$active_plan
    }
    brain_proxy$clear_localization(update_shiny = reset)
    snapshot <- snapshot_group( group_id )
    if(!is.list(snapshot)) { return() }

    group_table <- snapshot$group_table
    geometry_table <- snapshot$geometry_table
    prototype <- snapshot$prototype
    prototype_name <- snapshot$prototype_name
    hemisphere <- snapshot$hemisphere
    label_prefix <- snapshot$label_prefix

    try({
      # Old threeBrain might not have this method
      brain_proxy$set_incoming_localization_hemisphere( hemisphere )
    })

    ii <- 1L
    if(!reset) {
      if( is.data.frame(geometry_table) ) {

        item <- prototype$as_list(flattern = TRUE)
        item$is_prototype <- TRUE
        brain_proxy$add_localization_electrode(item, update_shiny = FALSE)

        # visualize prototype table
        has_na <- FALSE
        for(ii in seq_len(nrow(geometry_table))) {
          row <- geometry_table[ii, ]
          if(is.na(row$tkr_R) || is.na(row$tkr_A) || is.na(row$tkr_S)) {
            has_na <- TRUE
            break
          }
          item <- as.list(row)
          item$Electrode <- NULL
          item$VertexNumber <- NULL
          item$SurfaceType <- NULL
          item$Radius <- NULL
          item$Hemisphere <- hemisphere
          item$Coord_x <- row$tkr_R
          item$Coord_y <- row$tkr_A
          item$Coord_z <- row$tkr_S
          if( reset_labels ) {
            item$FSIndex <- NULL
            item$FSLabel <- NULL
          }
          brain_proxy$add_localization_electrode(item, update_shiny = FALSE)
        }
      } else {
        for(ii in seq_len(nrow(group_table))) {
          row <- group_table[ii, ]
          if(all(c(row$Coord_x, row$Coord_y, row$Coord_z) == 0)) {
            break
          }
          # Use OrigCoord_xyz if possible
          item <- as.list(row)
          # remove read-only attributes
          item$Electrode <- NULL
          item$VertexNumber <- NULL
          item$SurfaceType <- NULL
          item$Radius <- NULL

          if(length(item$Hemisphere)) {
            item$Hemisphere <- tolower(item$Hemisphere)
            if( !item$Hemisphere %in% c("left", "right") ) {
              item$Hemisphere <- NULL
            }
          }
          # If `OrigCoord_xyz` exist, use them, otherwise use `Coord_xyz`
          item$Coord_x <- row$OrigCoord_x
          item$Coord_y <- row$OrigCoord_y
          item$Coord_z <- row$OrigCoord_z
          item$Coord_x %?<-% row$Coord_x
          item$Coord_y %?<-% row$Coord_y
          item$Coord_z %?<-% row$Coord_z
          if( reset_labels ) {
            item$FSIndex <- NULL
            item$FSLabel <- NULL
          }
          brain_proxy$add_localization_electrode(item, update_shiny = FALSE)
        }
      }
      brain_proxy$set_localization_electrode(
        which = -1, update_shiny = TRUE,
        list(Coord_x = 0, Coord_y = 0, Coord_z = 0))
    } else {
      group_table$Coord_x <- 0
      group_table$Coord_y <- 0
      group_table$Coord_z <- 0
      local_data$plan_list[[group_id]]$group_table <- group_table

      if(length(prototype)) {
        prototype$reset_world_control_points()

        # clear_localization() will also reset the prototype on canvas
        # re-add
        if(is.data.frame(geometry_table)) {
          item <- prototype$as_list(flattern = TRUE)
          item$is_prototype <- TRUE
          item$transform <- as.vector(diag(1, 4))
          brain_proxy$add_localization_electrode(item, update_shiny = FALSE)
        }
      }
    }

    ct_exists <- isTRUE(component_container$data$ct_exists)

    # We want to set `Edit Mode` only when it was "disabled", "refine", do not
    # change "CT/volume" <-> "MRI slice" as users might want that
    if( isTRUE(shiny::isolate(brain_proxy$controllers[["Edit Mode"]]) %in% c("disabled", "refine")) ||
        !isTRUE(local_data$edit_mode_initialized) ) {
      brain_proxy$set_controllers(list(
        `Edit Mode` = ifelse(ct_exists, "CT/volume", "MRI slice")
      ))
      local_data$edit_mode_initialized <- TRUE
    }

    brain_proxy$set_controllers(list(
      `Interp Size` = snapshot$interpolation_string
      # `Interp Size` = snapshot$Interpolation[[1]]
    ))

    # check if brain shift is needed
    if(length( group_table$SurfaceElectrode )) {
      group_table$SurfaceElectrode <- as.logical(group_table$SurfaceElectrode)
      has_surface_electrode <- any(group_table$SurfaceElectrode)
      has_depth_electrode <- !all(group_table$SurfaceElectrode)
    } else if( length(group_table$LocationType) ){
      has_surface_electrode <- any(group_table$LocationType %in% "ECoG")
      has_depth_electrode <- any(group_table$LocationType %in% c("sEEG", "iEEG"))
    } else {
      has_surface_electrode <- FALSE
      has_depth_electrode <- TRUE
    }
    if( has_surface_electrode ) {
      max_shifted <- max(c(group_table$DistanceShifted, 0))
      shift_mode <- "soft threshold"
      if( has_depth_electrode ) {
        shift_mode <- "hard threshold"
        max_shifted <- ceiling(max_shifted * 10) / 10
      }
      brain_proxy$set_controllers(list(
        `Brain Shift` = shift_mode,
        `Max Shift` = max_shifted
      ))
    } else {
      brain_proxy$set_controllers(list(
        `Brain Shift` = "disabled"
      ))
    }

    return()
  }

  current_group <- shiny::reactive({
    local_reactives$refresh
    local_reactives$refresh_table
    group_id <- local_reactives$active_plan
    return( snapshot_group( group_id ) )
  })


  # render group table here
  format_group_table <- function(df) {
    mni152 <- ravecore::MNI305_to_MNI152 %*% rbind(df$MNI305_x, df$MNI305_y, df$MNI305_z, 1)
    sel <- df$Coord_x == 0 & df$Coord_y == 0 & df$Coord_z == 0
    mni152[, sel] <- 0
    mni152_text <- sprintf("%.0f,%.0f,%.0f", mni152[1,], mni152[2,], mni152[3,])
    mni152_link <- url_neurosynth(mni152[1,], mni152[2,], mni152[3,])
    mni152_col <- sprintf('<a href="%s" target="_blank">%s</a>', mni152_link, mni152_text)
    mni152_col[sel] <- ""
    table_output <- data.frame(
      row.names = df$Electrode,
      Label = df$Label,
      tkrRAS = sprintf("%.0f,%.0f,%.0f", df$Coord_x, df$Coord_y, df$Coord_z),
      MNI152 = mni152_col,
      FSLabel = df$FSLabel
    )
    table_output
  }
  shiny::bindEvent(
    ravedash::safe_observe({
      ginfo <- current_group()
      if(!(is.list(ginfo) && is.data.frame(ginfo$group_table))) {
        local_reactives$table_output <- NULL
      }

      group_id <- ginfo$group_id

      table_output <- NULL
      if( is.data.frame(ginfo$geometry_table) ) {
        # TODO: display control points instead of the group table
        geometry_table <- ginfo$geometry_table

        sel <- !(
          is.na( geometry_table$tkr_R ) |
            is.na( geometry_table$tkr_A ) |
            is.na( geometry_table$tkr_S )
        )
        tkrRAS_str <- sprintf("%.0f,%.0f,%.0f", geometry_table$tkr_R, geometry_table$tkr_A, geometry_table$tkr_S)
        tkrRAS_str[!sel] <- ""

        chan_str <- sprintf("%.0f", geometry_table$Channel)
        chan_str[ chan_str == "NA" ] <- "non-contact"

        if(is.data.frame(geometry_table)) {
          table_output <- data.frame(
            Electrode = chan_str,
            ModelXYZ = sprintf("%.0f,%.0f,%.0f", geometry_table$model_x, geometry_table$model_y, geometry_table$model_z),
            tkrRAS = tkrRAS_str
          )
        }
      }
      if(!is.data.frame(table_output)) {
        table_output <- format_group_table(ginfo$group_table)
      }

      if(identical(local_data$last_group_id, group_id)) {
        DT::replaceData(proxy_table, data = table_output)
      } else {
        local_reactives$table_output <- table_output
        local_data$last_group_id <- group_id
      }
    }),
    current_group()
  )

  proxy_table <- DT::dataTableProxy(outputId = "group_table")

  group_table_selected <- shiny::reactive({
    ridx <- input$group_table_rows_selected
    if(length(ridx) != 1 || ridx < 1) { return(FALSE) }
    group_id <- local_reactives$active_plan
    if(length(group_id) != 1 || !is.numeric(group_id)) { return(FALSE) }
    if(group_id > length(local_data$plan_list)) { return(FALSE) }
    snapshot <- local_data$plan_list[[group_id]]
    if(is.data.frame(snapshot$geometry_table)) { return(NA) }
    group_table <- snapshot$group_table
    if(!is.data.frame(group_table)) { return(FALSE) }
    if(nrow(group_table) < ridx) { return(FALSE) }
    return(TRUE)
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      ridx <- input$group_table_rows_selected
      # if(length(ridx) != 1 || ridx < 1) { return() }

      group_id <- local_reactives$active_plan
      # if(length(group_id) != 1 || !is.numeric(group_id)) { return() }
      # if(group_id > length(local_data$plan_list)) { return() }

      shidashi::show_notification(
        title = "Waiting for response",
        message = shiny::tagList(
          "Please double-click on the CT volume to re-localize this electrode. ",
          "If you accidentally requested re-localization, click ",
          shiny::actionLink(ns("relocalize_cancel"), "here"), " to cancel."
        ),
        type = "default", autohide = FALSE, session = session,
        class = ns("relocalize")
      )
      local_data$flag_relocalize <- list(
        group_id = group_id,
        which = ridx
      )
      # label <- input$fsindex_label
      # group_table <- local_data$plan_list[[group_id]]
      # # if(!is.data.frame(group_table)) { return() }
      # if(isTRUE(label %in% component_container$data$fslut$labels)) {
      #   # change label
      #   # if(nrow(group_table) < ridx) { return() }
      #   group_table$FSIndex[[ridx]] <- component_container$data$fslut$cmap$get_key(label)
      #   group_table$FSLabel[[ridx]] <- label
      #   local_data$plan_list[[group_id]] <- group_table
      # }

    }),
    input$relocalize_electrode,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      local_data$flag_relocalize <- NULL
      shidashi::clear_notifications(class = ns("relocalize"))
    }),
    input$relocalize_cancel,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$fsindex_selector <- shiny::renderUI({
    selected <- group_table_selected()
    if(length(selected) != 1 || isFALSE(selected)) { return() }
    ridx <- shiny::isolate(input$group_table_rows_selected)

    labels <- c("[unchanged]", unname(component_container$data$fslut$labels))
    selected <- shiny::isolate(input$fsindex_label) %OF% labels

    if(isTRUE(selected)) {
      shiny::tagList(
        shiny::div(
          class = "margin-top-5",
          shiny::p("To re-localize this electrode, click ", shiny::actionLink(ns("relocalize_electrode"), label = "here")),
          shiny::p("If you want to change the FreeSurfer label, please select one from below:")
        ),
        shidashi::flex_container(
          style = "align-items: end;",
          shidashi::flex_item(
            size = 2,
            shinyWidgets::pickerInput(
              inputId = ns("fsindex_label"), label = "FreeSurfer Label",
              choices = labels, selected = selected,
              multiple = FALSE, width = "100%",
              options = list(
                `live-search` = TRUE
              )
            )
          ),
          shidashi::flex_item(
            shiny::div(
              class = "form-group",
              dipsaus::actionButtonStyled(ns("fsindex_label_next"), "Save & Next", width = "100%")
            )
          )
        )
      )
    } else {
      shiny::div(
        class = "margin-top-5",
        shiny::p("To re-localize this electrode, click ", shiny::actionLink(ns("relocalize_electrode"), label = "here"), ".")
      )
    }
  })


  shiny::bindEvent(
    ravedash::safe_observe({
      if(!isTRUE(group_table_selected())) { return() }

      ridx <- input$group_table_rows_selected
      # if(length(ridx) != 1 || ridx < 1) { return() }

      group_id <- local_reactives$active_plan
      # if(length(group_id) != 1 || !is.numeric(group_id)) { return() }
      # if(group_id > length(local_data$plan_list)) { return() }

      label <- input$fsindex_label
      group_table <- local_data$plan_list[[group_id]]$group_table
      # if(!is.data.frame(group_table)) { return() }
      if(isTRUE(label %in% component_container$data$fslut$labels)) {
        # change label
        # if(nrow(group_table) < ridx) { return() }
        group_table$FSIndex[[ridx]] <- component_container$data$fslut$cmap$get_key(label)
        group_table$FSLabel[[ridx]] <- label
        local_data$plan_list[[group_id]]$group_table <- group_table
      }

      next_ridx <- ridx + 1L
      if(next_ridx > nrow(group_table)) {
        next_ridx <- 1L
      }
      DT::replaceData(proxy_table, data = format_group_table(group_table))

      DT::selectRows(proxy = proxy_table, selected = next_ridx)

    }),
    input$fsindex_label_next,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$group_table <- DT::renderDataTable({
    table_output <- local_reactives$table_output
    shiny::validate(
      shiny::need(is.data.frame(table_output), message = "Please choose one electrode group first.")
    )

    DT::datatable(
      table_output,
      selection = list(mode = "single", target = "row"),
      rownames = TRUE,
      class = "compact cell-border stripe",
      filter = "none",
      editable = FALSE, escape = -4,
      options = list(ordering = FALSE, bFilter = 0, paging = FALSE)
    )
  })

  output$instruction_text <- shiny::renderUI({
    ginfo <- current_group()
    if(!is.list(ginfo)) { return() }

    dim <- ginfo$group_table$Dimension[[1]]
    dim <- as.integer(dipsaus::parse_svec(dim, sep = "[,x]"))
    dim <- dim[!is.na(dim)]

    label_prefix <- ginfo$label_prefix
    batch_size <- ginfo$batch_size
    interpolation_string <- ginfo$interpolation_string

    shiny::div(
      shiny::p("Example instruction to localize electrode ", dipsaus::deparse_svec(ginfo$group_table$Electrode), ":"),
      shiny::tags$ul(
        shiny::tags$li(
          "Make sure the ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Edit Mode"),
          " is ",
          shiny::pre(class="pre-compact no-padding display-inline", "CT/volume"), " (with CT) or ",
          shiny::pre(class="pre-compact no-padding display-inline", "MRI slice"), " (w/o CT)"
        ),
        shiny::tags$li(
          sprintf("Double-click on the viewer to mark electrode %s1", label_prefix)
        ),
        shiny::tags$li(
          sprintf("Double-click on the viewer to mark electrode %s%d", label_prefix, batch_size)
        ),
        shiny::tags$li(
          "Set ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Interp Size"),
          " to ", batch_size - 2L, ", or enter the electrode spacing."
        ),
        shiny::tags$li(
          "Click on ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Interpolate"),
          " to automatically register the electrode contacts."
        )
      ),
      shiny::hr(),
      shiny::p("Example instruction to refine electrode locations:"),
      shiny::tags$ul(
        shiny::tags$li(
          "Make sure the ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Edit Mode"),
          " is set to ",
          shiny::pre(class="pre-compact no-padding display-inline", "refine")
        ),
        shiny::tags$li(
          "Click ",
          shiny::pre(class="pre-compact no-padding display-inline", "3D Viewer"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Electrode Localization"), " > ",
          shiny::pre(class="pre-compact no-padding display-inline", "Auto-Adjust All")
        ),
        shiny::tags$li(
          "Alternatively, if you want to adjust single electrode, ",
          shiny::tags$ul(
            shiny::tags$li("Double-click an electrode, the highlighted electrode will turn red"),
            shiny::tags$li(
              "Use keyboard ",
              shiny::pre(class="pre-compact no-padding display-inline", "1"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "2"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "3"),
              " or ",
              shiny::pre(class="pre-compact no-padding display-inline", "shift+1"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "shift+2"), ",",
              shiny::pre(class="pre-compact no-padding display-inline", "shift+3"),
              " to move the highlighted electrode around"
            )
          )
        )
      )
    )
  })

  .GlobalEnv$eee <- environment()

  shiny::bindEvent(
    ravedash::safe_observe({
      show_group(reset = TRUE)
      local_reactives$refresh_table <- Sys.time()
    }),
    input$action_reset_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      show_group(reset_labels = TRUE)
      local_reactives$refresh_table <- Sys.time()
    }),
    input$action_reset_fslabel_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      show_group()
    }),
    local_reactives$active_plan,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      subject <- component_container$data$subject
      quat <- brain_proxy$localization_add_quaternion
      if(!length(quat)) { return() }
      ginfo <- current_group()
      if(!is.list(ginfo)) { return() }
      prototype <- ginfo$prototype
      if(!inherits(prototype, "ElectrodePrototype")) { return() }
      geometry_table <- ginfo$geometry_table
      if( !is.data.frame(geometry_table) ) { return() }

      tryCatch({
        q <- ravetools::new_quaternion()
        s <- sin(quat$degreeRad)
        q$set(
          quat$direction[[1]] * s,
          quat$direction[[2]] * s,
          quat$direction[[3]] * s,
          cos(quat$degreeRad)
        )
        v <- ravetools::new_vector3(x = c(1, 0, 0),
                                    y = c(0, 1, 0),
                                    z = c(0, 0, 1))
        v$apply_quaternion(q)
        trans <- prototype$transform
        trans[1:3, 1:3] <- v[] %*% trans[1:3, 1:3]
        prototype$set_transform( trans );
        brain_proxy$set_matrix_world(
          name = sprintf("%s, Prototype - %s", subject$subject_code, prototype$name),
          m44 = prototype$transform
        )
      }, error = function(e) {
        ravepipeline::logger("Geometry {prototype$name} up direction is not set. Reason: {paste(e$message, collapse = '\n')}",
                         use_glue = TRUE, level = "debug")
      })
    }),
    brain_proxy$localization_add_quaternion,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      subject <- component_container$data$subject
      table <- brain_proxy$localization_table
      if(!is.data.frame(table)) { return() }

      interpolation <- brain_proxy$controllers[["Interp Size"]]

      # local_data$flag_relocalize <- list(
      #   group_id = group_id,
      #   which = ridx
      # )
      update_viewer <- FALSE
      if( is.list(local_data$flag_relocalize) ) {
        relocalize <- TRUE
        group_id <- local_data$flag_relocalize$group_id
        ginfo <- snapshot_group( group_id )
      } else {
        relocalize <- FALSE
        ginfo <- current_group()
        group_id <- ginfo$group_id
      }
      if(!is.list(ginfo)) { return() }

      group_table <- ginfo$group_table
      geometry_table <- ginfo$geometry_table
      prototype <- ginfo$prototype

      if( is.data.frame(geometry_table) ) {
        has_geometry <- TRUE
        gtable <- geometry_table
      } else {
        has_geometry <- FALSE
        gtable <- group_table
      }
      # local_data$plan_list[[group_id]]
      n <- nrow(table)
      if(min(nrow(table), nrow(gtable)) <= 0) { return() }

      if( relocalize ) {
        # reset flag
        flag_data <- local_data$flag_relocalize
        local_data$flag_relocalize <- NULL
        update_viewer <- TRUE
        shidashi::clear_notifications(class = ns("relocalize"))

        # source item
        idx1 <- n

        # which to update
        idx2 <- flag_data$which

      } else {
        if( has_geometry ) {
          n <- min(nrow(geometry_table), n)
        } else {
          n <- min(nrow(group_table), n)
        }
        idx1 <- seq_len(n)

        # which to update
        idx2 <- idx1
      }

      if( has_geometry ) {
        geometry_table$tkr_R[idx2] <- table$Coord_x[idx1]
        geometry_table$tkr_A[idx2] <- table$Coord_y[idx1]
        geometry_table$tkr_S[idx2] <- table$Coord_z[idx1]
        tryCatch({
          prototype$set_transform_from_points(
            x = geometry_table$tkr_R,
            y = geometry_table$tkr_A,
            z = geometry_table$tkr_S
          )
          ravepipeline::logger("Electrode {prototype$name} control point is set.", use_glue = TRUE, level = "debug")
          brain_proxy$set_matrix_world(
            name = sprintf("%s, Prototype - %s", subject$subject_code, prototype$name),
            m44 = prototype$transform
          )
          constact_tkr <- prototype$get_contact_positions( apply_transform = TRUE )
          nn <- min(nrow(group_table), nrow(constact_tkr))
          if( nn > 0 ) {
            group_table[1:nn, c("Coord_x", "Coord_y", "Coord_z")] <- constact_tkr[, 1:3]
          }
        }, error = function(e) {
          ravepipeline::logger("Geometry {prototype$name} control point is not yet set. Reason: {paste(e$message, collapse = '\n')}",
                           use_glue = TRUE, level = "debug")
        })
        if("Channel" %in% names(geometry_table)) {
          channel_order <- lapply(seq_along(idx2), function( ii ) {
            row_geom_ii <- idx2[[ ii ]]
            row_table_ii <- idx1[[ ii ]]
            chan <- geometry_table$Channel[[ row_geom_ii ]]
            if( !length(chan) || is.na(chan) ) { return(NULL) }
            jj <- which(group_table$Electrode == chan)
            if(!length(jj)) { return(NULL) }
            # new idx1, idx2
            return(c(row_table_ii, jj))
          })
          channel_order <- do.call("rbind", channel_order)
          if(length(channel_order)) {
            idx1 <- channel_order[, 1]
            idx2 <- channel_order[, 2]
          } else {
            idx2 <- integer(0L)
            idx1 <- integer(0L)
          }
        } else {
          idx2 <- integer(0L)
          idx1 <- integer(0L)
        }
      }

      if(length(idx1) > 0) {
        group_table$Coord_x[idx2] <- table$Coord_x[idx1]
        group_table$Coord_y[idx2] <- table$Coord_y[idx1]
        group_table$Coord_z[idx2] <- table$Coord_z[idx1]
        group_table$MNI305_x[idx2] <- table$MNI305_x[idx1]
        group_table$MNI305_y[idx2] <- table$MNI305_y[idx1]
        group_table$MNI305_z[idx2] <- table$MNI305_z[idx1]

        group_table$OrigCoord_x[idx2] <- table$OrigCoord_x[idx1]
        group_table$OrigCoord_y[idx2] <- table$OrigCoord_y[idx1]
        group_table$OrigCoord_z[idx2] <- table$OrigCoord_z[idx1]

        group_table$FSIndex[idx2] <- table$FSIndex[idx1]
        group_table$FSLabel[idx2] <- table$FSLabel[idx1]

        group_table$FSIndex_aparc_a2009s_aseg[idx2] <- table$FSIndex_aparc_a2009s_aseg[idx1]
        group_table$FSLabel_aparc_a2009s_aseg[idx2] <- table$FSLabel_aparc_a2009s_aseg[idx1]

        group_table$FSIndex_aparc_aseg[idx2] <- table$FSIndex_aparc_aseg[idx1]
        group_table$FSLabel_aparc_aseg[idx2] <- table$FSLabel_aparc_aseg[idx1]

        group_table$FSIndex_aparc_DKTatlas_aseg[idx2] <- table$FSIndex_aparc_DKTatlas_aseg[idx1]
        group_table$FSLabel_aparc_DKTatlas_aseg[idx2] <- table$FSLabel_aparc_DKTatlas_aseg[idx1]

        group_table$FSIndex_aseg[idx2] <- table$FSIndex_aseg[idx1]
        group_table$FSLabel_aseg[idx2] <- table$FSLabel_aseg[idx1]

        group_table$SurfaceElectrode[idx2] <- table$SurfaceElectrode[idx1]
        group_table$DistanceShifted[idx2] <- table$DistanceShifted[idx1]
        group_table$DistanceToPial[idx2] <- table$DistanceToPial[idx1]

        group_table$Sphere_x[idx2] <- table$Sphere_x[idx1]
        group_table$Sphere_y[idx2] <- table$Sphere_y[idx1]
        group_table$Sphere_z[idx2] <- table$Sphere_z[idx1]

      }

      # update interpolation settings
      if(length(interpolation) == 1) {
        n_interp <- NA
        interpolation <- trimws(interpolation)
        if( grepl("^[0-9]+$", interpolation) ) {
          n_interp <- as.integer(interpolation)
        } else {
          n_interp <- unlist(lapply(trimws(strsplit(interpolation, ",")[[1]]), function(item) {
            if(grepl("^[0-9]+$", item)) {
              return(as.integer(item))
            }
            item <- as.numeric(strsplit(item, "x")[[1]])
            if( length(item) == 0 || anyNA(item) ) { return(NA) }
            if( length(item) == 1 ) {
              if( item <= 0 ) { return(NA) }
              return( 1L )
            }
            if( item[[2]] < 0 ) { return(NA) }
            if( item[[2]] < 1 ) { return(0) }
            if( item[[1]] <= 0 ) { return(NA) }
            return(as.integer(item[[2]]))
          }))
          n_interp <- sum(c(n_interp, 0))
        }
        if( !is.na(n_interp) && n_interp > 0 ) {
          group_table$Interpolation <- interpolation
        }
      }

      if(update_viewer) {
        # Make sure the electrode table is updated, otherwise `show_group()`
        # will reset it. For prototypes, the table is updated within the
        # prototype so no need to change here
        if( !has_geometry ) {
          local_data$plan_list[[group_id]]$group_table <- group_table
        }
        show_group()
      } else {
        local_data$plan_list[[group_id]]$group_table <- group_table
        local_reactives$refresh_table <- Sys.time()
      }
    }),
    brain_proxy$localization_table,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
