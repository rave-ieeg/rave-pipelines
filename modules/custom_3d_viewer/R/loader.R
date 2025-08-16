# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(

      shiny::column(
        width = 6L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          ravedash::flex_group_box(
            title = "Project & Subject",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              loader_subject$ui_func()
            )
          ),

          ravedash::flex_group_box(
            title = "Electrode Coordinates",

            shidashi::flex_item(

              shiny::selectInput(
                inputId = ns("loader_electrode_source"),
                label = "Select a source of electrode coordinates",
                choices = c(
                  "Subject meta directory - electrodes.csv",
                  "File upload - auto",
                  "File upload - Scanner RAS",
                  "File upload - tk-registered (FreeSurfer) RAS",
                  "File upload - MNI152 RAS"
                ),
                selected = "Project",
                multiple = FALSE
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] !== 'Subject meta directory - electrodes.csv'",
                                    ns("loader_electrode_source")),
                shiny::fileInput(
                  inputId = ns("loader_electrode_tbl_upload"),
                  label = "Please upload a valid electrode table in [csv]",
                  multiple = FALSE, accept = ".csv"
                ),
                shiny::uiOutput(
                  outputId = ns("loader_electrode_tbl_upload_explanation")
                )
              )
            )

          )
        ),

        ravedash::input_card(
          title = "Table preview",
          tools = list(
            shidashi::card_tool(widget = "maximize")
          ),

          shiny::div(
            class = "fill-width overflow-x-scroll margin-bottom-10",
            DT::DTOutput(outputId = ns("loader_electrode_table"), width = "100%")
          )
        )
      ),

      shiny::column(
        width = 6L,
        ravedash::input_card(
          title = "Options",
          class_header = "",

          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Load subject",
              type = "primary",
              width = "100%"
            )
          ),

          ravedash::flex_group_box(
            title = "Additional Options",

            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_volume_types"),
                label = "Additional volumes",
                choices = c("aparc.DKTatlas+aseg", "aparc.a2009s+aseg"),
                selected = as.character(unlist(pipeline$get_settings("overlay_types"))),
                multiple = TRUE
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_surface_types"),
                label = "Additional surface types",
                choices = c("smoothwm", "inflated", "white", "pial-outer-smooth"),
                selected = as.character(unlist(pipeline$get_settings("surface_types"))),
                multiple = TRUE
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_annot_types"),
                label = "Additional surface annotations/measurements",
                choices = character(0L),
                selected = as.character(unlist(pipeline$get_settings("annot_types"))),
                multiple = TRUE
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::checkboxInput(
                inputId = ns("loader_use_spheres"),
                label = "Use spheres contacts",
                value = isTRUE(pipeline$get_settings("use_spheres"))
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::numericInput(
                inputId = ns("loader_override_radius"),
                label = "Override contact radius (sphere contact must be enabled)",
                value = NA_real_,
                step = 0.001,
                min = 0, max = 10
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::checkboxInput(
                inputId = ns("loader_use_template"),
                label = "Use template brain",
                value = FALSE
              )
            )

          )

          # shiny::textOutput(ns("loader_short_message"))
        )
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  local_reactives <- shiny::reactiveValues()
  local_data <- dipsaus::fastmap2()
  local_data$project_names <- dipsaus::fastmap2()

  output$loader_electrode_tbl_upload_explanation <- shiny::renderUI({
    source_type <- input$loader_electrode_source
    switch (
      source_type,
      "File upload - auto" = {
        shiny::column(
          12,
          shiny::tags$small("Upload a csv or tsv file with the following (case-sensitive) columns:"),
          shiny::tags$ul(shiny::tags$small(
            shiny::tags$li("`Electrode`: integer, electrode channel number"),
            shiny::tags$li("`Coord_x`: float, tk-registered left (negative) / right (positive)"),
            shiny::tags$li("`Coord_y`: float, tk-registered posterior (negative) / anterior (positive)"),
            shiny::tags$li("`Coord_z`: float, tk-registered inferior (negative) / superior (positive)"),
            shiny::tags$li("`Label`: characters, electrode label"),
            shiny::tags$li("`Radius`: optional electrode radius in mm"),
            shiny::tags$li("... (other optional columns)")
          ))
        )
      },
      "File upload - Scanner RAS" = {
        shiny::column(
          12,
          shiny::tags$small("Upload a csv or tsv file with the following (case-sensitive) columns:"),
          shiny::tags$ul(shiny::tags$small(
            shiny::tags$li("`name` or `Label`: characters, labels of the electrodes"),
            shiny::tags$li("`x`: float, T1 scanner left (negative) / right (positive)"),
            shiny::tags$li("`y`: float, T1 scanner posterior (negative) / anterior (positive)"),
            shiny::tags$li("`z`: float, T1 scanner inferior (negative) / superior (positive)"),
            shiny::tags$li("`Electrode`: optional integer, electrode channel number"),
            shiny::tags$li("`Radius`: optional electrode radius in mm"),
            shiny::tags$li("... (other optional columns)")
          ))
        )
      },
      "File upload - tk-registered (FreeSurfer) RAS" = {
        shiny::div(
          shiny::tags$small("Upload a csv or tsv file with the following (case-sensitive) columns:"),
          shiny::tags$ul(shiny::tags$small(
            shiny::tags$li("`name` or `Label`: characters, labels of the electrodes"),
            shiny::tags$li("`x`: float, tk-registered left (negative) / right (positive)"),
            shiny::tags$li("`y`: float, tk-registered posterior (negative) / anterior (positive)"),
            shiny::tags$li("`z`: float, tk-registered inferior (negative) / superior (positive)"),
            shiny::tags$li("`Electrode`: optional integer, electrode channel number"),
            shiny::tags$li("`Radius`: optional electrode radius in mm"),
            shiny::tags$li("... (other optional columns)")
          ))
        )
      },
      "File upload - MNI152 RAS" = {
        shiny::div(
          shiny::tags$small("Upload a csv or tsv file with the following (case-sensitive) columns:"),
          shiny::tags$ul(shiny::tags$small(
            shiny::tags$li("`name` or `Label`: characters, labels of the electrodes"),
            shiny::tags$li("`x`: float, MNI152 left (negative) / right (positive)"),
            shiny::tags$li("`y`: float, MNI152 posterior (negative) / anterior (positive)"),
            shiny::tags$li("`z`: float, MNI152 inferior (negative) / superior (positive)"),
            shiny::tags$li("`Electrode`: optional integer, electrode channel number"),
            shiny::tags$li("`Radius`: optional electrode radius in mm"),
            shiny::tags$li("... (other optional columns)")
          ))
        )
      }
    )
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      info <- input$loader_electrode_tbl_upload
      print(info)
      if(!length(info)) {
        local_reactives$electrode_table <- NULL
        return()
      }
      # check later!
      local_reactives$electrode_table <- ravecore::import_table(info$datapath, header = TRUE)
    }),
    input$loader_electrode_tbl_upload,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_electrode_coordinates <- shiny::reactive({
    project_name <- loader_project$get_sub_element_input()
    subject_code <- loader_subject$get_sub_element_input()
    if(!length(project_name) || !length(subject_code)) {
      return()
    }

    subject <- ravecore::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)

    electrode_table <- tryCatch(
      {
        subject$get_electrode_table(warn = FALSE)
      }, error = function(e) {
        NULL
      }
    )

    source_type <- paste(input$loader_electrode_source, collapse = "")

    # shiny::selectInput(
    #   inputId = ns("loader_electrode_source"),
    #   label = "Select a source of electrode coordinates",
    #   choices = c(
    #     "Subject meta directory - electrodes.csv",
    #     "File upload - auto",
    #     "File upload - Scanner RAS",
    #     "File upload - tk-registered (FreeSurfer) RAS",
    #     "File upload - MNI152 RAS"
    #   ),
    #   selected = "Project",
    #   multiple = FALSE
    # ),

    coordinate_sys <- ""

    switch (
      source_type,
      "File upload - auto" = {
        electrode_table <- local_reactives$electrode_table
        if(is.data.frame(electrode_table) && all(c("Coord_x", "Coord_y", "Coord_z") %in% names(electrode_table))) {
          electrode_table$x <- electrode_table$Coord_x
          electrode_table$y <- electrode_table$Coord_y
          electrode_table$z <- electrode_table$Coord_z
        }
        coordinate_sys <- "tkrRAS"
      },
      "File upload - Scanner RAS" = {
        electrode_table <- local_reactives$electrode_table
        coordinate_sys <- "ScannerRAS"
      },
      "File upload - tk-registered (FreeSurfer) RAS" = {
        electrode_table <- local_reactives$electrode_table
        coordinate_sys <- "tkrRAS"
      },
      "File upload - MNI152 RAS" = {
        electrode_table <- local_reactives$electrode_table
        coordinate_sys <- "MNI152"
      }
    )
    if(!is.data.frame(electrode_table)) { return() }
    nms <- names(electrode_table)
    if(
      !all(c("Coord_x", "Coord_y", "Coord_z") %in% nms) &&
      !all(c("x", "y", "z") %in% nms)
    ) {
      return()
    }
    if(!"Electrode" %in% nms) {
      if("Channel" %in% nms) {
        electrode_table$Electrode <- electrode_table$Channel
      } else {
        electrode_table$Electrode <- seq_len(nrow(electrode_table))
      }
    }
    electrode_table$Electrode <- as.integer(electrode_table$Electrode)
    if(!"Label" %in% nms) {
      if("name" %in% nms) {
        electrode_table$Label <- electrode_table$name
      } else {
        electrode_table$Label <- sprintf("Electrode%04d", electrode_table$Electrode)
      }
    }
    # remove these two reserved columns in case they are inconsistent
    electrode_table$Subject <- NULL
    electrode_table$SubjectCode <- NULL

    list(
      coordinate_table = electrode_table,
      coordinate_sys = coordinate_sys
    )
  })

  output$loader_electrode_table <- DT::renderDT({

    coords <- get_electrode_coordinates()

    coordinate_table <- coords$coordinate_table
    nms <- names(coordinate_table)

    shiny::validate(
      shiny::need(is.data.frame(coordinate_table),
                  message = "No electrode table found. No electrodes will be visualized")
    )

    re <- DT::datatable(coordinate_table, class = "display nowrap compact",
                        selection = "none", options = list(
                          pageLength = 5,
                          lengthMenu = c(5, 20, 100, 1000)
                        ))

    digit_nms <- c(
      'Coord_x', 'Coord_y', 'Coord_z', "MNI305_x", "MNI305_y", "MNI305_z",
      "MNI152_x", "MNI152_y", "MNI152_z", "T1R", "T1A", "T1S", "x", "y", "z",
      "OrigCoord_x", "OrigCoord_y", "OrigCoord_z", "DistanceShifted",
      "DistanceToPial", "Sphere_x", "Sphere_y", "Sphere_z"
    )
    digit_nms <- digit_nms[digit_nms %in% nms]

    re <- DT::formatRound(re, columns = digit_nms, digits = 2)
    re

  })

  shiny::bindEvent(
    ravedash::safe_observe({
      radius <- input$loader_override_radius
      if(isTRUE(radius > 0)) {
        shiny::updateCheckboxInput(
          session = session,
          inputId = "loader_use_spheres",
          value = TRUE
        )
      }
    }),
    input$loader_override_radius,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs

      coords <- get_electrode_coordinates()

      # Save the variables into pipeline settings file
      pipeline$save_data(
        data = coords$coordinate_table,
        name = "suggested_electrode_table",
        overwrite = TRUE
      )
      pipeline$set_settings(
        subject_code = input$loader_subject_code,
        project_name = input$loader_project_name,
        coordinate_sys = coords$coordinate_sys,
        overlay_types = input$loader_volume_types,
        surface_types = input$loader_surface_types,
        annot_types = input$loader_annot_types,
        use_spheres = input$loader_use_spheres,
        override_radius = input$loader_override_radius,
        use_template = input$loader_use_template,
        uploaded_source = NULL,
        controllers = list(),
        main_camera = list(),
        shiny_outputId = ns("viewer_ready")
      )

      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "Everything takes time. Some might need more patience than others."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      res <- pipeline$run(
        as_promise = TRUE,
        names = c("loaded_brain", "initial_brain_widget"),
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL
      )

      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravedash::logger("Data has been loaded loaded")

          # Close the alert
          dipsaus::close_alert2()
        },


        # this is what should happen when pipeline fails
        onRejected = function(e){

          # Close the alert
          dipsaus::close_alert2()

          # Immediately open a new alert showing the error messages
          dipsaus::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the 3D models:\n\n",
              paste(e$message, collapse = "\n")
            ),
            icon = "error",
            danger_mode = TRUE,
            auto_close = FALSE
          )
        }
      )
    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )



}
