# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

  overlay_types0 <- as.character(unlist(pipeline$get_settings("overlay_types")))
  surface_types0 <- as.character(unlist(pipeline$get_settings("surface_types")))
  annot_types0 <- as.character(unlist(pipeline$get_settings("annot_types")))
  streamline_types0 <- as.character(unlist(pipeline$get_settings("streamline_types")))

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

              shidashi::register_input(
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
                tooltip = "Where the electrode coordinates come from: the subject's electrodes.csv, or an uploaded table.",
                inputId = "loader_electrode_source",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "[Select] Where the electrode coordinates come from:",
                  "\"Subject meta directory - electrodes.csv\" (the subject's",
                  "electrode table; use this one), or a \"File upload - ...\"",
                  "choice, which needs the user to upload a table. Read by",
                  "script `load_data`."
                )
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
            ravedash::load_data_button(label = "Load subject", width = "100%")
          ),

          ravedash::flex_group_box(
            title = "Additional Options",

            shidashi::flex_item(
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns("loader_volume_types"),
                  label = "Additional volumes",
                  choices = unique(c("aparc.DKTatlas+aseg", "aparc.a2009s+aseg", overlay_types0)),
                  selected = overlay_types0,
                  multiple = TRUE
                ),
                tooltip = "Extra volumes (atlases) to load.",
                inputId = "loader_volume_types",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "[Multi-select, JSON array] Extra volumes (atlases) to load,",
                  "e.g. [\"aparc+aseg\"]; [] for none. The choices depend on",
                  "the subject and refresh when it changes. Read by script",
                  "`load_data`."
                )
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns("loader_surface_types"),
                  label = "Additional surface types",
                  choices = unique(c("smoothwm", "inflated", "white", "pial-outer-smoothed", surface_types0)),
                  selected = surface_types0,
                  multiple = TRUE
                ),
                tooltip = "Extra surface types to load.",
                inputId = "loader_surface_types",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "[Multi-select, JSON array] Extra surface types to load, e.g.",
                  "[\"smoothwm\", \"inflated\"]; [] for none. The choices depend",
                  "on the subject and refresh when it changes. Read by script",
                  "`load_data`."
                )
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns("loader_annot_types"),
                  label = "Additional surface annotations/measurements",
                  choices = as.character(annot_types0),
                  selected = annot_types0,
                  multiple = TRUE
                ),
                tooltip = "Surface annotations or measurements to load.",
                inputId = "loader_annot_types",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "[Multi-select, JSON array] Surface annotations or",
                  "measurements to load, e.g. [\"label/aparc.a2009s.annot\"];",
                  "[] for none. The choices depend on the subject and refresh",
                  "when it changes. Read by script `load_data`."
                )
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns("loader_streamline_types"),
                  label = "Additional streamlines",
                  choices = unique(c("default/*", streamline_types0)),
                  selected = streamline_types0,
                  multiple = TRUE
                ),
                tooltip = "Streamline (fiber tract) bundles to load.",
                inputId = "loader_streamline_types",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "[Multi-select, JSON array] Streamline (fiber tract) bundles",
                  "to load, e.g. [\"alic/*\"] for a group; [] for none. The",
                  "quick analysis needs at least one. The choices depend on the",
                  "subject and refresh when it changes. Read by script",
                  "`load_data`."
                )
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("loader_use_spheres"),
                  label = "Use spheres contacts",
                  value = isTRUE(pipeline$get_settings("use_spheres"))
                ),
                tooltip = "Draw the contacts as spheres instead of electrode shapes.",
                inputId = "loader_use_spheres",
                update = "shiny::updateCheckboxInput",
                description = paste(
                  "[Checkbox, true/false] Draw contacts as spheres instead of",
                  "electrode shapes (prototypes). Read by script `load_data`."
                )
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::numericInput(
                  inputId = ns("loader_override_radius"),
                  label = "Override contact radius (sphere contact must be enabled)",
                  value = NA_real_,
                  step = 0.001,
                  min = 0, max = 10
                ),
                tooltip = "Radius (mm) of the sphere contacts.",
                inputId = "loader_override_radius",
                update = "shiny::updateNumericInput",
                description = paste(
                  "[Number, mm, 0 to 10] Radius of sphere contacts; a value",
                  "above 0 also checks `loader_use_spheres`. Empty keeps the",
                  "radius from the electrode table. Read by script `load_data`."
                )
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("loader_use_template"),
                  label = "Use template brain",
                  value = FALSE
                ),
                tooltip = "Also load the template brain, e.g. to map electrodes to it.",
                inputId = "loader_use_template",
                update = "shiny::updateCheckboxInput",
                description = paste(
                  "[Checkbox, true/false] Also load the template brain, e.g.",
                  "to map electrodes to it. Read by script `load_data`."
                )
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
loader_server <- function(input, output, session, ...) {

  local_reactives <- shiny::reactiveValues()
  local_data <- dipsaus::fastmap2()
  local_data$project_names <- dipsaus::fastmap2()

  output$loader_electrode_tbl_upload_explanation <- shiny::renderUI({
    source_type <- input$loader_electrode_source
    switch(
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
      if (!length(info)) {
        local_reactives$electrode_table <- NULL
        return()
      }
      # check later!
      local_reactives$electrode_table <- ravecore::import_table(info$datapath, header = TRUE)
    }),
    input$loader_electrode_tbl_upload,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_subject_imaging_info <- shiny::reactive({
    project_name <- loader_project$get_sub_element_input()
    subject_code <- loader_subject$get_sub_element_input()
    if (!length(project_name) || !length(subject_code)) {
      return()
    }

    imaging_info <- subject_imaging_info(
      project_name = project_name,
      subject_code = subject_code,
      electrode_table = local_reactives$electrode_table,
      electrode_source_type = paste(input$loader_electrode_source, collapse = "")
    )

    imaging_info

  })

  output$loader_electrode_table <- DT::renderDT({

    coords <- get_subject_imaging_info()

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
      "Coord_x", "Coord_y", "Coord_z", "MNI305_x", "MNI305_y", "MNI305_z",
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
      if (isTRUE(radius > 0)) {
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

  # Runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    description = c(
      "Load the brain (surfaces, volumes, annotations, streamlines, and",
      "electrodes) chosen in the loader (same as clicking 'Load subject')."
    ),
    {
      # gather information from preset UIs

      coords <- get_subject_imaging_info()

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
        streamline_types = input$loader_streamline_types,
        use_spheres = input$loader_use_spheres,
        override_radius = input$loader_override_radius,
        use_template = input$loader_use_template,
        uploaded_source = NULL,
        controllers = list(),
        main_camera = list(),
        shiny_outputId = ns("viewer_ready")
      )

      pipeline$run(
        names = c("loaded_brain_info", "initial_brain_widget"),
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL
      )
      ravepipeline::logger("Data has been loaded loaded")
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(
      title = "Loading in progress",
      text = "Everything takes time. Some might need more patience than others."
    )
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      imaging_info <- get_subject_imaging_info()
      if (!length(imaging_info)) { return() }

      shiny::updateSelectInput(
        session = session,
        inputId = "loader_volume_types",
        choices = imaging_info$volumes,
        selected = input$loader_volume_types
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "loader_surface_types",
        choices = imaging_info$surfaces,
        selected = input$loader_surface_types
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "loader_annot_types",
        choices = imaging_info$annotations,
        selected = input$loader_annot_types
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "loader_streamline_types",
        choices = imaging_info$streamlines,
        selected = input$loader_streamline_types
      )
    }),
    get_subject_imaging_info(), ignoreNULL = TRUE, ignoreInit = FALSE
  )


}
