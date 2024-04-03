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

          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Load subject",
              type = "primary",
              width = "100%"
            )
          ),

          ravedash::flex_group_box(
            title = "Subject",

            shidashi::flex_item(
              loader_subject$ui_func()
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              loader_sync1$ui_func(),
              shiny::br(),
              loader_sync2$ui_func()
            )
          ),

          ravedash::flex_group_box(
            title = "Electrode",

            shidashi::flex_item(

              shiny::selectInput(
                inputId = ns("loader_project_name"),
                label = "Select a project to load electrodes from",
                choices = c("[Auto]", "[Upload]", "[None]", raveio::get_projects(FALSE)),
                selected = pipeline$get_settings("project_name"),
                multiple = FALSE
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] === '[Upload]'",
                                    ns("loader_project_name")),
                shiny::fileInput(
                  inputId = ns("loader_electrode_tbl_upload"),
                  label = "Please upload a valid electrode table in [csv]",
                  multiple = FALSE, accept = ".csv"
                )
              )
            )

          ),

          ravedash::flex_group_box(
            title = "Additional Options",

            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_surface_types"),
                label = "Additional surface types",
                choices = c("white", "smoothwm", "pial-outer-smooth"),
                selected = local({
                  v <- pipeline$get_settings("surface_types")
                  if(!length(v)) {
                    v <- character()
                  }
                  v
                }),
                multiple = TRUE
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
      ),
      shiny::column(
        width = 6L,
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
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  local_reactives <- shiny::reactiveValues()
  local_data <- dipsaus::fastmap2()
  local_data$project_names <- dipsaus::fastmap2()

  get_projects <- function(subject_code) {
    pnames <- local_data$project_names

    projects <- NULL

    if(isTRUE(pnames$`@has`(subject_code))) {
      projects <- pnames[[subject_code]]
    } else {
      projects <- get_projects_with_scode(subject_code)
      if(length(projects)) {
        local_data$project_names[[subject_code]] <- projects
      }
    }
    projects
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      subject_code <- loader_subject$get_sub_element_input()

      projects <- get_projects(subject_code)

      choices <- c("[Auto]", "[Upload]", "[None]", projects)
      shiny::updateSelectInput(
        session = session,
        inputId = "loader_project_name",
        choices = choices,
        selected = input$loader_project_name %OF% choices
      )

    }),
    loader_subject$get_sub_element_input(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      info <- input$loader_electrode_tbl_upload
      if(!length(info)) { return() }
      local_reactives$electrode_table <- utils::read.csv(info$datapath, header = TRUE)
    }),
    input$loader_electrode_tbl_upload,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_electrode_table <- shiny::reactive({
    project_name <- input$loader_project_name
    subject_code <- loader_subject$get_sub_element_input()
    if(!length(project_name) || !length(subject_code)) {
      return()
    }

    if(identical(project_name, "[None]")) { return() }

    re <- NULL
    rave_path <- raveio::raveio_getopt("data_dir")

    if(identical(project_name, "[Upload]")) {
      re <- local_reactives$electrode_table
    } else {

      if(identical(project_name, "[Auto]")) {
        all_projects <- raveio::get_projects()
        dirs <- file.path(rave_path, all_projects, subject_code)
        all_projects <- all_projects[dir.exists(dirs)]
        if(!length(all_projects)) { return() }
        project_name <- all_projects[[1]]
      }

      re <- raveio::load_meta2(meta_type = "electrodes",
                               project_name = project_name,
                               subject_code = subject_code)
    }

    return(re)

  })


  get_brain_path <- shiny::reactive({
    project_name <- input$loader_project_name
    subject_code <- loader_subject$get_sub_element_input()
    if(!length(project_name) || !length(subject_code)) {
      return()
    }

    rave_path <- raveio::raveio_getopt("data_dir")

    dir <- file.path(rave_path, project_name, subject_code)
    raveio::RAVESubject$new()

    if(identical(project_name, "[None]")) { return() }

    re <- NULL


    if(identical(project_name, "[Upload]")) {
      re <- local_reactives$electrode_table
    } else {

      if(identical(project_name, "[Auto]")) {
        all_projects <- raveio::get_projects()
        dirs <- file.path(rave_path, all_projects, subject_code)
        all_projects <- all_projects[dir.exists(dirs)]
        if(!length(all_projects)) { return() }
        project_name <- all_projects[[1]]
      }

      re <- raveio::load_meta2(meta_type = "electrodes",
                               project_name = project_name,
                               subject_code = subject_code)
    }

    return(re)

  })

  output$loader_electrode_table <- DT::renderDT({

    tbl <- get_electrode_table()

    shiny::validate(
      shiny::need(is.data.frame(tbl),
                  message = "No electrode table found. No electrodes will be generated")
    )

    required_names <- c("Electrode", "Coord_x", "Coord_y", "Coord_z", "Label")
    nms <- names(tbl)

    shiny::validate(
      shiny::need(
        all(required_names %in% nms),
        message = paste(
          "A valid electrode table in RAVE must contain the following columns:",
          paste(required_names, collapse = ", ")
        ))
    )

    re <- DT::datatable(tbl, class = "display nowrap compact",
                        selection = "none")

    digit_nms <- c(
      'Coord_x', 'Coord_y', 'Coord_z', "MNI305_x", "MNI305_y", "MNI305_z",
      "MNI152_x", "MNI152_y", "MNI152_z", "T1R", "T1A", "T1S"
    )
    digit_nms <- digit_nms[digit_nms %in% nms]

    re <- DT::formatRound(re, columns = digit_nms, digits = 2)
    re

  })



  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs

      # Save the variables into pipeline settings file
      pipeline$save_data(
        data = get_electrode_table(),
        name = "suggested_electrode_table",
        overwrite = TRUE
      )
      pipeline$set_settings(
        subject_code = input$loader_subject_code,
        project_name = input$loader_project_name,
        surface_types = input$loader_surface_types,
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
