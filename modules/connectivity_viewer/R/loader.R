# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){
  all_templates <- names(threeBrain::available_templates())

  current_rave_template <- ravepipeline::raveio_getopt("threeBrain_template_subject")
  template_root_path <- threeBrain::default_template_directory()

  template_exists <- sapply(all_templates, function(at) {
    path <- file.path(template_root_path, at)

    return(file.exists(path))
  })


  # template_strings <- sapply(all_templates, function(at) {
  #   path <- file.path(template_root_path, at)
  #   if(file.exists(path)) {
  #     return (at)
  #   }
  #
  #   print(at)
  #
  #   paste0(at, ' (req Download)')
  # })
  #
  # str_order = order(seq_along(template_strings) +
  #                     ifelse(stringr::str_detect(template_strings, stringr::fixed('req Download')), 100, 0)
  # )

  template_strings = all_templates[template_exists]

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(
        width = 6L, offset = 3L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Load template",
              type = "primary",
              width = "100%"
            )
          ),

          ravedash::flex_group_box(
            title = "Template brain",

            shidashi::flex_item(
              shiny::selectInput(ns('loader_selected_template'), 'Available templates',
                                 # choices = template_strings,
                                 choices = unname(template_strings),
                                 selected = current_rave_template)
            )#,
            # shidashi::flex_break(),
            # shidashi::flex_item(
            #   # shiny::down
            #   # loader_sync1$ui_func(),
            #   # shiny::br(),
            #   # loader_sync2$ui_func()
            # )
          ),

          # ravedash::flex_group_box(
          #   title = "Electrode",
          #
          #   shidashi::flex_item(
          #
          #     shiny::selectInput(
          #       inputId = ns("loader_project_name"),
          #       label = "Select a project to load electrodes from",
          #       choices = c("[Auto]", "[Upload]", "[None]", raveio::get_projects(FALSE)),
          #       selected = pipeline$get_settings("project_name"),
          #       multiple = FALSE
          #     ),
          #     shiny::conditionalPanel(
          #       condition = sprintf("input['%s'] === '[Upload]'",
          #                           ns("loader_project_name")),
          #       shiny::fileInput(
          #         inputId = ns("loader_electrode_tbl_upload"),
          #         label = "Please upload a valid electrode table in [csv]",
          #         multiple = FALSE, accept = ".csv"
          #       )
          #     )
          #   )
          #
          # ),

          ravedash::flex_group_box(
            title = "Select surfaces",

            shidashi::flex_item(
              shiny::p("Pial surface included by default."),
              shiny::selectInput(
                inputId = ns("loader_surface_types"),
                label = "Additional surface types",
                choices = c("sphere.reg", "inflated", "white", "smoothwm", "pial-outer-smooth"),
                selected = local({
                  v <- pipeline$get_settings("surface_types")
                  if(!length(v)) {
                    v <- character()
                  }
                  v
                }),
                multiple = TRUE
              )
            )
            # shidashi::flex_break(),
            # shidashi::flex_item(
            #   shiny::checkboxInput(
            #     inputId = ns("loader_use_template"),
            #     label = "Use template brain",
            #     value = FALSE
            #   )
            # )

          )

          # shiny::textOutput(ns("loader_short_message"))
        )
      ),
      shiny::column(
        width = 6L,
        # ravedash::input_card(
        #   title = "Table preview",
        #   tools = list(
        #     shidashi::card_tool(widget = "maximize")
        #   ),
        #
        #   shiny::div(
        #     class = "fill-width overflow-x-scroll margin-bottom-10",
        #     DT::DTOutput(outputId = ns("loader_electrode_table"), width = "100%")
        #   )
        # )
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

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs

      # Save the variables into pipeline settings file
      # pipeline$save_data(
      #   data = get_electrode_table(),
      #   name = "suggested_electrode_table",
      #   overwrite = TRUE
      # )
      pipeline$set_settings(
        # subject_code = input$loader_subject_code,
        # project_name = input$loader_project_name,
        surface_types = input$loader_surface_types,
        selected_template = input$loader_selected_template,
        # use_template = input$loader_use_template,
        # uploaded_source = NULL,
        controllers = list(),
        main_camera = list(),
        shiny_outputId = ns("viewer_ready")
      )

      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "Loading template brain..."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      res <- pipeline$run(
        as_promise = TRUE,
        names = c("template_details"),
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL
      )

      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())

          ravedash::logger("Data has been loaded!")

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
