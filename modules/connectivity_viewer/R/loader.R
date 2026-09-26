# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {
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

  template_strings <- all_templates[template_exists]

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(
        width = 6L, offset = 3L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          footer = shiny::tagList(
            ravedash::load_data_button(label = "Load template", width = "100%")
          ),

          ravedash::flex_group_box(
            title = "Template brain",

            shidashi::flex_item(
              shiny::selectInput(ns("loader_selected_template"), "Available templates",
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
          #       choices = c("[Auto]", "[Upload]", "[None]", ravecore::get_projects(FALSE)),
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
                choices = c("sphere.reg", "inflated", "white", "smoothwm", "pial-outer-smoothed"),
                selected = local({
                  v <- pipeline$get_settings("surface_types")
                  if (!length(v)) {
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
loader_server <- function(input, output, session, ...) {

  local_reactives <- shiny::reactiveValues()
  local_data <- dipsaus::fastmap2()
  local_data$project_names <- dipsaus::fastmap2()

  get_projects <- function(subject_code) {
    pnames <- local_data$project_names

    projects <- NULL

    if (isTRUE(pnames$`@has`(subject_code))) {
      projects <- pnames[[subject_code]]
    } else {
      projects <- get_projects_with_scode(subject_code)
      if (length(projects)) {
        local_data$project_names[[subject_code]] <- projects
      }
    }
    projects
  }

  # Runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    {
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

      pipeline$run(
        names = c("template_details"),
        scheduler = "none",
        type = "vanilla",
        callr_function = NULL
      )

      ravepipeline::logger("Data has been loaded!")
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(
      title = "Loading in progress",
      text = "Loading template brain..."
    )
  )



}
