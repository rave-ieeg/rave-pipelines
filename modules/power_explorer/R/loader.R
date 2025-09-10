
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = 'max-width:1444px;',
    input_ui = {
      # project & subject
      ravedash::input_card(
        title = "Data Selection",
        class_header = "",

        ravedash::flex_group_box(
          title = "Project and Subject",

          shidashi::flex_item(
            loader_project$ui_func()
          ),
          shidashi::flex_item(
            loader_subject$ui_func()
          )
        ),

        loader_epoch$ui_func(),

        ravedash::flex_group_box(
          title = "Electrodes and Reference",

          loader_reference$ui_func(),
          shidashi::flex_break(),
          shidashi::flex_item(
            loader_electrodes$ui_func()
          ),
          shidashi::flex_item(
            shiny::fileInput(
              inputId = ns("loader_mask_file"),
              label = "or Mask file"
            ))
        ),

        footer = shiny::tagList(
          dipsaus::actionButtonStyled(
            inputId = ns("loader_ready_btn"),
            label = "Load subject",
            type = "primary",
            width = "100%"
          )
        )

      )
    },
    output_ui = {
      ravedash::output_card(
        title = "3D Viewer",
        class_body = "no-padding min-height-650 height-650",
        loader_viewer$ui_func()
      )
    }
  )

}

loader_server <- function(input, output, session, ...){

  # list2env(list(session = session, input = input), envir=globalenv())

  # Add validator
  # session <- shiny::MockShinySession$new()
  # loader_project$server_func(input, output, session)
  # loader_subject$server_func(input, output, session)
  # loader_epoch$server_func(input, output, session)
  # loader_electrodes$server_func(input, output, session)
  # loader_reference$server_func(input, output, session)
  # loader_viewer$server_func(input, output, session)
  loader_validator_subject_code <- loader_subject$sv


  shiny::bindEvent(
    ravedash::safe_observe({

      # gather information
      settings <- dipsaus::fastmap2()

      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code",
          "loader_electrode_text",
          "loader_epoch_name",
          "loader_reference_name"
        ),
        map = settings
      )
      pipeline$set_settings(.list = settings)

      default_epoch <- isTRUE(loader_epoch$get_sub_element_input("default"))
      default_reference <- isTRUE(loader_reference$get_sub_element_input("default"))

      # Run the pipeline!
      tarnames <- pipeline$target_table$Names

      count <- length(tarnames) + length(dipsaus::parse_svec(loader_electrodes$current_value)) + 4

      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "Everything takes time. Some might need more patience than others."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      res <- pipeline$run(
        as_promise = TRUE,
        names = "repository",
        scheduler = "none",
        type = "smart",  # parallel
        # async = TRUE,
        callr_function = NULL,
        progress_quiet = TRUE
      )
      res$promise$then(
        onFulfilled = function(e){
          if(default_epoch || default_reference){
            repo <- pipeline$read("repository")
            if(default_epoch){
              repo$subject$set_default("epoch_name", repo$epoch_name)
            }
            if(default_reference) {
              repo$subject$set_default("reference_name", repo$reference_name)
            }
          }

          ravedash::fire_rave_event('data_changed', Sys.time())
          ravepipeline::logger("Data has been loaded loaded")
          dipsaus::close_alert2()
        },
        onRejected = function(e){
          dipsaus::close_alert2()
          dipsaus::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the power data:\n\n",
              paste(e$message, collapse = "\n")
            ),
            icon = "error",
            danger_mode = TRUE,
            auto_close = FALSE
          )
        }
      )
    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE)



}
