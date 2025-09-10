# UI components for loader
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

        ravedash::flex_group_box(
          title = "Electrodes and Epoch",

          shidashi::flex_item(
            loader_electrodes$ui_func()
          ),

          shidashi::flex_item(
            shiny::selectInput(
              inputId = ns("loader_epoch"),
              label = "Stimulation epoch",
              choices = "[None]",
              selected = "[None]"
            )
          )
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


# Server functions for loader
loader_server <- function(input, output, session, ...){

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code",
          "loader_electrode_text"
        )
      )

      epoch_name <- input$loader_epoch
      if(length(epoch_name) == 1 && epoch_name != "[None]") {
        settings$epoch_name <- epoch_name
      } else {
        settings$epoch_name <- NULL
      }

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      # --------------------- Run the pipeline! ---------------------

      # Pop up alert to prevent user from making any changes (auto_close=FALSE)
      # This requires manually closing the alert window
      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "Everything takes time. Some might need more patience than others."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      tryCatch(
        {
          res <- pipeline$run(names = "loaded_signals")

          Sys.sleep(0.5)
          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravepipeline::logger("Data has been loaded loaded")

          # Close the alert
          dipsaus::close_alert2()
        },
        error = function(e) {

          # Close the alert
          Sys.sleep(0.5)
          dipsaus::close_alert2()

          # Immediately open a new alert showing the error messages
          dipsaus::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the raw-voltage data:\n\n",
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


  shiny::bindEvent(
    ravedash::safe_observe({
      project_name <- loader_project$current_value
      subject_code <- loader_subject$current_value
      if(length(project_name) == 1 && length(subject_code) == 1) {
        subject <- ravecore::new_rave_subject(
          project_name = project_name, subject_code = subject_code, strict = FALSE)
        epoch_names <- c("[None]", subject$epoch_names)
        shiny::updateSelectInput(
          session = session,
          inputId = "loader_epoch",
          choices = epoch_names
        )
      }
    }),
    loader_project$current_value,
    loader_subject$current_value,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


}
