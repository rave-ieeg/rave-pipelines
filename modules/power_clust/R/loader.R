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
          "loader_electrode_text",
          "loader_epoch_name",
          "loader_reference_name"
        )
      )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      # Check if user has asked to set the epoch & reference to be the default
      default_epoch <- isTRUE(loader_epoch$get_sub_element_input("default"))
      default_reference <- isTRUE(loader_reference$get_sub_element_input("default"))

      # --------------------- Run the pipeline! ---------------------

      # Pop up alert to prevent user from making any changes (auto_close=FALSE)
      # This requires manually closing the alert window
      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "A good meal takes time to cook."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      # Run the pipeline target `repository`
      tryCatch(
        {
          repo <- pipeline$run(
            as_promise = FALSE,
            names = "repository",
            return_values = TRUE
          )
          if(default_epoch){
            repo$subject$set_default("epoch_name", repo$epoch_name)
          }
          if(default_reference) {
            repo$subject$set_default("reference_name", repo$reference_name)
          }

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
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )



}
