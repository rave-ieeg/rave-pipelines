# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(width = 3L),
      shiny::column(
        width = 6L,
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
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              loader_sync1$ui_func(),
              shiny::br(),
              loader_sync2$ui_func(),
              shiny::br()
            )
          ),

          ravedash::flex_group_box(
            title = "Presets",
            loader_reference$ui_func()
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
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      settings <- tryCatch({
        component_container$collect_settings(
          ids = c(
            "loader_project_name",
            "loader_subject_code",
            "loader_reference_name"
          )
        )
      }, error = function(e) {
        ravedash::error_alert("Please check your inputs and make sure the Notch filters and Wavelet transform have been applied.")
        class(e) <- c("rave_muffled", class(e))
        stop(e)
      })

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      res <- pipeline$run(
        as_promise = TRUE,
        names = c("reference_table_initial", "voltage_data"),
        return_values = FALSE
      )

      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          dipsaus::close_alert2()

          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravepipeline::logger("Data has been loaded loaded")

          # Save session-based state: project name & subject code
          ravedash::session_setopt(
            project_name = settings$project_name,
            subject_code = settings$subject_code
          )

        },


        # this is what should happen when pipeline fails
        onRejected = ravedash::error_alert
      )
    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
