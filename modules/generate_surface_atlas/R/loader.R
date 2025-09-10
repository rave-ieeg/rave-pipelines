# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",

    shiny::fixedRow(

      shiny::column(
        width = 6L, offset = 3L,

        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          ravedash::flex_group_box(
            title = "Project and Template",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_template"),
                label = "Template surface",
                choices = c("fsaverage", "cvs_avg35_inMNI152"),
                selected = "fsaverage",
                width = "100%"
              )
            )
          ), # flex-box

          ravedash::flex_group_box(
            title = "Value Source",

            shidashi::flex_item(
              dipsaus::fancyFileInput(
                inputId = ns("loader_file"),
                label = "Value file",
                width = "100%",
                size = 's'
              )
            ),
            shidashi::flex_break(),

            shidashi::flex_item(
              shiny::actionLink(inputId = ns("loader_file_example"), label = "Display an example of the value file.")
            )
          ),


          footer = shiny::tagList(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Start!",
              type = "primary",
              width = "100%"
            )
          )

        ) # .card

      ),  # col-4

    ) # .row
  )  # .container

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  sample_table <- data.frame(
    Subject = c("Subject001", "Subject001", "Subject002", "Subject002", "Subject003", NA),
    Electrode = c(1, 2, 1, 5, 2, NA),
    `audiovisual_multisensory_score` = c(12, 34, 11, 3, 15, NA)
  )

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe(error_wrapper = "alert", {
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name"
        )
      )
      # add the rest input values to the settings file
      settings$template_name <- input$loader_template

      if(length(input$loader_file$datapath) != 1) {
        stop("Please provide a value table.")
      }
      settings$value_table <- data.table::fread(input$loader_file$datapath)

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)


      dipsaus::shiny_alert2(
        title = "Loading...",
        text = "Preparing the data. If this is the first time that you use this template brain, some downloads from the internet (GitHub) may be needed.",
        auto_close = FALSE,
        buttons = FALSE,
        icon = "info"
      )
      Sys.sleep(0.5)

      tryCatch(
        {
          pipeline$run(names = c("cleaned_inputs"))
          Sys.sleep(0.5)
          dipsaus::close_alert2()
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravepipeline::logger("Data has been loaded loaded")

        },
        error = function(e) {
          Sys.sleep(0.5)
          dipsaus::close_alert2()
          stop(e)
        }
      )

    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::showModal(shiny::modalDialog(
        title = "Example value table",
        size = "l",
        easyClose = TRUE,
        shiny::div(
          shiny::p("Please prepare a comma-separated value (csv) table with the following case-sensitive columns: 'Subject', 'Electrode', followed by the atlas name (no space, only text, digits, and unders_cores). For example: "),
          shiny::tableOutput(ns('loader_file_example_table')),
          shiny::p(
            "The new atlases will be created from these columns. ",
            shiny::downloadLink(outputId = ns("loader_file_example_table_download"), label = "Click here to download this example.")
          )
        )
      ))
    }),
    input$loader_file_example,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$loader_file_example_table_download <- shiny::downloadHandler(
    filename = "example_value_table.csv",
    content = function(con) {
      utils::write.csv(x = sample_table[1:5, ], file = con, row.names = FALSE)
    }
  )
  output$loader_file_example_table <- shiny::renderTable(
    {
      sample_table
    },
    striped = TRUE,
    spacing = "xs",
    bordered = TRUE,
    width = "100%",
    rownames = FALSE,
    colnames = TRUE,
    na = "...",
    digits = 0
  )




}
