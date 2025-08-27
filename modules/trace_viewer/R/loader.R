# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){
  pre_downsample <- pipeline$get_settings("pre_downsample", default = NA)
  repository_datatype <- pipeline$get_settings("repository_datatype", default = "raw-voltage")

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
          title = "Electrodes and Reference",

          shidashi::flex_item(
            shiny::selectInput(
              inputId = ns("loader_repository_datatype"),
              label = "Data type",
              choices = c("voltage", "raw-voltage"),
              selected = repository_datatype
            )
          ),
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

        ravedash::flex_group_box(
          title = "Preprocess",

          shidashi::flex_item(

            shiny::numericInput(
              inputId = ns("loader_pre_downsample"),
              label = "Down-sample signals",
              min = 0L, step = 1L,
              width = "100%", value = pre_downsample
            ),

            shiny::p(shiny::tags$small(shiny::tags$span(
              style = "font-style:italic",
              shiny::textOutput(
                outputId = ns("loader_pre_downsample_info"),
                inline = TRUE
              )
            )))
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

  get_subject <- loader_subject$get_tool("get_subject")

  local_reactives <- shiny::reactiveValues(
    sample_rates = NULL
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if (!loader_subject$sv$is_valid()) {
        return()
      }
      subject <- get_subject()
      if (is.null(subject)) {
        return()
      }

      tbl <- data.frame(
        type = subject$electrode_types,
        sample_rate = subject$raw_sample_rates
      )
      if(!nrow(tbl)) { return() }
      tbl <- unique(tbl)

      local_reactives$sample_rates <- structure(
        names = tbl$type,
        as.list(tbl$sample_rate)
      )
    }),
    loader_project$current_value,
    loader_subject$current_value,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  output$loader_pre_downsample_info <- shiny::renderText({

    sample_rates <- local_reactives$sample_rates
    if(!length(sample_rates)) { return("This subject has no data imported") }

    srate_info <- paste(
      sprintf("%s (%g Hz)", names(sample_rates), unlist(sample_rates)),
      collapse = ", "
    )

    if(length(sample_rates$LFP)) {
      srate <- sample_rates$LFP[[1]]
      suggested_dsample <- floor(srate / 400)
      dsample_suggestion <- sprintf("If you want to analyze LFP signals with frequency < 200 Hz, you might want to down-sample the channels to improve speed (for example, by %d). If you don't want to down-sample, leave this field blank.", suggested_dsample)
    } else {
      dsample_suggestion <- NULL
    }

    re <- c(sprintf("Sampling rate(s) found for this subject: %s.", srate_info),
            dsample_suggestion)
    paste(re, collapse = " ")
  })

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
          "loader_reference_name"
        )
      )
      # add your own input values to the settings file
      pre_downsample <- input$loader_pre_downsample
      if(!is.na(pre_downsample)) {
        if(pre_downsample < 1 || abs(pre_downsample - round(pre_downsample)) > 0.001) {
          stop("Down-sampling rate must be an positive integer")
        }
        pre_downsample <- round(pre_downsample)
        if(pre_downsample == 1) {
          pre_downsample <- NA
        }
      }

      # Save the variables into pipeline settings file
      pipeline$set_settings(
        repository_datatype = input$loader_repository_datatype,
        pre_downsample = as.integer(pre_downsample),
        .list = settings
      )

      # Check if user has asked to set the epoch & reference to be the default
      default_reference <- isTRUE(loader_reference$get_sub_element_input("default"))

      # --------------------- Run the pipeline! ---------------------

      # Pop up alert to prevent user from making any changes (auto_close=FALSE)
      # This requires manually closing the alert window
      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "Everything takes time. Some might need more patience than others."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      # Run the pipeline target `repository`
      # Use `as_promise=TRUE` to make result as a promise
      res <- pipeline$run(
        as_promise = TRUE,
        scheduler = "none",
        type = "vanilla",
        names = "repository",
        return_values = FALSE
      )

      # The `res` contains a promise that might not have finished yet,
      # so register functions to run when the promise is resolved
      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          # Set epoch and/or reference as default
          repo <- pipeline$read("repository")
          if(default_reference) {
            repo$subject$set_default("reference_name", repo$reference_name)
          }

          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravepipeline::logger("Data has been loaded loaded")

          # Close the alert
          dipsaus::close_alert2()
        },


        # this is what should happen when pipeline fails
        onRejected = function(e){

          # Close the alert
          Sys.sleep(0.5)
          dipsaus::close_alert2()

          # Immediately open a new alert showing the error messages
          dipsaus::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the subject data:\n\n",
              paste(e$message, collapse = "\n")
            ),
            icon = "error",
            danger_mode = TRUE,
            auto_close = FALSE
          )
        }
      )
    }, error_wrapper = "alert"),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )



}
