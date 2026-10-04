# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {
  pre_downsample <- pipeline$get_settings("pre_downsample", default = NA)
  repository_datatype <- pipeline$get_settings("repository_datatype", default = "raw-voltage")

  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = "max-width:1444px;",
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
            ),
            shiny::tags$small(
              style = "font-style:italic;",
              shiny::textOutput(outputId = ns("loader_repository_datatype_info"), inline = TRUE)
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
          ravedash::load_data_button(label = "Load subject", width = "100%")
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
loader_server <- function(input, output, session, ...) {

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
      if (!nrow(tbl)) { return() }
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

  output$loader_repository_datatype_info <- shiny::renderText({
    dtype <- paste(input$loader_repository_datatype, collapse = "")
    switch(
      dtype,
      "voltage" = {
        "Notch filter & re-referenced"
      },
      "raw-voltage" = {
        "Reference will be ignored"
      },
      {
        ""
      }
    )
  })

  output$loader_pre_downsample_info <- shiny::renderText({

    sample_rates <- local_reactives$sample_rates
    if (!length(sample_rates)) { return("This subject has no data imported") }

    srate_info <- paste(
      sprintf("%s (%g Hz)", names(sample_rates), unlist(sample_rates)),
      collapse = ", "
    )

    if (length(sample_rates$LFP)) {
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

  # Runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    {
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
      if (!is.na(pre_downsample)) {
        if (pre_downsample < 1 || abs(pre_downsample - round(pre_downsample)) > 0.001) {
          stop("Down-sampling rate must be an positive integer")
        }
        pre_downsample <- round(pre_downsample)
        if (pre_downsample == 1) {
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

      # Run the pipeline target `repository`
      pipeline$run(
        scheduler = "none",
        type = "vanilla",
        names = "repository",
        return_values = FALSE
      )

      # Set epoch and/or reference as default
      repo <- pipeline$read("repository")
      if (default_reference) {
        repo$subject$set_default("reference_name", repo$reference_name)
      }

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



}
