# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  bids_datasets <- sort(get_BIDS_datasets())

  shiny::div(
    class = "container",
    shiny::fixedRow(

      shiny::column(
        width = 2L
      ),
      shiny::column(
        width = 8L,

        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          # BIDS dataset, subject
          shiny::fluidRow(
            shiny::column(
              width = 6L,
              shiny::selectInput(
                inputId = ns("loader_bids_dataset"),
                label = "BIDS dataset",
                choices = bids_datasets,
                selected = pipeline$get_settings("BIDS_dataset", constraint = bids_datasets)
              )
            ),

            shiny::column(
              width = 6L,
              shiny::selectInput(
                inputId = ns("loader_bids_subject"),
                label = "BIDS subject",
                choices = character(0L),
                selected = NULL
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12L,
              shiny::tags$label(
                class = "control-label",
                "Import settings"
              ),
              shiny::checkboxInput(
                inputId = ns('loader_conf_allsessions'),
                label = "Import all BIDS sessions and nested runs",
                value = TRUE,
                width = "100%"
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s']!==true", ns("loader_conf_allsessions")),
                class = "row",
                shiny::column(
                  width = 12L,

                  shiny::div(
                    style = "width: 20px; height: 1px; float: left"
                  ),
                  shiny::div(
                    style = "width: 100%",
                    shiny::fluidRow(
                      shiny::column(
                        width = 12L,
                        shiny::selectInput(
                          inputId = ns("loader_subject_session"),
                          label = "Choose BIDS sessions to search",
                          choices = character(),
                          multiple = TRUE
                        )
                      ),

                      shiny::column(
                        width = 12L,
                        shiny::selectInput(
                          inputId = ns("loader_subject_runs"),
                          label = "Choose BIDS runs to import",
                          choices = character(),
                          multiple = TRUE
                        )
                      )
                    )
                  ),
                  shiny::hr()
                )
              ),
              shiny::checkboxInput(
                inputId = ns('loader_conf_overwrite'),
                label = "Overwrite existing imports (otherwise only new runs will be imported)",
                value = isTRUE(pipeline$get_settings("overwrite")),
                width = "100%"
              ),
              shiny::checkboxInput(
                inputId = ns('loader_conf_backup'),
                label = "Backup existing imports if the blocks exist",
                value = isTRUE(pipeline$get_settings("backup")),
                width = "100%"
              )
            )
          ),


          footer = shiny::div(
            dipsaus::actionButtonStyled(
              inputId = ns("loader_do_import"),
              label = "Import from BIDS",
              type = "primary",
              class = "float-right"
            )
          )

        )
      )

    )
  )
}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  # Validator for `loader_bids_dataset` & `loader_bids_subject`
  sv_loader_subject <- shinyvalidate::InputValidator$new(session = session)
  sv_loader_subject$add_rule(
    inputId = "loader_bids_dataset",
    rule = function(value) {
      if(checkmate::test_character(
        value,
        len = 1L,
        min.chars = 1L,
        pattern = "^[a-zA-Z0-9]",
        any.missing = FALSE,
        null.ok = FALSE
      )) {
        return()
      }
      return("Invalid BIDS dataset name. The dataset must be non-empty and must start with letters (a-zA-Z) or digits (0-9)")
    }
  )
  sv_loader_subject$add_rule(
    inputId = "loader_bids_subject",
    rule = function(value) {
      if(checkmate::test_character(
        value,
        len = 1L,
        min.chars = 1L,
        pattern = "^sub-",
        any.missing = FALSE,
        null.ok = FALSE
      )) {
        return()
      }
      return("Invalid BIDS subject name. The dataset must be non-empty and must start with keyword \"sub-\"")
    }
  )
  sv_loader_subject$enable()

  # Update `loader_bids_subject` according to input `loader_bids_dataset`
  shiny::bindEvent(
    ravedash::safe_observe({
      dset <- input$loader_bids_dataset
      if(length(dset) != 1 || is.na(dset) || !nzchar(dset)) {
        return()
      }
      choices <- get_BIDS_subject(dset)
      selected <- pipeline$get_settings("BIDS_subject", constraint = choices)

      shiny::updateSelectInput(
        session = session, inputId = "loader_bids_subject",
        choices = choices, selected = selected
      )

    }),
    input$loader_bids_dataset,
    ignoreInit = FALSE, ignoreNULL = TRUE
  )

  # Update `loader_subject_session` list
  shiny::bindEvent(
    ravedash::safe_observe({
      if(!isTRUE(sv_loader_subject$is_valid())) { return() }
      sessions <- get_BIDS_subject_sessions(
        BIDS_dataset = input$loader_bids_dataset,
        BIDS_subject = input$loader_bids_subject
      )
      choices <- c("[All sessions]", sessions)
      shiny::updateSelectInput(
        session = session,
        inputId = "loader_subject_session",
        choices = choices,
        selected = shiny::isolate(input$loader_subject_session) %OF% choices
      )
    }),
    input$loader_bids_dataset,
    input$loader_bids_subject,
    sv_loader_subject$is_valid()
  )

  # Update `loader_subject_runs` list
  shiny::bindEvent(
    ravedash::safe_observe({
      if(!isTRUE(sv_loader_subject$is_valid())) { return() }

      BIDS_root <- ravepipeline::raveio_getopt("bids_data_dir")

      BIDS_sessions <- input$loader_subject_session
      if(!length(BIDS_sessions) || "[All sessions]" %in% BIDS_sessions) {
        BIDS_sessions <- NULL
      } else {
        BIDS_sessions <- BIDS_sessions[grepl("^ses-", BIDS_sessions)]
      }

      runs <- find_bids_runs(
        subject_path = file.path(BIDS_root, input$loader_bids_dataset, input$loader_bids_subject),
        BIDS_subject = input$loader_bids_subject,
        BIDS_sessions = BIDS_sessions
      )$runs
      current_selections <- shiny::isolate(input$loader_subject_runs)
      current_selections <- current_selections[current_selections %in% runs]
      shiny::updateSelectInput(
        session = session,
        inputId = "loader_subject_runs",
        choices = runs,
        selected = current_selections
      )
    }),
    sv_loader_subject$is_valid(),
    input$loader_bids_dataset,
    input$loader_bids_subject,
    input$loader_subject_session
  )



  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      BIDS_root <- ravepipeline::raveio_getopt("bids_data_dir")
      BIDS_dataset <- input$loader_bids_dataset
      BIDS_subject <- input$loader_bids_subject
      import_all_sessions <- input$loader_conf_allsessions

      if( isTRUE(import_all_sessions) ) {
        BIDS_sessions <- NULL
        BIDS_runs <- find_bids_runs(
          subject_path = file.path(BIDS_root, BIDS_dataset, BIDS_subject),
          BIDS_subject = BIDS_subject,
          BIDS_sessions = BIDS_sessions
        )$runs
      } else {
        BIDS_sessions <- input$loader_subject_session
        if(!length(BIDS_sessions) || "[All sessions]" %in% BIDS_sessions) {
          BIDS_sessions <- NULL
        } else {
          BIDS_sessions <- BIDS_sessions[grepl("^ses-", BIDS_sessions)]
        }
        BIDS_runs <- input$loader_subject_runs
      }

      # Save the variables into pipeline settings file
      overwrite <- isTRUE(input$loader_conf_overwrite)
      backup <- isTRUE(input$loader_conf_backup)

      pipeline$set_settings(
        BIDS_dataset = BIDS_dataset,
        BIDS_subject = BIDS_subject,
        BIDS_sessions = BIDS_sessions,
        BIDS_runs = BIDS_runs,
        overwrite = overwrite,
        backup = backup
      )

      migrate_plan <- pipeline$run(names = "migrate_plan",
                                   scheduler = "none",
                                   type = "vanilla",
                                   async = FALSE)
      plan_table <- migrate_plan[migrate_plan$Planned, ]
      not_planned <- migrate_plan[!migrate_plan$Planned, ]

      if(nrow(plan_table)) {
        confirm_btn <- dipsaus::actionButtonStyled(
          inputId = ns("loader_confirm_import"),
          label = "Confirm import",
          icon = ravedash::shiny_icons$rocket,
          type = "primary"
        )
      } else {
        confirm_btn <- NULL
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "BIDS to RAVE: Migration Plan",
          size = "l",
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::modalButton("Dismiss"),
            confirm_btn
          ),
          shiny::p(
            "Total ", nrow(migrate_plan), " BIDS runs have been scheduled. ",
            nrow(plan_table), " runs will be migrated to RAVE raw directory. ",
            "Please CAREFULLY check the migration plans below before importing."
          ),
          shiny::div(
            class = "padding-left-20",
            shiny::tags$details(
              open = "",
              shiny::tags$summary("BIDS runs planned: ", nrow(plan_table),
                                  shiny::tags$small("(click me to fold/unfold)", class = "text-secondary")),
              shiny::tagList(lapply(seq_len(nrow(plan_table)), function(ii) {
                is_overwrite <- plan_table$BlockExist[[ii]]
                if( is_overwrite ) {
                  if( backup ) {
                    op_msg <- "This BIDS run replace an existing RAVE block; the existing files will be renamed"
                    op_col <- "#FFA500"
                  } else {
                    op_msg <- "This BIDS run replace an existing RAVE block; the existing files will be overwritten"
                    op_col <- "#FF4500"
                  }
                } else {
                  op_msg <- "This BIDS run will be imported as a new RAVE block"
                  op_col <- "#1874CD"
                }

                shiny::tags$dl(
                  shiny::tags$dt(
                    plan_table$Block[[ii]]
                  ),
                  shiny::tags$dd(
                    "BIDS path: ",
                    plan_table$Source[[ii]],
                    shiny::br(),
                    shiny::tags$small(
                      sprintf("(*%s)", op_msg),
                      style = sprintf("color: %s;", op_col)
                    )
                  )
                )
              }))
            ),
            shiny::tags$details(
              shiny::tags$summary("BIDS runs NOT planned: ", nrow(not_planned),
                                  shiny::tags$small("(click me to fold/unfold)", class = "text-secondary")),
              shiny::tagList(lapply(seq_len(nrow(not_planned)), function(ii) {
                src_missing <- !not_planned$SourceExist[[ii]]
                dst_exists <- not_planned$BlockExist[[ii]]
                if( src_missing ) {
                  op_msg <- "Cannot find any data related"
                  op_col <- "#FFA500"
                } else {
                  if( dst_exists ) {
                    op_msg <- "An existing RAVE block with the same name exists and overwriting is disabled by the user"
                    op_col <- "#FF4500"
                  } else {
                    op_msg <- "This BIDS run is not planned by the user"
                    op_col <- "#1874CD"
                  }
                }

                shiny::tags$dl(
                  shiny::tags$dt(
                    not_planned$Block[[ii]]
                  ),
                  shiny::tags$dd(
                    "BIDS path: ",
                    not_planned$Source[[ii]],
                    shiny::br(),
                    shiny::tags$small(
                      sprintf("(*%s)", op_msg),
                      style = sprintf("color: %s;", op_col)
                    )
                  )
                )
              }))
            )
          )

        )
      )

    }, error_wrapper = "alert"),
    input$loader_do_import, ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      dipsaus::close_alert2()
      ravedash::clear_notifications(class = ns("notif-next-step"))
      dipsaus::shiny_alert2(
        title = "Migrating BIDS data to RAVE",
        text = ravedash::be_patient_text(), icon = "info",
        auto_close = FALSE, buttons = FALSE
      )
      on.exit({ shiny::removeModal() }, add = TRUE, after = TRUE)
      pipeline$run(names = "migrate_result",
                   scheduler = "none",
                   type = "vanilla",
                   async = FALSE)

      Sys.sleep(0.5)
      dipsaus::close_alert2()
      ravedash::show_notification(
        title = "Migrating BIDS data to RAVE",
        subtitle = "Success!",
        message = shiny::p(
          class = "no-margin",
          "Done migrating the data. Please go to RAVE module ",
          shiny::tags$a(
            id = ns("goto_lfp_module"), href = "#",
            class = "action-button badge badge-primary",
            "Import Signals > Native Structure"
          ),
          " to start the preprocess."
        ),
        autohide = FALSE, close = TRUE, type = "success",
        class = ns("notif-next-step")
      )

    }, error_wrapper = "alert"),
    input$loader_confirm_import, ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      BIDS_subject_info <- pipeline$read("BIDS_subject_info")
      pipeline_import_lfp <- ravepipeline::pipeline("import_lfp_native")
      pipeline_import_lfp$set_settings(
        import_setup__subject_code = BIDS_subject_info$subject_code
      )

      ravedash::clear_notifications(class = ns("notif-next-step"))
      ravedash::switch_module(
        module_id = "import_lfp_native",
        title = "Native Structure (from BIDS)"
      )

    }),
    input$goto_lfp_module, ignoreNULL = TRUE, ignoreInit = TRUE
  )

}
