# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    # style = 'max-width:1444px;',
    shiny::fluidRow(
      shiny::div(
        class = "col-md-12",
        shiny::div(
          class = 'row',
          shiny::column(
            width = 12L,

            # Step 1
            ravedash::input_card(
              title = "Step 1: Choose a project & subject to start",
              class_header = "",
              shiny::fluidRow(
                shiny::column(
                  width = 6L,
                  shiny::selectInput(
                    inputId = ns("loader_project_name"),
                    label = "Project name",
                    choices = all_projects,
                    selected = current_project
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::selectInput(
                    inputId = ns("loader_subject_code"),
                    label = "Subject code",
                    choices = character(0L),
                    selected = character(0L)
                  )
                )
              ),
              footer = shiny::div(
                class = "float-right",
                dipsaus::actionButtonStyled(
                  inputId = ns("loader_step1_btn"),
                  label = "Create/Load subject"
                )
                # shiny::uiOutput(
                #   comp$get_sub_element_id("actions", with_namespace = TRUE)
                # )
              )
            ),  # ravedash::input_card - step 1

            # Step 2
            shidashi::card2(
              inputId = ns("loader_step2_card"),
              title = "Step 2: Select recording blocks and formats",
              # class_header = "",
              class_body = "",
              body_main = shiny::div(
                class = 'padding-10 min-height-400',
                shiny::fluidRow(
                  shiny::column(
                    width = 6L,
                    shiny::selectInput(
                      inputId = ns("loader_session_block"),
                      label = "Recording Blocks",
                      choices = character(0L),
                      selected = character(0L),
                      multiple = TRUE
                    ),
                    shiny::selectInput(
                      inputId = ns("loader_format"),
                      label = "File Formats",
                      choices = names(all_formats)[c(1, 2, 3, 4, 7)],
                      selected = character(0L),
                      multiple = FALSE
                    ),
                    shiny::tags$small(
                      shiny::p(shiny::textOutput(
                        outputId = ns("loader_format_details")
                      )),
                      shiny::p("Please check the file format to make sure your data (preview has been printed in the right panel) is consistent with the configuration.")
                    )
                  ),
                  shiny::column(
                    width = 6L,
                    shiny::div(
                      class = "row overflow-y-scroll max-height-350",
                      shiny::column(
                        width = 12L,
                        shiny::verbatimTextOutput(
                          outputId = ns("loader_block_preview")
                        )
                      )
                    )
                  )
                )
              ),
              body_side = shiny::div(
                class = "bg-gray fill padding-10",
                shiny::textOutput(
                  outputId = ns("loader_step2_msg")
                )
              ),
              class_foot = "padding-10",
              footer = shiny::div(
                class = "float-right",
                dipsaus::actionButtonStyled(
                  ns("loader_step2_btn"),
                  label = "Confirm"
                )
              )
            ),  # ravedash::input_card - step 2

            # Step 3
            shidashi::card2(
              title = "Step 3: Enter channel information",
              inputId = ns("loader_step3_card"),
              class_body = "",
              body_main = shiny::div(
                class = 'padding-10',

                flex_group_box(
                  title = "Header inspector",
                  shidashi::flex_item(
                    shiny::selectInput(
                      inputId = ns("loader_electrode_file"),
                      label = "Data file(s)",
                      choices = "auto"
                    )
                  ),
                  # shidashi::flex_item(size = 2),
                  shidashi::flex_break(),
                  shidashi::flex_item(
                    shiny::tags$small(
                      shiny::uiOutput(
                        outputId = ns("loader_snapshot")
                      )
                    )
                  )
                ),

                flex_group_box(
                  title = "LFP channels",
                  shidashi::flex_item(
                    shiny::textInput(
                      inputId = ns('loader_lfp_channels'),
                      label = "Channel numbers",
                      placeholder = "E.g. 1-84, 100"
                    )
                  ),
                  shidashi::flex_item(
                    shiny::numericInput(
                      inputId = ns("loader_lfp_sample_rate"),
                      label = "Sample rate (Hz)",
                      value = NA,
                      min = 1
                    )
                  )
                ),

                flex_group_box(
                  title = "Microwire channels (single-unit, spikes)",
                  shidashi::flex_item(
                    shiny::textInput(
                      inputId = ns("loader_microwire_channels"),
                      label = "Channel numbers",
                      placeholder = "E.g. 257-268"
                    )
                  ),
                  shidashi::flex_item(
                    shiny::numericInput(
                      inputId = ns("loader_microwire_sample_rate"),
                      label = "Sample rate (Hz)",
                      value = NA,
                      min = 1
                    )
                  )
                ),

                flex_group_box(
                  title = "Auxiliary channels (audio, photodiode, ...)",
                  shidashi::flex_item(
                    shiny::textInput(
                      inputId = ns("loader_auxiliary_channels"),
                      label = "Channel numbers",
                      placeholder = "E.g. 259,260"
                    )
                  ),
                  shidashi::flex_item(
                    shiny::numericInput(
                      inputId = ns("loader_auxiliary_sample_rate"),
                      label = "Sample rate (Hz)",
                      value = NA,
                      min = 1
                    )
                  )
                ),

                flex_group_box(
                  title = "Compose channels",
                  shidashi::flex_item(
                    shiny::p(
                      class = 'mb-0',
                      'Create a "phantom" (not physically existed) channel from imported channels. You can ',
                      shiny::actionLink(
                        inputId = ns("loader_compose_upload"),
                        label = 'click here to upload'
                      ),
                      ' or enter the configurations directly below by clicking on ',
                      shiny::span("+", class = "inline code keyword"), 'button. ',
                      'Once the channel is set up, you can click on "Validate & import" to import the raw channels and compose the phantom channels. This will begin the pre-process. Alternatively, if the signals have been imported and processed, you can "Compose channels only". Notice this feature has a restriction that only allows you to add phantom channels. Existing channels will not be changed.'
                    ),
                    shiny::tags$details(
                      class = "mb-4",
                      shiny::tags$summary('Click me to see the explanation'),
                      'To compose a new channel, at least one existing channel (see entry `Compose from channels`) must be selected. By default, the new signal will be the average of all input signals with equal weights. Please use the entry `Weights` if unequal weights are to be used. ',
                      "For example, if you enter: ",
                      shiny::span("New channel = '100'", class = "inline code keyword"),
                      ", ",
                      shiny::span("Compose from channels = '14-16'", class = "inline code keyword"),
                      ", and ",
                      shiny::span("Weights = '3, -1, -2'", class = "inline code keyword"),
                      ", then RAVE will create a new \"phantom\" channel 100. ",
                      "The voltage potential of this new channel will be:",
                      shiny::tags$code("new channel = 3 x chan_14 - chan_15 - 2 x chan_16.", style = 'text-align: center')
                    )
                  ),
                  shidashi::flex_break(),
                  shidashi::flex_item(
                    dipsaus::compoundInput2(
                      inputId = ns("loader_compose_setup"),
                      label = "Configuration",
                      initial_ncomp = 0, min_ncomp = 0, max_ncomp = 1000,
                      max_height = "80vh",
                      components = shiny::fluidRow(
                        shiny::column(
                          width = 2L,
                          shiny::numericInput(
                            inputId = "number",
                            label = "New channel",
                            value = NA, min = 1L, step = 1L
                          )
                        ),
                        shiny::column(
                          width = 4L,
                          shiny::textInput(
                            inputId = "from",
                            label = "Compose from channels",
                            value = "",
                            placeholder = "Enter the channel numbers to compose, e.g. 1,2,14-25"
                          )
                        ),
                        shiny::column(
                          width = 4L,
                          shiny::textInput(
                            inputId = "weights",
                            label = "Weights",
                            value = "",
                            placeholder = "Leave it blank to use equal weights."
                          )
                        ),
                        shiny::column(
                          width = 2L,
                          shiny::selectInput(
                            inputId = "normalize",
                            label = "Normalize",
                            choices = c("no", "yes"),
                            selected = "no"
                          )
                        ),
                        shiny::column(
                          width = 12L,
                          shiny::textOutput(
                            outputId = "message",
                            container = shiny::div
                          )
                        )
                      )
                    )
                  ),
                  shidashi::flex_break(),
                  shidashi::flex_item(
                    shiny::uiOutput(
                      outputId = ns("loader_compose_message")
                    )
                  )
                )
              ),
              body_side = shiny::div(
                class = "bg-gray fill padding-10",
                shiny::textOutput(
                  outputId = ns("loader_step3_msg")
                )
              ),
              class_foot = "padding-10",
              footer = shiny::div(
                class = "float-right",
                dipsaus::actionButtonStyled(
                  ns("loader_actions_compose"),
                  label = "Compose channels only", type = "default"
                ),
                dipsaus::actionButtonStyled(
                  ns("loader_step3_alt_btn"),
                  label = "Skip validation & import", type = "default"
                ),
                dipsaus::actionButtonStyled(
                  ns("loader_step3_btn"),
                  label = "Validate & import"
                )
              )
            ) # End of step 3
          )
        )
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  local_reactives <- shiny::reactiveValues(
    refresh_step1 = NULL,
    refresh_step2 = NULL,
    refresh_step3 = NULL,
    valid_setup2 = FALSE,
    valid_setup3 = NULL,
    validation_message2 = "Waiting...",
    validation_message3 = "Waiting...",
    snapshot = NULL
  )
  local_data <- ravepipeline:::fastmap2()
  local_data$previous_project <- ""

  # ---- Reactive contents for STEP 1 ------------------------------------------
  shiny::bindEvent(
    ravedash::safe_observe({
      project_name <- input$loader_project_name
      if(!length(project_name) || is.na(project_name) || !nzchar(project_name)) { return() }
      if(!identical(project_name, "[New Project]")) {
        local_data$previous_project <- project_name
        if( grepl("@bids", project_name) ) {
          # This is a bids project
          # project_name <- 'demo@bids:ds005953'
          project <- ravecore::as_rave_project(project_name, strict = FALSE)
          participants <- bidsr::get_bids_participants(project$`@impl`@parent_path)
          subject_codes <- participants@content$participant_id
          subject_codes <- gsub("^sub-", "", subject_codes)
        } else {
          subject_codes <- all_native_subjects()
        }

        current_subject <- c(
          pipeline$get_settings("subject_code"),
          input$loader_subject_code
        ) %OF% subject_codes

        shiny::updateSelectInput(
          session = session,
          inputId = "loader_subject_code",
          choices = subject_codes,
          selected = current_subject
        )
        if(!identical(current_subject, input$loader_subject_code)) {
          later::later(delay = 0.5, function() {
            shiny::updateSelectInput(
              session = session,
              inputId = "loader_subject_code",
              selected = current_subject
            )
          })
        }
        return()
      }

      # New project
      ravepipeline::logger("Opening up a modal to create new project", level = "trace")
      shiny::showModal(shiny::modalDialog(
        title = "Create new project",
        easyClose = FALSE,
        shiny::div(
          class = 'fill-width',
          shiny::fluidRow(
            shiny::column(
              width = 12L,
              shiny::p(
                "RAVE project is a purpose-based collection of subjects. ",
                "For example, a project name can be the experiment or analysis names.",
                "For native projects, the name must only contain letters (a-z), ",
                "digits (0-9); for example, 'demo', 'audiovisual', etc. ",
                "For BIDS datasets, the project must contain an indicator of ",
                "the BIDS dataset name, for example, 'BHAanalysis@bids:ds005953', ",
                "where 'BHAanalysis' is the analysis name and 'bids:ds005953' refers to ",
                "a BIDS dataset ID"
              ),
              shiny::textInput(
                inputId = ns("loader_new_project_name"),
                label = "Enter a valid project name:"
              )
            )
          )
        ),
        footer = shiny::tagList(
          shiny::actionButton(
            inputId = ns('new_project_name_dismiss'),
            label = "Dismiss"
          ),
          dipsaus::actionButtonStyled(
            inputId = ns('new_project_name_confirm'),
            label = "Create"
          )
        )
      ))
    }),
    input$loader_project_name,
    local_reactives$refresh_step1,
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  # Check and remove modal if "dismiss" button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # check current project size
      all_projects <- update_projects(refresh = TRUE)

      # len = 2 means "" and "[New Project]" options
      if(length(all_projects) <= 2) {
        ravepipeline::logger("Dismiss button is pressed but there is no project", level = "warning")
        shidashi::show_notification(
          "You haven't created any project yet. Please create one so you can start to import subjects.",
          type = "warning", close = TRUE, autohide = TRUE
        )
        return()
      }

      ravepipeline::logger("Dismiss button is pressed", level = "trace")
      shiny::updateSelectInput(
        session = session, selected = local_data$previous_project,
        inputId = "loader_project_name"
      )
      shiny::removeModal()

    }),
    input$new_project_name_dismiss,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # Create new project name
  shiny::bindEvent(
    ravedash::safe_observe({
      # check current project size
      new_project_name <- input$loader_new_project_name

      raise_validation_error <- function(msg) {
        msg <- paste(msg, collapse = "")
        ravepipeline::logger("Invalid new project name: `{new_project_name}`; reason: {msg}", level = "error", use_glue = TRUE)
        shidashi::show_notification(
          title = "The project name is invalid",
          message = msg, type = "warning", close = TRUE, autohide = FALSE
        )
      }

      if(!length(new_project_name)) {
        new_project_name <- ""
      }
      new_project_name <- trimws(new_project_name, which = "both")
      if(new_project_name == ''){
        raise_validation_error("The project name cannot be blank")
        return()
      }

      if (!grepl("^[a-zA-Z0-9][a-zA-Z0-9@:_-]{0,}$", new_project_name)){
        raise_validation_error(c(
          "The subject code is invalid: can only contain letters, digits, dash (-), or underscore (_). ",
          "The first letter should only contain letters and digits."
        ))
        return()
      }

      all_projects <- update_projects(refresh = TRUE)

      if( tolower(new_project_name) %in% tolower(all_projects) ){
        raise_validation_error("Project has already existed")
        return()
      }

      project <- ravecore::as_rave_project(new_project_name, strict = FALSE)
      ravepipeline::dir_create2(project$path)

      ravepipeline::logger("Created new project name: `{new_project_name}`", level = "success", use_glue = TRUE)

      shidashi::clear_notifications()
      shidashi::show_notification(
        message = sprintf(
          "A new RAVE project folder [%s] has been created!",
          new_project_name
        ),
        type = "success",
        title = "Success!",
        subtitle = "New Project"
      )
      shiny::updateSelectInput(
        session = session, selected = new_project_name,
        choices = c(new_project_name, all_projects),
        inputId = "loader_project_name"
      )
      later::later(delay = 0.5, function() {
        shiny::updateSelectInput(
          session = session, selected = new_project_name,
          inputId = "loader_project_name"
        )
      })
      shiny::removeModal()

    }),
    input$new_project_name_confirm,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  validate_step1 <- shiny::bindEvent(
    shiny::reactive({
      project_name <- trimws(input$loader_project_name)
      subject_code <- trimws(input$loader_subject_code)
      tryCatch({
        if(!length(project_name) || project_name == "" ||
           !length(subject_code) || trimws(subject_code) == "" ){
          stop("Blank project/subject found. Please enter the inputs")
        }
        subject <- ravecore::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)

        has_preprocess_path <- FALSE
        has_raw_path2 <- TRUE

        if(dir.exists(subject$preprocess_path)) {
          # raw_path2 might not be needed since the signals might have been imported already
          has_preprocess_path <- TRUE
        }
        if(!dir.exists(subject$preprocess_settings$raw_path2)) {
          if(!has_preprocess_path) {
            stop(sprintf("Cannot find raw folder for subject `%s`", subject$subject_id))
          }
          has_raw_path2 <- FALSE
        }

        list(
          valid = TRUE,
          initialized = has_preprocess_path,
          raw_exists = has_raw_path2,
          project_name = project_name,
          subject_code = subject_code
        )
      }, error = function(e) {
        list(
          valid = FALSE,
          reason = paste(e$message, collapse = " ")
        )
      })
    }),
    input$loader_project_name,
    input$loader_subject_code,
    local_reactives$force_refresh,
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  # Update `actions` button's label text
  shiny::bindEvent(
    ravedash::safe_observe({
      validation <- validate_step1()
      if(!validation$valid){
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = 'loader_step1_btn',
          disabled = TRUE,
          label = validation$reason
        )
      } else if(validation$initialized){
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = 'loader_step1_btn',
          disabled = TRUE,
          label = "Subject already exists"
        )
      } else {
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = 'loader_step1_btn',
          disabled = FALSE,
          label = "Create subject"
        )
      }
    }, error_wrapper = "notification"),
    validate_step1(),
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      validator <- validate_step1()
      if(!isTRUE(validator$valid) || !isFALSE(validator$initialized)) { return() }
      # Create a new subject!
      project_name <- validator$project_name
      subject_code <- validator$subject_code
      ravepipeline::logger("Initializing RAVE subject folders {project_name}/{subject_code}", level = "trace", use_glue = TRUE)
      preprocess <- ravecore::RAVEPreprocessSettings$new(subject = sprintf("%s/%s", project_name, subject_code), read_only = FALSE)
      preprocess$subject$initialize_paths(include_freesurfer = FALSE)
      preprocess$save()
      local_reactives$force_refresh <- Sys.time()
      ravepipeline::logger("Initialized RAVE subject folders {project_name}/{subject_code}", level = "success", use_glue = TRUE)
    }),
    input$loader_step1_btn,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )


  # ---- Reactive contents for STEP 2 ------------------------------------------
  disable_step2 <- function(){
    dipsaus::updateActionButtonStyled(
      session = session,
      inputId = "loader_step2_btn",
      disabled = TRUE
    )
    local_reactives$valid_setup2 <- FALSE

    shiny::updateSelectInput(
      session = session,
      inputId = "loader_session_block",
      label = "Recording Blocks",
      choices = character(0L)
    )

    shidashi::card2_open("loader_step2_card", session = session)
  }

  preproc_from_info <- function(info) {
    if(!isTRUE(info$valid)) { return() }
    # enable UI
    project_name <- info$project_name
    subject_code <- info$subject_code
    subject <- ravecore::RAVESubject$new(
      project_name = project_name,
      subject_code = subject_code, strict = FALSE)
    preproc <- subject$preprocess_settings
    preproc
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      # return(list(
      # valid = TRUE,
      # initialized = FALSE,
      # project_name = project_name,
      # subject_code = subject_code
      # # format = format
      # ))
      info <- validate_step1()
      is_valid <- TRUE
      if(!is.list(info)){
        local_reactives$validation_message2 <- "Waiting..."
        is_valid <- FALSE
      }
      if(!isTRUE(info$valid)){
        local_reactives$validation_message2 <- "Please select valid project and subject in the previous step."
        is_valid <- FALSE
      }
      if(!isTRUE(info$initialized)) {
        local_reactives$validation_message2 <- "Subject folders have not been created yet. Please create them in the previous step."
        is_valid <- FALSE
      }

      if(!is_valid) {
        disable_step2()
        return()
      }

      # enable UI
      project_name <- info$project_name
      subject_code <- info$subject_code
      preproc <- preproc_from_info(info)
      format_selection <- preproc$data$format %OF% seq_along(all_formats)

      shiny::updateSelectInput(
        session = session,
        inputId = "loader_format",
        selected = names(all_formats)[[format_selection]]
      )

      if(any(preproc$data_imported)) {
        shiny::updateSelectInput(
          session = session,
          inputId = "loader_session_block",
          label = "Recording Blocks (read-only)",
          choices = preproc$all_blocks,
          selected = preproc$blocks
        )
      } else {
        shiny::updateSelectInput(
          session = session,
          inputId = "loader_session_block",
          label = "Recording Blocks",
          choices = preproc$all_blocks,
          selected = preproc$blocks
        )
      }

      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = "loader_step2_btn",
        disabled = FALSE
      )
      shidashi::card2_close("loader_step2_card")


    }),
    validate_step1(),
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  # validations here
  validate_step2 <- shiny::bindEvent(
    shiny::reactive({
      tryCatch({
        info <- validate_step1()
        if(!is.list(info) || !isTRUE(info$initialized)) {
          stop("Waiting for previous steps")
        }
        info$blocks <- input$loader_session_block
        if(!length(info$blocks)) {
          stop("No block selected")
        }
        # FIXME
        # info$format <- which(names(all_formats) %in% loader_format)
        info$format <- which(names(ravecore::IMPORT_FORMATS) %in% input$loader_format)

        preproc <- preproc_from_info(info)

        if(any(preproc$data_imported)) {
          info$any_imported <- TRUE
          info$current_blocks <- preproc$blocks
          info$current_format <- preproc$data$format %OF% seq_along(all_formats)

        } else {
          info$any_imported <- FALSE
          if(length(preproc$blocks)) {
            info$current_blocks <- preproc$blocks
          } else {
            info$current_blocks <- NULL
          }
          info$current_format <- preproc$data$format %OF% c(info$format, seq_along(all_formats))
        }

        block_files <- find_block_files(preproc$subject, info$blocks, info$format)
        lapply(info$blocks, function(block) {
          item <- as.list(block_files[[block]])
          if(!length(item) || !length(item$signal_files)) {
            stop(sprintf("Recording block `%s` has no signal files matching the chosen format",
                         block))
          }
        })
        info


      }, error = function(e) {
        list(
          valid = FALSE,
          reason = paste(e$message, collapse = " ")
        )
      })

    }),
    validate_step1(),
    input$loader_session_block,
    input$loader_format,
    local_reactives$refresh_step1,
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      info <- validate_step2()
      if(!isTRUE(info$valid)) {
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = "loader_step2_btn",
          disabled = TRUE,
          label = info$reason
        )
        return()
      }
      if(setequal(info$current_blocks, info$blocks) &&
         isTRUE(info$current_format == info$format)) {

        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = "loader_step2_btn",
          disabled = TRUE,
          label = "No change detected"
        )
        return()

      }
      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = "loader_step2_btn",
        disabled = FALSE,
        label = "Confirm"
      )
    }),
    validate_step2(),
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  # Outputs for step 2
  output$loader_step2_msg <- shiny::renderText({
    if(isTRUE(local_reactives$valid_setup2)) {
      "Subject folder has been created. Please choose session blocks."
    } else {
      local_reactives$validation_message2
    }
  })

  output$loader_format_details <- shiny::renderText({

    info <- validate_step2()
    if(!is.list(info) || !isTRUE(info$valid)) { return() }
    fmt_idx <- info$format
    if(length(fmt_idx) != 1 || !fmt_idx %in% seq_along(ravecore::IMPORT_FORMATS)) {
      return()
    }

    fmt_str <- ravecore::IMPORT_FORMATS[[fmt_idx]]

    switch (
      as.character(fmt_idx),
      'native_matlab' = {
        paste0("In each block folder, one Matlab/HDF5 file stands for one electrode. ",
               "File name should match with format XXX1.h5 or xxx2.mat. ",
               "Each file only contains a one-dimensional vector. ",
               "The vector lengths stand for total time points and they must be the same across all electrode files. ")
      },
      'native_matlab2' = {
        paste0("A single Matlab/HDF5 file containing all electrode information. ",
               "Data must be a matrix. One of the dimension must be electrodes, ",
               "the other dimension must be time points. ",
               "ALL blocks must share the same file & data name.")
        # shiny::div(
        #   shiny::p("A single Matlab/HDF5 file containing all electrode information. ",
        #            "Data must be a matrix. One of the dimension must be electrodes, ",
        #            "the other dimension must be time points. ",
        #            "ALL blocks must share the same file & data name; for example:"),
        #   shiny::tags$pre(
        #     dipsaus::print_directory_tree(
        #       c('block1', 'block2'),
        #       root = '<subject folder>',
        #       child = c(
        #         'datafile.mat <one big matrix>'
        #       ),
        #       collapse = '\n'
        #     )
        #   ))
      },
      'native_blackrock' = {
        paste0("In each block folder, one Neuro-Event file [.nev] and corresponding NSX files [.ns1, .ns2, ..., .ns6] containing electrode data.")
      },
      {
        paste0("In each block folder, one EDF(+)/EEG file containing all electrode data.")
        # shiny::div(
        #   shiny::p("In each block folder, one EDF(+) file containing all electrode data; for example:"),
        #   shiny::tags$pre(
        #     dipsaus::print_directory_tree(
        #       c('block1', 'block2'),
        #       root = '<subject folder>',
        #       child = c(
        #         'datafile.edf <ONLY one EDF file per block>'
        #       ),
        #       collapse = '\n'
        #     )
        #   ))
      }
    )
  })

  output$loader_block_preview <- shiny::renderPrint({
    info <- validate_step2()
    if(!is.list(info) || !isTRUE(info$valid)) {
      return("Please choose valid blocks and format")
    }
    fmt_str <- ravecore::IMPORT_FORMATS[[info$format]]
    blocks <- info$blocks

    subject_code <- info$subject_code
    preproc <- preproc_from_info(info)
    if(!dir.exists(preproc$raw_path2)) {
      return("Cannot find raw data path")
    }

    root_str <- sprintf("%s (subject folder)", subject_code)
    regexp <- native_regexps[[fmt_str]]

    switch (
      preproc$raw_path2_type,
      "bids" = {
        fs <- list.files(
          preproc$raw_path2,
          include.dirs = FALSE,
          recursive = TRUE,
          pattern = regexp,
          all.files = FALSE,
          full.names = FALSE,
          no.. = TRUE,
          ignore.case = TRUE
        )
        if(!length(fs)) {
          return("No file matching the format")
        }
        fs <- t(sapply(strsplit(fs, "/|\\\\"), function(f) {
          c(paste(f[-length(f)], collapse = " > "), f[[length(f)]])
        }))
        fs <- data.frame(padir = fs[, 1], fname = fs[, 2])
        lapply(split(fs, fs$padir), function(sub) {
          print(
            dipsaus::print_directory_tree(
              target = sub$padir[[1]],
              root = sprintf("sub-%s", subject_code),
              dir_only = TRUE,
              child = sub$fname
            )
          )
          invisible()
        })
        root_str <- " "
      },
      {
        for(block in blocks) {
          fs <- list.files(
            file.path(preproc$raw_path2, block),
            pattern = regexp,
            recursive = FALSE,
            all.files = FALSE,
            full.names = FALSE,
            ignore.case = TRUE
          )
          max_components <- 3
          if(length(fs) > max_components) {
            fs <- c(fs[seq_len(max_components - 1)], "...")
          }
          print(dipsaus::print_directory_tree(
            sprintf("%s (session folder)", block),
            root = root_str,
            dir_only = TRUE,
            child = fs
          ))
          root_str <- " "
        }
      }
    )

  })

  # blocks, format, any_imported
  set_blocks <- function(preproc, info){
    ravepipeline::logger("Current subject: ", preproc$subject$subject_id, level = "info")
    new_blocks <- info$blocks
    format <- info$format

    if(any(preproc$data_imported)) {

      if(!isFALSE(preproc$data$stringent)){

        if(!setequal(preproc$blocks, new_blocks)) {
          ravepipeline::logger("Subject is set with less stringent validation.", level = "info")
          preproc$data$stringent <- FALSE
        }

      }

    }

    block_files <- find_block_files(preproc$subject, new_blocks, format)
    lapply(new_blocks, function(block) {
      item <- as.list(block_files[[block]])
      if(!length(item) || !length(item$signal_files)) {
        stop(sprintf("Recording block `%s` has no signal files matching the chosen format",
                     block))
      }
    })

    ravepipeline::logger("Setting session blocks: ", paste(new_blocks, collapse = ", "), level = "info")
    preproc$data$blocks <- new_blocks

    ravepipeline::logger("Setting session format: {all_formats[[format]]} ({names(all_formats)[[format]]})", level = "info", use_glue = TRUE)
    preproc$data$format <- format

    format_str <- ravecore::IMPORT_FORMATS[[format]]
    format_full <- names(ravecore::IMPORT_FORMATS)[ravecore::IMPORT_FORMATS == format_str]

    preproc$save()

    pipeline$set_settings(
      skip_validation = FALSE,
      force_import = TRUE,

      subject_code = info$subject_code,
      project_name = info$project_name,

      import_blocks = info$blocks,
      import_format = all_formats[[format]]

      # import_channels__lfp_unit = "NA",
      # lfp_sample_rate = 2000L,
      # lfp_channels = "14-16",
      #
      # import_channels__auxiliary_unit = "NA",
      # auxiliary_sample_rate = 15000L,
      # auxiliary_channels = "13",
      #
      # import_channels__microwire_unit = "NA",
      # microwire_sample_rate = 30000L,
      # microwire_channels = "24",
      #
      # import_channels__electrodes = NULL,
      # import_channels__electrode_file = "auto",
      # import_channels__compose_upload_file = NULL,
      # import_channels__compose_upload_example = NULL,
      # import_channels__compose_upload = 0L,
      # import_channels__compose_setup = structure(list(), names = character(0)),
      #
      # compose_setup = list()
    )

    shidashi::clear_notifications()
    shidashi::show_notification(
      title = "Set blocks and format",
      subtitle = "Success!",
      type = "success",
      message = shiny::p(
        "The subject [", preproc$subject$subject_id, "] has been edited: ",
        shiny::tags$ul(
          shiny::tags$li("Session blocks: ", paste(new_blocks, collapse = ", ")),
          shiny::tags$li("Session format: ", all_formats[[format]], " (",
                         names(all_formats)[[format]], ")")
        )
      ),
      autohide = TRUE
    )

    local_reactives$refresh_step1 <- Sys.time()

  }

  shiny::bindEvent(
    ravedash::safe_observe({

      info <- validate_step2()
      if(!is.list(info) || !isTRUE(info$valid)) {
        shidashi::show_notification(
          title = "Error", type = "danger", autohide = FALSE,
          message = paste(c(
            "One or more errors found while trying to configure the subject: ",
            info$reason
          ), collapse = ""),
          class = "_rave-import-LFP-notif-error_"
        )
        return()
      }


      format_idx <- info$format
      if(!length(format_idx)){
        shidashi::show_notification(
          title = "Error", type = "danger", autohide = FALSE,
          message = paste(c(
            "Cannot recognize the format: ",
            info$format
          ), collapse = ""),
          class = "_rave-import-LFP-notif-error_"
        )
        return()
      }

      new_blocks <- info$blocks
      preproc <- preproc_from_info(info)

      if(info$any_imported && !setequal(preproc$blocks, new_blocks)) {
        shidashi::show_notification(
          title = "Block inconsistent", type = "warning",
          autohide = FALSE, close = TRUE,
          message = shiny::div(
            shiny::p(
              "It seems you have imported this subject with different session/block settings before. Please proceed and press the `OK` button if this change is intentional. Please be aware that this feature (changing the session blocks after importing the data) is experimental and will result in less stringent validation."
            ),
            shiny::tags$ul(
              shiny::tags$li(
                "Existing blocks: ", paste(preproc$blocks, collapse = ", ")
              ),
              shiny::tags$li(
                "New blocks: ", paste(new_blocks, collapse = ", ")
              )
            ),
            shiny::actionButton(
              ns("loader_action_dbl_confirm"),
              label = "OK",
            )
          )
        )
        return()
      }

      set_blocks(preproc, info)
    }, error_wrapper = "notification"),
    input$loader_step2_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      info <- validate_step2()
      if(!isTRUE(info$valid)) { return() }
      preproc <- preproc_from_info(info)
      set_blocks(preproc, info)
    }),
    input$loader_action_dbl_confirm,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # ---- Reactive contents for STEP 3 ----

  disable_step3 <- function(){
    dipsaus::updateActionButtonStyled(
      session = session,
      inputId = "loader_step3_btn",
      disabled = TRUE
    )
    dipsaus::updateActionButtonStyled(
      session = session,
      inputId = "loader_step3_alt_btn",
      disabled = TRUE
    )

    local_reactives$valid_setup3 <- FALSE
    shidashi::card2_open("loader_step3_card", session = session)
  }

  enable_compose_button <- function(enable = TRUE) {
    info <- shiny::isolate({ validate_step2() })
    if(!is.list(info) || !isTRUE(info$valid)) {
      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = "loader_actions_compose",
        disabled = TRUE
      )
      return()
    }
    dipsaus::updateActionButtonStyled(
      session = session,
      inputId = "loader_actions_compose",
      disabled = !enable
    )
  }

  output$loader_step3_msg <- shiny::renderText({
    if(isTRUE(local_reactives$valid_setup3)) {
      "Subject folder has been created. Please choose session blocks."
    } else {
      local_reactives$validation_message3
    }
  })

  output$loader_snapshot <- shiny::renderUI({
    local_reactives$snapshot
  })

  output$loader_compose_upload_example <- shiny::downloadHandler(
    filename = function(...) {
      info <- validate_step2()
      sprintf(
        "RAVE_compose_plan-%s_%s.csv",
        paste(info$project_name, collapse = ""),
        paste(info$subject_code, collapse = "")
      )
    },
    content = function(con) {
      info <- validate_step2()
      preproc <- preproc_from_info(info)
      tbl <- preproc$get_compose_weights(flat = TRUE)
      if(!is.data.frame(tbl)) {
        elec <- preproc$electrodes
        if(!length(elec)) { elec <- 1:4 }
        tbl <- data.frame(
          Source = elec,
          Target = 10^ceiling(log10(max(elec))) + 1,
          Weight = rep(1 / length(elec), length(elec))
        )
      }
      utils::write.csv(x = tbl,
                       file = con,
                       row.names = FALSE)
    }
  )

  # Fileupload widget to help compose channels
  shiny::bindEvent(
    safe_observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "Upload composing plan",
          size = "l",
          easyClose = FALSE,
          footer = shiny::modalButton(label = "Cancel"),
          dipsaus::fancyFileInput(
            inputId = ns("loader_compose_upload_file"),
            label = NULL, width = "100%", after_content = "Accepts CSV file",
            size = "m", multiple = FALSE, accept = ".csv"
          ),
          shiny::p(
            "Please upload a CSV file of electrode channels and a weight matrix. The column names will be interpreted as new channel number. ",
            shiny::downloadLink(
              outputId = ns("loader_compose_upload_example"),
              label = "Click here to download an example."
            )
          )
        )
      )
    }),
    input$loader_compose_upload,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    safe_observe({
      finfo <- input$loader_compose_upload_file
      if(length(finfo$datapath) != 1 || !file.exists(finfo$datapath)) {
        stop("The file upload might have failed/been invalid.")
      }
      tbl <- utils::read.csv(finfo$datapath)
      if(!nrow(tbl)) {
        stop("The uploaded table contains no data.")
      }
      if(!all(c("Source", "Target", "Weight") %in% names(tbl))) {
        stop("The uploaded table column must contains the following columns (case-sensitive): [Source, Target, Weight]; one or more missing...")
      }
      tbl$Source <- as.integer(tbl$Source)
      tbl$Target <- as.integer(tbl$Target)
      tbl$Weight <- as.numeric(tbl$Weight)
      tbl <- tbl[stats::complete.cases(tbl), ,drop = FALSE]
      value <- lapply(split(tbl, tbl$Target), function(sub) {
        o <- order(sub$Source)
        w <- sub$Weight[o]
        if(length(unique(w)) == 1) {
          w <- w[[1]]
        }
        list(
          number = sub$Target[[1]],
          from = deparse_svec(sub$Source),
          weights = w,
          normalize = "no"
        )
      })

      dipsaus::updateCompoundInput2(
        session = session,
        inputId = "loader_compose_setup",
        value = value,
        ncomp = length(value)
      )
      shiny::removeModal()
    }, error_wrapper = "notification"),
    input$loader_compose_upload_file,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Validate and store the cleaned channel compose_setup
  shiny::bindEvent(
    ravedash::safe_observe({
      local_reactives$compose_setup <- NULL
      if(isTRUE(local_reactives$valid_setup3)) {
        enable_compose_button(FALSE)
        return()
      }
      lfp_channels <- dipsaus::parse_svec(input$loader_lfp_channels)
      microwire_channels <- dipsaus::parse_svec(input$loader_microwire_channels)
      auxiliary_channels <- dipsaus::parse_svec(input$loader_auxiliary_channels)
      electrodes <- c(lfp_channels, microwire_channels, auxiliary_channels)
      compose_setup <- input$loader_compose_setup
      if(!length(compose_setup)) {
        enable_compose_button(FALSE)
        return()
      }

      new_channels <- NULL
      re <- lapply(seq_along(compose_setup), function(ii) {
        item <- compose_setup[[ii]]
        item$number <- as.integer(item$number)
        set_message <- function(..., color = "red") {
          msg <- paste(..., sep = "", collapse = "")
          elem_id <- ns(sprintf("loader_compose_setup_message_%d", ii))
          session$sendCustomMessage(
            type = "shidashi.set_html",
            message = list(
              selector = sprintf("#%s", elem_id),
              content = sprintf("<span style='color:%s;'>%s</span>", color, msg)
            )
          )
          return(msg)
        }
        if( length(item$number) != 1 || is.na(item$number) ) {
          set_message("")
          return(NULL)
        }
        if(item$number < 1) {
          return(set_message("Invalid channel number to compose: ", item$number))
        }
        if(item$number %in% electrodes) {
          return(set_message("Channel number conflicts with existing physical channels", item$number))
        }
        if(item$number %in% new_channels) {
          return(set_message("Channel number already used. Please choose another one.", item$number))
        }
        from <- parse_svec(item$from)
        weights <- unlist(strsplit(item$weights, ","))
        weights <- trimws(weights)
        weights <- weights[weights != '']
        if(!length(from)) {
          return(set_message("Cannot compose signals from empty list of channels: ", item$number))
        }
        if(!all(from %in% electrodes)) {
          return(set_message("Cannot compose channel from non-existing channels: ", item$from))
        }

        if(length(weights) > 0) {
          if(length(weights) > 1 && length(from) != length(weights)) {
            return(set_message("Length of weights must be 0, 1, or equal to the length of composing channels."))
          }
          weights <- as.numeric(weights)
          if(anyNA(weights) || any(weights == 0)) {
            return(set_message("Weights must be non-zero number."))
          }
        }
        if(length(weights) == 0) {
          weights <- 1 / length(from)
        }
        if(length(weights) == 1) {
          weights <- rep(weights, length(from))
        }

        if(identical(item$normalize, "yes")) {
          weights2 <- weights / sqrt(sum(weights^2))
          normalize <- TRUE
        } else {
          weights2 <- weights
          normalize <- FALSE
        }


        new_channels <<- c(new_channels, item$number)

        set_message(sprintf(
          "New channel [%s] will be composed from [%s]",
          item$number,
          paste(sprintf("#%d (w=%.1f%%)", from, weights2 * 100), collapse = ", ")
        ),
        color = "#ccc")

        list(
          number = item$number,
          from = from,
          weights = weights,
          normalize = normalize
        )
      })
      re <- re[!vapply(re, is.null, FALSE)]


      if(!length(re) || any(vapply(re, is.character, FALSE))) {
        enable_compose_button(FALSE)
      } else {
        enable_compose_button(TRUE)
      }
      local_reactives$compose_setup <- re
    }),
    local_reactives$valid_setup3,
    input$loader_lfp_channels,
    input$loader_microwire_channels,
    input$loader_auxiliary_channels,
    input$loader_compose_setup
  )

  shiny::bindEvent(
    safe_observe({
      # return(list(
      # valid = TRUE,
      # initialized = FALSE,
      # project_name = project_name,
      # subject_code = subject_code
      # any_imported
      # current_blocks
      # current_format
      # blocks
      # format
      # ))
      info <- validate_step2()
      is_valid <- TRUE
      if(!is.list(info)){
        local_reactives$validation_message3 <- "Waiting..."
        is_valid <- FALSE
      }
      if(!isTRUE(info$valid)){
        local_reactives$validation_message3 <- "Please finish the previous steps."
        is_valid <- FALSE
      }

      if(!is_valid) {
        disable_step3()
        return()
      }

      # enable UI
      blocks <- info$current_blocks
      format <- info$current_format

      preproc <- preproc_from_info(info)

      # get electrode files
      block_files <- find_block_files(preproc$subject, blocks, format)
      common_names <- do.call("rbind", lapply(blocks, function(block) {
        block_info <- block_files[[block]]
        cbind(
          sprintf("%s/%s", block, basename(block_info$signal_files)),
          block_info$signal_files
        )
      }))

      all_electrode_files <- c("auto", common_names[, 1])
      shiny::updateSelectInput(
        session = session,
        inputId = "loader_electrode_file",
        choices = all_electrode_files,
        selected = preproc$data$electrode_file %OF% all_electrode_files
      )

      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = "loader_step3_btn",
        disabled = FALSE
      )
      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = "loader_step3_alt_btn",
        disabled = FALSE
      )
      shidashi::card2_close("loader_step3_card")

    }),
    validate_step2(),
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  shiny::bindEvent(
    safe_observe({
      info <- validate_step2()
      if(!is.list(info) || !isTRUE(info$valid)) { return() }

      local_reactives$snapshot <- NULL

      format <- info$current_format
      format_str <- ravecore::IMPORT_FORMATS[[format]]
      blocks <- info$current_blocks

      preproc <- preproc_from_info(info)

      # get electrode files
      block_files <- find_block_files(preproc$subject, blocks, format)
      common_names <- do.call("rbind", lapply(blocks, function(block) {
        block_info <- block_files[[block]]
        cbind(
          sprintf("%s/%s", block, basename(block_info$signal_files)),
          block_info$signal_files
        )
      }))
      if(!length(common_names)) { return() }

      user_signal_file <- input$loader_electrode_file
      if(identical(user_signal_file, "auto")) {
        user_signal_file <- common_names[, 1]
      }

      sel <- common_names[, 1] %in% user_signal_file
      # get potential electrodes

      switch (
        format_str,
        "native_matlab" = {
          fs <- common_names[sel, 2]
          # fs <- list.files(file.path(preproc$raw_path, blocks[[1]]))
          es <- gsub("(^.*[^0-9]|^)([0-9]+)\\.(mat|h5)", "\\2", fs)
          es <- es[grepl("^[0-9]+$", es)]
          es <- as.integer(es)
          es <- es[!is.na(es)]

          local_reactives$snapshot <- shiny::p(
            "With given data format (single .mat/.h5 file per electrode), ",
            "I found the following potential channels from `",
            user_signal_file[[1]], "`: ", deparse_svec(es)
          )
        },
        "native_edf" = {
          edf_path <- common_names[sel, 2]
          if(length(edf_path) > 1){
            edf_path <- edf_path[which.max(file.size(edf_path))]
          }
          tryCatch({
            header <- ieegio::read_edf(
              edf_path,
              extract_path = nullfile(),
              header_only = TRUE,
              cache_ok = FALSE
            )
            is_signal <- !header$channel_table$Annotation
            channels <- header$channel_table$Channel[is_signal]
            duration <- header$basic$record_duration * header$basic$n_records
            sample_rates <- header$channel_table$SampleRate[is_signal]
            units <- header$channel_table$Unit[is_signal]

            local_reactives$snapshot <- shiny::p(
              "With given data format (EDF), I found the following header information: ",
              shiny::tags$ul(
                shiny::tags$li(
                  "Potential channels: ", dipsaus::deparse_svec(channels)
                ),
                shiny::tags$li(
                  "Recording length: ",
                  sprintf("%.4f seconds", duration)
                ),
                shiny::tags$li(
                  "Potential sample rates: ",
                  paste(sort(unique(sample_rates)), "Hz", collapse = ", ")
                ),
                shiny::tags$li(
                  "Potential physical units: ",
                  paste(unique(units), collapse = ",")
                ),
                shiny::tags$li(
                  "Channel labels: ",
                  paste(header$channel_table$Label[is_signal], collapse = ", ")
                )
              )
            )
          }, error = function(e){
            if(isTRUE(electrode_file == "auto")) {
              local_reactives$snapshot <- shiny::p(
                "Cannot read the EDF file ", basename(edf_path),
                " in the first block (",
                blocks[[1]], "). Please manually choose the EDF file."
              )
            } else {
              local_reactives$snapshot <- shiny::p(
                "Cannot read the EDF file ", basename(edf_path),
                " in the first block (",
                blocks[[1]], "). Please make sure the file is valid."
              )
            }

          })
        },
        "native_brainvis" = {
          brainvis_path <- common_names[sel, 2]
          brainvis_path <- brainvis_path[grepl("\\.vhdr$", brainvis_path, ignore.case = TRUE)]
          if(!length(brainvis_path)) {
            local_reactives$snapshot <- shiny::p(
              "Cannot find any VHDR header file from"
            )
          } else {
            brainvis_path <- brainvis_path[[1]]
            tryCatch({
              header <- ieegio::read_brainvis(brainvis_path,
                                              extract_path = tempdir(),
                                              header_only = TRUE)

              n_channels <- header$CommonInfos$NumberOfChannels
              sample_rates <- unique(header$CommonInfos$SampleRate)

              local_reactives$snapshot <- shiny::p(
                "With given data format (BrainVision), I found the following information from ",
                "`", basename(brainvis_path), "`",
                shiny::tags$ul(
                  shiny::tags$li("Total number of channels: ", n_channels),
                  shiny::tags$li("Sample rate(s): ",
                                 paste(sprintf("%.4f Hz", sample_rates), collapse = ","))

                )
              )
            }, error = function(e){
              local_reactives$snapshot <- shiny::p(
                "Cannot read the BrainVision file ",
                basename(brainvis_path)
              )
              ravepipeline::logger_error_condition(e)
            })
          }
        },
        "native_blackrock" = {
          # Ignore electrode_file
          nev_path <- common_names[sel, 2]
          nev_path <- nev_path[grepl("\\.nev$", nev_path, ignore.case = TRUE)]
          if(!length(nev_path)) {
            local_reactives$snapshot <- shiny::p(
              "Cannot find any NEV file"
            )
          } else {
            tryCatch({

              brpath <- nev_path[[1]]
              # brpath <- '~/rave_data/raw_dir/PAV058/001/raw/PAV054_Datafile_001.nev'
              header <- readNSx::import_nsp(
                brpath, prefix = tempfile(),
                exclude_events = c('spike', 'video_sync', 'digital_inputs',
                                   'tracking', 'button_trigger', 'comment'),
                exclude_nsx = 1:9, verbose = TRUE)

              elec_table <- header$nev$header_extended$NEUEVLBL
              elec_table$labelprefix <- gsub('[0-9_-]+$', '', elec_table$label)

              nsinfo <- lapply(split(elec_table, elec_table$labelprefix), function(x) {
                labelprefix <- x$labelprefix[[1]]
                list(
                  min_channel = min(x$electrode_id),
                  tag = shiny::tags$li(
                    sprintf(
                      "%s: %s", labelprefix,
                      dipsaus::deparse_svec(x$electrode_id)
                    )
                  )
                )
              })
              nsinfo <- nsinfo[order(sapply(nsinfo, "[[", "min_channel"))]
              nsinfo <- lapply(nsinfo, "[[", "tag")

              local_reactives$snapshot <- shiny::p(
                "With given data format (BlackRock), I found the following NEV information from ",
                "`", blocks[[1]], "`: ",
                shiny::tags$ul(
                  shiny::tags$li(
                    "Total number of channels: ", nrow(elec_table)
                  ),
                  unname(nsinfo)
                )
              )
            }, error = function(e){
              local_reactives$snapshot <- shiny::p(
                "Cannot read the BlackRock files ",
                gsub("\\.nev", ".*", basename(nev_path), ignore.case = TRUE),
                ". Please check if the file version is at least 2.3."
              )
              ravepipeline::logger_error_condition(e)
            })
          }
        },
        {
          local_reactives$snapshot <- NULL
        }
      )

      etypes <- preproc$electrode_types
      lfp_sel <- etypes %in% "LFP"
      spike_sel <- etypes %in% "Spike"
      misc_sel <- etypes %in% c("Unknown", "Audio", "Photodiode", "Auxiliary")

      lfp_channels <- preproc$electrodes[lfp_sel]
      spike_channels <- preproc$electrodes[spike_sel]
      misc_channels <- preproc$electrodes[misc_sel]

      # get composed channels
      # preproc <- ravecore::RAVEPreprocessSettings$new("demo/DemoSubject")

      # Update LFP inputs
      compose_params <- lapply(lfp_channels, function(e) {
        if(!isTRUE(preproc$data[[e]]$composed)) {
          return(NULL)
        }
        tryCatch({
          params <- preproc$data[[e]]$composed_params
          o <- order(params$from)
          weights <- params$original_weights[o]
          if(length(unique(weights)) == 1) {
            weights <- weights[[1]]
          }
          list(
            number = e,
            from = deparse_svec(params$from),
            weights = paste(weights, collapse = ","),
            normalize = ifelse(isTRUE(params$normalize_factor == 1), "no", "yes")
          )
        }, error = function(...){ NULL })
      })
      compose_params <- compose_params[!vapply(compose_params, is.null, FALSE)]
      electrodes <- lfp_channels[!lfp_channels %in% unlist(lapply(compose_params, "[[", "number"))]
      electrodes <- deparse_svec(electrodes)

      shiny::updateTextInput(
        session = session,
        inputId = "loader_lfp_channels",
        value = electrodes
      )

      lfp_srate <- preproc$sample_rates[lfp_sel]
      if(length(lfp_srate)) {
        shiny::updateNumericInput(
          session = session,
          inputId = "loader_lfp_sample_rate",
          value = lfp_srate[[1]]
        )
      }

      dipsaus::updateCompoundInput2(
        session = session,
        inputId = "loader_compose_setup",
        ncomp = length(compose_params),
        value = compose_params
      )

      # Update Microwires
      shiny::updateTextInput(
        session = session,
        inputId = "loader_microwire_channels",
        value = deparse_svec(spike_channels)
      )

      spike_srate <- preproc$sample_rates[spike_sel]
      if(length(spike_srate)) {
        shiny::updateNumericInput(
          session = session,
          inputId = "loader_microwire_sample_rate",
          value = spike_srate[[1]]
        )
      }

      # Update Auxiliary
      shiny::updateTextInput(
        session = session,
        inputId = "loader_auxiliary_channels",
        value = deparse_svec(misc_channels)
      )

      misc_srate <- preproc$sample_rates[misc_sel]
      if(length(misc_srate)) {
        shiny::updateNumericInput(
          session = session,
          inputId = "loader_auxiliary_sample_rate",
          value = misc_srate[[1]]
        )
      }


    }),
    validate_step2(),
    input$loader_electrode_file,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )


  shiny::bindEvent(
    safe_observe({
      info <- validate_step2()
      compose_setup <- local_reactives$compose_setup

      if(!is.list(info) || !isTRUE(info$valid)) {
        stop("The inputs are invalid. Please check your inputs.")
      }
      preproc <- preproc_from_info(info)
      compose_setup <- dipsaus::drop_nulls(lapply(compose_setup, function(item) {
        if(!is.list(item)) {
          if(is.character(item)) {
            stop(item)
          }
          return(NULL)
        }
        item
      }))
      if(!length(compose_setup)) {
        return()
      }
      composing_channels <- unlist(lapply(
        compose_setup, "[[", "number"
      ))
      sel <- composing_channels %in% preproc$electrodes
      composed <- composing_channels[sel]
      composing_channels <- composing_channels[!sel]

      shiny::showModal(
        shiny::modalDialog(
          title = "Compose the following channels:",
          size = "m",
          easyClose = FALSE,
          shiny::fluidRow(
            local({
              if(length(composing_channels)) {
                shiny::column(
                  width = 12,
                  shiny::p(
                    "The following NEW channels will be composed: ",
                    shiny::tags$code(class = "text-center",
                                     deparse_svec(composing_channels))
                  )
                )
              } else { NULL }
            }),
            local({
              if(length(composed)) {
                shiny::column(
                  width = 12,
                  shiny::p(
                    "*The following channels will ",
                    shiny::span(class = "text-danger", "NOT"),
                    "be composed because they have been already generated: ",
                    shiny::tags$code(class = "text-center", deparse_svec(composed)),
                    "If you have already composed these channels using the same compose settings, please ignore this message. However, if you do want to overwrite these channels using different source signals or weights, please dismiss this pop-up and choose `Validate & import`. Notice by doing so, you will need to re-run the whole preprocessing pipeline."
                  )
                )
              } else { NULL }
            })
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            local({
              if(length(composing_channels)) {
                dipsaus::actionButtonStyled(
                  inputId = ns("loader_actions_compose_do"),
                  label = "Let's go"
                )
              } else { NULL }
            })
          )
        )
      )
    }),
    input$loader_actions_compose,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    safe_observe({
      info <- validate_step2()
      compose_setup <- local_reactives$compose_setup
      if(!is.list(info) || !isTRUE(info$valid)) {
        stop("The inputs are invalid. Please check your inputs.")
      }
      preproc <- preproc_from_info(info)
      compose_setup <- as.list(drop_nulls(lapply(compose_setup, function(item) {
        if(!is.list(item)) {
          if(is.character(item)) {
            stop(item)
          }
          return(NULL)
        }
        item
      })))
      composing_channels <- unlist(lapply(
        compose_setup, "[[", "number"
      ))
      sel <- composing_channels %in% preproc$electrodes
      composed <- composing_channels[sel]
      composing_channels <- composing_channels[!sel]
      if(!length(composing_channels)) {
        stop("No channel to compose.")
      }

      compose_setup <- compose_setup[!sel]
      subject_id <- preproc$subject$subject_id

      dipsaus::shiny_alert2(
        title = "Composing channels",
        text = "Shouldn't take too long...",
        icon = "info",
        auto_close = FALSE,
        buttons = FALSE
      )

      finished <- FALSE
      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2()
        if(finished) {
          shiny::removeModal()
        }
      }, add = TRUE, after = FALSE)

      ravepipeline::lapply_jobs(
        compose_setup, function(item) {
          ravecore::compose_channel(
            subject = subject_id,
            number = item$number,
            from = item$from,
            weights = item$weights,
            normalize = item$normalize,
            force = TRUE
          )
          return()
        },
        .globals = list(
          subject_id = subject_id
        ),
        callback = function(item) {
          sprintf("Composing channel|%s", item$number)
        }
      )

      finished <- TRUE
    }),
    input$loader_actions_compose_do,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Logic for checking and importing signals
  validate_step3 <- function(skip_validation = TRUE) {
    tryCatch({
      info <- validate_step2()
      if(!is.list(info) || !isTRUE(info$initialized)) {
        stop("The inputs are invalid. Please check your inputs.")
      }
      preproc <- preproc_from_info(info)
      if(is.null(preproc)){
        stop("The inputs are invalid. Please check your inputs.")
      }
      format <- info$current_format
      blocks <- info$current_blocks
      # electrode_file <- comp$get_sub_element_input("electrode_file")

      lfp_channels <- parse_svec(input$loader_lfp_channels)
      lfp_sample_rate <- input$loader_lfp_sample_rate

      microwire_channels <- parse_svec(input$loader_microwire_channels)
      microwire_sample_rate <- input$loader_microwire_sample_rate

      auxiliary_channels <- parse_svec(input$loader_auxiliary_channels)
      auxiliary_sample_rate <- input$loader_auxiliary_sample_rate

      compose_setup <- local_reactives$compose_setup

      if(!isTRUE(format %in% seq_along(all_formats))) {
        stop("The format is invalid.")
      }
      if(!length(blocks)) {
        stop("The session block has zero length.")
      }
      if(!length(c(lfp_channels, microwire_channels, auxiliary_channels))) {
        stop("No channels will be imported.")
      }
      if(length(lfp_channels) > 0 && !isTRUE(lfp_sample_rate > 1)) {
        stop("LFP sample rate must be positive.")
      }
      if(length(microwire_channels) > 0 && !isTRUE(microwire_sample_rate > 1)) {
        stop("Microwire sample rate must be positive.")
      }
      if(length(auxiliary_channels) > 0 && !isTRUE(auxiliary_sample_rate > 1)) {
        stop("Auxiliary sample rate must be positive.")
      }

      compose_setup <- drop_nulls(lapply(compose_setup, function(item) {
        if(!is.list(item)) {
          if(is.character(item)) {
            stop(item)
          }
          return(NULL)
        }
        item
      }))

      any_imported <- any(preproc$data_imported)

      pipeline$set_settings(
        skip_validation = skip_validation,

        force_import = TRUE,

        subject_code = info$subject_code,
        project_name = info$project_name,

        import_blocks = blocks,
        import_format = all_formats[[format]],

        lfp_sample_rate = lfp_sample_rate,
        lfp_channels = deparse_svec(lfp_channels),

        auxiliary_sample_rate = auxiliary_sample_rate,
        auxiliary_channels = deparse_svec(auxiliary_channels),

        microwire_sample_rate = microwire_sample_rate,
        microwire_channels = deparse_svec(microwire_channels),

        compose_setup = compose_setup
      )

      ravedash::shiny_alert2(
        title = "Validating...",
        text = "Validating the input data... (some might take a while)",
        auto_close = FALSE, buttons = FALSE, icon = "info"
      )

      tryCatch({
        # ravepipeline::with_mirai_parallel({
          result <- pipeline$run(
            names = "validation_result",
            scheduler = "none",
            type = "vanilla"
          )
        # })

        Sys.sleep(time = 0.5)
        dipsaus::close_alert2()
        shiny::showModal(
          shiny::modalDialog(
            title = "Ready to import data",
            easyClose = FALSE, size = "l",
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              dipsaus::actionButtonStyled(
                inputId = ns("loader_do_import"),
                label = "Import data"
              )
            ),
            shiny::div(
              "Please make sure the following information is correct before proceeding: ",
              shiny::tags$ul(
                shiny::tags$li(
                  "Subject: ", preproc$subject$subject_id
                ),
                shiny::tags$li(
                  "Session blocks: ", paste(blocks, collapse = ", ")
                ),
                shiny::tags$li(
                  sprintf("Session format: %s (%s)",
                          all_formats[[format]],
                          names(all_formats)[[format]])
                ),
                shiny::tags$li(
                  "Macrowire LFP: ",
                  shiny::tags$ul(
                    shiny::tags$li("Channels: ", deparse_svec(lfp_channels)),
                    shiny::tags$li(sprintf("Sample rate: %.4f Hz", lfp_sample_rate))
                  )
                ),
                shiny::tags$li(
                  "Microwire: ",
                  shiny::tags$ul(
                    shiny::tags$li("Channels: ", deparse_svec(microwire_channels)),
                    shiny::tags$li(sprintf("Sample rate: %.4f Hz", microwire_sample_rate))
                  )
                ),
                shiny::tags$li(
                  "Auxiliary: ",
                  shiny::tags$ul(
                    shiny::tags$li("Channels: ", deparse_svec(auxiliary_channels)),
                    shiny::tags$li(sprintf("Sample rate: %.4f Hz", auxiliary_sample_rate))
                  )
                ),
                shiny::tags$li(
                  "Composing channels: ", deparse_svec(unlist(lapply(
                    as.list(compose_setup), "[[", "number"
                  )))
                )
              ),

              {
                if(any_imported){
                  "* The subject has been imported before. Proceed and you will need to re-process all other modules, including Wavelet."
                } else {
                  NULL
                }
              }

            )
          )
        )
      }, error = function(e) {
        Sys.sleep(time = 0.5)
        dipsaus::close_alert2()

        stop(e)
      })
    },
    error = function(e) {
      e <- ravepipeline::logger_error_condition(e)
      ravedash::shiny_alert2(
        title = "Validation failure",
        icon = "error",
        text = e$message,
        buttons = "Gotcha",
        danger_mode = TRUE,
        auto_close = FALSE
      )
    })
  }

  shiny::bindEvent(
    safe_observe({
      validate_step3(skip_validation = list(
        value = FALSE,
        time = as.character(Sys.time())
      ))
    }),
    input$loader_step3_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    safe_observe({
      validate_step3(skip_validation = TRUE)
    }),
    input$loader_step3_alt_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    safe_observe({
      # info <- local_reactives$info
      # preproc <- info$preproc
      #
      # format <- info$current_format
      # blocks <- info$current_blocks
      # electrode_file <- comp$get_sub_element_input("electrode_file")
      # sample_rate <- comp$get_sub_element_input("sample_rate")
      # electrodes <- dipsaus::parse_svec(comp$get_sub_element_input("electrodes"))
      #
      # pipeline <- comp$container$get_pipeline()
      #
      # settings <- ravepipeline::load_yaml(comp$container$settings_path)
      # settings <- comp$container$collect_settings(c(
      #   import_setup_id,
      #   import_blocks_id,
      #   id
      # ), map = settings)

      pipeline$set_settings(
        skip_validation = TRUE,
        force_import = TRUE
      )

      ravedash::shiny_alert2(
        title = "Importing in progress",
        text = "It's time to stand up and stretch yourself...",
        icon = "info",
        auto_close = FALSE,
        danger_mode = FALSE,
        buttons = FALSE
      )

      tryCatch({
        # ravepipeline::with_mirai_parallel({
          pipeline$run(
            scheduler = "none",
            type = "vanilla",
            return_values = FALSE
          )
        # })
        Sys.sleep(0.5)
        ravedash::close_alert2()
        shiny::removeModal()
        ravedash::shiny_alert2(
          title = "Success!",
          text = "Finished importing the data. Please proceed to the next modules.",
          icon = "success",
          danger_mode = FALSE,
          auto_close = TRUE,
          buttons = "Got it!"
        )
        ravepipeline::logger("Finished importing the data. Please proceed to the next modules.",
                             level = "success")
      }, error = function(e) {
        Sys.sleep(0.5)
        ravedash::close_alert2()
        ravepipeline::logger_error_condition(e)
        ravedash::shiny_alert2(
          title = "Error",
          text = paste(c(
            "Found errors while trying to import data: ",
            e$message
          ), collapse = "\n"),
          icon = "error",
          danger_mode = TRUE,
          auto_close = TRUE,
          buttons = "Dismiss"
        )
      })
    }),
    input$loader_do_import,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )



}

