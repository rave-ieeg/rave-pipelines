# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(
        width = 8L,
        ravedash::input_card(
          title = "Data settings",
          class_header = "",

          shiny::p(
            shiny::tags$small(
              "This module is NOT mandatory for YAEL. You may integrate your own pipelines. ",
              "Please check ", shiny::a(href = "https://yael.wiki", target = "_blank", "yael.wiki"),
              " or ask the developers for more information.",
              shiny::br(),
              "Some functions require optional installation of `dcm2niix`, `FreeSurfer`, or `FSL`. ",
              "Please consider installing them if you want to use these functions. "
            )
          ),

          ravedash::flex_group_box(
            title = "Project and Subject",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              loader_subject$ui_func()
            )
          ),

          shiny::hr(),

          shiny::fluidRow(
            shiny::column(
              width = 12L,
              shiny::tags$small("If you have already imported MRI and CT before, you may ignore these inputs."),
              shiny::br(),
              shiny::tags$small("If this is the first time that you pre-process this subject, please choose carefully.")
            ),
            shiny::column(
              width = 6L,
              ravedash::flex_group_box(
                title = "MRI",
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("mri_path"),
                      label = "Raw MRI (DICOM folder or Nifti file)",
                      choices = character(0L)
                    ),
                    inputId = "mri_path",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select] Raw pre-surgery T1 MRI to import: a NIfTI file or a DICOM folder,",
                      "as a path relative to the subject's raw folder. Choices load once the",
                      "subject is chosen. Only needed when the subject's MRI was never imported.",
                      "`\"[Upload]\"` opens an upload dialog that only a person can use: never",
                      "choose it. Read by script `load_data`; the import is script `import_t1`."
                    ),
                    tooltip = "Pre-surgery T1 MRI to import: a NIfTI file or a DICOM folder."
                  )
                )
              #   shidashi::flex_break(),
              #   shidashi::flex_item(
              #     shiny::fileInput(
              #       inputId = ns("mri_path_upload"),
              #       label = "Please upload",
              #       value = FALSE
              #     )
              #   )
              )
            ),
            shiny::column(
              width = 6L,
              ravedash::flex_group_box(
                title = "CT",
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("ct_path"),
                      label = "Raw CT (DICOM folder or Nifti file)",
                      choices = character(0L)
                    ),
                    inputId = "ct_path",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select] Raw post-implant CT to import: a NIfTI file or a DICOM folder, as",
                      "a path relative to the subject's raw folder. Choices load once the subject",
                      "is chosen. Only needed when the subject's CT was never imported.",
                      "`\"[Upload]\"` opens an upload dialog that only a person can use: never",
                      "choose it. Read by script `load_data`; the import is script `import_ct`."
                    ),
                    tooltip = "Post-implant CT to import: a NIfTI file or a DICOM folder."
                  )
                )
                # shidashi::flex_break(),
                # shidashi::flex_item(
                #   shiny::checkboxInput(
                #     inputId = ns("skip_coregistration"),
                #     label = "Skip the CT co-registration",
                #     value = FALSE
                #   )
                # )
              )
            )
          ),


          footer = shiny::tagList(
            loader_sync1$ui_func(),
            shiny::br(),
            loader_sync2$ui_func()
          )

        )
      ),

      shiny::column(
        width = 4L,

        ravedash::input_card(
          title = "Command-line (optional)",
          class_header = "",

          ravedash::flex_group_box(
            title = "Command-line path",
            shidashi::flex_item(
              shidashi::register_input(
                shiny::textInput(
                  inputId = ns("cmd_dcm2niix_path"),
                  label = "Dcm2niix path (needed to convert DICOM images to Nifti format, optional)",
                  value = ravecore::cmd_dcm2niix(error_on_missing = FALSE, unset = "")
                ),
                inputId = "cmd_dcm2niix_path",
                update = "shiny::updateTextInput",
                description = paste(
                  "[Text, read-only for agents] Path to the dcm2niix program; filled in when RAVE finds it.",
                  "A path entered here is saved to RAVE's settings for every subject, so agents",
                  "must never change it: ask the user. Read by script `load_data`."
                ),
                tooltip = "Path to the dcm2niix program, needed to import DICOM folders.",
                writable = FALSE
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::textInput(
                  inputId = ns("cmd_fs_path"),
                  label = "FreeSurfer home (`FREESURFER_HOME`, needed for surface reconstruction, optional)",
                  value = ravecore::cmd_freesurfer_home(error_on_missing = FALSE, unset = "")
                ),
                inputId = "cmd_fs_path",
                update = "shiny::updateTextInput",
                description = paste(
                  "[Text, read-only for agents] Path to the FreeSurfer home folder; filled in when RAVE finds it.",
                  "A path entered here is saved to RAVE's settings for every subject, so agents",
                  "must never change it: ask the user. Read by script `load_data`."
                ),
                tooltip = "FreeSurfer home folder (`FREESURFER_HOME`), needed for surface reconstruction.",
                writable = FALSE
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::textInput(
                  inputId = ns("cmd_fsl_path"),
                  label = "FSL home (`FSLDIR`, needed for co-registration using FLIRT, optional)",
                  value = ravecore::cmd_fsl_home(error_on_missing = FALSE, unset = "")
                ),
                inputId = "cmd_fsl_path",
                update = "shiny::updateTextInput",
                description = paste(
                  "[Text, read-only for agents] Path to the FSL home folder; filled in when RAVE finds it.",
                  "A path entered here is saved to RAVE's settings for every subject, so agents",
                  "must never change it: ask the user. Read by script `load_data`."
                ),
                tooltip = "FSL home folder (`FSLDIR`), needed for co-registration with FLIRT.",
                writable = FALSE
              )
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              shidashi::register_input(
                shiny::textInput(
                  inputId = ns("cmd_afni_path"),
                  label = "AFNI home (optional)",
                  value = ravecore::cmd_afni_home(error_on_missing = FALSE, unset = "")
                ),
                inputId = "cmd_afni_path",
                update = "shiny::updateTextInput",
                description = paste(
                  "[Text, read-only for agents] Path to the AFNI home folder; filled in when RAVE finds it.",
                  "A path entered here is saved to RAVE's settings for every subject, so agents",
                  "must never change it: ask the user. Read by script `load_data`."
                ),
                tooltip = "AFNI home folder, needed for co-registration with AFNI.",
                writable = FALSE
              )
            )
          ),

          footer = shiny::tagList(
            ravedash::load_data_button(label = "Check data and command-line tools", width = "100%")
          )
        )
      )

    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...) {

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed

  local_data <- dipsaus::fastmap2()

  register_file_uploader <- function(type = "mri", title = "Upload presurgery MRI") {

    image_path_inputId <- sprintf("%s_path", type)
    image_path_dismiss_btn <- sprintf("%s_upload_dismiss", type)
    image_path_upload_inputId <- sprintf("%s_upload", type)
    image_selector_choice_name <- sprintf("%s_path_choices", type)

    shiny::bindEvent(
      ravedash::safe_observe({
        input_choice <- input[[image_path_inputId]]
        if (!length(input_choice)) { return() }
        if ( identical(input_choice, "[Upload]")) {

          subject_code <- loader_subject$get_sub_element_input()
          if (!length(subject_code)) {
            shiny::updateSelectInput(session = session, inputId = image_path_inputId,
                                     selected = " ")
          }

          max_size <- getOption("shiny.maxRequestSize", 300 * 1024^2)
          if ( max_size < 300 * 1024^2) {
            options("shiny.maxRequestSize" = 300 * 1024^2)
          }

          shiny::showModal(shiny::modalDialog(
            title = title,
            size = "m",
            easyClose = FALSE,
            footer = shiny::actionButton(ns(image_path_dismiss_btn), label = "Cancel"),
            tryCatch({
              dipsaus::fancyFileInput(
                inputId = ns( image_path_upload_inputId ),
                label = "Please upload a NIfTI file (.nii or .nii.gz)",
                multiple = FALSE, accept = c(".nii,.gz"),
                width = "100%", size = "m"
              )
            }, error = function(e) {
              shiny::fileInput(
                inputId = ns( image_path_upload_inputId ),
                label = "Please upload a NIfTI file (.nii or .nii.gz)",
                multiple = FALSE, accept = c(".nii,.gz"),
                width = "100%", placeholder = "Use button or drag & drop"
              )
            })
          ))
        } else {
          local_data[[image_path_inputId]] <- input_choice
        }
      }),
      input[[image_path_inputId]],
      ignoreNULL = TRUE, ignoreInit = FALSE
    )
    shiny::bindEvent(
      ravedash::safe_observe({
        current_val <- as.character(local_data[[ image_path_inputId ]])
        if (length(current_val)) {
          shiny::updateSelectInput(session = session, inputId = image_path_inputId,
                                   selected = current_val)
        } else {
          shiny::updateSelectInput(session = session, inputId = image_path_inputId,
                                   selected = " ")
        }

        shiny::removeModal()
      }),
      input[[ image_path_dismiss_btn ]],
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
    shiny::bindEvent(
      ravedash::safe_observe({
        finfo <- input[[ image_path_upload_inputId ]]

        if (nrow( finfo ) != 1) {
          stop("Please upload a valid NIfTI file.")
        }
        project_name <- loader_project$get_sub_element_input()
        subject_code <- loader_subject$get_sub_element_input()
        if (!length(subject_code)) {
          shiny::removeModal()
          stop("Invalid subject code. Please choose a valid one before uploading.")
        }

        subject <- ravecore::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
        selected <- file.path("rave-uploads", toupper(type), finfo$name)


        upload_path <- file.path(subject$preprocess_settings$raw_path,
                                 "rave-uploads", toupper(type))
        upload_path <- ravepipeline::dir_create2(upload_path)

        file.copy(finfo$datapath, file.path(upload_path, finfo$name),
                  overwrite = TRUE, recursive = FALSE)

        image_selector_choices <- unique(c( selected, local_data[[ image_selector_choice_name ]] ))
        local_data[[ image_selector_choice_name ]] <- image_selector_choices

        shiny::updateSelectInput(
          session = session, inputId = image_path_inputId, selected = selected,
          choices = c("[Upload]", " ", image_selector_choices)
        )
        shiny::removeModal()
      }, error_wrapper = "notification"),
      input[[ image_path_upload_inputId ]],
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
  }

  register_file_uploader(type = "mri", title = "Upload presurgery MRI")
  register_file_uploader(type = "ct", title = "Upload post-implant CT")

  # Check the data and command-line tools, then ask the user to confirm;
  # runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    {
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code"
        )
      )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline$set_settings(
        path_mri = input$mri_path,
        path_ct = input$ct_path,
        skip_recon = FALSE,
        skip_coregistration = FALSE,
        dcm2niix_path = input$cmd_dcm2niix_path,
        freesurfer_path = input$cmd_fs_path,
        fsl_path = input$cmd_fsl_path,
        afni_path = input$cmd_afni_path,
        .list = settings
      )

      pipeline$run(
        names = "check_result",
        type = "vanilla",
        scheduler = "none"
      )

      check_result <- pipeline$read("check_result")
      cmd_tools <- pipeline$read("cmd_tools")

      msg_ui <- NULL
      warn_ui <- NULL
      if (length(check_result$messages)) {
        msg_ui <- shiny::tagList(
          shiny::p("The following pipeline will run:"),
          shiny::tags$ul(
            lapply(check_result$messages, shiny::tags$li)
          )
        )
      }

      if (length(check_result$warnings)) {
        warn_ui <- shiny::tagList(
          shiny::p("Please check the following warnings:"),
          shiny::tags$ul(
            lapply(check_result$warnings, shiny::tags$li)
          )
        )
      }
      cmd_ui <- shiny::tagList(
        shiny::p("External commandline paths. "),
        shiny::tags$ul(
          shiny::tags$li(shiny::strong("dcm2niix"), ": ", paste(cmd_tools$dcm2niix, collapse = "")),
          shiny::tags$li(shiny::strong("FreeSurfer"), ": ", paste(cmd_tools$freesurfer, collapse = "")),
          shiny::tags$li(shiny::strong("FSL-flirt"), ": ", paste(cmd_tools$flirt, collapse = "")),
          shiny::tags$li(shiny::strong("AFNI"), ": ", paste(cmd_tools$afni, collapse = ""))
        )
      )

      shiny::showModal(shiny::modalDialog(
        title = "Confirmation",
        size = "l",
        shiny::p("Please confirm the to-do list and carefully read warnings. ",
                 "Please read warnings ",
                 "that have statements such as ",
                 '"the script will fail/error".'),
        msg_ui,
        warn_ui,
        cmd_ui,

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          dipsaus::actionButtonStyled(ns("loader_ready_btn2"), "Proceed")
        )
      ))

      # What the dialog shows, printed for agents (MCP)
      tool_path <- function(x) {
        x <- paste(x[!is.na(x)], collapse = "")
        if (nzchar(x)) { x } else { "not found" }
      }
      summary <- c(
        sprintf("subject=%s/%s", check_result$project_name, check_result$subject_code),
        sprintf("raw_mri=%s", paste(check_result$path_mri, collapse = "")),
        sprintf("raw_ct=%s", paste(check_result$path_ct, collapse = "")),
        sprintf("todo=%s", check_result$messages),
        sprintf("warning=%s", check_result$warnings),
        sprintf("dcm2niix=%s", tool_path(cmd_tools$dcm2niix)),
        sprintf("freesurfer=%s", tool_path(cmd_tools$freesurfer)),
        sprintf("fsl_flirt=%s", tool_path(cmd_tools$flirt)),
        sprintf("afni=%s", tool_path(cmd_tools$afni)),
        "next=dialog 'Confirmation' is open: click 'Proceed' (`loader_ready_btn2`) to load"
      )
      cat(summary, sep = "\n")
      summary
    },
    description = c(
      "Same as clicking 'Check data and command-line tools'. Reads the subject",
      "(`loader_project_name`, `loader_subject_code`), the raw images `mri_path`",
      "and `ct_path`, and the command-line paths; saves them to the pipeline",
      "settings, checks the subject and the programs, and opens the 'Confirmation'",
      "dialog. Data are NOT loaded until 'Proceed' is clicked: `shiny_ui_operate`",
      "action `click`, target `loader_ready_btn2`. Output and result: the",
      "subject, raw image paths, to-do lines, warnings (tell them to the user;",
      "they never block 'Proceed'), and program paths ('not found' when missing).",
      "Then run script `imaging_status`."
    ),
    binding_event = "load_data",
    # No `dispatch_event`: the data are loaded once the user clicks
    # "Proceed" in the confirmation (`input$loader_ready_btn2`)
    alert_params = list(title = "Checking data and command-line tools")
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::removeModal()
      # Let the module know the data has been changed
      ravedash::fire_rave_event("data_changed", Sys.time())
      ravepipeline::logger("Data has been loaded loaded")

      # Save session-based state: project name & subject code
      ravedash::session_setopt(
        project_name = pipeline$read("project_name"),
        subject_code = pipeline$read("subject_code")
      )
    }),
    input$loader_ready_btn2,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  local_cache <- dipsaus::fastmap2()

  shiny::bindEvent(
    ravedash::safe_observe({
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()
      if (!length(project_name) || !length(subject_code)) { return() }
      subject_code <- trimws(paste(subject_code, collapse = ""))
      if (!nzchar(subject_code)) { return() }

      if (is.null(local_cache$subject) || !isTRUE(local_cache$subject$subject_id == sprintf("%s/%s", project_name, subject_code))) {
        local_cache$subject <- ravecore::RAVESubject$new(
          project_name = project_name,
          subject_code = subject_code,
          strict = FALSE)
      }
      subject <- local_cache$subject

      if (!length(subject)) { return() }

      # paths <- list.dirs(subject$preprocess_settings$raw_path, full.names = FALSE, recursive = TRUE)
      # check BIDS path
      paths_bids <- NULL
      if (subject$preprocess_settings$raw_path2_type == "bids") {
        paths_bids <- list.files(
          subject$preprocess_settings$raw_path2,
          pattern = "nii($|\\.gz$)",
          ignore.case = TRUE,
          all.files = TRUE,
          full.names = FALSE,
          include.dirs = FALSE,
          no.. = TRUE,
          recursive = TRUE
        )
      }
      paths_native <- list.files(
        subject$preprocess_settings$raw_path,
        all.files = TRUE,
        full.names = FALSE,
        include.dirs = TRUE,
        no.. = TRUE,
        recursive = TRUE
      )
      paths_native <- paths_native[
        dir.exists(file.path(subject$preprocess_settings$raw_path, paths_native)) |
        grepl("nii($|\\.gz$)", x = paths_native, ignore.case = TRUE)
      ]
      paths_native <- paths_native[!paths_native %in% c("", ".", "..", "/")]
      paths_native <- paths_native[!startsWith(paths_native, "rave-imaging")]
      paths <- c(paths_bids, paths_native)
      local_data$mri_path_choices <- paths
      local_data$ct_path_choices <- paths

      selected <- NULL
      if (length(paths)) {
        selected <- c(paths[startsWith(paths, file.path("rave-uploads", "MRI", ""))],
                      paths[grepl("_T1w.nii", paths)], paths[grepl("MR", paths)])
        if (length(selected)) {
          selected <- selected[[1]]
        }
      }
      selected <- c(
        subject$get_default("raw_mri_path", namespace = pipeline$pipeline_name),
        selected
      )
      if ( identical(pipeline$get_settings("subject_code"), subject_code) ) {
        selected <- c(pipeline$get_settings("path_mri"), selected)
      }
      selected <- selected %OF% paths
      if (!length(selected)) { selected <- " " }
      shiny::updateSelectInput(
        session = session,
        inputId = "mri_path",
        choices = c("[Upload]", " ", paths),
        selected = selected
      )

      selected <- NULL
      if (length(paths)) {
        selected <- c(paths[startsWith(paths, file.path("rave-uploads", "CT", ""))],
                      paths[grepl("CT", paths)])
        if (length(selected)) {
          selected <- selected[[1]]
        }
      }
      selected <- c(
        subject$get_default("raw_ct_path", namespace = pipeline$pipeline_name),
        selected
      )
      if ( identical(pipeline$get_settings("subject_code"), subject_code) ) {
        selected <- c(pipeline$get_settings("path_ct"), selected)
      }
      selected <- selected %OF% paths
      if (!length(selected)) { selected <- " " }
      shiny::updateSelectInput(session = session, inputId = "ct_path", choices = c("[Upload]", " ", paths), selected = selected)

      fs_reconstructed <- FALSE
      fs_path <- subject$freesurfer_path
      if (length(fs_path) == 1 && !is.na(fs_path) && isTRUE(dir.exists(fs_path))) {
        fs_reconstructed <- threeBrain::check_freesurfer_path(
          fs_path,
          autoinstall_template = FALSE,
          check_volume = TRUE,
          check_surface = FALSE
        )
      }
      shiny::updateCheckboxInput(
        session = session,
        inputId = "skip_recon",
        value = fs_reconstructed
      )

    }),
    loader_subject$get_sub_element_input(),
    loader_project$get_sub_element_input(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


}
