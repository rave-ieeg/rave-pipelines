# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container-fluid", style = "max-width: 1600px",
    shiny::fluidRow(
      shiny::column(
        width = 5L,
        ravedash::input_card(
          title = "Subject Selection",
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
              shiny::br(),
              loader_sync3$ui_func()
            )
          )
        ),
        shidashi::card_tabset(
          title = "Image Selection",
          inputId = ns("image_selection_tabset"),
          class_header = "",
          class_body = "padding-10",
          "Preprocess" = ravedash::flex_group_box(
            title = "Fast preprocess on the fly",
            shidashi::flex_break(),
            shidashi::flex_item(
              dipsaus::fancyFileInput(ns("loader_preprocess_t1"), "Upload T1w.nii[.gz]", size = "s")
            ),
            shidashi::flex_item(
              dipsaus::fancyFileInput(ns("loader_preprocess_ct"), "Upload CT.nii[.gz]", size = "s")
            ),
            shidashi::flex_break(),
            shidashi::flex_item(
              dipsaus::actionButtonStyled(ns("loader_preprocess_run"), "Run YAEL preprocess pipeline", width = "100%")
            )
          ),
          "Data selector" = ravedash::flex_group_box(
            title = "Image data to load",

            shidashi::flex_item(
              shiny::selectInput(
                inputId = ns("loader_method"),
                label = "Localization method",
                choices = c(
                  "CT (IJK) to MR (RAS) transform + Raw CT",
                  "FSL transform + Raw CT + MRI",
                  "Re-sampled CT",
                  "Localize without CT"
                )
              )
            ),

            shidashi::flex_break(),

            shiny::conditionalPanel(
              condition = sprintf("input['%s']!=='Localize without CT'", ns("loader_method")),
              class = "padding-5",
              style = "flex:1; ",

              shidashi::flex_container(
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("loader_ct_fname"),
                    label = "Choose CT",
                    choices = character(0L)
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s']!=='Re-sampled CT'", ns("loader_method")),

                shidashi::flex_container(
                  shidashi::flex_item(
                    shiny::selectInput(
                      inputId = ns("loader_mri_fname"),
                      label = "Choose raw MRI",
                      choices = character(0L)
                    )
                  )
                ),
                shidashi::flex_container(
                  shidashi::flex_item(
                    shiny::selectInput(
                      inputId = ns("loader_transform_fname"),
                      label = "Transform matrix",
                      choices = character(0L)
                    )
                  )
                )

              ),

              shiny::div(
                class = "float-right",
                shiny::actionLink(ns("loader_ct_refresh"), "Refresh file list")
              )

            )
          )
        )
      ),
      shiny::column(
        width = 7L,
        ravedash::input_card(
          title = "Electrode Plan",
          class_header = "",
          append_tools = FALSE,
          tools = list(
            shidashi::card_tool(
              inputId = ns("loader_plan_upload_wizard"),
              title = "Upload electrode plan table",
              widget = "custom",
              icon = ravedash::shiny_icons$upload
            ),
            # shiny::tags$a(
            #   id = ns("loader_plan_download_wizard"),
            #   class = "btn btn-tool shiny-download-link disabled",
            #   href = "", target = "_blank", download = NA,
            #   `aria-disabled` = "true", tabindex = "-1", title = "Download electrode plan table")
            shiny::downloadLink(
              outputId = ns("loader_plan_download_wizard"),
              label = NULL,
              class = "btn btn-tool",
              title = "Download electrode plan table",
              ravedash::shiny_icons$download
            )
          ),
          dipsaus::compoundInput2(
            max_height = "80vh",
            inputId = ns("loader_plan"),
            label = "Electrode group",
            initial_ncomp = 1L,
            min_ncomp = 1L,
            max_ncomp = 100L,
            label_color = "#c8c9ca",
            components = shidashi::flex_container(
              class = "margin-m10",
              shidashi::flex_item(size = 2, shiny::textInput("label", "GroupLabel")),
              shidashi::flex_item(size = 2, shiny::textInput("dimension", "Channels")),
              shidashi::flex_item(size = 4, shiny::selectInput("type", "Type", choices = electrode_types)),
              shidashi::flex_item(size = 1, shiny::selectInput("hemisphere", "Hemisphere",
                                                     choices = c("auto", "left", "right"))),
              shidashi::flex_break(),
              shidashi::flex_item(shiny::tags$small(
                shiny::textOutput("info", inline = TRUE)
              ))
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
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  get_plan <- shiny::debounce(shiny::reactive({
    input$loader_plan
  }), millis = 300)

  local_reactives <- shiny::reactiveValues()
  local_data <- dipsaus::fastmap2()

  shiny::bindEvent(
    ravedash::safe_observe({
      plan <- summarize_plan_list( get_plan() )

      lapply(seq_along(plan), function(ii) {
        item <- plan[[ii]]
        if(is.null(item)) {
          msg <- "No channel in this group. This group will be skipped."
        } else {
          msg <- item$msg
        }
        session$sendCustomMessage(
          "shidashi.set_html",
          list(
            selector = sprintf("#%s", ns(sprintf("loader_plan_info_%s", ii))),
            content = msg
          )
        )
      })

    }),
    get_plan(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::showModal(shiny::modalDialog(
        title = "Upload electrode plan table",
        size = "m",
        dipsaus::fancyFileInput(
          inputId = session$ns("loader_plan_upload"),
          label = NULL, width = "100%", size = "m"
        )
      ))
    }),
    input$loader_plan_upload_wizard,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  output$loader_plan_download_wizard <- shiny::downloadHandler(
    filename = "electrode_plan.csv",
    content = function(con) {
      plan_table <- lapply(get_plan(), function(x) {
        data.frame(
          GroupLabel = x$label[[1]],
          Channels = x$dimension[[1]],
          Type = x$type[[1]],
          Hemisphere = x$hemisphere[[1]]
        )
      })
      plan_table <- data.table::rbindlist(plan_table)
      data.table::fwrite(plan_table, con, row.names = FALSE)
    }
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      file_info <- input$loader_plan_upload
      if(!length(file_info) || !length(file_info$datapath)) {
        return()
      }
      plan_table <- utils::read.csv(file_info$datapath)
      plan_table_names <- tolower(names(plan_table))
      names(plan_table) <- plan_table_names

      plan_list <- NULL

      # case 1: With GroupLabel, Channels, Type, Hemisphere
      if(all(c("grouplabel", "channels") %in% plan_table_names)) {
        plan_list <- lapply(split(plan_table, as.character(plan_table$grouplabel)), function(sub) {
          grouplabel <- sub$grouplabel[[1]]
          channels <- dipsaus::parse_svec(sub$channels)
          min_channel <- min(channels, na.rm = TRUE)
          if(length(channels) == 1) {
            channels <- sprintf("%.0f-%.0f", channels, channels)
          } else {
            channels <- dipsaus::deparse_svec(channels)
          }
          type <- unique(sub$type)
          type <- type[type %in% electrode_types]
          if(length(type)) {
            type <- type[[1]]
          } else {
            type <- "iEEG"
          }
          hemisphere <- unique(tolower(c(sub$hemisphere, "auto")))
          hemisphere <- hemisphere[hemisphere %in% c("auto", "left", "right")]
          hemisphere <- hemisphere[[1]]
          list(
            label = grouplabel,
            dimension = channels,
            type = type,
            hemisphere = hemisphere,
            min_channel = min_channel
          )
        })
      }

      # case 2: electrodes.csv or similar to that
      if("electrode" %in% plan_table_names && any(c("labelprefix", "label") %in% plan_table_names)) {
        if(!length(plan_table$labelprefix)) {
          plan_table$labelprefix <- gsub("[0-9]+$", "", x = plan_table$label)
        }
        plan_list <- lapply(split(plan_table, plan_table$labelprefix), function(sub) {
          channels <- dipsaus::parse_svec(sub$electrode)
          min_channel <- min(channels, na.rm = TRUE)
          if(length(channels) == 1) {
            channels <- sprintf("%.0f-%.0f", channels, channels)
          } else {
            channels <- dipsaus::deparse_svec(channels)
          }
          grouplabel <- sub$labelprefix[[1]]
          type <- unique(c(sub$Prototype, "iEEG"))
          type <- type[type %in% electrode_types][[1]]
          hemisphere <- unique(tolower(c(sub$hemisphere, "auto")))
          hemisphere <- hemisphere[hemisphere %in% c("auto", "left", "right")]
          hemisphere <- hemisphere[[1]]
          list(
            label = grouplabel,
            dimension = channels,
            type = type,
            hemisphere = hemisphere,
            min_channel = min_channel
          )
        })
      }

      if(length(plan_list)) {
        plan_list <- plan_list[order(sapply(plan_list, "[[", "min_channel"))]
        dipsaus::updateCompoundInput2(
          session = session,
          inputId = "loader_plan",
          value = plan_list,
          ncomp = length(plan_list)
        )
      } else {
        stop("No electrode plan is found. Please make sure the format is correct or simply manually enter the electrode plans.")
      }

      shiny::removeModal(session = session)

    }, error_wrapper = "notification"),
    input$loader_plan_upload,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      clip_text <- local_data$clip_text
      title <- local_data$clip_title
      if( !length(clip_text) ) { return() }
      if( !length(title) ) { title <- "Notification!" }

      local_data$clip_text <- NULL
      local_data$clip_title <- NULL

      shidashi::show_notification(
        message = shiny::div(
          paste(local_data$clip_message, collapse = ""),
          shiny::hr(),
          shiny::pre(
            class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
            `data-dismiss`="toast",
            type = "button",
            `aria-label`="Close",
            `data-clipboard-text` = clip_text,
            shiny::code( clip_text )
          )
        ),
        title = title,
        autohide = FALSE, close = TRUE,
        icon = ravedash::shiny_icons$terminal, session = session
      )
    }),
    local_reactives$show_clipboard_notification,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  show_notification_with_clipboard <- function(title, message, clip_text) {
    local_data$clip_text <- clip_text
    local_data$clip_title <- title
    local_data$clip_message <- message
    local_reactives$show_clipboard_notification <- Sys.time()
  }

  load_coreg_params <- function() {
    project_name <- loader_project$get_sub_element_input()
    subject_code <- loader_subject$get_sub_element_input()
    if(!loader_subject$sv$is_valid() || !length(project_name) || !length(subject_code) ||
       is.na(project_name) || is.na(subject_code) || project_name == '' || subject_code == '') {
      return()
    }
    subject <- ravecore::RAVESubject$new(project_name = project_name,
                                       subject_code = subject_code,
                                       strict = FALSE)
    fs_path <- subject$freesurfer_path
    if( length(fs_path) != 1 || is.na(fs_path) || !file.exists(fs_path) ) { return() }

    path_coreg_conf <- file.path(subject$preprocess_settings$raw_path, "rave-imaging",
                                 "derivative", "conf-coregistration.yaml")
    coreg_files <- list.files(
      file.path(subject$preprocess_settings$raw_path,
                "rave-imaging",
                "coregistration")
    )

    localization_method <- "Re-sampled CT"

    ct_name <- character()
    mri_name <- character()
    trans_name <- NULL

    if( file.exists(path_coreg_conf) ) {
      coreg_conf <- ravepipeline::load_yaml(path_coreg_conf)

      # check outputs
      if("CT_IJK_to_MR_RAS" %in% names(coreg_conf$outputs)) {
        localization_method <- "CT (IJK) to MR (RAS) transform + Raw CT"
        ct_name <- unique(basename(coreg_conf$input_image$backup))
        ct_name <- ct_name[ct_name %in% coreg_files]
        mri_name <- unique(basename(coreg_conf$reference_image$backup))
        mri_name <- mri_name[mri_name %in% coreg_files]
        trans_name <- basename(coreg_conf$outputs$CT_IJK_to_MR_RAS$path)
      } else if ("ct2t1" %in% names(coreg_conf$outputs)) {
        localization_method <- "FSL transform + Raw CT + MRI"
        ct_name <- unique(basename(coreg_conf$input_image$backup))
        ct_name <- ct_name[ct_name %in% coreg_files]
        mri_name <- unique(basename(coreg_conf$reference_image$backup))
        mri_name <- mri_name[mri_name %in% coreg_files]
        trans_name <- unique(basename(coreg_conf$outputs$ct2t1$path))
      } else if ("ct_in_t1" %in% names(coreg_conf$outputs)) {
        localization_method <- "Re-sampled CT"
        ct_name <- basename(coreg_conf$outputs$ct_in_t1$path)
        mri_name <- NULL
        trans_name <- NULL
      }
    } else {
      selected_method <- subject$get_default(
        "transform_space", default_if_missing = NULL,
        namespace = "electrode_localization")
      if( length(selected_method) == 1 ) {
        localization_method <- names(LOCALIZATION_METHODS)[
          unlist(LOCALIZATION_METHODS) == selected_method]
      }
      ct_name <- subject$get_default(
        "path_ct", default_if_missing = NULL,
        namespace = "electrode_localization")
      if(length(ct_name) == 1) {
        ct_name <- basename(ct_name)
        ct_name <- ct_name[ct_name %in% coreg_files]
      }
      if(!length(ct_name)) {
        ct_name <- coreg_files[grepl("^CT.*\\.nii(\\.gz|)$", coreg_files)]
        if(length(ct_name)) { ct_name <- ct_name[[1]] }
      }

      mri_name <- subject$get_default(
        "path_mri", default_if_missing = shiny::isolate(input$loader_mri_fname),
        namespace = "electrode_localization")
      if(length(mri_name) == 1) {
        mri_name <- basename(mri_name)
        mri_name <- mri_name[mri_name %in% coreg_files]
      }
      if(!length(mri_name)) {
        mri_name <- coreg_files[grepl("^MR.*\\.nii(\\.gz|)$", coreg_files)]
        if(length(mri_name)) { mri_name <- mri_name[[1]] }
      }

      trans_name <- subject$get_default(
        "path_transform", default_if_missing = shiny::isolate(input$loader_transform_fname),
        namespace = "electrode_localization")
      if(length(trans_name) == 1) {
        trans_name <- basename(trans_name)
        trans_name <- trans_name[trans_name %in% coreg_files]
      }
      if(!length(trans_name)) {
        trans_name <- coreg_files[grepl("\\.(mat|txt)$", coreg_files)]
        if(length(trans_name)) { trans_name <- trans_name[[1]] }
      }

      if( length(trans_name) == 1 &&
          (
            identical(localization_method, "Re-sampled CT") ||
            !isTRUE(localization_method %in% names(LOCALIZATION_METHODS))
          ) ) {
        if( startsWith(trans_name, "CT_IJK") ) {
          localization_method <- "CT (IJK) to MR (RAS) transform + Raw CT"
        } else if ( startsWith(trans_name, "ct2t1") ) {
          localization_method <- "FSL transform + Raw CT + MRI"
        }
      }
    }

    return(list(
      method = localization_method,
      ct_filename = ct_name,
      mr_filename = mri_name,
      transform_filename = trans_name,
      project_name = project_name,
      subject_code = subject_code,
      subject = subject
    ))

  }

  refresh_ct_chocies <- function(value_ct = NULL, value_mri = NULL, value_transform = NULL, reset_method = FALSE){
    coreg_params <- load_coreg_params()

    # coreg_params <- list(
    #   method = '',
    #   ct_filename = ct_name,
    #   mr_filename = mri_name,
    #   transform_filename = trans_name,
    #   project_name = project_name,
    #   subject_code = subject_code,
    #   subject = subject
    # )

    if( !length(coreg_params) ) {
      shiny::updateSelectInput(
        session = session, inputId = "loader_ct_fname",
        choices = character(0L)
      )
      shiny::updateSelectInput(
        session = session, inputId = "loader_mri_fname",
        choices = character(0L)
      )
      shiny::updateSelectInput(
        session = session, inputId = "loader_transform_fname",
        choices = character(0L)
      )
      shidashi::card_tabset_activate(
        inputId = "image_selection_tabset",
        title = "Preprocess",
        notify_on_failure = FALSE
      )
      return()
    }

    subject <- coreg_params$subject

    coreg_path <- file.path(subject$preprocess_settings$raw_path, "rave-imaging", "coregistration")

    nifti_files <- list.files(coreg_path, pattern = "nii(?:\\.gz)?$",
                              recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
                              full.names = FALSE, all.files = FALSE)
    transform_files <- list.files(coreg_path, pattern = "(mat|txt)$",
                                  recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
                                  full.names = FALSE, all.files = FALSE)
    transform_files <- unique(c(transform_files[transform_files %in% c("CT_IJK_to_MR_RAS.txt", "ct2t1.mat")], transform_files))


    shiny::updateSelectInput(
      session = session, inputId = "loader_method", selected = coreg_params$method)
    shiny::updateSelectInput(
      session = session, inputId = "loader_ct_fname", choices = nifti_files,
      selected = coreg_params$ct_filename
    )
    shiny::updateSelectInput(
      session = session, inputId = "loader_mri_fname", choices = nifti_files,
      selected = coreg_params$mr_filename
    )
    shiny::updateSelectInput(
      session = session, inputId = "loader_transform_fname", choices = transform_files,
      selected = coreg_params$transform_filename
    )

    if(!length(coreg_params$ct_filename) && !isTRUE(coreg_params$method %in% "Localize without CT")) {
      shidashi::card_tabset_activate(
        inputId = "image_selection_tabset",
        title = "Preprocess",
        notify_on_failure = FALSE
      )
    } else {
      shidashi::card_tabset_activate(
        inputId = "image_selection_tabset",
        title = "Data selector",
        notify_on_failure = FALSE
      )
    }

    plan <- read_plan_list( file.path(subject$meta_path, c("electrodes_unsaved.csv", "electrodes.csv")) )

    dipsaus::updateCompoundInput2(
      session = session,
      inputId = "loader_plan",
      value = plan,
      ncomp = length(plan)
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      refresh_ct_chocies()
    }),
    input$loader_ct_refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({

      if(!loader_project$sv$is_valid() || !loader_subject$sv$is_valid()) {
        loading_error("Invalid project/subject. Please specify a valid subject first before uploading CT.")
        return()
      }
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()

      subject <- ravecore::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code,
                                           strict = FALSE)

      dipsaus::shiny_alert2(
        title = "Checking current environment",
        icon = "info",
        text = "Loading Python environment and checking images...",
        auto_close = FALSE, buttons = FALSE, session = session
      )
      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2()
      })

      yael_process <- ravecore::as_yael_process(subject)

      t1w <- input$loader_preprocess_t1
      if(length(t1w)) {
        if(endsWith(tolower(t1w$name), "nii.gz")) {
          new_path <- gsub(
            pattern = "gz$",
            replacement = "nii.gz",
            x = t1w$datapath,
            ignore.case = TRUE
          )
          file.copy(t1w$datapath, new_path, overwrite = TRUE)
          unlink(t1w$datapath)
          t1w$datapath <- new_path
        }
        yael_process$set_input_image(path = t1w$datapath,
                                     type = "T1w",
                                     overwrite = TRUE)
        unlink(t1w$datapath)
      }

      ct <- input$loader_preprocess_ct
      if(length(ct)) {
        if(endsWith(tolower(ct$name), "nii.gz")) {
          new_path <- gsub(
            pattern = "gz$",
            replacement = "nii.gz",
            x = ct$datapath,
            ignore.case = TRUE
          )
          file.copy(ct$datapath, new_path, overwrite = TRUE)
          unlink(ct$datapath)
          ct$datapath <- new_path
        }
        yael_process$set_input_image(path = ct$datapath,
                                     type = "CT",
                                     overwrite = TRUE)
        unlink(ct$datapath)
      }

      t1w_path <- yael_process$get_input_image("T1w")
      ct_path <- yael_process$get_input_image("CT")

      if(!length(t1w_path)) {
        stop("T1w MRI image is missing. Please upload one.")
      }
      if(!length(ct_path)) {
        stop("CT image is missing. Please upload one.")
      }

      subject <- yael_process$get_subject(project_name = project_name)
      subject$initialize_paths(include_freesurfer = FALSE)

      simple_cmd <- ravecore::cmd_run_yael_preprocess(
        subject = subject,
        normalize_template = NULL,
        run_recon_all = FALSE,
        dry_run = TRUE
      )
      complete_cmd <- ravecore::cmd_run_yael_preprocess(
        subject = subject,
        normalize_template = c(
          "mni_icbm152_nlin_asym_09b",
          "mni_icbm152_nlin_asym_09a",
          "mni_icbm152_nlin_asym_09c"
        ),
        run_recon_all = TRUE,
        dry_run = TRUE
      )

      ravedash::clear_notifications(class = ns("loader_preprocess_notif"))

      complete_script <- complete_cmd$execute(dry_run = TRUE, backup = FALSE)
      run_command_pipeline(
        simple_cmd,
        wait = FALSE,
        title = "YAEL Preprocessing",
        command = simple_cmd$command,
        session = session,
        on_modal_removal = function() {
          refresh_ct_chocies()
          show_notification_with_clipboard(
            title = "Finish the rest of pipeline!",
            message = paste0(
              "The first phase of the image pipeline is finished. ",
              "You can start to localize electrodes now. ",
              "However, to get more accurate results, ",
              "please finish the rest steps by opening your terminal ",
              "and pasting the command below:"),
            clip_text = complete_script
          )
        }
      )

    }, error_wrapper = "alert"),
    input$loader_preprocess_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      refresh_ct_chocies(reset_method = TRUE)
    }),
    loader_project$get_sub_element_input(),
    loader_subject$get_sub_element_input(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


  loading_error <- function(message) {
    dipsaus::shiny_alert2(
      title = "Errors",
      text = paste(
        "Found an error while loading the data:\n\n",
        message
      ),
      icon = "error",
      danger_mode = TRUE,
      auto_close = FALSE
    )
  }


  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code"
        )
      )

      if(!loader_project$sv$is_valid() || !loader_subject$sv$is_valid()) {
        loading_error("Invalid project/subject.")
        return()
      }

      # add your own input values to the settings file
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()
      subject <- ravecore::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
      fs_path <- subject$freesurfer_path
      if(length(fs_path) == 0 || is.na(fs_path) || !dir.exists(fs_path)) {
        loading_error("Cannot find surface/volume reconstruction directory. Please at least run FreeSurfer autorecon1 (only ~10 min)")
        return()
      }

      check_path <- function(fname, type) {
        if(length(fname) != 1 || is.na(fname) || fname == "") {
          loading_error(sprintf("Invalid %s file. Please specify or upload your own.", type))
          return(NULL)
        }
        fpath <- file.path(fs_path, "..", "coregistration", fname)
        if(!file.exists(fpath)) {
          loading_error(sprintf("Invalid %s path. Please check or upload your own.", type))
          return(NULL)
        }
        file.path("{subject$freesurfer_path}", "..", "coregistration", fname)
      }

      path_ct <- NULL
      path_mri <- NULL
      path_transform <- NULL
      transform_space <- "resampled"
      switch(
        paste(input$loader_method),
        "Re-sampled CT" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          if(is.null(path_ct)) { return() }
        },
        "FSL transform + Raw CT + MRI" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          if(is.null(path_ct)) { return() }
          path_mri <- check_path(input$loader_mri_fname, "MRI")
          if(is.null(path_mri)) { return() }
          path_transform <- check_path(input$loader_transform_fname, "transform matrix")
          if(is.null(path_transform)) { return() }
          transform_space <- "fsl"
        },
        "CT (IJK) to MR (RAS) transform + Raw CT" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          if(is.null(path_ct)) { return() }
          path_mri <- check_path(input$loader_mri_fname, "MRI")
          if(is.null(path_mri)) { return() }
          path_transform <- check_path(input$loader_transform_fname, "transform matrix")
          if(is.null(path_transform)) { return() }
          transform_space <- "ijk2ras"
        }
      )

      # Save the variables into pipeline settings file
      pipeline$set_settings(
        path_ct = path_ct,
        path_mri = path_mri,
        path_transform = path_transform,
        transform_space = transform_space,
        localization_plan = input$loader_plan,
        .list = settings
      )

      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = "Loading the viewer and CT file. It will take a while if this is the first time.",
        icon = "info",
        auto_close = FALSE, buttons = FALSE
      )

      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2()
      }, after = TRUE, add = TRUE)
      res <- pipeline$run(
        as_promise = FALSE,
        async = FALSE,
        names = c("plan_list", "pial_envelope", "brain", "localize_data",
                  "ct_exists", "fslut"),
        type = "vanilla",
        scheduler = "none"
      )

      dipsaus::close_alert2()

      # Let the module know the data has been changed
      ravedash::fire_rave_event('data_changed', Sys.time())
      ravedash::logger("Data has been loaded loaded")

      # Save session-based state: project name & subject code
      ravedash::session_setopt(
        project_name = project_name,
        subject_code = subject_code
      )

    }, error_wrapper = "alert"),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )


}
