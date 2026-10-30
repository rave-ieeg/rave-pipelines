# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

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
        shidashi::register_input(
          inputId = "image_selection_tabset",
          update = "shidashi::card_tabset_activate(value=title)",
          description = paste(
            "Active tab of the 'Image Selection' card: 'Data selector' (method",
            "and image files: `loader_method`, `loader_ct_fname`,",
            "`loader_mri_fname`, `loader_transform_fname`) or 'Preprocess'",
            "(people drop a T1 MRI and a CT there; then `loader_preprocess_run`).",
            "The loader shows 'Preprocess' when the subject has no co-registered CT."
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
              shidashi::register_input(
                dipsaus::actionButtonStyled(ns("loader_preprocess_run"), "Run YAEL preprocess pipeline", width = "100%"),
                inputId = "loader_preprocess_run",
                update = "dipsaus::updateActionButtonStyled",
                description = paste(
                  "[Button] 'Run YAEL preprocess pipeline': imports the T1 MRI and CT",
                  "that the person dropped on this tab (agents cannot drop files),",
                  "copies the MRI into place without processing it (simple-import),",
                  "and co-registers the CT to the MRI with YAEL-reg3d (FastRigid,",
                  "about a minute). A log window opens; when its button reads",
                  "'Done', click it (`shiny_ui_operate` click `dismiss_modal`) to",
                  "fill in the image files. Only for subjects without a co-registered",
                  "CT: refused when the subject has one, or when either file is not",
                  "dropped (the MRI/CT Preprocessing module handles those cases).",
                  "Click only after the person dropped both files and confirmed."
                ),
                tooltip = "Import the dropped images and co-register the CT to the MRI."
              )
            )
          ),
          "Data selector" = ravedash::flex_group_box(
            title = "Image data to load",

            shidashi::flex_item(
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns("loader_method"),
                  label = "Localization method",
                  choices = c(
                    "CT (IJK) to MR (RAS) transform + Raw CT",
                    "FSL transform + Raw CT + MRI",
                    "Re-sampled CT",
                    "Localize without CT"
                  )
                ),
                inputId = "loader_method",
                hint = "loader_forbidden",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "[Select] How the CT is aligned to the MRI: 'CT (IJK) to MR (RAS)",
                  "transform + Raw CT' (RAVE's co-registration: YAEL-reg3d, ANTs,",
                  "NiftyReg), 'FSL transform + Raw CT + MRI' (FSL flirt),",
                  "'Re-sampled CT' (a CT already resampled to the MRI), or",
                  "'Localize without CT' (MRI slices only). Filled in from the",
                  "subject's co-registration; change it only when the user asks.",
                  "Read by script `load_data`."
                ),
                tooltip = "How the CT is aligned to the MRI."
              )
            ),

            shidashi::flex_break(),

            shiny::conditionalPanel(
              condition = sprintf("input['%s']!=='Localize without CT'", ns("loader_method")),
              class = "padding-5",
              style = "flex:1; ",

              shidashi::flex_container(
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("loader_ct_fname"),
                      label = "Choose CT",
                      choices = character(0L)
                    ),
                    inputId = "loader_ct_fname",
                    hint = "loader_optional",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select] CT image, a file in the subject's co-registration",
                      "folder (e.g. `CT_RAW.nii.gz`); hidden for 'Localize without",
                      "CT'. No choices means no co-registration yet. Read by",
                      "script `load_data`."
                    ),
                    tooltip = "CT image to localize the electrodes on."
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s']!=='Re-sampled CT'", ns("loader_method")),

                shidashi::flex_container(
                  shidashi::flex_item(
                    shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns("loader_mri_fname"),
                        label = "Choose raw MRI",
                        choices = character(0L)
                      ),
                      inputId = "loader_mri_fname",
                      hint = "loader_optional",
                      update = "shiny::updateSelectInput(value=selected)",
                      description = paste(
                        "[Select] MRI that the transform maps the CT onto (e.g.",
                        "`MRI_reference.nii.gz`); shown for the two transform methods.",
                        "Read by script `load_data`."
                      ),
                      tooltip = "MRI that the CT was co-registered to."
                    )
                  )
                ),
                shidashi::flex_container(
                  shidashi::flex_item(
                    shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns("loader_transform_fname"),
                        label = "Transform matrix",
                        choices = character(0L)
                      ),
                      inputId = "loader_transform_fname",
                      hint = "loader_optional",
                      update = "shiny::updateSelectInput(value=selected)",
                      description = paste(
                        "[Select] Transform from the CT to the MRI:",
                        "`CT_IJK_to_MR_RAS.txt` for 'CT (IJK) to MR (RAS)', `ct2t1.mat`",
                        "for FSL. Read by script `load_data`."
                      ),
                      tooltip = "Transform from the CT to the MRI."
                    )
                  )
                )

              ),

              shiny::div(
                class = "float-right",
                shidashi::register_input(
                  shiny::actionLink(ns("loader_ct_refresh"), "Refresh file list"),
                  inputId = "loader_ct_refresh",
                  update = "shiny::updateActionLink",
                  description = paste(
                    "[Link] 'Refresh file list': reads the subject's co-registration",
                    "files again and fills in `loader_method`, the image files, and",
                    "the electrode plan (`loader_plan`) from the subject. Writes",
                    "nothing."
                  ),
                  tooltip = "Read the image file lists again."
                )
              )

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
          shidashi::register_input(
            inputId = "loader_plan",
            hint = "loader_mandatory",
            update = "dipsaus::updateCompoundInput2",
            description = paste(
              "Electrode plan: one entry per electrode group, as JSON, e.g.",
              "[{\"label\":\"LA\",\"dimension\":\"1-12\",\"type\":\"SEEG-ADTECH-SD12R-SP05X-000\",\"hemisphere\":\"auto\"}].",
              "`label`: the group's label prefix (contacts become LA1, LA2, ...).",
              "`dimension`: its channels, e.g. \"1-12\"; for an electrode model",
              "list them in contact order, with `NAx2` for 2 unplugged contacts.",
              "`type`: \"iEEG\", \"ECoG\", \"Others\", or an electrode model",
              "(prototype) from the Type list, e.g. `DBS-ABBOTT-6170`.",
              "`hemisphere`: \"auto\", \"left\", or \"right\".",
              "Filled in from the subject's electrode table. Confirm the plan",
              "with the user before `load_data`: loading writes it into the",
              "subject's staged table (meta/electrodes_unsaved.csv). Placed",
              "channels left out of the plan are removed from that table, and a",
              "group planned with another electrode model starts over (loading",
              "warns)."
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
          )
          ),
          footer = shiny::tagList(
            ravedash::load_data_button(label = "Load subject", width = "100%")
          )
        )
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...) {

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
        if (is.null(item)) {
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
      if (!length(file_info) || !length(file_info$datapath)) {
        return()
      }
      plan_table <- utils::read.csv(file_info$datapath)
      plan_table_names <- tolower(names(plan_table))
      names(plan_table) <- plan_table_names

      plan_list <- NULL

      # case 1: With GroupLabel, Channels, Type, Hemisphere
      if (all(c("grouplabel", "channels") %in% plan_table_names)) {
        plan_list <- lapply(split(plan_table, as.character(plan_table$grouplabel)), function(sub) {
          grouplabel <- sub$grouplabel[[1]]
          channels <- dipsaus::parse_svec(sub$channels)
          min_channel <- min(channels, na.rm = TRUE)
          if (length(channels) == 1) {
            channels <- sprintf("%.0f-%.0f", channels, channels)
          } else {
            channels <- dipsaus::deparse_svec(channels)
          }
          type <- unique(sub$type)
          type <- type[type %in% electrode_types]
          if (length(type)) {
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
      if ("electrode" %in% plan_table_names && any(c("labelprefix", "label") %in% plan_table_names)) {
        if (!length(plan_table$labelprefix)) {
          plan_table$labelprefix <- gsub("[0-9]+$", "", x = plan_table$label)
        }
        plan_list <- lapply(split(plan_table, plan_table$labelprefix), function(sub) {
          channels <- dipsaus::parse_svec(sub$electrode)
          min_channel <- min(channels, na.rm = TRUE)
          if (length(channels) == 1) {
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

      if (length(plan_list)) {
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
      if ( !length(clip_text) ) { return() }
      if ( !length(title) ) { title <- "Notification!" }

      local_data$clip_text <- NULL
      local_data$clip_title <- NULL

      shidashi::show_notification(
        message = shiny::div(
          paste(local_data$clip_message, collapse = ""),
          shiny::hr(),
          shiny::pre(
            class = "pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output",
            `data-dismiss` = "toast",
            type = "button",
            `aria-label` = "Close",
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
    if (!loader_subject$sv$is_valid() || !length(project_name) || !length(subject_code) ||
       is.na(project_name) || is.na(subject_code) || project_name == "" || subject_code == "") {
      return()
    }
    subject <- ravecore::RAVESubject$new(project_name = project_name,
                                       subject_code = subject_code,
                                       strict = FALSE)
    fs_path <- subject$freesurfer_path
    if ( length(fs_path) != 1 || is.na(fs_path) || !file.exists(fs_path) ) { return() }

    path_coreg_conf <- file.path(subject$imaging_path, "derivative", "conf-coregistration.yaml")
    coreg_files <- unique(unlist(lapply(coregistration_dirs(subject), list.files)))

    localization_method <- "Re-sampled CT"

    ct_name <- character()
    mri_name <- character()
    trans_name <- NULL

    if ( file.exists(path_coreg_conf) ) {
      coreg_conf <- ravepipeline::load_yaml(path_coreg_conf)

      # check outputs
      if ("CT_IJK_to_MR_RAS" %in% names(coreg_conf$outputs)) {
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
      if ( length(selected_method) == 1 ) {
        localization_method <- names(LOCALIZATION_METHODS)[
          unlist(LOCALIZATION_METHODS) == selected_method]
      }
      ct_name <- subject$get_default(
        "path_ct", default_if_missing = NULL,
        namespace = "electrode_localization")
      if (length(ct_name) == 1) {
        ct_name <- basename(ct_name)
        ct_name <- ct_name[ct_name %in% coreg_files]
      }
      if (!length(ct_name)) {
        ct_name <- coreg_files[grepl("^CT.*\\.nii(\\.gz|)$", coreg_files)]
        if (length(ct_name)) { ct_name <- ct_name[[1]] }
      }

      mri_name <- subject$get_default(
        "path_mri", default_if_missing = shiny::isolate(input$loader_mri_fname),
        namespace = "electrode_localization")
      if (length(mri_name) == 1) {
        mri_name <- basename(mri_name)
        mri_name <- mri_name[mri_name %in% coreg_files]
      }
      if (!length(mri_name)) {
        mri_name <- coreg_files[grepl("^MR.*\\.nii(\\.gz|)$", coreg_files)]
        if (length(mri_name)) { mri_name <- mri_name[[1]] }
      }

      trans_name <- subject$get_default(
        "path_transform", default_if_missing = shiny::isolate(input$loader_transform_fname),
        namespace = "electrode_localization")
      if (length(trans_name) == 1) {
        trans_name <- basename(trans_name)
        trans_name <- trans_name[trans_name %in% coreg_files]
      }
      if (!length(trans_name)) {
        trans_name <- coreg_files[grepl("\\.(mat|txt)$", coreg_files)]
        if (length(trans_name)) { trans_name <- trans_name[[1]] }
      }

      if ( length(trans_name) == 1 &&
          (
            identical(localization_method, "Re-sampled CT") ||
            !isTRUE(localization_method %in% names(LOCALIZATION_METHODS))
          ) ) {
        if ( startsWith(trans_name, "CT_IJK") ) {
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

  refresh_ct_chocies <- function(value_ct = NULL, value_mri = NULL, value_transform = NULL, reset_method = FALSE) {
    coreg_params <- load_coreg_params()

    # coreg_params <- list(
    #   method = "",
    #   ct_filename = ct_name,
    #   mr_filename = mri_name,
    #   transform_filename = trans_name,
    #   project_name = project_name,
    #   subject_code = subject_code,
    #   subject = subject
    # )

    if ( !length(coreg_params) ) {
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

    coreg_dirs <- coregistration_dirs(subject)

    nifti_files <- unique(unlist(lapply(
      coreg_dirs, list.files, pattern = "nii(?:\\.gz)?$",
      recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
      full.names = FALSE, all.files = FALSE)))
    transform_files <- unique(unlist(lapply(
      coreg_dirs, list.files, pattern = "(mat|txt)$",
      recursive = FALSE, ignore.case = TRUE, include.dirs = FALSE,
      full.names = FALSE, all.files = FALSE)))
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

    if (!length(coreg_params$ct_filename) && !isTRUE(coreg_params$method %in% "Localize without CT")) {
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

      if (!loader_project$sv$is_valid() || !loader_subject$sv$is_valid()) {
        loading_error("Invalid project/subject. Please specify a valid subject first before uploading CT.")
        return()
      }
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()

      subject <- ravecore::RAVESubject$new(project_name = project_name,
                                           subject_code = subject_code,
                                           strict = FALSE)

      # Only for subjects without co-registered images, and only with both
      # images dropped here: the MRI/CT Preprocessing module handles the rest.
      # (Checked before the alert below, which closes when this observer ends)
      coreg_params <- load_coreg_params()
      if (length(coreg_params$ct_filename)) {
        loading_error(sprintf(paste(
          "This subject already has a co-registered CT (%s). To co-register",
          "again, please use the MRI/CT Preprocessing module."
        ), paste(coreg_params$ct_filename, collapse = ", ")))
        return()
      }
      if (!is_dropped_file(input$loader_preprocess_t1) ||
          !is_dropped_file(input$loader_preprocess_ct)) {
        loading_error(paste(
          "Please drop both a T1w MRI and a CT image (NIfTI) on this tab.",
          "To use images already imported for this subject, please use the",
          "MRI/CT Preprocessing module."
        ))
        return()
      }

      dipsaus::shiny_alert2(
        title = "Checking current environment",
        icon = "info",
        text = "Importing images...",
        auto_close = FALSE, buttons = FALSE, session = session
      )
      on.exit({
        Sys.sleep(0.5)
        dipsaus::close_alert2()
      })

      subject$initialize_paths(include_freesurfer = FALSE)

      # Import the dropped images, then `simple-import` the MRI and
      # co-register the CT with YAEL-reg3d: nothing here needs Python
      mri_path <- import_dropped_image(input$loader_preprocess_t1, subject, type = "MRI")
      ct_path <- import_dropped_image(input$loader_preprocess_ct, subject, type = "CT")

      if (!length(mri_path)) {
        stop("T1w MRI image is missing. Please upload one.")
      }
      if (!length(ct_path)) {
        stop("CT image is missing. Please upload one.")
      }

      simple_import_mri(subject, mri_path)

      reg3d_cmd <- ravecore::cmd_run_reg3d_rigid(
        subject = subject,
        ct_path = ct_path,
        mri_path = mri_path,
        # quick (seconds); MRI/CT preprocessing offers finer registrations
        reg_type = "FastRigid",
        cost = "mattes",
        interp = "trilinear",
        verbose = FALSE,
        dry_run = TRUE
      )

      ravedash::clear_notifications(class = ns("loader_preprocess_notif"))

      run_command_pipeline(
        reg3d_cmd,
        wait = FALSE,
        title = "YAEL-reg3d co-registration",
        command = reg3d_cmd$command,
        session = session,
        on_modal_removal = function() {
          refresh_ct_chocies()
          # YAEL + recon-all on the same MRI; it rebuilds the FreeSurfer
          # folder that `simple-import` created
          complete_script <- tryCatch({
            complete_cmd <- ravecore::cmd_run_yael_preprocess_native(
              subject = subject,
              t1w_path = mri_path,
              normalize_template = c(
                "mni_icbm152_nlin_asym_09a"
              ),
              run_recon_all = TRUE,
              overwrite = FALSE,
              dry_run = TRUE,
              verbose = FALSE
            )
            complete_cmd$execute(dry_run = TRUE, backup = FALSE)
          }, error = function(e) {
            ravepipeline::logger("Cannot generate the YAEL+recon-all command: ",
                                 paste(e$message, collapse = "\n"), level = "warning")
            NULL
          })
          if (length(complete_script)) {
            show_notification_with_clipboard(
              title = "Finish the rest of pipeline!",
              message = paste0(
                "The CT is co-registered to the MRI (YAEL-reg3d, FastRigid: a quick ",
                "registration; the MRI/CT preprocessing module offers finer ones), and ",
                "the MRI is imported without processing. You can start to localize ",
                "electrodes now. To get FreeSurfer surfaces, atlas labels, and MNI ",
                "coordinates, please finish the rest steps by opening your terminal ",
                "and pasting the command below. Run it before saving the electrodes: ",
                "it rebuilds the FreeSurfer folder, which removes the saved electrode ",
                "shapes (fs/RAVE/geometry)."),
              clip_text = complete_script
            )
          }
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


  # Runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools).
  # Invalid inputs stop the script, so the loader stays open
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

      if (!loader_project$sv$is_valid() || !loader_subject$sv$is_valid()) {
        stop("Invalid project/subject.")
      }

      # add your own input values to the settings file
      project_name <- loader_project$get_sub_element_input()
      subject_code <- loader_subject$get_sub_element_input()
      subject <- ravecore::RAVESubject$new(project_name = project_name,
                                         subject_code = subject_code,
                                         strict = FALSE)
      fs_path <- subject$freesurfer_path
      if (length(fs_path) == 0 || is.na(fs_path) || !dir.exists(fs_path)) {
        stop("Cannot find surface/volume reconstruction directory. Please at least run FreeSurfer autorecon1 (only ~10 min)")
      }

      check_path <- function(fname, type) {
        if (length(fname) != 1 || is.na(fname) || fname == "") {
          stop(sprintf("Invalid %s file. Please specify or upload your own.", type))
        }
        # the imaging folder (where RAVE's preprocessing writes; not under
        # the raw folder for BIDS projects), then next to the FreeSurfer
        # folder (older layouts)
        if (file.exists(file.path(subject$imaging_path, "coregistration", fname))) {
          return(file.path("{subject$imaging_path}", "coregistration", fname))
        }
        if (!file.exists(file.path(fs_path, "..", "coregistration", fname))) {
          stop(sprintf("Invalid %s path. Please check or upload your own.", type))
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
        },
        "FSL transform + Raw CT + MRI" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          path_mri <- check_path(input$loader_mri_fname, "MRI")
          path_transform <- check_path(input$loader_transform_fname, "transform matrix")
          transform_space <- "fsl"
        },
        "CT (IJK) to MR (RAS) transform + Raw CT" = {
          path_ct <- check_path(input$loader_ct_fname, "CT")
          path_mri <- check_path(input$loader_mri_fname, "MRI")
          path_transform <- check_path(input$loader_transform_fname, "transform matrix")
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

      pipeline$run(
        as_promise = FALSE,
        async = FALSE,
        names = c("plan_list", "pial_envelope", "brain", "localize_data",
                  "ct_exists", "fslut"),
        type = "vanilla",
        scheduler = "none"
      )

      ravepipeline::logger("Data has been loaded loaded")

      # Save session-based state: project name & subject code
      ravedash::session_setopt(
        project_name = project_name,
        subject_code = subject_code
      )

      # Summary for agents. `warning=`: placed electrodes the plan removed or
      # reset (people see them in a notification once the module opens)
      plan_list <- pipeline$read("plan_list")
      merge_notes <- attr(plan_list, "merge_notes")
      c(
        sprintf("subject=%s/%s", project_name, subject_code),
        sprintf("method=%s", paste(input$loader_method, collapse = "")),
        sprintf("ct=%s", paste(c(path_ct, "none")[[1]], collapse = "")),
        sprintf("groups=%s", paste(vapply(plan_list, function(sub) {
          sprintf("%s (%s, %d channels)", sub$LabelPrefix[[1]],
                  ifelse(nzchar(sub$Prototype[[1]]), sub$Prototype[[1]], sub$LocationType[[1]]),
                  nrow(sub))
        }, ""), collapse = "; ")),
        sprintf("warning=%s", merge_notes),
        "next=pick a group (buttons `switch_plan_btn_<i>`), then run script `localization_status`"
      )
    },
    description = c(
      "Same as clicking 'Load subject'. Reads the subject (`loader_project_name`,",
      "`loader_subject_code`), `loader_method`, the image files (`loader_ct_fname`,",
      "`loader_mri_fname`, `loader_transform_fname`), and the electrode plan",
      "(`loader_plan`). Confirm the plan with the user first: loading writes it",
      "into the subject's staged electrode table (meta/electrodes_unsaved.csv),",
      "keeping the coordinates already localized. Placed channels left out of",
      "the plan are removed from that table, and a group planned with another",
      "electrode model loses its positions; the previous table is backed up and the",
      "result has a `warning=` line for each. Loads the brain, CT, and 3D viewer",
      "(up to a minute). Result: subject, method, CT file, and the groups with",
      "their electrode models."
    ),
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(
      title = "Loading in progress",
      text = "Loading the viewer and CT file. It will take a while if this is the first time."
    )
  )


}


# A file dropped on a `fancyFileInput` that is still in its upload folder
is_dropped_file <- function(file_info) {
  length(file_info) > 0 && length(file_info$datapath) == 1 &&
    isTRUE(file.exists(file_info$datapath))
}

# Import a dropped NIfTI image the way the MRI/CT preprocessing module imports
# NIfTI files (`ravecore::cmd_run_dcm2niix()`): `inputs/<type>/<type>_RAW.nii[.gz]`
# under the imaging folder, with a copy in `derivative/`. Unlike that function,
# the temporary upload path is not recorded as the image source. Without a
# dropped file, returns NULL.
import_dropped_image <- function(file_info, subject, type = c("MRI", "CT")) {
  type <- match.arg(type)
  if (!is_dropped_file(file_info)) { return(NULL) }

  imaging_path <- subject$imaging_path
  input_dir <- file.path(imaging_path, "inputs", type)
  if (!grepl("\\.nii(\\.gz)?$", file_info$name, ignore.case = TRUE)) {
    stop(sprintf("The %s image must be a NIfTI file (.nii or .nii.gz).", type))
  }
  ext <- ifelse(grepl("\\.gz$", file_info$name, ignore.case = TRUE), ".nii.gz", ".nii")
  deriv_dir <- file.path(imaging_path, "derivative")
  ravepipeline::dir_create2(input_dir)
  ravepipeline::dir_create2(deriv_dir)

  # one image per type: drop the copy with the other extension
  stale <- sprintf("%s_RAW%s", type, setdiff(c(".nii.gz", ".nii"), ext))
  unlink(file.path(c(input_dir, deriv_dir), stale))

  fname <- sprintf("%s_RAW%s", type, ext)
  for (dst in file.path(c(input_dir, deriv_dir), fname)) {
    if (!file.copy(file_info$datapath, dst, overwrite = TRUE, copy.date = TRUE)) {
      stop(sprintf("Cannot import the %s image to %s", type, dst))
    }
  }
  unlink(file_info$datapath)
  normalizePath(file.path(input_dir, fname), winslash = "/")
}

# `simple-import` of the MRI/CT preprocessing module: put the MRI in the
# FreeSurfer folder as `mri/brain.nii[.gz]` (the viewer's MRI), with a copy
# as `derivative/MRI_RAW.nii[.gz]`. A reconstruction made by another program
# (FreeSurfer, YAEL) is left alone; a folder that only holds an earlier
# `simple-import` is replaced, so the viewer and the co-registration use the
# same MRI.
simple_import_mri <- function(subject, mri_path) {
  fs_existing <- subject$freesurfer_path
  if (length(fs_existing) == 1 && !is.na(fs_existing) && dir.exists(fs_existing)) {
    mri_files <- list.files(file.path(fs_existing, "mri"))
    only_simple_import <- !dir.exists(file.path(fs_existing, "surf")) &&
      all(mri_files %in% c("brain.nii", "brain.nii.gz"))
    if (!only_simple_import) {
      ravepipeline::logger("Found surface/volume reconstruction at ", fs_existing,
                           "; skipping simple-import of the MRI", level = "info")
      return(invisible(FALSE))
    }
  }
  imaging_path <- subject$imaging_path
  ext <- ifelse(grepl("\\.gz$", mri_path, ignore.case = TRUE), ".nii.gz", ".nii")
  mri_dir <- file.path(imaging_path, "fs", "mri")
  deriv_dir <- file.path(imaging_path, "derivative")
  ravepipeline::dir_create2(mri_dir)
  ravepipeline::dir_create2(deriv_dir)

  # the viewer prefers `brain.nii.gz` over `brain.nii`: keep one of them
  unlink(file.path(mri_dir, sprintf("brain%s", setdiff(c(".nii.gz", ".nii"), ext))))
  dst <- c(file.path(mri_dir, sprintf("brain%s", ext)),
           file.path(deriv_dir, sprintf("MRI_RAW%s", ext)))
  for (p in dst) {
    if (normalizePath(p, winslash = "/", mustWork = FALSE) == mri_path) { next }
    if (!file.copy(mri_path, p, overwrite = TRUE, copy.date = TRUE)) {
      stop("Cannot copy the MRI to ", p)
    }
  }
  invisible(TRUE)
}

# Folders that may hold the co-registered images: `coregistration` in the
# imaging folder (where RAVE's preprocessing writes; not under the raw folder
# for BIDS projects), then the one next to the FreeSurfer folder (older layouts)
coregistration_dirs <- function(subject) {
  dirs <- file.path(subject$imaging_path, "coregistration")
  fs_path <- subject$freesurfer_path
  if (length(fs_path) == 1 && !is.na(fs_path) && dir.exists(fs_path)) {
    dirs <- c(dirs, file.path(fs_path, "..", "coregistration"))
  }
  dirs <- dirs[dir.exists(dirs)]
  unique(normalizePath(dirs, winslash = "/"))
}
