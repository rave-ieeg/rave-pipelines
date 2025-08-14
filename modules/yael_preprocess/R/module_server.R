
module_server <- function(input, output, session, ...){

  error_notification <- function(e) {
    shidashi::clear_notifications(class = ns("error_notif"))
    if(!inherits(e, "condition")) {
      e <- simpleError(message = e$message)
    }
    ravepipeline::logger_error_condition(e)
    Sys.sleep(0.1)
    shidashi::show_notification(
      message = e$message,
      title = "Error found!",
      type = "danger",
      close = TRUE,
      autohide = TRUE, delay = 30000,
      class = ns("error_notif"),
      collapse = "\n"
    )
  }
  info_notification <- function(...) {
    ravepipeline::logger(..., level = "info")
    shidashi::show_notification(
      message = paste(..., sep = ""),
      title = "Notification",
      type = "success",
      close = TRUE,
      autohide = TRUE,
      class = ns("info_notif"),
      collapse = "\n"
    )
  }


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()
  local_env <- new.env()

  gray_colors <- gray.colors(256, 0, 1)

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)


  # check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      # There is not too many interaction, so update everytime
      check_result <- pipeline$read(var_names = "check_result")
      print(check_result)
      cmd_tools <- pipeline$read(var_names = "cmd_tools")
      local_reactives$project_name <- check_result$project_name
      local_reactives$subject_code <- check_result$subject_code
      local_reactives$temp_dir <- check_result$path_temp
      local_reactives$path_mri <- check_result$path_mri
      local_reactives$path_ct <- check_result$path_ct
      local_reactives$subject_fspath <- check_result$fs_path
      local_reactives$actions <- list(
        fs_reconstructed = check_result$fs_reconstructed,
        skip_recon = check_result$skip_recon,
        skip_coregistration = check_result$skip_coregistration
      )
      local_reactives$acpc <- list()
      local_reactives$tools <- cmd_tools
      local_reactives$viewer_acpc <- NULL
      local_reactives$acpc_alignment <- NULL
      local_reactives$build_command <- Sys.time()

      shidashi::card_operate(title = "Import DICOM Folders or Nifti Images", method = "collapse")
      shidashi::card_operate(title = "MRI Preprocessing", method = "collapse")
      shidashi::card_operate(title = "Co-registration CT and T1", method = "collapse")
      shidashi::card_operate(title = "ACPC Re-alignment (Optional)", method = "collapse")

      component_container$reset_data()
      component_container$data$subject <- ravecore::RAVESubject$new(
        project_name = check_result$project_name,
        subject_code = check_result$subject_code,
        strict = FALSE
      )

      update_nii_t1()
      update_nii_ct()

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Folders or Nifti Images", method = "expand")
      shidashi::card_operate(title = "MRI Preprocessing", method = "collapse")
      shidashi::card_operate(title = "Co-registration CT and T1", method = "collapse")
      shidashi::card_operate(title = "ACPC Re-alignment (Optional)", method = "collapse")
    }),
    input$jump_import_dicom,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Folders or Nifti Images", method = "collapse")
      shidashi::card_operate(title = "MRI Preprocessing", method = "expand")
      shidashi::card_operate(title = "Co-registration CT and T1", method = "collapse")
      shidashi::card_operate(title = "ACPC Re-alignment (Optional)", method = "collapse")
    }),
    input$jump_recon,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Folders or Nifti Images", method = "collapse")
      shidashi::card_operate(title = "MRI Preprocessing", method = "collapse")
      shidashi::card_operate(title = "Co-registration CT and T1", method = "expand")
      shidashi::card_operate(title = "ACPC Re-alignment (Optional)", method = "collapse")
    }),
    input$jump_coregistration,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Import DICOM Folders or Nifti Images", method = "collapse")
      shidashi::card_operate(title = "MRI Preprocessing", method = "collapse")
      shidashi::card_operate(title = "Co-registration CT and T1", method = "collapse")
      shidashi::card_operate(title = "ACPC Re-alignment (Optional)", method = "expand")
    }),
    input$jump_acpc_realign,
    ignoreInit = TRUE, ignoreNULL = TRUE
  )


  output$basic_info <- shiny::renderUI({
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }

    project_name <- local_reactives$project_name
    subject_code <- local_reactives$subject_code
    actions <- local_reactives$actions
    tools <- local_reactives$tools
    if(!length(project_name)) { return() }

    shiny::div(
      "Project: ", shiny::strong(project_name),
      shiny::br(),
      "Subject: ", shiny::strong(subject_code),
      shiny::br(),
      "Actions:",
      shiny::tags$ul(
        shiny::tags$li(
          shiny::actionLink(
            inputId = ns("jump_import_dicom"),
            label = "Import DICOM or Nifti images "
          )
        ),
        shiny::tags$li(
          shiny::actionLink(
            inputId = ns("jump_acpc_realign"),
            label = "ACPC Re-alignment (Optional)"
          )
        ),
        shiny::tags$li(
          shiny::actionLink(
            inputId = ns("jump_recon"),
            label = "MRI Preprocessing"
          )
        ),
        shiny::tags$li(
          shiny::actionLink(
            inputId = ns("jump_coregistration"),
            label = "CT MRI co-registration"
          )
        )
      )
    )


  })


  update_nii_t1 <- function(){
    mri_path <- shiny::isolate(file.path(local_reactives$temp_dir, "inputs", "MRI"))
    choices <- character(0L)
    if(length(mri_path) == 1 && !is.na(mri_path) && dir.exists(mri_path)) {
      choices <- list.files(mri_path, pattern = "nii($|\\.gz$)", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
    }
    if(!length(choices)) {
      choices <- character(0L)
    }

    selected <- tryCatch({
      subject <- component_container$data$subject
      subject$get_default("nii_t1", default_if_missing = input$param_fs_infile,
                          namespace = "yael_preprocess")
    }, error = function(e){
      input$param_fs_infile
    })

    selected <- selected %OF% choices
    shiny::updateSelectInput(
      session = session, inputId = "param_fs_infile",
      choices = choices, selected = selected
    )

    choices <- choices[!endsWith(choices, "ACPC.nii.gz")]
    selected <- input$param_acpc_infile %OF% choices
    shiny::updateSelectInput(
      session = session, inputId = "param_acpc_infile",
      choices = choices, selected = selected
    )
  }

  update_nii_ct <- function(){
    ct_path <- shiny::isolate(file.path(local_reactives$temp_dir, "inputs", "CT"))
    choices <- character(0L)
    if(length(ct_path) == 1 && !is.na(ct_path) && dir.exists(ct_path)) {
      choices <- list.files(ct_path, pattern = "nii($|\\.gz$)", all.files = FALSE,
                            full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
    }
    if(!length(choices)) {
      choices <- character(0L)
    }

    selected <- tryCatch({
      subject <- component_container$data$subject
      subject$get_default("nii_ct", default_if_missing = input$param_coreg_ct,
                          namespace = "yael_preprocess")
    }, error = function(e){
      input$param_coreg_ct
    })

    selected <- selected %OF% choices
    shiny::updateSelectInput(
      session = session, inputId = "param_coreg_ct",
      choices = choices, selected = selected
    )

    deriv_path <- shiny::isolate(file.path(local_reactives$temp_dir, "derivative"))
    choices <- list.files(
      deriv_path,
      pattern = "\\.(nii|nii\\.gz)$",
      full.names = FALSE,
      recursive = FALSE,
      ignore.case = TRUE,
      include.dirs = FALSE,
      all.files = FALSE
    )
    choices <- choices[!startsWith(choices, "ct_in_t1")]
    sel <- startsWith(toupper(choices), "MRI")
    choices <- c(choices[sel], choices[!sel])
    selected <- input$param_coreg_mri %OF% choices
    shiny::updateSelectInput(
      session = session, inputId = "param_coreg_mri",
      choices = choices, selected = selected
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      update_nii_t1()
    }),
    input$param_fs_refresh,
    input$param_acpc_refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      update_nii_ct()
    }),
    input$param_coreg_refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  watch_log <- function(){
    path <- local_data$log_file
    if(length(path) != 1 || is.na(path) || !file.exists(path) || path == '') {
      msg <- NULL
    } else {
      suppressWarnings({
        msg <- readLines(path)
      })
      if(!length(msg) || isTRUE(msg == "")) {
        msg <- "Waiting for outputs..."
      }
      later::later(watch_log, delay = 0.5)
    }
    msg <- paste(msg, collapse = "\n")

    session$sendCustomMessage(
      "shidashi.set_html",
      list(
        selector = sprintf("pre#%s", ns("verbatim_log")),
        content = paste0(
          '<code class="hljs-literal" style="word-wrap:break-word;width: 100%;white-space: pre-wrap;">',
          msg,
          '</code>'
        )
      )
    )
  }

  shiny::bindEvent(
    ravedash::safe_observe({
      local_data$log_file <- NULL
      shidashi::clear_notifications(class = "dismissible")
      shiny::removeModal()
    }),
    input$dismiss_modal,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  renderMsg <- function(msg) {
    msg <- paste(msg, collapse = "\n")
    session$sendCustomMessage(
      "shidashi.set_html",
      list(
        selector = sprintf("pre#%s", ns("verbatim_log")),
        content = paste0(
          '<code class="hljs-literal" style="word-wrap:break-word;width: 100%;white-space: pre-wrap;">',
          msg,
          '</code>'
        )
      )
    )
  }

  run_command_pipeline <- function(cmd, wait = TRUE, title = "Running Terminal Command", command = NULL, ...) {

    shidashi::clear_notifications(class = ns("error_notif"))

    shiny::showModal(shiny::modalDialog(
      title = title,
      size = "l", easyClose = FALSE,
      shidashi::flex_container(
        class = "fill-width max-height-500 overflow-y-auto",
        style = "flex-direction: column-reverse",
        shidashi::flex_item(
          shiny::verbatimTextOutput(ns("verbatim_log"), placeholder = TRUE)
        )
      ),
      footer = dipsaus::actionButtonStyled(ns("dismiss_modal"), "Running...", disabled = "")
    ))

    if( !length(command) ) {
      command <- cmd$command
      if(!length(command)) {
        command <- "bash"
      }
    }
    cmd$execute(dry_run = TRUE, backup = FALSE)

    job_id <- ravepipeline::start_job(
      function(command, script, script_path, log_path) {
        if(!length(command)) {
          command <- "bash"
        }
        writeLines(c(
          "=================== Start: shell script ===================",
          script,
          paste("=================== Log:", Sys.time(), "==================="),
          "",
          sprintf("# Running above script using system command: %s", paste(command, collapse = " ")),
          ""
        ), con = log_path)
        cat(script)
        code <- ravecore::cmd_execute(
          script = script,
          script_path = script_path,
          stdout = log_path,
          stderr = log_path,
          command = command
        )
        # return TRUE
        structure(TRUE, code = code)
      },
      fun_args = list(
        command = command,
        script = cmd$script,
        script_path = cmd$script_path,
        log_path = cmd$log_file
      ),
      packages = "ravecore",
      method = "rs_job",
      name = "Imaging pipeline",
      ensure_init = TRUE
    )

    local_data$current_command <- list(
      job_id = job_id,
      cmd = cmd
    )
    local_data$current_logs <- NULL

    # construct a promise to check the job
    promise <- promises::promise(function(resolve, reject) {

      check <- function() {

        msg <- "Waiting for outputs..."
        tryCatch({
          path <- cmd$log_file
          if(length(path) != 1 || is.na(path) || !file.exists(path) || path == '') {
            msg <- NULL
          } else {
            suppressWarnings({
              msg <- readLines(path)
            })
            if(!length(msg) || isTRUE(msg == "")) {
              msg <- "Waiting for outputs..."
            }
          }
        }, error = function(e) {
          msg <- sprintf("Preparation error: %s", paste(e$message, collapse = "\n"))
        })
        local_data$current_logs <- msg

        tryCatch(
          {
            renderMsg(msg)
            res <- ravepipeline::resolve_job(job_id = job_id, timeout = 0, unresolved = "silent")
            if(isTRUE(res)) {
              msg <- c(msg, "Finished.")
              local_data$current_logs <- msg
              renderMsg(msg)
              resolve(msg)
              return()
            }
            # not yet!
            later::later(check, 1)
          },
          error = function(e) {
            msg <- c(msg, "An error is detected.", e$message)
            local_data$current_logs <- msg
            reject(e)
          }
        )
      }
      check()

    })

    promise$then(
      onFulfilled = function(msg) {
        renderMsg(msg)
        dipsaus::updateActionButtonStyled(
          session = session, inputId = 'dismiss_modal', disabled = FALSE,
          label = "Done")
        return(TRUE)
      },
      onRejected = function() {
        dipsaus::updateActionButtonStyled(
          session = session, inputId = 'dismiss_modal', disabled = FALSE,
          label = "Gotcha")
        return(FALSE)
      }
    )$then(
      function(succeed = TRUE) {
        local_reactives$logfile_path <- cmd$log_file
        local_data$current_command <- NULL

        shidashi::show_notification(
          message = shiny::div(
            "The command seems to have finished. ",
            "However, RAVE does NOT know if the command finishes correctly.",
            "Please check the ", shiny::downloadLink(ns("logfile_download"), "log file"),
            "to see ", ifelse(
              succeed, "the full log, especially the last 10 lines.",
              "the error information."
            )
          ),
          title = "Notice!",
          autohide = FALSE, close = TRUE, type = "dark",
          icon = ravedash::shiny_icons$terminal,
          class = "dismissible"
        )
      }
    )
  }

  output$logfile_download <- shiny::downloadHandler(
    filename = "logfile.txt",
    content = function(conn) {
      path <- local_reactives$logfile_path
      if( length(path) == 1 && !is.na(path) && file.exists(path) ) {
        file.copy(path, conn, overwrite = TRUE, recursive = FALSE)
      } else {
        writeLines("Cannot find the log file...", con = conn)
      }
    },
    contentType = "text/plain"
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd1 <- res$import_T1
      # cmd2 <- res$import_CT
      cmd1 <- local_reactives$bash_scripts$import_T1
      cmd2 <- local_reactives$bash_scripts$import_CT
      if(isTRUE(cmd1$error)) {
        error_notification(cmd1$condition)
        return()
      }
      if(isTRUE(cmd2$error)) {
        error_notification(cmd2$condition)
        return()
      }

      script1 <- cmd1$execute(dry_run = TRUE, backup = FALSE)
      script2 <- cmd2$execute(dry_run = TRUE, backup = FALSE)

      script <- paste(script1, script2, sep = "\n")

      shidashi::show_notification(
        message = shiny::div(
          paste0("Shell script saved to [", dirname(cmd1$script_path),
                 "]! Please open your terminal, ",
                 "paste the command below:"),
          shiny::hr(),
          shiny::pre(
            class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
            `data-dismiss`="toast",
            type = "button",
            `aria-label`="Close",
            `data-clipboard-text` = script,
            shiny::code( script )
          )
        ),
        title = "Saved!", autohide = FALSE, close = TRUE,
        class = "dismissible",
        icon = ravedash::shiny_icons$terminal
      )

    }),
    input$btn_dcm2niix_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd1 <- res$import_T1
      cmd <- local_reactives$bash_scripts$import_T1
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravepipeline::logger("Running dcm2niix from console - Importing T1 MRI", level = "info")

      run_command_pipeline(cmd = cmd, wait = FALSE, title = "Importing T1")
    }),
    input$btn_dcm2niix_run_t1,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd1 <- res$import_T1
      cmd <- local_reactives$bash_scripts$import_CT
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravepipeline::logger("Running dcm2niix from console - Importing CT", level = "info")

      run_command_pipeline(cmd = cmd, wait = FALSE, title = "Importing CT")
    }),
    input$btn_dcm2niix_run_ct,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      # cmd <- res$image_segmentation
      # cmd2 <- res$import_CT
      cmd <- local_reactives$bash_scripts$image_segmentation
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }

      script <- cmd$execute(dry_run = TRUE, backup = FALSE)

      shidashi::show_notification(
        message = shiny::div(
          paste0("Shell script saved to [", cmd$script_path,
                 "]! Please open your terminal, ",
                 "paste the command below:"),
          shiny::hr(),
          shiny::pre(
            class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
            `data-dismiss`="toast",
            type = "button",
            `aria-label`="Close",
            `data-clipboard-text` = script,
            shiny::code( script )
          )
        ),
        title = "Saved!", autohide = FALSE, close = TRUE,
        class = "dismissible",
        icon = ravedash::shiny_icons$terminal
      )

    }),
    input$btn_recon_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      cmd <- local_reactives$bash_scripts$image_segmentation
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravepipeline::logger("Running FreeSurfer recon-all from console", level = "info")
      run_command_pipeline(cmd = cmd, wait = FALSE, title = "FreeSurfer recon-all")
    }),
    input$btn_recon_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      program <- paste(input$coreg_ct_program, collapse = "")
      cmd <- switch(
        program,
        "AFNI" = {
          local_reactives$bash_scripts$coreg_3dallineate
        },
        "FSL" = {
          local_reactives$bash_scripts$coreg_flirt
        },
        "img_pipe" = {
          local_reactives$bash_scripts$coreg_nipy
        },
        "NiftyReg" = {
          local_reactives$bash_scripts$coreg_niftyreg
        },
        "ANTs" = {
          local_reactives$bash_scripts$coreg_ants
        },
        {
          stop("Unknown coregistration command")
        }
      )
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }

      script <- cmd$execute(dry_run = TRUE, backup = FALSE)

      shidashi::show_notification(
        message = shiny::div(
          paste0("Shell script saved to [", cmd$script_path,
                 "]! Please open your terminal, ",
                 "paste the command below:"),
          shiny::hr(),
          shiny::pre(
            class='pre-compact bg-gray-90 clipboard-btn shidashi-clipboard-output',
            `data-dismiss`="toast",
            type = "button",
            `aria-label`="Close",
            `data-clipboard-text` = script,
            shiny::code( script )
          )
        ),
        title = "Saved!", autohide = FALSE, close = TRUE,
        class = "dismissible",
        icon = ravedash::shiny_icons$terminal
      )

    }, error_wrapper = "notification"),
    input$btn_coreg_copy,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      program <- paste(input$coreg_ct_program, collapse = "")
      native <- FALSE
      switch(
        program,
        "AFNI" = {
          bin <- "AFNI/3dAllineate"
          cmd <- local_reactives$bash_scripts$coreg_3dallineate
        },
        "FSL" = {
          bin <- "FSL/flirt"
          cmd <- local_reactives$bash_scripts$coreg_flirt
        },
        "img_pipe" = {
          bin <- "RAVE+img_pipe"
          cmd <- local_reactives$bash_scripts$coreg_nipy
          native <- TRUE
        },
        "NiftyReg" = {
          bin <- "RAVE+NiftyReg"
          cmd <- local_reactives$bash_scripts$coreg_niftyreg
          native <- TRUE
        },
        "ANTs" = {
          bin <- "RAVE+ANTs"
          cmd <- local_reactives$bash_scripts$coreg_ants
          native <- TRUE
        },
        {
          stop("Unknown coregistration program")
        }
      )
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravepipeline::logger("Running {bin} from console", level = "info", use_glue = TRUE)

      # set flag so rpymat is not checked
      check_rpymat <- FALSE
      if(native) {
        if(dipsaus::get_os() == "windows") {
          dipsaus::shiny_alert2(
            title = sprintf("Running coregistration via %s", bin),
            text = ravedash::be_patient_text(),
            icon = "info", auto_close = FALSE, buttons = FALSE
          )
          on.exit({
            Sys.sleep(0.5)
            dipsaus::close_alert2()
          }, add = TRUE, after = TRUE)
          ravecore::backup_file(cmd$script_path, remove = TRUE, quiet = TRUE)
          writeLines(cmd$script, con = cmd$script_path, sep = "\n")
          source(cmd$script_path, local = TRUE)
        } else {
          run_command_pipeline(
            cmd = cmd,
            wait = FALSE,
            title = bin,
            command = Sys.which("Rscript"),
            args = c("--no-save", "--no-restore")
          )
        }
      } else {
        run_command_pipeline(cmd = cmd, wait = FALSE, title = bin)
      }
    }),
    input$btn_coreg_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      native <- TRUE
      bin <- "RAVE+ANTs"
      cmd <- local_reactives$bash_scripts$morphmri_ants
      if(isTRUE(cmd$error)) {
        error_notification(cmd$condition)
        return()
      }
      ravepipeline::logger("Running {bin} from console", level = "info", use_glue = TRUE)

      # set flag so rpymat is not checked
      if(dipsaus::get_os() == "windows") {
        dipsaus::shiny_alert2(
          title = sprintf("Morphing MRI to template via %s", bin),
          text = ravedash::be_patient_text(),
          icon = "info", auto_close = FALSE, buttons = FALSE
        )
        on.exit({
          Sys.sleep(0.5)
          dipsaus::close_alert2()
        }, add = TRUE, after = TRUE)
        ravecore::backup_file(cmd$script_path, remove = TRUE, quiet = TRUE)
        writeLines(cmd$script, con = cmd$script_path, sep = "\n")
        source(cmd$script_path, local = TRUE)
      } else {
        run_command_pipeline(
          cmd = cmd,
          wait = FALSE,
          title = sprintf("Morphing MRI to template via %s", bin),
          command = Sys.which("Rscript"),
          args = c("--no-save", "--no-restore")
        )
      }
    }),
    input$btn_mri_morph_run,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  input_params <- shiny::debounce(shiny::reactive({

    list(
      nii_t1 = input$param_fs_infile,
      nii_ct = input$param_coreg_ct,
      template_brain = input$mri_morph_template_subject,
      dcm2niix = list(
        merge = input$param_dcm2niix_merge,
        float = input$param_dcm2niix_float,
        crop = input$param_dcm2niix_crop
      ),
      segmentation = local({
        switch(
          paste(input$param_fs_prog, collapse = ""),
          "recon-all -all" = {
            list(
              program = "recon-all",
              flag = "-all",
              fresh_start = isTRUE(input$param_fs_fresh_start)
            )
          },
          "recon-all -autorecon1" = {
            list(
              program = "recon-all",
              flag = "-autorecon1",
              fresh_start = isTRUE(input$param_fs_fresh_start)
            )
          },
          "recon-all-clinical.sh" = {
            list(
              program = "recon-all-clinical.sh",
              fresh_start = isTRUE(input$param_fs_fresh_start)
            )
          },
          "ants-preprocessing" = {
            list(
              program = "ants-preprocessing"
            )
          },
          "YAEL+recon-all" = {
            list(
              program = "YAEL+recon-all",
              fresh_start = isTRUE(input$param_fs_fresh_start)
            )
          },
          {
            list(
              program = "simple-import",
              fresh_start = isTRUE(input$param_fs_fresh_start)
            )
          }
        )
      }),
      niftyreg = list(
        reference = input$param_coreg_mri,
        reg_type = input$coreg_niftyreg_type,
        interp = input$coreg_niftyreg_interp
      ),
      ants = list(
        reference = input$param_coreg_mri,
        reg_type = input$coreg_ants_type,
        aff_metric = input$coreg_ants_aff_metric,
        syn_metric = input$coreg_ants_syn_metric
      ),
      flirt = list(
        reference = input$param_coreg_mri,
        dof = as.integer(gsub("[^0-9]+$", "", input$coreg_fsl_dof)) %OF% c(6L, 7L, 9L, 12L),
        cost = input$coreg_fsl_cost %OF% FSL_COST_FUNCTIONS,
        search = as.integer(input$coreg_fsl_search) %OF% c(90L, 180L),
        searchcost = input$coreg_fsl_searchcost %OF% FSL_COST_FUNCTIONS
      ),
      nipy = list(
        reference = input$param_coreg_mri,
        clean_source = input$coreg_nipy_clean_source,
        inverse_target = input$coreg_nipy_inverse_target,
        precenter_source = input$coreg_nipy_precenter_source,
        reg_type = input$coreg_nipy_reg_type,
        similarity = input$coreg_nipy_cost,
        interp = input$coreg_nipy_interp,
        optimizer = input$coreg_nipy_optimizer
      ),
      afni = list(
        reference = input$param_coreg_mri
      )
    )
  }), priority = 1L, millis = 300)


  ct_preview_path <- shiny::reactive({
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }
    subject <- pipeline$read("subject")

    params <- input_params()
    nii_ct <- params$nii_ct
    path <- file.path(subject$preprocess_settings$raw_path,
                      "rave-imaging", "inputs", "CT", nii_ct)
    path
  })
  output$ct_preview <- shiny::bindEvent(
    shiny::bindCache(
      shiny::renderPlot({
        loaded_flag <- ravedash::watch_data_loaded()
        if(!loaded_flag){ return() }
        subject <- pipeline$read("subject")

        params <- input_params()
        nii_ct <- params$nii_ct
        path <- file.path(subject$preprocess_settings$raw_path,
                          "rave-imaging", "inputs", "CT", nii_ct)
        has_path <- FALSE
        nii <- NULL
        if(length(path) == 1 && !is.na(path) && file.exists(path)) {
          try({
            nii <- ieegio::read_volume(path)
            has_path <- TRUE
          }, silent = TRUE)
        }
        shiny::validate(shiny::need(has_path, message = "No valid CT file selected"))

        vox2ras <- nii$transforms$vox2ras
        voxel_size <- vox2ras %*% c(1, 1, 1, 0)
        voxel_size <- voxel_size[c(1, 2, 3)]
        shape <- dim(nii)
        vlim <- range(nii[])

        position <- vox2ras %*% c(shape / 2, 1)
        position <- position[c(1, 2, 3)]

        old_par <- par(c("mfrow", "mar", "bg", "fg", "cex"))
        on.exit({
          do.call(par, old_par)
        })
        layout(matrix(c(4,4,4,1,2,3), nrow = 2, byrow = TRUE),
               heights = c(lcm(1.5), 1))
        par(mar = c(0,0,0,0), bg = "black", fg = "white", cex = 1.5)
        plot(nii, position = position, center_position = TRUE, which = "axial", pixel_width = 0.5, vlim = vlim)
        plot(nii, position = position, center_position = TRUE, which = "coronal", pixel_width = 0.5, vlim = vlim)
        plot(nii, position = position, center_position = TRUE, which = "sagittal", pixel_width = 0.5, vlim = vlim)
        plot.new()
        legend("center",
               sprintf(
                 "Slices: %s, Voxel sizes: %s (Anatomical View)",
                 paste(shape, collapse = "x"),
                 paste(sprintf("%.2f", voxel_size), collapse = ", ")
               ), bty = "n", cex = 1.2)

      }),
      cache = "session",
      ct_preview_path()
    ),
    ct_preview_path(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


  mri_preview_path <- shiny::reactive({
    loaded_flag <- ravedash::watch_data_loaded()
    if(!loaded_flag){ return() }
    subject <- pipeline$read("subject")

    params <- input_params()
    nii_t1 <- params$nii_t1
    path <- file.path(subject$preprocess_settings$raw_path,
                      "rave-imaging", "inputs", "MRI", nii_t1)
    path
  })
  output$mri_preview <- shiny::bindEvent(
    shiny::bindCache(
      shiny::renderPlot({
        loaded_flag <- ravedash::watch_data_loaded()
        if(!loaded_flag){ return() }
        subject <- pipeline$read("subject")

        params <- input_params()
        nii_t1 <- params$nii_t1
        path <- file.path(subject$preprocess_settings$raw_path,
                          "rave-imaging", "inputs", "MRI", nii_t1)
        has_path <- FALSE
        nii <- NULL
        if(length(path) == 1 && !is.na(path) && file.exists(path)) {
          try({
            nii <- ieegio::read_volume(path)
            has_path <- TRUE
          }, silent = TRUE)
        }
        shiny::validate(shiny::need(has_path, message = "No valid MRI file selected"))

        voxel_size <- nii$transforms[[1]] %*% c(1, 1, 1, 0)
        voxel_size <- voxel_size[c(1, 2, 3)]
        shape <- dim(nii)
        vlim <- range(nii[])

        old_par <- par(c("mfrow", "mar", "bg", "fg", "cex"))
        on.exit({
          do.call(par, old_par)
        })
        layout(matrix(c(4,4,4,1,2,3), nrow = 2, byrow = TRUE),
               heights = c(lcm(1.5), 1))
        par(mar = c(0,0,0,0), bg = "black", fg = "white", cex = 1.5)
        plot(nii, position = c(0, 0, 0), center_position = TRUE, which = "axial", pixel_width = 0.5, vlim = vlim)
        plot(nii, position = c(0, 0, 0), center_position = TRUE, which = "coronal", pixel_width = 0.5, vlim = vlim)
        plot(nii, position = c(0, 0, 0), center_position = TRUE, which = "sagittal", pixel_width = 0.5, vlim = vlim)
        plot.new()
        legend("center",
               sprintf(
                 "Slices: %s, Voxel sizes: %s (Anatomical View)",
                 paste(shape, collapse = "x"),
                 paste(sprintf("%.2f", voxel_size), collapse = ", ")
               ), bty = "n", cex = 1.2)
      }),
      cache = "session",
      mri_preview_path()
    ),
    mri_preview_path(),
    ignoreNULL = TRUE, ignoreInit = FALSE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      params <- input_params()
      # save parameters
      pipeline$set_settings(
        params = params
      )
      shidashi::clear_notifications(class = ns("error_notif"))

      res <- pipeline$eval(
        names = c("settings_path", "settings", "params", "subject", "cmd_tools",
                  "check_result", "import_T1", "import_CT", "image_segmentation",
                  "morphmri_ants", "coreg_flirt", "coreg_niftyreg", "coreg_ants",
                  "coreg_3dallineate", "coreg_nipy"),
        env = local_env, clean = FALSE
      )
      # res <- pipeline$run(
      #   as_promise = FALSE,
      #   names = c("settings", 'subject', "params", "import_T1", "import_CT",
      #             "image_segmentation", "coreg_flirt", "coreg_3dallineate"),
      #   type = "vanilla",
      #   scheduler = "none",
      #   shortcut = TRUE
      # )
      local_reactives$bash_scripts <- as.list(res)

    }, error_wrapper = "notification"),
    local_reactives$build_command,
    input_params(),
    ignoreNULL = FALSE, ignoreInit = FALSE
  )


  render_shell <- function(cmd, shell = "/bin/sh"){
    cmd <- paste(cmd, collapse = "\n")
    cmd <- unlist(strsplit(cmd, "\n"))
    shiny::div(
      # class = "clipboard-btn shidashi-clipboard-output",
      shiny::pre(
        class='pre-compact bg-gray-90',
        lapply(cmd, function(s){
          if(startsWith(trimws(s), "#")) {
            shiny::tags$code(class = "hljs-comment", s)
          } else {
            shiny::tags$code(class = "hljs-literal", s)
          }
        })
      ),
      `data-clipboard-text` = paste(c(
        shell,
        unlist(lapply(cmd, function(s){
          if(startsWith(trimws(s), "#")) { s <- NULL }
          s
        })),
        "\n"
      ), collapse = "\n"),
      # title='Click to copy!',
      # role = "button"
    )
  }

  output$panel_import_T1 <- shiny::renderUI({
    cmd <- local_reactives$bash_scripts$import_T1
    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    render_shell(cmd$script)
  })

  output$panel_import_CT <- shiny::renderUI({
    cmd <- local_reactives$bash_scripts$import_CT
    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    render_shell(cmd$script)
  })

  output$panel_image_segmentation <- shiny::renderUI({
    cmd <- local_reactives$bash_scripts$image_segmentation
    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    render_shell(cmd$script)
  })

  output$panel_coreg <- shiny::renderUI({
    program <- paste(input$coreg_ct_program, collapse = "")
    cmd <- switch(
      program,
      "AFNI" = {
        local_reactives$bash_scripts$coreg_3dallineate
      },
      "FSL" = {
        local_reactives$bash_scripts$coreg_flirt
      },
      "img_pipe" = {
        local_reactives$bash_scripts$coreg_nipy
      },
      "NiftyReg" = {
        local_reactives$bash_scripts$coreg_niftyreg
      },
      "ANTs" = {
        local_reactives$bash_scripts$coreg_ants
      },
      {
        "stop('Unknown coregistration command')"
      }
    )

    shiny::validate(
      shiny::need(!isTRUE(cmd$error),
                  message = cmd$condition$message)
    )
    if(program %in% c("AFNI", "FSL")) {
      shell <- "/bin/sh"
    } else {
      shell <- Sys.which("Rscript")
    }

    render_shell(cmd$script, shell = shell)
  })



  # ACPC alignment
  shiny::bindEvent(
    ravedash::safe_observe({
      mri_file <- input$param_acpc_infile
      if(!length(mri_file)) {
        stop("No MRI file specified. Please select one.")
      }
      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = "param_acpc_start",
        type = "default",
        disabled = TRUE
      )
      ravedash::show_notification(
        title = "Loading viewer",
        message = "Loading viewer... This might take a while. Please be patient...",
        type = "info",
        autohide = FALSE
      )
      on.exit({
        ravedash::clear_notifications()
      }, add = TRUE, after = TRUE)
      pipeline$set_settings(
        acpc_infile = mri_file
      )
      results <- pipeline$eval('viewer_acpc')
      local_reactives$viewer_acpc <- results$viewer_acpc
    }, error_wrapper = "notification"),
    input$param_acpc_start,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$acpc_3dviewer <- threeBrain::renderBrain({

    on.exit({
      dipsaus::updateActionButtonStyled(
        session = session,
        inputId = "param_acpc_start",
        disabled = FALSE
      )
    }, add = TRUE, after = TRUE)

    if( !is.null(local_reactives$viewer_acpc) ) {
      viewer_acpc <- local_reactives$viewer_acpc
      viewer_acpc$plot(
        control_presets = "acpcrealign",
        background = "#000000",
      )
    }
  })

  acpc_proxy <- threeBrain::brain_proxy('acpc_3dviewer', session = session)

  output$acpc_results <- shiny::renderUI({
    acpc <- local_reactives$acpc_alignment
    if(is.null(local_reactives$viewer_acpc)) {
      return(shiny::p("Please choose a MRI file (if there is no file in the dropdown menu, please click on ", ravedash::shiny_icons$refresh, " button to update file list). Then click on 'Start re-alignment' button."))
    }
    if(!is.list(acpc) || !isTRUE(acpc$ac_set)) {
      return(p("Please use the controller 'Click to Register/Reset AC' to start localizing AC. Try to focus the crosshair (in the slice panels) to the anterior commissure. Once finished, click on 'Confirm Updating AC'."))
    }
    if(!isTRUE(acpc$pc_set)) {
      return("You have AC set. Now use the controller 'Click to Register/Reset PC' to start localizing PC. Again, focus the crosshair to the posterior commissure. Once finished, click on 'Confirm Updating PC'.")
    }
    re <- apply(acpc$ras2acpc, 1, function(r) {
      paste(sprintf("%.2f", r), collapse = ",")
    })
    re <- paste(sprintf("[%s]", re), collapse = "\n")
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        "ScannerRAS to ACPC transform: ",
        shiny::code(re),
        "You can always use the controller 'Click to Set/Reset Rotation' to adjust the Interhemispheric point until you are satisfied with this alignment. Then click the button below to save the ACPC alignment."
      ),
      shiny::column(
        width = 12L,
        dipsaus::actionButtonStyled(ns("acpc_save"), "Save ACPC alignment results.", width = "100%")
      )
    )

  })
  shiny::bindEvent(
    ravedash::safe_observe({
      local_reactives$acpc_alignment <- acpc_proxy$acpc_alignment
    }),
    acpc_proxy$acpc_alignment,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      acpc <- local_reactives$acpc_alignment
      if(!is.list(acpc) || !isTRUE(acpc$ac_set)) {
        stop("Anterior commissure is unset. Please set the position of AC.")
      }
      if(!isTRUE(acpc$pc_set)) {
        stop("Posterior commissure is unset. Please set the position of PC.")
      }

      dipsaus::shiny_alert2(
        title = "Wait...",
        text = "Saving ACPC alignment results.",
        icon = "info",
        auto_close = FALSE,
        buttons = FALSE
      )
      suc <- FALSE
      on.exit({
        if(!suc) {
          Sys.sleep(0.5)
          dipsaus::close_alert2()
        }
      })

      subject <- pipeline$read("subject")
      acpc_infile <- pipeline$get_settings("acpc_infile")
      path_root <- file.path(subject$preprocess_settings$raw_path, "rave-imaging")
      path_deriv <- ravepipeline::dir_create2(file.path(path_root, "derivative"))
      path_mri <- file.path(path_root, "inputs", "MRI", acpc_infile)
      path_acpc_root <- file.path(path_root, "acpc-alignment")
      path_acpc_mri <- ravepipeline::dir_create2(file.path(path_root, "acpc-alignment", "mri"))
      path_acpc_img <- file.path(path_acpc_mri, "brain_acpc.nii.gz")
      path_acpc_trans <- file.path(path_acpc_root, "MR_Scanner_to_MR_ACPC.txt")

      # Create ACPC aligned image
      orig <- ieegio::read_volume(file = path_mri)
      sform <- acpc$ras2acpc %*% orig$transforms$vox2ras

      # save
      ieegio::io_write_nii(x = orig[], con = path_acpc_img, vox2ras = sform, gzipped = TRUE)
      file.copy(path_acpc_img, file.path(path_root, "inputs", "MRI", "MRI_acpc.nii.gz"))
      utils::write.table(acpc$ras2acpc, file = path_acpc_trans,
                         row.names = FALSE, col.names = FALSE)

      # Backup
      bkuppath_acpc_img <- file.path(path_deriv, "MRI_acpc.nii.gz")
      bkuppath_acpc_trans <- file.path(path_deriv, "MR_Scanner_to_MR_ACPC.txt")
      file.copy(path_acpc_img, bkuppath_acpc_img)
      utils::write.table(acpc$ras2acpc, file = bkuppath_acpc_trans,
                         row.names = FALSE, col.names = FALSE)

      ravepipeline::save_yaml(
        file = file.path(path_deriv, "conf-acpc.yaml"),
        list(
          "Heads up" = "Do NOT edit this file",
          "profile" = "ACPC Re-alignment",
          "work_path" = path_root,
          "timestamp" = strftime(Sys.time(), "%a %b %d %H:%M:%S UTC %Y"),
          "input_image" = list(
            type = "nifti",
            path = path_mri,
            comment = "original MRI image file"
          ),
          "outputs" = list(
            "aligned_image" = list(
              type = "nifti",
              path = "./acpc-alignment/mri/brain_acpc.nii.gz",
              backup = "./derivative/MRI_acpc.nii.gz",
              comment = "ACPC re-aligned image with sform set as transform from IJK (voxel) to ACPC, and qform set as transform from IJK to ScannerRAS."
            ),
            "ACPC" = list(
              type = "transform",
              "dimension" = "4x4",
              "transform_from" = list(
                coordinate_system = "scanner",
                orientation = "RAS"
              ),
              "transform_to" = list(
                coordinate_system = "ACPC",
                orientation = "RAS"
              ),
              path = './acpc-alignment/MR_Scanner_to_MR_ACPC.txt',
              backup = "./derivative/MR_Scanner_to_MR_ACPC.txt",
              comment = "From Scanner to ACPC coordinate"
            )
          )
        )
      )
      Sys.sleep(0.5)
      dipsaus::close_alert2()
      suc <- TRUE
      dipsaus::shiny_alert2(
        title = "Done!",
        icon = "success",
        text = "ACPC alignment results saved to rave-imaging derivatives folder."
      )
      update_nii_t1()
    }),
    input$acpc_save,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


}
