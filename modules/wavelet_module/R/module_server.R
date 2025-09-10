
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    refresh = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_registry <- ravedash::register_rave_session()
  server_tools <- get_default_handlers(session = session)
  report_wizard <- ravedash::create_report_wizard(pipeline = pipeline, session = session)

  error_notification <- function(e) {
    shidashi::show_notification(
      message = e$message,
      title = "Error found!",
      type = "danger",
      close = TRUE,
      autohide = TRUE,
      class = ns("error_notif"),
      collapse = "\n"
    )
  }

  # ---- generate reports ------------------------------------
  report_btn_onclick <- shiny::bindEvent(
    shiny::reactive({
      if(!ravedash::watch_data_loaded()) { return(FALSE) }
      if(ravedash::watch_loader_opened()) { return(FALSE) }
      res <- server_registry$rave_event$message_button_clicked
      structure(!is.null(res), timestamp = res)
    }),
    server_registry$rave_event$message_button_clicked,
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(!report_btn_onclick()) { return() }
      if(is.null(report_wizard)) { return() }
      report_wizard$launch(subject = pipeline$read("subject"), multiple = TRUE)
    }),
    report_btn_onclick(),
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  # --------------------------------------------------------


  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      tryCatch({
        if(!sv$is_valid()) {
          stop("There are some invalid inputs. Please fix them before applying wavelet")
        }

        # collect information
        tbl <- kernel_params()
        if(!is.data.frame(tbl)) {
          stop("No kernel table found. ")
        }

        use_float <- input$precision
        target_sample_rate <- as.numeric(input$target_sample_rate)
        pre_downsample <- as.numeric(input$pre_downsample)
        pipeline$set_settings(
          precision = ifelse(use_float, "float", "double"),
          pre_downsample = pre_downsample,
          target_sample_rate = target_sample_rate,
          kernel_table = tbl
        )

        res <- pipeline$run(as_promise = TRUE, names = "kernels")

        res$promise$then(
          onFulfilled = function(...){

            settings <- pipeline$get_settings()

            shiny::showModal(shiny::modalDialog(
              title = "Confirmation",
              size = "l",
              easyClose = FALSE,
              shiny::p("Wavelet will take a while to run. Please make sure that the following information is correct before proceeding."),
              shiny::tags$ul(
                shiny::tags$li(
                  shiny::strong("Subject: "),
                  settings$project_name, "/", settings$subject_code
                ),
                shiny::tags$li(
                  shiny::strong("Frequencies: "),
                  dipsaus::deparse_svec(settings$kernel_table$Frequency, collapse = ", ")
                ),
                shiny::tags$li(
                  shiny::strong("# of cycles: "),
                  paste(sprintf("%.1f", settings$kernel_table$Cycles), collapse = ", ")
                ),
                shiny::tags$li(
                  shiny::strong("Precision: "),
                  settings$precision
                )
              ),
              shiny::p("The following steps will be performed:"),
              shiny::tags$ol(
                shiny::tags$li(
                  ifelse(
                    settings$pre_downsample == 1,
                    "No down-sample will be performed before wavelet",
                    sprintf("Signals will be down-sampled by %s", settings$pre_downsample)
                  )
                ),
                shiny::tags$li("Wavelet will run on each block"),
                shiny::tags$li(sprintf("The wavelet coefficients will be down-sampled to %.1fHz before saving to disk", settings$target_sample_rate))
              ),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton(ns("wavelet_confirm_btn2"), "Confirm and run in background"),
                dipsaus::actionButtonStyled(ns("wavelet_confirm_btn"), "Confirm")
              )
            ))

          },
          onRejected = function(e){
            error_notification(e)
          }
        )


      }, error = function(e){
        error_notification(e)
      })

      return()

    }),
    input$wavelet_do_btn,
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  run_wavelet <- function(async = FALSE) {
    dipsaus::shiny_alert2(
      title = "Running Wavelet in progress",
      text = ravedash::be_patient_text(),
      icon = 'info',
      auto_close = FALSE,
      buttons = FALSE
    )

    if( async ) {
      local_data$wavelet_jobid <- ravepipeline::start_job(
        function(pipeline) {
          print(pipeline)
          pipeline$run(
            names = "wavelet_params",
            scheduler = "none",
            type = "vanilla",
            callr_function = NULL,
            return_values = FALSE
          )
        },
        fun_args = list(pipeline = pipeline),
        name = "Running Wavelet", method = "callr"
      )
      promise <- promises::as.promise(local_data$wavelet_jobid)
    } else {
      local_data$wavelet_results <- pipeline$run(
        names = "wavelet_params",
        scheduler = "none",
        type = "smart",
        callr_function = NULL,
        as_promise = TRUE,
        return_values = FALSE
      )
      promise <- local_data$wavelet_results$promise
    }

    promise$then(
      onFulfilled = function(...){

        dipsaus::close_alert2()
        dipsaus::shiny_alert2(
          title = "Done!",
          text = "Wavelet has finished. Trying to make data compatible with RAVE 1.0 modules... (Please wait...)",
          icon = 'success',
          auto_close = FALSE,
          buttons = FALSE
        )
        Sys.sleep(0.5)

        tryCatch({
          pipeline$run(
            as_promise = FALSE,
            names = c("subject", "clear_cache"),
            scheduler = "none",
            callr_function = NULL,
            type = "vanilla",
            async = FALSE,
            shortcut = TRUE
          )

          # save pipeline to subject
          subject <- pipeline$read("subject")
          fork_path <- file.path(subject$pipeline_path, pipeline$pipeline_name)
          if(file.exists(fork_path)) {
            ravecore::backup_file(fork_path, remove = TRUE)
          }
          pipeline$fork(fork_path)

          # RAVE 1.0 is over... We do not need them anymore
          # raveio::with_future_parallel({
          #   raveio::rave_subject_format_conversion(subject = subject)
          # })
          dipsaus::close_alert2()
          dipsaus::shiny_alert2(
            title = "Done!",
            text = "Please feel free to close this dialogue",
            icon = 'success',
            auto_close = TRUE,
            buttons = list("OK" = TRUE)
          )
        }, error = function(e) {
          dipsaus::close_alert2()
          dipsaus::shiny_alert2(
            title = "Wavelet done, but...",
            text = sprintf("Wavelet has finished. You should be able to run RAVE 2.0 pipelines freely. However, an error occurred while back-porting data to RAVE 1.0 format. Therefore, you might encounter some troubles running 1.0 modules. If you do need RAVE 1.0 modules, please go to [Data Tools] module to validate your data and manually convert the format.\n\nError message: %s", e$message),
            icon = 'warning',
            auto_close = TRUE,
            buttons = list("OK" = TRUE)
          )
        })

        shiny::removeModal()

      },
      onRejected = function(e){

        dipsaus::close_alert2()
        dipsaus::shiny_alert2(
          title = "Errors",
          text = paste(c("The following error is found while applying wavelet:",
                         e$message), collapse = " \n"),
          icon = 'error',
          danger_mode = TRUE,
          auto_close = FALSE,
          buttons = list("OK" = TRUE)
        )

      }
    )
  }
  shiny::bindEvent(
    ravedash::safe_observe({
      run_wavelet(async = FALSE)
    }),
    input$wavelet_confirm_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      run_wavelet(async = TRUE)
    }),
    input$wavelet_confirm_btn2,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }


      subject <- pipeline$read("subject")
      subject <- ravecore::as_rave_subject(subject, strict = FALSE)

      all_electrodes <- subject$electrodes
      etypes <- subject$preprocess_settings$electrode_types
      notch_filtered <- subject$preprocess_settings$notch_filtered

      electrodes <- all_electrodes[
        etypes %in% c("LFP", "EKG", "Audio") & notch_filtered
      ]
      sample_rates <- subject$raw_sample_rates
      sample_rates <- sapply(electrodes, function(e){
        sample_rates[all_electrodes == e]
      })
      etypes <- sapply(electrodes, function(e){
        etypes[all_electrodes == e]
      })

      local_data$subject <- subject
      local_data$electrodes <- electrodes
      local_data$sample_rates <- sample_rates
      local_data$electrode_types <- etypes


      local_reactives$refresh <- Sys.time()

      # reset inputs
      fork_path <- file.path(subject$pipeline_path, pipeline$pipeline_name)
      if(dir.exists(fork_path)) {
        try(silent = FALSE, expr = {
          forked_pipeline <- ravepipeline::pipeline(pipeline_name = pipeline$pipeline_name,
                                              paths = subject$pipeline_path)
          # set default settings
          previous_settings <- forked_pipeline$get_settings()
          target_sample_rate <- previous_settings$target_sample_rate
          pre_downsample <- previous_settings$pre_downsample
          precision <- previous_settings$precision

          if(length(pre_downsample) == 1 && !is.na(pre_downsample) &&
             pre_downsample >= 1) {
            dsamp <- floor(min(sample_rates) / target_sample_rate / 2)
            if(dsamp <= 1){
              dsamp <- 1
            }
            dsamp <- 2^(seq_len(floor(log2(dsamp)) + 1) - 1)
            pre_downsample <- as.integer(pre_downsample) %OF% dsamp
            local_reactives$pre_downsample <- pre_downsample
            shiny::updateSelectInput(
              session = session,
              inputId = "pre_downsample",
              choices = dsamp,
              selected = pre_downsample
            )

          }

          if(length(target_sample_rate) == 1 && !is.na(target_sample_rate) &&
             target_sample_rate > 0) {
            shiny::updateNumericInput(
              session = session,
              inputId = "target_sample_rate",
              value = target_sample_rate
            )
          }
          shiny::updateCheckboxInput(
            session = session,
            inputId = "precision",
            value = identical(precision, "float")
          )

        })
      }


      # Reset outputs
      shidashi::reset_output("kernel_plot", message = "Subject has been reset")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  # validator
  sv <- shinyvalidate::InputValidator$new(session = session)
  sv$add_rule(
    "target_sample_rate",
    function(value) {
      if(value <= 1) {
        return("Cannot down-sample wavelet coefficients to a sample rate less-equal than 1")
      }
      return()
    }
  )
  sv$add_rule(
    "pre_downsample",
    function(value) {
      if(!length(value)) {
        return("Please choose a value. (I suggest `1`)")
      }
      return()
    }
  )
  sv$add_rule(
    "preset_upload",
    function(value) {
      if(isTRUE(input$use_preset == "Upload preset")) {
        if(!is.data.frame(local_reactives$wavelet_param_tbl)) {
          return("Please upload a preset kernel table in csv format. The table should contain two columns: `Frequency` and `Cycles`")
        }
      }

      return()
    }
  )
  sv$enable()


  # updates
  shiny::bindEvent(
    ravedash::safe_observe({
      if(is.null(local_data$subject)){ return() }
      target_sample_rate <- input$target_sample_rate
      if(!length(target_sample_rate) ||
         is.na(target_sample_rate) ||
         target_sample_rate < 1){ return() }

      dsamp <- floor(min(local_data$sample_rates) / target_sample_rate / 2)
      if(dsamp <= 1){
        dsamp <- 1
      }
      dsamp <- 2^(seq_len(floor(log2(dsamp)) + 1) - 1)

      pre_downsample <- local_reactives$pre_downsample
      if(!length(pre_downsample)) {
        pre_downsample <- input$pre_downsample
      }
      local_reactives$pre_downsample <- NULL
      shiny::updateSelectInput(
        session = session,
        inputId = "pre_downsample",
        choices = dsamp,
        selected = as.integer(pre_downsample) %OF% dsamp
      )

    }),
    input$target_sample_rate,
    local_reactives$refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      if(is.null(local_data$subject)){ return() }
      dsamp <- input$pre_downsample
      if(!length(dsamp)) { return() }
      dsamp <- as.integer(dsamp)
      if(is.na(dsamp)){ return() }

      max_freq <- floor(min(local_data$sample_rates) / 2 / dsamp)

      shiny::updateSliderInput(
        session = session,
        inputId = "freq_range",
        max = max_freq
      )

    }),
    input$pre_downsample,
    local_reactives$refresh,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )



  shiny::bindEvent(
    ravedash::safe_observe({

      tryCatch({

        datapath <- input$preset_upload$datapath
        if(!file.exists(datapath)){ stop("The uploaded file is corrupted") }

        tbl <- utils::read.csv(datapath)
        if(!all(c("Frequency", "Cycles") %in% names(tbl))){
          stop("The uploaded csv table must contain the following two columns: 'Frequency', 'Cycles' (case-sensitive)")
        }
        tbl <- tbl[
          !is.na(tbl$Frequency) & tbl$Frequency != "" &
            !is.na(tbl$Cycles) & tbl$Cycles != "",
        ]
        freqs <- as.numeric(tbl$Frequency)
        cycle <- as.numeric(tbl$Cycles)

        if(any(is.na(freqs)) || any(is.na(cycle))) {
          stop("Cannot parse the parameter table: none numerical values detected.")
        }

        local_reactives$wavelet_param_tbl <- data.frame(
          Frequency = freqs,
          Cycles = cycle
        )

      }, error = function(e){
        error_notification(e)
      })

    }),
    input$preset_upload,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  kernel_params <- shiny::reactive({

    use_preset <- input$use_preset

    if(isTRUE(use_preset == "Builtin tool")) {
      freq_range <- input$freq_range
      step <- input$freq_step
      cycle_range <- input$cycle_range
      if(length(freq_range) != 2){
        freq_range <- c(2, 200)
      }
      if(length(cycle_range) != 2){
        cycle_range <- c(3, 20)
      }
      if(length(step) != 1){
        step <- 2
      }

      freqs <- seq(freq_range[1], freq_range[2], by = step)

      v1 <- log(cycle_range[[1]])
      v2 <- log(cycle_range[[2]])

      cycle <- (v2 - v1) / (log(freq_range[[2]]) - log(freq_range[[1]])) *
        (log(freqs) - log(freq_range[[1]])) + v1

      cycle <- round(exp(cycle))

      tbl <- data.frame(
        Frequency = freqs,
        Cycles = cycle
      )
      return(tbl)
    } else {
      return(local_reactives$wavelet_param_tbl)
    }

  })



  # shiny::outputOptions(output, "kernel_table", suspendWhenHidden = FALSE)
  ravedash::register_output(
    DT::renderDataTable({
      tbl <- kernel_params()
      shiny::validate(
        shiny::need(
          is.data.frame(tbl),
          message = "Please upload preset table"
        )
      )
      DT::datatable(tbl)
    }),
    outputId = "kernel_table",
    output_type = "data",
    extensions = list("CSV" = "csv"),
    download_function = function(con, params, ...) {
      tbl <- kernel_params()
      utils::write.csv(tbl, file = con, row.names = FALSE)
    }
  )

  output$download_kernel_table <- shiny::downloadHandler(
    "RAVE-wavelet-parameters.csv",
    function(conn){
      tryCatch({
        tbl <- kernel_params()
        utils::write.csv(tbl, file = conn, row.names = FALSE)
      }, error = function(e){
        error_notification(list(message = "The wavelet does not have any configurations"))
      })
    }
  )


  ravedash::register_output(
    shiny::renderPlot({

      local_reactives$refresh
      tbl <- kernel_params()

      srate <- local_data$sample_rates
      etypes <- local_data$electrode_types
      theme <- ravedash::current_shiny_theme()

      shiny::validate(
        shiny::need(
          is.data.frame(tbl),
          message = "Please upload preset table"
        ),
        shiny::need(
          length(srate) > 0,
          message = "No electrodes found"
        )
      )

      if('LFP' %in% etypes){
        srate <- srate[etypes == "LFP"][[1]]
      } else {
        srate <- min(srate)
      }

      old_theme <- graphics::par(c("fg", "bg", "col.axis", "col.lab", "col.main", "col.sub"))
      graphics::par(
        fg = theme$foreground,
        bg = theme$background,
        col.axis = theme$foreground,
        col.lab = theme$foreground,
        col.main = theme$foreground,
        col.sub = theme$foreground
      )
      on.exit({
        do.call(graphics::par, old_theme)
      }, add = TRUE)
      kernel <- ravetools::wavelet_kernels(
        freqs = tbl$Frequency, wave_num = tbl$Cycles,
        srate = srate
      )
      plot(kernel, cex = 1.4, mai = c(1.02,0.82,0.82,0.42))
    }),
    outputId = "kernel_plot"
  )

}
