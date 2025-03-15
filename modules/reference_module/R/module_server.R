
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  error_notification <- function(e) {
    if(!inherits(e, "condition")) {
      e <- simpleError(message = e$message)
    }
    ravedash::logger_error_condition(e)
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
    ravedash::logger(..., level = "info")
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

  get_reference_options <- function(){
    loaded_flag <- shiny::isolate(ravedash::watch_data_loaded())
    if(!loaded_flag){ return() }

    repo <- component_container$data$repository
    if(!length(repo)) { return() }
    subject <- repo$subject
    if(!length(subject)) { return() }

    refs <- list.files(subject$reference_path, pattern = "^ref_.*\\.h5", ignore.case = TRUE, full.names = FALSE, include.dirs = FALSE, recursive = FALSE)
    refs <- gsub('\\.h5$', "", refs, ignore.case = TRUE)
    refs <- unique(refs)

    if(!length(refs)) {
      ravedash::logger("Trying to get reference signals... No reference found.", level = "trace")
      return()
    }
    ravedash::logger("Trying to get reference signals... Available references are: ",
                     paste(refs, collapse = ", "), level = "trace")

    return(refs)
  }

  get_reference_data <- function(name) {
    loaded_flag <- shiny::isolate(ravedash::watch_data_loaded())
    if(!loaded_flag){ return() }

    repo <- component_container$data$repository
    if(!length(repo)) { return() }
    subject <- repo$subject
    if(!length(subject)) { return() }
    if(!name %in% get_reference_options()) { return() }

    re <- local_data$reference_data[[name]]

    has_wavelet <- all(subject$has_wavelet)

    if( has_wavelet ) {
      if(all(c("voltage", "wavelet") %in% names(re))){ return(re) }
    } else {
      if(all(c("voltage") %in% names(re))){ return(re) }
    }

    re <- list()
    inst <- raveio::new_reference(subject = subject, number = name)
    re$voltage <- inst$load_blocks(subject$blocks, simplify = FALSE, type = "voltage")

    if( has_wavelet ) {
      re$wavelet <- inst$load_blocks(subject$blocks, simplify = FALSE, type = "wavelet-coefficient")
    }
    local_data$reference_data[[name]] <- re
    return(re)

  }

  voltage_data <- shiny::bindEvent(
    shiny::reactive({
      data_loaded <- ravedash::watch_data_loaded()
      if(!data_loaded) { return() }
      block <- input$plot_block
      if(!length(block)){ return() }

      repo <- component_container$data$repository
      if(!length(repo)){ return() }
      subject <- repo$subject
      if(!length(subject)){ return() }
      if(!isTRUE(block %in% subject$blocks)){ return() }

      re <- local_data$voltage_data[[block]]

      if(is.list(re)) {
        return(re)
      }

      voltage_data <- pipeline$read(var_names = "voltage_data")
      if(!is.list(voltage_data) || !is.list(voltage_data$data)){ return() }
      arr <- voltage_data$data[[block]]

      srates <- subject$raw_sample_rates[subject$electrodes %in% voltage_data$electrodes]

      if(!length(srates)){
        error_notification(list(message = sprintf("Cannot determine the sample rate from this group of electrodes. Please contact RAVE team to report this bug.")))
        return()
      }

      re <- list(
        subject = subject,
        data = arr[drop=FALSE],
        electrodes = voltage_data$electrodes,
        sample_rate = srates[[1]]
      )
      local_data$voltage_data[[block]] <- re
      return(re)
    }),
    input$plot_block,
    ravedash::watch_data_loaded(),
    ignoreInit = FALSE, ignoreNULL = FALSE
  )

  output$reference_output_tabset_footer <- shiny::renderUI({

    data_loaded <- ravedash::watch_data_loaded()

    ginfo <- current_group()
    shiny::validate(
      shiny::need(isTRUE(data_loaded), message = "Data not loaded"),
      shiny::need(is.list(ginfo), message = "Please choose a reference group")
    )

    subject <- ginfo$subject
    sample_rate <- ginfo$sample_rate

    current_tab <- input$reference_output_tabset
    if(!length(current_tab)){ current_tab <- "Group inspection" }
    blocks <- subject$blocks
    block <- shiny::isolate(input$plot_block) %OF% blocks


    switch (
      current_tab,
      `Group inspection` = {

        ginsp_type_choices <- c(
          "Show all",
          "Show original signals only",
          "Show referenced signals only"
        )

        ginsp_type <- shiny::isolate(input$ginsp_type) %OF% ginsp_type_choices
        ginsp_gap <- shiny::isolate(input$ginsp_gap)
        if(!length(ginsp_gap) || !is.numeric(ginsp_gap) || ginsp_gap <= 0) { ginsp_gap <- 0.999 }

        ginsp_duration <- shiny::isolate(input$ginsp_duration)
        if(!length(ginsp_duration) || !is.numeric(ginsp_duration) || ginsp_duration <= 0) { ginsp_duration <- 5 }

        ginsp_start_max <- ginsp_start_max()
        ginsp_start <- shiny::isolate(input$ginsp_start)
        if(!length(ginsp_start) || !is.numeric(ginsp_start)){ ginsp_start <- 0 }

        if(ginsp_start > ginsp_start_max){ ginsp_start <- ginsp_start_max }

        shiny::fluidRow(
          shiny::column(
            width = 2L,
            shiny::selectInput(
              inputId = ns("plot_block"),
              label = "Session block",
              choices = blocks,
              selected = block,
              selectize = FALSE
            )
          ),
          shiny::column(
            width = 2L,
            shiny::sliderInput(
              inputId = ns("ginsp_start"),
              label = "Start time (s)",
              min = 0, max = ginsp_start_max, value = ginsp_start
            )
          ),
          shiny::column(
            width = 2L,
            shiny::sliderInput(
              inputId = ns("ginsp_duration"),
              label = "Plot duration (s)",
              min = 1, max = 30, value = ginsp_duration
            )

          ),
          shiny::column(
            width = 2L,
            shiny::numericInput(
              inputId = ns("ginsp_gap"),
              label = "Vertical spacing",
              min = 0.9, step = 0.001, value = ginsp_gap
            )
          ),
          shiny::column(
            width = 2L,
            shiny::textInput(
              inputId = ns("ginsp_hide"),
              label = "Hide electrodes",
              value = c(shiny::isolate(input$ginsp_hide), "")[[1]],
              placeholder = "e.g. 1-12,64"
            )
          ),
          shiny::column(
            width = 2L,
            shiny::selectInput(
              inputId = ns("ginsp_type"),
              label = "Signal type",
              choices = ginsp_type_choices,
              selected = ginsp_type, selectize = FALSE
            )
          )
        )
      },
      `Electrode details` = {

        einsp_winlen_max <- floor(sample_rate * 2)
        einsp_winlen <- shiny::isolate(input$einsp_winlen)

        if(length(einsp_winlen) != 1 || !(is.numeric(einsp_winlen)) || einsp_winlen < 100 ||
           einsp_winlen > einsp_winlen_max) {
          einsp_winlen <- einsp_winlen_max
        }

        einsp_freq_max <- floor(sample_rate / 2)
        einsp_freq <- shiny::isolate(input$einsp_freq)
        if(!length(einsp_freq) || !(is.numeric(einsp_freq)) || einsp_freq < 10 ) {
          einsp_freq <- 300
        }
        if(einsp_freq > einsp_freq_max) {
          einsp_freq <- einsp_freq_max
        }

        electrode_choice <- as.character(ginfo$data$Electrode)

        shiny::fluidRow(
          shiny::column(
            width = 2L,
            shiny::selectInput(
              inputId = ns("plot_block"),
              label = "Session block",
              choices = blocks,
              selected = block,
              selectize = FALSE
            )
          ),
          shiny::column(
            width = 4L,
            shidashi::flex_container(
              shidashi::flex_item(
                .class = "fill-width",
                shiny::tags$label(
                  class="control-label",
                  "Electrode"
                )
              ),
              shidashi::flex_break(),
              shidashi::flex_item(
                size = 2L,
                .class = "fill-width",
                shiny::selectInput(
                  inputId = ns("einsp_electrode"),
                  label = NULL,
                  choices = electrode_choice,
                  selected = shiny::isolate(input$einsp_electrode) %OF% electrode_choice,
                  width = "100%",
                  selectize = FALSE
                )
              ),
              shidashi::flex_item(
                size = 1L,
                .class = "fill-width padding-left-5 padding-right-5",
                shiny::actionButton(inputId = ns("einsp_prev"), "Previous", width = "100%")
              ),
              shidashi::flex_item(
                size = 1L,
                .class = "fill-width",
                shiny::actionButton(inputId = ns("einsp_next"), "Next", width = "100%")
              )
            )
          ),
          shiny::column(
            width = 3L,
            shiny::sliderInput(ns("einsp_winlen"), "Pwelch window length",
                               min = 100, max = einsp_winlen_max, value = einsp_winlen)
          ),
          shiny::column(
            width = 3L,
            shiny::sliderInput(ns("einsp_freq"), "Max frequency",
                               min = 10, max = einsp_freq_max, value = einsp_freq)
          )
        )

      },
      `Reference signal` = {


        epoch_chohices <- subject$epoch_names
        refinsp_epoch <- c(
          shiny::isolate(input$refinsp_epoch),
          subject$get_default("epoch_name")
        ) %OF% epoch_chohices

        refinsp_pre <- shiny::isolate(input$refinsp_pre)
        if(!length(refinsp_pre) || !isTRUE(refinsp_pre >= 0)) {
          refinsp_pre <- 1L
        }
        refinsp_post <- shiny::isolate(input$refinsp_post)
        if(!length(refinsp_post) || !isTRUE(refinsp_post >= 0)) {
          refinsp_post <- 2L
        }
        refinsp_baseline <- shiny::isolate(input$refinsp_baseline)
        if(length(refinsp_baseline) != 2 || refinsp_baseline[[1]] > refinsp_baseline[[2]]) {
          refinsp_baseline <- c(-refinsp_pre, 0)
        }

        refinsp_range <- shiny::isolate(input$refinsp_range)
        if(!length(refinsp_range) || is.na(refinsp_range)) {
          refinsp_range <- 0.99
        } else {
          refinsp_range <- abs(refinsp_range)
        }

        freq_range <- range(subject$preprocess_settings$wavelet_params$frequencies)
        refinsp_freq_range <- shiny::isolate(input$refinsp_freq_range)
        if(length(refinsp_freq_range) != 2) {
          refinsp_freq_range <- freq_range
        } else {
          refinsp_freq_range[refinsp_freq_range < freq_range[[1]]] <- freq_range[[1]]
          refinsp_freq_range[refinsp_freq_range > freq_range[[2]]] <- freq_range[[2]]
        }

        baseline_methods <- c("Power % change", "sqrt(power) % change", "decibel", "Power z-score", "sqrt(power) z-score")


        shiny::fluidRow(
          shiny::column(
            width = 3L,
            shidashi::flex_container(
              wrap = "nowrap",
              shidashi::flex_item(
                .class = "full-width padding-right-2",
                shiny::selectInput(
                  inputId = ns("refinsp_epoch"),
                  label = "Epoch",
                  choices = epoch_chohices,
                  selected = refinsp_epoch,
                  selectize = FALSE
                )
              ),
              shidashi::flex_item(
                .class = "full-width padding-right-2",
                shiny::numericInput(
                  inputId = ns("refinsp_pre"),
                  label = "Before",
                  value = refinsp_pre,
                  step = 1L,
                  min = 0L,
                  width = "100%"
                )
              ),
              shidashi::flex_item(
                .class = "full-width",
                shiny::numericInput(
                  inputId = ns("refinsp_post"),
                  label = "After",
                  value = refinsp_post,
                  step = 1L,
                  min = 0L,
                  width = "100%"
                )
              )
            )

          ),
          shiny::column(
            width = 3L,
            shiny::sliderInput(
              inputId = ns("refinsp_freq_range"),
              label = "Frequency range",
              min = freq_range[[1]],
              max = freq_range[[2]],
              value = refinsp_freq_range
            )
          ),
          shiny::column(
            width = 3L,
            shiny::sliderInput(
              inputId = ns("refinsp_baseline"),
              label = "Baseline window",
              min = -refinsp_pre, max = refinsp_post,
              value = refinsp_baseline,
              step = 0.1
            )
          ),
          shiny::column(
            width = 2L,
            shiny::selectInput(
              inputId = ns("refinsp_baseline_method"),
              label = "Baseline method",
              choices = baseline_methods,
              selected = shiny::isolate(input$refinsp_baseline_method) %OF% baseline_methods,
              selectize = FALSE
            )
          ),
          shiny::column(
            width = 1L,
            shiny::numericInput(
              inputId = ns("refinsp_range"),
              label = "Range",
              value = refinsp_range
            )
          )
        )
      },
      `Preview & Export` = {

        preview_save_name <- shiny::isolate(input$preview_save_name)
        if(!length(preview_save_name) || !nchar(trimws(preview_save_name))) {
          preview_save_name <- "default"
        }

        shiny::fluidRow(
          shiny::column(
            width = 4L,
            shiny::textInput(ns("preview_save_name"), "Reference name",
                             value = preview_save_name)
          ),
          shiny::column(
            width = 8L,
            shiny::textOutput(ns("preview_save_path"),
                              container = function(...){
                                shiny::div(style = "margin-bottom: 0.5rem;", ...)
                              }),
            dipsaus::actionButtonStyled(ns("preview_save_btn"), "Generate & save")
          )
        )
      }
    )
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      data_loaded <- ravedash::watch_data_loaded()
      repo <- component_container$data$repository
      subject <- repo$subject
      if(!data_loaded || is.null(subject)) { return() }

      table <- local_reactives$reference_table
      if(!is.data.frame(table)) {
        error_notification(list(
          "Fatal error: reference table is missing..."
        ))
        return()
      }

      name <- trimws(input$preview_save_name)
      if(!length(name) || !nchar(name)) {
        error_notification(list(
          "Reference name cannot be blank"
        ))
        return()
      }
      if(!grepl("^[a-zA-Z0-9_]+$", name)) {
        error_notification(list(
          sprintf("A valid reference name can only contain letters (a-zA-Z), digits (0-9), or underscore (_). Please revise your current input: [%s]", name)
        ))
        return()
      }

      save_path <- file.path(subject$meta_path, sprintf("reference_%s.csv", name))

      table <- table[order(table$Electrode), ]

      raveio::safe_write_csv(table, save_path, row.names = FALSE)

      dipsaus::shiny_alert2(
        title = "Succeed!",
        text = sprintf("Reference [%s] has been generated!", name),
        icon = "success", danger_mode = FALSE, auto_close = TRUE,
        buttons = list("OK" = TRUE)
      )

    }),
    input$preview_save_btn,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  output$preview_save_path <- shiny::renderText({
    data_loaded <- ravedash::watch_data_loaded()
    if(!data_loaded) { return() }
    name <- input$preview_save_name
    name <- trimws(name)
    if(!length(name) || !nchar(name)) {
      dipsaus::updateActionButtonStyled(
        session = session, inputId = "preview_save_btn", disabled = TRUE
      )
      return("Please enter a valid reference name")
    }
    if(!grepl("^[a-zA-Z0-9_]+$", name)) {
      dipsaus::updateActionButtonStyled(
        session = session, inputId = "preview_save_btn", disabled = TRUE
      )
      return("A valid reference name can only contain letters (a-zA-Z), digits (0-9), or underscore (_)")
    }
    dipsaus::updateActionButtonStyled(
      session = session, inputId = "preview_save_btn", disabled = FALSE
    )
    repo <- component_container$data$repository
    subject <- repo$subject
    exists <- name %in% subject$reference_names
    return(sprintf("Will be saved to [subject path]/meta/reference_%s.csv (mode: %s)",
                   name, ifelse(exists, "overwrite", "create")))
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      ginfo <- current_group()
      if(!is.list(ginfo)){ return() }

      electrode_choice <- as.character(ginfo$data$Electrode)
      nchoices <- length(electrode_choice)
      if(!nchoices) { return() }

      einsp_electrode <- as.character(input$einsp_electrode)
      if(!length(einsp_electrode)){
        idx <- length(electrode_choice)
      } else {
        idx <- which(electrode_choice == einsp_electrode)
      }
      idx <- idx - 1
      if(idx <= 0) {
        idx <- length(electrode_choice)
      }
      einsp_electrode <- electrode_choice[[idx]]

      shiny::updateSelectInput(
        session = session,
        inputId = "einsp_electrode",
        selected = einsp_electrode
      )
    }),
    input$einsp_prev,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      ginfo <- current_group()
      if(!is.list(ginfo)){ return() }

      electrode_choice <- as.character(ginfo$data$Electrode)
      nchoices <- length(electrode_choice)
      if(!nchoices) { return() }

      einsp_electrode <- as.character(input$einsp_electrode)
      if(!length(einsp_electrode)){
        idx <- 1
      } else {
        idx <- which(electrode_choice == einsp_electrode) + 1
        if(idx > nchoices) {
          idx <- 1
        }
      }
      einsp_electrode <- electrode_choice[[idx]]

      shiny::updateSelectInput(
        session = session,
        inputId = "einsp_electrode",
        selected = einsp_electrode
      )
    }),
    input$einsp_next,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  ginsp_start_max <- shiny::reactive({
    vdata <- voltage_data()
    if(!is.list(vdata)){ return(300) }
    floor(nrow(vdata$data) / vdata$sample_rate)
  })

  ravedash::register_output(
    shiny::renderPlot({

      data_loaded <- ravedash::watch_data_loaded()
      vdata <- voltage_data()
      ginsp_start <- input$ginsp_start
      ginsp_duration <- input$ginsp_duration
      ginsp_gap <- input$ginsp_gap
      ginsp_type <- input$ginsp_type
      ginsp_hide <- dipsaus::parse_svec(input$ginsp_hide)
      ginfo <- current_group()
      theme <- ravedash::current_shiny_theme()
      block <- input$plot_block
      existing_refs <- get_reference_options()

      if(!length(ginsp_gap) || is.na(ginsp_gap)){ ginsp_gap <- 0.999 }

      shiny::validate(
        shiny::need(isTRUE(data_loaded), message = "Data not loaded"),
        shiny::need(is.list(vdata), message = "Waiting for the data"),
        shiny::need(isTRUE(
          ginsp_start >= 0 && ginsp_duration >= 0 &&
            ginsp_gap > 0 && length(ginsp_type) == 1
        ), message = "Waiting for initialization"),
        shiny::need(is.list(ginfo), message = "Please choose a valid group")
      )

      electrodes <- ginfo$data$Electrode
      electrodes <- electrodes[!electrodes %in% ginsp_hide]
      electrodes <- vdata$electrodes[vdata$electrodes %in% electrodes]

      shiny::validate(
        shiny::need(length(electrodes) >= 1,
                    message = "At least 1 electrodes need to be selected to show."),
        shiny::need(length(block) == 1,
                    message = "Invalid block choice"),
      )

      old_theme <- graphics::par(c("fg", "bg", "col.axis", "col.lab", "col.main", "col.sub", "mai"))
      fg <- theme$foreground
      bg <- theme$background
      graphics::par(fg = fg, bg = bg, col.axis = fg, col.lab = fg, col.main = fg,
                    col.sub = fg, mai = c(0.8, 0.5, 0.42, 0.1))
      on.exit({ do.call(graphics::par, old_theme) }, add = TRUE)

      tidx_start <- round(ginsp_start * vdata$sample_rate)
      tidx_end <- round((ginsp_start + ginsp_duration) * vdata$sample_rate)
      max_tps <- nrow(vdata$data)

      if(tidx_start <= 0 ) { tidx_start <- 1 }
      if( tidx_end > max_tps ){ tidx_end <- max_tps }
      if( tidx_end <= tidx_start ) { tidx_start <- tidx_end - min(100, max_tps - 1) }
      ginsp_start <- (tidx_start - 1) / vdata$sample_rate
      ginsp_duration <- (tidx_end - tidx_start) / vdata$sample_rate

      tp_idx <- seq.int(tidx_start, tidx_end)
      elec_idx <- vdata$electrodes %in% electrodes
      signals <- vdata$data[tp_idx, , drop = FALSE]

      if(ginsp_gap <= 1) {
        ginsp_gap <- stats::quantile(signals, max(ginsp_gap, 0.01), na.rm = TRUE)
      }
      ginsp_gap <- abs(ginsp_gap)

      is_bipolar <- isTRUE(ginfo$data$Type[[1]] %in% reference_choices[4])

      get_cols <- function(col, invalid = "red"){
        re <- rep(col, length(electrodes))
        re[invalids] <- invalid
        if(!is_bipolar) {
          re <- c("orange", re)
        }
        re
      }

      ref_names <- ginfo$data$Reference
      invalids <- ref_names == ""
      can_show_refs <- TRUE


      if( is_bipolar ) {
        elec_idx2 <- which(elec_idx)
        refed_signals <- sapply(seq_along(ref_names), function(ii){
          name <- ref_names[[ii]]
          eidx <- elec_idx2[[ii]]
          name2 <- dipsaus::parse_svec(gsub("^ref_", "", name))
          if(name %in% c("", "noref") || !length(name2)) {
            return(signals[,eidx])
          }

          if(length(name2) == 1 && isTRUE(name2 %in% vdata$electrodes)) {
            ref_data <- signals[, vdata$electrodes %in% name2, drop = FALSE]
            return(signals[,eidx] - ref_data)
          }

          if(name %in% existing_refs) {
            ref_data <- get_reference_data(name)
            if(is.null(ref_data)) {
              ref_data <- 0
            } else {
              ref_data <- ref_data$voltage[[block]][tp_idx]
            }
            return(signals[,eidx] - ref_data)
          }
          return(signals[,eidx])
        })
        if(!is.matrix(refed_signals)) {
          dim(refed_signals) <- c(length(refed_signals) / length(ref_names), length(ref_names))
        }
        # refed_signals <- t(refed_signals[, elec_idx, drop = FALSE])
        refed_signals <- t(refed_signals)
        signals <- t(signals[, elec_idx, drop = FALSE])
        channel_names <- electrodes
        main <- "Reference: Bi-polar"
      } else {
        ref_signal <- unique(ginfo$data$Reference)
        ref_signal <- ref_signal[ref_signal %in% existing_refs]
        if(length(ref_signal)) {
          ref_signal <- ref_signal[[1]]
          ref_data <- get_reference_data(ref_signal)
          if(is.null(ref_data)) {
            ref_signal <- "noref"
            ref_data <- 0
          } else {
            ref_data <- ref_data$voltage[[block]][tp_idx]
          }
        } else {
          ref_signal <- ref_names[!ref_names %in% c("", "noref")]
          ref_signal <- unlist(lapply(ref_signal, function(x) {
            re <- dipsaus::parse_svec(gsub("^ref_", "", x))
            if(length(re) == 1 && re %in% vdata$electrodes) { return(re) }
            return(NULL)
          }))
          if(!length(ref_signal)) {
            ref_signal <- "noref"
            ref_data <- 0
          } else {
            ref_signal <- ref_signal[[1]]
            ref_data <- signals[, vdata$electrodes %in% ref_signal, drop = TRUE]
            ref_signal <- sprintf("ref_%s", ref_signal)
          }
        }
        signals <- signals[, elec_idx, drop = FALSE]
        refed_signals <- signals - ref_data

        signals <- t(cbind(ref_data, signals))
        refed_signals <- t(cbind(ref_data, refed_signals))
        channel_names <- c('REF', electrodes)

        main <- sprintf("Reference: %s", ref_signal)
      }

      if(ginsp_type == "Show original signals only") {
        ravetools::plot_signals(
          signals = signals,
          sample_rate = vdata$sample_rate,
          space = ginsp_gap,
          space_mode = "absolute",
          time_shift = ginsp_start,
          duration = ginsp_duration,
          compress = TRUE,
          channel_names = channel_names,
          ylab = "Electrode Channels",
          new_plot = TRUE,
          main = main,
          col = get_cols('dodgerblue3')
        )
      } else if (ginsp_type == "Show referenced signals only") {
        ravetools::plot_signals(
          signals = refed_signals,
          sample_rate = vdata$sample_rate,
          space = ginsp_gap,
          space_mode = "absolute",
          time_shift = ginsp_start,
          duration = ginsp_duration,
          compress = TRUE,
          channel_names = channel_names,
          ylab = "Electrode Channels",
          new_plot = TRUE,
          main = main,
          col = get_cols('gray60')
        )
      } else {
        ravetools::plot_signals(
          signals = signals,
          sample_rate = vdata$sample_rate,
          space = ginsp_gap,
          space_mode = "absolute",
          time_shift = ginsp_start,
          duration = ginsp_duration,
          compress = TRUE,
          channel_names = channel_names,
          ylab = "Electrode Channels",
          new_plot = TRUE,
          main = main,
          col = get_cols('dodgerblue3')
        )
        ravetools::plot_signals(
          signals = refed_signals,
          sample_rate = vdata$sample_rate,
          space = ginsp_gap,
          space_mode = "absolute",
          time_shift = ginsp_start,
          duration = ginsp_duration,
          compress = TRUE,
          channel_names = channel_names,
          ylab = "Electrode Channels",
          new_plot = FALSE,
          main = main,
          col = get_cols('gray60')
        )
      }

      return()

    }),
    outputId = "reference_plot_signals"
  )

  ravedash::register_output(
    shiny::renderPlot({
      data_loaded <- ravedash::watch_data_loaded()
      vdata <- voltage_data()
      einsp_electrode <- as.integer(input$einsp_electrode)
      einsp_winlen <- input$einsp_winlen
      einsp_freq <- input$einsp_freq
      ginfo <- current_group()
      theme <- ravedash::current_shiny_theme()
      block <- input$plot_block

      shiny::validate(
        shiny::need(isTRUE(data_loaded), message = "Data not loaded"),
        shiny::need(is.list(vdata), message = "Waiting for the data"),
        shiny::need(isTRUE(
          length(vdata$sample_rate) > 0 &&
            length(einsp_winlen) == 1 && length(einsp_freq) == 1 &&
            einsp_winlen > 0 && einsp_freq > 0 &&
            einsp_winlen <= 2 * vdata$sample_rate && einsp_freq <= vdata$sample_rate / 2
        ), message = "Waiting for initialization"),
        shiny::need(is.list(ginfo), message = "Please choose a valid group"),
        shiny::need(
          length(einsp_electrode) == 1 && !is.na(einsp_electrode) &&
            einsp_electrode %in% vdata$electrodes,
          message = "Invalid electrode"),
        shiny::need(length(block) == 1,
                    message = "Invalid block choice")
      )

      srate <- vdata$sample_rate


      # old_theme <- graphics::par(c("fg", "bg", "col.axis", "col.lab", "col.main", "col.sub", "mai"))
      # fg <- theme$foreground
      # bg <- theme$background
      # graphics::par(fg = fg, bg = bg, col.axis = fg, col.lab = fg, col.main = fg,
      #               col.sub = fg, mai = c(0.8, 0.5, 0.42, 0.1))
      # on.exit({ do.call(graphics::par, old_theme) }, add = TRUE)


      signals <- vdata$data[,vdata$electrodes == einsp_electrode, drop = TRUE]

      ref_name <- ginfo$data$Reference[ginfo$data$Electrode == einsp_electrode]
      if(length(ref_name) == 1 && startsWith(ref_name, "ref_")) {

        elecs <- dipsaus::parse_svec(gsub("^ref_", "", ref_name))

        if(length(elecs) == 0) {
          ref_data <- 0
        } else if(length(elecs) == 1 && elecs %in% vdata$electrodes) {
          ref_data <- vdata$data[,vdata$electrodes == elecs, drop = TRUE]

        } else {
          ref_data <- get_reference_data(ref_name)
          if(is.null(ref_data)) {
            ref_data <- 0
          } else {
            ref_data <- ref_data$voltage[[block]]
          }
        }



      } else {
        ref_data <- 0
      }


      ravetools::pwelch(x = signals, fs = srate, window = einsp_winlen, )
      ravetools::diagnose_channel(
        signals - ref_data,
        signals,
        srate = srate,
        max_freq = einsp_freq,
        try_compress = TRUE,
        window = einsp_winlen,
        cex = 2,
        std = 3,
        lwd = 0.3,
        name = c("Referenced", "Raw"),
        col = c("dodgerblue3", "gray60"),
        nclass = 30,
        main = sprintf("Block %s, Electrode %s",
                       block, einsp_electrode)
      )

    }),
    outputId = "reference_plot_electrode"
  )


  ravedash::register_output(
    shiny::renderPlot({

      refinsp_epoch <- input$refinsp_epoch
      refinsp_pre <- input$refinsp_pre
      refinsp_post <- input$refinsp_post
      refinsp_baseline <- input$refinsp_baseline
      refinsp_baseline_method <- input$refinsp_baseline_method
      refinsp_range <- input$refinsp_range
      refinsp_freq_range <- input$refinsp_freq_range

      ginfo <- current_group()
      data_loaded <- ravedash::watch_data_loaded()
      block <- input$plot_block

      repo <- component_container$data$repository
      subject <- repo$subject
      has_wavelet <- all(subject$has_wavelet)

      shiny::validate(
        shiny::need(
          data_loaded && !is.null(subject),
          message = "Subject is not loaded",
        ),
        shiny::need(
          isTRUE(has_wavelet),
          message = "No wavelet has been applied to this subject"
        ),
        shiny::need(
          length(refinsp_epoch) == 1 && refinsp_epoch != "",
          message = "No epoch is found"
        ),
        shiny::need(
          length(refinsp_pre) == 1 && refinsp_pre >= 0,
          message = "Epoch pre-onset must be non-negative"
        ),
        shiny::need(
          length(refinsp_post) == 1 && refinsp_post >= 0,
          message = "Epoch post-onset must be non-negative"
        ),
        shiny::need(
          is.list(ginfo),
          message = "Please select a reference group"
        ),
        shiny::need(
          length(refinsp_baseline) == 2 &&
            length(refinsp_baseline_method) == 1 &&
            length(refinsp_freq_range) == 2 &&
            length(block) > 0,
          message = ifelse(isTRUE(has_wavelet), "", "Waiting for the initialization...")
        )
      )

      ref_type <- ginfo$data$Type
      ref_name <- unique(ginfo$data$Reference)
      ref_name <- ref_name[startsWith(ref_name, "ref_")]

      shiny::validate(
        shiny::need(
          length(ref_type) > 0 &&
            ref_type[[1]] %in% reference_choices[c(2,3)] &&
            length(ref_name) > 0,
          message = "This plot is designed to visualize non-zero common-average or white-matter reference signals."
        )
      )

      ref_data <- get_reference_data(ref_name)
      ref_data <- ref_data$wavelet
      ref_data <- structure(lapply(ref_data, function(x) {
        Mod(x)^2
      }), names = names(ref_data))

      # epoch
      # refinsp_epoch <- input$refinsp_epoch
      # refinsp_pre <- input$refinsp_pre
      # refinsp_post <- input$refinsp_post
      # refinsp_baseline <- input$refinsp_baseline
      # refinsp_baseline_method <- input$refinsp_baseline_method
      # refinsp_range <- input$refinsp_range
      srate <- subject$power_sample_rate
      epoch <- subject$get_epoch(epoch_name = refinsp_epoch, as_table = FALSE)
      tidx <- seq(-refinsp_pre * srate, refinsp_post * srate, by = 1L)
      freq <- subject$preprocess_settings$wavelet_params$frequencies
      freq_sel <- freq >= refinsp_freq_range[[1]] & freq <= refinsp_freq_range[[2]]
      if(!any(freq_sel)) {
        freq_sel <- which.min(abs(freq - refinsp_freq_range[[1]]))
      }

      power <- lapply(seq_len(epoch$n_trials), function(ii) {
        trial <- epoch$trial_at(ii, df = FALSE)
        idx <- round(trial$Time * srate + tidx)
        block_data <- ref_data[[trial$Block]]
        if(!is.matrix(block_data)) { return(NULL) }
        dm <- dim(block_data)

        if(any(idx <= 1 | idx > dm[[1]])){
          return(NULL)
        }
        re <- block_data[idx,freq_sel,drop=FALSE]
        if(!length(re)) { return(NULL) }
        re
      })
      missing_trial <- vapply(power, is.null, FUN.VALUE = FALSE)
      power <- power[!missing_trial]
      if(!length(power)) {
        error_notification(list(message = "No trial satisfies the condition. Please make sure the epoch is valid, and epoch time range (-pre, post) is reasonable."))
        return()
      }
      if(sum(missing_trial) > 0) {
        shidashi::clear_notifications(class = "error_notif")
        shidashi::show_notification(
          message = sprintf("Dropped %d trials due to missing data. (Missing blocks, or invalid epoch time range)", sum(missing_trial)),
          title = "Trials dropped",
          type = "warning",
          close = TRUE,
          autohide = TRUE,
          class = ns("error_notif"),
          collapse = "\n"
        )
      }

      power <- simplify2array(power, higher = TRUE)
      baseline_method <- structure(list(
        "percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"
      ), names = c("Power % change", "sqrt(power) % change", "decibel",
                   "Power z-score", "sqrt(power) z-score")) [[refinsp_baseline_method]]

      idxpts <- tidx / srate
      idxpts <- which(idxpts >= refinsp_baseline[[1]] & idxpts <= refinsp_baseline[[2]])
      bl <- ravetools::baseline_array(power, along_dim = 1L,
                                      method = baseline_method,
                                      baseline_indexpoints = idxpts)

      bl <- ravetools::collapse(bl, keep = c(1, 3), average = TRUE)
      pal <- colorRampPalette(c("navy", "white", "red"))(101)

      if(length(refinsp_range) == 1 && isTRUE(refinsp_range > 0)) {
        if(refinsp_range <= 1) {
          zlim <- quantile(abs(bl), probs = refinsp_range)
        } else {
          zlim <- refinsp_range
        }
      } else {
        zlim <- max(abs(range(bl)))
      }
      bl[bl > zlim] <- zlim
      bl[bl < -zlim] <- -zlim

      layout(matrix(c(1,2), nrow = 1), widths = c(1, lcm(2.5)))

      par(mai = c(0.82, 0.82, 0.82, 0.1))
      image(z = bl, x = tidx / srate, y = seq_len(ncol(bl)), las = 1,
            xlab = "Time (s)", ylab = "Trial", zlim = zlim * c(-1, 1),
            col = pal, axes = FALSE, main = sprintf("Baselined power heatmap (%s)", ref_name))
      axis(side = 1, at = pretty(tidx / srate))
      axis(side = 2, at = pretty(seq_len(ncol(bl))), las = 1)
      abline(v = 0, lty = 3, col = "gray40", lwd = 2)

      par(mai = c(0.82, 0, 0.82, 0.5))
      image(z = matrix(seq(-zlim, zlim, length.out = length(pal)),
                       nrow = 1),
            col = pal, axes = FALSE)

      zlim_str <- sprintf("%.1f", zlim)
      zlim_str <- gsub("\\.0$", "", zlim_str)

      axis(side = 4, at = c(0, 0.5, 1),
           labels = c(zlim_str, 0, sprintf("-%s", zlim_str)))

    }),
    outputId = "reference_plot_heatmap"
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      refinsp_pre <- input$refinsp_pre
      refinsp_post <- input$refinsp_post
      if(!length(refinsp_pre) || !length(refinsp_post) ||
         is.na(refinsp_pre) || is.na(refinsp_post)) {return()}
      if(refinsp_pre < 0 || refinsp_post < 0) {
        error_notification(list(message = "Epoch pre & post must be non-negative"))
        return()
      }
      refinsp_baseline <- input$refinsp_baseline

      refinsp_baseline[refinsp_baseline < -refinsp_pre] <- -refinsp_pre
      refinsp_baseline[refinsp_baseline > refinsp_post] <- refinsp_post

      shiny::updateSliderInput(
        session = session,
        inputId = "refinsp_baseline",
        min = -refinsp_pre,
        max = refinsp_post,
        value = refinsp_baseline
      )
    }, priority = 1L),
    input$refinsp_pre,
    input$refinsp_post,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # # Run analysis once the following input IDs are changed
  # # This is used by auto-recalculation feature
  # server_tools$run_analysis_onchange(
  #   component_container$get_input_ids(c(
  #     "electrode_text", "baseline_choices",
  #     "analysis_ranges", "condition_groups"
  #   ))
  # )

  # Register event: main pipeline need to run
  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #
  #     # Invalidate previous results (stop them because they are no longer needed)
  #     if(!is.null(local_data$results)) {
  #       local_data$results$invalidate()
  #       ravedash::logger("Invalidating previous run", level = "trace")
  #     }
  #
  #
  #     # Collect input data
  #     settings <- component_container$collect_settings(ids = c(
  #       "electrode_text", "baseline_choices", "condition_groups", "analysis_ranges"
  #     ))
  #
  #     pipeline_set(.list = settings)
  #
  #     results <- ravepipeline::pipeline_run(
  #       pipe_dir = pipeline_path,
  #       scheduler = "none",
  #       type = "smart",
  #       callr_function = NULL,
  #       progress_title = "Calculating in progress",
  #       async = TRUE,
  #       check_interval = 0.1,
  #       shortcut = TRUE,
  #       names = c(
  #         "settings",
  #         names(settings),
  #         "requested_electrodes", "analysis_ranges_index", "cond_groups",
  #         "bl_power", "collapsed_data"
  #       )
  #     )
  #
  #
  #     local_data$results <- results
  #     ravedash::logger("Scheduled: ", pipeline_name, level = 'debug', reset_timer = TRUE)
  #
  #     results$promise$then(
  #       onFulfilled = function(...){
  #         ravedash::logger("Fulfilled: ", pipeline_name, level = 'debug')
  #         shidashi::clear_notifications(class = "pipeline-error")
  #         local_reactives$update_outputs <- Sys.time()
  #         return(TRUE)
  #       },
  #       onRejected = function(e, ...){
  #         msg <- paste(e$message, collapse = "\n")
  #         if(inherits(e, "error")){
  #           ravedash::logger(msg, level = 'error')
  #           ravedash::logger(traceback(e), level = 'error', .sep = "\n")
  #           shidashi::show_notification(
  #             message = msg,
  #             title = "Error while running pipeline", type = "danger",
  #             autohide = FALSE, close = TRUE, class = "pipeline-error"
  #           )
  #         }
  #         return(msg)
  #       }
  #     )
  #
  #     return()
  #
  #   }),
  #   server_tools$run_analysis_flag(),
  #   ignoreNULL = TRUE, ignoreInit = TRUE
  # )


  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      new_subject <- pipeline$read("subject")
      new_subject <- raveio::as_rave_subject(new_subject$subject_id)
      new_repository <- raveio::prepare_subject_bare0(subject = new_subject, electrodes = new_subject$electrodes, reference_name = "_unsaved")

      if(!inherits(new_repository, "rave_prepare_subject_bare0")){
        ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_subject_bare0`. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_subject_bare0")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # Reset custom UI
      ref_tbl <- new_subject$get_reference('_unsaved')
      groups <- unique(ref_tbl$Group)
      electrode_group <- dipsaus::drop_nulls(lapply(groups, function(gname){
        if(!nzchar(trimws(gname))) { return() }
        channels <- ref_tbl$Electrode[ref_tbl$Group == gname]
        if(!length(channels)) { return() }
        list(
          electrodes = dipsaus::deparse_svec(channels),
          name = gname
        )
      }))

      ravedash::logger("The initial reference table is generated, with {length(electrode_group)} groups.", level = "trace", use_glue = TRUE)
      dipsaus::updateCompoundInput2(
        session = session, inputId = "electrode_group",
        value = electrode_group, ncomp = length(electrode_group)
      )

      shiny::updateSelectInput(
        session = session, inputId = "plot_block",
        choices = new_subject$blocks, selected = new_subject$blocks[[1]]
      )



      # Reset preset UI & data
      component_container$reset_data()

      component_container$data$repository <- new_repository
      component_container$initialize_with_new_data()
      local_reactives$reference_table <- ref_tbl
      local_reactives$group_confirmed <- FALSE

      local_data$bipolar_table <- NULL
      local_data$voltage_data <- list()
      local_data$reference_data <- list()

      local_reactives$refresh <- Sys.time()

      # Reset outputs
      # shidashi::reset_output("collapse_over_trial")

      shidashi::card_operate(title = "Electrode groups", method = "expand")
      shidashi::card_operate(title = "Reference settings", method = "collapse")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      ravedash::logger("Applying changes to electrode groups", level = "trace")

      electrode_group <- input$electrode_group
      pipeline$set_settings(
        reference_name = "_unsaved",
        electrode_group = electrode_group,
        changes = list()
      )

      res <- pipeline$run(
        as_promise = TRUE,
        scheduler = "none", type = "vanilla",
        names = c(
          'electrode_group', 'reference_group'),
      )

      res$promise$then(
        onFulfilled = function(...){
          tbl_new <- pipeline$read("reference_group")
          tbl_new <- tbl_new[, c("Electrode", "Group")]
          tbl_old <- shiny::isolate(local_reactives$reference_table)
          # update
          nms <- names(tbl_old)
          tbl_old <- tbl_old[, nms[!nms %in% c("Group")]]
          tbl_new <- merge(tbl_old, tbl_new, by = "Electrode", all.y = TRUE)
          local_reactives$reference_table <- tbl_new
          local_reactives$group_confirmed <- TRUE
          shidashi::card_operate(title = "Electrode groups", method = "collapse")
          shidashi::card_operate(title = "Reference settings", method = "expand")

          shidashi::clear_notifications(class = ns("error_notif"))
        },
        onRejected = function(e){
          ravedash::logger_error_condition(e)
          error_notification(e)
        }
      )


    }),
    input$electrode_group_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  group_info <- shiny::bindEvent(
    shiny::reactive({
      ref_tbl <- local_reactives$reference_table
      if(!is.data.frame(ref_tbl)){ return(list()) }

      ravedash::logger("Gathering reference group information...",
                       level = "trace")
      group_names <- unique(ref_tbl$Group)

      if(length(group_names)) {
        selected <- input$group_name %OF% group_names
      } else {
        selected <- character(0L)
      }

      shiny::updateSelectInput(
        session = session, inputId = "group_name",
        choices = group_names,
        selected = selected
      )

      structure(lapply(group_names, function(gname){
        sub <- ref_tbl[ref_tbl$Group == gname, ]
        list(
          name = gname,
          data = sub,
          electrode_text = dipsaus::deparse_svec(sub$Electrode)
        )
      }), names = group_names)
    }),
    local_reactives$reference_table,
    ignoreNULL = FALSE, ignoreInit = FALSE
  )



  shiny::bindEvent(
    ravedash::safe_observe({

      channels <- input$reference_channels_new
      channels <- gsub("^ref_", "", channels)
      channels <- dipsaus::parse_svec(channels)

      ravedash::logger("Generating reference channels from ", dipsaus::deparse_svec(channels), level = "trace")

      repo <- component_container$data$repository
      subject_id <- repo$subject$subject_id

      shidashi::clear_notifications()
      dipsaus::shiny_alert2(title = "Generating reference", text = ravedash::be_patient_text(), auto_close = FALSE, buttons = FALSE)
      on.exit({
        dipsaus::close_alert2()
        shiny::removeModal()
      }, add = TRUE, after = TRUE)

      progress <- dipsaus::progress2(max = 2, shiny_auto_close = TRUE, title = "Overall progress")
      progress$inc("")

      raveio::with_future_parallel({
        raveio::generate_reference(subject = subject_id, electrodes = channels)
      })


      shidashi::show_notification("Done!", title = "Success!", type = "success")

      ref_choices <- c(
        get_reference_options(),
        "[new reference]"
      )
      selected <- sprintf("ref_%s", dipsaus::deparse_svec(channels)) %OF% ref_choices

      ravedash::logger("Updating `Reference to` ", selected, level = "trace")
      shiny::updateSelectInput(
        session = session,
        inputId = "reference_channels",
        choices = ref_choices,
        selected = selected
      )


    }),
    input$reference_channels_btn2,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      channels <- input$reference_channels_new
      channels <- gsub("^ref_", "", channels)
      channels <- dipsaus::parse_svec(channels)

      repo <- component_container$data$repository
      subject <- repo$subject

      misschan <- channels[!channels %in% subject$electrodes]
      if(length(misschan)) {
        error_notification(list(message = sprintf(
          "Cannot generate reference. The following channels are invalid: %s",
          dipsaus::deparse_svec(misschan)
        )))
        return()
      }

      channels <- channels[channels %in% subject$electrodes]
      if(!length(channels)) {
        error_notification(list(message = "Cannot generate reference: none of the channels is valid"))
        return()
      }
      channels <- dipsaus::deparse_svec(channels)

      shiny::showModal(shiny::modalDialog(
        title = "Confirmation",
        shiny::p("Are you sure to generate a referencing signal as an average of: "),
        shiny::tags$ul(
          shiny::tags$li(channels)
        ),

        shiny::tags$small(
          "* RAVE does not limit the channels included in the reference signals as long as their sample rates are the same. This means even if you accidentally mix LFP with spike signals, this module will do it as requested. Therefore, please make sure the channels above are correct."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          dipsaus::actionButtonStyled(
            inputId = ns("reference_channels_btn2"),
            label = 'Confirm'
          )
        )
      ))

    }),
    input$reference_channels_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$reference_details <- shiny::renderUI({
    local_reactives$refresh

    if(!isTRUE(local_reactives$group_confirmed)) {
      shidashi::card_operate(title = "Electrode groups", method = "expand")
      return(shiny::p("Please confirm the electrode groups by pressing the button ",
                      shiny::strong("Set groups"), " first."))
    }


    reference_type <- input$reference_type
    group_name <- input$group_name
    group_info <- group_info()

    if(isTRUE(reference_type %in% reference_choices[c(2,3)])) {

      ref_choices <- c(
        get_reference_options(),
        "[new reference]"
      )
      sub_data <- group_info[[group_name]]$data
      ref_selection <- unique(sub_data$Reference)
      ref_selection <- ref_selection[!ref_selection %in% c("", "noref")] %OF% ref_choices

      return(shiny::tagList(
        shiny::selectInput(
          inputId = ns("reference_channels"),
          label = "Reference to",
          choices = ref_choices,
          selected = ref_selection
        ),
        shiny::uiOutput(ns("ref_generator")),
        dipsaus::actionButtonStyled(
          inputId = ns("reference_btn"),
          label = "Confirm changes & visualize",
          width = "100%"
        )
      ))

    } else if(isTRUE(reference_type %in% reference_choices[4])) {
      # Bipolar
      return(dipsaus::actionButtonStyled(
        inputId = ns("bipolar_btn"),
        label = "Open Bipolar reference editor",
        width = "100%"
      ))
    } else {
      return(dipsaus::actionButtonStyled(
        inputId = ns("reference_btn"),
        label = "Confirm changes & visualize",
        width = "100%"
      ))
    }

  })


  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::showModal(shiny::modalDialog(
        title = "Bipolar reference editor",
        size = "l", easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          dipsaus::actionButtonStyled(
            inputId = ns("reference_btn"),
            label = "Confirm changes & visualize"
          )
        ),
        DT::dataTableOutput(ns("bipolar_table")),
        shiny::p(
          shiny::tags$small("* Double-click on the `Reference` column to edit the reference.")
        )
      ))
    }),
    input$bipolar_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_bipolar_table <- shiny::reactive({
    local_reactives$refresh_bipolar_table
    ginfo <- current_group()
    if(!is.list(ginfo) || !is.data.frame(ginfo$data)) {
      return("Initializing...")
    }

    table <- ginfo$data[, c("Electrode", "Group", "Reference", "Type")]
    if(!nrow(table)) {
      return("Current group has no electrode")
    }
    re <- local_data$bipolar_table

    if(is.data.frame(re) && setequal(re$Electrode, table$Electrode)) {
      return(re)
    }
    if(!all(table$Type == reference_choices[4])) {
      table$Type <- reference_choices[4]
      table <- table[order(table$Electrode),]
      table$Reference <- c(sprintf("ref_%s", table$Electrode[-1]), "")
    }
    local_data$bipolar_table <- table

    return(table)
  })

  output$bipolar_table <- DT::renderDataTable({
    table <- get_bipolar_table()

    shiny::validate(shiny::need(
      is.data.frame(table),
      message = ifelse(length(table) == 1, table, "Initializing")
    ))


    re <- DT::datatable(
      table,
      selection = "none",
      rownames = FALSE,
      class = "compact",
      filter = "none",
      editable = list(target = "cell",
                      disable = list(columns = c(0,1,3))),
      extensions = "KeyTable",
      options = list(
        ordering = FALSE,
        bFilter = 0,
        paging = FALSE,
        keys = TRUE,
        initComplete = DT::JS(
          "function(setting, json) {

            const KEY_CODES = ['Tab','ArrowDown','ArrowRight','ArrowLeft','ArrowUp'];
            const KEY_ENTER = 'Enter';

            this.on('key', function(e, datatable, key, cell, originalEvent){
              const targetName = originalEvent.target.localName;
              const keycode = originalEvent.code;

              if( keycode == KEY_ENTER && (
                targetName == 'div' ||
                targetName == 'body'
              )) {
                $(cell.node()).trigger('dblclick.dt');
              }
            });

            this.on('keydown', function(e){

              if(e.target.localName == 'input' && (
                KEY_CODES.indexOf(e.code) > -1 ||
                e.code === KEY_ENTER
              )){

                $(e.target).trigger('blur');
              }
            });

            this.on('key-focus', function(e, datatable, cell, originalEvent){
              const targetName = originalEvent.target.localName;
              const type = originalEvent.type;
              const keycode = originalEvent.code;

              if(type == 'keydown' && targetName == 'input' &&
                KEY_CODES.includes(keycode) ){

                $(cell.node()).trigger('dblclick.dt');
              }
            });
          }"

        )
      )
    )
    # re <- DT::formatRound(re, columns=c('Time', 'Diff'), digits = 2)
    re
  }, server = FALSE)

  proxy = DT::dataTableProxy(
    outputId = ns('bipolar_table'),
    deferUntilFlush = TRUE,
    session = session$rootScope()
  )
  shiny::bindEvent(
    ravedash::safe_observe({
      info = input$bipolar_table_cell_edit

      val0 <- trimws(info$value)
      val <- gsub("[^0-9,-]", "", val0)
      elecs <- dipsaus::parse_svec(val)

      if( length(elecs) ) {
        val <- dipsaus::deparse_svec(elecs)
        val <- sprintf("ref_%s", val)
        if(length(elecs) > 1) {
          if(!isTRUE(val %in% get_reference_options())) {
            error_notification(list(
              message = sprintf(
                "Cannot find reference signal called `%s`. Set to no-reference as a fallback. Please correct.",
                val
              )
            ))
            val <- "noref"
          }
        } else {
          repo <- component_container$data$repository
          if(!length(repo)) { return() }
          subject <- repo$subject
          if(!length(subject)) { return() }
          if(!elecs %in% subject$electrodes){
            error_notification(list(
              message = sprintf(
                "Channel %s is not valid. Set to no-reference as a fallback. Please fix.",
                elecs
              )
            ))
            val <- "noref"
          }
        }

        info$value <- val
      } else if(startsWith(tolower(val0), "n")) {
        info$value <- "noref"
      } else {
        info$value <- ""
      }
      info$col <- 3L
      local_data$bipolar_table <- DT::editData(local_data$bipolar_table, info)

      # DT::replaceData(proxy,
      #                 local_data$bipolar_table,
      #                 resetPaging = FALSE,
      #                 rownames = FALSE)

      local_reactives$refresh_bipolar_table <- Sys.time()

    }),
    input$bipolar_table_cell_edit,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$ref_generator <- shiny::renderUI({
    if(!isTRUE(input$reference_channels == "[new reference]")) {
      return()
    }
    shidashi::flex_container(
      style = "margin: -5px;",
      shidashi::flex_item(
        size = 2,
        shiny::textInput(
          inputId = ns("reference_channels_new"),
          label = NULL,
          placeholder = "Enter reference channels"
        )
      ),
      shidashi::flex_item(
        size = 1,
        shiny::actionButton(
          inputId = ns("reference_channels_btn"),
          label = "Generate", width = "100%"
        )
      )
    )
  })

  # set when group name changed
  current_group <- shiny::bindEvent(
    shiny::reactive({

      data_loaded <- ravedash::watch_data_loaded()
      data_opened <- ravedash::watch_loader_opened()
      if(!data_loaded || data_opened){ return() }

      repo <- component_container$data$repository
      subject <- repo$subject

      if(is.null(subject)) { return() }

      ginfo <- group_info()
      group_name <- input$group_name
      if(!group_name %in% names(ginfo)){ return() }

      # list(
      #   name = gname,
      #   data = sub,
      #   electrode_text = dipsaus::deparse_svec(sub$Electrode)
      # )
      ginfo <- ginfo[[group_name]]

      ginfo$subject <- subject

      sample_rate <- subject$raw_sample_rates[subject$electrodes %in% ginfo$data$Electrode]
      if(!length(sample_rate)) { return() }

      ginfo$sample_rate <- sample_rate[[1]]

      ravedash::logger("Current reference group is set to [{ginfo$name}]",
                       level = "trace", use_glue = TRUE)
      ravedash::logger("Reference group [{ginfo$name}] contains electrodes [{ginfo$electrode_text}] with reference type [{ginfo$data$Type[[1]]}]", level = "trace", use_glue = TRUE)
      return(ginfo)

    }),
    input$group_name,
    group_info(),
    ignoreNULL = FALSE, ignoreInit = TRUE
  )

  output$reference_table_preview <- shiny::renderTable({
    loaded_flag <- ravedash::watch_data_loaded()
    tbl <- local_reactives$reference_table
    shiny::validate(
      shiny::need(
        loaded_flag && is.data.frame(tbl),
        message = "Reference table not loaded..."
      )
    )
    tbl[, c("Electrode", "Group", "Reference", "Type")]
  }, striped = TRUE, bordered = TRUE, spacing = "xs", width = "100%",
  rownames = TRUE, colnames = TRUE)

  # shiny::outputOptions(output, "group_3dviewer", suspendWhenHidden = FALSE)
  output$group_3dviewer <- threeBrain::renderBrain({
    ginfo <- current_group()

    theme <- ravedash::current_shiny_theme()

    shiny::validate(
      shiny::need(
        is.list(ginfo),
        message = "No refernce group selected"
      )
    )
    subject <- ginfo$subject
    brain <- raveio::rave_brain(subject = subject)

    shiny::validate(shiny::need(!is.null(brain), message = "No 3D brain available"))

    value <- rep("Others", length(subject$electrodes))

    electrodes <- ginfo$data$Electrode
    invalid_electrodes <- electrodes[ginfo$data$Reference == ""]

    value[subject$electrodes %in% electrodes] <- "Valid"
    value[subject$electrodes %in% invalid_electrodes] <- "Invalid"

    value <- factor(value, levels = c("Valid", "Invalid", "Others"))

    tbl <- data.frame(
      Subject = subject$subject_code,
      Electrode = subject$electrodes,
      Value = value,
      stringsAsFactors = FALSE
    )
    brain$set_electrode_values(tbl)


    brain$plot(
      volumes = FALSE,
      atlases = FALSE,
      background = theme$background,
      side_canvas = FALSE,
      side_display = FALSE,
      palettes = list("Value" = c("navy", "red", "gray80")),
      control_panel = FALSE,
      control_display = FALSE,
      controllers = list(
        "Show Time" = FALSE
      ),
      cex = 0.5,
      start_zoom = 1.5
    )


  })

  shiny::bindEvent(
    ravedash::safe_observe({
      ginfo <- current_group()

      if(is.null(ginfo)){ return() }

      # get current type
      ref_type <- ginfo$data$Type[[1]] %OF% reference_choices
      ravedash::logger("Triggered UI update: `Reference type` [{ref_type}]", level = "trace", use_glue = TRUE)
      shiny::updateSelectInput(
        session = session,
        inputId = "reference_type",
        selected = ref_type
      )

      # update anyway
      ref_chan <- unique(ginfo$data$Reference)
      ref_chan <- ref_chan[startsWith(ref_chan, "ref_")]
      if(length(ref_chan)) {
        ref_chan <- ref_chan[[1]]
        ravedash::logger("Triggered UI update: `Reference to` [{ref_chan}]", level = "trace", use_glue = TRUE)
        shiny::updateSelectInput(
          session = session,
          inputId = "reference_channels",
          selected = ref_chan
        )
      }

    }),
    current_group(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      ginfo <- current_group()
      # list(
      #   name = gname,
      #   data = sub,
      #   electrode_text = dipsaus::deparse_svec(sub$Electrode)
      # )

      ravedash::logger("Applying changes to reference group [{ginfo$name}]",
                       level = "trace", use_glue = TRUE)

      ref_type <- input$reference_type
      if(length(ref_type)) {

        ref_signals <- input$reference_channels
        if(ref_type == reference_choices[[1]]) {
          ref_signals <- "noref"
        } else if(ref_type %in% reference_choices[4]) {
          bipolar_table <- get_bipolar_table()
          ref_signals <- bipolar_table$Reference
        }

        current_change <- list(
          group_name = ginfo$name,
          electrodes = ginfo$electrode_text,
          reference_type = ref_type,
          reference_signal = ref_signals
        )

        changes <- as.list(pipeline$get_settings("changes"))

        # Make changes
        dup <- vapply(changes, function(item){
          ginfo$name %in% item$group_name
        }, FUN.VALUE = FALSE)
        changes <- changes[!dup]
        changes[[length(changes) + 1]] <- current_change


        pipeline$set_settings(changes = changes)
      }

      res <- pipeline$run(
        as_promise = TRUE,
        names = "reference_updated",
        scheduler = "none",
        type = "vanilla")

      res$promise$then(
        onFulfilled = function(...){

          updated_reftable <- pipeline$read("reference_updated")

          local_reactives$reference_table <- updated_reftable

          shiny::removeModal()

          info_notification("Reference table updated. Updating the visualizations...")

        }, onRejected = function(e){

          error_notification(e)

        }
      )

    }),
    input$reference_btn,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::onSessionEnded(function(){
    local_data$`@reset`()
    gc()
  })

}
