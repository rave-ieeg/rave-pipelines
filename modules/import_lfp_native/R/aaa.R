library(dipsaus)

# options("shinyvalidate.verbose" = TRUE)

# Stores global variables
local_data <- fastmap::fastmap()
local_reactives <- shiny::reactiveValues()


# module_ui_loader <- function(){
#   shiny::tagList(
#     # ui_step1(),
#     ui_step2()
#   )
# }
#
# module_ui_main <- function(){
#   module_ui_loader()
# }


# ----- Presets ------
comp_import_setup <- ravedash::presets_import_setup_native()
comp_import_format <- ravedash::presets_import_setup_blocks()
# comp_import_channels <- ravedash::presets_import_setup_channels()

comp_import_channels <- with(asNamespace("ravedash"), {
  function(
    id = "import_channels",
    label = "Channel information",
    import_setup_id = "import_setup",
    import_blocks_id = "import_blocks"
  ){

    comp <- RAVEShinyComponent$new(id = id)
    comp$depends <- c(import_setup_id, import_blocks_id)
    comp$no_save <- c("", "msg", "actions", "actions_alt", "snapshot",
                      "do_import")

    all_formats <- raveio::IMPORT_FORMATS[c(1,2,3,4,7)]

    comp$ui_func <- function(id, value, depends){

      shidashi::card2(
        title = label,
        inputId = comp$get_sub_element_id(with_namespace = TRUE),
        tools = list(
          shidashi::as_badge("STEP 3")
        ),
        class_body = "",
        body_main = shiny::div(
          class = 'padding-10',

          flex_group_box(
            title = "Header inspector",
            shidashi::flex_item(
              shiny::selectInput(
                inputId = comp$get_sub_element_id("electrode_file", TRUE),
                label = "Data file(s)",
                choices = "auto"
              )
            ),
            shidashi::flex_item(size = 2),
            shidashi::flex_break(),
            shidashi::flex_item(
              shiny::tags$small(
                shiny::uiOutput(
                  outputId = comp$get_sub_element_id("snapshot", TRUE)
                )
              )
            )
          ),

          flex_group_box(
            title = "LFP channels",
            shidashi::flex_item(
              shiny::textInput(
                inputId = comp$get_sub_element_id("lfp_channels", TRUE),
                label = "Channel numbers",
                placeholder = "E.g. 1-84, 100"
              )
            ),
            shidashi::flex_item(
              shiny::selectInput(
                inputId = comp$get_sub_element_id("lfp_unit", TRUE),
                label = "Physical unit",
                choices = c("NA", "uV", "mV", "V"),
                selected = "NA",
                multiple = FALSE
              )
            ),
            shidashi::flex_item(
              shiny::numericInput(
                inputId = comp$get_sub_element_id("lfp_sample_rate", TRUE),
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
                inputId = comp$get_sub_element_id("microwire_channels", TRUE),
                label = "Channel numbers",
                placeholder = "E.g. 257-268"
              )
            ),
            shidashi::flex_item(
              shiny::selectInput(
                inputId = comp$get_sub_element_id("microwire_unit", TRUE),
                label = "Physical unit",
                choices = c("NA", "uV", "mV", "V"),
                selected = "NA",
                multiple = FALSE
              )
            ),
            shidashi::flex_item(
              shiny::numericInput(
                inputId = comp$get_sub_element_id("microwire_sample_rate", TRUE),
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
                inputId = comp$get_sub_element_id("auxiliary_channels", TRUE),
                label = "Channel numbers",
                placeholder = "E.g. 259,260"
              )
            ),
            shidashi::flex_item(
              shiny::selectInput(
                inputId = comp$get_sub_element_id("auxiliary_unit", TRUE),
                label = "Physical unit",
                choices = c("NA", "uV", "mV", "V"),
                selected = "NA",
                multiple = FALSE
              )
            ),
            shidashi::flex_item(
              shiny::numericInput(
                inputId = comp$get_sub_element_id("auxiliary_sample_rate", TRUE),
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
                  inputId = comp$get_sub_element_id("compose_upload", TRUE),
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
                inputId = comp$get_sub_element_id("compose_setup", TRUE),
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
                outputId = comp$get_sub_element_id("compose_message", TRUE)
              )
            )
          )
        ),
        body_side = shiny::div(
          class = "bg-gray fill padding-10",
          shiny::textOutput(
            outputId = comp$get_sub_element_id("msg", TRUE)
          )
        ),
        class_foot = "padding-10",
        footer = shiny::div(
          class = "float-right",
          dipsaus::actionButtonStyled(
            comp$get_sub_element_id("actions_compose", with_namespace = TRUE),
            label = "Compose channels only", type = "default"
          ),
          dipsaus::actionButtonStyled(
            comp$get_sub_element_id("actions_alt", with_namespace = TRUE),
            label = "Skip validation & import", type = "default"
          ),
          dipsaus::actionButtonStyled(
            comp$get_sub_element_id("actions", with_namespace = TRUE),
            label = "Validate & import"
          )
        )
      )

    }


    comp$server_func <- function(input, output, session) {

      comp_import_blocks <- comp$get_dependent_component(import_blocks_id)
      block_setups <- comp_import_blocks$get_tool("block_setups")

      local_reactives <- shiny::reactiveValues(
        valid_setup = FALSE,
        validation_message = "Waiting...",
        refresh = NULL,
        info = NULL,
        preproc = NULL,
        snapshot = NULL
      )

      output[[comp$get_sub_element_id("msg", FALSE)]] <- shiny::renderText({
        if(isTRUE(local_reactives$valid_setup)) {
          "Subject folder has been created. Please choose session blocks."
        } else {
          local_reactives$validation_message
        }
      })

      disable_ui <- function(){
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
          disabled = TRUE
        )
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = comp$get_sub_element_id("actions_alt", with_namespace = FALSE),
          disabled = TRUE
        )

        local_reactives$valid_setup <- FALSE
        local_reactives$info <- NULL
        shidashi::card2_open(id)
      }

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
          info <- block_setups()
          is_valid <- TRUE
          if(!is.list(info)){
            local_reactives$validation_message <- "Waiting..."
            is_valid <- FALSE
          }
          if(!isTRUE(info$valid)){
            local_reactives$validation_message <- "Please finish the previous steps."
            is_valid <- FALSE
          }

          if(!is_valid) {
            disable_ui()
            return()
          }

          # enable UI
          project_name <- info$project_name
          subject_code <- info$subject_code
          blocks <- info$current_blocks
          format <- info$current_format

          subject <- raveio::RAVESubject$new(project_name = project_name,
                                             subject_code = subject_code, strict = FALSE)
          preproc <- subject$preprocess_settings

          info$preproc <- preproc
          local_reactives$info <- info

          # get electrode files
          fs <- lapply(file.path(preproc$raw_path, blocks), function(f){
            list.files(f)
          })
          common_names <- table(unlist(fs))
          common_names <- names(common_names)[common_names == length(blocks)]

          all_electrode_files <- c("auto", common_names)

          shiny::updateSelectInput(
            session = session,
            inputId = comp$get_sub_element_id("electrode_file", FALSE),
            choices = all_electrode_files,
            selected = preproc$data$electrode_file %OF% all_electrode_files
          )

          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions", with_namespace = FALSE),
            disabled = FALSE
          )
          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions_alt", with_namespace = FALSE),
            disabled = FALSE
          )
          shidashi::card2_close(id)

          # preproc <- raveio::RAVEPreprocessSettings$new(subject = )

        }),
        block_setups(),
        ignoreInit = FALSE,
        ignoreNULL = TRUE
      )

      output[[comp$get_sub_element_id("snapshot", FALSE)]] <- shiny::renderUI({
        local_reactives$snapshot
      })


      shiny::bindEvent(
        safe_observe({
          info <- local_reactives$info
          if(!is.list(info) || !isTRUE(info$valid)) { return() }
          preproc <- info$preproc
          if(is.null(preproc)){ return() }
          format <- info$current_format
          blocks <- info$current_blocks

          # get potential electrodes
          if(format == 1) {
            fs <- list.files(file.path(preproc$raw_path, blocks[[1]]))
            es <- gsub("(^.*[^0-9]|^)([0-9]+)\\.(mat|h5)", "\\2", fs)
            es <- es[grepl("^[0-9]+$", es)]

            local_reactives$snapshot <- shiny::p(
              "With given data format (single .mat/.h5 file per electrode), ",
              "I found the following potential channels from the first block (",
              blocks[[1]], "): ", dipsaus::deparse_svec(as.integer(es))
            )
          } else if (format == 3) {
            electrode_file <- comp$get_sub_element_input("electrode_file")
            if(!length(electrode_file) || electrode_file == "auto") {
              electrode_file <- list.files(file.path(preproc$raw_path, blocks[[1]]),
                                           pattern = "\\.edf$", ignore.case = TRUE)
            }
            if(!length(electrode_file)) {
              local_reactives$snapshot <- shiny::p(
                "Cannot find any EDF file in the first block (",
                blocks[[1]], ")"
              )
            } else {
              edf_path <- file.path(preproc$raw_path, blocks[[1]], electrode_file)
              if(length(edf_path) > 1){
                edf_path <- edf_path[which.max(file.size(edf_path))]
              }
              tryCatch({
                header <- raveio::read_edf_header(edf_path)

                local_reactives$snapshot <- shiny::p(
                  "With given data format (EDF), I found the following file in ",
                  "the first block (", blocks[[1]], "): ", basename(edf_path),
                  ". Here is the header information: ",
                  shiny::tags$ul(
                    shiny::tags$li(
                      "Total number of channels: ", header$nSignals
                    ),
                    shiny::tags$li(
                      "Recording length: ",
                      sprintf("%.4f seconds", header$recordedPeriod)
                    ),
                    shiny::tags$li(
                      "Potential sample rates: ",
                      paste(unique(header$sampleRate2), " Hz", collapse = ", ")
                    ),
                    shiny::tags$li(
                      "Potential physical units: ",
                      paste(unique(header$unit2), collapse = ",")
                    ),
                    shiny::tags$li(
                      "Channel labels: ",
                      paste(header$sHeaders$label, collapse = ", ")
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


            }
          } else if (format == 5) {
            # Ignore electrode_file
            electrode_file <- list.files(file.path(preproc$raw_path, blocks[[1]]),
                                         pattern = "\\.nev$", ignore.case = TRUE)
            if(!length(electrode_file)) {
              local_reactives$snapshot <- shiny::p(
                "Cannot find any NEV file in the first block (",
                blocks[[1]], ")"
              )
            } else {
              tryCatch({
                brfile <- raveio::BlackrockFile$new(
                  path = file.path(preproc$raw_path, blocks[[1]], electrode_file[[1]]),
                  block = blocks[[1]], nev_data = FALSE
                )
                elec_table <- brfile$electrode_table
                duration <- brfile$recording_duration

                nsinfo <- lapply(split(elec_table, elec_table$NSType), function(x) {
                  ns_type <- x$NSType[[1]]
                  shiny::tags$li(
                    sprintf(
                      "%s: %s [%.0f Hz, %.2f sec]", ns_type,
                      dipsaus::deparse_svec(x$Electrode),
                      x$SampleRate[[1]],
                      duration[[ns_type]]
                    )
                  )
                })

                local_reactives$snapshot <- shiny::p(
                  "With given data format (BlackRock), I found the following NSX files in ",
                  "the first block (", blocks[[1]], "): ",
                  paste(names(brfile$has_nsx)[brfile$has_nsx], collapse = ", "),
                  ". Here is the header information: ",
                  shiny::tags$ul(
                    shiny::tags$li(
                      "Total number of channels: ", nrow(elec_table)
                    ),
                    nsinfo
                  ),
                  "If no physical unit specified, all the signals will use [uV] by default."
                )
              }, error = function(e){
                local_reactives$snapshot <- shiny::p(
                  "Cannot read the BlackRock files ",
                  gsub("\\.nev", ".*", basename(electrode_file), ignore.case = TRUE),
                  " in the first block (",
                  blocks[[1]], "). Please check if the file version is at least 2.3."
                )
                logger_error_condition(e)
              })


            }
          } else {
            local_reactives$snapshot <- NULL
          }

          etypes <- preproc$electrode_types
          lfp_sel <- etypes %in% "LFP"
          spike_sel <- etypes %in% "Spike"
          misc_sel <- etypes %in% c("Unknown", "Audio", "Photodiode")

          lfp_channels <- preproc$electrodes[lfp_sel]
          spike_channels <- preproc$electrodes[spike_sel]
          misc_channels <- preproc$electrodes[misc_sel]

          # get composed channels
          # preproc <- raveio::RAVEPreprocessSettings$new("demo/DemoSubject")

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
                from = dipsaus::deparse_svec(params$from),
                weights = paste(weights, collapse = ","),
                normalize = ifelse(isTRUE(params$normalize_factor == 1), "no", "yes")
              )
            }, error = function(...){ NULL })
          })
          compose_params <- compose_params[!vapply(compose_params, is.null, FALSE)]
          electrodes <- lfp_channels[!lfp_channels %in% unlist(lapply(compose_params, "[[", "number"))]
          electrodes <- dipsaus::deparse_svec(electrodes)

          shiny::updateTextInput(
            session = session,
            inputId = comp$get_sub_element_id("lfp_channels", FALSE),
            value = electrodes
          )

          lfp_srate <- preproc$sample_rates[lfp_sel]
          if(length(lfp_srate)) {
            shiny::updateNumericInput(
              session = session,
              inputId = comp$get_sub_element_id("lfp_sample_rate", FALSE),
              value = lfp_srate[[1]]
            )
          }

          physical_unit <- preproc$data$physical_unit %OF% c("NA", "uV", "mV", "V")
          shiny::updateSelectInput(
            session = session,
            inputId = comp$get_sub_element_id("lfp_unit", FALSE),
            selected = physical_unit
          )

          dipsaus::updateCompoundInput2(
            session = session,
            inputId = comp$get_sub_element_id("compose_setup", FALSE),
            ncomp = length(compose_params),
            value = compose_params
          )

          # Update Microwires
          shiny::updateTextInput(
            session = session,
            inputId = comp$get_sub_element_id("microwire_channels", FALSE),
            value = dipsaus::deparse_svec(spike_channels)
          )

          spike_srate <- preproc$sample_rates[spike_sel]
          if(length(spike_srate)) {
            shiny::updateNumericInput(
              session = session,
              inputId = comp$get_sub_element_id("lfp_sample_rate", FALSE),
              value = spike_srate[[1]]
            )
          }

          # Update Auxiliary
          shiny::updateTextInput(
            session = session,
            inputId = comp$get_sub_element_id("auxiliary_channels", FALSE),
            value = dipsaus::deparse_svec(misc_channels)
          )

          misc_srate <- preproc$sample_rates[misc_sel]
          if(length(misc_srate)) {
            shiny::updateNumericInput(
              session = session,
              inputId = comp$get_sub_element_id("auxiliary_sample_rate", FALSE),
              value = misc_srate[[1]]
            )
          }


        }),
        local_reactives$info,
        comp$get_sub_element_input("electrode_file"),
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      enable_compose_button <- function(enable = TRUE) {
        info <- local_reactives$info
        if(!is.list(info) || !isTRUE(info$valid)) {
          dipsaus::updateActionButtonStyled(
            session = session,
            inputId = comp$get_sub_element_id("actions_compose", with_namespace = FALSE),
            disabled = TRUE
          )
          return()
        }
        dipsaus::updateActionButtonStyled(
          session = session,
          inputId = comp$get_sub_element_id("actions_compose", with_namespace = FALSE),
          disabled = !enable
        )
      }

      check_compose_setup <- function() {
        if(isTRUE(local_reactives$valid_setup)) {
          enable_compose_button(FALSE)
          return()
        }
        lfp_channels <- dipsaus::parse_svec(comp$get_sub_element_input("lfp_channels"))
        microwire_channels <- dipsaus::parse_svec(comp$get_sub_element_input("microwire_channels"))
        auxiliary_channels <- dipsaus::parse_svec(comp$get_sub_element_input("auxiliary_channels"))
        electrodes <- c(lfp_channels, microwire_channels, auxiliary_channels)
        compose_setup <- comp$get_sub_element_input("compose_setup")
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
            elem_id <- comp$get_sub_element_id(sprintf("compose_setup_message_%d", ii), TRUE)
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
          from <- dipsaus::parse_svec(item$from)
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
        re
      }

      safe_observe({

        check_compose_setup()

      })

      shiny::bindEvent(
        safe_observe({
          info <- local_reactives$info
          if(!is.list(info) || !isTRUE(info$valid)) {
            stop("The inputs are invalid. Please check your inputs.")
          }
          preproc <- info$preproc
          compose_setup <- dipsaus::drop_nulls(lapply(check_compose_setup(), function(item) {
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
                                         dipsaus::deparse_svec(composing_channels))
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
                        shiny::tags$code(class = "text-center",
                                         dipsaus::deparse_svec(composed)),
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
                      inputId = comp$get_sub_element_id("action_compose_do", TRUE),
                      label = "Let's go"
                    )
                  } else { NULL }
                })
              )
            )
          )
        }),
        comp$get_sub_element_input("actions_compose"),
        ignoreNULL = TRUE, ignoreInit = TRUE
      )

      shiny::bindEvent(
        safe_observe({
          info <- local_reactives$info
          if(!is.list(info) || !isTRUE(info$valid)) {
            stop("The inputs are invalid. Please check your inputs.")
          }
          preproc <- info$preproc
          compose_setup <- as.list(dipsaus::drop_nulls(lapply(check_compose_setup(), function(item) {
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
          subject <- preproc$subject$subject_id

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

          raveio::lapply_async(compose_setup, function(item) {
            raveio::compose_channel(
              subject = subject,
              number = item$number,
              from = item$from,
              weights = item$weights,
              normalize = item$normalize,
              force = TRUE
            )
          }, callback = function(item) {
            sprintf("Composing channel|%s (%s)", subject, item$number)
          })
          finished <- TRUE
        }),
        comp$get_sub_element_input("action_compose_do"),
        ignoreNULL = TRUE, ignoreInit = TRUE
      )


      check_before_import <- function(skip_validation = TRUE) {
        shidashi::clear_notifications()

        with_error_notification({
          info <- local_reactives$info
          if(!is.list(info) || !isTRUE(info$valid)) {
            stop("The inputs are invalid. Please check your inputs.")
          }
          preproc <- info$preproc
          if(is.null(preproc)){
            stop("The inputs are invalid. Please check your inputs.")
          }
          format <- info$current_format
          blocks <- info$current_blocks
          electrode_file <- comp$get_sub_element_input("electrode_file")

          lfp_channels <- dipsaus::parse_svec(comp$get_sub_element_input("lfp_channels"))
          lfp_sample_rate <- comp$get_sub_element_input("lfp_sample_rate")
          lfp_physical_unit <- comp$get_sub_element_input("lfp_unit")

          microwire_channels <- dipsaus::parse_svec(comp$get_sub_element_input("microwire_channels"))
          microwire_sample_rate <- comp$get_sub_element_input("microwire_sample_rate")
          microwire_physical_unit <- comp$get_sub_element_input("microwire_unit")

          auxiliary_channels <- dipsaus::parse_svec(comp$get_sub_element_input("auxiliary_channels"))
          auxiliary_sample_rate <- comp$get_sub_element_input("auxiliary_sample_rate")
          auxiliary_physical_unit <- comp$get_sub_element_input("auxiliary_unit")

          compose_setup <- comp$get_sub_element_input("compose_setup")

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

          compose_setup <- lapply(check_compose_setup(), function(item) {
            if(!is.list(item)) {
              if(is.character(item)) {
                stop(item)
              }
              return(NULL)
            }
            item
          })

          preproc <- info$preproc

          any_imported <- any(preproc$data_imported)

          pipeline <- comp$container$get_pipeline()
          settings <- comp$container$collect_settings(c(
            import_setup_id,
            import_blocks_id,
            id
          ))
          settings$skip_validation <- skip_validation
          settings$compose_setup <- compose_setup
          pipeline$set_settings(.list = settings)

          dipsaus::shiny_alert2(
            title = "Validating...",
            text = "Validating the input data... (some might take a while)",
            auto_close = FALSE, buttons = FALSE, icon = "info"
          )

          tryCatch({
            result <- pipeline$run(
              names = "validation_result",
              scheduler = "none",
              type = "smart",
              async = FALSE,
              as_promise = FALSE
            )
            Sys.sleep(time = 0.5)
            dipsaus::close_alert2()
            shiny::showModal(
              shiny::modalDialog(
                title = "Ready to import data",
                easyClose = FALSE, size = "l",
                footer = shiny::tagList(
                  shiny::modalButton("Cancel"),
                  dipsaus::actionButtonStyled(
                    inputId = comp$get_sub_element_id("do_import", TRUE),
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
                        shiny::tags$li("Channels: ", dipsaus::deparse_svec(lfp_channels)),
                        shiny::tags$li(sprintf("Sample rate: %.4f Hz", lfp_sample_rate)),
                        shiny::tags$li(sprintf("Physical unit: %s", lfp_physical_unit))
                      )
                    ),
                    shiny::tags$li(
                      "Microwire: ",
                      shiny::tags$ul(
                        shiny::tags$li("Channels: ", dipsaus::deparse_svec(microwire_channels)),
                        shiny::tags$li(sprintf("Sample rate: %.4f Hz", microwire_sample_rate)),
                        shiny::tags$li(sprintf("Physical unit: %s", microwire_physical_unit))
                      )
                    ),
                    shiny::tags$li(
                      "Auxiliary: ",
                      shiny::tags$ul(
                        shiny::tags$li("Channels: ", dipsaus::deparse_svec(auxiliary_channels)),
                        shiny::tags$li(sprintf("Sample rate: %.4f Hz", auxiliary_sample_rate))
                      )
                    ),
                    shiny::tags$li(
                      "Composing channels: ", dipsaus::deparse_svec(unlist(lapply(
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

            e <- logger_error_condition(e)
            dipsaus::shiny_alert2(
              title = "Validation failure",
              icon = "error",
              text = e$message,
              buttons = "Gotcha",
              danger_mode = TRUE,
              auto_close = FALSE
            )
          })

        })

      }

      shiny::bindEvent(
        safe_observe({
          check_before_import(skip_validation = list(
            value = FALSE,
            time = as.character(Sys.time())
          ))
        }),
        comp$get_sub_element_input("actions")
      )

      shiny::bindEvent(
        safe_observe({
          check_before_import(skip_validation = TRUE)
        }),
        comp$get_sub_element_input("actions_alt")
      )


      shiny::bindEvent(
        safe_observe({
          info <- local_reactives$info
          preproc <- info$preproc

          format <- info$current_format
          blocks <- info$current_blocks
          electrode_file <- comp$get_sub_element_input("electrode_file")
          sample_rate <- comp$get_sub_element_input("sample_rate")
          physical_unit <- comp$get_sub_element_input("unit")
          electrodes <- dipsaus::parse_svec(comp$get_sub_element_input("electrodes"))

          pipeline <- comp$container$get_pipeline()

          settings <- raveio::load_yaml(comp$container$settings_path)
          settings <- comp$container$collect_settings(c(
            import_setup_id,
            import_blocks_id,
            id
          ), map = settings)

          settings$skip_validation <- TRUE
          settings$force_import <- TRUE

          # raveio::save_yaml(settings, comp$container$settings_path, sorted = TRUE)
          pipeline$set_settings(.list = settings)


          dipsaus::shiny_alert2(
            title = "Importing in progress",
            text = "It's time to stand up and stretch yourself...",
            icon = "info",
            auto_close = FALSE,
            danger_mode = FALSE,
            buttons = FALSE
          )

          tryCatch({
            result <- pipeline$run(
              scheduler = "none",
              type = "smart",
              async = FALSE,
              as_promise = FALSE
            )
            Sys.sleep(0.5)
            dipsaus::close_alert2()
            shiny::removeModal()
            dipsaus::shiny_alert2(
              title = "Success!",
              text = "Finished importing the data. Please proceed to the next modules.",
              icon = "success",
              danger_mode = FALSE,
              auto_close = TRUE,
              buttons = "Got it!"
            )
          }, error = function(e) {
            Sys.sleep(0.5)
            dipsaus::close_alert2()
            logger_error_condition(e)
            dipsaus::shiny_alert2(
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
        comp$get_sub_element_input("do_import"),
        ignoreNULL = TRUE, ignoreInit = TRUE
      )

      shiny::bindEvent(
        safe_observe({
          shiny::showModal(
            shiny::modalDialog(
              title = "Upload composing plan",
              size = "l",
              easyClose = FALSE,
              footer = shiny::modalButton(label = "Cancel"),
              dipsaus::fancyFileInput(
                inputId = comp$get_sub_element_id("compose_upload_file", TRUE),
                label = NULL, width = "100%", after_content = "Accepts CSV file",
                size = "m", multiple = FALSE, accept = ".csv"
              ),
              shiny::p(
                "Please upload a CSV file of electrode channels and a weight matrix. The column names will be interpreted as new channel number. ",
                shiny::downloadLink(
                  outputId = comp$get_sub_element_id("compose_upload_example", TRUE),
                  label = "Click here to download an example."
                )
              )
            )
          )
        }),
        comp$get_sub_element_input("compose_upload"),
        ignoreNULL = TRUE, ignoreInit = TRUE
      )

      shiny::bindEvent(
        safe_observe({
          finfo <- comp$get_sub_element_input("compose_upload_file")
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
              from = dipsaus::deparse_svec(sub$Source),
              weights = w,
              normalize = "no"
            )
          })

          dipsaus::updateCompoundInput2(
            session = session,
            inputId = comp$get_sub_element_id("compose_setup", FALSE),
            value = value,
            ncomp = length(value)
          )
          shiny::removeModal()
        }, error_wrapper = "notification"),
        comp$get_sub_element_input("compose_upload_file"),
        ignoreNULL = TRUE, ignoreInit = TRUE
      )

      output[[comp$get_sub_element_id("compose_upload_example", FALSE)]] <-
        shiny::downloadHandler(
          filename = function(...) {
            info <- local_reactives$info
            preproc <- info$preproc
            sprintf(
              "RAVE_compose_plan-%s_%s.csv",
              preproc$subject$project_name,
              preproc$subject$subject_code
            )
          },
          content = function(con) {
            info <- local_reactives$info
            preproc <- info$preproc
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


    }



    # ------------------------------- Validators --------------------------------



    return(comp)
  }

})()

# comp <- comp_import_channels
component_container <- ravedash:::RAVEShinyComponentContainer$new(
  module_id = module_id, pipeline_name = pipeline_name,
  pipeline_path = pipeline$pipeline_path)
component_container$add_components(
  comp_import_setup,
  comp_import_format,
  comp_import_channels
)
