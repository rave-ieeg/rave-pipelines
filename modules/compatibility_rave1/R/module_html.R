

module_html <- function() {

  shiny::fixedPage(
    shiny::fixedRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Quick access",
              shiny::tags$ul(
                shiny::tags$li(
                  shidashi::register_input(
                    shiny::actionLink(
                      inputId = ns("quickaccess_data_integrity"),
                      label = "Data integrity check"
                    ),
                    inputId = "quickaccess_data_integrity",
                    update = "shiny::updateActionLink",
                    description = paste(
                      "[Action link] Expands the 'Data integrity check' card and",
                      "collapses the other two (after loading, all three are",
                      "collapsed). Open it to show the user the validation",
                      "results, or before reading output `validation_check` with",
                      "`shiny_query_ui`: hidden outputs do not update. Script",
                      "`validation_results` reads the results without it."
                    )
                  )
                ),
                shiny::tags$li(
                  shidashi::register_input(
                    shiny::actionLink(
                      inputId = ns("quickaccess_compatibility"),
                      label = "Backward compatibility"
                    ),
                    inputId = "quickaccess_compatibility",
                    update = "shiny::updateActionLink",
                    description = paste(
                      "[Action link] Expands the 'Backward compatibility' card and",
                      "collapses the other two. Open it to show the user the",
                      "'Make this subject RAVE 1.0 compatible' button",
                      "(`compatibility_do`), which only the user may click."
                    )
                  )
                ),
                shiny::tags$li(
                  shidashi::register_input(
                    shiny::actionLink(
                      inputId = ns("quickaccess_export"),
                      label = "Export data"
                    ),
                    inputId = "quickaccess_export",
                    update = "shiny::updateActionLink",
                    description = paste(
                      "[Action link] Expands the 'Export data' card and collapses",
                      "the other two. Open it to show the user the export inputs",
                      "(`export_*`) and the messages under invalid ones."
                    )
                  )
                )
              )
            )

          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,
            ravedash::output_card(
              title = "Data integrity check",
              shiny::p("Data integrity check examines the data files for any possible issues within the subject. Basic checks only validate small files such as preprocess configuration and meta data. Full checks will also  validate large data, looking for broken or obsolete files."),
              shidashi::flip_box(
                front = shiny::uiOutput(ns("validation_check")),
                back = DT::DTOutput(ns("validation_table"), width = "100%")
              ),
              footer = shidashi::flex_container(
                align_content = "flex-end",
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("validation_version"),
                      label = "Data version",
                      choices = c("2", "1"),
                      selected = "2",
                      selectize = FALSE
                    ),
                    inputId = "validation_version",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select: `2` or `1`] Data format that 'Validate subject'",
                      "(script `run_analysis`) checks. `2` (default): RAVE 2.0.",
                      "`1`: also checks the referenced (`ref/`) copies of the",
                      "voltage, power and phase data that the RAVE 1.0 format",
                      "keeps. It only matters in `validation_mode` `normal`."
                    )
                  )
                ),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("validation_mode"),
                      label = "Validation mode",
                      choices = c("normal", "basic"),
                      selected = "normal",
                      selectize = FALSE
                    ),
                    inputId = "validation_mode",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select: `normal` or `basic`] How much 'Validate subject'",
                      "(script `run_analysis`) checks. `basic`: small files only",
                      "(subject folders, preprocess settings, and the meta tables",
                      "of electrodes, references, and epochs). `normal` (default):",
                      "also reads the voltage, power, and phase data, and checks",
                      "every epoch and reference table against the data; it takes",
                      "longer."
                    )
                  )
                ),
                shidashi::flex_item(
                  shiny::div(
                    class = "form-group",
                    ravedash::run_analysis_button(label = "Validate subject", width = "100%")
                  )
                )
              )
            ),

            ravedash::output_card(
              title = "Backward compatibility",
              shiny::p("Convert data format so subjects can be loaded by RAVE 1.0 modules."),
              shidashi::register_input(
                dipsaus::actionButtonStyled(
                  inputId = ns("compatibility_do"),
                  label = "Make this subject RAVE 1.0 compatible"
                ),
                inputId = "compatibility_do",
                update = "dipsaus::updateActionButtonStyled",
                description = paste(
                  "[Action button, read-only] 'Make this subject RAVE 1.0",
                  "compatible' converts the loaded subject so RAVE 1.0 modules",
                  "can read it, rewriting subject files. Only the user may click",
                  "it: agents never run the conversion. When asked, open the card",
                  "with `quickaccess_compatibility` and ask the user to click it."
                ),
                writable = FALSE
              )
            ),

            ravedash::output_card(
              title = "Export data",

              shiny::fluidRow(

                shiny::column(
                  width = 6L,

                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("export_type"),
                      label = "Data type",
                      choices = c("power", "voltage", "raw-voltage")
                    ),
                    inputId = "export_type",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select: `power`, `voltage`, or `raw-voltage`] Data to",
                      "export: `power` (wavelet power; needs the Wavelet module),",
                      "`voltage` (Notch-filtered voltage; needs the Notch filter),",
                      "or `raw-voltage` (voltage without any processing or",
                      "reference). Set it before the other export inputs: it",
                      "resets the choices of `export_reference` (only `noref` for",
                      "`raw-voltage`) and `export_epoch`. Read by script",
                      "`generate_exports`."
                    )
                  ),

                  shidashi::register_input(
                    shiny::textInput(
                      inputId = ns("export_electrode"),
                      label = "Electrode channels",
                      value = "",
                      placeholder = "Leave blank to export all"
                    ),
                    inputId = "export_electrode",
                    update = "shiny::updateTextInput",
                    description = paste(
                      "[Text] Channels to export, e.g. `14-15` or `1-5,8`; blank",
                      "(default) exports all channels. Channels not in the subject",
                      "are dropped. Invalid unless at least one channel has a",
                      "valid reference in `export_reference` (for `raw-voltage`:",
                      "any channel of the subject)."
                    )
                  ),

                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("export_reference"),
                      label = "Reference name",
                      choices = character()
                    ),
                    inputId = "export_reference",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select] Reference applied to the exported data: one of the",
                      "subject's reference names (script `load_data` lists them).",
                      "Fixed to `noref` when `export_type` is `raw-voltage`."
                    )
                  )

                ),

                shiny::column(
                  width = 6L,

                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("export_epoch"),
                      label = "Epoch name",
                      choices = character()
                    ),
                    inputId = "export_epoch",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select] Epoch (table of trial onsets) that cuts the data",
                      "into trials: one of the subject's epoch names (script",
                      "`load_data` lists them)."
                    )
                  ),

                  shiny::fluidRow(
                    shiny::column(
                      width = 6L,
                      shidashi::register_input(
                        shiny::numericInput(
                          inputId = ns("export_pre"),
                          label = "Pre-onset",
                          max = 0, step = 0.1, value = -1
                        ),
                        inputId = "export_pre",
                        update = "shiny::updateNumericInput",
                        description = paste(
                          "[Numeric, seconds] Start of each trial window relative",
                          "to its onset; must be negative. Default `-1`."
                        )
                      )
                    ),

                    shiny::column(
                      width = 6L,
                      shidashi::register_input(
                        shiny::numericInput(
                          inputId = ns("export_post"),
                          label = "Post-onset",
                          min = 0, step = 0.1, value = 2
                        ),
                        inputId = "export_post",
                        update = "shiny::updateNumericInput",
                        description = paste(
                          "[Numeric, seconds] End of each trial window after its",
                          "onset; must be positive. Default `2`."
                        )
                      )
                    )
                  )

                )

              ),


              dipsaus::actionButtonStyled(
                inputId = ns("export_do"),
                label = "Generate exports"
              ),
              shiny::downloadButton(
                outputId = ns("export_download_do"),
                label = "Export & download",
                icon = ravedash::shiny_icons$download
              )
            )
          )
        )
      )

    )
  )
}
