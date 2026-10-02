

module_html <- function() {

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Filter settings",

              ravedash::flex_group_box(
                title = "Frequencies and bandwidths",

                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::numericInput(
                      inputId = ns("notch_filter_base_freq"),
                      label = "Base frequency (Hz)",
                      value = 60L,
                      step = 1L,
                      min = 1L
                    ),
                    tooltip = "Base frequency of the notch filter (Hz).",
                    inputId = "notch_filter_base_freq",
                    update = "shiny::updateNumericInput",
                    description = "[Numeric] The base frequency of the notch filter. For example, `60` Hz for power line noise. Pick 50 Hz for power line noise in Europe/Asia."
                  ) # notch_filter_base_freq
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::textInput(
                      inputId = ns("notch_filter_times"),
                      label = "x (Times)",
                      value = "1,2,3"
                    ),
                    tooltip = "Multiples of the base frequency to filter, separated by commas (e.g. 1,2,3).",
                    inputId = "notch_filter_times",
                    update = "shiny::updateTextInput",
                    description = "[Text of integers separated by commas] The multiples of the base frequency to be filtered. For example, `1,2,3` will filter 60 Hz, 120 Hz, and 180 Hz (for a `notch_filter_base_freq` = 60 Hz base frequency)."
                  ) # notch_filter_times
                ),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::textInput(
                      inputId = ns("notch_filter_bandwidth"),
                      label = "+- Bandwidth (Hz)",
                      value = "1,2,2"
                    ),
                    tooltip = "Half bandwidth (Hz) of the filter at each multiple, separated by commas.",
                    inputId = "notch_filter_bandwidth",
                    update = "shiny::updateTextInput",
                    description = paste(
                      "[Text of integers separated by commas] The half bandwidths of the notch filter for each frequency to be filtered.",
                      "For example, `1,2,2` will filter 60 Hz with a bandwidth of +-1 Hz (59-61 Hz) with total 2 Hz bandwidth,",
                      "120 Hz with a bandwidth of 2 Hz (118-122 Hz) with total 4 Hz bandwidth,",
                      "and 180 Hz with a bandwidth of 2 Hz (178-182 Hz) with total 4 Hz bandwidth."
                    )
                  )
                )
              ),
              shiny::uiOutput(ns("notch_filter_preview")),

              ravedash::flex_group_box(
                title = "Channel types",

                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("notch_filter_channel_types"),
                      label = "Additional channel types (LFP macro-channels will always be included)",
                      choices = c("Spike", "Auxiliary"),
                      selected = character(0L),
                      multiple = TRUE
                    ),
                    inputId = "notch_filter_channel_types",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "The channel types to be included in the notch filter. LFP macro-channels will always be included.",
                      "You can select additional channel types to be included in the notch filter.",
                      "However, in the most cases, you should only apply notch filter to LFP macro-channels."
                    )
                  )
                )
              ),
              footer = tagList(
                ravedash::run_analysis_button(
                  label = "Apply Notch filters",
                  width = "100%"
                )
              )
            ),

            ravedash::input_card(
              title = "Inspection",

              ravedash::flex_group_box(
                title = "Channel selector",
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("block"),
                      label = "Block",
                      choices = character(0L)
                    ),
                    tooltip = "Recording block to inspect.",
                    inputId = "block",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = "[Select] The block to be inspected in output `signal_plot`."
                  )
                ),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("electrode"),
                      label = "Electrode",
                      choices = character(0L)
                    ),
                    tooltip = "Electrode to inspect in the diagnostic plots.",
                    inputId = "electrode",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = "[Select] The electrode to be inspected in Welch's plot - output `signal_plot`."
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("previous_electrode"),
                      label = "Previous",
                      width = "100%"
                    ),
                    tooltip = "Inspect the previous electrode.",
                    inputId = "previous_electrode",
                    update = "shiny::updateActionButton",
                    description = "[Action button] Click to select the previous electrode in the list of electrodes for Welch's plot - output `signal_plot`."
                  )
                ),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("next_electrode"),
                      label = "Next",
                      width = "100%"
                    ),
                    tooltip = "Inspect the next electrode.",
                    inputId = "next_electrode",
                    update = "shiny::updateActionButton",
                    description = "[Action button] Click to select the next electrode in the list of electrodes for Welch's plot - output `signal_plot`."
                  )
                )
              ),
              shiny::div(
                class = "rave-optional",
                ravedash::flex_group_box(
                  title = "Welch periodogram parameters",
                  shidashi::flex_item(
                    shidashi::register_input(
                      shiny::sliderInput(
                        inputId = ns("pwelch_winlen"),
                        label = "Window length (seconds)",
                        min = 0,
                        max = 4,
                        value = 2,
                        step = 0.1
                      ),
                      tooltip = "Window length (seconds) of the Welch periodogram.",
                      inputId = "pwelch_winlen",
                      update = "shiny::updateSliderInput",
                      description = "[Numeric] Select the window length for the Welch's plot - output `signal_plot`."
                    )
                  ),
                  shidashi::flex_break(),
                  shidashi::flex_item(
                    shidashi::register_input(
                      shiny::sliderInput(
                        inputId = ns("pwelch_freqlim"),
                        label = "Frequency limit",
                        min = 20,
                        max = 1000,
                        value = 300,
                        step = 1
                      ),
                      tooltip = "Highest frequency shown in the Welch periodogram.",
                      inputId = "pwelch_freqlim",
                      update = "shiny::updateSliderInput",
                      description = "[Numeric] Select the frequency limit for the Welch's plot - output `signal_plot`."
                    )
                  ),
                  shidashi::flex_break(),
                  shidashi::flex_item(
                    shidashi::register_input(
                      shiny::sliderInput(
                        inputId = ns("pwelch_nbins"),
                        label = "Number of histogram bins",
                        min = 20,
                        max = 200,
                        value = 60,
                        step = 5
                      ),
                      tooltip = "Number of bins of the voltage histogram.",
                      inputId = "pwelch_nbins",
                      update = "shiny::updateSliderInput",
                      description = "[Numeric] Select the number of histogram bins for the Welch's plot - output `signal_plot`."
                    )
                  )
                )
              ),
              footer = tagList(
                shiny::downloadLink(
                  outputId = ns("download_as_pdf"),
                  label = "Download as PDF"
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
              "Notch - Inspect signals",
              class_body = "no-padding fill-width screen-height height-700 min-height-450 resize-vertical",
              shiny::div(
                class = "position-relative fill",
                shiny::plotOutput(
                  ns("signal_plot"),
                  width = "100%",
                  height = "100%"
                )
              )
            )
          )
        )
      )
    )
  )
}
