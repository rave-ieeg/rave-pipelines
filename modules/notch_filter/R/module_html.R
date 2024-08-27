

module_html <- function(){

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
                  shiny::numericInput(
                    inputId = ns("notch_filter_base_freq"),
                    label = "Base frequency (Hz)",
                    value = 60L,
                    step = 1L,
                    min = 1L
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::textInput(
                    inputId = ns("notch_filter_times"),
                    label = "x (Times)",
                    value = "1,2,3"
                  )
                ),
                shidashi::flex_item(
                  shiny::textInput(
                    inputId = ns("notch_filter_bandwidth"),
                    label = "+- Bandwidth (Hz)",
                    value = "1,2,2"
                  )
                )
              ),
              shiny::uiOutput(ns("notch_filter_preview")),
              footer = tagList(
                dipsaus::actionButtonStyled(
                  ns("notch_filter_btn"),
                  "Apply Notch filters",
                  width = "100%"
                )
              )
            ),


            ravedash::input_card(
              title = "Inspection",

              ravedash::flex_group_box(
                title = "Channel selector",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("block"),
                    label = "Block",
                    choices = character(0L)
                  )
                ),
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("electrode"),
                    label = "Electrode",
                    choices = character(0L)
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::actionButton(
                    inputId = ns("previous_electrode"),
                    label = "Previous", width = "100%"
                  )
                ),
                shidashi::flex_item(
                  shiny::actionButton(
                    inputId = ns("next_electrode"),
                    label = "Next", width = "100%"
                  )
                )
              ),
              shiny::div(
                class = "rave-optional",
                ravedash::flex_group_box(
                  title = "Welch periodogram parameters",
                  shidashi::flex_item(
                    shiny::sliderInput(
                      inputId = ns("pwelch_winlen"),
                      label = "Window length",
                      min = 100,
                      max = 4000,
                      value = 4000,
                      step = 1
                    )
                  ),
                  shidashi::flex_break(),
                  shidashi::flex_item(
                    shiny::sliderInput(
                      inputId = ns("pwelch_freqlim"),
                      label = "Frequency limit",
                      min = 20,
                      max = 1000,
                      value = 300,
                      step = 1
                    )
                  ),
                  shidashi::flex_break(),
                  shidashi::flex_item(
                    shiny::sliderInput(
                      inputId = ns("pwelch_nbins"),
                      label = "Number of histogram bins",
                      min = 20,
                      max = 200,
                      value = 60,
                      step = 5
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
              'Notch - Inspect signals',
              class_body = "no-padding fill-width screen-height height-700 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                ravedash::output_gadget_container(
                  shiny::plotOutput(ns("signal_plot"),
                                    width = '100%', height = "100%")
                )

              )
            )
          )
        )
      )

    )
  )
}
