

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

            # electrode_selector$ui_func(),
            ravedash::input_card(
              title = "Configure Block & Electrodes",
              class_body = "padding-5",

              shidashi::flex_container(
                align_content = "flex-end",

                shidashi::flex_item(
                  size = 2,
                  shiny::selectInput(
                    inputId = ns("analysis_block"),
                    label = "Session block",
                    choices = character()
                  )
                ),

                shidashi::flex_item(
                  .class = "fill-width padding-left-5 padding-bottom-5",
                  shiny::div(
                    class = "fill-width text-right mb-4",
                    shiny::actionLink(
                      inputId = ns("analysis_block__prev"),
                      label = shiny::tagList(
                        ravedash::shiny_icons$angle_double_left,
                        "Previous"
                      )
                    )
                  )
                ),
                shidashi::flex_item(
                  .class = "fill-width padding-right-5 padding-bottom-5",
                  shiny::div(
                    class = "fill-width text-left mb-4 padding-left-10",
                    shiny::actionLink(
                      inputId = ns("analysis_block__next"),
                      label = shiny::tagList(
                        "Next",
                        ravedash::shiny_icons$angle_double_right
                      )
                    )
                  )
                ),

                shidashi::flex_break(),

                shidashi::flex_item(
                  shiny::textInput(
                    inputId = ns("electrode_text"),
                    label = "Electrodes to visualize",
                    value = "",
                    placeholder = "Leave empty to calculate all"
                  )
                )

              )
            ),



            ravedash::input_card(
              title = "Signal Filters",
              class_body = "padding-5",
              class_foot = "padding-5",
              footer = shiny::uiOutput(
                outputId = ns("filter_summary")
              ),

              ravedash::group_box(
                title = "Band-passing",
                class = "padding-5",

                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::div(
                      class = "text-right",
                      shiny::checkboxInput(
                        inputId = ns("filter_bandpass__enabled"),
                        label = "Enabled"
                      )
                    )
                  )
                ),

                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] === true", ns("filter_bandpass__enabled")),

                  shiny::fluidRow(
                    shiny::column(
                      width = 6L,
                      shiny::numericInput(
                        inputId = ns("filter_bandpass__freq_lb"),
                        label = "Lower-bound (Hz)",
                        min = 0, max = 1000000, step = 0.1,
                        width = "100%", value = 0
                      )
                    ),
                    shiny::column(
                      width = 6L,
                      shiny::numericInput(
                        inputId = ns("filter_bandpass__freq_ub"),
                        label = "Upper-bound (Hz)",
                        min = 0, max = 1000000, step = 0.1,
                        width = "100%", value = 0
                      )
                    )
                  )

                )

              ),

              ravedash::group_box(
                title = "Notch-filter",
                class = "padding-5",

                shiny::fluidRow(
                  shiny::column(
                    width = 12L,

                    shiny::div(
                      class = "text-right",
                      shiny::checkboxInput(
                        inputId = ns("filter_notch__enabled"),
                        label = "Enabled"
                      )
                    )
                  )
                ),

                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] === true", ns("filter_notch__enabled")),

                  shiny::fluidRow(
                    shiny::column(
                      width = 12L,
                      shiny::numericInput(
                        inputId = ns("filter_notch__base_frequency"),
                        label = "Base frequency",
                        min = 0, value = 60, step = 1, width = "100%"
                      )
                    ),
                    shiny::column(
                      width = 6L,
                      shiny::textInput(
                        inputId = ns("filter_notch__harmonics"),
                        label = "x (Times)",
                        value = "1,2,3,4,5"
                      )
                    ),
                    shiny::column(
                      width = 6L,
                      shiny::textInput(
                        inputId = ns("filter_notch__bandwidths"),
                        label = "+- (Hz)",
                        value = "1,2,2,3,3"
                      )
                    )
                  )

                )

              )
            ),


            ravedash::input_card(
              inputId = ns("card_viewer_setting"),
              title = "3D Viewer Configurations",
              class_body = "padding-5",

              shiny::fluidRow(

                shiny::column(
                  width = 12L,
                  shiny::tags$small(
                    "3D viewer does not update automatically. ",
                    "Please select a reasonable time range and manually click on `Generate viewer` button."
                  )
                ),

                shiny::column(
                  width = 12L,
                  shiny::sliderInput(
                    inputId = ns("viewer_timestart"),
                    label = "Time start",
                    min = 0, max = 1, value = 0, step = 0.01,
                    round = -2
                  )
                ),
                shiny::column(
                  width = 12L,
                  shiny::sliderInput(
                    inputId = ns("viewer_timesduration"),
                    label = "Duration",
                    min = 0, max = 10, value = 2, step = 0.01,
                    round = -2
                  )
                ),
                shiny::column(
                  width = 12L,
                  shiny::actionLink(
                    inputId = ns("viewer_sync"),
                    label = "Sync from [Channel plot] selection"
                  ),
                  shiny::hr(),
                  dipsaus::actionButtonStyled(
                    inputId = ns("viewer_generate"),
                    label = "Generate viewer", width = "100%"
                  )
                )

              )
            ),

            ravedash::input_card(
              title = "Welch Periodogram Settings",
              class_body = "padding-5",
              start_collapsed = TRUE,

              shidashi::flex_container(

                shidashi::flex_item(
                  shiny::sliderInput(
                    inputId = ns("pwelch_window_size"),
                    label = "Window size (time-point)",
                    min = 100, max = 4000,
                    value = 4000,
                    width = "100%",
                    round = TRUE
                  )
                ),

                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::sliderInput(
                    inputId = ns("pwelch_window_overlap"),
                    label = "Window sliding overlap (%)",
                    min = 10, max = 100,
                    value = 50,
                    width = "100%", post = "%",
                    round = TRUE
                  )
                ),

                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::sliderInput(
                    inputId = ns("pwelch_frequency_limit"),
                    label = "Frequency limit for figures (Hz)",
                    min = 0, max = 2000, value = c(0, 300),
                    width = "100%", post = "Hz",
                    round = -1
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
              title = "Channel plots",
              class_body = "fill-height min-height-vh70 height-700 resize-vertical padding-5",
              class_foot = "padding-5",

              footer = shiny::div(
                class = "row text-small",
                shiny::column(
                  width = 4L,
                  shidashi::flex_container(

                    shidashi::flex_item(
                      size = 2L,
                      shiny::textInput(
                        inputId = ns("highlight_channels"),
                        label = "Highlight",
                        value = ""
                      )
                    ),
                    shidashi::flex_item(
                      size = 2L,
                      shiny::textInput(
                        inputId = ns("hide_channels"),
                        label = "Hide channels",
                        value = ""
                      )
                    ),
                    shidashi::flex_item(
                      class = "overflow-hidden text-nowrap",
                      shiny::numericInput(
                        inputId = ns("vertical_spacing"),
                        label = "Spacing",
                        value = 0.999
                      )
                    )
                  )
                ),
                shiny::column(
                  width = 8L,
                  shiny::uiOutput(ns("graphic_summary"))
                )
              ),

              shiny::div(
                class = "row fill-height",
                shiny::div(
                  class = "col-sm-6 fill-height",
                  ravedash::group_box(
                    title = "Filtered signals",
                    style = "height: calc(100% - 3rem);",
                    shiny::div(
                      class = "fill",
                      shiny::div(
                        style = "height: 70%;",
                        ravedash::output_gadget_container(
                          shiny::plotOutput(
                            ns("plot_filtered_signals"), width = '100%',
                            height = "100%",
                            brush = shiny::brushOpts(
                              id = ns("plot_filtered_signals__brush"),
                              direction = "x",
                              clip = TRUE,
                              delayType = "debounce",
                              delay = 300,
                              opacity = 0.25,
                              resetOnNew = FALSE
                            )
                          )
                        )
                      ),

                      shiny::div(
                        style = "height: 30%;",
                        ravedash::output_gadget_container(
                          shiny::plotOutput(
                            ns("plot_pwelch"), width = '100%',
                            height = "100%",
                            click = shiny::clickOpts(id = ns("plot_pwelch__click"), clip = TRUE)
                          )
                        )
                      )
                    )
                  )
                ),

                shiny::div(
                  class = "col-sm-6 fill-height",
                  ravedash::group_box(
                    title = "Selected subset",
                    style = "height: calc(100% - 3rem);",
                    shiny::div(
                      class = "fill",
                      shiny::div(
                        style = "height: 70%;",
                        ravedash::output_gadget_container(
                          shiny::plotOutput(
                            ns("plot_filtered_signals_subset"), width = '100%',
                            height = "100%"
                          )
                        )
                      ),

                      shiny::div(
                        style = "height: 30%;",
                        ravedash::output_gadget_container(
                          shiny::plotOutput(
                            ns("plot_pwelch_subset"), width = '100%',
                            height = "100%",
                            click = shiny::clickOpts(id = ns("plot_pwelch_subset__click"))
                          )
                        )
                      )
                    )
                  )
                )

              )
            ),


            ravedash::output_card(
              inputId = ns("card_3dviewer"),
              title = "3D visualization",
              class_body = "min-height-300 height-500 resize-vertical no-padding",

              ravedash::output_gadget_container(
                threeBrain::threejsBrainOutput(
                  outputId = ns("3dviewer"),
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
