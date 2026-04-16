

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
              title = "Data Selector",

              shiny::fluidRow(
                shiny::column(
                  width = 12L,

                  shiny::selectInput(
                    inputId = ns("recording_block"),
                    label = "Recording block",
                    choices = "",
                    selected = character()
                  ),

                  dipsaus::actionButtonStyled(
                    inputId = ns("load_block_btn"),
                    label = "Visualize block data",
                    width = "100%"
                  )

                ) # col-12
              ) # row
            ), # Data Selector

            ravedash::input_card(
              title = "Pulse Finders",

              shiny::fluidRow(
                shiny::column(
                  width = 12L,

                  ravedash::flex_group_box(
                    title = "Initial finder",
                    class = "row",

                    shidashi::flex_item(
                      shiny::numericInput(
                        inputId = ns("pulse_duration"),
                        label = shiny::span("Pulse duration (ms)",
                                            title = "A rough estimated duration in milliseconds; can be slightly larger (not smaller) than the actual stimulation pulse duration"),
                        value = 2,
                        min = 0,
                        step = 0.001
                      )
                    ),

                    shidashi::flex_break(),

                    shidashi::flex_item(
                      shiny::numericInput(
                        inputId = ns("pulse_count"),
                        label = shiny::span(
                          "Estimated #",
                          title = "A rough estimated number of pulses for the selected duration; leave it blank if you are unsure"
                        ),
                        value = NA,
                        min = 0,
                        step = 1
                      )
                    ),

                    shidashi::flex_item(
                      shiny::numericInput(
                        inputId = ns("pulse_threshold"),
                        label = shiny::span("Threshold", title = "Threshold to find pulses; leave it blank if you are unsure"),
                        value = NA,
                        min = 0,
                        step = 50
                      )
                    ),

                    shidashi::flex_break(),

                    shidashi::flex_item(shiny::tags$label(class = "control-label", "Find/clear pulses throghout the recording block")),
                    shidashi::flex_break(),

                    shidashi::flex_item(
                      shiny::actionButton(inputId = ns("clearall_btn"), label = "Clear all", width = "100%")
                    ),
                    shidashi::flex_item(
                      dipsaus::actionButtonStyled(inputId = ns("findall_btn"), label = "Find all pulses", width = "100%")
                    ),

                    shidashi::flex_break(),

                    shidashi::flex_item(shiny::tags$label(class = "control-label", "Find/clear pulses within the figure window")),
                    shidashi::flex_break(),

                    shidashi::flex_item(
                      shiny::actionButton(inputId = ns("clear_within_btn"), label = "Clear w/ window", width = "100%")
                    ),
                    shidashi::flex_item(
                      dipsaus::actionButtonStyled(inputId = ns("find_within_btn"), label = "Find w/ window", width = "100%")
                    )


                  ), # intial finder .row

                  ravedash::flex_group_box(

                    title = "Align pulses",

                    shidashi::flex_item(
                      shiny::numericInput(inputId = ns("expand_pre"), label = "Search time-points before stim onset",
                                          min = 1, step = 1, value = 20, width = "100%")
                    ),
                    shidashi::flex_item(
                      shiny::numericInput(inputId = ns("expand_post"), label = "Search time-points after stim offset",
                                          min = 1, step = 1, value = 40, width = "100%")
                    ),

                    shidashi::flex_break(),
                    shidashi::flex_item(
                      shiny::actionButton(inputId = ns("align_window_btn"), label = "Align w/ window", width = "100%")
                    ),
                    shidashi::flex_item(
                      dipsaus::actionButtonStyled(inputId = ns("align_all_btn"), label = "Align all", width = "100%")
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
              'Collapsed over frequency',
              class_body = "no-padding fill-width min-height-600 resize-vertical",
              shiny::div(
                class = 'position-relative height-350 fill-width',
                plotly::plotlyOutput(
                  outputId = ns("stream_plot"),
                  width = "100%",
                  height = "100%"
                )
              ),

              shiny::div(
                class = "padding-15",
                shiny::fluidRow(
                  shiny::column(
                    width = 2L,
                    shiny::selectInput(
                      inputId = ns("quality"),
                      label = "Quality",
                      choices = c("performance", "balanced", "high-quality"),
                      selected = "performance"
                    )
                  ),

                  shiny::column(
                    width = 2L,
                    shiny::numericInput(
                      inputId = ns("channel_gap"),
                      label = "Channel range",
                      min = 0, step = 50,
                      value = 1000
                    )
                  ),

                  shiny::column(
                    width = 2L,
                    shiny::numericInput(
                      inputId = ns("start_time"),
                      label = "Start time",
                      min = 0, max = 100, step = 1,
                      value = 0
                    )
                  ),

                  shiny::column(
                    width = 2L,
                    shiny::numericInput(
                      inputId = ns("duration"),
                      label = "Duration",
                      min = 0, step = 0.5,
                      value = 5
                    )
                  ),

                  shiny::column(
                    width = 2L,
                    shiny::actionButton(
                      inputId = ns("sync"),
                      label = "Sync selection"
                    )
                  )
                )
              ), # row

              shiny::div(
                class = 'position-relative fill-width height-350',
                shidashi::flex_container(
                  style = "width:100%; height:100%",

                  shidashi::flex_item(
                    shiny::plotOutput(
                      outputId = ns("snippet_plot"),
                      width = "100%",
                      height = "100%"
                    )
                  ),

                  shidashi::flex_item(
                    shiny::plotOutput(
                      outputId = ns("snippet_window_plot"),
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

    )
  )
}
