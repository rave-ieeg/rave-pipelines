

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
              title = "Configurations",
              shiny::fluidRow(
                shiny::column(
                  width = 12L,

                  ravedash::flex_group_box(
                    title = "Recording block",

                    shidashi::flex_item(
                      shiny::selectInput(
                        inputId = ns("recording_block"),
                        label = "Block name",
                        choices = c(""),
                        selected = ""
                      )
                    )

                  ),

                  ravedash::flex_group_box(
                    title = "Source of annotations",

                    shidashi::flex_item(
                      shiny::selectInput(
                        inputId = ns("annotation_source"),
                        label = "Epoch name",
                        choices = c(""),
                        selected = ""
                      )
                    ),

                    shidashi::flex_break(),

                    shidashi::flex_item(
                      shiny::selectInput(
                        inputId = ns("annotation_events"),
                        label = "Event names (trial onset is always included)",
                        choices = character(),
                        selected = character(),
                        multiple = TRUE
                      )
                    )

                  ), # Source of annotations

                  ravedash::flex_group_box(
                    title = "Signal filters",

                    shidashi::flex_item(
                      shiny::numericInput(
                        inputId = ns("filter_highpass"),
                        label = "High-pass (Hz)",
                        min = 0, value = NA, max = 100
                      )
                    ),

                    shidashi::flex_item(
                      shiny::numericInput(
                        inputId = ns("filter_lowpass"),
                        label = "Low-pass (Hz)",
                        min = 0, value = NA, max = 100
                      )
                    )

                  ),

                  ravedash::run_analysis_button("Visualize!", width = "100%")

                ) # col-12
              ) # row
            ), # "Data Configurations"

            electrode_selector$ui_func(),

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
              'Channel Viewer',
              class_body = "no-padding fill-width height-vh75 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                plotly::plotlyOutput(ns("channel_viewer"), width = '100%', height = "100%")
              ),
              footer = shidashi::flex_container(
                shidashi::flex_item(
                  size = 2,
                  shiny::selectInput(
                    inputId = ns("auto_decimate"),
                    label = "Quality",
                    choices = c("performance", "balanced", "high-quality"),
                    selected = "performance"
                  )
                ),

                shidashi::flex_item(
                  size = 2,
                  shiny::numericInput(
                    inputId = ns("viewer_channel_gap"),
                    label = "Channel gap",
                    value = NA, step = 50,
                    min = 0, width = "100%"
                  )
                ),

                shidashi::flex_item(
                  size = 2,
                  shiny::numericInput(
                    inputId = ns("viewer_start_time"),
                    min = 0,
                    max = 1,
                    step = 4,
                    label = "Start time",
                    value = 0
                  )
                ),

                shidashi::flex_item(
                  size = 2,
                  shiny::numericInput(
                    inputId = ns("viewer_duration"),
                    label = "Max duration",
                    min = 1,
                    value = 5, step = 1
                  )
                ),

                shidashi::flex_item(
                  size = 1,

                  shiny::div(
                    style = "width:100%;",
                    shiny::div(
                      shiny::tags$label("Sync", class = "control-label")
                    ),
                    shiny::div(
                      shiny::actionButton(
                        inputId = ns("viewer_apply_brush"),
                        label = "",
                        icon = ravedash::shiny_icons$sync
                      )
                    )
                  )

                ),

                shidashi::flex_item(
                  size = 5,
                  shiny::fluidRow(
                    shiny::column(width = 8L, shiny::selectInput(
                      inputId = ns("viewer_trial"),
                      label = "Condition",
                      choices = "", selectize = FALSE
                    )),
                    shiny::column(width = 4L, shiny::div(
                      style = "width:100%;",
                      shiny::div(
                        shiny::tags$label("Switch trials", class = "control-label")
                      ),
                      shiny::div(
                        shiny::actionButton(
                          inputId = ns("prev_trial"),
                          label = "",
                          icon = ravedash::shiny_icons$angle_left
                        ),
                        shiny::actionButton(
                          inputId = ns("next_trial"),
                          label = "",
                          icon = ravedash::shiny_icons$angle_right
                        )
                      )
                    ))
                  )
                )
              )
            ) # ravedash::output_card( 'Channel Viewer',
          )
        )
      )

    )
  )
}
