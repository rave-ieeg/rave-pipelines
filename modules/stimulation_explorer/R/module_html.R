

module_html <- function(){

  baseline_settings <- pipeline$read("baseline_settings")
  baseline_method <- "none"
  baseline_window <- c(-1, 0)
  if(is.list(baseline_settings) &&
     isTRUE(baseline_settings$method %in% BASELINE_CHOICES) &&
     baseline_settings$method != "none") {
    baseline_method <- baseline_settings$method
    baseline_window <- baseline_settings$window
    if(length(baseline_window) != 2) {
      baseline_window <- c(-1, 0)
    } else {
      baseline_window <- sort(baseline_window)
    }
  }

  interpolate <- pipeline$read("interpolate")
  interpolate_method <- "none"
  if(is.list(interpolate) &&
     isTRUE(interpolate$method %in% INTERPOLATE_CHOICES) &&
     interpolate$method != "none") {
    interpolate_method <- interpolate$method
  }

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            electrode_selector$ui_func(),

            ravedash::input_card(
              title = "Data Reshape & Transform",
              class_body = "fill-width padding-5",

              ravedash::flex_group_box(
                title = "Baseline settings",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("baseline_method"),
                    label = "Baseline method",
                    choices = BASELINE_CHOICES,
                    selected = baseline_method,
                    multiple = FALSE
                  )
                ),
                shidashi::flex_break(),
                shiny::conditionalPanel(
                  class = "fill-width padding-5",
                  style="flex:1",
                  condition = sprintf("input['%s'] !== 'none'", ns("baseline_method")),
                  shiny::sliderInput(
                    inputId = ns("baseline_window"),
                    label = "Baseline window",
                    min = min(c(-1, baseline_window)),
                    max = max(c(1, baseline_window)),
                    value = baseline_window, step = 0.01,
                    round = -2, ticks = TRUE, post = " s"
                  )
                )
              ),

              ravedash::flex_group_box(
                title = "Interpolation",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("interpolate_method"),
                    label = "Interpolate stimulation",
                    choices = INTERPOLATE_CHOICES,
                    selected = NULL, multiple = FALSE
                  )
                ),
                shidashi::flex_break(),
                shiny::conditionalPanel(
                  class = "fill-width padding-5",
                  style="flex:1",
                  condition = sprintf("input['%s'] !== 'none'", ns("interpolate_method")),
                  shiny::selectInput(
                    inputId = ns("interpolate_from"),
                    label = "From event",
                    choices = character()
                  )
                ),
                shiny::conditionalPanel(
                  class = "fill-width padding-5",
                  style="flex:1",
                  condition = sprintf("input['%s'] !== 'none'", ns("interpolate_method")),
                  shiny::selectInput(
                    inputId = ns("interpolate_to"),
                    label = "To event",
                    choices = character()
                  )
                )
              ),

              ravedash::flex_group_box(
                title = "Time-lock event",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("analysis_event"),
                    label = "Time-lock 0s to event",
                    choices = character(),
                    selected = NULL, multiple = FALSE
                  )
                )
              ),

              ravedash::flex_group_box(
                title = "Down-sample",
                shidashi::flex_item(
                  shiny::numericInput(
                    inputId = ns("downsample"),
                    label = "Down-sample signals",
                    min = 1, value = 1, step = 1, max = 30
                  ),
                  shiny::tags$small("It is recommended to down-sample to 1000Hz if the raw sample rate is high. If there is brain stimulation, please make sure the signal is interpolated to avoid down-sample aliases.", class = "text-gray")
                )
              )

            ),

            comp_condition_groups$ui_func(),

            comp_analysis_ranges$ui_func()

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
              'Collapsed over trial',
              class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                shiny::plotOutput(ns("collapse_over_trial"), width = '100%', height = "100%")
              ),
              footer = shiny::fluidRow(
                shiny::column(
                  width = 2L,
                  shiny::selectInput(
                    inputId = ns("collapse_over_trial_style"),
                    label = "Graph style",
                    choices = c("Stack vertically", "Shared canvas"),
                    selected = "Shared canvas"
                  )
                ),
                shiny::column(
                  width = 2L,
                  shiny::numericInput(
                    inputId = ns("collapse_over_trial_value_range"),
                    label = "Data range",
                    min = 0, value = 0
                  )
                ),
                shiny::column(
                  width = 2L,
                  shiny::numericInput(
                    inputId = ns("collapse_over_trial_time_range_min"),
                    label = "Min time", value = 0, step = 0.05
                  )
                ),
                shiny::column(
                  width = 2L,
                  shiny::numericInput(
                    inputId = ns("collapse_over_trial_time_range_max"),
                    label = "Max time", value = 0, step = 0.05
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
