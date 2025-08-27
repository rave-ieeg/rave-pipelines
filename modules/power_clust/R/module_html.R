

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

            comp_condition_groups$ui_func(),

            baseline_choices$ui_func(),

            # comp_analysis_ranges$ui_func()

            ravedash::input_card(
              class_header = "shidashi-anchor",
              title = "Configure Analysis",

              shiny::fluidRow(
                shiny::column(
                  width = 12L,
                  shiny::sliderInput(
                    inputId = ns("time_range"),
                    label = "Time range (relative to event start)",
                    min = 0, max = 1, step = 0.1,
                    value = c(0, 1)
                  )
                ),
                shiny::column(
                  width = 12L,
                  shiny::sliderInput(
                    inputId = ns("frequency_range"),
                    label = "Frequency range",
                    min = 0, max = 200, step = 1,
                    value = c(0, 200)
                  )
                ),


                shiny::column(
                  width = 12L,
                  shiny::sliderInput(
                    inputId = ns("zeta_threshold"),
                    label = "Zeta threshold",
                    min = 0.05, max = 0.95, step = 0.05,
                    value = 0.5
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
            ravedash::output_cardset(
              title = 'Reuslts',
              class_body = "no-padding fill-width min-height-450",

              "Channel time-series" = shiny::div(
                class = 'position-relative fill-width height-vh80 min-height-450 resize-vertical',
                shiny::plotOutput(ns("channel_cluster_timeseries"), width = '100%', height = "100%")
              ),

              "Diagnosis" = shiny::div(
                class = 'position-relative fill-width height-450 min-height-450 resize-vertical',
                shidashi::flex_container(
                  direction = "row", style = "height: 100%",

                  shidashi::flex_item(
                    size = 2,
                    shiny::plotOutput(ns("cluster_dendrogram_plot"), width = '100%', height = "100%")
                  ),

                  shidashi::flex_item(
                    size = 1,
                    shiny::plotOutput(ns("cluster_silhouette_plot"), width = '100%', height = "100%")
                  )

                )
              ),

              footer = shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  shiny::numericInput(
                    inputId = ns("n_clusters"),
                    label = "# of clusters",
                    min = 1, step = 1, max = 100,
                    value = 1
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
