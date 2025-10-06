

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
                    inputId = ns("zeta_threshold"),
                    label = "Zeta threshold",
                    min = 0.05, max = 0.95, step = 0.05,
                    value = 0.5
                  )
                ),


                shiny::column(
                  width = 6L,
                  shiny::actionButton(
                    inputId = ns("btn_load_settings"),
                    label = "Load Settings",
                    icon = ravedash::shiny_icons$upload
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::downloadButton(
                    outputId = ns("btn_download_settings"),
                    label = "Download Settings",
                    icon = ravedash::shiny_icons$download,
                    class = "fill-width"
                  )
                )

              )
            ),

            baseline_choices$ui_func(),

            comp_condition_groups$ui_func()
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
              title = "3D Viewer",
              class_body = "no-padding fill-width height-550 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                threeBrain::threejsBrainOutput(outputId = ns("viewer"), height = "100%")
              )
            ),

            ravedash::output_cardset(
              title = ' ',
              class_body = "no-padding fill-width min-height-450",

              "Channel time-series" = shiny::div(
                class = 'position-relative fill-width height-vh80 min-height-450 resize-vertical',
                shiny::plotOutput(ns("channel_cluster_timeseries"), width = '100%', height = "100%")
              ),

              "Diagnosic plots" = shiny::div(
                class = 'position-relative fill-width height-800 min-height-600 resize-vertical',
                shidashi::flex_container(
                  direction = "row", style = "height: 40%",

                  shidashi::flex_item(
                    size = 2,
                    shiny::plotOutput(ns("cluster_dendrogram_plot"), width = '100%', height = "100%")
                  ),

                  shidashi::flex_item(
                    size = 1,
                    shiny::plotOutput(ns("cluster_silhouette_plot"), width = '100%', height = "100%", click = shiny::clickOpts(id = ns("cluster_silhouette_plot_click")))
                  )

                ),
                shidashi::flex_container(
                  direction = "row", style = "height: 60%",
                  shidashi::flex_item(
                    shiny::plotOutput(ns("cluster_mean_plot"), width = '100%', height = "100%")
                  )
                )
              ),

              "Clustering table" = shiny::div(
                class = 'position-relative fill-width height-800 resize-vertical',
                shiny::tableOutput(outputId = ns("cluster_table"))
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
