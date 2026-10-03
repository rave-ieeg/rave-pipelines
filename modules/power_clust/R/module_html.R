

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
              class_header = "shidashi-anchor",
              title = "Configure Analysis",

              shiny::fluidRow(
                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::sliderInput(
                      inputId = ns("time_range"),
                      label = "Time range (relative to event start)",
                      min = 0, max = 1, step = 0.1,
                      value = c(0, 1)
                    ),
                    tooltip = "Window to analyze, in seconds after each condition group's start event.",
                    inputId = "time_range",
                    update = "shiny::updateSliderInput",
                    description = paste(
                      "Analysis window, JSON `[start, end]` in seconds relative to",
                      "the start event of each condition group (see",
                      "`condition_groups`): each group is analyzed from `start` to",
                      "`end` after its start event, or until its finish event.",
                      "Bounds are the loaded epoch window (step 0.1 s); it exists",
                      "after `load_data`. Read by script `run_analysis`, which",
                      "saves it as `analysis_window`."
                    )
                  )
                ),
                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::sliderInput(
                      inputId = ns("frequency_range"),
                      label = "Frequency range",
                      min = 0, max = 200, step = 1,
                      value = c(0, 200)
                    ),
                    tooltip = "Frequency band (Hz) whose power is averaged; it must include at least one wavelet frequency.",
                    inputId = "frequency_range",
                    update = "shiny::updateSliderInput",
                    description = paste(
                      "Frequency band in Hz, JSON `[low, high]` (step 1); bounds",
                      "are the subject's wavelet frequencies. The band must contain",
                      "at least one wavelet frequency, otherwise script",
                      "`run_analysis` stops with 'Frequency range is too narrow'.",
                      "Power is averaged over the frequencies in the band."
                    )
                  )
                ),


                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::sliderInput(
                      inputId = ns("zeta_threshold"),
                      label = "Zeta threshold",
                      min = 0.05, max = 0.95, step = 0.05,
                      value = 0.5
                    ),
                    tooltip = "Threshold of the decomposition of each group's electrode similarity matrix; lower values tend to keep fewer components (default 0.5).",
                    inputId = "zeta_threshold",
                    update = "shiny::updateSliderInput",
                    description = paste(
                      "Zeta threshold, a number from 0.05 to 0.95 (step 0.05,",
                      "default 0.5), passed to the decomposition of each condition",
                      "group's electrode similarity matrix. Read by script",
                      "`run_analysis`."
                    )
                  )
                ),


                shiny::column(
                  width = 6L,
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("btn_load_settings"),
                      label = "Load Settings",
                      icon = ravedash::shiny_icons$upload
                    ),
                    tooltip = "Load the analysis settings from a YAML file.",
                    inputId = "btn_load_settings",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "Button 'Load Settings': opens a dialog to upload a settings",
                      "YAML file. For people only: agents set the analysis inputs",
                      "directly instead."
                    ),
                    writable = FALSE
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

            # Registered at the call site: the preset's functions live in the
            # ravedash namespace, where `register_input` cannot reach the
            # module's registry. The card itself is unchanged
            shidashi::register_input(
              comp_condition_groups$ui_func(),
              inputId = "condition_groups",
              update = "dipsaus::updateCompoundInput2",
              description = paste(
                "Condition groups (a compound input in the card 'Create",
                "Condition Contrast'), JSON array with one object per group:",
                "`[{\"group_name\": \"Auditory\", \"group_conditions\": [\"drive_a\",",
                "\"known_a\"], \"group_start_event\": \"Trial Onset\",",
                "\"group_finish_event\": \"[Analysis end]\"}]` (1 to 40 groups).",
                "`group_conditions` are condition names of the loaded epoch;",
                "`group_start_event` is 'Trial Onset' or an event of the epoch;",
                "`group_finish_event` is '[Analysis end]' (use the end of",
                "`time_range`), an event, or 'Trial Onset'. A group without a",
                "valid condition is skipped; an empty name becomes 'groupNN'.",
                "Electrodes are clustered on their responses across all groups.",
                "Loading data resets it to the saved groups, or to one group",
                "'All Conditions'. Read by script `run_analysis`."
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
              title = "3D Viewer",
              class_body = "no-padding fill-width height-550 min-height-450 resize-vertical",
              shiny::div(
                class = "position-relative fill",
                threeBrain::threejsBrainOutput(outputId = ns("viewer"), height = "100%")
              )
            ),

            # Registered so that agents can switch the tabs (MCP); the stable
            # `inputId` replaces the random one, so the tab set looks the same
            shidashi::register_input(
              inputId = "cluster_tabset",
              update = "shidashi::card_tabset_activate(value=title)",
              description = paste(
                "Active tab of the output card under the 3D viewer: 'Channel",
                "time-series' (plot `channel_cluster_timeseries`), 'Diagnosic",
                "plots' (`cluster_dendrogram_plot`, `cluster_silhouette_plot`,",
                "`cluster_mean_plot`), or 'Clustering table' (`cluster_table`).",
                "These outputs are not registered: picture them with",
                "`shiny_query_ui` (e.g. `#power_clust-cluster_mean_plot`) only",
                "while their tab is showing. Its footer holds `n_clusters`."
              ),
            ravedash::output_cardset(
              inputId = ns("cluster_tabset"),
              title = " ",
              class_body = "no-padding fill-width min-height-450",

              "Channel time-series" = shiny::div(
                class = "position-relative fill-width height-vh80 min-height-450 resize-vertical",
                shiny::plotOutput(ns("channel_cluster_timeseries"), width = "100%", height = "100%")
              ),

              "Diagnostic plots" = shiny::div(
                class = "position-relative fill-width height-800 min-height-600 resize-vertical",
                shidashi::flex_container(
                  direction = "row", style = "height: 50%",

                  shidashi::flex_item(
                    size = 2,
                    shiny::plotOutput(ns("cluster_dendrogram_plot"), width = "100%", height = "100%")
                  ),

                  shidashi::flex_item(
                    size = 1,
                    shiny::plotOutput(ns("cluster_silhouette_plot"), width = "100%", height = "100%", click = shiny::clickOpts(id = ns("cluster_silhouette_plot_click")))
                  )

                ),
                shidashi::flex_container(
                  direction = "row", style = "height: 50%",
                  shidashi::flex_item(
                    shiny::plotOutput(ns("cluster_mean_plot"), width = "100%", height = "100%")
                  )
                )
              ),

              "Clustering table" = shiny::div(
                class = "position-relative fill-width height-500 resize-vertical",
                shiny::tableOutput(outputId = ns("cluster_table"))
              ),

              footer = shiny::fluidRow(
                shiny::column(
                  width = 3L,
                  shidashi::register_input(
                    shiny::numericInput(
                      inputId = ns("n_clusters"),
                      label = "# of clusters",
                      min = 1, step = 1, max = 100,
                      value = 1
                    ),
                    tooltip = "Number of clusters to cut the electrode tree into; each run sets it to the suggested number.",
                    inputId = "n_clusters",
                    update = "shiny::updateNumericInput",
                    description = paste(
                      "Number of clusters k (integer), in the footer of the output",
                      "card. Each `run_analysis` sets it to the suggested k (best",
                      "silhouette score) and its maximum to the largest k scored.",
                      "Changing it cuts the same tree again, without re-running:",
                      "the plots, the table, script `cluster_summary`, and the 3D",
                      "viewer's 'Cluster' colors follow."
                    )
                  )
                )
              )


            )) # end of register_input("cluster_tabset")
          )
        )
      )

    )
  )
}
