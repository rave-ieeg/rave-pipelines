make_by_frequency_tabset <- function() {
  shidashi::register_input(
    ravedash::output_cardset(
      inputId = ns('by_frequency_tabset'),
      title='By Frequency',
      class_body = "no-padding position-relative fill height-400 min-height-400 resize-vertical",
      tools = list(
        shidashi::card_tool(
          widget = "custom", icon = ravedash::shiny_icons$puzzle,
          inputId = ns("by_frequency_tabset_config")
        ),
        shidashi::card_tool(
          widget = "custom", icon = ravedash::shiny_icons$camera,
          inputId = ns("by_frequency_tabset_camera")
        )
      ),
      append_tools = FALSE,
      `Over time` =
        shiny::div(
          class = "min-height-400 resize-vertical position-relative fill",

          # Control Panel
          make_heatmap_control_panel(prefix = 'bfot', config='by_frequency_tabset_config'),

          # Plot
          # MIGRATED: removed ravedash::output_gadget_container() wrapper
          # ravedash::output_gadget_container(
            ravedash::plotOutput2(
              outputId = ns("by_frequency_over_time"),
              width = '100%', height='100%'
            )
          # )
        ),

      `Correlation` = shiny::div(
        class = "min-height-400 resize-vertical position-relative fill",

        # Control Panel
        make_heatmap_control_panel(prefix = 'bfc', config='by_frequency_tabset_config',
                                   max = c(1, 0, 1, .1), percentile=FALSE),

        # Plot

        # MIGRATED: removed ravedash::output_gadget_container() wrapper
        # ravedash::output_gadget_container(
          ravedash::plotOutput2(
            outputId = ns("by_frequency_correlation"),
            width = '100%', height='100%'
          )
        # )
      )
    ),
    inputId = "by_frequency_tabset",
    update = "shidashi::card_tabset_activate(value=title)",
    description = paste(
      "Active tab of the 'By Frequency' card: 'Over time' (plot",
      "`by_frequency_over_time`, frequency by time heatmaps) or 'Correlation'",
      "(plot `by_frequency_correlation`). Set it to show the user a tab."
    )
  )
}

make_heatmap_control_panel <- function(prefix, config, max=c(99, 0, 1e7, 1), percentile=TRUE,
                                       range_is_global=TRUE, do_xlim=TRUE, do_aw_only_scale=FALSE, range_is_aw_only = do_aw_only_scale) {

  # Which plot the options belong to, for the input descriptions (MCP)
  plot_names <- c(
    bfot = "the 'By Frequency' card, tab 'Over time' (plot `by_frequency_over_time`)",
    bfc = "the 'By Frequency' card, tab 'Correlation' (plot `by_frequency_correlation`)",
    otbt = "the 'Over Time' card, tab 'By Trial' (plot `over_time_by_trial`)",
    otbe = "the 'By Electrode' card, tab 'Over Time' (plot `over_time_by_electrode`)",
    bewot = "the 'By Electrode' card, tab 'Waterfall over Time' (plot `waterfall_by_electrode_plot`)"
  )
  plot_name <- plot_names[prefix]
  if(is.na(plot_name)) {
    plot_name <- sprintf("the plot with prefix `%s`", prefix)
  }
  describe <- function(...) {
    paste(..., sprintf(
      "Option of %s, in the option panel that the card's puzzle-piece icon opens.",
      plot_name
    ))
  }

  shiny::conditionalPanel(
    condition = sprintf("input['%s'] %% 2 == 1", config),
    ns = ns,
    shiny::div(
      class = "container-fluid",
      shiny::fluidRow(
        shiny::column(
          width = 1L,
          shidashi::register_input(
            shiny::numericInput(ns(prefix %&% '_range'), label = 'Plot Max',
                                value = max[1], min = max[2], max = max[3], step = max[4]),
            inputId = prefix %&% '_range',
            update = "shiny::updateNumericInput",
            description = describe(
              "'Plot Max': the upper limit of the symmetric color scale (for the",
              "waterfall plot, the value that spans one row). 0 uses the largest",
              "absolute value. With `" %&% prefix %&% "_range_is_percentile`",
              "('Max is %') checked, a value below 100 is a percentile of the",
              "absolute values (e.g. 99) and 100 or more is a percent of the",
              "largest one; unchecked, it is in the plot's units."
            )
          )
        ),
        shiny::column(width = 1L, style='text-align: left; margin-top:37px; margin-left:0px',
                      shidashi::register_input(
                        shiny::checkboxInput(ns(prefix %&% '_range_is_percentile'),
                                             label = 'Max is %', value = percentile),
                        inputId = prefix %&% '_range_is_percentile',
                        update = "shiny::updateCheckboxInput",
                        description = describe(
                          "'Max is %': whether `" %&% prefix %&% "_range` is a",
                          "percentile (TRUE) or a value in the plot's units (FALSE)."
                        )
                      )),

        shiny::column(width = 1L, style='text-align: left; margin-top:37px; margin-left:0px',
                      shidashi::register_input(
                        shiny::checkboxInput(ns(prefix %&% '_scale_is_global'),
                                             label = 'Global scale', value = range_is_global),
                        inputId = prefix %&% '_scale_is_global',
                        update = "shiny::updateCheckboxInput",
                        description = describe(
                          "'Global scale': TRUE gives all panels one color scale;",
                          "FALSE scales each panel on its own (only with 'Max is %')."
                        )
                      )),
        if(do_aw_only_scale) {
          shiny::column(width = 1L, style='text-align: left; margin-top:37px; margin-left:0px',
                        shidashi::register_input(
                          shiny::checkboxInput(ns(prefix %&% '_scale_based_on_aw'),
                                               label = 'Range is AW', value = range_is_aw_only),
                          inputId = prefix %&% '_scale_based_on_aw',
                          update = "shiny::updateCheckboxInput",
                          description = describe(
                            "'Range is AW': TRUE computes the 'Plot Max' percentile",
                            "within the analysis window only."
                          )
                        )
          )
        },
        if(do_xlim){
          shiny::column(width = 2L,
                        shidashi::register_input(
                          shiny::sliderInput(ns(prefix %&% '_xlim'),value = c(-1,2), step=c(0.01),
                                             label = 'X range', min = -10, max = 10),
                          inputId = prefix %&% '_xlim',
                          update = "shiny::updateSliderInput",
                          description = describe(
                            "'X range': [start, end] of the time shown, in seconds, e.g.",
                            "[-0.5, 1.5]. Loading data and full runs can reset it to the",
                            "full time range, so set it after `run_analysis`."
                          )
                        ))
        },
        shiny::column(width = 1L, offset = ifelse(do_xlim, 0, 1),
                      shidashi::register_input(
                        shiny::numericInput(ns(prefix %&% '_ncol'), label = '# Col',
                                            value = 3, min = 0, max = 1e7),
                        inputId = prefix %&% '_ncol',
                        update = "shiny::updateNumericInput",
                        description = describe(
                          "'# Col': the number of panel columns (default 3)."
                        )
                      )),
        shiny::column(width = 1L, style='text-align: left;
                            margin-top:37px; margin-left:0px',
                      shidashi::register_input(
                        shiny::checkboxInput(ns(prefix %&% '_byrow'),
                                             label = 'Order by row', value = TRUE),
                        inputId = prefix %&% '_byrow',
                        update = "shiny::updateCheckboxInput",
                        description = describe(
                          "'Order by row': TRUE fills the panels row by row, FALSE",
                          "column by column."
                        )
                      )),

        shiny::column(width = 2L, style='text-align: left;
                            margin-top:37px; margin-left:0px',
                      shidashi::register_input(
                        shiny::checkboxInput(ns(prefix %&% '_show_window'),
                                             label = 'Show AW', value = TRUE),
                        inputId = prefix %&% '_show_window',
                        update = "shiny::updateCheckboxInput",
                        description = describe(
                          "'Show AW': outline the analysis window(s) on the plot."
                        )
                      ))
      )
    )
  )
}
