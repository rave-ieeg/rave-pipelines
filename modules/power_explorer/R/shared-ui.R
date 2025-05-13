make_by_frequency_tabset <- function() {
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
        ravedash::output_gadget_container(
          ravedash::plotOutput2(
            outputId = ns("by_frequency_over_time"),
            width = '100%', height='100%'
          )
        )
      ),

    `Correlation` = shiny::div(
      class = "min-height-400 resize-vertical position-relative fill",

      # Control Panel
      make_heatmap_control_panel(prefix = 'bfc', config='by_frequency_tabset_config',
                                 max = c(1, 0, 1, .1), percentile=FALSE),

      # Plot

      ravedash::output_gadget_container(
        ravedash::plotOutput2(
          outputId = ns("by_frequency_correlation"),
          width = '100%', height='100%'
        )
      )
    )
  )
}

make_heatmap_control_panel <- function(prefix, config, max=c(99, 0, 1e7, 1), percentile=TRUE, range_is_global=TRUE, do_xlim=TRUE) {
  shiny::conditionalPanel(
    condition = sprintf("input['%s'] %% 2 == 1", config),
    ns = ns,
    shiny::div(
      class = "container-fluid",
      shiny::fluidRow(
        shiny::column(width = 2L,
                      shiny::numericInput(ns(prefix %&% '_range'), label = 'Plot Max',
                                          value = max[1], min = max[2], max = max[3], step = max[4])),
        shiny::column(width = 1L, style='text-align: left; margin-top:37px; margin-left:0px',
                      shiny::checkboxInput(ns(prefix %&% '_range_is_percentile'),
                                           label = 'Max is %', value = percentile)),
        shiny::column(width = 1L, style='text-align: left; margin-top:37px; margin-left:0px',
                      shiny::checkboxInput(ns(prefix %&% '_scale_is_global'),
                                           label = 'Global scale', value = range_is_global)),
        if(do_xlim){
          shiny::column(width = 3L,
                        shiny::sliderInput(ns(prefix %&% '_xlim'),value = c(-1,2), step=c(0.01),
                                           label = 'X range', min = -10, max = 10))
        },
        shiny::column(width = 1L, offset = ifelse(do_xlim, 0, 1),
                      shiny::numericInput(ns(prefix %&% '_ncol'), label = '# Col',
                                          value = 3, min = 0, max = 1e7)),
        shiny::column(width = 2L, style='text-align: left;
                            margin-top:37px; margin-left:0px',
                      shiny::checkboxInput(ns(prefix %&% '_byrow'),
                                           label = 'Order by row', value = TRUE)),

        shiny::column(width = 2L, style='text-align: left;
                            margin-top:37px; margin-left:0px',
                      shiny::checkboxInput(ns(prefix %&% '_show_window'),
                                           label = 'Show Window', value = TRUE))
      )
    )
  )
}
