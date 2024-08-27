

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
              title = "Electrode groups",

              dipsaus::compoundInput2(
                inputId = ns("electrode_group"), min_ncomp = 1, max_ncomp = 100,
                label_color = "#c8c9ca", label = NULL,
                components = {
                  shidashi::flex_container(
                    style = "margin:-5px;",
                    shidashi::flex_item(
                      shiny::textInput(inputId = "name", label = "Group name")
                    ),
                    shidashi::flex_item(
                      shiny::textInput(inputId = "electrodes", label = "Electrodes")
                    )
                    # shiny::selectInput(inputId = "reference_type", label = "Reference type", multiple = FALSE, choices = c("No Reference", "Common Average Reference", "White-matter Reference", "Bipolar Reference")),
                  )
                }
              ),

              footer = dipsaus::actionButtonStyled(
                inputId = ns("electrode_group_btn"),
                label = "Set groups", width = "100%"
              )

            ),

            ravedash::input_card(
              title = "Reference settings",
              start_collapsed = TRUE,
              class_foot = "no-padding",

              shiny::selectInput(
                inputId = ns("group_name"),
                label = "Group name",
                choices = character(0L)
              ),


              shiny::selectInput(
                inputId = ns("reference_type"),
                label = "Reference type",
                choices = reference_choices,
                selected = "No Reference"
              ),


              shiny::uiOutput(outputId = ns("reference_details")),


              footer = shiny::tagList(
                shiny::uiOutput(
                  outputId = ns("group_description")
                ),
                threeBrain::threejsBrainOutput(
                  outputId = ns("group_3dviewer"),
                  height = "300px"
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

            # ravedash::output_card(
            #   title = "Group inspection",
            #   class_body = "vh-80 resize-vertical min-height-450",
            #   shiny::plotOutput(ns("reference_plot_signals"), width = '100%', height = "100%")
            # )
            shidashi::card_tabset(

              inputId = ns("reference_output_tabset"),
              title = "Reference table & visualization",

              tools = list(
                shidashi::card_tool(widget = "maximize")
              ),
              class_body = "no-padding",
              class_foot = "padding-left-8 padding-right-8",

              footer = shiny::uiOutput(ns("reference_output_tabset_footer")),
              # shiny::fluidRow(
              #   shiny::div(
              #     class = "border-right col-sm-2",
              #     shiny::selectInput(
              #       inputId = ns("plot_block"),
              #       label = "Session block",
              #       choices = character(0),
              #       selectize = FALSE
              #     )
              #   ),
              #   shiny::column(
              #     width = 10L,
              #     shiny::uiOutput(ns("reference_output_tabset_footer"))
              #   )
              # ),

              # First tab
              `Group inspection` = shiny::div(
                class = "fill height-600 resize-vertical",
                ravedash::output_gadget_container(
                  shiny::plotOutput(ns("reference_plot_signals"),
                                    width = "100%", height = '100%')
                )
              ),
              `Electrode details` = shiny::div(
                class = "fill height-600 resize-vertical",
                ravedash::output_gadget_container(
                  shiny::plotOutput(ns("reference_plot_electrode"),
                                    width = "100%", height = '100%')
                )
              ),
              `Reference signal` = shiny::div(
                class = "fill height-600 resize-vertical",
                ravedash::output_gadget_container(
                  shiny::plotOutput(ns("reference_plot_heatmap"),
                                    width = "100%", height = '100%')
                )
              ),
              `Preview & Export` = shiny::div(
                class = "fill height-600 resize-vertical padding-5",
                shiny::tableOutput(ns("reference_table_preview"))
              )

            )

            # ravedash::output_card(
            #   'Collapsed over frequency',
            #   class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
            #   shiny::div(
            #     class = 'position-relative fill',
            #     shiny::plotOutput(ns("collapse_over_trial"), width = '100%', height = "100%")
            #   )
            # )
          )
        )
      )

    )
  )
}
