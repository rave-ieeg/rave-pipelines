

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
              title = "Electrode groups",

              shidashi::register_input(
                {
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
                  )
                },
                inputId = "electrode_group",
                update = "dipsaus::updateCompoundInput2",
                description = paste(
                  "Set groups for electrode channels so that each group can be assigned a reference type.",
                  "The value of the compound input is a list of groups, each group is a list with two elements: name and electrodes.",
                  "The electrodes can be specified as a comma-separated list of channel indices, or a range of channel indices (e.g. 1-10).",
                  "An example value (JSON) is: [{\"name\":\"Group 1\",\"electrodes\":\"1-10,15,20\"},{\"name\":\"Group 2\",\"electrodes\":\"11-14,16-19\"}].",
                  "Three rules: 1. no overlap between groups; 2. each LFP channel must be assigned to a group; 3. group names must be unique.",
                  "A group lists the channels to be referenced; their reference channels may be outside the group.",
                  "Run script `update_electrode_group` (button `electrode_group_btn`) to apply the groups."
                )
              ),
              footer = shidashi::register_input(
                dipsaus::actionButtonStyled(
                  inputId = ns("electrode_group_btn"),
                  label = "Set groups", width = "100%"
                ),
                inputId = "electrode_group_btn",
                update = "dipsaus::updateActionButtonStyled", 
                description = "Click to set the electrode groups. This will update the reference settings and the reference table."
              )
            ),

            ravedash::input_card(
              title = "Reference settings",
              start_collapsed = TRUE,
              class_foot = "no-padding",

              shiny::fluidRow(

                shiny::column(
                  width = 12,
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("group_name"),
                      label = "Group name",
                      choices = character(0L)
                    ),
                    inputId = "group_name",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = "Select a group to set the reference type for that group. Make sure to set the electrode groups and click on `electrode_group_btn` first."
                  )
                ),

                shiny::column(
                  width = 12,
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("reference_type"),
                      label = "Reference type",
                      choices = reference_choices,
                      selected = "No Reference"
                    ),
                    tooltip = "Reference type for the selected group.",
                    inputId = "reference_type",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = "Select a reference type for the selected group (by input `group_name`)."
                  )
                )

              ),

              shiny::uiOutput(outputId = ns("reference_details")),


              footer = shiny::tagList(
                shiny::uiOutput(
                  outputId = ns("group_description")
                ),
                threeBrain::threejsBrainOutput(
                  outputId = ns("group_3dviewer"),
                  height = "450px"
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
            #   shiny::plotOutput(ns("reference_plot_signals"), width = "100%", height = "100%")
            # )
            shidashi::register_input(
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
                  # MIGRATED: removed ravedash::output_gadget_container() wrapper
                  # ravedash::output_gadget_container(
                    shiny::plotOutput(ns("reference_plot_signals"),
                                      width = "100%", height = "100%")
                  # )
                ),
                `Electrode details` = shiny::div(
                  class = "fill height-600 resize-vertical",
                  # MIGRATED: removed ravedash::output_gadget_container() wrapper
                  # ravedash::output_gadget_container(
                    shiny::plotOutput(ns("reference_plot_electrode"),
                                      width = "100%", height = "100%")
                  # )
                ),
                `Reference signal` = shiny::div(
                  class = "fill height-600 resize-vertical",
                  # MIGRATED: removed ravedash::output_gadget_container() wrapper
                  # ravedash::output_gadget_container(
                    shiny::plotOutput(ns("reference_plot_heatmap"),
                                      width = "100%", height = "100%")
                  # )
                ),
                `Preview & Export` = shiny::div(
                  class = "fill height-600 resize-vertical padding-5",
                  shiny::tableOutput(ns("reference_table_preview"))
                )

              ),
              inputId = "reference_output_tabset",
              update = "shidashi::card_tabset_activate(value=title)",
              description = paste(
                "Active tab of the output card: 'Group inspection', 'Electrode details',",
                "'Reference signal', or 'Preview & Export'. Set it to 'Preview & Export'",
                "to show the reference table and input `preview_save_name` for saving."
              )
            )

            # ravedash::output_card(
            #   "Collapsed over frequency",
            #   class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
            #   shiny::div(
            #     class = "position-relative fill",
            #     shiny::plotOutput(ns("collapse_over_trial"), width = "100%", height = "100%")
            #   )
            # )
          )
        )
      )

    )
  )
}
