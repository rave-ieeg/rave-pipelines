

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
              title = "Load epoch channel",

              ravedash::flex_group_box(
                title = "Basic configuration",
                shiny::selectInput(
                  inputId = ns("load_epoch"),
                  label = "Load existing epoch",
                  choices = "New epoch..."
                ),
                shiny::selectInput(
                  inputId = ns("block"),
                  label = "Choose a block to generate epoch",
                  choices = character(0L)
                ),
                shiny::selectInput(
                  inputId = ns("epoch_file"),
                  label = "File containing epoch channel",
                  choices = character(0L)
                ),
                dipsaus::actionButtonStyled(
                  inputId = ns("load_epoch_btn"),
                  label = "Load epoch file",
                  width = "100%"
                )
              ),

              ravedash::flex_group_box(
                title = "Epoch channel settings",
                shiny::selectInput(
                  inputId = ns("varname"),
                  label = "Variable name",
                  choices = character(0L)
                ),
                shiny::numericInput(
                  inputId = ns("sample_rate"),
                  label = "Epoch channel sample rate",
                  value = 1
                ),
                dipsaus::actionButtonStyled(
                  inputId = ns("load_signal_btn"),
                  label = "Load epoch signal",
                  width = "100%"
                )
              )
            ),


            ravedash::input_card(
              title = "Transform & threshold",
              class_body = "padding-10",
              shiny::fluidRow(
                shiny::column(
                  width = 12L,
                  shiny::numericInput(
                    ns("plot_range"),
                    "Plot range",
                    min = 0, step = 1, value = 0
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::checkboxInput(
                    ns("plot_difference"),
                    "Difference plot"
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::checkboxInput(
                    ns("plot_absolute"),
                    "Absolute plot"
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::selectInput(
                    ns("threshold_direction"),
                    "Threshold direction",
                    choices = c("Above", "Below")
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    ns("num_duration"),
                    "Minimum duration (s)",
                    value = 0, step = 1, min = 0
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
              title = "Interactive selector",
              tools = list(shidashi::card_tool(
                inputId = ns("download_btn1"),
                widget = "custom",
                icon = ravedash::shiny_icons$download
              )),
              footer = shiny::div(
                class = "fill-width text-align-right",
                shiny::actionLink(
                  ns("download_btn2"),
                  icon = ravedash::shiny_icons$download,
                  "Save & download staged table to subject's meta folder"
                )
              ),
              class_body = "padding-10",
              ravedash::flex_group_box(
                title = "Graphical visualization",

                shidashi::flex_item(
                  size = 1, class = "overflow-hidden",
                  title = "Press down, hold, and draw a time-range to see details",
                  ravedash::output_gadget_container(
                    shiny::plotOutput(
                      outputId = ns("plot_overall"),
                      width = "100%", height = "200px",
                      click = NULL,
                      dblclick = shiny::dblclickOpts(ns("plot_overall__dblclick"),
                                                     clip = TRUE),
                      brush = shiny::brushOpts(ns("plot_overall__brush"), resetOnNew = TRUE,
                                               clip = TRUE, direction = "x")
                    )
                  )

                ),
                shidashi::flex_item(
                  size = 1, class = "overflow-hidden",

                  ravedash::output_gadget_container(
                    shiny::plotOutput(
                      outputId = ns("plot_subset"),
                      width = "100%", height = "200px"
                    )
                  )

                ),
                shidashi::flex_break()

              ),

              ravedash::flex_group_box(
                title = "Threshold results",
                class = "text-align-center",
                shidashi::flex_item(
                  flex = "5 50px",
                  DT::dataTableOutput(
                    ns("table_threshold_initial")
                  )
                ),
                shidashi::flex_item(
                  flex = "2",
                  shiny::actionButton(
                    inputId = ns("toggle_selection"),
                    "Toggle selection",
                    width = "100%"
                  ),
                  shiny::div(class = "padding-top-5"),
                  shiny::actionButton(
                    inputId = ns("clear_selection"),
                    "Clear selection",
                    width = "100%"
                  ),
                  shiny::hr(),
                  shiny::actionButton(
                    inputId = ns("save_changes"),
                    "Stage selected",
                    icon = ravedash::shiny_icons$arrow_right,
                    width = "100%"
                  ),
                  shiny::div(class = "padding-top-5"),
                  shiny::actionButton(
                    inputId = ns("undo_changes"),
                    "Unstage selected",
                    icon = ravedash::shiny_icons$arrow_left,
                    width = "100%"
                  ),
                  shiny::div(class = "padding-top-5"),
                  shiny::actionButton(
                    inputId = ns("undo_all"),
                    "Unstage all",
                    width = "100%"
                  ),
                  shiny::hr(),
                  shiny::div(
                    class = "fill-width",
                    "<Danger>"
                  ),
                  shiny::actionButton(
                    inputId = ns("discard_epoch"),
                    "Discard epoch",
                    width = "100%"
                  )
                ),
                shidashi::flex_item(
                  flex = "5 50px",
                  DT::dataTableOutput(
                    ns("table_threshold_staged")
                  )
                )
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
