

module_html <- function(){

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 4L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Wavelet settings",
              ravedash::flex_group_box(
                title = "Basic configurations",
                shidashi::flex_item(
                  shiny::numericInput(
                    inputId = ns("target_sample_rate"),
                    label = "Power sample rate (Hz):",
                    value = 100,
                    min = 1
                  )
                ),
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("pre_downsample"),
                    label = "Down-sample before wavelet",
                    choices = "1"
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shiny::checkboxInput(
                    inputId = ns("precision"),
                    label = "Use single float precision to speed up",
                    value = FALSE
                  )
                )
              ),

              ravedash::flex_group_box(
                title = "Frequency & cycle",
                shidashi::flex_item(
                  shiny::selectInput(
                    inputId = ns("use_preset"),
                    label = "Select method to generate wavelet parameters",
                    choices = c("Builtin tool", "Upload preset"),
                    selected = "Builtin tool"
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(

                  shiny::conditionalPanel(
                    condition = sprintf("input['%s']==='Upload preset'", ns("use_preset")),
                    shiny::fileInput(
                      inputId = ns("preset_upload"),
                      label = "Upload",
                      multiple = FALSE,
                      accept = ".csv",
                      width = "100%",
                      placeholder = "No preset uploaded"
                    )
                  ),

                  shiny::conditionalPanel(
                    condition = sprintf("input['%s']==='Builtin tool'", ns("use_preset")),
                    shiny::sliderInput(
                      inputId = ns("freq_range"),
                      label = "Frequency range",
                      min = 1,
                      max = 1000,
                      value = c(2, 200),
                      step = 1
                    ),
                    shiny::sliderInput(
                      inputId = ns("freq_step"),
                      label = "Frequency step size",
                      min = 1,
                      max = 40,
                      value = 2,
                      step = 1
                    ),
                    shiny::sliderInput(
                      inputId = ns("cycle_range"),
                      label = "Wavelet cycles",
                      min = 1,
                      max = 40,
                      value = c(3, 20),
                      step = 1
                    )
                  )

                )
              ),

              footer = shiny::tagList(
                dipsaus::actionButtonStyled(
                  inputId = ns("wavelet_do_btn"),
                  label = "Run wavelet",
                  width = "100%"
                )
              )

            )

          )
        )
      ),

      shiny::column(
        width = 8L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper overflow-x-hidden",
          shiny::column(
            width = 12L,
            ravedash::output_card(
              'Wavelet kernel',
              class_body = "no-padding fill-width",
              tools = shidashi::card_tool(
                inputId = ns("kernel_flip_btn"),
                widget = "flip",
                icon = shiny_icons$table
              ),
              shidashi::flip_box(
                inputId = ns("kernel_flip_container"),
                front = shiny::div(
                  title = "Double-click to see settings table",
                  class = "fill height-700 min-height-450 resize-vertical",
                  shiny::div(
                    class = 'position-relative fill',
                    ravedash::output_gadget_container(
                      shiny::plotOutput(ns("kernel_plot"),
                                        width = '100%', height = "100%")
                    )
                  )
                ),
                back = shiny::div(
                  class = "padding-7 bg-white",
                  title = "Double-click again to view the kernel figure",
                  ravedash::output_gadget_container(
                    DT::dataTableOutput(ns("kernel_table"), width = "100%")
                  )
                )
              ),

              footer = shiny::tagList(
                shiny::downloadLink(ns("download_kernel_table"),
                                    "Download kernel parameters")
              )

            )
          )
        )
      )

    )
  )
}
