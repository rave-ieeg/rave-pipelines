

module_html <- function() {

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
                  shidashi::register_input(
                    shiny::numericInput(
                      inputId = ns("target_sample_rate"),
                      label = "Power sample rate (Hz):",
                      value = 100,
                      min = 1
                    ),
                    tooltip = "Sample rate (Hz) of the saved power and phase.",
                    inputId = "target_sample_rate",
                    update = "shiny::updateNumericInput",
                    description = paste(
                      "[Numeric, Hz] The sample rate to which the wavelet power/phase coefficients are down-sampled after wavelet computation but before saving.",
                      "Must be greater than 1. Default `100` (recommended). Read by script `run_analysis`."
                    )
                  )
                ),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("pre_downsample"),
                      label = "Down-sample before wavelet",
                      choices = "1"
                    ),
                    tooltip = "Down-sample the voltage by this factor before the wavelet; this lowers the Nyquist frequency.",
                    inputId = "pre_downsample",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select, integer as string] Down-sample factor applied to the voltage signal BEFORE the wavelet (will affect Nyquist frequency).",
                      "Choices are powers of two computed from the subject sample rate; `1` means no pre-down-sample.",
                      "The available choices only appear after data are loaded. Read by script `run_analysis`."
                    )
                  )
                ),
                shidashi::flex_break(),
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::checkboxInput(
                      inputId = ns("precision"),
                      label = "Use single float precision to speed up",
                      value = FALSE
                    ),
                    tooltip = "Compute the wavelet in single (float) precision, which is faster; unchecked uses double precision.",
                    inputId = "precision",
                    update = "shiny::updateCheckboxInput",
                    description = paste(
                      "[Boolean] When `true`, the wavelet is computed in single (float) precision, which is faster;",
                      "when `false` (default) it uses double precision. Read by script `run_analysis`."
                    )
                  )
                )
              ),

              ravedash::flex_group_box(
                title = "Frequency & cycle",
                shidashi::flex_item(
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("use_preset"),
                      label = "Select method to generate wavelet parameters",
                      choices = c("Builtin tool", "Upload preset"),
                      selected = "Builtin tool"
                    ),
                    tooltip = "Generate the wavelet frequencies and cycles with the built-in tool, or upload a preset table.",
                    inputId = "use_preset",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select] How the wavelet kernel table (Frequency + Cycles) is generated: `Builtin tool`",
                      "(default; use `freq_range`, `freq_step`, `cycle_range`) or `Upload preset` (needs a CSV upload,",
                      "which agents cannot do). Agents should keep this at `Builtin tool`."
                    )
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
                    shidashi::register_input(
                      shiny::sliderInput(
                        inputId = ns("freq_range"),
                        label = "Frequency range",
                        min = 1,
                        max = 1000,
                        value = c(2, 200),
                        step = 1
                      ),
                      tooltip = "Lowest and highest wavelet frequency (Hz).",
                      inputId = "freq_range",
                      update = "shiny::updateSliderInput",
                      description = paste(
                        "[Numeric length of two, Hz] Lower and upper frequency of the built-in kernel table, as `[low, high]`.",
                        "Default `[2, 200]`. Its upper limit is capped by the subject sample rate / pre-down-sample.",
                        "Used only when `use_preset` = `Builtin tool`."
                      )
                    ),
                    shidashi::register_input(
                      shiny::sliderInput(
                        inputId = ns("freq_step"),
                        label = "Frequency step size",
                        min = 1,
                        max = 40,
                        value = 2,
                        step = 1
                      ),
                      tooltip = "Spacing between the wavelet frequencies (Hz).",
                      inputId = "freq_step",
                      update = "shiny::updateSliderInput",
                      description = paste(
                        "[Numeric, Hz] Spacing between consecutive frequencies in the built-in kernel table. Default `2`.",
                        "For example, freq_range = `[2, 200]` and freq_step = `2` gives wavelet frequencies `2, 4, 6, ..., 200`.",
                        "Used only when `use_preset` = `Builtin tool`."
                      )
                    ),
                    shidashi::register_input(
                      shiny::sliderInput(
                        inputId = ns("cycle_range"),
                        label = "Wavelet cycles",
                        min = 1,
                        max = 40,
                        value = c(3, 20),
                        step = 1
                      ),
                      tooltip = "Number of wavelet cycles at the lowest and the highest frequency.",
                      inputId = "cycle_range",
                      update = "shiny::updateSliderInput",
                      description = paste(
                        "[Integer length-2] Number of Morlet wavelet cycles at the lowest and highest frequency, as `[low, high]`.",
                        "In between, log(cycles) is interpolated linearly in log(frequency) and rounded. Default `[3, 20]`.",
                        "The pipeline rejects any cycle count of 1 or less, so the lower value must be at least 2.",
                        "Used only when `use_preset` = `Builtin tool`."
                      )
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
              "Wavelet kernel",
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
                    class = "position-relative fill",
                    # MIGRATED: removed ravedash::output_gadget_container() wrapper
                    # ravedash::output_gadget_container(
                      shiny::plotOutput(ns("kernel_plot"),
                                        width = "100%", height = "100%")
                    # )
                  )
                ),
                back = shiny::div(
                  class = "padding-7 bg-white",
                  title = "Double-click again to view the kernel figure",
                  # MIGRATED: removed ravedash::output_gadget_container() wrapper
                  # ravedash::output_gadget_container(
                    DT::dataTableOutput(ns("kernel_table"), width = "100%")
                  # )
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
