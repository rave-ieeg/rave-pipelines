

module_html <- function() {

  shiny::fluidPage(
    shiny::fluidRow(

      # ---- Input column (width = 3) -------------------------------------------

      shiny::column(
        width = 3L,
        shiny::div(
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            # ---- Electrode Selector -------------------------------------------

            electrode_selector$ui_func(),

            # ---- Signal Filters card ------------------------------------------
            # Enforced order: Downsample -> FIR/IIR filters -> Baseline

            ravedash::input_card(
              title = "Signal Filters",
              class_header = "shidashi-anchor",

              # -- 1. Downsample -----------------------------------------------

              ravedash::group_box(
                title = "1. Downsample",
                class = "row",

                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::checkboxInput(
                      inputId = ns("downsample_enabled"),
                      label = "Enable downsampling",
                      value = FALSE
                    ),
                    inputId = "downsample_enabled",
                    update = "shiny::updateCheckboxInput",
                    description = "Whether to decimate the signal before filtering"
                  )
                ),

                shiny::column(
                  width = 12L,
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == true", ns("downsample_enabled")),
                    shiny::fluidRow(
                      shiny::column(
                        width = 6L,
                        shidashi::register_input(
                          shiny::numericInput(
                            inputId = ns("downsample_factor"),
                            label = "Decimation factor",
                            value = 4L, min = 1L, step = 1L
                          ),
                          inputId = "downsample_factor",
                          update = "shiny::updateNumericInput",
                          description = "Integer factor by which the signal is downsampled (e.g. 4 reduces 2000 Hz to 500 Hz)"
                        )
                      ),
                      shiny::column(
                        width = 6L,
                        shiny::uiOutput(ns("ui_nyquist_info"))
                      )
                    )
                  )
                )
              ),

              # -- 2. FIR / IIR filters ----------------------------------------

              ravedash::group_box(
                title = "2. Frequency Filters",
                class = "row",

                shiny::column(
                  width = 12L,
                  shiny::p(
                    shiny::em(
                      "Leave high-pass or low-pass blank to apply a one-sided filter.",
                      "Nyquist limit is updated automatically based on downsample factor."
                    )
                  ),
                  shidashi::register_input(
                    dipsaus::compoundInput2(
                      inputId = ns("signal_filter_configurations"),
                      label = "Filter",
                      initial_ncomp = 1L, min_ncomp = 0L, max_ncomp = 8L,
                      components = shiny::div(
                        shiny::selectInput(
                          inputId = "type",
                          label = "Method",
                          choices = c(
                            "FIR (Kaiser)"          = "fir",
                            "FIR (least squares)"   = "firls",
                            "FIR (Parks-McClellan)" = "fir_remez",
                            "IIR (Butterworth)"     = "iir",
                            "Chebyshev I"           = "cheby1",
                            "Chebyshev II"          = "cheby2",
                            "Elliptic"              = "ellip",
                            "Hilbert (amplitude)"   = "hilbert"
                          ),
                          selected = "fir"
                        ),
                        shiny::fluidRow(
                          shiny::column(
                            width = 6L,
                            shiny::numericInput(
                              inputId = "high_pass_freq",
                              label = "High-pass (Hz)",
                              value = NA, min = 0
                            )
                          ),
                          shiny::column(
                            width = 6L,
                            shiny::numericInput(
                              inputId = "low_pass_freq",
                              label = "Low-pass (Hz)",
                              value = NA, min = 0
                            )
                          )
                        ),
                        shiny::fluidRow(
                          shiny::column(
                            width = 6L,
                            shiny::numericInput(
                              inputId = "high_pass_trans_freq",
                              label = "HP transition (Hz)",
                              value = NA, min = 0
                            )
                          ),
                          shiny::column(
                            width = 6L,
                            shiny::numericInput(
                              inputId = "low_pass_trans_freq",
                              label = "LP transition (Hz)",
                              value = NA, min = 0
                            )
                          )
                        ),
                        shiny::numericInput(
                          inputId = "stopband_attenuation",
                          label = "Stopband attenuation (dB)",
                          value = 40, min = 1
                        )
                      )
                    ),
                    inputId = "signal_filter_configurations",
                    update = "dipsaus::updateCompoundInput2",
                    description = "FIR/IIR frequency filters applied after downsampling"
                  )
                )
              ),

              # -- 3. Baseline -------------------------------------------------

              ravedash::group_box(
                title = "3. Baseline",
                class = "row",

                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("baseline_method"),
                      label = "Method",
                      choices = c(
                        "None"            = "none",
                        "Detrend"         = "detrend",
                        "Center (demean)" = "demean",
                        "Detrend + Center" = "detrend+demean"
                      ),
                      selected = "detrend+demean"
                    ),
                    inputId = "baseline_method",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = "Baseline correction applied after filtering: detrend removes linear trend, center removes mean computed in the baseline window"
                  )
                ),

                shiny::column(
                  width = 12L,
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] != 'none'", ns("baseline_method")),
                    shidashi::register_input(
                      shiny::sliderInput(
                        inputId = ns("baseline_window"),
                        label = "Baseline window (seconds)",
                        min = -0.5, max = 1,
                        value = c(-0.5, 0),
                        step = 0.01
                      ),
                      inputId = "baseline_window",
                      update = "shiny::updateSliderInput",
                      description = "Time window (seconds) used for baseline correction; updated automatically when epoch data is loaded"
                    )
                  )
                )
              )

            ), # end Signal Filters card

            # ---- Condition Groups card ----------------------------------------

            ravedash::input_card(
              title = "Condition Groups",
              class_header = "shidashi-anchor",

              shiny::p(
                "Define one or more condition groups. ",
                "Each group needs a unique label and at least one trial condition."
              ),

              shidashi::register_input(
                dipsaus::compoundInput2(
                  inputId = ns("condition_groups"),
                  label = "Group",
                  initial_ncomp = 2L, min_ncomp = 1L, max_ncomp = 15L,
                  components = shiny::div(
                    shiny::textInput(
                      inputId = "label",
                      label = "Group label"
                    ),
                    shiny::selectInput(
                      inputId = "conditions",
                      label = "Conditions",
                      choices = character(0L),
                      multiple = TRUE
                    )
                  )
                ),
                inputId = "condition_groups",
                update = "dipsaus::updateCompoundInput2",
                description = "Condition groupings used for trial averaging and contrast"
              )
            ),

            # ---- Plot Options card --------------------------------------------

            ravedash::input_card(
              title = "Plot Options",
              class_header = "shidashi-anchor",

              ravedash::group_box(
                title = "Time window",
                class = "row",

                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    inputId = ns("plot_time_start"),
                    label = "Start (s)",
                    value = NA, step = 0.1
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    inputId = ns("plot_time_end"),
                    label = "End (s)",
                    value = NA, step = 0.1
                  )
                )
              ),

              ravedash::group_box(
                title = "Annotations",
                class = "row",

                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    inputId = ns("plot_onset_mark"),
                    label = "Onset mark (s)",
                    value = 0, step = 0.1
                  )
                ),
                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    inputId = ns("plot_cex"),
                    label = "Text size (cex)",
                    value = 1.6, min = 0.5, max = 3, step = 0.1
                  )
                )
              ),

              shiny::div(
                class = "rave-optional",

                ravedash::group_box(
                  title = "ERP by channel",
                  class = "row",

                  shiny::column(
                    width = 6L,
                    shiny::selectInput(
                      inputId = ns("channel_annotation"),
                      label = "Channel label style",
                      choices = c("number", "short", "label", "full"),
                      selected = "number"
                    )
                  ),
                  shiny::column(
                    width = 6L,
                    shiny::numericInput(
                      inputId = ns("erp_ncols"),
                      label = "Number of columns",
                      value = 2L, min = 1L, max = 8L, step = 1L
                    )
                  )
                ),

                ravedash::group_box(
                  title = "Trial heatmap",
                  class = "row",

                  shiny::column(
                    width = 12L,
                    shiny::selectInput(
                      inputId = ns("trial_sort_by"),
                      label = "Sort trials by",
                      choices = c("stimuli", "trial"),
                      selected = "stimuli"
                    )
                  )
                ),

                ravedash::group_box(
                  title = "Mean response",
                  class = "row",

                  shiny::column(
                    width = 6L,
                    shiny::checkboxInput(
                      inputId = ns("mean_erp_flip_y"),
                      label = "Flip y-axis",
                      value = FALSE
                    )
                  ),
                  shiny::column(
                    width = 6L,
                    shiny::checkboxInput(
                      inputId = ns("mean_erp_crp"),
                      label = "Show CRP decoration",
                      value = TRUE
                    )
                  )
                )

              ) # end rave-optional

            ) # end Plot Options card

          )
        )
      ),

      # ---- Output column (width = 9) ------------------------------------------

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,

            # ---- Output cardset: By Condition ---------------------------------
            # Plots organized by condition group
            #   - Mean Response  : figure_collapse_by_condition
            #   - ERP by Condition: figure_by_channel_condition_cond
            #   - Trial Overview : figure_by_trial_per_condition

            ravedash::output_card(
              title = "Overall plot (collapse trials and channels)",
              class_body = "no-padding fill-width min-height-450 height-450 resize-vertical",
              append_tools = FALSE,
              shiny::plotOutput(
                outputId = ns("figure_collapse_by_condition"),
                width = "100%", height = "100%"
              )
            ),

            # ERP traces stacked by electrode; one panel per condition group
            ravedash::output_cardset(
              title = "Collapse trials",
              class_body = "no-padding fill-width min-height-450",
              append_tools = FALSE,

              "By Condition" = div(
                class = "position-relative min-height-450 height-450 resize-vertical",
                shiny::plotOutput(
                  outputId = ns("figure_by_channel_condition_cond"),
                  width = "100%", height = "100%"
                )
              ),

              "By Channel" = div(
                class = "position-relative min-height-450 height-900 resize-vertical",
                shiny::plotOutput(
                  outputId = ns("figure_by_channel_condition_ch"),
                  width = "100%", height = "100%"
                )
              )

            ),

            # Time x trial heatmap; one panel per condition group
            ravedash::output_card(
              title = "Per-trial voltage response",
              class_body = "no-padding fill-width min-height-450 height-450 resize-vertical",
              append_tools = FALSE,
              shiny::plotOutput(
                outputId = ns("figure_by_trial_per_condition"),
                width = "100%", height = "100%"
              )
            )

          )
        )
      )

    )
  )
}
