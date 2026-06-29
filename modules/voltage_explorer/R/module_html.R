

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


            # ---- Signal Filters card ------------------------------------------
            # Enforced order: Detrend -> pre-Downsample -> FIR/IIR filters -> post-Downsample -> Baseline

            ravedash::input_card(
              title = "Signal Configurations",
              class_header = "shidashi-anchor",
              toggle_advanced = TRUE,


              # -- 3. FIR / IIR filters ----------------------------------------

              ravedash::group_box(
                title = "Frequency Filters",
                class = "row",

                shiny::column(
                  width = 12L,

                  shidashi::register_input(
                    shiny::checkboxInput(
                      inputId = ns("passing_filter_enabled"),
                      label = "Enable low/high/band-pass filter",
                      value = FALSE
                    ),
                    inputId = "passing_filter_enabled",
                    update = "shiny::updateCheckboxInput",
                    description = "Enable one passing filter (low-pass, high-pass, or band-pass)."
                  ),

                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] === true", ns("passing_filter_enabled")),

                    shiny::fluidRow(
                      shiny::column(
                        width = 12L,
                        shidashi::register_input(
                          shiny::selectInput(
                            inputId = ns("passing_filter_type"),
                            label = "Type",
                            choices = c(
                              "Low-pass" = "low_pass",
                              "High-pass" = "high_pass",
                              "Band-pass" = "band_pass"
                            ),
                            selected = "low_pass"
                          ),
                          inputId = "passing_filter_type",
                          update = "shiny::updateSelectInput(value=selected)",
                          description = "Passing filter type."
                        )
                      )
                    ),
                    shiny::div(
                      class = "row rave-optional soft-hidden",
                      shiny::column(
                        width = 12L,
                        shidashi::register_input(
                          shiny::selectInput(
                            inputId = ns("passing_filter_method"),
                            label = "Method",
                            choices = c(
                              "FIR (least squares)"   = "firls",
                              "FIR (Kaiser)"          = "fir",
                              "FIR (Parks-McClellan)" = "fir_remez",
                              "IIR (Butterworth)"     = "iir",
                              "Chebyshev I"           = "cheby1",
                              "Chebyshev II"          = "cheby2",
                              "Elliptic"              = "ellip"
                            ),
                            selected = "firls"
                          ),
                          inputId = "passing_filter_method",
                          update = "shiny::updateSelectInput(value=selected)",
                          description = "Passing filter design method."
                        )
                      )
                    ),

                    shiny::fluidRow(
                      shiny::column(
                        width = 6L,
                        shidashi::register_input(
                          shiny::numericInput(
                            inputId = ns("passing_freq1"),
                            label = "Cutoff freq (Hz)",
                            value = NA, min = 0
                          ),
                          inputId = "passing_freq1",
                          update = "shiny::updateNumericInput",
                          description = "For low/high-pass filter, this is the cutoff frequency; for band-pass filter, this is the high-pass cutoff."
                        )
                      ),
                      shiny::column(
                        width = 6L,
                        shiny::conditionalPanel(
                          condition = sprintf(
                            "input['%s'] === 'band_pass'",
                            ns("passing_filter_type")
                          ),
                          shidashi::register_input(
                            shiny::numericInput(
                              inputId = ns("passing_freq2"),
                              label = "",
                              value = NA, min = 0
                            ),
                            inputId = "passing_freq2",
                            update = "shiny::updateNumericInput",
                            description = "For low/high-pass filter, this is the cutoff frequency; for band-pass filter, this is the low-pass cutoff."
                          )
                        )
                      )
                    )
                  ),

                  shidashi::register_input(
                    shiny::checkboxInput(
                      inputId = ns("bandstop_filter_enabled"),
                      label = "Enable band-stop filter",
                      value = FALSE
                    ),
                    inputId = "bandstop_filter_enabled",
                    update = "shiny::updateCheckboxInput",
                    description = "Enable one band-stop (notch-like) filter."
                  ),

                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] === true", ns("bandstop_filter_enabled")),
                    shidashi::register_input(
                      shiny::textInput(
                        inputId = ns("bandstop_filter_ranges"),
                        label = "Stopband frequencies (Hz)",
                        value = "59-61, 119-121, 179-181",
                        placeholder = "59-61, 119-121, 179-181"
                      ),
                      inputId = "bandstop_filter_ranges",
                      update = "shiny::updateTextInput(value=value)",
                      description = "Comma-separated stopband ranges, e.g. 59-61, 119-121, 179-181."
                    )
                  ),

                  shiny::fluidRow(
                    shiny::column(
                      width = 12L,
                      shiny::actionButton(
                        inputId = ns("filter_inspector_btn"),
                        label = "Inspect combined filter",
                        width = "100%"
                      )
                    )
                  )
                )
              ),

              # -- 5. Baseline -------------------------------------------------

              ravedash::group_box(
                title = "Baseline",
                class = "row",

                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::checkboxInput(
                      inputId = ns("enable_baseline_method"),
                      label = "Enable baseline correction",
                      value = TRUE
                    ),
                    inputId = "enable_baseline_method",
                    update = "shiny::updateCheckboxInput(value=value)",
                    description = "Enable/Disable baseline correction after down-sampling signals"
                  )
                ),

                shiny::column(
                  width = 12L,
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] === true", ns("enable_baseline_method")),
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
              ),

              # -- 1. Detrend -----------------------------------------------

              ravedash::group_box(
                title = "Detrend",
                class = "row rave-optional soft-hidden",

                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("remove_drift_method"),
                      label = "Remove drifts",
                      choices = c(
                        "None"            = "none",
                        "Detrend"         = "detrend",
                        "Center (demean)" = "demean",
                        "Detrend + Center" = "detrend+demean"
                      ),
                      selected = "none"
                    ),
                    inputId = "detrend_method",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = "Remove linear drifting before filtering."
                  )
                )

              ),

              # -- 2. Pre-downsample -----------------------------------------------

              ravedash::group_box(
                title = "Pre-filtering down-sample",
                class = "row rave-optional soft-hidden",
                shiny::column(
                  width = 12L,

                  shidashi::register_input(
                    shiny::checkboxInput(
                      inputId = ns("pre_downsample_factor_auto"),
                      label = "Automatic down-sample",
                      value = TRUE
                    ),
                    inputId = "pre_downsample_factor_auto",
                    update = "shiny::updateCheckboxInput",
                    description = paste(
                      "Whether to automatically decimate the signals before",
                      "filters. Disable it to override."
                    )
                  ),

                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] === false", ns("pre_downsample_factor_auto")),

                    shidashi::register_input(
                      shiny::numericInput(
                        inputId = ns("pre_downsample_factor"),
                        label = "Pre-filtering decimation factor",
                        value = 1L, min = 1L, step = 1L
                      ),
                      inputId = "pre_downsample_factor",
                      update = "shiny::updateNumericInput",
                      description = paste(
                        "Integer factor by which the signal is downsampled",
                        "before signal filtering (e.g. 15 reduces 30000 Hz to",
                        "2000 Hz); set to 1 to disable."
                      )
                    )
                  ),

                  shiny::textOutput(
                    outputId = ns("pre_ui_nyquist_info"),
                    container = function(...) {
                      shiny::span(shiny::tags$small(...))
                    }
                  )
                )
              ),


              # -- 4. Post-downsample -----------------------------------------------

              ravedash::group_box(
                title = "Post-filtering down-sample",
                class = "row rave-optional soft-hidden",
                shiny::column(
                  width = 12L,

                  shidashi::register_input(
                    shiny::checkboxInput(
                      inputId = ns("post_downsample_factor_auto"),
                      label = "Automatic down-sample",
                      value = TRUE
                    ),
                    inputId = "post_downsample_factor_auto",
                    update = "shiny::updateCheckboxInput",
                    description = paste(
                      "Whether to automatically decimate the signals after",
                      "filters. Disable it to override."
                    )
                  ),


                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] === false", ns("post_downsample_factor_auto")),

                    shidashi::register_input(
                      shiny::numericInput(
                        inputId = ns("post_downsample_factor"),
                        label = "Decimation factor",
                        value = 1L, min = 1L, step = 1L
                      ),
                      inputId = "post_downsample_factor",
                      update = "shiny::updateNumericInput",
                      description = paste(
                        "Integer factor by which the signal is downsampled",
                        "after signal filtering (e.g. 4 reduces 2000 Hz to 500",
                        "Hz); set to 1 to disable."
                      )
                    )
                  ),

                  shiny::textOutput(
                    outputId = ns("post_ui_nyquist_info"),
                    container = function(...) {
                      shiny::span(shiny::tags$small(...))
                    }
                  )
                )
              ),


              footer = shiny::fluidRow(
                shiny::column(
                  width = 12L,
                  shiny::div(
                    class = "row rave-optional soft-hidden",

                    shiny::column(
                      width = 12L,
                      shiny::p(shiny::em(shiny::tags$small(
                        "Signal process includes the following steps in sequential ",
                        "order: linear drift removal, pre-filtering down-sample, ",
                        "FIR/IIR filters, post-filtering down-sample, baseline. "
                      )))
                    )
                  )
                ),

                shiny::column(
                  width = 12L,
                  shiny::actionLink(
                    inputId = ns("signal_config_reset"),
                    label = "Reset to defaults"
                  )
                )
              )



            ), # end Signal Filters card

            # ---- Electrode Selector -------------------------------------------

            electrode_selector$ui_func(),

            # ---- Condition Groups card ----------------------------------------

            ravedash::input_card(
              title = "Condition Groups",
              class_header = "shidashi-anchor",

              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns("analysis_event"),
                  label = "Analysis event",
                  choices = "Trial Onset"
                ),
                inputId = "analysis_event",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "Align the trials based on events after signal filtering ",
                  "and baseline so the event onset becomes the time zero."
                )
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

            # ---- CRP Parameters card ------------------------------------------

            ravedash::input_card(
              title = "CRP Parameters",
              class_header = "shidashi-anchor",

              ravedash::group_box(
                title = "Detection window",
                class = "row",

                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::sliderInput(
                      inputId = ns("crp_detection_window"),
                      label = "Response window (seconds)",
                      min = 0, max = 1,
                      value = c(0.01, 1),
                      step = 0.01
                    ),
                    inputId = "crp_detection_window",
                    update = "shiny::updateSliderInput",
                    description = paste(
                      "Post-stimulus window [t_start, t_end] within which the",
                      "canonical response duration is estimated; the evoked",
                      "response must start after t_start and end before t_end.",
                      "Bounds are set automatically when epoch data is loaded."
                    )
                  )
                )
              ),

              ravedash::group_box(
                title = "Advanced",
                class = "row",

                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("crp_onset_border"),
                      label = "Onset detection border",
                      choices = c(
                        "Earliest possible"        = "earliest_possible",
                        "Event onset (0 s)"        = "event_onset",
                        "Detection start (t_start)" = "t_start",
                        "Disabled (no onset)"      = "disabled"
                      ),
                      selected = "earliest_possible"
                    ),
                    inputId = "crp_onset_border",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "Earliest time the onset scan may reach. 'Disabled' skips",
                      "onset estimation entirely."
                    )
                  )
                ),

                shiny::column(
                  width = 6L,
                  shidashi::register_input(
                    shiny::numericInput(
                      inputId = ns("crp_time_step"),
                      label = "step",
                      value = 5L, min = 1L, step = 1L
                    ),
                    inputId = "crp_time_step",
                    update = "shiny::updateNumericInput",
                    description = paste(
                      "Sampling step (in samples) used when sweeping candidate",
                      "response durations. Larger is faster but smoother."
                    )
                  )
                ),

                shiny::column(
                  width = 6L,
                  shidashi::register_input(
                    shiny::numericInput(
                      inputId = ns("crp_threshold_quantile"),
                      label = "Threshold (%)",
                      value = 98, min = 1, max = 100, step = 1
                    ),
                    inputId = "crp_threshold_quantile",
                    update = "shiny::updateNumericInput",
                    description = paste(
                      "Fraction (percent) of the peak mean projection magnitude",
                      "used to derive the duration-uncertainty bounds."
                    )
                  )
                )
              ),

              ravedash::group_box(
                title = "Channel filter",
                class = "row",

                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    dipsaus::compoundInput2(
                      inputId = ns("crp_channel_filter"),
                      label = "Filter",
                      initial_ncomp = 1L, min_ncomp = 0L, max_ncomp = 10L,
                      components = shiny::fluidRow(
                        shiny::column(
                          width = 8L,
                          shiny::selectInput(
                            inputId = "name",
                            label = NULL,
                            choices = character(0L)
                          )
                        ),
                        shiny::column(
                          width = 4L,
                          shiny::div(
                            class = ns("compound-filter"),
                            shiny::selectInput(
                              inputId = "operator",
                              label = NULL,
                              choices = c("AND" = "and", "OR" = "or"),
                              selected = "or"
                            )
                          )
                        ),
                        shiny::column(
                          width = 8L,
                          shiny::selectInput(
                            inputId = "criteria",
                            label = NULL,
                            choices = c(
                              "v = T1"           = "eq",
                              "|v| < T1"         = "abs_lt",
                              "|v| >= T1"        = "abs_gte",
                              "v < T1"           = "lt",
                              "v >= T1"          = "gte",
                              "v in [T1, T2]"    = "in",
                              "v not in [T1, T2]" = "not_in"
                            ),
                            selected = "abs_gte"
                          )
                        ),
                        shiny::column(
                          width = 4L,
                          shiny::textInput(
                            inputId = "threshold",
                            label = NULL,
                            value = "",
                            placeholder = "T1 or T1, T2"
                          )
                        )
                      )
                    ),
                    inputId = "crp_channel_filter",
                    update = "dipsaus::updateCompoundInput2",
                    description = paste(
                      "Restrict the CRP-by-channel plots to electrodes whose CRP",
                      "metrics satisfy the combined filters. Operators apply",
                      "left-to-right (the first is ignored), e.g. c1 AND c2 OR c3",
                      "= (c1 & c2) | c3. No rows shows all channels."
                    )
                  )
                ),

                shiny::column(
                  width = 12L,
                  shiny::actionButton(
                    inputId = ns("crp_filter_apply"),
                    label = "Update visualization",
                    width = "100%"
                  )
                )
              ),

              footer = shiny::div(
                shiny::actionLink(
                  inputId = ns("crp_params_reset"),
                  label = "Reset to defaults"
                )
              )

            ), # end CRP Parameters card

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

                # shiny::column(
                #   width = 12L,
                #   shiny::checkboxInput(
                #     inputId = ns("mean_erp_flip_y"),
                #     label = "Flip y-axis",
                #     value = FALSE
                #   )
                # )
              ),

              ravedash::group_box(
                title = "Plot max / spacing",
                class = "row",

                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    inputId = ns("plot_space_value"),
                    label = "Max",
                    value = use_plot_space(), min = 0, step = 1
                  )
                ),
                shiny::column(
                  width = 6L,
                  style = "margin-top: 37px;",
                  shiny::checkboxInput(
                    inputId = ns("plot_space_is_percentile"),
                    label = "Max is %",
                    value = use_plot_space_is_percentile()
                  )
                )
              ),

              ravedash::group_box(
                title = "Annotations",
                class = "row",

                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    inputId = ns("plot_cex"),
                    label = "Text size (cex)",
                    value = use_cex(), min = 0.5, max = 3, step = 0.1
                  )
                ),

                shiny::column(
                  width = 6L,
                  shiny::selectInput(
                    inputId = ns("channel_annotation"),
                    label = "Channel",
                    choices = OPTIONS_CHAN_ANNOT,
                    selected = use_channel_annotation_style()
                  )
                ),

                shiny::column(
                  width = 6L,
                  shiny::selectInput(
                    inputId = ns("trial_sort_by"),
                    label = "Sort trials by",
                    choices = OPTIONS_TRIAL_SORT,
                    selected = use_trial_sort_by()
                  )
                ),

                shiny::column(
                  width = 6L,
                  shiny::numericInput(
                    inputId = ns("plot_onset_mark"),
                    label = "Onset mark (s)",
                    value = 0, step = 0.01
                  )
                ),

                shiny::column(
                  width = 12L,
                  shiny::checkboxInput(
                    inputId = ns("mean_erp_crp"),
                    label = "Show CRP decoration",
                    value = TRUE
                  )
                )

              ),

              footer = shiny::div(
                shiny::actionLink(
                  inputId = ns("plot_options_reset"),
                  label = "Reset to defaults"
                )
              )

            ), # end Plot Options card

            # ---- Export Configurations card -----------------------------------

            ravedash::input_card(
              title = "Export Configurations",
              class_header = "shidashi-anchor",

              shiny::p("Generate a standalone HTML voltage analysis report in the background."),
              shiny::actionButton(
                inputId = ns("open_report_modal"),
                label = "Generate Report",
                class = "btn-primary",
                icon = ravedash::shiny_icons$save
              )

            ) # end Export Configurations card

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

            ravedash::output_cardset(
              title = "Brain Viewers",
              class_body = "no-padding fill-width min-height-450 height-450 resize-vertical",
              append_tools = FALSE,

              "Results Viewer" = div(
                class = "position-relative fill",
                threeBrain::threejsBrainOutput(
                  outputId = ns("brain_viewer"),
                  width = "100%", height = "100%"
                )
              )

            ),

            # ERP traces stacked by electrode
            ravedash::output_cardset(
              title = "By Electrode",
              class_body = "no-padding fill-width min-height-450 height-550 resize-vertical",
              append_tools = FALSE,


              "Canonical (Lines)" = div(
                class = "position-relative fill",
                shiny::plotOutput(
                  outputId = ns("figure_crp_by_channel"),
                  width = "100%", height = "100%"
                )
              ),
              "Canonical (Heatmap)" = div(
                class = "position-relative fill",
                shiny::plotOutput(
                  outputId = ns("figure_crp_by_channel_heatmap"),
                  width = "100%", height = "100%"
                )
              ),


              "Analysis Electrode Over Time" = div(
                class = "position-relative fill",
                shiny::plotOutput(
                  outputId = ns("figure_by_channel_condition_ch"),
                  width = "100%", height = "100%"
                )
              ),

              "Per Condition" = div(
                class = "position-relative fill",
                shiny::plotOutput(
                  outputId = ns("figure_by_channel_condition_cond"),
                  width = "100%", height = "100%"
                )
              )

            ),


            ravedash::output_cardset(
              title = "By Condition (Collapse Channels)",
              class_body = "no-padding fill-width min-height-450 height-450 resize-vertical",
              append_tools = FALSE,

              "Over Time" = div(
                class = "position-relative fill",
                shiny::plotOutput(
                  outputId = ns("figure_by_condition_over_time"),
                  width = "100%", height = "100%"
                )
              ),
              "By Trial (Lines)" = div(
                class = "position-relative fill",
                shiny::plotOutput(
                  outputId = ns("figure_by_trial_per_condition"),
                  width = "100%", height = "100%"
                )
              ),
              "By Trial (Heatmap)" = div(
                class = "position-relative fill",
                shiny::plotOutput(
                  outputId = ns("figure_by_trial_per_condition_heatmap"),
                  width = "100%", height = "100%"
                )
              )

            )




          )
        )
      )

    )
  )
}
