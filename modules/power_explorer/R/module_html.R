
module_html <- function(){

  line_palettes <- get_line_palette(get_palette_names = TRUE)
  heatmap_palettes <- get_heatmap_palette(get_palette_names = TRUE)

  shiny::fluidPage(shiny::fluidRow(

    # ---- Input tabs (width = 3) -------------------------------

    shiny::column(
      width = 3L,
      shiny::div(
        # class = "row fancy-scroll-y stretch-inner-height",
        class = "row screen-height overflow-y-scroll",
        shiny::column(
          width = 12L,

          # ---- Input tab: Electrode selector -------------------------------

          electrode_selector$ui_func(),

          # ---- Input tab: Custom ROI ---------------------------------------

          ravedash::input_card(
            class_header = "shidashi-anchor",
            title = shidashi::register_input(
              shiny::checkboxInput(ns('enable_custom_ROI'), 'Custom ROI', value = FALSE),
              inputId = "enable_custom_ROI",
              update = "shiny::updateCheckboxInput",
              description = paste(
                "Custom ROI on/off. When on and `custom_roi_variable` is not",
                "'none', `run_analysis` analyzes the loaded electrodes whose ROI",
                "value is in `custom_roi_groupings`, instead of `electrode_text`.",
                "Set it and `custom_roi_variable` before `custom_roi_groupings`,",
                "whose choices load then."
              )
            ),
            shiny::conditionalPanel(
              condition = 'input["power_explorer-enable_custom_ROI"] == 1',
              shiny::p('ROI variables are read from electrodes.csv and are based on currently available electrodes (not currently selected electrodes).',
              shiny::strong("Choices here will override selected electrode(s) above."), "Any changes require re-RAVE! to update."),
              #You must click the Cluster -> ROI button after each RAVE if you want to update the cluster table"),
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns('custom_roi_variable'), label='ROI Variable',
                  selected = character(0), choices=character(0)),
                inputId = "custom_roi_variable",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "ROI variable: 'none', or a column of the subject's",
                  "electrodes.csv with 2 to n-1 distinct values among the loaded",
                  "electrodes (e.g. `FSLabel`); `PE_Cluster` appears after script",
                  "`cluster_to_roi`. Changing it loads the category choices of",
                  "`custom_roi_groupings`."
                )
              ),
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns('custom_roi_type'), label = 'How to use ROI',
                  selected = 'Filter only',
                  choices = c('Filter only',
                              'Group/Stratify results',
                              'Interaction model')),
                inputId = "custom_roi_type",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "How to use the custom ROI: 'Filter only' (default) only limits",
                  "the electrodes. 'Group/Stratify results' and 'Interaction",
                  "model' (treated the same by the pipeline) also split each",
                  "analysis window by ROI group, labelled '<ROI>_<window>', and",
                  "add the ROI as a factor of the across-electrode model."
                )
              ),
              shidashi::register_input(
                shiny::actionButton(ns('auto_assign_levels_to_roi_groupings'),
                                    'Assign all ROI levels to groups', icon = ravedash::shiny_icons$magic),
                inputId = "auto_assign_levels_to_roi_groupings",
                update = "shiny::updateActionButton",
                description = paste(
                  "Button 'Assign all ROI levels to groups': one ROI group per",
                  "value of `custom_roi_variable`. Agents run script",
                  "`assign_roi_levels` instead."
                ),
                writable = FALSE
              ),
              shidashi::register_input(
                shiny::actionButton(ns('clear_roi_grouping_levels'),
                                    'Clear groups', icon = ravedash::shiny_icons$trash),
                inputId = "clear_roi_grouping_levels",
                update = "shiny::updateActionButton",
                description = paste(
                  "Button 'Clear groups': one ROI group 'All levels' with every",
                  "value of `custom_roi_variable`. Agents run script",
                  "`clear_roi_groups` instead."
                ),
                writable = FALSE
              ),
              shiny::HTML("<p>&nbsp;</p>"),
              shidashi::register_input(
                dipsaus::compoundInput2(
                  inputId = ns('custom_roi_groupings'),
                  label = "ROI Group", initial_ncomp = 1L, min_ncomp = 1L,
                  max_ncomp = 15L,
                  components = shiny::div(
                    shiny::textInput(inputId = "label", label = "Label"),
                    shiny::selectInput(inputId = "conditions", label = "Categories", choices = "",
                                       multiple = TRUE))
                ),
                inputId = "custom_roi_groupings",
                update = "dipsaus::updateCompoundInput2",
                description = paste(
                  "ROI groups, as a JSON array of 1 to 15 objects, e.g.",
                  "[{\"label\":\"STG\",\"conditions\":[\"ctx_lh_G_temp_sup-Lateral\"]}];",
                  "`conditions` are values of `custom_roi_variable`. Set it after",
                  "`enable_custom_ROI` and `custom_roi_variable`, then check the",
                  "stored value with `shiny_input_info`."
                )
              )
            )
          ),


          # ---- Input tab: Save/Load Settings -----------------------------------------

          ravedash::input_card(
            title = "Save/Load Settings",class_header = "shidashi-anchor",

            shidashi::register_input(
              shiny::fileInput(inputId = ns("file_load_settings"), "Load Settings",
                               multiple = FALSE, accept = c('.yaml')),
              inputId = "file_load_settings",
              update = "shiny::updateActionButton",
              description = paste(
                "File upload 'Load Settings': a YAML file saved with 'Save",
                "settings'. Only people can upload files; agents set the inputs",
                "directly instead."
              ),
              writable = FALSE
            ),
            shiny::hr(),
            shiny::p("Settings are updated after an analysis has been run (press RAVE!)"),

            shiny::downloadButton(
              outputId = ns("btn_save_settings"),
              label = "Save settings",
            )
          ),

          # ---- Input tab: Baseline -----------------------------------------

          ravedash::input_card(
            title = "Baseline",class_header = "shidashi-anchor",

            shidashi::register_input(
              shiny::checkboxInput(inputId=ns('omnibus_includes_all_electrodes'),
                                   'Baseline unselected electrodes (for inclusion in stats/3dViewer, but takes time)', value=FALSE
              ),
              inputId = "omnibus_includes_all_electrodes",
              update = "shiny::updateCheckboxInput",
              description = "Whether to baseline all loaded electrodes (not just selected) for inclusion in stats and 3D viewer"
            ),
            shidashi::register_input(
              shiny::sliderInput(
                inputId = ns("baseline_window"), "Window", value = c(0,1),
                min =0, max=1, step = 0.01, dragRange = TRUE),
              inputId = "baseline_window",
              update = "shiny::updateSliderInput",
              description = paste(
                "Baseline window [start, end] in seconds, relative to each",
                "trial's anchor event, e.g. [-1, 0]. Its range is the loaded",
                "trial window."
              )
            ),
            shidashi::register_input(
              shiny::selectInput(
                inputId = ns("baseline_scope"), label = 'Baseline Scope',
                selected=get_baseline_scope(names=TRUE)[1],
                choices =get_baseline_scope(names=TRUE)),
              inputId = "baseline_scope",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Baseline scope: 'Per frequency, trial, and electrode' (default),",
                "'Across trials (aka global baseline)', 'Across trials and",
                "electrodes', or 'Across electrodes only'."
              )
            ),
            shidashi::register_input(
              shiny::selectInput(
                inputId = ns("baseline_unit"), label = 'Unit of Analysis',
                selected=get_unit_of_analysis(names=TRUE)[1],
                choices=get_unit_of_analysis(names=TRUE)),
              inputId = "baseline_unit",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Unit of analysis (baseline correction): '% Change Power'",
                "(default), '% Change Amplitude', 'Decibel', 'z-score Power',",
                "'z-score Amplitude', or 'z-score Decibel'."
              )
            )
          ),

          # ---- Input tab: Analysis window ----------------------------------

          ravedash::input_card(
            class_header = "shidashi-anchor",
            title = "Analysis Windows",

            shidashi::register_input(
              shiny::checkboxInput(
                inputId=ns('quick_omnibus_only'), 'Just get univariate stats + 3dViewer (fast)', value=FALSE
              ),
              inputId = "quick_omnibus_only",
              update = "shiny::updateCheckboxInput",
              description = paste(
                "Quick mode. TRUE: `run_analysis` computes only the per-electrode",
                "statistics and the electrode-by-time data (faster), which refresh",
                "the 'By Electrode' card (except 'Waterfall over Time') and the 3D",
                "viewer; the other plots are not refreshed. FALSE (default):",
                "everything."
              )
            ),

            shidashi::register_input(
              dipsaus::compoundInput2(
                inputId = ns('ui_analysis_settings'),
                label = "Analysis Window",
                initial_ncomp = 1L, min_ncomp = 1L, max_ncomp = 5L,
                components = shiny::div(
                  shiny::textInput(inputId = "label", label = "Label"),
                  shiny::selectInput(
                    inputId = "event",
                    label = "Event",
                    choices = NULL,
                    selected = NULL
                  ),
                  shiny::sliderInput(
                    inputId = "time",
                    label = "Time",
                    min = 0,
                    max = 1,
                    value = c(0, 1),
                    step = 1 / 100,
                    dragRange = TRUE
                  ),
                  shiny::selectInput('frequency_dd', 'Choose preset band',
                                     choices=c('delta (1-4)', 'theta (4-8)', 'alpha (8-12)', 'beta (13-30)',
                                               'gamma (30-70)', 'high gamma (70-150)', 'Select one'), selected='Select one'),
                  shiny::sliderInput(
                    inputId = "frequency",
                    label = "Frequency",
                    min = 0,
                    max = 200,
                    value = c(70, 150),
                    step = 1
                  )
                )
              ),
              inputId = "ui_analysis_settings",
              update = "dipsaus::updateCompoundInput2",
              description = paste(
                "Analysis windows, as a JSON array of 1 to 5 objects, e.g.",
                "[{\"label\":\"HighGamma\",\"event\":\"Trial Onset\",\"time\":[0,1],",
                "\"frequency_dd\":\"Select one\",\"frequency\":[70,150]}]. `event`:",
                "'Trial Onset' or an event of the epoch; `time` in seconds;",
                "`frequency` in Hz. A `frequency_dd` preset other than 'Select one'",
                "(e.g. 'high gamma (70-150)') is copied into `frequency`, then reset.",
                "Windows with the same event and nearly the same time and frequency",
                "ranges stop `run_analysis`."
              )
            )
          ),


          # ---- Input tab: First Trial Factor -------------------------------

          ravedash::input_card(
            class_header = "shidashi-anchor",
            title = "First Trial Factor",
            shidashi::register_input(
              shiny::selectInput(inputId=ns('condition_variable'),
                                 "Condition Variable", choices=c('Condition')),
              inputId = "condition_variable",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Epoch column that holds the trial conditions (the columns whose",
                "name contains 'Condition'; default 'Condition'). Changing it",
                "resets `first_condition_groupings` to one group 'All",
                "Conditions', so set it first."
              )
            ),
            shidashi::register_input(
              dipsaus::compoundInput2(
                inputId = ns('first_condition_groupings'),
                label = "Trial Group", initial_ncomp = 1L, min_ncomp = 1L,
                max_ncomp = 15L,
                components = shiny::div(
                  shiny::textInput(inputId = "label", label = "Label"),
                  shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE))),
              inputId = "first_condition_groupings",
              update = "dipsaus::updateCompoundInput2",
              description = paste(
                "Levels of the first trial factor, as a JSON array of 1 to 15",
                "objects, e.g. [{\"label\":\"Auditory\",\"conditions\":[\"drive_a\",",
                "\"known_a\"]},{\"label\":\"AudioVisual\",\"conditions\":[\"drive_av\"]}].",
                "`conditions` are values of `condition_variable` (script",
                "`load_data` lists them). A condition in two levels is kept only in",
                "the first; a level left empty stops `run_analysis`."
              )
            )
          ),



          # ---- Input tab: Second Trial Factor ------------------------------

          ravedash::input_card(
            class_header = "shidashi-anchor",
            title = shidashi::register_input(
              shiny::checkboxInput(ns('enable_second_condition_groupings'), 'Second Trial Factor', value = FALSE),
              inputId = "enable_second_condition_groupings",
              update = "shiny::updateCheckboxInput",
              description = paste(
                "Second trial factor on/off (default off). Turn it on before",
                "setting `second_condition_groupings`."
              )
            ),
            shiny::conditionalPanel(
              condition = 'input["power_explorer-enable_second_condition_groupings"] == 1',
              shiny::p("To create a second factor, specify at least 2 levels and ensure all available conditions are assigned to exactly 1 level."),
              shidashi::register_input(
                dipsaus::compoundInput2(
                  inputId = ns('second_condition_groupings'),
                  label = "Trial Group", initial_ncomp = 2L, min_ncomp = 2L,
                  max_ncomp = 15L,
                  components = shiny::div(
                    shiny::textInput(inputId = "label", label = "Label"),
                    shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE))
                ),
                inputId = "second_condition_groupings",
                update = "dipsaus::updateCompoundInput2",
                description = paste(
                  "Levels of the second trial factor (with",
                  "`enable_second_condition_groupings` on), as a JSON array of 2 to",
                  "15 objects shaped like `first_condition_groupings`. The choices",
                  "are the conditions used in the first factor; put each in exactly",
                  "one level. Changing the first factor drops the conditions it no",
                  "longer uses."
                )
              )
            )
          ),

          # ---- Input tab: Plot Options -------------------------------------

          ravedash::input_card(
            class_header='shidashi-anchor', title='Global plot options',
            shidashi::register_input(
              shiny::checkboxInput(ns('do_over_time_by_electrode_dataframe'), 'Calculate electrode over time (movie maker)', value=FALSE),
              inputId = "do_over_time_by_electrode_dataframe",
              update = "shiny::updateCheckboxInput",
              description = paste(
                "TRUE: a full `run_analysis` also computes the electrode-by-time",
                "table for the 'Movie Maker' 3D viewer (`brain_viewer_movies`),",
                "which takes longer. Default FALSE."
              )
            ),
            shidashi::register_input(
              shiny::selectInput(ns("gpo_lines_palette"), "Lines/Points palette",
                                 choices = line_palettes,
                                 selected = pe_graphics_settings_cache$get('line_color_palette') %OF% line_palettes),
              inputId = "gpo_lines_palette",
              update = "shiny::updateSelectInput(value=selected)",
              description = "Color palette for lines and points in plots"
            ),
            shidashi::register_input(
              shiny::selectInput(ns("gpo_heatmap_palette"), "Heatmap palette",
                                 choices = heatmap_palettes,
                                 selected = pe_graphics_settings_cache$get('heatmap_color_palette') %OF% heatmap_palettes
              ),
              inputId = "gpo_heatmap_palette",
              update = "shiny::updateSelectInput(value=selected)",
              description = "Color palette for heatmap plots"
            )
          ),
          # ---- Input tab: Save for group analysis --------------------------------

          ravedash::input_card(
            class_header = "shidashi-anchor", title = "Save for Group Analysis",
            shiny::p("Data from currently-selected electrodes will be saved on the RAVE server in the subject's directory.", shiny::br(),
                     "Be sure to provide a unique label (timestamps are automatic). ",
                     "If you select Create New, but reuse an existing label, the original export will not be deleted, "
                     , "but it may become inaccessible from within RAVE."),
            shiny::p("Using the same label across multiple subjects will greatly facilitate group analysis"),
            shidashi::register_input(
              shiny::selectInput(ns("replace_existing_group_anlysis_pipeline"), label = 'Create new / Replace existing pipeline',
                                 choices = 'Create New'),
              inputId = "replace_existing_group_anlysis_pipeline",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "'Create New' (default), or the label of results saved earlier for",
                "this subject; choosing a label copies it into",
                "`save_pipeline_for_group_analysis_label`. Anything other than",
                "'Create New' asks the save to delete the older saves with the",
                "same label; with the current ravecore they are kept (see the",
                "manual). Confirm with the user first."
              )
            ),
            shidashi::register_input(
              shiny::textInput(
                inputId = ns("save_pipeline_for_group_analysis_label"),
                label = "Label for results (spaces/special chars will be replaced)",
                placeholder = "compare_A_and_B_pct_signal_change"
              ),
              inputId = "save_pipeline_for_group_analysis_label",
              update = "shiny::updateTextInput",
              description = paste(
                "Label of the results saved for group analysis (required). In the",
                "folder name, characters other than letters, digits, '_', '.' and",
                "'-' become '_'. The same label across subjects eases group",
                "analysis."
              )
            ),

            shidashi::register_input(
              dipsaus::actionButtonStyled(ns('save_pipeline_for_group_analysis'), 'Save!', icon = ravedash::shiny_icons$save),
              inputId = "save_pipeline_for_group_analysis",
              update = "shiny::updateActionButton",
              description = paste(
                "Button 'Save!': saves the current results for group analysis",
                "into the subject. Agents run script `save_for_group_analysis`",
                "instead, after confirming the label with the user."
              ),
              writable = FALSE
            )
          ),

          # ---- Input tab: Export Electrodes --------------------------------

          ravedash::input_card(
            class_header = "shidashi-anchor", title = "Export electrodes to csv",
            shiny::p("All exported data are baseline corrected according to ",
                     "the current analysis settings.",
                     "Use the options below to fine-tune the export."),
            shidashi::register_input(
              shiny::textInput(
                inputId = ns("electrodes_to_export"),
                label = "Electrodes to export",
                placeholder = "1-20,80-100"
              ),
              inputId = "electrodes_to_export",
              update = "shiny::updateTextInput",
              description = paste(
                "Electrodes to export, e.g. `14-15` or `1-20,80-100`; only loaded",
                "electrodes count. The export re-runs the analysis on them."
              )
            ),
            shidashi::register_input(
              shiny::selectInput(
                inputId = ns("electrodes_to_export_roi_name"),
                label = "Add ROI filter",
                choices = c('none', 'anything else')
              ),
              inputId = "electrodes_to_export_roi_name",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Optional ROI filter of the export: 'none' (default) or a column",
                "of electrodes.csv; then only the electrodes whose value is in",
                "`electrodes_to_export_roi_categories` are exported."
              )
            ),
            shiny::conditionalPanel(
              condition = "input.electrodes_to_export_roi_name != 'none'",
              ns = ns,
              shidashi::register_input(
                shiny::selectInput(
                  inputId = ns("electrodes_to_export_roi_categories"),
                  multiple = TRUE,
                  label = "Categories to include in export",
                  choices = 'unknown'
                ),
                inputId = "electrodes_to_export_roi_categories",
                update = "shiny::updateSelectInput(value=selected)",
                description = paste(
                  "Values of `electrodes_to_export_roi_name` to export; all are",
                  "selected when the filter column changes."
                )
              )
            ),
            shidashi::register_input(
              shiny::selectInput(
                inputId = ns("frequencies_to_export"),
                label = "How to export frequency",
                choices = c(
                  'Collapsed, Analysis window(s) only',
                  'Raw, Analysis window(s) only',
                  'Raw, All available frequencies'
                )
              ),
              inputId = "frequencies_to_export",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Frequencies in the export: 'Collapsed, Analysis window(s) only'",
                "(default; the mean over each window's band), 'Raw, Analysis",
                "window(s) only', or 'Raw, All available frequencies'."
              )
            ),
            shidashi::register_input(
              shiny::selectInput(
                inputId = ns("times_to_export"),
                label = "How to export time",
                choices = c(
                  'Collapsed, Analysis window(s) only',
                  'Raw, Analysis window(s) only',
                  'Raw, All available times'
                ),
                selected = 'Raw, All available times'
              ),
              inputId = "times_to_export",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Time points in the export: 'Collapsed, Analysis window(s) only'",
                "(the mean over each window's time range), 'Raw, Analysis",
                "window(s) only', or 'Raw, All available times' (default)."
              )
            ),
            shidashi::register_input(
              shiny::selectInput(
                inputId = ns("trials_to_export"),
                label = "How to export trial",
                choices = c(
                  'Raw, Only trials used in grouping factors',
                  'Raw, All available trials',
                  'Collapsed by grouping factors'
                ),
                # 'Collapsed by condition',

                selected = 'Raw, only Conditions used in grouping factors'
              ),
              inputId = "trials_to_export",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Trials in the export: 'Raw, Only trials used in grouping factors'",
                "(default), 'Raw, All available trials', or 'Collapsed by grouping",
                "factors' (the mean per trial group)."
              )
            ),
            # shiny::selectInput(
            #   inputId = ns('electrode_export_file_type'),
            #   label = "Export format",
            #   choices = c('HDF5', 'FST',
            #               'Compressed CSV', 'RDS')
            # ),
            # shiny::selectInput(
            #   inputId = ns('electrode_export_data_type'),
            #   label = "Data structure",
            #   choices = c('tensor', 'flat')
            # ),
            shidashi::register_input(
              shiny::actionButton(
                inputId = ns('btn_export_electrodes'),
                label = "Export",
                icon = ravedash::shiny_icons$export
              ),
              inputId = "btn_export_electrodes",
              update = "shiny::updateActionButton",
              description = paste(
                "Button 'Export': writes the baseline-corrected data of",
                "`electrodes_to_export` to CSV files in the subject folder. Agents",
                "run script `export_electrodes` instead, after confirming the",
                "export settings with the user."
              ),
              writable = FALSE
            )
          ),


          # ---- Input tab: Export Markdown Report --------------------------------

          # ravedash::input_card(
          #   class_header='shidashi-anchor', title='Export PowerPoint',
          #   shiny::selectInput(ns('epp_ppt_template'), label = 'PPT Template',
          #                      choices='RAVE 16:9 White Background'),
          #   shiny::selectInput(ns('epp_markdown_template'),
          #                      label = 'Markdown Template', choices='All Plots'),
          #
          #   shiny::downloadButton(
          #     outputId = ns('btn_export_powerpoint'),
          #     label = "Generate Report",
          #     class='btn-primary',
          #     icon = ravedash::shiny_icons$export
          #   )
          # )

          ravedash::input_card(
            class_header='shidashi-anchor', title='Export HTML Report',
            shiny::p("Only selected electrodes are included in this report (not all loaded electrodes)."),
            shidashi::register_input(
              shiny::selectInput(ns('exp_html_electrodes_to_include'),
                                 label = 'Electrode groupings', choices=c('Aggregate only', 'Aggregate + individual'), selected='Aggregate + individual'
                                 ),
              inputId = "exp_html_electrodes_to_include",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "HTML report content: 'Aggregate only', or 'Aggregate + individual'",
                "(default), which adds plots of each selected electrode."
              )
            ),
            shidashi::register_input(
              shiny::selectInput(ns('exp_html_graphs'), label = 'Graphs to include',
                                 choices=c('over_time_by_electrode', 'by_frequency_over_time', 'over_time_by_trial','over_time_by_condition'),
                                 selected=c('over_time_by_electrode', 'by_frequency_over_time', 'over_time_by_trial','over_time_by_condition'), multiple = TRUE),
              inputId = "exp_html_graphs",
              update = "shiny::updateSelectInput(value=selected)",
              description = paste(
                "Graphs in the HTML report, a JSON array of any of",
                "'over_time_by_electrode', 'by_frequency_over_time',",
                "'over_time_by_trial', 'over_time_by_condition' (default: all)."
              )
            ),

            shidashi::register_input(
              shiny::actionButton(inputId = ns('btn_export_html_report'),
                label = "Generate Report",
                class='btn-primary',
                icon = ravedash::shiny_icons$save
              ),
              inputId = "btn_export_html_report",
              update = "shiny::updateActionButton",
              description = paste(
                "Button 'Generate Report': writes an HTML report into the",
                "subject's reports folder, in the background. Agents run script",
                "`generate_report` instead, after confirming with the user, then",
                "poll `report_status`."
              ),
              writable = FALSE
            )
          )



          # baseline_choices$ui_func(),
          # comp_analysis_ranges$ui_func()
        )
      )
    ),

    # ---- Output tabs (width = 9) -------------------------------

    shiny::column(
      width = 9L,
      shiny::div(
        class = "row screen-height overflow-y-scroll output-wrapper",
        shiny::column(
          width = 12L,


          # ---- Output tab-set: Brain Viewers -------------------------------

          # Registered so that agents can switch the tabs (MCP); the tab set
          # itself is unchanged
          shidashi::register_input(
            inputId = "brain_viewers",
            update = "shidashi::card_tabset_activate(value=title)",
            description = paste(
              "Active tab of the 'Brain Viewers' card: 'Results Viewer' (3D",
              "viewer `brain_viewer`: the statistics of each electrode after",
              "`run_analysis`) or 'Movie Maker' (3D viewer `brain_viewer_movies`:",
              "the electrode-by-time animation, which needs",
              "`do_over_time_by_electrode_dataframe`). Set it to show the user a",
              "tab."
            ),
          ravedash::output_cardset(
            inputId = ns('brain_viewers'), title = 'Brain Viewers',
            class_body = "no-padding min-height-400 height-400 resize-vertical",
            tools = list(
            ),
            append_tools = FALSE,
            `Results Viewer` =shiny::div(class='position-relative fill',
                                         # MIGRATED: removed ravedash::output_gadget_container() wrapper
                                         # ravedash::output_gadget_container(
                                           threeBrain::threejsBrainOutput(
                                             outputId = ns("brain_viewer"), height = "100%"
                                           )
                                         # )
            )
            ,
            `Movie Maker` =
              # MIGRATED: removed ravedash::output_gadget_container() wrapper
              # ravedash::output_gadget_container(
                threeBrain::threejsBrainOutput(
                  outputId = ns("brain_viewer_movies"), height = "100%"
                )
              # )
          )), # end of register_input("brain_viewers")


          # ---- Output tab-set: By Electrode -----------------------

          # Registered so that agents can switch the tabs (MCP); the tab set
          # itself is unchanged
          shidashi::register_input(
            inputId = "by_electrode_tabset",
            update = "shidashi::card_tabset_activate(value=title)",
            description = paste(
              "Active tab of the 'By Electrode' card: 'Over Time' (heatmap",
              "`over_time_by_electrode`, electrodes by time, with sorting and",
              "clusters), 'Waterfall over Time' (`waterfall_by_electrode_plot`),",
              "'By Condition' (per-electrode mean, t and p plots), 'Tabular",
              "Results' (table `per_electrode_results_table`), or 'Custom Plot'",
              "(`by_electrode_custom_plot`). A plain output such as the table",
              "can be read with `shiny_query_ui` only while its tab is showing."
            ),
          ravedash::output_cardset(
            inputId = ns('by_electrode_tabset'),
            title = "By Electrode",
            class_body = "no-padding fill-width",
            append_tools = FALSE,
            tools = list(
              shidashi::card_tool(
                widget = "custom", icon = ravedash::shiny_icons$puzzle,
                inputId = ns("by_electrode_tabset_config")
              ),
              shidashi::card_tool(
                widget = "custom", icon = ravedash::shiny_icons$camera,
                inputId = ns("by_electrode_tabset_camera")
              )
            ),

            # --- By Electrode - Over time tab, raster style plot per electrode, color is activity
            `Over Time` = shiny::tagList(
              shiny::div(
                # opens a fluid container
                class = "container-fluid",
                make_heatmap_control_panel(prefix = 'otbe', config = 'by_electrode_tabset_config', do_xlim=FALSE),
                shiny::conditionalPanel(
                  condition = sprintf("input['%s'] %% 2 == 1", 'by_electrode_tabset_config'),
                  ns = ns,
                  shiny::div(
                    class = "container-fluid",
                    shiny::fluidRow(
                      shiny::column(width = 3L,
                                    # shiny::p("Clustering options")
                                    shidashi::register_input(
                                      shiny::selectInput(ns('otbe_yaxis_sort'), label = 'How to sort electrodes',
                                                         choices = c('Electrode #',
                                                                     'Activity Correlation',
                                                                     'Activity Correlation (spearman)',
                                                                     'Activity distance (Euclidean)',
                                                                     'Coordinate distance', #'Coordinate distance (ignore Hemi)',
                                                                     'ROI distance'#, 'ROI distance (ignore Hemi)'
                                                         )
                                      ),
                                      inputId = "otbe_yaxis_sort",
                                      update = "shiny::updateSelectInput(value=selected)",
                                      description = paste(
                                        "How the 'By Electrode' heatmap sorts electrodes:",
                                        "'Electrode #' (default), 'Activity Correlation',",
                                        "'Activity Correlation (spearman)', 'Activity distance",
                                        "(Euclidean)', 'Coordinate distance', or 'ROI distance'.",
                                        "Any choice but 'Electrode #' also clusters the",
                                        "electrodes into `otbe_yaxis_cluster_k` groups when the",
                                        "plot is drawn; scripts `cluster_to_roi` and",
                                        "`cluster_to_viewer` use these clusters."
                                      )
                                    )),
                      shiny::column(width = 3L,
                                    shidashi::register_input(
                                      shiny::selectInput(ns('otbe_yaxis_sort_combine_rule'),
                                                         label = 'Distance combination rule',
                                                         choices = c('total (sum across conditions)', 'min (smallest distance wins)', 'max (largest distance wins)')
                                      ),
                                      inputId = "otbe_yaxis_sort_combine_rule",
                                      update = "shiny::updateSelectInput(value=selected)",
                                      description = "Rule for combining distances across conditions when sorting electrodes"
                                    )
                      ),
                      shiny::column(width = 1L,
                                    shidashi::register_input(
                                      shiny::numericInput(ns('otbe_yaxis_cluster_k'),
                                                          label = '# Clusters', value = 2, min = 2, max = 8
                                      ),
                                      inputId = "otbe_yaxis_cluster_k",
                                      update = "shiny::updateNumericInput",
                                      description = "Number of electrode clusters for sorting"
                                    )

                      ),
                      shiny::column(width = 1L,
                                    shidashi::register_input(
                                      shiny::selectInput(ns('otbe_yaxis_cluster_palette'),
                                                         label = 'Palette',
                                                         choices = rev(get_line_palette(get_palette_names = T)), selected ='Set2'
                                      ),
                                      inputId = "otbe_yaxis_cluster_palette",
                                      update = "shiny::updateSelectInput(value=selected)",
                                      description = "Color palette for electrode clusters"
                                    )
                      ),
                      shiny::column(width = 2L,
                                    shidashi::register_input(
                                      shiny::actionButton(ns('otbe_update_3dviewer'),
                                                          label = 'Cluster -> brain viewer',class='btn-small',
                                                          icon = ravedash::shiny_icons$arrow_up
                                      ),
                                      inputId = "otbe_update_3dviewer",
                                      update = "shiny::updateActionButton",
                                      description = paste(
                                        "Button 'Cluster -> brain viewer': redraws the 3D",
                                        "viewer `brain_viewer` with the clusters as variable",
                                        "`PE_Cluster`. Agents run script `cluster_to_viewer`",
                                        "instead."
                                      ),
                                      writable = FALSE
                                    ),
                                    shidashi::register_input(
                                      shiny::actionButton(ns('otbe_create_roi'),
                                                          label = 'Cluster -> ROI',class='btn-small',
                                                          icon = ravedash::shiny_icons$magic
                                      ),
                                      inputId = "otbe_create_roi",
                                      update = "shiny::updateActionButton",
                                      description = paste(
                                        "Button 'Cluster -> ROI': turns on the custom ROI with",
                                        "variable `PE_Cluster` (the clusters). Agents run",
                                        "script `cluster_to_roi` instead."
                                      ),
                                      writable = FALSE
                                    )
                      ),
                      shiny::column(width=2,
                                    clipboardOutput(outputId=ns('otbe_cluster_clipboard'), as_card_tool = FALSE,
                                                    message='Cluster -> clipboard', class='btn-small'),
                                    shidashi::register_input(
                                      shiny::actionButton(ns('otbe_cluster_to_electrodes_csv'), width = "75%",
                                                          label = 'Cluster -> electrodes.csv', class='clipboard-btn btn btn-default'
                                      ),
                                      inputId = "otbe_cluster_to_electrodes_csv",
                                      update = "shiny::updateActionButton",
                                      description = paste(
                                        "Button 'Cluster -> electrodes.csv': opens a dialog that",
                                        "writes the clusters into the subject's",
                                        "meta/electrodes.csv. Only the user may use it: agents",
                                        "never write electrodes.csv, have no tool for it, and",
                                        "point the user to this button instead."
                                      ),
                                      writable = FALSE
                                    )
                      )
                    )
                  )
                )
              ),
              shiny::div(
                class = "fill-width no-padding min-height-400 resize-vertical",
                # MIGRATED: removed ravedash::output_gadget_container() wrapper
                # ravedash::output_gadget_container(
                  ravedash::plotOutput2(
                    outputId = ns('over_time_by_electrode'),
                    min_height = 400
                  )
                # )
              )
            ),

            # ---- Output tab: By Electrode > Graphical Results -----
            `Waterfall over Time` = shiny::div(
              class = "fill-width no-padding min-height-400 resize-vertical",
              make_heatmap_control_panel(prefix = 'bewot', config = 'by_electrode_tabset_config', do_aw_only_scale=TRUE),
              ravedash::plotOutput2(
                outputId = ns('waterfall_by_electrode_plot'),
                min_height = 400
              )
            ),
            # ---- Output tab: By Electrode > Graphical Results -----
            `By Condition` = shiny::div(
              shiny::conditionalPanel(
                condition = "input['by_electrode_tabset_config']%2 == 1",
                ns = ns,
                shiny::div(
                  class = "container-fluid",
                  shiny::fluidRow(
                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns('per_electrode_statistics_chooser'),
                          label = 'Data group to display',
                          choices = c('No groups available')
                        ),
                        inputId = "per_electrode_statistics_chooser",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "Which statistics the three per-electrode plots of the 'By",
                          "Condition' tab show, e.g. 'overall', a trial group, or a",
                          "contrast ('A - B'). The choices load after `run_analysis`."
                        )
                      )
                    ),
                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns('pes_select_mode'),
                          label = 'Select mode',
                          choices=c('Label maker', 'Threshold |v| > x', 'Threshold v > x',
                                    'Threshold v < x', 'Manual threshold', 'Invert selection',
                                    'Clear labels')
                        ),
                        inputId = "pes_select_mode",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "How clicks on the per-electrode plots select electrodes",
                          "('Label maker', thresholds); 'Manual threshold' opens a",
                          "dialog. For people only: agents cannot click plots or fill",
                          "in or close the dialog, so leave it at 'Label maker' and",
                          "choose electrodes with `electrode_text`."
                        )
                      )
                    ),
                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns('pes_label_type'),
                          label = 'Label type',
                          multiple = TRUE,
                          selected='number',
                          choices = c('number', 'name', 'color', 'showcase')
                        ),
                        inputId = "pes_label_type",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = "Types of labels to show on per-electrode statistics plots"
                      )
                    ),
                    # shiny::column(width = 2,
                    #               shiny::selectInput(ns('pes_magic_thresholding'), multiple = TRUE,
                    #                                  label = 'Auto-threshold', selected = character(0),
                    #                                  choices=c('p < 0.05','v > x','v < x'))
                    # ),
                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns('pes_selected_action'),
                          multiple = FALSE,
                          label = 'Actions',
                          selected = character(0),
                          choices = c(
                            'Click for choices', 'Analyze selection', 'Send selection to export'
                            # 'Select nearby electrodes'
                          )
                        ),
                        inputId = "pes_selected_action",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "Acts on the electrodes selected by plot clicks: 'Analyze",
                          "selection' re-runs the analysis on them; 'Send selection to",
                          "export' copies them into `electrodes_to_export`. For people",
                          "only: agents set `electrode_text` or `electrodes_to_export`",
                          "directly."
                        )
                      )
                    )
                  ),

                  shiny::fluidRow(
                    shiny::column(
                      width = 12L,
                      shiny::textOutput(outputId = ns('pes_threshold_string'))
                    )
                  )
                )
              ),
              shiny::div(
                # a container with 100% width, 400px initial (and minimum) height, vertically resizable
                class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                shiny::div(
                  class = "row fill-height",
                  shiny::div(class = "col-sm-4 fill-height",
                             ravedash::plotOutput2(
                               outputId = ns('per_electrode_statistics_mean'),
                               click = ns('pes_click_m'),
                               min_height = 400
                             )),
                  shiny::div(class = "col-sm-4 fill-height",
                             ravedash::plotOutput2(
                               outputId = ns('per_electrode_statistics_tstat'),
                               click = ns('pes_click_t'),
                               min_height = 400
                             )),
                  shiny::div(class = "col-sm-4 fill-height",
                             ravedash::plotOutput2(
                               outputId = ns('per_electrode_statistics_fdrp'),
                               click = ns('pes_click_p'),
                               min_height = 400
                             ))
                )
              )
            ),

            # ---- Output tab: By Electrode > Tabular Results -------
            `Tabular Results` = shiny::div(
              class = "fill-width min-height-400",
              shiny::conditionalPanel(
                condition = "input['by_electrode_tabset_config']%2 == 1",
                ns = ns,
                shiny::div(
                  class = "container-fluid",
                  shiny::fluidRow(
                    shiny::column(width=4, shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns('bet_variables_to_hide'),
                        label = 'Variables to remove',
                        choices = character(0), multiple = TRUE),
                      inputId = "bet_variables_to_hide",
                      update = "shiny::updateSelectInput(value=selected)",
                      description = "Variables to hide from the tabular results"
                    )),
                    shiny::column(width=2, shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns('bet_metrics_to_show'),
                        label = 'Metrics to include',choices = c('m', 't', 'p'),
                        selected = c('m', 't', 'p'), multiple = TRUE),
                      inputId = "bet_metrics_to_show",
                      update = "shiny::updateSelectInput(value=selected)",
                      description = "Statistical metrics to show in the table (m=mean, t=t-statistic, p=p-value)"
                    )),
                    shiny::column(width=2, style='text-align: left; margin-top:37px; margin-left:0px',
                                  shidashi::register_input(
                                    shiny::checkboxInput(
                                      inputId = ns('bet_show_contrasts'), label = 'Show Contrasts',
                                      value = TRUE
                                    ),
                                    inputId = "bet_show_contrasts",
                                    update = "shiny::updateCheckboxInput",
                                    description = "Whether to show contrast results in the table"
                                  )),
                    shiny::column(width=4, shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns('bet_meta_data'),
                        label = 'Meta data to include',
                        choices = character(0), multiple = TRUE),
                      inputId = "bet_meta_data",
                      update = "shiny::updateSelectInput(value=selected)",
                      description = "Electrode meta data columns to include in the table"
                    ))
                  )
                )
              ),
              DT::dataTableOutput(outputId = ns('per_electrode_results_table'))
            ),

            # ---- Output tab: By Electrode > Custom Plot -------
            `Custom Plot` = shiny::div(
              class = "fill-width min-height-400",
              shiny::conditionalPanel(
                condition = "input['by_electrode_tabset_config']%2 == 1",
                ns = ns,
                shiny::div(
                  class = "container-fluid",
                  shiny::fluidRow(
                    shiny::column(width = 2L,
                                  shiny::conditionalPanel(condition = 'input["power_explorer-bec_yvar_is_multi"] == 1',
                                                          shidashi::register_input(
                                                            shiny::selectInput(inputId = ns('bec_yvar_chooser_multi'), label = 'Data for Y axis',
                                                                               choices = c('Empty'), multiple = TRUE),
                                                            inputId = "bec_yvar_chooser_multi",
                                                            update = "shiny::updateSelectInput(value=selected)",
                                                            description = "Multi-select data variable(s) for the custom plot Y axis"
                                                          ),
                                  ),
                                  shiny::conditionalPanel(condition = 'input["power_explorer-bec_yvar_is_multi"] == 0',
                                                          shidashi::register_input(
                                                            shiny::selectInput(inputId = ns('bec_yvar_chooser'), label = 'Data for Y axis',
                                                                               choices = c('Empty'), multiple = FALSE),
                                                            inputId = "bec_yvar_chooser",
                                                            update = "shiny::updateSelectInput(value=selected)",
                                                            description = "Data variable for the custom plot Y axis"
                                                          ),
                                  )
                    ),
                    shiny::conditionalPanel(condition = 'input["power_explorer-bec_yvar_is_multi"] == 1',
                                            shiny::column(width = 12,
                                                          shidashi::register_input(
                                                            shiny::selectInput(inputId = ns('bec_yvar_collapser'), label = 'Collapse', choices = PE_COLLAPSE_METHODS),
                                                            inputId = "bec_yvar_collapser",
                                                            update = "shiny::updateSelectInput(value=selected)",
                                                            description = "Method to collapse multiple Y-axis variables"
                                                          )
                                            )
                    ),
                    shiny::column(width = 1L, style='text-align: left; margin-top:37px; margin-left:0px',
                                  shidashi::register_input(
                                    shiny::checkboxInput(inputId = ns('bec_yvar_is_multi'), label='multi', value = FALSE),
                                    inputId = "bec_yvar_is_multi",
                                    update = "shiny::updateCheckboxInput",
                                    description = "Toggle multi-select mode for Y-axis variable"
                                  )
                    ),
                    shiny::column(width = 1L,
                                  shidashi::register_input(
                                    shiny::selectInput(inputId = ns('bec_yvar_unit'), label = 'unit', choices = c('m', 't', 'p')),
                                    inputId = "bec_yvar_unit",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Unit for Y-axis: m=mean, t=t-statistic, p=p-value"
                                  )
                    ),
                    shiny::column(width = 2L, offset=1,
                                  shiny::conditionalPanel(condition = 'input["power_explorer-bec_xvar_is_multi"] == 1',
                                                          shidashi::register_input(
                                                            shiny::selectInput(inputId = ns('bec_xvar_chooser_multi'), label = 'Data for X axis',
                                                                               choices = c('Empty'), multiple = TRUE),
                                                            inputId = "bec_xvar_chooser_multi",
                                                            update = "shiny::updateSelectInput(value=selected)",
                                                            description = "Multi-select data variable(s) for the custom plot X axis"
                                                          ),
                                  ),
                                  shiny::conditionalPanel(condition = 'input["power_explorer-bec_xvar_is_multi"] == 0',
                                                          shidashi::register_input(
                                                            shiny::selectInput(inputId = ns('bec_xvar_chooser'), label = 'Data for X axis',
                                                                               choices = c('Empty'), multiple = FALSE),
                                                            inputId = "bec_xvar_chooser",
                                                            update = "shiny::updateSelectInput(value=selected)",
                                                            description = "Data variable for the custom plot X axis"
                                                          ),
                                  )
                    ),
                    shiny::conditionalPanel(condition = 'input["power_explorer-bec_xvar_is_multi"] == 1',
                                            shiny::column(width = 12,
                                                          shidashi::register_input(
                                                            shiny::selectInput(inputId = ns('bec_xvar_collapser'), label = 'Collapse', choices = PE_COLLAPSE_METHODS),
                                                            inputId = "bec_xvar_collapser",
                                                            update = "shiny::updateSelectInput(value=selected)",
                                                            description = "Method to collapse multiple X-axis variables"
                                                          )
                                            )
                    ),
                    shiny::column(width = 1L, style='text-align: left; margin-top: 37px; margin-left:0px',
                                  shidashi::register_input(
                                    shiny::checkboxInput(inputId = ns('bec_xvar_is_multi'), label='multi', value = FALSE),
                                    inputId = "bec_xvar_is_multi",
                                    update = "shiny::updateCheckboxInput",
                                    description = "Toggle multi-select mode for X-axis variable"
                                  )
                    ),
                    shiny::column(width = 1L,
                                  shidashi::register_input(
                                    shiny::selectInput(inputId = ns('bec_xvar_unit'), label = 'unit', choices = c('m', 't', 'p')),
                                    inputId = "bec_xvar_unit",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Unit for X-axis: m=mean, t=t-statistic, p=p-value"
                                  )
                    )
                  ),
                  shiny::fluidRow(
                    shiny::column(width = 2L,
                                  shidashi::register_input(
                                    shiny::selectInput(inputId = ns('bec_only_selected_electrodes'), label = 'Display',
                                                       choices=c('All Electrodes', 'Currently selected')),
                                    inputId = "bec_only_selected_electrodes",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Display all loaded electrodes or only currently selected ones in custom plot"
                                  )
                    ),
                    shiny::column(width = 1L,
                                  shidashi::register_input(
                                    shiny::numericInput(ns('bec_plot_width_scale'), 'Plot width',
                                                        value = 1, min=0.1, max=10, step = .1),
                                    inputId = "bec_plot_width_scale",
                                    update = "shiny::updateNumericInput",
                                    description = "Scale factor for the custom plot width"
                                  )
                    ),
                    shiny::column(width=1,
                                  shidashi::register_input(
                                    shiny::numericInput(ns('bec_pt.alpha'), 'Pt alpha', value = 100, min=0, max=100),
                                    inputId = "bec_pt.alpha",
                                    update = "shiny::updateNumericInput",
                                    description = "Point opacity (0-100) for the custom plot"
                                  )
                    ),
                    shiny::column(width=1,
                                  shidashi::register_input(
                                    shiny::numericInput(ns('bec_pt.cex'), 'Pt scale', value = 1, min=0.1, max=10, step = .1),
                                    inputId = "bec_pt.cex",
                                    update = "shiny::updateNumericInput",
                                    description = "Point size scaling factor for the custom plot"
                                  )
                    ),
                    shiny::column(width=2,
                                  shidashi::register_input(
                                    shiny::selectInput(ns('bec_plot_decorators'), 'Plot Decor', choices=get_plot_decorators(names_only=TRUE), multiple = TRUE),
                                    inputId = "bec_plot_decorators",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Decorations to add to the custom plot"
                                  )
                    )


                    #,
                    # shiny::column(width = 1L,
                    #               shiny::numericInput(ns('bec_plot_width_scale'), 'Scale plot width',
                    #                                   value = 1, min=0.1, max=10, step = .1)
                    # )
                  )
                )
              ),
              ravedash::plotOutput2(outputId = ns('by_electrode_custom_plot'))
            )
          )), # end of register_input("by_electrode_tabset")

          # ---- Output tab-set: By Frequency --------------------------------
          make_by_frequency_tabset(),

          # ---- Output tab-set: Over Time -----------------------------------
          # Registered so that agents can switch the tabs (MCP); the tab set
          # itself is unchanged
          shidashi::register_input(
            inputId = "over_time_tabset",
            update = "shidashi::card_tabset_activate(value=title)",
            description = paste(
              "Active tab of the 'Over Time' card: 'By Condition' (plot",
              "`over_time_by_condition`, time courses per trial group) or 'By",
              "Trial' (heatmap `over_time_by_trial`). Set it to show the user a",
              "tab."
            ),
          ravedash::output_cardset(
            inputId = ns('over_time_tabset'),
            title='Over Time',
            class_body="no-padding fill-width",
            append_tools = FALSE,
            tools = list(
              shidashi::card_tool(
                widget = "custom", icon = ravedash::shiny_icons$puzzle,
                inputId = ns("over_time_tabset_config")
              ),
              shidashi::card_tool(
                widget = "custom", icon = ravedash::shiny_icons$camera,
                inputId = ns("over_time_tabset_camera")
              )
            ),

            # ---- Output tab: Over Time > By Condition ----------------------
            `By Condition` = shiny::tagList(
              shiny::div(
                # opens a fluid container
                class = "container-fluid",
                shiny::conditionalPanel(
                  condition = "input['over_time_tabset_config']%2 == 1",
                  ns = ns,
                  shiny::fluidRow(
                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns('over_time_by_condition_switch'),
                          label='Plot type', selected = 'Combine conditions',
                          choices = c('Combine conditions', 'Combine events',
                                      'Combine all', 'Separate all')
                        ),
                        inputId = "over_time_by_condition_switch",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = "How to combine conditions and events in the over-time-by-condition plot"
                      )),
                    shiny::column(offset = 1,
                                  width = 4L,
                                  shidashi::register_input(
                                    shiny::sliderInput(
                                      inputId = ns('over_time_by_condition_plot_range'),
                                      label='Plot range', value = c(0,1),
                                      min =0, max=1, step = 0.01, dragRange = TRUE
                                    ),
                                    inputId = "over_time_by_condition_plot_range",
                                    update = "shiny::updateSliderInput",
                                    description = "Y-axis plot range for the over-time-by-condition plot"
                                  )
                    )
                  )
                )
              ),
              shiny::div(
                class = "fill-width no-padding min-height-400 height-400 resize-vertical",
                # MIGRATED: removed ravedash::output_gadget_container() wrapper
                # ravedash::output_gadget_container(
                  ravedash::plotOutput2(
                    outputId = ns('over_time_by_condition'),
                    min_height = 400)
                # )
              )
            ),
            `By Trial` = shiny::tagList(
              shiny::div(
                class = "fill-width no-padding min-height-400 resize-vertical",
                make_heatmap_control_panel(prefix = 'otbt', config = 'over_time_tabset_config'),
                # MIGRATED: removed ravedash::output_gadget_container() wrapper
                # ravedash::output_gadget_container(
                  ravedash::plotOutput2(
                    outputId = ns('over_time_by_trial'),
                    min_height = 400)
                # )
              )
            )
          )), # end of register_input("over_time_tabset")

          # ---- Output tab-set: By Condition ------------------------------------
          # Registered so that agents can switch the tabs (MCP); the tab set
          # itself is unchanged
          shidashi::register_input(
            inputId = "by_condition_tabset",
            update = "shidashi::card_tabset_activate(value=title)",
            description = paste(
              "Active tab of the 'By Condition' card: 'By Trial' (plot",
              "`by_condition_by_trial` and its click table), 'Overall model test'",
              "(`by_condition_statistics`), 'Conditions vs. Baseline'",
              "(`by_condition_statistics_emmeans`), or 'Pairwise comparisons'",
              "(`by_condition_statistics_contrasts`). Set it to show the user a",
              "tab."
            ),
          ravedash::output_cardset(
            inputId = ns('by_condition_tabset'),
            title='By Condition',
            class_body='',
            append_tools = FALSE,
            tools = list(
              clipboardOutput(outputId=ns('by_condition_tabset_clipboard'), as_card_tool = TRUE, message='copy data'),
              shidashi::card_tool(
                widget = "custom", icon = ravedash::shiny_icons$puzzle,
                inputId = ns("by_condition_tabset_config")
              ),
              shidashi::card_tool(
                widget = "custom", icon = ravedash::shiny_icons$camera,
                inputId = ns("by_condition_tabset_camera")
              )
            ),
            `By Trial` = #shiny::tagList(
              shiny::div(
                # id='makeinline',
                # class='height-400 fill-width container-fluid',
                # class = "no-padding",
                {shiny::conditionalPanel(
                  "input['power_explorer-by_condition_tabset_config'] %2 == 1",
                  shiny::fluidRow(
                    shiny::column(
                      width=2, shidashi::register_input(
                        shiny::selectInput(ns('btp_basic_unit'),
                                           label = 'Points are: ',
                                           choices=c('Trials', 'Electrodes')),
                        inputId = "btp_basic_unit",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = "Whether points represent trials or electrodes in the by-condition plot"
                      )
                    ),
                    shiny::column(width=1, style='text-align: left; margin-top:37px; margin-left:0px',
                                  shidashi::register_input(
                                    shiny::checkboxInput(ns('bcbt_show_outliers'), "Show Outliers", value = TRUE),
                                    inputId = "bcbt_show_outliers",
                                    update = "shiny::updateCheckboxInput",
                                    description = "Show or hide outlier points in the by-condition plot"
                                  )
                    ),
                    shiny::column(
                      width=3,
                      shidashi::register_input(
                        shiny::selectInput(ns("btp_types"), label = 'Plot types',
                                           multiple = TRUE,
                                           choices = c('jitter points', 'means', 'ebar polygons', 'sd polygons',
                                                       'points', 'connect points',
                                                       'densities', 'density polygons',
                                                       'bars', 'borders', 'ebars'),
                                           selected=c('jitter points', 'means', 'ebar polygons')
                        ),
                        inputId = "btp_types",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = "Visual elements to display in the by-condition plot"
                      )),
                    shiny::column(width=2,
                                  shidashi::register_input(
                                    shiny::selectInput(ns("btp_xvar"),
                                                       "X-axis", choices=c('First Factor',
                                                                           'Analysis Group')),
                                    inputId = "btp_xvar",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Variable for the X axis in the by-condition plot"
                                  )
                    ),
                    shiny::column(width=2,
                                  shidashi::register_input(
                                    shiny::selectInput(ns("btp_gvar"),
                                                       "Group by", choices=c('none', 'Analysis Group', 'First Factor')),
                                    inputId = "btp_gvar",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Variable to group by (color) in the by-condition plot"
                                  ),
                    ),
                    shiny::column(width=2,
                                  shidashi::register_input(
                                    shiny::selectInput(ns("btp_panelvar"),
                                                       "Panel by", choices=c('none', 'Analysis Group',
                                                                             'First Factor')),
                                    inputId = "btp_panelvar",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Variable to create separate panels in the by-condition plot"
                                  )
                    )
                  ),
                  shiny::fluidRow(
                    shiny::column(width=3,
                                  shidashi::register_input(
                                    shiny::numericInput(ns('btp_pt.alpha'), 'Point alpha (opacity)', value = 100, min=0, max=100),
                                    inputId = "btp_pt.alpha",
                                    update = "shiny::updateNumericInput",
                                    description = "Point opacity (0-100) for the by-condition-by-trial plot"
                                  )
                    ),
                    shiny::column(width=3,
                                  shidashi::register_input(
                                    shiny::numericInput(ns('btp_pt.cex'), 'Point scaling', value = 1, min=0.1, max=10, step = .1),
                                    inputId = "btp_pt.cex",
                                    update = "shiny::updateNumericInput",
                                    description = "Point size scaling for the by-condition-by-trial plot"
                                  )
                    ),
                    shiny::column(width=3,
                                  shidashi::register_input(
                                    shiny::numericInput(ns('scale_pbtbc'), 'Plot width scaling', value = 1, min=0.1, max=10, step = .1),
                                    inputId = "scale_pbtbc",
                                    update = "shiny::updateNumericInput",
                                    description = "Plot width scaling for the by-condition-by-trial plot"
                                  )
                    ),
                    shiny::column(width=2,
                                  shidashi::register_input(
                                    shiny::selectInput(ns("btp_highlight_clicks"), selected='labels',
                                                       "Highlight Clicks", choices=c('points', 'lines',
                                                                                     'labels'), multiple = TRUE),
                                    inputId = "btp_highlight_clicks",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "How to highlight clicked data in the by-condition plot"
                                  )
                    ),
                    shiny::column(width=1,
                                  shidashi::register_input(
                                    shiny::selectInput(ns("btp_highlight_text_location"), selected='top',
                                                       "Highlight pos", choices=c('center', 'bottom', 'left', 'top', 'right')),
                                    inputId = "btp_highlight_text_location",
                                    update = "shiny::updateSelectInput(value=selected)",
                                    description = "Position for highlight text labels"
                                  )
                    )
                  )
                )},
                shiny::fluidRow(
                  shiny::column(width=7,
                                # shiny::div(class='col-sm-8',
                                # ravedash::output_gadget_container(
                                ravedash::plotOutput2(outputId = ns('by_condition_by_trial'),
                                                      click = shiny::clickOpts(ns('btbc_click'), clip=TRUE),
                                                      dblclick =ns('btbc_dblclick')
                                                      # hover=ns('btbc_hover')
                                ),
                  ),
                  #),
                  shiny::column(width=5,
                                # shiny::div(class='col-sm-4',
                                # ravedash::output_gadget_container(
                                shidashi::register_input(
                                  shiny::textInput(ns('flagged_trials'),
                                                   label = 'Flagged trials (applied at RAVE!)',
                                                   value = '', placeholder = 'e.g. 3,17,40-42',
                                                   updateOn = 'blur'),
                                  inputId = "flagged_trials",
                                  update = "shiny::updateTextInput",
                                  description = paste(
                                    "Trials flagged as outliers, e.g. '3,17,40-42': the whole list,",
                                    "so setting it replaces the list and an empty value clears it.",
                                    "From the next RAVE! (script `run_analysis`) on, they are left",
                                    "out of all averages, statistics, the 3D viewer, and exports,",
                                    "for every electrode and analysis window. Trials not in the",
                                    "loaded epoch are dropped. When data load, the list starts",
                                    "from the trials the epoch marks as excluded (column",
                                    "ExcludedHint), unless this session already flagged trials of",
                                    "the same epoch. Agents may set it. Only people save the list",
                                    "into the epoch, so that it lasts beyond this session: ask the",
                                    "user to click 'Save Flags to Epoch' above the Click Details",
                                    "table (By Condition card, By Trial tab). Unsaved flags are",
                                    "lost when another epoch is loaded or the page is reloaded."
                                  )
                                ),
                                DT::dataTableOutput(outputId = ns('by_condition_by_trial_clicks'))
                                # )
                  )
                  # )
                )
              ),
            # ), # end of By trial tag list
            `Overall model test` = shiny::div(style='margin:20px', class="",
                                              shiny::htmlOutput(ns('by_condition_statistics'))
            ),
            `Conditions vs. Baseline` = shiny::div(style='margin:20px',
                                                   class="",
                                                   shiny::fluidRow(
                                                     shiny::column(width = 4,
                                                                   shidashi::register_input(
                                                                     shiny::selectInput(ns('bcs_choose_emmeans'),
                                                                                        "Which means to display?", choices=c('All possible')),
                                                                     inputId = "bcs_choose_emmeans",
                                                                     update = "shiny::updateSelectInput(value=selected)",
                                                                     description = "Choose which estimated marginal means to display"
                                                                   ))
                                                   ),

                                                   shiny::htmlOutput(ns('by_condition_statistics_emmeans'))
            ),
            `Pairwise comparisons` = shiny::div(style='margin:20px',
                                                class="",
                                                shiny::fluidRow(
                                                  shiny::column(width = 5,
                                                                shidashi::register_input(
                                                                  shiny::selectInput(ns('bcs_choose_contrasts'),
                                                                                     "Which contrasts to display?", selected = 'All-possible pairwise',
                                                                                     choices=c('All-possible pairwise',
                                                                                               'Stratified contrasts (more power!)',
                                                                                               'ITX Contrasts (diff of diff)')
                                                                  ),
                                                                  inputId = "bcs_choose_contrasts",
                                                                  update = "shiny::updateSelectInput(value=selected)",
                                                                  description = paste(
                                                                    "Contrasts in tab 'Pairwise comparisons' (output",
                                                                    "`by_condition_statistics_contrasts`): 'All-possible pairwise'",
                                                                    "(default), 'Stratified contrasts (more power!)' (pairwise within",
                                                                    "each level of another factor), or 'ITX Contrasts (diff of diff)'",
                                                                    "(interaction contrasts). The last two are offered after a full",
                                                                    "`run_analysis` with two or more factors (first and second",
                                                                    "factor, ROI groups with 'Group/Stratify results', or several",
                                                                    "analysis windows); pick the layer with",
                                                                    "`bcs_choose_specific_contrast`."
                                                                  )
                                                                )
                                                  ),
                                                  shiny::column(width=4,
                                                                shiny::conditionalPanel('input["power_explorer-bcs_choose_contrasts"] != "All-possible pairwise"',
                                                                                        shidashi::register_input(
                                                                                          shiny::selectInput(ns('bcs_choose_specific_contrast'),
                                                                                                             'Choose layer/grouping', choices=''),
                                                                                          inputId = "bcs_choose_specific_contrast",
                                                                                          update = "shiny::updateSelectInput(value=selected)",
                                                                                          description = paste(
                                                                                            "Which stratification or interaction to show when",
                                                                                            "`bcs_choose_contrasts` is not 'All-possible pairwise': e.g.",
                                                                                            "'Factor1' (contrasts within each level of the first factor) or",
                                                                                            "'Factor1_Factor2' (interaction). The choices load when",
                                                                                            "`bcs_choose_contrasts` changes."
                                                                                          )
                                                                                        )))
                                                ),
                                                shiny::htmlOutput(ns('by_condition_statistics_contrasts'))
            )
          )) # end of register_input("by_condition_tabset")

          #   `card with flip` = shidashi::flip_box(
          #     front = shidashi::info_box("Side A"),
          #     back = shidashi::info_box("Side B"),
          #     inputId = 'flip_box1')
          # ),
        )
      )
    )
  )
  )
}
