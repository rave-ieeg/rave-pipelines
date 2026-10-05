

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
              title = "Load epoch channel",

              ravedash::flex_group_box(
                title = "Basic configuration",
                shidashi::register_input(
                  shiny::selectInput(
                    inputId = ns("load_epoch"),
                    label = "Load existing epoch",
                    choices = "New epoch..."
                  ),
                  inputId = "load_epoch",
                  update = "shiny::updateSelectInput(value=selected)",
                  description = paste(
                    "Loads a saved epoch of the subject into the staged table:",
                    "choosing a name replaces the staged onsets of EVERY block",
                    "(conditions are dropped), then the input resets itself to",
                    "'New epoch...'. Ask the user first when anything is staged;",
                    "check the result with script `epoch_status`."
                  ),
                  tooltip = "Load a saved epoch into the staged table (replaces the staged onsets)."
                ),
                shidashi::register_input(
                  shiny::selectInput(
                    inputId = ns("block"),
                    label = "Choose a block to generate epoch",
                    choices = character(0L)
                  ),
                  inputId = "block",
                  update = "shiny::updateSelectInput(value=selected)",
                  description = paste(
                    "Recording block to work on, e.g. '008' (the subject's",
                    "blocks). Set it before `epoch_file`: a block change re-lists",
                    "the files, keeps a file of the same name selected, and",
                    "otherwise selects a guessed one, so set `epoch_file` and",
                    "confirm it afterwards. Staging labels the onsets with this",
                    "block."
                  ),
                  tooltip = "Block whose trigger signal is loaded and whose onsets are staged."
                ),
                shidashi::register_input(
                  shiny::selectInput(
                    inputId = ns("epoch_file"),
                    label = "File containing epoch channel",
                    choices = character(0L)
                  ),
                  inputId = "epoch_file",
                  update = "shiny::updateSelectInput(value=selected)",
                  description = paste(
                    "File in the block's raw folder, or imported channel",
                    "('[Channel N]'), that holds the trigger signal (e.g. a",
                    "photodiode or audio track). NEVER choose it yourself: ask",
                    "the user which file or channel marks the trials; the",
                    "preselected value is only the module's guess. Script",
                    "`epoch_channel_info` lists the files offered. Then run",
                    "script `load_epoch_file`. Read the manual first: skill",
                    "`rave-module`, reference `references/epoch_generator.md`."
                  ),
                  tooltip = "File or imported channel that holds the trigger signal (photodiode, audio...)."
                ),
                shidashi::register_input(
                  dipsaus::actionButtonStyled(
                    inputId = ns("load_epoch_btn"),
                    label = "Load epoch file",
                    width = "100%"
                  ),
                  inputId = "load_epoch_btn",
                  update = "dipsaus::updateActionButtonStyled",
                  description = paste(
                    "[Action button, read-only] 'Load epoch file' reads the",
                    "selected file. Agents run script `load_epoch_file` instead."
                  ),
                  tooltip = "Read the selected file and list its variables or channels.",
                  writable = FALSE
                )
              ),

              ravedash::flex_group_box(
                title = "Epoch channel settings",
                shidashi::register_input(
                  shiny::selectInput(
                    inputId = ns("varname"),
                    label = "Variable name",
                    choices = character(0L)
                  ),
                  inputId = "varname",
                  update = "shiny::updateSelectInput(value=selected)",
                  description = paste(
                    "Variable (MATLAB/HDF5) or channel (EDF/NEV) of the loaded",
                    "file that holds the trigger signal, e.g. 'analogTraces'.",
                    "Choices appear after `load_epoch_file` and are cleared when",
                    "`epoch_file` changes. It must be a numeric signal, not a",
                    "struct such as 'analogInfos'."
                  ),
                  tooltip = "Variable or channel in the file that holds the trigger signal."
                ),
                shidashi::register_input(
                  shiny::numericInput(
                    inputId = ns("sample_rate"),
                    label = "Epoch channel sample rate",
                    value = 1
                  ),
                  inputId = "sample_rate",
                  update = "shiny::updateNumericInput",
                  description = paste(
                    "Sample rate (Hz) of the trigger signal; set it before",
                    "`load_epoch_signal`. EDF, NEV and imported channels: filled",
                    "from the file, tell the user. MATLAB (.mat): REQUIRED from",
                    "the user. The module fills in an estimate (channel length /",
                    "iEEG block length x iEEG rate) only while the field is empty",
                    "or at most 1, otherwise it keeps the previous value: tell the",
                    "user the value and ask them to confirm or correct it. HDF5",
                    "(.h5): the same estimate; confirming is optional, ask when",
                    "unsure. Script `epoch_channel_info` shows the duration check."
                  ),
                  tooltip = "Sample rate (Hz) of the trigger signal; check it before loading the signal."
                ),
                shidashi::register_input(
                  dipsaus::actionButtonStyled(
                    inputId = ns("load_signal_btn"),
                    label = "Load epoch signal",
                    width = "100%"
                  ),
                  inputId = "load_signal_btn",
                  update = "dipsaus::updateActionButtonStyled",
                  description = paste(
                    "[Action button, read-only] 'Load epoch signal' loads the",
                    "selected variable. Agents run script `load_epoch_signal`",
                    "instead."
                  ),
                  tooltip = "Load the trigger signal and plot it.",
                  writable = FALSE
                )
              )
            ),


            ravedash::input_card(
              title = "Transform & threshold",
              class_body = "padding-10",
              shiny::fluidRow(
                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::numericInput(
                      ns("plot_range"),
                      "Plot range",
                      min = 0, step = 1, value = 0
                    ),
                    inputId = "plot_range",
                    update = "shiny::updateNumericInput",
                    description = paste(
                      "Half-height of the overview plot's y-axis (0 means",
                      "automatic). Changes the plot only, not the thresholding."
                    ),
                    tooltip = "Half-height of the plot's y-axis (0 means automatic)."
                  )
                ),
                shiny::column(
                  width = 6L,
                  shidashi::register_input(
                    shiny::checkboxInput(
                      ns("plot_difference"),
                      "Difference plot"
                    ),
                    inputId = "plot_difference",
                    update = "shiny::updateCheckboxInput",
                    description = paste(
                      "Plot and threshold the sample-to-sample difference of the",
                      "signal (true/false), e.g. to find sharp edges. Changing it",
                      "does not re-run the thresholding: set `threshold` again."
                    ),
                    tooltip = "Use the sample-to-sample difference of the signal."
                  )
                ),
                shiny::column(
                  width = 6L,
                  shidashi::register_input(
                    shiny::checkboxInput(
                      ns("plot_absolute"),
                      "Absolute plot"
                    ),
                    inputId = "plot_absolute",
                    update = "shiny::updateCheckboxInput",
                    description = paste(
                      "Plot and threshold the absolute value of the signal",
                      "(true/false), e.g. for audio that swings around zero.",
                      "Changing it does not re-run the thresholding: set",
                      "`threshold` again."
                    ),
                    tooltip = "Use the absolute value of the signal (e.g. audio)."
                  )
                ),
                shiny::column(
                  width = 6L,
                  shidashi::register_input(
                    shiny::selectInput(
                      ns("threshold_direction"),
                      "Threshold direction",
                      choices = c("Above", "Below")
                    ),
                    inputId = "threshold_direction",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "'Above': onsets where the plotted signal rises to the",
                      "threshold or above; 'Below': where it falls to the",
                      "threshold or below. Photodiodes can have either polarity:",
                      "check the plot and ask the user when unsure. Changing it",
                      "re-runs the thresholding."
                    ),
                    tooltip = "Whether onsets are where the signal goes above or below the threshold."
                  )
                ),
                shiny::column(
                  width = 6L,
                  shidashi::register_input(
                    shiny::numericInput(
                      ns("num_duration"),
                      "Minimum duration (s)",
                      value = 0, step = 1, min = 0
                    ),
                    inputId = "num_duration",
                    update = "shiny::updateNumericInput",
                    description = paste(
                      "Minimum duration (s): crossings separated by shorter gaps",
                      "merge into one onset, and new candidates within this many",
                      "seconds of staged onsets of the block are dropped (only",
                      "when above 0). Use about half the pause between trials:",
                      "shorter than the pause, longer than gaps inside a trial.",
                      "Changing it re-runs the thresholding."
                    ),
                    tooltip = "Crossings closer than this many seconds merge into one onset."
                  )
                ),
                shiny::column(
                  width = 12L,
                  shidashi::register_input(
                    shiny::numericInput(
                      ns("threshold"),
                      "Threshold",
                      value = NA, step = "any", updateOn = "blur"
                    ),
                    inputId = "threshold",
                    update = "shiny::updateNumericInput",
                    description = paste(
                      "Threshold on the plotted (transformed) signal: candidates",
                      "are where it crosses in `threshold_direction`. A",
                      "double-click on the plot fills it; empty means no",
                      "candidates. It is cleared when a new signal or subject is",
                      "loaded: wait until `shiny_input_info` shows it empty, then",
                      "set a number in a later call. Set `plot_absolute`,",
                      "`plot_difference`, `threshold_direction` and `num_duration`",
                      "first. To clear it send \"\" (never null). NEVER change it",
                      "only to reach the number of trials the user expects: report",
                      "the mismatch and ask (reversed polarity, threshold too low,",
                      "other events, or trials this channel cannot mark). Read",
                      "results with script `epoch_status`; script `threshold_scan`",
                      "helps choose. Manual: skill `rave-module`, reference",
                      "`references/epoch_generator.md`."
                    ),
                    tooltip = "Signal value that marks a trial onset: double-click the plot, or type a number and press Enter."
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
              tools = list(shidashi::register_input(
                shidashi::card_tool(
                  inputId = ns("download_btn1"),
                  widget = "custom",
                  icon = ravedash::shiny_icons$download
                ),
                inputId = "download_btn1",
                update = "shiny::updateActionLink",
                description = paste(
                  "[Card tool, read-only] Opens the 'Preview & export' dialog,",
                  "whose 'Save & Download' is for people only. Agents save with",
                  "script `save_agent_epoch` instead."
                ),
                tooltip = "Preview the staged epoch, then save it to the subject and download it.",
                writable = FALSE
              )),
              footer = shiny::div(
                class = "fill-width text-align-right",
                shidashi::register_input(
                  shiny::actionLink(
                    ns("download_btn2"),
                    icon = ravedash::shiny_icons$download,
                    "Save & download staged table to subject's meta folder"
                  ),
                  inputId = "download_btn2",
                  update = "shiny::updateActionLink",
                  description = paste(
                    "[Link, read-only] Opens the 'Preview & export' dialog, whose",
                    "'Save & Download' is for people only. Agents save with",
                    "script `save_agent_epoch` instead."
                  ),
                  tooltip = "Preview the staged epoch, then save it to the subject and download it.",
                  writable = FALSE
                )
              ),
              class_body = "padding-10",
              ravedash::flex_group_box(
                title = "Graphical visualization",

                shidashi::flex_item(
                  size = 1, class = "overflow-hidden",
                  title = "Press down, hold, and draw a time-range to see details",
                  # MIGRATED: removed ravedash::output_gadget_container() wrapper
                  # ravedash::output_gadget_container(
                    shiny::plotOutput(
                      outputId = ns("plot_overall"),
                      width = "100%", height = "200px",
                      click = NULL,
                      dblclick = shiny::dblclickOpts(ns("plot_overall__dblclick"),
                                                     clip = TRUE),
                      brush = shiny::brushOpts(ns("plot_overall__brush"), resetOnNew = TRUE,
                                               clip = TRUE, direction = "x")
                    )
                  # )

                ),
                shidashi::flex_item(
                  size = 1, class = "overflow-hidden",

                  # MIGRATED: removed ravedash::output_gadget_container() wrapper
                  # ravedash::output_gadget_container(
                    shiny::plotOutput(
                      outputId = ns("plot_subset"),
                      width = "100%", height = "200px"
                    )
                  # )

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
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("toggle_selection"),
                      "Toggle selection",
                      width = "100%"
                    ),
                    inputId = "toggle_selection",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "[Action button] 'Toggle selection' inverts the selection",
                      "of candidate rows: unselected rows become selected and",
                      "selected rows unselected (all rows when none are",
                      "selected). To stage all candidates: click",
                      "`clear_selection`, wait until `epoch_status` shows 0",
                      "selected, click this once, wait until all are selected,",
                      "then run script `stage_selected`. Needs the module page to",
                      "be visible."
                    ),
                    tooltip = "Invert the selection: select the unselected rows and unselect the selected ones (all rows when none are selected)."
                  ),
                  shiny::div(class = "padding-top-5"),
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("clear_selection"),
                      "Clear selection",
                      width = "100%"
                    ),
                    inputId = "clear_selection",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "[Action button] 'Clear selection' unselects all candidate",
                      "rows; `epoch_status` then shows 0 selected."
                    ),
                    tooltip = "Unselect all candidate rows."
                  ),
                  shiny::hr(),
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("save_changes"),
                      "Stage selected",
                      icon = ravedash::shiny_icons$arrow_right,
                      width = "100%"
                    ),
                    inputId = "save_changes",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "[Action button, read-only] 'Stage selected' moves the",
                      "selected candidates into the staged table. Agents run",
                      "script `stage_selected` instead."
                    ),
                    tooltip = "Move the selected candidates into the staged table.",
                    writable = FALSE
                  ),
                  shiny::div(class = "padding-top-5"),
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("undo_changes"),
                      "Unstage selected",
                      icon = ravedash::shiny_icons$arrow_left,
                      width = "100%"
                    ),
                    inputId = "undo_changes",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "[Action button, read-only] 'Unstage selected' removes the",
                      "rows selected in the staged table. Agents run script",
                      "`unstage_selected` instead; people select those rows."
                    ),
                    tooltip = "Remove the selected rows from the staged table.",
                    writable = FALSE
                  ),
                  shiny::div(class = "padding-top-5"),
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("undo_all"),
                      "Unstage all",
                      width = "100%"
                    ),
                    inputId = "undo_all",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "[Action button, read-only] 'Unstage all' removes the",
                      "staged onsets of the selected block. Agents run script",
                      "`unstage_all` instead."
                    ),
                    tooltip = "Remove the staged onsets of the current block.",
                    writable = FALSE
                  ),
                  shiny::hr(),
                  shiny::div(
                    class = "fill-width",
                    "<Danger>"
                  ),
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("discard_epoch"),
                      "Discard epoch",
                      width = "100%"
                    ),
                    inputId = "discard_epoch",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "[Action button, read-only] 'Discard epoch' removes the",
                      "staged onsets of every block. Agents run script",
                      "`discard_epoch` instead."
                    ),
                    tooltip = "Remove the staged onsets of every block.",
                    writable = FALSE
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
