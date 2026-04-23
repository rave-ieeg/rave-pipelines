

module_html <- function(){

  available_template_subjects <- names(threeBrain::available_templates())
  saved_template <- pipeline$get_settings(
    "template_subject",
    default = ravepipeline::raveio_getopt("threeBrain_template_subject", default = "cvs_avg35_inMNI152"),
    constraint = available_template_subjects
  )

  shiny::fluidPage(
    shiny::fluidRow(
      # ---- Left column: configuration inputs ----
      shiny::column(
        width = 3L,
        shiny::div(
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            # --- Card 1: Subject Selection ---
            ravedash::input_card(
              title = "Subject Selection",
              class_header = "",

              shidashi::flex_item(
                shidashi::register_input(
                  shiny::selectInput(
                    inputId = ns("subject_codes"),
                    label = "Subjects (blank = all)",
                    choices = character(0),
                    multiple = TRUE
                  ),
                  inputId = "subject_codes",
                  update = "shiny::updateSelectInput",
                  description = "Select subjects to include in the report (blank = all)"
                )
              )
            ),

            # --- Card 2: Group-Level Sections ---
            ravedash::input_card(
              title = "Group-Level Sections",
              class_header = "",

              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("group_viewer"),
                  label = "3D Group Viewer",
                  value = TRUE,
                  width = "100%"
                ),
                inputId = "group_viewer",
                update = "shiny::updateCheckboxInput",
                description = "Include group-level 3D brain viewer in the report"
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s']", ns("group_viewer")),
                shiny::div(
                  class = "padding-left-10",
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("template_subject"),
                      label = "Template brain",
                      choices = available_template_subjects,
                      selected = saved_template
                    ),
                    inputId = "template_subject",
                    update = "shiny::updateSelectInput",
                    description = "Template brain subject for the group 3D viewer"
                  )
                )
              ),

              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("electrode_coverage"),
                  label = "Electrode Coverage",
                  value = TRUE,
                  width = "100%"
                ),
                inputId = "electrode_coverage",
                update = "shiny::updateCheckboxInput",
                description = "Include electrode coverage table in the report"
              ),

              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("subjects_metadata"),
                  label = "Subjects Summary",
                  value = TRUE,
                  width = "100%"
                ),
                inputId = "subjects_metadata",
                update = "shiny::updateCheckboxInput",
                description = "Include subjects metadata summary table in the report"
              ),

              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("epoch_references"),
                  label = "Epoch & Reference Tables",
                  value = TRUE,
                  width = "100%"
                ),
                inputId = "epoch_references",
                update = "shiny::updateCheckboxInput",
                description = "Include epoch and reference tables in the report"
              )
            ),

            # --- Card 3: Subject-Level Sections ---
            ravedash::input_card(
              title = "Subject-Level Sections",
              class_header = "",

              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("module_reports"),
                  label = "Module Reports",
                  value = TRUE,
                  width = "100%"
                ),
                inputId = "module_reports",
                update = "shiny::updateCheckboxInput",
                description = "Include per-subject module reports table in the report"
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s']", ns("module_reports")),
                shiny::div(
                  class = "padding-left-10",
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("module_filter"),
                      label = "Filter by module (blank = all)",
                      choices = character(0),
                      multiple = TRUE
                    ),
                    inputId = "module_filter",
                    update = "shiny::updateSelectInput",
                    description = "Filter module reports by module name (blank = all modules)"
                  )
                )
              ),

              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("native_viewer"),
                  label = "Native 3D Viewer",
                  value = FALSE,
                  width = "100%"
                ),
                inputId = "native_viewer",
                update = "shiny::updateCheckboxInput",
                description = "Include per-subject native 3D brain viewers in the report (slow)"
              ),

              shidashi::register_input(
                shiny::checkboxInput(
                  inputId = ns("validation"),
                  label = "Validation",
                  value = FALSE,
                  width = "100%"
                ),
                inputId = "validation",
                update = "shiny::updateCheckboxInput",
                description = "Include per-subject data validation results in the report (slow)"
              ),

              footer = dipsaus::actionButtonStyled(
                inputId = ns("generate_btn"),
                label = "Generate Report",
                type = "primary",
                width = "100%"
              )
            )
          )
        )
      ),

      # ---- Right column: output cardsets ----
      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,

            # ---- Unified output cardset ----
            ravedash::output_cardset(
              inputId = ns("output_cardset"),
              title = "Project Overview",
              class_body = "no-padding min-height-vh70 height-vh80 resize-vertical",
              tools = list(
                shidashi::card_tool(
                  inputId = ns("output_cardset_config"),
                  title = "Toggle output configurations",
                  widget = "custom",
                  icon = ravedash::shiny_icons$cog
                )
              ),

              `Subject Summary` = shiny::div(
                class = "fill",
                DT::dataTableOutput(ns("subjects_summary_table"))
              ),
              `Module Reports` = shiny::div(
                class = "fill padding-5",
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shidashi::register_input(
                      shiny::checkboxInput(
                        inputId = ns("reports_latest_only"),
                        label = "Show only the most recent report per module & report name",
                        value = TRUE,
                        width = "100%"
                      ),
                      inputId = "reports_latest_only",
                      update = "shiny::updateCheckboxInput",
                      description = "When checked, show only the latest report per subject/module/name combination"
                    )
                  )
                ),
                DT::dataTableOutput(ns("module_reports_table"))
              ),
              `Electrode Coverage` = shiny::div(
                class = "fill",
                DT::dataTableOutput(ns("electrode_coverage_table"))
              ),
              `3D Viewer` = shiny::div(
                class = "fill",
                shiny::div(
                  class = "row soft-hidden",
                  id = ns("viewer_config_panel"),
                  shiny::column(
                    width = 4L,
                    shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns("brain_viewer_selector"),
                        label = "View",
                        choices = character(0),
                        selectize = FALSE
                      ),
                      inputId = "brain_viewer_selector",
                      update = "shiny::updateSelectInput",
                      description = "Select which brain to display: group brain or a specific subject's native brain"
                    )
                  )
                ),
                threeBrain::threejsBrainOutput(
                  ns("brain_widget"),
                  height = "100%"
                )
              ),
              `Epoch & References` = shiny::div(
                class = "fill",
                DT::dataTableOutput(ns("epoch_reference_table"))
              ),
              `Validation` = shiny::div(
                class = "fill",
                DT::dataTableOutput(ns("validation_table"))
              )
            )
          )
        )
      )
    )
  )
}
