

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
              title = "Localization Plan Details",
              shiny::uiOutput(
                outputId = ns("label_selectors_wrapper")
              ),
              shiny::hr(),
              shiny::span(
                shidashi::register_input(
                  shiny::actionLink(ns("action_reset_btn"), "Reset current group"),
                  inputId = "action_reset_btn",
                  update = "shiny::updateActionLink",
                  description = paste(
                    "[Link] 'Reset current group': removes every placed electrode of",
                    "the active group from the table and the 3D viewer (positions",
                    "back to 0), so the group can be placed again; the staged table",
                    "changes at the next RAVE! (script `run_analysis`). Destructive:",
                    "ask the user first."
                  ),
                  tooltip = "Remove the active group's electrodes to place them again."
                ),
                " | ",
                shidashi::register_input(
                  shiny::actionLink(ns("action_reset_fslabel_btn"), "Re-compute FreeSurfer labels"),
                  inputId = "action_reset_fslabel_btn",
                  update = "shiny::updateActionLink",
                  description = paste(
                    "[Link] 'Re-compute FreeSurfer labels': labels every electrode of",
                    "the active group again at its current position (labels picked",
                    "by hand are dropped). Writes nothing."
                  ),
                  tooltip = "Label the active group's electrodes again from their positions."
                )
              ),
              shiny::div(
                class = "overflow-auto max-height-vh50",
                DT::dataTableOutput(outputId = ns("group_table"), width = "auto")
              ),
              shiny::uiOutput(ns("fsindex_selector")),
              footer = shiny::tagList(
                shiny::a(class = "toggle-advance-options",
                         href = "#",
                         "Show/Hide localization instructions"),
                shiny::uiOutput(ns("instruction_text"), container = function(..., class = "") {
                  shiny::div(..., class = dipsaus::combine_html_class(class, "rave-optional soft-hidden"))
                })
              )
            )


          )
        )
      ),

      shiny::column(
        width = 8L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,
            ravedash::output_card(
              "YAEL 3D Viewer",
              class_body = "no-padding fill-width height-vh80 min-height-450 resize-vertical",
              shiny::div(
                class = "position-relative fill",
                # MIGRATED: removed ravedash::output_gadget_container() wrapper
                threeBrain::threejsBrainOutput(
                    outputId = ns("localization_viewer"),
                    width = "100%",
                    height = "100%"
                  )
              )
            )
          )
        )
      )

    )
  )
}
