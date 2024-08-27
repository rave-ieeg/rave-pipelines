

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
              title = "Localization Plan Details",
              shiny::uiOutput(
                outputId = ns("label_selectors_wrapper")
              ),
              shiny::hr(),
              shiny::span(
                shiny::actionLink(ns("action_reset_btn"), "Reset current group"),
                " | ",
                shiny::actionLink(ns("action_reset_fslabel_btn"), "Re-compute FreeSurfer labels")
              ),
              shiny::div(
                class = "overflow-auto max-height-vh50",
                DT::dataTableOutput(outputId = ns("group_table"), width = "auto")
              ),
              shiny::uiOutput(ns("fsindex_selector")),
              footer = shiny::tagList(
                shiny::a(class = "toggle-advance-options", href="#", "Show/Hide localization instructions"),
                shiny::uiOutput(ns("instruction_text"), container = function(..., class = ""){
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
              'YAEL 3D Viewer',
              class_body = "no-padding fill-width height-vh80 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                ravedash::output_gadget_container(
                  threeBrain::threejsBrainOutput(
                    outputId = ns("localization_viewer"),
                    width = '100%',
                    height = "100%"
                  )
                )
              )
            )
          )
        )
      )

    )
  )
}
