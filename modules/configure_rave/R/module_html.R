

module_html <- function(){

  shiny::fixedPage(
    shiny::fluidRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Quick access",
              class_body = "padding-5",
              shiny::fluidRow(
                shiny::column(
                  width = 12L,
                  shiny::tags$ul(

                    shiny::tags$li(
                      shiny::actionLink(
                        inputId = ns("quickaccess_summary"),
                        label = "System summary"
                      )
                    ),

                    shiny::tags$li(
                      shiny::actionLink(
                        inputId = ns("quickaccess_paths"),
                        label = "Directory settings"
                      )
                    )

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
              'Collapsed over frequency',
              class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                shiny::plotOutput(ns("collapse_over_trial"), width = '100%', height = "100%")
              )
            )
          )
        )
      )

    )
  )
}
