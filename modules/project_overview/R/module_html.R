

module_html <- function(){

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 12L,
        shiny::uiOutput(
          outputId = ns("report_container"),
          inline = FALSE,
          fill = TRUE
        )
      # shiny::column(
      #   width = 9L,
      #   shiny::div(
      #     class = "row screen-height overflow-y-scroll output-wrapper",
      #     shiny::column(
      #       width = 12L,
      #       ravedash::output_card(
      #         'Collapsed over frequency',
      #         class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
      #         shiny::div(
      #           class = 'position-relative fill',
      #           shiny::plotOutput(ns("collapse_over_trial"), width = '100%', height = "100%")
      #         )
      #       )
      #     )
      #   )
      )

    )
  )
}
