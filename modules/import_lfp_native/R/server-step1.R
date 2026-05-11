module_html <- function() {}
module_server <- function(...) {}

loader_html <- function() {

  shiny::fixedPage(
    shiny::fixedRow(

      shiny::column(
        width = 12L,
        comp_import_setup$ui_func(),
      ),
      shiny::column(
        width = 12L,
        comp_import_format$ui_func()
      ),
      shiny::column(
        width = 12L,
        comp_import_channels$ui_func()
      )
    )
  )
}

loader_server <- function(input, output, session, ...) {
}
