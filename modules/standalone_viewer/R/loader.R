# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::uiOutput("viewer", container = function(...) {
    shiny::div(
      class = "no-padding no-margin",
      style = "width:100vw; height:100vh",
      ...
    )
  })
}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  root_session <- session$rootScope()

  # query_string <- "/?type=widget&output_id=plot_overall&rave_id=Pnd8MuxNVsZGcbrRWn8G&module=standalone_viewer"
  query_string <- session$clientData$url_search

  query_list <- httr::parse_url(query_string)

  # This is a widget that should belongs to some module
  output_id <- query_list$query$output_id
  if(length(output_id) != 1 || is.na(output_id) || !nchar(output_id)) {
    stop("The output ID is blank or invalid.")
  }

  ravedash::standalone_viewer(
    outputId = output_id, rave_id = query_list$query$rave_id,
    wrapper_id = "viewer"
  )



}
