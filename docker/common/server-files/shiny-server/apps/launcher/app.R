library(shiny)
library(ravedash)

# DIPSAUS DEBUG START
# print(shiny::shinyAppDir(appDir = "./docker/scripts/shiny-server/"))


rave_session_root <- normalizePath(
  Sys.getenv("RAVE_SERVER_ROOT", "session"),
  mustWork = FALSE
)

correct_max_workers <- function() {
  tryCatch({
    max_cores <- dipsaus::detectCores()
    current_workers <- raveio::raveio_getopt("max_worker", max_cores)
    if (current_workers > max_cores) {
      raveio::raveio_setopt("max_worker", max_cores)
    }
  }, error = function(e) {})
}

list_sessions <- function() {
  if (!dir.exists(rave_session_root)) {
    return(NULL)
  }
  sessions <- ravedash::list_session(
    order = "descend",
    path = rave_session_root
  )
  session_ids <- lapply(sessions, function(session) {
    session$session_id
  })
  unlist(session_ids)
}

ui <- shiny::basicPage(
  shiny::tags$head(
    shiny::tags$title("RAVE Session Manager"),
  ),
  shiny::includeCSS("www/css/intro.css"),
  shiny::div(
    id = "main-intro",
    shiny::div(
      class = "jumbotron jumbotron-fluid center-screen",
      shiny::fluidRow(
        shiny::column(
          width = 4L,
          shiny::img(
            src = "images/rave-round-logo-CC-BY-NC-SA.png",
            width = "100%"
          )
        ),
        shiny::column(
          width = 8L,
          shiny::h3("RAVE Session Manager"),
          shiny::hr(),
          shiny::p(
            "Please start a new session or continue an existing one."
          ),
          shiny::fluidRow(
            shiny::column(
              width = 8L,
              shiny::selectInput(
                inputId = "rave_session",
                label = NULL,
                width = "100%",
                multiple = FALSE,
                choices = c("[New session]", list_sessions())
              )
            ),
            shiny::column(
              width = 4L,
              dipsaus::actionButtonStyled(
                inputId = "start_session",
                width = "100%",
                label = "Launch",
                icon = ravedash::shiny_icons$rocket
              )
            )
          )
        )
      )
    )
  ),
  shiny::includeScript("www/js/rave-session-manager.js")
)

server <- function(input, output, session) {

  # get current session list
  local({
    session_opts <- c("[New session]", list_sessions())
    shiny::updateSelectInput(session, "rave_session", choices = session_opts)

    correct_max_workers()
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      session_id <- input$rave_session
      if (length(session_id) != 1) {
        return()
      }
      if (session_id == "[New session]") {
        if (!dir.exists(rave_session_root)) {
          dir.create(rave_session_root, showWarnings = FALSE, recursive = TRUE)
        }
        rave_sess <- ravedash::new_session(app_root = rave_session_root)
        session_id <- rave_sess$session_id
      }
      session$sendCustomMessage(
        type = "set_url",
        session_id
      )
    }),
    input$start_session,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

}

shiny::shinyApp(ui, server)
