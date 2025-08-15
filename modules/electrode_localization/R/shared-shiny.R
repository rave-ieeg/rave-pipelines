run_command_pipeline <- function(cmd, wait = TRUE, title = "Running Terminal Command", command = NULL,
                                 on_modal_removal = NULL,
                                 ..., session = shiny::getDefaultReactiveDomain()) {

  if(is.null(session)) {
    stop("`run_command_pipeline` must run in shiny context")
  }
  ns <- session$ns
  local_reactives <- shiny::reactiveValues()

  shidashi::clear_notifications(class = ns("error_notif"))

  session$output$verbatim_res <- shiny::renderText({
    paste(local_reactives$heads_on, collapse = "")
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      shiny::removeModal(session = session)
      local_reactives$heads_on <- NULL
      if(is.function(on_modal_removal)) {
        on_modal_removal()
      }
    }),
    session$input$dismiss_modal,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::showModal(shiny::modalDialog(
    title = title,
    size = "l", easyClose = FALSE,
    shidashi::flex_container(
      class = "fill-width max-height-500 overflow-y-auto",
      style = "flex-direction: column-reverse",
      shidashi::flex_item(
        shiny::verbatimTextOutput(ns("verbatim_log"), placeholder = TRUE)
      )
    ),
    footer = shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 12L,
          shiny::textOutput(ns("verbatim_res"))
        )
      ),
      dipsaus::actionButtonStyled(ns("dismiss_modal"), "Running...", disabled = "")
    )
  ))

  if( !length(command) ) {
    command <- cmd$command
    if(!length(command)) {
      command <- "bash"
    }
  }

  cmd$execute(dry_run = TRUE, backup = FALSE)

  renderMsg <- function(msg) {
    msg <- paste(msg, collapse = "\n")
    session$sendCustomMessage(
      "shidashi.set_html",
      list(
        selector = sprintf("pre#%s", ns("verbatim_log")),
        content = paste0(
          '<code class="hljs-literal" style="word-wrap:break-word;width: 100%;white-space: pre-wrap;">',
          msg,
          '</code>'
        )
      )
    )
  }

  if(dipsaus::get_os() == "windows") {
    wait <- TRUE
    renderMsg("Detected operating system - Windows... \nDue to technical issues, the console messages will not be displayed here interactively. Don't panic, the program has been scheduled. Please grab a cup of coffee and wait...\n\n\nEstimated run time: 5 min")
  }

  check <- dipsaus::rs_exec(bquote({
    script <- .(cmd$script)
    script_path <- .(cmd$script_path)
    log_path <- .(cmd$log_file)
    writeLines(c(
      "=================== Start: shell script ===================",
      script,
      paste("=================== Log:", Sys.time(), "===================")
    ), con = log_path)
    if(.(!wait)) {
      cat(script)
    }
    Sys.sleep(0.5)
    command <- .(command)
    cat("\n\n# Running above script using system command:", command)
    ravecore::cmd_execute(
      script = script,
      script_path = script_path,
      stdout = log_path,
      stderr = log_path,
      command = command
    )
  }), wait = wait, quoted = TRUE, name = title)

  final <- function(has_error = FALSE) {
    dipsaus::updateActionButtonStyled(
      session = session, inputId = 'dismiss_modal', disabled = FALSE,
      label = ifelse(has_error, "Gotcha", "Done"))
    ravedash::logger("Terminal command finished.", level = "info")

    local_reactives$heads_on <- paste(
      collapse = "",
      c("* The command seems to have finished. ",
        "However, RAVE does NOT know if the command finishes correctly. ",
        "Please check ", ifelse(
          has_error, "the log to see the error information.",
          "the full log, especially the last 10 lines."
        ))
    )

  }

  promise <- promises::promise(function(resolve, reject) {
    listener <- function() {
      code <- 0
      msg <- "Waiting for outputs..."
      try({
        if(is.function(check)) {
          code <- check()
        } else {
          code <- check
        }
        path <- cmd$log_file
        if(length(path) != 1 || is.na(path) || !file.exists(path) || path == '') {
          msg <- NULL
        } else {
          suppressWarnings({
            msg <- readLines(path)
          })
          if(!length(msg) || isTRUE(msg == "")) {
            msg <- "Waiting for outputs..."
          }
        }
      })

      if(code == 0) {
        renderMsg(c(msg, "Finished."))
        resolve(attr(code, "rs_exec_result"))
      } else if(code < 0) {
        renderMsg(c(msg, "An error is detected."))
        reject(1)
      } else {
        renderMsg(msg)
        later::later(listener, delay = 0.5)
      }
    }
    listener()
  })

  promises::then(
    promise,
    onFulfilled = function(...) {
      final(has_error = FALSE)
    },
    onRejected = function(...) {
      final(has_error = TRUE)
    }
  )
}


