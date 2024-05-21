library(shiny)

# Debug
if(FALSE){
  template_settings$set(
    'root_path' = "inst/template/"
  )
}


if(system.file(package = 'raveio') != ""){
  if(dir.exists("./_pipelines")) {
    raveio::pipeline_root(c("./_pipelines", ".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
  } else {
    raveio::pipeline_root(c(".", file.path(raveio:::R_user_dir('raveio', 'data'), "pipelines")))
  }
}

if(file.exists("prelaunch.R")) {
  source("prelaunch.R")
}

server <- function(input, output, session){

  # Sync input ID
  shared_data <- shidashi::register_session_id(session)
  # shared_data$enable_broadcast()
  # shared_data$enable_sync()

  # Set max upload file size to be 300MB by default
  if(!isTRUE(getOption('shiny.maxRequestSize', 0) > 0)) {
    options(shiny.maxRequestSize = 300 * 1024 ^ 2)
  }
  is_single_session <- isTRUE(getOption("ravedash.single.session", default = FALSE))

  # # Register bindings for compound input
  # dipsaus::registerInputBinding('textOutput', 'shiny', 'shiny.textOutput', update_function = NULL)

  # tools <- ravedash::register_rave_session(session = session)

  # Fixed usage, call modules
  shiny::bindEvent(
    ravedash::safe_observe({
      query_string <- session$clientData$url_search
      if(length(query_string) != 1) {
        query_string <- "/"
      }

      ravedash::logger("GET request: /{query_string}", level = "trace", use_glue = TRUE)

      # query_string <- "/?type=widget&output_id=aaaa&rave_id=NAXzMcGKxoqwFeCjswfX"
      query_list <- httr::parse_url(query_string)

      parse_env <- new.env(parent = globalenv())
      resource <- shidashi::load_module(request = list(QUERY_STRING = query_string),
                                        env = parse_env)
      if(resource$has_module){

        module_table <- shidashi::module_info()
        module_table <- module_table[module_table$id %in% resource$module$id, ]
        if(nrow(module_table)){
          group_name <- as.character(module_table$group[[1]])
          if(is.na(group_name)){
            group_name <- "<no group>"
          }
          if(system.file(package = "logger") != ''){
            ravedash::logger(
              level = "info",
              "Loading - { module_table$label[1] } ({group_name}/{ module_table$id })",
              use_glue = TRUE
            )
          }
          rave_action <- list(
            type = "active_module",
            id = module_table$id,
            label = module_table$label[1]
          )
          # ravedash::fire_rave_event(key = rave_action$type, value = rave_action)
          # ravedash::logger("[{rave_action$type}] (rave-action).", level = "trace", use_glue = TRUE)
          shiny::moduleServer(resource$module$id, function(input, output, session, ...){

            # ravedash::register_rave_session(session = session)

            # Register a common screen
            ravedash::module_server_common(
              resource$module$id,
              check_data_loaded = parse_env$check_data_loaded,
              session = session,
              parse_env = parse_env,
              ...
            )

            resource$module$server(input, output, session, ...)

          }, session = session)
        }
      } else {
        # No module, render rave_options
        if(!isTRUE(raveio::raveio_getopt(key = "secure_mode", default = FALSE))) {
          source("./R/rave-options.R", local = parse_env)
          shiny::moduleServer("._raveoptions_.", parse_env$rave_option_server)

          get_jupyter_configuration <- function() {
            re <- list()
            sess_root <- ravedash:::current_session_path()
            jupyter_confpath <- file.path(sess_root, "jupyter.yaml")
            re$confpath <- jupyter_confpath
            try({
              if(length(jupyter_confpath) == 1 && !is.na(jupyter_confpath) &&
                 file.exists(jupyter_confpath)) {
                jupyter_conf <- raveio::load_yaml(jupyter_confpath)
                port <- as.integer(jupyter_conf$port)
                if(isTRUE(!is.na(port) & port >= 1024 & port <= 65535)) {
                  re$port <- port
                  re$host <- jupyter_conf$host
                }
              }
            })
            re
          }
          shiny::bindEvent(
            ravedash::safe_observe({
              ravedash::shutdown_session(session = session, jupyter = TRUE)
            }),
            input$ravedash_shutdown_all,
            ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE
          )
          shiny::bindEvent(
            ravedash::safe_observe({
              ravedash::shutdown_session(session = session, jupyter = FALSE)
            }),
            input$ravedash_shutdown_rave,
            ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE
          )
          shiny::bindEvent(
            ravedash::safe_observe({
              conf <- get_jupyter_configuration()
              if(length(conf$confpath) != 1 || is.na(conf$confpath) ||
                 !file.exists(conf$confpath)) {
                return()
              }
              rpymat::jupyter_server_stop(conf$port)
              unlink(conf$confpath)
              shiny::removeModal()
              ravedash::show_notification(
                title = "JupyterLab server stopped",
                type = "warning",
                message = sprintf("JupyterLab instance at port %s has been shut down", conf$port),
                position = "bottomRight"
              )
            }),
            input$ravedash_shutdown_jupyter,
            ignoreNULL = TRUE, ignoreInit = TRUE
          )
          shiny::bindEvent(
            ravedash::safe_observe({
              on.exit({
                Sys.sleep(0.5)
                shiny::removeModal()
              })
              conf <- get_jupyter_configuration()

              if(length(conf$confpath) != 1 || is.na(conf$confpath)) {
                stop("Unable to obtain the RAVE session path. Please contact the RAVE support team to report this bug.")
              }
              jupyter_port <- as.integer(conf$port)
              if(!length(jupyter_port)) {
                jupyter_port <- raveio::raveio_getopt("jupyter_port", default = 17284L)
              }
              host <- conf$host
              if(!length(host)) {
                host <- "127.0.0.1"
              }
              jupyter_wd <- raveio::raveio_getopt('data_dir')

              ravedash::show_notification(
                title = "Starting JupyterLab-server",
                message = sprintf(
                  "Starting JupyterLab at %s:%s ... (This might take a while if this is the first time that you configure the Python environment for RAVE)", host, jupyter_port),
                close = FALSE, autohide = FALSE, position = "bottomRight", type = "default",
                class = "notif-start-jupyter"
              )

              if(dipsaus::package_installed("ravemanager")) {
                ravemanager::configure_python(verbose = FALSE)
              }

              rpymat::jupyter_check_launch(
                open_browser = FALSE, workdir = jupyter_wd,
                port = jupyter_port,
                host = host, async = TRUE)

              raveio::save_yaml(
                list(
                  host = host,
                  port = jupyter_port
                ),
                file = conf$confpath
              )

              ravedash::clear_notifications(class = "notif-start-jupyter")
              ravedash::show_notification(
                title = "JupyterLab server started",
                subtitle = "success!",
                type = "success",
                message = "A JupyterLab instance is running. You might need to refresh this webpage.",
                position = "bottomRight"
              )
            }, error_wrapper = "notification"),
            input$ravedash_start_jupyter,
            ignoreNULL = TRUE, ignoreInit = TRUE
          )

          shiny::bindEvent(
            ravedash::safe_observe({

              jupyter_conf <- get_jupyter_configuration()
              jupyter_port <- jupyter_conf$port
              if(!length(jupyter_port)) {
                jupyter_port <- raveio::raveio_getopt("jupyter_port", default = 17284L)
              }
              # check if jupyter_port is in server list
              jupyter_running <- FALSE
              try({
                tbl <- rpymat::jupyter_server_list()
                if(jupyter_port %in% tbl$port) {
                  jupyter_running <- TRUE
                }
              }, silent = TRUE)

              # shutdown jupyter UI
              if(jupyter_running) {
                start_jupyter_ui <- NULL
                shutdown_jupyter_ui <- shiny::actionButton(
                  inputId = "ravedash_shutdown_jupyter",
                  label = "Stop Jupyter"
                )
                shutdown_rave_ui <- shiny::actionButton(
                  inputId = "ravedash_shutdown_rave",
                  label = "Stop RAVE"
                )
                shutdown_all_ui <- dipsaus::actionButtonStyled(
                  inputId = "ravedash_shutdown_all",
                  label = "Stop RAVE + Jupyter",
                  icon = ravedash::shiny_icons[["power-off"]]
                )
              } else {
                shutdown_jupyter_ui <- NULL
                shutdown_all_ui <- NULL
                shutdown_rave_ui <- dipsaus::actionButtonStyled(
                  inputId = "ravedash_shutdown_rave",
                  label = "Stop RAVE",
                  icon = ravedash::shiny_icons[["power-off"]]
                )
                start_jupyter_ui <- shiny::actionButton(
                  inputId = "ravedash_start_jupyter",
                  label = "Start Jupyter"
                )
              }



              shiny::showModal(
                shiny::modalDialog(
                  title = "Power-On/Off RAVE Services",
                  easyClose = TRUE, size = "l",
                  footer = shiny::tagList(
                    shiny::modalButton("Cancel"),
                    shutdown_jupyter_ui,
                    start_jupyter_ui,
                    shutdown_rave_ui,
                    shutdown_all_ui
                  ),
                  local({
                    if(jupyter_running) {
                      shiny::tagList("* If you shut down RAVE only, the active Jupyter server will still run in the background. Please manually stop the Jupyter server via ", shiny::tags$span(class = "font-weight-bold", sprintf("rpymat::jupyter_server_stop(%s)", jupyter_port)))
                    } else {
                      "Please choose from the following options."
                    }
                  })

                )
              )
            }),
            input$ravedash_shutdown_btn,
            ignoreNULL = TRUE, ignoreInit = TRUE
          )

          if( is_single_session ) {
            ravedash::logger("Single-session mode is ON: closing the website tab will shutdown the RAVE application.", level = "info")
            session$onEnded(function() {
              shiny::stopApp()
            })
          } else {
            ravedash::logger("Single-session mode is OFF: closing the website tab will NOT shutdown the RAVE application. Please use the builtin shutdown button.", level = "info")
          }
        }
      }

    }),
    session$clientData$url_search, ignoreNULL = TRUE, label = "ravedash-SitePath"
  )

}
