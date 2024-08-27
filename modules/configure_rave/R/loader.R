
# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(

      shiny::column(
        width = 12L,
        ravedash::output_cardset(
          title = "Basic Configurations",
          inputId = ns("basic_settings"),

          class_body = "min-height-100",

          `Settings` = shiny::fluidRow(

            shiny::column(
              width = 12L,

              shiny::fluidRow(


                shiny::column(
                  width = 6L,

                  shinyWidgets::searchInput(
                    inputId = ns("raw_data_dir"),
                    label = "Raw data directory",
                    value = raveio::raveio_getopt("raw_data_dir", default = ""),
                    placeholder = "Root folder containing raw signals & imaging data",
                    btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
                    width = "100%"
                  ),
                  shinyWidgets::searchInput(
                    inputId = ns("data_dir"),
                    label = "Main data directory",
                    value = raveio::raveio_getopt("data_dir", default = ""),
                    placeholder = "Where RAVE should store its generated files",
                    btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
                    width = "100%"
                  ),
                  shinyWidgets::searchInput(
                    inputId = ns("temp_dir"),
                    label = "Session & cache directory",
                    value = raveio::raveio_getopt("tensor_temp_path", default = ""),
                    placeholder = "Removable temporary session files",
                    btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
                    width = "100%"
                  ),


                ),


                shiny::column(
                  width = 6L,


                  shinyWidgets::searchInput(
                    inputId = ns("template_subject"),
                    label = "Template Brain",
                    value = raveio::raveio_getopt("threeBrain_template_subject",
                                                  default = "N27"),
                    placeholder = "N27, fsaverage, bert, ...",
                    btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
                    width = "100%"
                  ),

                  shinyWidgets::searchInput(
                    inputId = ns("max_worker"),
                    label = "Max parallel cores",
                    value = raveio::raveio_getopt("max_worker", default = 1L),
                    placeholder = "Recommended 2GB RAM per CPU core",
                    btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
                    width = "100%"
                  ),

                  shinyWidgets::switchInput(
                    inputId = ns("allow_fork_clusters"),
                    label = "Allow forked process",
                    labelWidth = "100%",width = "100%",
                    onStatus = "success",
                    offStatus = "danger",
                    onLabel = "Enabled",
                    offLabel = "Disabled",
                    value = isFALSE(raveio::raveio_getopt("disable_fork_clusters", default = FALSE))
                  )

                )

              ),


              shiny::fluidRow(

                shiny::column(
                  width = 12L,

                  shiny::hr()
                ),
                shiny::column(
                  width = 12L,

                  shiny::div(
                    class = "rave-optional soft-hidden",
                    shiny::tags$pre(
                      .noWS = c("outside", "after-begin", "before-end"),
                      shiny::tags$code(
                        id = ns("basic-config"),
                        class = "shiny-text-output hljs-literal",
                        style = "word-wrap:break-word;width: 100%;white-space: pre-wrap;",
                        .noWS = c("outside", "after-begin", "before-end")
                      )
                    )
                  ),

                  shiny::p(
                    shiny::a(
                      href = sprintf("#%s", ns("basic-config")),
                      class = "toggle-advance-options",
                      "Show/Hide nerd stats"
                    )
                  )

                )
              )

            )

          ),


          `Installed R Packages` = shiny::fluidRow(

            shiny::column(
              width = 12L,

              DT::DTOutput(ns("basic-packages"))

            )
          )

        )
      ),

      shiny::column(
        width = 12L,

        ravedash::output_card(
          title = "Python Configurations",
          inputId = ns("python_settings"),

          class_body = "min-height-100",

          shiny::fluidRow(
            shiny::column(
              width = 12L,

              dipsaus::actionButtonStyled(
                inputId = ns("btn_update_python"),
                label = "Check Python status"
              ),
              dipsaus::actionButtonStyled(
                inputId = ns("btn_config_python"),
                label = "Configure python for RAVE",
                type = "default"
              ),
              shiny::hr()
            ),

            shiny::column(
              width = 12L,

              DT::DTOutput(ns("python-packages")),

              shiny::hr(class = "margin-top-40"),
            ),

            shiny::column(
              width = 12L,

              shiny::div(
                class = "rave-optional soft-hidden",
                shiny::tags$pre(
                  .noWS = c("outside", "after-begin", "before-end"),
                  shiny::tags$code(
                    id = ns("python-config"),
                    class = "shiny-text-output hljs-literal noplaceholder",
                    style = "word-wrap:break-word;width: 100%;white-space: pre-wrap;",
                    .noWS = c("outside", "after-begin", "before-end")
                  )
                )
              ),

              shiny::p(
                shiny::a(
                  href = sprintf("#%s", ns("python-config")),
                  class = "toggle-advance-options",
                  "Show/Hide nerd stats"
                )
              )
            )
          ),

          footer = shiny::fluidRow(
            shiny::column(
              width = 12L,
              shidashi::flex_container(
                shidashi::flex_item(
                  size = 6L,
                  shiny::textInput(
                    inputId = ns("python_install_packages"),
                    label = "Packages to install (separated by comma)",
                    placeholder = "e.g. pandas, antspyx>=0.3.8, pynwb==2.3.1",
                    width = "100%"
                  )
                ),
                shidashi::flex_item(
                  size = 2L,
                  shiny::selectInput(
                    inputId = ns("python_install_use_pip"),
                    label = "Use pip3/conda",
                    choices = c("conda", "pip3"),
                    width = "100%"
                  )
                ),
                shidashi::flex_item(
                  size = 2L,
                  shiny::textInput(
                    inputId = ns("python_install_conda_channel"),
                    label = "Conda channel",
                    placeholder = "e.g. conda-forge",
                    width = "100%"
                  )
                ),
                shidashi::flex_item(
                  size = 2L,
                  shiny::div(
                    style = "margin-bottom:0.8rem; margin-top:2rem;",
                    shiny::actionButton(
                      inputId = ns("python_install_do"),
                      label = "Add/Update",
                      width = "100%"
                    )
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


# Server functions for loader
loader_server <- function(input, output, session, ...){

  max_cores <- dipsaus::detectCores()
  local_reactives <- shiny::reactiveValues()

  output[["basic-config"]] <- shiny::renderPrint({
    local_reactives$update_basic_info
    get_basic_info()
  })


  load_py_info <- function() {
    ravedash::show_notification(
      "Getting Python configurations. Please wait...",
      autohide = FALSE,
      close = FALSE,
      class = ns("py-notif")
    )
    on.exit({
      Sys.sleep(0.5)
      ravedash::clear_notifications(class = ns("py-notif"))
    })


    msg <- utils::capture.output({
      python_info <- pipeline$run("python_summary", type = "callr")
      if(is.list(python_info)) {
        cat(format(python_info$summary), sep = "\n")
      }
    }, type = "output")

    if(!is.list(python_info) || isFALSE(python_info$summary)) {
      python_config <- FALSE
    } else {
      python_config <- dipsaus::ansi_strip(msg)
    }

    local_reactives$python_config <- python_config

    if(isFALSE(python_config)) {
      local_reactives$python_package_table <- NULL
    } else {
      local_reactives$python_package_table <- python_info$packages
    }

  }

  shiny::bindEvent(
    ravedash::safe_observe({

      load_py_info()

    }, error_wrapper = "notification"),
    input$btn_update_python,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output[["python-config"]] <- shiny::renderPrint({
    python_config <- local_reactives$python_config

    shiny::validate(
      shiny::need(
        length(python_config) > 0,
        message = "Please click button [Check Python status] to start..."
      )
    )
    if(isFALSE(python_config)) {
      cat("Python is not configured for RAVE yet...\n")
    } else {
      cat(python_config, sep = "\n")
    }
  })

  output[["python-packages"]] <- DT::renderDT({
    python_package_table <- local_reactives$python_package_table
    shiny::validate(
      shiny::need(
        is.data.frame(python_package_table),
        message = "Please click button [Check Python status] to start..."
      )
    )

    DT::datatable(python_package_table, class = "display nowrap compact",
                  selection = "none")

  })

  shiny::bindEvent(
    ravedash::safe_observe({
      pkgs <- unlist(strsplit(input$python_install_packages, ","))
      pkgs <- trimws(pkgs)
      pkgs <- pkgs[pkgs != ""]
      if(!length(pkgs)) {
        stop("No package to install. Please specify the packages.")
      }

      use_pip <- identical(input$python_install_use_pip, "pip3")
      conda_channel <- trimws(paste(input$python_install_conda_channel, collapse = ""))
      if( !nzchar(conda_channel) ) {
        conda_channel <- character()
      }

      python_config <- local_reactives$python_config
      if(!length(python_config)) {
        load_py_info()
        python_config <- local_reactives$python_config
      }

      promise <- ravedash::with_log_modal(
        title = "Adding Python/Conda packages",
        expr = bquote({
          if( isFALSE(.(python_config)) ) {
            cat("Configuring Python for RAVE (might take a while)...\n")
            # configure python
            ravemanager::configure_python()
          }

          cat("Installing package(s)\n")
          rpymat::add_packages(packages = .(pkgs), pip = .(use_pip), channel = .(conda_channel))
        }),
        quoted = TRUE
      )

      promise$then(function(...) {

        try({
          load_py_info()
          shidashi::show_notification(
            message = "Installation successful! You can find the installed packages in [Installed Packages] tab.", title = "Success!", type = "success", close = TRUE, autohide = TRUE, icon = ravedash::shiny_icons$tools
          )
        })

      })

      return()


    }, error_wrapper = "alert"),
    input$python_install_do,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({

      promise <- ravedash::with_log_modal(
        title = "Configuring Python for RAVE",
        expr = {
          ravemanager::configure_python()
          TRUE
        }
      )

      promise$then(function(...) {
        try({
          load_py_info()
        })
      })

      return()


    }, error_wrapper = "alert"),
    input$btn_config_python,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output[["basic-packages"]] <- DT::renderDT({
    tbl <- as.data.frame(utils::installed.packages(), row.names = FALSE)

    tbl <- tbl[, c("Package", "LibPath", "Version", "Built")]

    DT::datatable(tbl, class = "display nowrap compact",
                  selection = "none")

  })

  register_path_input <- function(input_id, opt_key, opt_name, ok_ifnot_exists = FALSE) {
    # require(dipsaus); session %?<-% shiny::MockShinySession$new()
    sv <- shinyvalidate::InputValidator$new(session = session)

    sv$add_rule(paste0(input_id, "_text"), function(value) {
      if(length(value) != 1 || is.na(value) || trimws(value) == '') {
        return(sprintf(
          "Path to [%s] is blank. Please enter a valid path",
          opt_name
        ))
      }
      if(!ok_ifnot_exists && !dir.exists(value)) {
        return(sprintf(
          "Path to [%s] does not exists",
          opt_name
        ))
      }
      return(NULL)
    }, session. = session)

    sv$enable()

    shiny::bindEvent(
      ravedash::safe_observe({
        if(!sv$is_valid()) { return() }
        val <- normalizePath(input[[input_id]])
        ravedash::logger(
          "Trying to set RAVE option [{opt_key}] <- {val}",
          level = "debug", use_glue = TRUE
        )
        raveio::raveio_setopt(opt_key, value = val, .save = TRUE)

        current_val <- raveio::raveio_getopt(opt_key)

        ravedash::logger("RAVE option [{opt_key}] is set: {current_val}", level = "info",  use_glue = TRUE)
        shidashi::show_notification(
          title = "RAVE option",
          message = sprintf(
            "RAVE option %s (%s) has been set to: %s",
            opt_name, opt_key, current_val
          ), type = "success", close = TRUE, autohide = TRUE,
          icon = ravedash::shiny_icons$tools, class = ns("opt_notif")
        )

        shinyWidgets::updateSearchInput(
          session = session,
          inputId = input_id,
          value = current_val,
          trigger = FALSE
        )
        local_reactives$refresh <- Sys.time()

      }),
      input[[input_id]],
      ignoreNULL = TRUE, ignoreInit = TRUE
    )

  }

  register_path_input(input_id = "raw_data_dir",
                      opt_key = "raw_data_dir",
                      opt_name = "raw data")

  register_path_input(input_id = "data_dir",
                      opt_key = "data_dir",
                      opt_name = "main data")

  register_path_input(input_id = "temp_dir",
                      opt_key = "tensor_temp_path",
                      opt_name = "session data",
                      ok_ifnot_exists = TRUE)

  local({
    sv <- shinyvalidate::InputValidator$new(session = session)

    sv$add_rule(inputId = "max_worker_text", function(value) {
      suppressWarnings({
        value <- as.integer(value)
      })
      if(length(value) != 1 || is.na(value)) {
        return("Invalid CPU cores: must be an integer")
      }
      if(value < 1 || value > max_cores) {
        return(sprintf("Invalid CPU cores: must be an integer from 1 to %.0f", max_cores))
      }
      return()
    }, session. = session)

    sv$enable()

    shiny::bindEvent(
      ravedash::safe_observe({
        if(!sv$is_valid()) { return() }
        max_worker <- as.integer(input$max_worker)
        if(!length(max_worker) || is.na(max_worker) ||
           max_worker < 1 || max_worker > max_cores) {
          return()
        }


        ravedash::logger(
          "Trying to set RAVE option [max_worker] <- {max_worker}",
          level = "debug", use_glue = TRUE
        )
        raveio::raveio_setopt("max_worker", max_worker)

        current_val <- raveio::raveio_getopt("max_worker")

        ravedash::logger("RAVE option [max_worker] is set: {current_val}",
                         level = "info",  use_glue = TRUE)
        shidashi::show_notification(
          title = "RAVE option",
          message = sprintf(
            "RAVE option: max CPU cores allowed to use (max_worker) has been set to: %s", current_val
          ), type = "success", close = TRUE, autohide = TRUE,
          icon = ravedash::shiny_icons$tools, class = ns("opt_notif")
        )

        shinyWidgets::updateSearchInput(
          session = session, inputId = "max_worker",
          value = as.character(current_val), trigger = FALSE
        )
        local_reactives$refresh <- Sys.time()


      }),
      input$max_worker,
      ignoreNULL = TRUE, ignoreInit = TRUE
    )
  })


  shiny::bindEvent(
    ravedash::safe_observe({
      v <- input$allow_fork_clusters
      if(length(v) != 1) { return() }
      disable_fork_clusters <- !v

      ravedash::logger(
        "Trying to set RAVE option [disable_fork_clusters] <- {disable_fork_clusters}",
        level = "debug", use_glue = TRUE
      )
      raveio::raveio_setopt("disable_fork_clusters", disable_fork_clusters)

      current_val <- raveio::raveio_getopt("disable_fork_clusters", default = FALSE)

      ravedash::logger("RAVE option [disable_fork_clusters] is set: {current_val}",
                       level = "info",  use_glue = TRUE)

    }),
    input$allow_fork_clusters,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  get_available_templates <- local({
    templates <- NULL
    function(){
      if(is.null(templates)) {
        templates <<- threeBrain::available_templates()
      }
      templates
    }
  })

  shiny::bindEvent(
    ravedash::safe_observe({
      template_subject <- input$template_subject
      if(!length(template_subject)) { return() }
      if(is.na(template_subject)) { return() }

      template_subject <- gsub("[^a-zA-Z0-9_-]", "", template_subject)
      if( !nchar(template_subject) ) {
        shinyWidgets::updateSearchInput(
          session = session, inputId = "template_subject",
          value = "", trigger = FALSE
        )
        return()
      }

      ravedash::logger(
        "Trying to set RAVE option [threeBrain_template_subject] <- {template_subject}",
        level = "debug", use_glue = TRUE
      )

      # check path
      root_path <- threeBrain::default_template_directory()
      path <- file.path(root_path, template_subject)

      if(dir.exists(path)) {
        raveio::raveio_setopt("threeBrain_template_subject", value = template_subject)
        shidashi::show_notification("New template is set!", title = "Succeed!", type = "success")
      } else {
        templates <- get_available_templates()
        if(template_subject %in% names(templates)) {

          timeout <- getOption("timeout")
          shidashi::show_notification(
            message = sprintf(
              "Template [%s] is missing. Trying to download... Please wait",
              template_subject
            ),
            title = "Downloading template",
            class = ns("download_notif"),
            delay = 30000,
            type = "default"
          )
          options("timeout" = 100000)
          on.exit({
            shidashi::clear_notifications(class = ns("download_notif"))
            options("timeout" = timeout)
          })
          template_subject <- tryCatch({
            threeBrain::download_template_subject(subject_code = template_subject,
                                                  url = templates[[template_subject]],
                                                  template_dir = root_path)
            raveio::raveio_setopt("threeBrain_template_subject", value = template_subject)
            shidashi::show_notification("New template is set!", title = "Succeed!", type = "success")
            template_subject
          }, error = function(e){
            old_template <- raveio::raveio_getopt("threeBrain_template_subject",
                                                  default = "N27")
            shidashi::show_notification(sprintf(
              "Cannot download template subject [%s] due to the following reason: \n'%s'. Rewinding to previous subject [%s]",
              template_subject, paste(e$message, collapse = ""), old_template
            ), title = "Fail to download the template", type = 'danger')
            old_template
          })
        } else {
          shidashi::show_notification(sprintf(
            "Cannot set template subject [%s]. Please check your template folder and make sure this name is correct", template_subject
          ), title = "Fail to set the template", type = 'danger')
          return()
        }
      }

      ravedash::logger("RAVE option [threeBrain_template_subject] is set: {template_subject}", level = "info",  use_glue = TRUE)

      shinyWidgets::updateSearchInput(
        session = session, inputId = "template_subject",
        value = template_subject, trigger = FALSE
      )
      local_reactives$refresh <- Sys.time()

    }),
    input$template_subject,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )


}
