rave_option_server <- function(input, output, session){

  ns <- session$ns
  max_cores <- dipsaus::detectCores()

  local_reactives <- shiny::reactiveValues(
    refresh = NULL
  )

  output$snapshot <- shiny::renderPrint({
    local_reactives$refresh

    session_info <- utils::sessionInfo()

    blas <- session_info$BLAS
    if (is.null(blas)) { blas <- "" }
    lapack <- session_info$LAPACK
    if (is.null(lapack)) { lapack <- "" }

    raw_dir <- raveio::raveio_getopt("raw_data_dir", default = "<Missing>")
    data_dir <- raveio::raveio_getopt("data_dir", default = "<Missing>")
    cache_dir <- raveio::cache_root()

    healthy_directory <- function(path){
      ifelse(dir.exists(path), "[healthy]", "[unable to reach]")
    }

    package_ver <- function(name, version_only = FALSE) {
      suppressWarnings({
        if(name %in% names(session_info$otherPkgs)) {
          desc <- session_info$otherPkgs[[name]]
        } else if(name %in% names(session_info$loadedOnly)) {
          desc <- session_info$loadedOnly[[name]]
        } else {
          desc <- utils::packageDescription(name, drop = TRUE)
        }
      })

      if(!inherits(desc, "packageDescription")) { return(NULL) }

      if(version_only) {
        return(desc$Version)
      }
      sprintf("%s [%s]", desc$Package, desc$Version)

    }


    # get basic information
    cat(
      sep = "",
      "Operating system:\n",
      "  OS type:         ", session_info$platform, "\n",
      "  OS name:         ", session_info$running, "\n",
      "  File separator:  ", .Platform$file.sep, "\n",
      "  Endianess:       ", .Platform$endian, "\n",

      "\nR information\n",
      "  Version:         ", session_info$R.version$version.string, "\n",
      "  Architecture:    ", session_info$R.version$arch, "\n",
      "  Matrix products: ", session_info$matprod, "\n",
      local({
        if (blas == lapack && nzchar(blas))
          c("  BLAS/LAPACK:     ", blas, "\n", sep = "")
        else {
          if (nzchar(blas)) {
            c("  BLAS:            ", blas, "\n", sep = "")
          }
          if (nzchar(lapack)) {
            c("  LAPACK:          ", lapack, "\n", sep = "")
          }
        }
      }),

      "\nRAVE status\n",
      "  Version:        ", package_ver("rave", version_only = TRUE), "\n",
      "  Template brain: ", raveio::raveio_getopt('threeBrain_template_subject', default = "N27"), "\n",
      "  Directories:    ",
      "Raw ", healthy_directory(raw_dir), ", ",
      "Main ", healthy_directory(data_dir), ", ",
      "Session ", healthy_directory(cache_dir), "\n",

      "  Core dependencies: \n",
      "    ",
      package_ver("ravemanager"), ", ",
      package_ver("rave"), ", ",
      package_ver("ravetools"), ", ",

      "\n    ",
      package_ver("filearray"), ", ",
      package_ver("shidashi"), ", ",
      package_ver("future"), ", ",

      "\n    ",
      package_ver("rpymat"), ", ",
      package_ver("dipsaus"), ", ",
      package_ver("threeBrain"), ", ",

      "\n    ",
      package_ver("shiny"), ", ",
      package_ver("targets"), ", ",
      package_ver("raveio"), ", ",

      "\n    ",
      package_ver("ravedash"), ", ",
      package_ver("readNSx"), ", ",
      package_ver("rpyANTs"), ", ",

      "\n    ",
      package_ver("freesurferformats"), ", ",
      package_ver("rutabaga"), ", ",
      package_ver("ravebuiltins"), ", ",


      ""
    )




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
