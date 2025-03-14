library(shiny)
page_title <- function(complete = TRUE){
  if(complete){
    re <- "RAVE"
  } else {
    re <- "RAVE 2.0"
  }
  re
}
page_logo <- function(size = c("normal", "small", "large")){
  "shidashi/img/icon.png"
}
page_loader <- function(){
  # if no loader is needed, then return NULL
  shiny::div(
    class = "preloader flex-column justify-content-center align-items-center",
    shiny::img(
      class = "animation__shake",
      src = page_logo("large"),
      alt = "Logo", height="60", width="60"
    )
  )
}
body_class <- function(){
  unique(c(
    #--- Fix the navigation banner ---
    #"layout-navbar-fixed",

    #--- Collapse the sidebar at the beginning ---
    # "sidebar-collapse",

    #--- Let control sidebar open at the beginning ---
    # "control-sidebar-slide-open",

    #--- Fix the sidebar position ---
    "overflow-hidden",
    "layout-fixed",
    # "sidebar-collapse",

    #--- Default behavior when collapsing sidebar
    # "sidebar-mini", "sidebar-mini-md", "sidebar-mini-xs",

    #--- Start as dark-mode ---
    # "dark-mode",

    #--- Hide the navbar-nav-iframe
    "navbar-iframe-hidden",


    #--- Make scrollbar thinner ---
    # "fancy-scroll-y",

    get0("additional_body_class", ifnotfound = NULL)

  ))
}
nav_class <- function(){
  c(
    "main-header",
    "navbar",
    "navbar-expand-md",
    # "bg-blend-darken",
    # "navbar-dark",
    # "navbar-light",
    "navbar-primary",
    "border-bottom-0"
    # "no-padding"
  )
}

module_breadcrumb <- function(){}

frontpage <- function(){

  if(isTRUE(ravepipeline::raveio_getopt(key = "secure_mode", default = FALSE))) {
    return(shiny::tags$h2(
      class = "display-4",
      "Please choose any module to start"
    ))
  }


  ns <- shiny::NS(namespace = "._raveoptions_.")
  shiny::div(
    class = "screen-height overflow-y-scroll fill padding-top-20",
    shiny::div(
      class = "container",
      shiny::fixedRow(
        shiny::column(
          width = 7L,

          shidashi::card(
            title = "System snapshot",
            class_body = "padding-bottom-0 padding-5",
            # shiny::verbatimTextOutput(ns("snapshot"))
            shiny::pre(
              id = ns("snapshot"),
              class = "shiny-text-output noplaceholder",
              style = "word-wrap: break-word; word-break: break-word; white-space: break-spaces; background:transparent; "
            )
          )

        ),

        shiny::column(
          width = 5L,
          shidashi::card(
            title = "Preferences",
            class_body = "padding-10",

            shinyWidgets::searchInput(
              inputId = ns("raw_data_dir"),
              label = "Raw data directory",
              value = ravepipeline::raveio_getopt("raw_data_dir", default = ""),
              placeholder = "Root folder containing raw signals & imaging data",
              btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
              width = "100%"
            ),
            shinyWidgets::searchInput(
              inputId = ns("data_dir"),
              label = "Main data directory",
              value = ravepipeline::raveio_getopt("data_dir", default = ""),
              placeholder = "Where RAVE should store its generated files",
              btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
              width = "100%"
            ),
            shinyWidgets::searchInput(
              inputId = ns("temp_dir"),
              label = "Session directory",
              value = ravepipeline::raveio_getopt("tensor_temp_path", default = ""),
              placeholder = "Removable temporary session files",
              btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
              width = "100%"
            ),

            shinyWidgets::searchInput(
              inputId = ns("template_subject"),
              label = "Template Brain",
              value = ravepipeline::raveio_getopt("threeBrain_template_subject",
                                            default = "N27"),
              placeholder = "N27, fsaverage, bert, ...",
              btnSearch = shiny::tagList(ravedash::shiny_icons$arrow_right),
              width = "100%"
            ),

            shinyWidgets::searchInput(
              inputId = ns("max_worker"),
              label = "Max parallel cores",
              value = ravepipeline::raveio_getopt("max_worker", default = 1L),
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
              value = isFALSE(ravepipeline::raveio_getopt("disable_fork_clusters", default = FALSE))
            )

          )
        )
      )
    )
  )

}

