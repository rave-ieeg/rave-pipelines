add_step <- function(title, ui, observe_inputs = NULL, check, quoted = FALSE, isolate = length(observe_inputs) > 0,
                     ignoreNULL = TRUE, ignoreInit = TRUE, env = parent.frame()) {

  if(!quoted){
    check <- substitute(check)
  }
  if(isolate){
    check <- bquote(shiny::isolate({ .(check) }))
  }
  observer <- shiny::observe(x = bquote({
    res <- tryCatch({
      .(check)
    }, error = function(e){
      e
    })
    if(inherits(res, 'error')){
      map[[.(as.character(title))]] <- list(
        pass = FALSE,
        error = res,
        timestamp = Sys.time()
      )
    } else {
      map[[.(as.character(title))]] <- list(
        pass = TRUE,
        message = res
      )
    }
  }), quoted = TRUE, env = env)

  if(length(observe_inputs)){

    observer <- shiny::bindEvent(observer, {
      watch_list <- lapply(observe_inputs, function(nm){
        input[[nm]]
      })
      if(.(ignoreNULL)){
        watch_list <- dipsaus::drop_nulls(watch_list)
      }
      if(!length(watch_list)){
        watch_list <- NULL
      }
      watch_list
    }, ignoreNULL = ignoreNULL, ignoreInit = ignoreInit)

  }

  if(is.function(ui)){
    render_function <- function(){
      ui()
    }
  } else {
    render_function <- function(){
      ui
    }
  }

  list(
    render = render_function,
    observer = observer
  )

}

renderSteps <- function(..., env = parent.frame(), session = shiny::getDefaultReactiveDomain()){

  steps <- rlang::enquos(...)
  nsteps <- length(steps)
  if(!nsteps){
    return()
  }
  env2 <- new.env(parent = env)
  env2$map <- shiny::reactiveValues()

  step_names <- sapply(steps, function(step){
    eval(rlang::call_args(step)[['title']], env)
  })

  # names(steps) <- step_names

  steps <- lapply(steps, function(step){
    shiny::withReactiveDomain(session, {
      step <- rlang::eval_tidy(rlang::call_modify(step, env = env2))
      step$observer
    })
    step
  })

  shiny::renderUI({

    ._pass <- TRUE
    lapply(seq_along(steps), function(ii){
      ._item <- env2$map[[step_names[[ii]]]]
      if(!._pass || !is.list(._item) || !length(._item$pass)) { return(NULL) }
      if(!isTRUE(._item)){
        print(._item)
        ._pass <<- FALSE
        cat(glue::glue("Stopped at [{ step_names[[ii]] }], reason: { ._item$error$message }\n", .trim = FALSE))
      }
      return(steps[[ii]]$render())
    })

  })

}
