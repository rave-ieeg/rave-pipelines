require(testthat)

# Unit tests for the root-level agent tools in `agents/tools`: what a script
# prints reaches the agent, and `shiny_ui_operate` sends the right messages to
# the browser. No app or browser is needed; modules/*/test-mcp.R drive a live
# app end to end.
source(testthat::test_path(
  "..", "..", "agents", "tools", "module_interactive_script.R"
), local = TRUE)
source(testthat::test_path(
  "..", "..", "agents", "tools", "shiny_ui_operate.R"
), local = TRUE)

# ---- capture_script_output --------------------------------------------------

test_that("capture_script_output returns the value and what the script printed", {
  captured <- capture_script_output({
    cat("to stdout\n")
    message("to message")
    cat("to stderr\n", file = stderr())
    cli::cli_alert_success("kernels completed")
    42
  })
  expect_equal(captured$value, 42)
  expect_null(captured$error)
  expect_match(captured$output, "to stdout")
  expect_match(captured$output, "to message")
  expect_match(captured$output, "to stderr")
  expect_match(captured$output, "kernels completed")
})

test_that("capture_script_output keeps the error and the output before it", {
  captured <- capture_script_output({
    message("before the error")
    stop("boom")
  })
  expect_s3_class(captured$error, "error")
  expect_equal(conditionMessage(captured$error), "boom")
  expect_null(captured$value)
  expect_match(captured$output, "before the error")
})

test_that("capture_script_output records warnings", {
  captured <- suppressWarnings(capture_script_output({
    warning("careful")
    1
  }))
  expect_match(captured$output, "Warning: careful")
})

test_that("capture_script_output restores the console sinks", {
  n_sinks <- sink.number()
  message_sink <- sink.number(type = "message")
  capture_script_output(message("x"))
  capture_script_output(stop("y"))
  capture_script_output(sink(textConnection("leaked", "w", local = TRUE)))
  expect_equal(sink.number(), n_sinks)
  expect_equal(sink.number(type = "message"), message_sink)
})

test_that("capture_script_output strips ANSI codes and keeps the end of long output", {
  captured <- capture_script_output({
    cat("\033[31mred\033[39m\n")
    cat(strrep("a", 50), "\n", sep = "")
    cat("the end\n")
  }, max_chars = 20)
  expect_false(grepl("\033", captured$output, fixed = TRUE))
  expect_lte(nchar(captured$output), 23)
  expect_match(captured$output, "the end$")
})

# ---- shiny_ui_operate -------------------------------------------------------

# A stand-in for a module session: records what the tool sends to the browser
fake_session <- function(module_id = "wavelet_module", inputs = character()) {
  session <- new.env()
  session$ns <- shiny::NS(module_id)
  session$input <- structure(as.list(seq_along(inputs)), names = inputs)
  session$sent <- list()
  session$sendCustomMessage <- function(type, message) {
    session$sent[[length(session$sent) + 1]] <- list(type = type, message = message)
  }
  session$sendModal <- function(type, message) {
    session$sent[[length(session$sent) + 1]] <- list(
      type = paste0("modal-", type), message = message)
  }
  session
}

operate <- function(session, ...) {
  tool <- shiny_ui_operate(session = session)[[1]]
  tool(...)
}

test_that("click on a module input sends shidashi.click with its namespaced ID", {
  session <- fake_session(inputs = "wavelet_confirm_btn2")
  reply <- operate(session, action = "click", target = "wavelet_confirm_btn2")
  expect_length(session$sent, 1)
  expect_equal(session$sent[[1]]$type, "shidashi.click")
  expect_equal(session$sent[[1]]$message$selector, "#wavelet_module-wavelet_confirm_btn2")
  expect_match(reply, "shiny_query_ui")
})

test_that("click also takes the input ID with the module prefix", {
  session <- fake_session(inputs = "wavelet_confirm_btn2")
  operate(session, action = "click", target = "wavelet_module-wavelet_confirm_btn2")
  expect_equal(session$sent[[1]]$message$selector, "#wavelet_module-wavelet_confirm_btn2")
})

test_that("click refuses a module input that is not on the page", {
  session <- fake_session(inputs = "wavelet_do_btn")
  expect_error(
    operate(session, action = "click", target = "wavelet_confirm_btn"),
    "not on the page"
  )
  expect_length(session$sent, 0)
})

test_that("click sends a CSS selector as it is", {
  session <- fake_session()
  operate(session, action = "click", target = ".swal-button")
  expect_equal(session$sent[[1]]$type, "shidashi.click")
  expect_equal(session$sent[[1]]$message$selector, ".swal-button")
})

test_that("click needs a target", {
  expect_error(operate(fake_session(), action = "click"), "target")
})

test_that("dismiss_modal closes the dialog", {
  session <- fake_session()
  operate(session, action = "dismiss_modal")
  expect_equal(session$sent[[1]]$type, "modal-remove")
})

test_that("show_notification stays until it is closed or removed", {
  session <- fake_session()
  operate(session, action = "show_notification", message = "Please review the dialog",
          title = "From the agent", type = "warning")
  sent <- session$sent[[1]]
  expect_equal(sent$type, "shidashi.show_notification")
  expect_equal(sent$message$body, "Please review the dialog")
  expect_equal(sent$message$title, "From the agent")
  expect_false(sent$message$autohide)
  expect_match(sent$message$class, "bg-warning")
  expect_match(sent$message$class, "wavelet_module-agent_notification")
})

test_that("show_notification needs a message", {
  expect_error(operate(fake_session(), action = "show_notification"), "message")
})

test_that("remove_notification removes notifications by CSS class, or all", {
  session <- fake_session()
  operate(session, action = "remove_notification", target = "wavelet_module-error_notif")
  operate(session, action = "remove_notification")
  expect_equal(session$sent[[1]]$type, "shidashi.clear_notification")
  expect_equal(session$sent[[1]]$message$selector, ".wavelet_module-error_notif.toast")
  expect_equal(session$sent[[2]]$message$selector, ".toast")
})

test_that("an unknown action is an error", {
  expect_error(operate(fake_session(), action = "scroll"), "action")
})
