# agents/tools/shiny_ui_operate.R
#
# Root-level MCP tool: operate the module page as a person would, for what
# `shiny_input_update` cannot reach: buttons in a dialog that a script opened,
# alert buttons, the dialog itself, and notifications. Enable it per module in
# agents.yaml.

shiny_ui_operate <- shidashi::mcp_wrapper(

  function(session) {
    ellmer::tool(
      fun = function(action, target = "", message = "", title = "", type = "info") {
        target <- trimws(paste(target, collapse = ""))
        message <- paste(message, collapse = "")
        title <- paste(title, collapse = "")

        switch(
          action,
          "click" = {
            if (!nzchar(target)) {
              stop("Action `click` needs `target`: a module input ID (e.g. a button ID) or a CSS selector.")
            }
            prefix <- session$ns("")
            if (startsWith(target, prefix)) {
              target <- substring(target, nchar(prefix) + 1L)
            }
            if (grepl("^[A-Za-z][A-Za-z0-9_.-]*$", target)) {
              # A module input ID. Shiny keeps an input after its element is
              # gone (e.g. a closed dialog), so this check is not proof that
              # the element is on the page
              active <- shiny::isolate(names(session$input))
              if (!target %in% active) {
                stop(sprintf(paste(
                  "Input `%s` is not on the page (e.g. the dialog that holds",
                  "it is not open). Open it first, or pass a CSS selector."
                ), target))
              }
              selector <- sprintf("#%s", session$ns(target))
            } else {
              selector <- target
            }
            session$sendCustomMessage("shidashi.click", list(selector = selector))
            sprintf(paste(
              "Clicked `%s` (nothing happens if no element matches).",
              "The app reacts after this call returns: check the result with",
              "`shiny_query_ui` (or a module script that reports progress)."
            ), selector)
          },
          "dismiss_modal" = {
            shiny::removeModal(session = session)
            "Closed the open dialog, if there was one (same as its Cancel button)."
          },
          "show_notification" = {
            if (!nzchar(message)) {
              stop("Action `show_notification` needs `message`.")
            }
            css_class <- session$ns("agent_notification")
            shidashi::show_notification(
              message = message,
              title = if (nzchar(title)) title else "Notification!",
              type = type,
              autohide = FALSE,
              class = css_class,
              session = session
            )
            sprintf(paste(
              "Showing the notification until the user closes it, or until",
              "`remove_notification` removes CSS class `%s`."
            ), css_class)
          },
          "remove_notification" = {
            shidashi::clear_notifications(
              class = if (nzchar(target)) target else NULL,
              session = session
            )
            if (nzchar(target)) {
              sprintf("Removed the notifications with CSS class `%s`.", target)
            } else {
              "Removed all notifications."
            }
          },
          stop(sprintf(paste(
            "Unknown action `%s`. Use one of: click, dismiss_modal,",
            "show_notification, remove_notification."
          ), action))
        )
      },
      name = "shiny_ui_operate",
      description = paste(
        "Operate the module page as a person would, for what `shiny_input_update`",
        "cannot reach: buttons in a dialog that a script opened (e.g. 'Confirm'),",
        "alert buttons, the dialog itself, and notifications.",
        "Actions: `click` clicks `target`, a module input ID (e.g. a button ID;",
        "it must be on the page) or a CSS selector (e.g. `.swal-button`);",
        "`dismiss_modal` closes the open dialog, like its Cancel button;",
        "`show_notification` shows `message` (with optional `title` and `type`)",
        "until the user closes it or `remove_notification` removes it;",
        "`remove_notification` removes the notifications with CSS class `target`",
        "(all if empty).",
        "The app reacts after this call returns: confirm the result with",
        "`shiny_query_ui`. A click can start long or destructive work (e.g. a",
        "dialog's Confirm button): ask the user first."
      ),
      arguments = list(
        action = ellmer::type_enum(
          values = c("click", "dismiss_modal", "show_notification", "remove_notification"),
          description = "What to do."
        ),
        target = ellmer::type_string(
          description = paste(
            "`click`: a module input ID (without the module prefix) or a CSS",
            "selector. `remove_notification`: the CSS class of the notifications",
            "to remove, e.g. `<module_id>-agent_notification`; empty removes all."
          ),
          required = FALSE
        ),
        message = ellmer::type_string(
          description = "`show_notification`: the text to show.",
          required = FALSE
        ),
        title = ellmer::type_string(
          description = "`show_notification`: the title.",
          required = FALSE
        ),
        type = ellmer::type_enum(
          values = c("info", "success", "warning", "danger"),
          description = "`show_notification`: the color. Default `info`.",
          required = FALSE
        )
      )
    )
  }

)
