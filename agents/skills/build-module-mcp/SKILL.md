---
name: build-module-mcp
description: Use when making a RAVE module in rave-pipelines operable by AI agents over MCP - converting button observers to interactive scripts (server_tools$set_script), registering inputs for shiny_input_update, writing a module's agents.yaml, its agent manual under agents/skills/rave-module/references, or its test-mcp.R - or when testing a module through a live app's /mcp endpoint.
---

# Building MCP support for a RAVE module

## Overview

The goal: an agent can do everything a person does in a module, through MCP
tools, without anyone clicking. The module stays exactly the same for people.

**Core principle:** add a machine-readable layer (registered inputs, scripts,
manual, test). Never change what people see, how the module computes, or what
its terms mean without asking the user first.

Worked examples in this repo:
* `modules/notch_filter` and `modules/reference_module`: `R/module_server.R`
  (scripts), `agents.yaml`, and `test-mcp.R`.
* `modules/wavelet_module`: agents click the buttons of a confirmation dialog
  with `shiny_ui_operate`, since the function behind them stays untouched,
  and poll a `pipeline_progress` script during a background run.
* `agents/skills/rave-module/references/*.md` (manuals) and `TEMPLATE.md`.

## Ask first: these need explicit approval

Ask each as its own question, naming the file and the effect. A line buried in a
long plan does not count as approval.

| Change | Examples |
|---|---|
| Visible UI | New or removed inputs, buttons, labels, or layout. **`shidashi::register_output` adds popout/download buttons to the output.** Swapping a custom loader button for `ravedash::load_data_button()`. |
| Functions buttons call | Any change inside a function that a person's button runs (e.g. wavelet's `run_wavelet()`), even to make it agent-friendly: sync instead of promises, new error handling, split into helpers. Such functions carry years of fixes. Add agent access around them (a script that calls them, `shiny_ui_operate` clicks); a change inside needs approval plus a justification. |
| Pipeline | `main.Rmd` targets, `make-<module>.R`, new or re-purposed `settings.yaml` keys |
| Domain meaning | What a RAVE term means, e.g. "bad channel", "excluded", `noref` |
| Behaviour for people | New defaults, validation that now errors, different results from a button |
| Writes during tests | Running preprocessing (notch, wavelet) or overwriting subject files the user did not name |

Module UIs have been used for years; changing one means re-validating it and
retraining people.

Allowed without asking:
* `shidashi::register_input` wrappers (they return the same tag).
* Input and script descriptions.
* Moving an observer body, verbatim, into `set_script`, with the button
  running the script.
* Read-only scripts, e.g. `pipeline_progress`.
* `agents.yaml`, the manual, and `test-mcp.R`.

**Red flags: stop and ask.**

| Thought | Reality |
|---|---|
| "The feature can't work without one small input" | Ask. There is usually a way through existing inputs, groups, or scripts. |
| "It's in the plan the user approved" | Plans get skimmed. UI changes need their own question. |
| "A hidden input doesn't count" | It still changes the page and its behaviour. Ask. |
| "`register_output` is just registration" | It adds visible widgets. |
| "'Bad channel' obviously means drop it" | In RAVE referencing, bad channels are left out of reference *signals* but still referenced; an empty Reference drops a channel from loading. Ask what terms mean. |
| "It's only a small validation fix" | It changes what people see when they click. List it for the user. |
| "I'll add a script that does what the Confirm button does" | Two code paths drift apart. Agents click the button with `shiny_ui_operate`; its function stays the only copy. |
| "I'll make this promise-based body synchronous so errors reach the agent" | That rewrites a function people use. Print the error instead: scripts return what they print. |

## How agents reach a module

* **Tools:**
  * `shiny_input_info` / `shiny_input_update` (registered inputs)
  * `shiny_output_info` / `shiny_output_result` (registered outputs)
  * `shiny_query_ui` (any element, by CSS selector)
  * `module_interactive_script_list` / `_inspect` / `_run` (`_run` in Execute mode only)
  * `shiny_ui_operate` (`agents/tools/shiny_ui_operate.R`; enable it in
    `agents.yaml`, Execute mode only): `click` a module input ID (e.g. a
    dialog button) or a CSS selector, `dismiss_modal`, `close_alert2` (runs
    `dipsaus::close_alert2()`), `show_notification`, and
    `remove_notification` (by CSS class)
  * `rave_3dviewer_get` / `rave_3dviewer_set` (`agents/tools/rave_3dviewer.R`)
    for a module with a threeBrain viewer: read and change its controllers,
    camera, and crosshair through `threeBrain::brain_proxy()`, with values
    checked against the viewer's controller specs (threeBrain 1.3.0.62 or
    newer). To adopt them, enable both in `agents.yaml` (`get`: exploratory;
    `set`: executing, so no approval per call) and name the viewer's
    `outputId` (e.g. `viewer`) in the manual and system prompt. The viewer
    reports its camera only after a mouse drag, and `shiny_query_ui` pictures
    leave the 3D view blank. To add a `name`, add an entry to the tool file's
    `rave_3dviewer_getters` or `rave_3dviewer_setters` list
  * `switch_module` (a shidashi meta tool, MCP only: the in-app chat does
    not have it, and it needs no `agents.yaml` entry): shows a module in the
    dashboard, bringing its open tab to the front or opening it
    (`auto_new = false` only switches to an open tab), and returns once the
    module has loaded. Refused while a module is pinned. A live test that
    opens the dashboard `/` (not `?module=`) can open modules with it
* **Inputs round-trip through the browser.** `shiny_input_update` calls the
  registered update function; the browser applies it and sends the value back.
  * A browser session with the module open is required.
  * New values are not visible at once: poll `shiny_input_info` (`exists`, `current_value`).
  * An input that is not on the page cannot be set: `renderUI` branches, closed
    modals, footers of inactive tabs.
* **Buttons** registered with `updateActionButton`, `updateActionLink`, or
  `dipsaus::updateActionButtonStyled` are clicked in the browser.
* **Values** are JSON strings, decoded with `simplifyDataFrame = FALSE`, so an
  array of objects becomes a list of lists.
* **Scripts** (`server_tools$set_script`) take no arguments. An agent sets the
  inputs first, then runs the script.
  * `trigger_script` evaluates the script in `isolate()` inside the session.
  * Errors the script throws come back to the agent as `isError`.
  * The return value reaches the agent only if it is atomic and at most 100 long.
  * What the script prints while it runs comes back as `output` (last 3000
    characters): stdout, `message()`, cli and `targets` progress, warnings,
    and `ravepipeline::logger()` lines. The console still gets all of it.
  * Not in `output`: anything printed after the script returns, i.e. promise
    callbacks (`pipeline$run(as_promise = TRUE)` then `$then()`) and observers
    that a click sets off. A failing target shows as "✖ <target> errored";
    `ravepipeline` logs the target's code, not its message.
* **Script names:**
  * `load_data` is the only script allowed before data are loaded.
  * `run_analysis` is reserved for the run-analysis button; never give it a
    `binding_event`.
* **Dialogs:** each dialog button has one code path. Either its observer runs
  a script (`trigger_script`, e.g. reference_module's `generate_reference`), or
  agents click it with `shiny_ui_operate` after the script that opens the
  dialog (e.g. wavelet's `run_analysis`, then `wavelet_confirm_btn2`). Never
  both: no script re-implements what a button's function does.
  * Inputs inside a dialog are not registered, so agents cannot set them. A
    script behind a dialog button must work when the dialog was never opened.
  * Shiny keeps an input after its dialog closes, so `shiny_ui_operate` can
    click a button that is gone. Check with `shiny_query_ui` first.

## Workflow

1. **Inventory.** Read `R/loader.R`, `R/module_html.R`, and `R/module_server.R`.
   Record, for every button observer (`bindEvent(..., input$*_btn)`):
   * what it reads (by input ID), and where each input lives: static,
     `renderUI` branch, modal, or tab footer;
   * what it writes: `settings.yaml`, subject files, reactive state.

   Show the user the table and every gap that seems to need a UI or pipeline
   change, with your non-UI alternative. Wait for answers.
2. **Register inputs** agents must set (see Quick reference). This includes
   inputs inside `renderUI` and tab footers.
3. **Convert actions to scripts** (see Script pattern). Each button keeps one
   code path: its observer runs the script, or agents click it with
   `shiny_ui_operate` (dialog buttons whose function must stay as it is).
   Script descriptions tell agents which buttons to click next.
4. **Outputs.** Use already-registered outputs through `shiny_output_result`.
   Read plain outputs with `shiny_query_ui` and `#<module_id>-<outputId>`; the
   output must be visible (e.g. its tab active), because hidden outputs do not
   update. Do not add `register_output` without approval.
5. **`agents.yaml`.**
   * Copy `modules/notch_filter/agents.yaml`, and check that the module ID is right.
   * The system prompt covers: the data flow, the script order, and the
     interactive rules the user gave (e.g. confirm groupings, ask before overwriting).
   * Point it to the manual. Do not list this skill there.
6. **Manual** at `agents/skills/rave-module/references/<module_id>.md`, from
   `TEMPLATE.md`, with its headings unchanged.
   * Check every claim about behaviour against the code; drop what you cannot verify.
   * Domain semantics come from the user.
   * Give agents: the script order, which inputs exist in which state, and the
     questions to ask the user.
7. **`test-mcp.R`.** Start from `modules/reference_module/test-mcp.R`. Keep its
   helpers: `set_input` (JSON-encodes like a real agent), `wait_input`,
   `set_input_wait` (re-sends until the value sticks), `run_script`, and
   `save_as`.
   * Drive the whole workflow the way an agent would.
   * Check the persisted results by reading the files the module wrote. Prove
     they are fresh (e.g. a saved timestamp later than the run's start), and
     compare exact values; never trim the expectations to what was saved.
   * Include an error case, checked through the script's `output`, and a check
     that nothing (e.g. no dialog) followed it.
   * Include a non-default case (e.g. custom groups) when the user asks for one.
   * Name the subject in variables at the top; the user approves which subject
     the test writes. Never call `quit()`: stop with `stop()`.
8. **Live test**, then run the Done checklist.

## Script pattern

```r
# Before: a button observer
shiny::bindEvent(
  ravedash::safe_observe({
    res <- pipeline$run(as_promise = TRUE, names = "result_x")
    res$promise$then(onFulfilled = function(...) {
      local_reactives$x <- pipeline$read("result_x")
    }, onRejected = function(e) error_notification(e))
  }),
  input$apply_btn, ignoreNULL = TRUE, ignoreInit = TRUE
)

# After: the same body, verbatim, as a script; the button runs the script
server_tools$set_script(
  name = "apply_x",
  description = c(
    "What it does (same as clicking 'Apply'). Inputs it reads, by ID, and the",
    "state they must be in. What it writes. What its `output` shows on",
    "success and on failure. What it returns."
  ),
  expr = {
    res <- pipeline$run(as_promise = TRUE, names = "result_x")
    res$promise$then(onFulfilled = function(...) {
      local_reactives$x <- pipeline$read("result_x")
    }, onRejected = function(e) error_notification(e))
  }
)

shiny::bindEvent(
  ravedash::safe_observe({
    server_tools$trigger_script("apply_x")
  }),
  input$apply_btn, ignoreNULL = TRUE, ignoreInit = TRUE
)

# The module's error helper: people see the toast, agents read the log line
error_notification <- function(e) {
  ravepipeline::logger("Error found! ", paste(e$message, collapse = "\n"),
                       level = "error")
  shidashi::show_notification(message = e$message, title = "Error found!",
                              type = "danger", class = ns("error_notif"))
}
```

* **One code path.** The button runs `trigger_script()`. Never keep a second
  copy of the body next to the script, and never add a script that re-does
  what a button's function does: agents click that button with
  `shiny_ui_operate` instead.
* **Verbatim.** Keep the body as it is, promises and error handling included.
  Making a promise synchronous, or changing how errors are handled, changes a
  function people use: ask (see Ask first).
* **Errors.** An error that the body catches and shows only to people (a toast
  or an alert) never reaches the agent: also print it, e.g. with the log line
  above. What prints while the script runs comes back in `output`. A failing
  pipeline target shows only as "✖ <target> errored"; a `pipeline_progress`
  script gives its message.
* **Descriptions and results.** Every script gets a `description`,
  `run_analysis` and `load_data` included; without one, agents see only the
  name. Return a short atomic summary, e.g. `load_data` returns the subject,
  electrodes, and sample rate.
* **Long runs.** When the module has a background option (e.g. "Confirm and
  run in background"), agents use it and poll a read-only `pipeline_progress`
  script: `"<target>: <progress>"` lines from `pipeline$progress("details")`,
  plus `targets::tar_meta(fields = "error")` messages for errored targets (see
  `modules/wavelet_module/R/module_server.R`). A foreground run blocks the app,
  and the agent's next call, until it ends.
* **Modal parameters.** Read the modal's inputs when it has been opened
  (`!is.null(input$<modal_input>)`); otherwise use the defaults the modal would
  show.
* **Alerts.** Use `alert_params` in place of manual `shiny_alert2()` plus
  `on.exit(close_alert2())`. `trigger_script` closes the open alert when the
  script ends, so a flow that ends with its own alert (e.g. "Done!") keeps
  managing its alerts itself.
* **Loader button.** Register `load_data` with
  `binding_event = "load_data", dispatch_event = "data_changed"` only if the
  loader already uses `ravedash::load_data_button()`. Otherwise keep its button
  and add `bindEvent(safe_observe(trigger_script("load_data")), input$<its_btn>)`.
* **Per-row vectors.** When a script builds a vector the pipeline assigns
  row by row, order it by the pipeline target's rows, not by electrode number.

## Quick reference: registering inputs

```r
shidashi::register_input(
  shiny::selectInput(ns("group_name"), "Group name", choices = character(0)),
  inputId = "group_name",                             # without namespace
  update = "shiny::updateSelectInput(value=selected)",
  description = "What it controls; value format; when it exists; which script reads it."
)
```

| Widget | `update` |
|---|---|
| text / numeric | `shiny::updateTextInput`, `shiny::updateNumericInput` |
| select | `shiny::updateSelectInput(value=selected)` |
| compound input | `dipsaus::updateCompoundInput2` (describe the value as JSON: `[{"name":"A","electrodes":"1-5"}]`) |
| button (gets clicked) | `shiny::updateActionButton`, `dipsaus::updateActionButtonStyled` |
| card tabset | `shidashi::card_tabset_activate(value=title)` (lets agents open a tab so its footer inputs exist) |

## Live testing

The developer's own app usually runs on port 17283: never stop it or drive it
unless asked. It keeps the old module code until its tab reloads.

```sh
SCRATCH=<scratch dir>
cp modules/<id>/settings.yaml "$SCRATCH/settings.yaml.bak"  # live runs rewrite it
Rscript -e 'ravedash::debug_modules(".", port = 17299, launch_browser = FALSE, as_job = FALSE)' &
node agents/skills/build-module-mcp/live-browser.js "http://127.0.0.1:17299/?module=<id>" "$SCRATCH" &
# wait until tool `shidashi_sessions` lists the module, then:
RAVE_TEST_PORT=17299 Rscript modules/<id>/test-mcp.R
touch "$SCRATCH/shot"           # screenshot -> $SCRATCH/screenshot.png
touch "$SCRATCH/stop"           # close the browser
kill "$(lsof -tiTCP:17299 -sTCP:LISTEN)"
cp "$SCRATCH/settings.yaml.bak" modules/<id>/settings.yaml
```

* **Reload after edits.** Module R files are sourced on each page load: after
  editing them, stop and relaunch the browser.
* **Restore `settings.yaml` from the copy.** `git checkout` would also discard
  the developer's uncommitted edits.
* **Harmless noise.** Headless threeBrain logs `getController` errors.
* **Report writes.** Tell the user which subject files the test wrote.

## Gotchas

| Symptom | Cause, fix |
|---|---|
| Update "succeeds", value unchanged | Choices not loaded yet (a subject list waits for its project). Re-send until `shiny_input_info` shows it. |
| A select resets right after you set it | An observer on another input resets it (e.g. choosing a group resets its type). Set in dependency order and wait between. |
| `Input ... is inactive or missing` | `renderUI` branch, modal, or tab footer not showing. Set the controlling input first (e.g. activate the tab). |
| Groups arrive empty | Sent as an R list, or the installed shidashi decodes JSON into a data.frame. Send a JSON string; check `simplifyDataFrame = FALSE`. |
| Compound input has one empty row after load | Initial value. Poll until the populated value arrives. |
| "Error found!" dialog stays open | A script with `alert_params` failed. Someone must click Confirm, or the agent closes it (`shiny_ui_operate` action `close_alert2`); say so in the manual. |
| Output text is stale | Hidden outputs do not update. Activate the tab first. |
| A script "succeeds", but nothing happened | The body caught the error and showed it only to people. Read `output`; if the error is not there, print it in the module's notification helper. |
| `output` says "✖ <target> errored" without a reason | `ravepipeline` logs the target's code, not its message. A `pipeline_progress` script reads it from `targets::tar_meta(fields = "error")`. |
| `shiny_ui_operate` click does nothing | The element is gone: Shiny keeps an input after its dialog closes. Check with `shiny_query_ui` before clicking. |

## Done checklist

* All edited R files parse (`Rscript -e 'parse("<file>")'`).
* `test-mcp.R` passes end to end on the test copy (port 17299).
* Each converted button, clicked as a person would (`shiny_ui_operate` or
  `shiny_input_update` on the button, plus a screenshot), still works and still
  shows its errors.
* No button has two code paths: no script repeats what a button's function
  does, and the old observer body is gone wherever a script replaced it.
* The git diff has no visible UI change, no pipeline change, and no change
  inside functions that buttons call, beyond what the user approved;
  `settings.yaml` is restored.
* The report to the user lists every behaviour change for people and every
  subject file written.
