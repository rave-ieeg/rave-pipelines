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
* `agents/skills/rave-module/references/*.md` (manuals) and `TEMPLATE.md`.

## Ask first: these need explicit approval

Ask each as its own question, naming the file and the effect. A line buried in a
long plan does not count as approval.

| Change | Examples |
|---|---|
| Visible UI | New or removed inputs, buttons, labels, or layout. **`shidashi::register_output` adds popout/download buttons to the output.** Swapping a custom loader button for `ravedash::load_data_button()`. |
| Pipeline | `main.Rmd` targets, `make-<module>.R`, new or re-purposed `settings.yaml` keys |
| Domain meaning | What a RAVE term means, e.g. "bad channel", "excluded", `noref` |
| Behaviour for people | New defaults, validation that now errors, different results from a button |
| Writes during tests | Running preprocessing (notch, wavelet) or overwriting subject files the user did not name |

Module UIs have been used for years; changing one means re-validating it and
retraining people.

Allowed without asking:
* `shidashi::register_input` wrappers (they return the same tag).
* Input and script descriptions.
* Moving an observer body into `set_script` with the same behaviour.
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

## How agents reach a module

* **Tools:**
  * `shiny_input_info` / `shiny_input_update` (registered inputs)
  * `shiny_output_info` / `shiny_output_result` (registered outputs)
  * `shiny_query_ui` (any element, by CSS selector)
  * `module_interactive_script_list` / `_inspect` / `_run` (`_run` in Execute mode only)
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
  * Errors come back to the agent as `isError`.
  * The return value reaches the agent only if it is atomic and at most 100 long.
* **Script names:**
  * `load_data` is the only script allowed before data are loaded.
  * `run_analysis` is reserved for the run-analysis button; never give it a
    `binding_event`.
* **Modals:** inputs inside a modal are unreachable. A script behind a modal's
  button must work when the modal was never opened.

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
3. **Convert actions to scripts** (see Script pattern). Keep people-only dialog
   buttons as they are; their descriptions point agents to the scripts.
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
   * Check the persisted results by reading the files the module wrote.
   * Include a non-default case (e.g. custom groups) when the user asks for one.
8. **Live test**, then run the Done checklist.

## Script pattern

```r
# Before: a button observer (async; errors only as notifications)
shiny::bindEvent(
  ravedash::safe_observe({
    res <- pipeline$run(as_promise = TRUE, names = "result_x")
    res$promise$then(onFulfilled = function(...) {
      local_reactives$x <- pipeline$read("result_x")
    }, onRejected = function(e) error_notification(e))
  }),
  input$apply_btn, ignoreNULL = TRUE, ignoreInit = TRUE
)

# After: the same work as a script, plus a button that runs it
server_tools$set_script(
  name = "apply_x",
  description = c(
    "What it does (same as clicking 'Apply'). Inputs it reads, by ID, and the",
    "state they must be in. What it writes. What it returns."
  ),
  # alert_params = list(title = "Applying..."),  # long runs: progress dialog
  expr = {
    if (!isTRUE(input$type %in% valid_types)) {
      # say what is wrong and which input or script fixes it
      stop("Please set input `type` to one of: ...; then run this script again.")
    }
    pipeline$run(names = "result_x", scheduler = "none", type = "vanilla",
                 return_values = FALSE)   # synchronous: done when the tool returns
    local_reactives$x <- pipeline$read("result_x")
    shiny::removeModal()                  # if a dialog led here
    sprintf("Applied [%s]", input$type)   # short atomic summary for the agent
  }
)

shiny::bindEvent(
  ravedash::safe_observe({
    server_tools$trigger_script("apply_x")
  }, error_wrapper = "notification"),     # people still see errors
  input$apply_btn, ignoreNULL = TRUE, ignoreInit = TRUE
)
```

* **Modal parameters.** Read the modal's inputs when it has been opened
  (`!is.null(input$<modal_input>)`); otherwise use the defaults the modal would
  show.
* **Alerts.** Use `alert_params` in place of manual `shiny_alert2()` plus
  `on.exit(close_alert2())`.
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
| "Error found!" dialog stays open | A script with `alert_params` failed. Someone must click Confirm; say so in the manual. |
| Output text is stale | Hidden outputs do not update. Activate the tab first. |

## Done checklist

* All edited R files parse (`Rscript -e 'parse("<file>")'`).
* `test-mcp.R` passes end to end on the test copy (port 17299).
* Each converted button, clicked as a person would (`shiny_input_update` on the
  button plus a screenshot), still works and still shows its errors.
* The git diff has no visible UI change and no pipeline change beyond what the
  user approved; `settings.yaml` is restored.
* The report to the user lists every behaviour change for people and every
  subject file written.
