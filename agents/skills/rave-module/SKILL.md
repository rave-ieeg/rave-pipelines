---
name: rave-module
description: "Load, configure, run pipelines and read module source files to understand RAVE module architecture"
---

## Instructions

Interact with RAVE modules under `modules/{module_id}/`. This skill provides
two categories of operations:

1. **Pipeline operations** — Configure inputs, run pipelines, read results
2. **Source reading** — Read module source code to understand architecture

### Module vs. Pipeline

- A **RAVE module** is a Shiny module that provides UI interactivity.
  It may or may not contain a pipeline.
- A **pipeline** is a standalone `targets`-based computation unit with
  `settings.yaml` (inputs) and target scripts. Pipelines can run without
  Shiny, offering coding portability.
- When a module contains a pipeline, `module_id` and `pipeline_name` are
  identical, and the UI delegates computation to the pipeline.
  
### When to use module or pipeline

- Use UI modules when user asks to change inputs, interpret results
- Use UI first when user asks for quantitative results. If the UI contains no such information, you may then use pipelines
- Use pipelines when user asks for low-level implementations

- Do NOT use pipelines when user asks to set UI inputs

### Data-flow (when a module wraps a pipeline)

1. **Shiny UI inputs** — user-facing widgets
2. **User clicks "Run Analysis"** (script `run_analysis`) — UI "reshapes" values into `settings.yaml`
3. **Pipeline targets** — reads `settings.yaml`, runs computation, produces results
4. **UI outputs** — reads pipeline results and renders visualizations


---

## Module tools (recommended)

| Tool | Purpose |
|--------|---------|
| shiny_input_info | obtain shiny-app HTML input information |
| shiny_input_update | Update shiny-app input from module UI |
| shiny_output_info | get shiny-app HTML output information |
| shiny_query_ui | get HTML elements by css selector |
| shiny_output_result | get a registered output's rendered content by ID, even in a hidden tab or collapsed card |
| module_interactive_script_list | list the module's interactive scripts (e.g. `load_data`, `run_analysis`) and whether the data are loaded |
| module_interactive_script_inspect | show what an interactive script does: its description and R code |
| module_interactive_script_run | run an interactive script, as if the user clicked its button |

### Interactive scripts

Modules register interactive scripts with `server_tools$set_script(name, expr)`
(usually in `R/loader.R` and `R/module_server.R`). Each script runs the same
code as a button in the module UI:

- `load_data`: the load-data button in the loader (e.g. "Load subject")
- `run_analysis`: the run-analysis button (e.g. "RAVE!")
- module-specific scripts, e.g. `apply_notch_filter` in `notch_filter`

Call `module_interactive_script_list` first, then
`module_interactive_script_inspect` to read what a script does, then
`module_interactive_script_run`. Run `load_data` first: every other script
needs the data loaded. Scripts change the module state (UI inputs,
`settings.yaml`, results), so ask the user before running one that saves
results.

## How to call the scripts

Scripts run through the `skill_run__rave-module` tool. The examples in this
skill are written as the shell command each call stands for. Turn a command
into a tool call like this:

- `file_name` is the script name, the word after `Rscript`
- `args` holds every word after the script name, one item per word, in the
  same order
- drop the shell quotes: a JSON value is one item, as-is

| Shell command | `skill_run__rave-module` arguments |
|---|---|
| `Rscript get_targets.R power_explorer` | `file_name: "get_targets.R"`, `args: ["power_explorer"]` |
| `Rscript get_results.R power_explorer --target=omnibus_results` | `file_name: "get_results.R"`, `args: ["power_explorer", "--target=omnibus_results"]` |
| `Rscript run.R notch_filter --targets=apply_notch,diagnostic_plots` | `file_name: "run.R"`, `args: ["notch_filter", "--targets=apply_notch,diagnostic_plots"]` |
| `Rscript set_inputs.R notch_filter '{"subject_code":"YAB"}'` | `file_name: "set_inputs.R"`, `args: ["notch_filter", "{\"subject_code\":\"YAB\"}"]` |

- Every script takes the module ID first (`power_explorer`, `notch_filter`,
  ...). A call that leaves out a required argument is refused, and the reply
  shows the script's usage (`<x>` required, `[x]` optional). The list at the
  end of this readme shows every script's usage.
- Never put `Rscript`, a path, or `action` in `args`, and never pack several
  words into one item (`["power_explorer --target=x"]` is wrong).
- Reference files are read with `skill_load__rave-module`, not run:
  `action: "reference"`, `file_name: "references/power_explorer.md"`.

## Pipeline Scripts 

> Not recommended unless the task requires no UI interaction (e.g. check current status, get pipeline input formats)

| Script | Purpose |
|--------|---------|
| `set_inputs.R` | Read or update a module's `settings.yaml` |
| `get_targets.R` | List targets, dependencies, and build status |
| `run.R` | Execute pipeline targets |
| `get_results.R` | Read results from completed targets |

### Usage

Written as shell commands; see "How to call the scripts" above.

Read-only ops:

**Read current settings:**

    Rscript set_inputs.R notch_filter

**List targets and status:**

    Rscript get_targets.R notch_filter
    
**Read a target result:**

    Rscript get_results.R notch_filter --target=apply_notch

Destructive/Dangerous ops: 

> Although you have access to the pipelines, in general you should NOT modify the pipelines directly. Instruct users on how to set up module UI instead, unless the user asks specifically to change/run the pipeline.

**Update settings:**

    Rscript set_inputs.R notch_filter '{"subject_code":"YAB","project_name":"demo"}'

**Run all targets:**

    Rscript run.R notch_filter

**Run specific targets:**

    Rscript run.R notch_filter --targets=apply_notch,diagnostic_plots


---

## Source Reading Scripts

Read module source files to understand UI components, pipeline logic, and
module architecture. **Privacy protected**: only canonical source files are
accessible (no user data, settings, or cached results).

| Script | Purpose |
|--------|---------|
| `list_source_files.R` | List canonical source files in a module |
| `read_source_file.R` | Read file with line numbers (default 200 lines) |
| `grep_source_file.R` | Search file with context (before/after lines) |

### Canonical Files

| File/Directory | Purpose |
|----------------|---------|
| `DESCRIPTION` | Module metadata (title, version, authors) |
| `main.Rmd` | Pipeline targets defined as R Markdown chunks |
| `R/` | R source files (UI, server logic, utilities) |
| `py/` | Python source files (if present) |
| `server.R` | Shiny server entry point |
| `report-*.Rmd` | Report templates |

### Usage

Written as shell commands; see "How to call the scripts" above.

**List all source files:**

    Rscript list_source_files.R notch_filter

**Read file (first 200 lines, with line numbers):**

    Rscript read_source_file.R notch_filter --file=DESCRIPTION

**Read file starting from line 50:**

    Rscript read_source_file.R notch_filter --file=R/module_server.R --start=50

**Read file with custom line count:**

    Rscript read_source_file.R notch_filter --file=main.Rmd --start=1 --nlines=100

**Search for pattern with context:**

    Rscript grep_source_file.R notch_filter --file=R/module_server.R --pattern=bindEvent

**Search with custom context (5 lines before, 20 after):**

    Rscript grep_source_file.R notch_filter --file=R/module_html.R --pattern=sliderInput --before=5 --after=20

### Output Format

`read_source_file.R` outputs:
```
## module_id/file.R (lines 1-200 of 350)

  1: # First line
  2: # Second line
...
```

`grep_source_file.R` outputs (matching lines marked with `>`):
```
--- Match 1 (line 45) ---
 40: context before
 45: >matching line
 50: context after
```

---

## Understanding Module Architecture

1. **Start with DESCRIPTION** — understand what the module does
2. **Read main.Rmd** — see pipeline targets and data flow
3. **Read R/module_html.R** — UI component definitions
4. **Read R/module_server.R** — server logic and reactivity
5. **Read R/loader.R** — data loading logic (if present)
6. **Read report-*.Rmd** — report generation templates

### Key Patterns to Search

- `pipeline$read(var_names)` — read pipeline target results
- `pipeline$run(as_promise = TRUE)` — trigger pipeline execution
- `server_tools$set_script(name, expr)` — register an interactive script (`load_data`, `run_analysis`, ...)
- `ravedash::watch_data_loaded()` — react to data loading
- `local_reactives` / `local_data` — module state management
- `shiny::bindEvent` — reactive event bindings
- `shiny::updateSelectInput` — UI updates

---

## Reference Files

Module-specific documentation is read with `skill_load__rave-module`
(`action: "reference"`):

    skill_load__rave-module  action: "reference", file_name: "references/notch_filter.md"

Add `pattern` to grep a reference, or `line_start` / `n_lines` to page
through it.

Each module reference is a **usage manual** for operating the module and
interpreting its results (not a technical spec). It has the following sections
(you can `grep` the reference):

```
# {module_name} Module Reference

Brief description of what the module does + one-line prerequisite.

## Table of contents

## Step-by-step guide
### 1. Load data
### 2. Analysis inputs
### 3. Outputs

## Common procedures

## Caveats

## Run the pipeline without the UI

## Drive the module with MCP tools
```

See `references/TEMPLATE.md` for the authoring template. For low-level
implementation details, read the module source instead (see below).

---

## Notes

- The first argument to every script is always `module_id` (e.g., `notch_filter`)
- Module path resolves to `modules/{module_id}/` relative to project root
- Source reading is privacy-protected: `settings.yaml` and `_targets/` are blocked
