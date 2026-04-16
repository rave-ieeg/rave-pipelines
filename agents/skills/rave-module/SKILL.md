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
2. **User clicks "Run Analysis"** — UI "reshapes" values into `settings.yaml`
3. **Pipeline targets** — reads `settings.yaml`, runs computation, produces results
4. **UI outputs** — reads pipeline results and renders visualizations


---

## Module tools (recommended)

| Script | Purpose |
|--------|---------|
| shiny_input_info | obtain input information |
| shiny_input_update | Update input from module UI |
| shiny_output_info | get output information |
| shiny_query_ui | get HTML elements (mostly output) by css selector |
| trigger_rave_analysis | trigger module to "run-analysis" |

## Pipeline Scripts 

> Not recommended unless the task requires no UI interaction (e.g. check current status, get pipeline input formats)

| Script | Purpose |
|--------|---------|
| `set_inputs.R` | Read or update a module's `settings.yaml` |
| `get_targets.R` | List targets, dependencies, and build status |
| `run.R` | Execute pipeline targets |
| `get_results.R` | Read results from completed targets |

### Usage

Read-only ops:

**Read current settings:**

    action='script', file_name='set_inputs.R', args=['notch_filter']

**List targets and status:**

    action='script', file_name='get_targets.R', args=['notch_filter']
    
**Read a target result:**

    action='script', file_name='get_results.R', args=['notch_filter', '--target=apply_notch']

Destructive/Dangerous ops: 

> Although you have access to the pipelines, in general you should NOT modify the pipelines directly. Instruct users on how to set up module UI instead, unless the user asks specifically to change/run the pipeline.

**Update settings:**

    action='script', file_name='set_inputs.R', args=['notch_filter', '{"subject_code":"YAB","project_name":"demo"}']

**Run all targets:**

    action='script', file_name='run.R', args=['notch_filter']

**Run specific targets:**

    action='script', file_name='run.R', args=['notch_filter', '--targets=apply_notch,diagnostic_plots']


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

**List all source files:**

    action='script', file_name='list_source_files.R', args=['notch_filter']

**Read file (first 200 lines, with line numbers):**

    action='script', file_name='read_source_file.R', args=['notch_filter', '--file=DESCRIPTION']

**Read file starting from line 50:**

    action='script', file_name='read_source_file.R', args=['notch_filter', '--file=R/module_server.R', '--start=50']

**Read file with custom line count:**

    action='script', file_name='read_source_file.R', args=['notch_filter', '--file=main.Rmd', '--start=1', '--nlines=100']

**Search for pattern with context:**

    action='script', file_name='grep_source_file.R', args=['notch_filter', '--file=R/module_server.R', '--pattern=bindEvent']

**Search with custom context (5 lines before, 20 after):**

    action='script', file_name='grep_source_file.R', args=['notch_filter', '--file=R/module_html.R', '--pattern=sliderInput', '--before=5', '--after=20']

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
- `ravedash::watch_data_loaded()` — react to data loading
- `local_reactives` / `local_data` — module state management
- `shiny::bindEvent` — reactive event bindings
- `shiny::updateSelectInput` — UI updates

---

## Reference Files

Module-specific documentation is available via `action='reference'`:

    action='reference', file_name='notch_filter.md'

Each module reference has the following contents: (you can `grep` the reference)

```
# {module_name} Module Reference

Brief description of what the module does.

## 1. Pipeline Interface

### 1.1 Prerequisites
### 1.2 Pipeline Settings
### 1.3 Key Targets

## 2. Module UI Interface

### 2.1 Input Components
### 2.2 UI to Pipeline Mapping
### 2.3 Pipeline Execution Flow
### 2.4 Outputs and Visualizations

## 3. Data Export and Reports

### 3.1 Reading Pipeline Results in R
### 3.2 Built-in Export Functions
### 3.3 Available Reports
```

---

## Notes

- The first argument to every script is always `module_id` (e.g., `notch_filter`)
- Module path resolves to `modules/{module_id}/` relative to project root
- Source reading is privacy-protected: `settings.yaml` and `_targets/` are blocked
