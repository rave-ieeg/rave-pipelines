# Module Reference Documentation Template

This document provides guidelines for writing comprehensive RAVE module reference documentation.

---

## Document Structure

Each module reference should follow this structure: (you can grep the reference)

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

# Section Guidelines

## 1. Pipeline Interface

RAVE pipeline infrastructure is offered by R package `ravepipeline`, can be found on CRAN or source repo at https://github.com/dipterix/ravebuiltins

The package allows a separation of code and UI logics, allowing the analyses to be done without shiny code. A typical pipeline starts with `main.Rmd` in a form of code blocks, typically starts with `{rave, export="<target_name>", ...}`. The target name, is the key variable name that should be extracted from the enclosing code block. A `target_name` is immutable and can be used/depended by the subsequential code blocks.

The `main.Rmd` file is designed to be user-friendly. The interpreted make file is stored at `make-{module_id}.R`. This file reveals the underlying logics on how `ravepipeline` runs the pipeline and underlying dependency network between target variables. 


> If you cannot see `make-{module_id}.R`, this means the module is interactive shiny application only, and does not use RAVE pipeline, even though sometimes `main.Rmd` and `settings.yaml` are used.


When running a pipeline, `ravepipeline` 

1. loads the shared helper functions from `R/shared-*.R` for R scripts and `py/` for Python modules. 
2. The helper functions will be available throughout the pipeline. 
3. The pipeline loads the user-inputs from `settings.yaml`. 
4. Based on the user's build targets, `ravepipeline` calculates the target variables to run, and execute them in sequential order. 
  - If a target's inputs are unchanged and `cue` is not `"always"`, then the target does not need to update and the variable will be skipped (use cache)
5. User can use `pipeline$read()` to load the calculated results


### 1.1 Prerequisites

Describe requirements before using this module:

- **Required prior modules**: What preprocessing steps must be completed? Best to provide those module IDs & labels if exist
- **Data requirements**: What data must exist (imported signals, epochs, etc.)?
- **Loader components**: What does the loader UI collect?

Example format:

| Requirement | Description |
|-------------|-------------|
| Imported signals | Raw LFP signals must be imported via "Import Signals" module |
| Project/Subject | Valid RAVE project and subject must exist |

---

### 1.2 Pipeline Settings

Document all keys in `settings.yaml`: these variables can be obtained via `pipeline$get_settings()` and set via `pipeline$set_settings()`. 

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `setting_name` | type | value | What it controls |

For nested objects, create subsections:

#### nested_object_name

| Key | Type | Default | Description |
|-----|------|---------|-------------|

---

### 1.3 Key Targets

Document pipeline targets from `main.Rmd`:

| Target | Dependencies | Export | Description |
|--------|--------------|--------|-------------|
| `target_name` | dep1, dep2 | exported_var | What it computes |

Include:
- **Exported variable name**: What you can read via `pipeline$read()`
- **Data type**: What the result contains
- **Side effects**: File writes, setting changes
- **When to run**: Conditions for running this target

The built target script is located at `make-{module_id}.R`. If this file exists, you can fine more implementation details and dependency tree among the targets. 

---

## 2. Module UI Interface

RAVE UI is built on top of `ravedash` package (see https://github.com/dipterix/ravedash). The UI uses `shidashi`, a modular based `shiny` framework. 

The module UI logics are stored at `R/` folder, typically `R/module_html.R`, `R/module_server.R`, and `loader.R`. However, this file convention is a convention, not a must. For example, the "import_signals" does not follow this convention. 

When rendering the shiny module, `shidashi` sources all the R/ scripts, and make those variables available to its template builder. The builder parses `module-ui.html` and calls all the expressions wrapped by `{{ }}`. The parsed result serves as UI, and server component is stored at `server.R` under the module folder.

`ravedash` separates the module app into a loader and a main contents. The loader UI ensures that the pre-requisites are satisfied. Here is how the UI works in general:

1. An user clicks on loading the the content, 
2. A loader shiny observer set initial inputs to the pipeline and run to load a data repository
3. If 2 passes, then trigger `ravedash::fire_rave_event('data_changed', Sys.time())` to notify `ravedash` that loader is triggered
4. `ravedash` runs `check_data_loaded` for validation
5. Once validator returns `TRUE`, `ravedash` fires `data_loaded` event
6. All the observers watching `ravedash::watch_data_loaded()` will be triggered
  - These observers initialize the input UIs in the main contents
  - In the meanwhile, the loading screen will be dismissed and main UI will be presented
7. Users configure the analysis parameters, then click on `Run Analysis` button at the bottom-right of the screen
8. A run-analysis flag will be triggered (`server_tools$run_analysis_flag`)
9. All observers watching `server_tools$run_analysis_flag()` will execute. If the module contains a pipeline, these observers will
  - set pipeline inputs and run pipeline
  - Load the pipeline data back, trigger update output flag
  - The output renderers render the outputs

There could be other logics not offered by `ravedash`, those should work under general `shiny` framework.


### 2.1 Input Components

Document all UI inputs from `R/module_html.R`:

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `input_name` | widget type | "Label" | default | validation rules |

Group by UI section (Filter Settings, Inspection, etc.)

`input_name` should not contain module prefix (`ns()`, or shiny namespace). For example `"btn"`, not `"power_explorer--btn"` under `power_explorer` module.

Many inputs don't have specific values at initialization. This is because their options require update once the data is loaded. Typically you can find a shiny observe expression that watches `ravedash::watch_data_loaded()`

Some UI components are provided by `ravedash::preset_*`: those are common reused input output components across modules. Please request to show the function source code to see the implementation details. 

---

### 2.2 UI to Pipeline Mapping

Explain how UI inputs are transformed into pipeline settings:

1. **Input validation rules** (from shinyvalidate)
2. **Conversion formulas** (e.g., `lb = base_freq * times - bandwidth`)
3. **Code reference**: Which function/lines perform the conversion

Example:

#### Frequency Calculation

UI inputs:
- `base_freq` = 60
- `times` = "1,2,3" → [1, 2, 3]
- `bandwidth` = "1,2,2" → [1, 2, 2]

Pipeline settings:
- `lowerbound` = [60×1 - 1, 60×2 - 2, 60×3 - 2] = [59, 118, 178]
- `upperbound` = [60×1 + 1, 60×2 + 2, 60×3 + 2] = [61, 122, 182]

---

### 2.3 Pipeline Execution Flow

Document how/when the pipeline runs from the UI:

1. **Goal**: What's the goal? (load repository, generate data for visualization, export data, ...)
2. **Trigger**: What user action starts execution?
3. **Targets run**: Which targets are executed?
4. **Result**: Progress indicators, confirmations

Example:

#### Stage 1: Validation
- **Trigger**: "Apply" button clicked
- **Targets**: `filter_settings`, `channels_to_apply_filters`
- **Result**: Confirmation dialog shown

#### Stage 2: Execution  
- **Trigger**: "Confirm" button clicked
- **Targets**: `apply_notch`
- **Result**: Data written, UI refreshed

---

### 2.4 Outputs and Visualizations

Document rendered outputs from `R/module_server.R`:

| Output ID | Type | Description | Data Source |
|-----------|------|-------------|-------------|
| `plot_id` | plotOutput | What it shows | target or function |

Include:
- Interactive features (shiny brush, click, plotly integration, 3D viewer, ...)
- Triggering reactives
- Parameter controls

---

### 3.1 Reading Pipeline Results in R

Provide code examples for reading pipeline data:

```r
# Using ravepipeline
pipeline <- ravepipeline::pipeline("module_id", temporary = TRUE)
result <- pipeline$read("target_name")

# Direct H5 file access (if applicable)
h5_path <- "path/to/electrode_1.h5"
data <- ieegio::io_read_h5(h5_path, "data/path")
```

Document:
- **Available targets**: What can be read
- **Data structures**: What the result contains
- **File locations**: Where data is stored persistently

Use high-level RAVE packages as references if possible. Here is a list of core packages

- `ravecore`: Core RAVE packages to handle RAVE-specific file structure resolution (you might see another `raveio` somewhere, `raveio` is deprecated in favor of `ravecore` but many functions share in common)
- `ravepipeline`, `ravedash` for pipeline and module UI respectively
- `threeBrain`: 3D viewer engine
- `filearray`: low-level file-based array for out-of-memory computation
- `ieegio`: low-level neuroimaging format solutions - read/write signal data, imaging data files, HDF5 & matlab formats
- `shidashi`: Modular UI framework that integrates AI agents
- `rpymat`: Python integration for RAVE
- `rpyANTs`: Python wrapper of `ANTsPyx` for RAVE (for some imaging registration and normalization)

---

### 3.2 Built-in Export Functions

Document download handlers and export functions:

| Export | Format | Trigger | Contents |
|--------|--------|---------|----------|
| Diagnostic PDF | PDF | Download button | Description |

Include:
- File naming conventions
- What parameters are included
- Where files are saved

---

### 3.3 Available Reports

Document reports from `report-list.yaml`:

| Report Name | Entry File | Description |
|-------------|------------|-------------|
| `report_id` | report-xxx.Rmd | What it generates |

Include:
- **How to generate**: UI button, automatic, or programmatic
- **Parameters**: What settings affect output
- **Output location**: Where reports are saved

---

## Style Guidelines

1. **Use tables** for structured information (settings, inputs, targets)
2. **Include code examples** for programmatic access
3. **Reference file locations** (R/file.R) when helpful
4. **Document side effects** (file writes, setting changes)
5. **Provide real examples** with actual values
6. **Cross-reference** related packages, modules, and skill reference/ folder files when applicable

---

## Example Workflow Section (Optional)

For complex modules, include a typical workflow:

## Typical Workflow

1. **Load subject**
   - Select project/subject in loader
   - Verify data requirements met

2. **Configure parameters**
   - Set analysis parameters
   - Review preview if available

3. **Execute and verify**
   - Click "Run" → Review confirmation
   - Inspect outputs
   - Check diagnostic plots

4. **Export results**
   - Download data files
   - Generate formal report
