# {module_name} Module Reference

<!--
AUTHORING GUIDE (delete this comment when done)

This reference is a USAGE MANUAL for end users and AI agents, not a technical
spec. Explain how to *operate* the module and *interpret* its results, not how
it is implemented. Keep the section headings below unchanged so every module
reference stays consistent and greppable. Replace every {placeholder} and
remove the guidance comments before publishing.
-->

{One to three sentences: what the module does and what the user gets out of it.}

**Prerequisite:** {One sentence naming the module or data that must come first.
Example: "This module analyzes power spectrograms; run the Wavelet module first
so the power data exist." Write "None" if there is no prerequisite.}

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: {Choose the RAVE project and subject in the loader screen.}
Step 2: {Any other loader inputs, e.g. epoch, reference, electrodes. Delete if none.}
Step 3: Click the "{Load subject}" button to load the data.

### 2. Analysis inputs

The key inputs are on the left panel. For each input, give the label the user
sees, what it controls, its default, and a sensible example:

* **{Input label}** — {What it controls.} Default `{value}`. Example: `{example}`.
* {Repeat for each input.}

{If the module has a run/apply action, name the button and describe what happens
when it is clicked, e.g. a validation check, a confirmation dialog, or saving
results to disk.}

### 3. Outputs

Describe each output and how the user should read it to interpret results:

* **{Output name}** — {What it shows and how to interpret it.}
* {Repeat for each output.}

## Common procedures

{One line introducing the task recipes below. Each recipe is a short sequence of
steps for one concrete goal; reuse the step-by-step guide instead of repeating
it.}

### Procedure — {task name}

Step 1: ...
Step 2: ...

### Procedure — {another task}

Steps 1-2: reuse [Procedure — {task name}](#procedure--task-name).
Step 3: {Only the steps that differ.}

## Caveats

* {A lesson, gotcha, or constraint the user should know before or after running.}
* {Repeat as needed.}

## Run the pipeline without the UI

<!-- 
Keep this section only if the module ships a pipeline (you will see code such as `pipeline$run()` 
from the server code; do not use file existence of main.Rmd for deciding if the pipeline exists); 
delete otherwise. 
-->

Reproduce the analysis in plain R, without the front-end, via the RAVE pipeline.
Note that the pipeline settings may differ from the UI inputs: describe any
conversion the UI performs before it writes `settings.yaml`.

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("{module_id}")

# Set inputs (one per line; comment each so the mapping is clear)
pipeline$set_settings(
  {input_name1} = {value1},   # {what it means}
  {input_name2} = {value2}    # {what it means}
)

# Build a target and read its result
result <- pipeline$run("{target_name}")

# Optional: reuse a module helper (from the shared env) to visualize the result
env <- pipeline$shared_env()
env${helper_function}(result)
```

## Drive the module with MCP tools

Operate the live module the same way a user would, following the step-by-step
guide above. Each step names an MCP tool; call it as `tool("tool__NAME", arg =
value)`. Be explicit about input IDs and values.

Run the interactive scripts in the same order the UI enforces (typically
`load_data` first, then a validation step, then an apply/save step). Do not skip
the validation step even if the apply step would run without it — validation is
what checks the parameters before results are written. State the required order
explicitly for this module so an agent cannot reorder or skip a step.

A script's reply has `result` (its return value) and `output` (what it printed
while it ran). Say what `output` shows when the step fails, since some scripts
finish anyway.

### Load data

* Set a loader input: `tool("tool__shiny_input_update", inputId = "{loader_input_id}", value = "{value}")`
* {Repeat for each loader input.}
* Load the data: `tool("tool__module_interactive_script_run", name = "load_data")`

### Configure and run

* Inspect the current inputs: `tool("tool__shiny_input_info")`
* Set an input: `tool("tool__shiny_input_update", inputId = "{input_id}", value = "{value}")`
* {Repeat for each analysis input.}
* {Validate / confirm step, if any:} `tool("tool__module_interactive_script_run", name = "{run_analysis}")`
* {Apply / save step, if any — ask the user first:} `tool("tool__module_interactive_script_run", name = "{apply_script}")`
* {Dialog button, if a script opens a dialog — check that it is open, ask the user first:}
  `tool("tool__shiny_query_ui", css_selector = "#{module_id}-{button_id}")`, then
  `tool("tool__shiny_ui_operate", action = "click", target = "{button_id}")`
* {Long run, if any — poll until it is done:} `tool("tool__module_interactive_script_run", name = "pipeline_progress")`

### Inspect results

* Read any shidashi-registered output with `tool("tool__shiny_output_result", outputId = "{registered output_id}")`.
* For plain UI elements that are not registered outputs (e.g. a `uiOutput`
  preview or a download link), read the DOM with
  `tool("tool__shiny_query_ui", css_selector = "#{module_id}-{output_id}")`.

