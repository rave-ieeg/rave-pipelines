---
name: use-rave-pipeline
description: Load, configure, run, and read results from RAVE pipelines
---

## Instructions

Interact with RAVE pipelines under `modules/{pipeline_name}/`.

### Module vs. pipeline

- A **RAVE module** is a Shiny module that provides UI
  interactivity. It may or may not contain a pipeline.
- A **pipeline** is a standalone `targets`-based computation unit with a
  `settings.yaml` (inputs) and target scripts. Pipelines can run without
  Shiny, offering coding portability.
- When a module contains a pipeline, `module_id` and `pipeline_name` are
  identical, and the UI delegates computation to the pipeline.

This skill operates on the **pipeline level** (`settings.yaml` and
targets). To interact with the **Shiny UI level**, use the `shiny_*`
tools instead.

### Data-flow (when a module wraps a pipeline)

1. **Shiny UI inputs** — user-facing widgets.
2. **User clicks "Run Analysis"** — UI "reshapes" current values into
   `settings.yaml` and triggers pipeline execution.
3. **Pipeline targets** — reads `settings.yaml`, runs computation,
   produces results.
4. **UI outputs** — reads pipeline results and renders visualizations.

UI inputs and `settings.yaml` may differ at any moment because the user
can change widgets without clicking Run.

Call `action='readme'` first to unlock other actions.

### Scripts

| Script | Purpose |
|--------|---------|
| `set_inputs.R` | Read or update a module's `settings.yaml` |
| `get_targets.R` | List targets, dependencies, and build status |
| `run.R` | Execute pipeline targets |
| `get_results.R` | Read results from completed targets |

### Usage

**Read current settings:**

    action='script', file_name='set_inputs.R', args=['notch_filter']

**Update settings:**

    action='script', file_name='set_inputs.R', args=['notch_filter', '{"subject_code":"YAB","project_name":"demo"}']

Array values use JSON arrays: `'{"notch_filter_lowerbound":[59,118,178]}'`

**List targets and status:**

    action='script', file_name='get_targets.R', args=['notch_filter']

**Run all targets:**

    action='script', file_name='run.R', args=['notch_filter']

**Run specific targets:**

    action='script', file_name='run.R', args=['notch_filter', '--targets=apply_notch,diagnostic_plots']

**Read a target result:**

    action='script', file_name='get_results.R', args=['notch_filter', '--target=apply_notch']

### Reference Files

Module-specific documentation is available via `action='reference'`:

    action='reference', file_name='notch_filter.md'

### Notes

- The first argument to every script is always `module_id` (e.g. `notch_filter`).
- Module path resolves to `modules/{module_id}/` relative to project root.
