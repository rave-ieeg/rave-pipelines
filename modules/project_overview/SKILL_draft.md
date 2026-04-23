# SKILL: Writing RAVE Pipeline `main.Rmd`

## Overview

RAVE (Reproducible Analysis and Visualization of iEEG) pipelines are built from R Markdown files (`main.Rmd`) using a custom `{rave}` knitr engine provided by the `ravepipeline` package. Each `{rave ...}` code block defines a **pipeline target** — a cached, dependency-tracked unit of computation. When users interact with the RAVE dashboard, the pipeline executes targets selectively based on what has changed.

The `main.Rmd` is the **single source of truth** for a module's computation. UI code (`server.R`, `module_html.R`) reads pipeline results via `pipeline$read("target_name")` and triggers runs via `pipeline$run(names = c("target1", "target2"))`.

---

## File Structure

Every pipeline `main.Rmd` follows this skeleton:

```yaml
---
title: "RAVE Pipeline - Module Name"
output:
  html_document: default
  pdf_document: default
editor_options:
  chunk_output_type: console
---
```

```r
# Setup chunk (REQUIRED — must be first)
{r setup, include = FALSE}
ravepipeline::pipeline_setup_rmd("module_id")
```

```
# Pipeline targets...
{rave target_label, language = "R", export = "variable_name"}
```

```r
# Build chunk (REQUIRED — must be last)
{r build, echo=FALSE, results='hide'}
build_pipeline(make_file = "make-module_id.R")
```

```r
# Visualize chunk (optional, for interactive development)
{r visualize, echo=FALSE}
try({
  asNamespace("ravepipeline")$pipeline_dependency_graph(
    pipeline_path = ".", glimpse = TRUE)
}, silent = TRUE)
```

---

## The `{rave}` Code Block

### Syntax

````
```{rave target_label, language = "R", export = "exported_var", cue = "thorough"}
# R code that computes `exported_var`
exported_var <- ...
```
````

### Chunk Options

| Option | Required | Default | Description |
|--------|----------|---------|-------------|
| `export` | **Yes** | — | Variable name exported by this target. Must match an object created in the block. Must be unique across all targets. Cannot start with `.` or match any key in `settings.yaml`. |
| `language` | No | `"R"` | Programming language: `"R"` or `"python"`. |
| `cue` | No | `"thorough"` | Cache invalidation strategy. `"thorough"` (default) — re-run when code or any upstream dependency changes. `"always"` — always re-run regardless of cache. Use `"always"` for targets that read external state (files on disk, database queries, user selections). |
| `format` | No | auto | Serialization format. Common values: `"rave-subject"` (for `RAVESubject` objects), `"user-defined-r"` (custom serializer via `R/shared-*.R`). Most targets use the default (auto-detected). |
| `depends` | No | auto | Explicit dependency list (comma-separated target names). For R targets, dependencies are **auto-detected** by scanning for global variable references. Only needed for Python targets or to override auto-detection. |
| `pattern` | No | `NULL` | Dynamic branching pattern (advanced; maps over a list target). Rarely used. |

### Target Label vs Export Name

The **target label** (after `rave`) is a human-readable description (used in logs). The **export name** is the programmatic identifier. They can differ:

```
{rave load_and_validate_subject, language = "R", export = "subject"}
```

Here `load_and_validate_subject` is the label; `subject` is the exported variable.

---

## `settings.yaml` — Pipeline Inputs

Every pipeline has a `settings.yaml` in the same directory as `main.Rmd`. All keys in this file are automatically injected as variables into the knit environment. This is how the UI passes configuration to the pipeline.

```yaml
# settings.yaml
project_name: demo
subject_code: DemoSubject
epoch_choice: auditory_onset
loaded_electrodes: "13-16,24"
reference_name: default
```

Inside any `{rave}` block, you can directly reference `project_name`, `subject_code`, etc. — they are already bound in the environment.

**Rules:**
- `~` (YAML null) becomes `NULL` in R
- Lists and nested structures are preserved as R lists
- Target export names **cannot** collide with settings.yaml keys
- The pipeline engine creates hidden targets (`settings_path` → `settings`) that track `settings.yaml` as a file dependency, so changes to settings automatically invalidate downstream targets

---

## Dependency Graph & Caching

### How Dependencies Work

1. `settings.yaml` keys are automatically injected as individual targets
2. Each `{rave}` block's code is scanned for references to other exported variables
3. A **directed acyclic graph (DAG)** is built; `targets` (the R package) manages execution order and caching
4. The target store lives in `_targets/` (or a shared location per `_targets.yaml`)

### `_targets.yaml`

Each module has a `_targets.yaml` that configures the `targets` pipeline store:

```yaml
module_name:
  script: make-main.R
  store: shared        # shared store across modules (common)
  reporter_make: terse
  reporter_outdated: terse
```

- `store: shared` means target results are cached in a shared directory (not per-module)
- `script` points to the generated make file (produced by `build_pipeline()`)

### Cache Invalidation Tips

- Use `cue = "always"` for targets that read live data from disk (subject files, report directories, preprocessing state) — these can change outside the pipeline
- Use default `cue = "thorough"` for pure computation targets that only depend on upstream targets
- **Avoid bundling unrelated variables** into a single config target — if one changes, all downstream targets rebuild. Instead, export individual values and let each downstream target reference only what it needs
- The DAG structure determines rebuild scope — design your targets so that expensive operations only depend on the inputs they actually need

---

## Common Patterns

### 1. Load and Validate Subject

```r
{rave load_subject, language = "R", export = "subject", cue = "always", format = "rave-subject"}
subject <- ravecore::RAVESubject$new(
  project_name = project_name,
  subject_code = subject_code,
  strict = FALSE
)
```

Use `format = "rave-subject"` so the subject object is properly serialized/deserialized. Use `cue = "always"` because subject state can change on disk.

### 2. Iterate Over Subjects (Named List Pattern)

For multi-subject pipelines, return a **named list** keyed by subject code:

```r
{rave generate_per_subject_data, language = "R", export = "per_subject_results"}
per_subject_results <- structure(
  names = resolved_subjects,
  lapply(resolved_subjects, function(sc) {
    subject <- ravecore::new_rave_subject(
      project_name = pname, subject_code = sc, strict = FALSE)
    # ... compute something ...
    result
  })
)
```

The UI can then index by subject: `per_subject_results[["DemoSubject"]]`.

### 3. Brain Objects

```r
# Single subject brain
brain <- ravecore::rave_brain(subject)

# Merged group brain
brain_list <- lapply(subjects, function(sc) {
  tryCatch(ravecore::rave_brain(subj), error = function(e) NULL)
})
merged <- threeBrain::merge_brain(
  .list = brain_list,
  template_surface_types = c("inflated", "sphere.reg"),
  template_subject = template_subject,
  electrode_priority = "sphere"
)
```

Use `format = "user-defined-r"` for large brain objects that need custom serialization.

### 4. Reading Subject Metadata

```r
# Electrode table (lightweight, no reference loading)
etable <- subject$meta_data("electrodes")

# Epoch table
epoch_tbl <- subject$meta_data(meta_type = "epoch", meta_name = "auditory_onset")

# Preprocess settings
preproc <- subject$preprocess_settings
preproc$data_imported      # logical vector per electrode
preproc$notch_filtered     # logical vector per electrode
preproc$has_wavelet        # logical vector per electrode
```

**Prefer `subject$meta_data("electrodes")` over `subject$get_electrode_table()`** — the latter also loads reference data which is unnecessary overhead.

### 5. Subject Validation

`ravecore::validate_subject()` returns a `fastmap2` (nested list-like), **not** a data.frame. Each section (`paths`, `preprocess`, `meta`, `voltage_data`, etc.) contains sub-items with fields: `$name`, `$valid` (TRUE/FALSE/NA), `$message`, `$description`. You must flatten it to a table yourself.

---

## Shared Scripts (`R/shared-*.R`)

Files matching `R/shared-*.R` in the module directory are automatically sourced into the knit environment by `pipeline_setup_rmd()`. They are loaded in alphabetical order.

Use shared scripts for:
- Helper functions used by both `main.Rmd` targets and `server.R`
- Custom serializers (when `format = "user-defined-r"`)
- Plot functions loaded via `pipeline$shared_env()`

These functions are available inside `{rave}` blocks during knitting, but **not** available at pipeline execution time unless accessed through `pipeline$shared_env()`.

---

## Key API Reference

### `ravepipeline`
- `pipeline_setup_rmd(module_id)` — Initialize the knitr engine, load settings, register `{rave}` engine
- `build_pipeline(make_file)` — Compile all `{rave}` blocks into a `targets` make script
- `pipeline_from_path(path)` — Get a pipeline object from module directory
- `pipeline$run(names)` — Run specific targets
- `pipeline$read(name)` — Read a cached target result
- `pipeline$set_settings(...)` — Update settings.yaml programmatically
- `dir_create2(path)` — Create directory recursively (safe)
- `lapply_jobs(x, FUN)` — Parallel lapply for heavy computation

### `ravecore`
- `ravecore::RAVESubject$new(project_name, subject_code, strict)` or `ravecore::new_rave_subject(...)` — Create subject instance
- `ravecore::as_rave_project(name, strict)` — Get project object
- `ravecore::get_projects(refresh)` — List available projects
- `ravecore::rave_brain(subject)` — Load 3D brain object
- `ravecore::validate_subject(subject, method, verbose)` — Validate subject data integrity (returns `fastmap2`, not data.frame)
- `subject$meta_data(meta_type, meta_name)` — Read electrode/epoch/reference tables
- `subject$preprocess_settings` — Access preprocessing state
- `subject$electrodes`, `$blocks`, `$epoch_names`, `$reference_names`, `$report_path`
- `project$subjects()` — List subjects in a project
- `project$group_path(module_id)` — Get shared storage directory for a module

### `threeBrain`
- `threeBrain::merge_brain(.list, template_surface_types, template_subject)` — Merge multiple brain objects
- `threeBrain::save_brain(widget, path)` — Export brain viewer as standalone HTML
- `threeBrain::default_template_directory()` — Path to template brain directory
- `threeBrain::available_templates()` — List downloadable template brains
- `threeBrain::download_template_subject(name)` — Download a template brain

---

## Gotchas & Best Practices

1. **Export names must be unique** across all `{rave}` blocks. Duplicate export names cause a build error.

2. **Export names cannot match settings.yaml keys.** The settings are already auto-injected as individual targets.

3. **Do not use `sprintf("%s/%s", project, subject)` with `as_rave_subject()`** — prefer `new_rave_subject(project_name = ..., subject_code = ...)` for clarity.

4. **`tryCatch` around subject operations** — not all subjects have complete data. A subject might lack localization, epochs, or reference tables. Guard accordingly.

5. **Avoid side effects** in pipeline targets. Targets should compute and return a value, not write files as their primary purpose. If you must write files (e.g., saving a brain viewer HTML), return the file path so downstream targets can depend on it.

6. **Design for cache efficiency.** Put volatile inputs (user selections, settings) into early validation targets with `cue = "always"`. Expensive computation targets should use default cue and depend only on stable upstream targets. This way, re-running with the same inputs is instant.

7. **The final exported variable must exist.** If your code conditionally creates the export variable, ensure all branches assign it. The pipeline will error if the export variable is not found after execution.

8. **Python targets require explicit `depends`.** Unlike R targets where dependencies are auto-detected, Python targets need `depends = "var1, var2"` to declare upstream dependencies.

9. **Development workflow:** Knit `main.Rmd` interactively to test targets one by one (they execute in the knitr environment like normal code blocks). The `build_pipeline()` call at the end generates the make script. In production, RAVE runs the make script directly via `targets::tar_make()`.

10. **`head()` / print statements in targets:** These are fine during development (they show in the knit output) but have no effect in production pipeline runs. The last expression in the block is ignored — only the exported variable matters.
