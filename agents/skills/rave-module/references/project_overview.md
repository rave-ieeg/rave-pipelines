# Project Overview Module Reference

Project Overview (module ID `project_overview`) summarizes a whole RAVE
project: each subject's preprocessing state, the module reports saved for
each subject, electrode coverage by brain region, a group 3D viewer on a
template brain, the epochs and references, and data validation. The
overview can be exported as a zipped HTML report.

**Prerequisite:** None; the more subjects are imported, preprocessed, and
localized, the more the overview shows.

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: Choose the project; the card "Project Info" lists its subjects.
Step 2: Click "Load project".

### 2. Analysis inputs

Card "Subject Selection":

* **Subjects (blank = all)** — the subjects to include.

Card "Group-Level Sections" (all on by default):

* **3D Group Viewer** — every included subject's electrodes on a template
  brain; **Template brain** chooses the template (a template that is not
  installed is downloaded).
* **Electrode Coverage** — electrode counts per brain region (`FSLabel`).
* **Subjects Summary** — the subjects' preprocessing state.
* **Epoch & Reference Tables** — each subject's epochs and references.

Card "Subject-Level Sections":

* **Module Reports** (on) — the reports saved by modules for each subject;
  **Filter by module (blank = all)** keeps only some modules.
* **Native 3D Viewer** (off) — each subject's own brain (slow).
* **Validation** (off) — each subject's data validation (slow).

Click **Generate Report** (footer of the card) to build the checked
sections; the tabs of the card "Project Overview" show them. Click **Export
Report** (page footer) to build them and package an HTML report as a zip; a
dialog then offers "Download".

### 3. Outputs

Card "Project Overview":

* **Subject Summary** — one row per subject: electrodes, and whether the
  data are imported, Notch-filtered, wavelet-transformed, and localized (yes
  only when done for every electrode), the blocks, and the number of epochs
  and references.
* **Module Reports** — one row per saved report: subject, module, report
  name, creation time, and an "Open" link. "Show only the most recent report
  per module & report name" is on by default.
* **Electrode Coverage** — one row per brain region (`FSLabel`), the number
  of electrodes of each subject and the total, sorted by the total.
* **3D Viewer** — the group brain, or (after building the native viewers)
  one subject's brain; the cog of the card shows the "View" selector.
* **Epoch & References** — each subject's epochs (trials, conditions) and
  references.
* **Validation** — each subject's checks (section, check, valid, message).

## Common procedures

### Procedure — summarize some subjects of a project

Step 1: Choose the project and click "Load project".
Step 2: Choose the subjects, keep the group sections and "Module Reports",
and click "Generate Report".
Step 3: Read the tabs, e.g. "Electrode Coverage".

### Procedure — export the overview

Steps 1-2: reuse [Procedure — summarize some subjects of a project](#procedure--summarize-some-subjects-of-a-project).
Step 3: Click "Export Report", wait for the dialog "Download packaged
report", and click "Download" (`<project>_overview.zip`).

## Caveats

* A tab whose section is unchecked keeps showing what it showed after its
  last build, which may be of other subjects; the export leaves unchecked
  sections out.
* Choosing a template brain that is not installed downloads it.
* "Native 3D Viewer" and "Validation" take long for large projects.
* The export renders into a temporary folder; downloading the zip deletes
  that folder. Nothing is written into the project.
* With a ravepipeline that runs background jobs on an undrained pipe, the
  export of a large report can stop while zipping, and the dialog never
  opens. During MCP calls the module zips quietly to avoid this; if
  `export_status` stays "running" for long, tell the user.

## Run the pipeline without the UI

The UI saves `project_name`, `subject_codes` (empty means all),
`template_subject`, and `module_filter`; the section checkboxes decide which
targets are built.

```r
pipeline <- ravepipeline::pipeline("project_overview")

pipeline$set_settings(
  project_name = "demo",                          # project
  subject_codes = c("DemoSubject", "YAB"),        # subjects (NULL = all)
  template_subject = "cvs_avg35_inMNI152",        # template brain
  module_filter = NULL                            # modules of the report table (NULL = all)
)

pipeline$run(c(
  "resolved_subjects",
  "snapshot_group_subject_summary",     # Subjects Summary
  "snapshot_group_electrode_coverage",  # Electrode Coverage
  "snapshot_subject_module_reports",    # Module Reports (one table per subject)
  "snapshot_subject_meta_summary",      # Epoch & Reference Tables
  "snapshot_group_brain"                # 3D Group Viewer
))
coverage <- pipeline$read("snapshot_group_electrode_coverage")
brain <- pipeline$read("snapshot_group_brain")
brain$plot()
```

## Drive the module with MCP tools

Operate the live module the way a person does. Each step names an MCP tool;
call it as `tool("tool__NAME", arg = value)`. The module registers these
interactive scripts: `load_data` ("Load project"), `generate_report`
("Generate Report"), `run_analysis` ("Export Report"), and the read-only
`export_status` and `pipeline_progress`.

**Run `load_data` first, then `generate_report`.** Nothing writes into the
project. Confirm a template brain that is not installed, the slow sections,
and an export with the user first.

A script's reply has `result` (its return value) and `output` (what it
printed while it ran).

### Load data

* `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`
* `tool("tool__module_interactive_script_run", name = "load_data")`. The
  `result` has one line per subject:
  `DemoSubject (5 electrodes; imported yes, notch yes, wavelet yes, localized yes; epochs 2, references 2)`.

### Configure and run

* `tool("tool__shiny_input_update", inputId = "subject_codes", value = "[\"DemoSubject\", \"YAB\"]")`
  (`"[]"` for all subjects)
* Group sections: `group_viewer` (`"true"`/`"false"`), then
  `template_subject` (e.g. `"cvs_avg35_inMNI152"`); `electrode_coverage`,
  `subjects_metadata`, `epoch_references`
* Subject sections: `module_reports`, then `module_filter` (a JSON array of
  module names, `"[]"` for all); `native_viewer`, `validation`
* Check them: `tool("tool__shiny_input_info")`.
* Build: `tool("tool__module_interactive_script_run", name = "generate_report")`.
  The `result` is "Built for project ...: subjects ...; sections ...". An
  error makes the call fail with its message (people see an alert; close it
  with `tool("tool__shiny_ui_operate", action = "close_alert2")`), and
  `pipeline_progress` lists each step.

### Inspect results

* Tables: show the table's tab first (a table in a hidden tab comes back
  empty), then read it:
  * `tool("tool__shiny_input_update", inputId = "output_cardset", value = "Module Reports")`
    (tabs: `Subject Summary`, `Module Reports`, `Electrode Coverage`,
    `3D Viewer`, `Epoch & References`, `Validation`)
  * `tool("tool__shiny_output_result", outputId = "module_reports_table", transform_image = false)`;
    the others are `subjects_summary_table`, `electrode_coverage_table`,
    `epoch_reference_table`, and `validation_table`. The reply is the
    table's HTML: the page showing (25 or 50 rows) with its row count, e.g.
    "Showing 1 to 3 of 3 entries".
* 3D viewer (`outputId = "brain_widget"`): choose the brain with
  `tool("tool__shiny_input_update", inputId = "brain_viewer_selector", value = "Group Brain")`
  (or a subject code, after building the native viewers), then
  `tool("tool__rave_3dviewer_get", outputId = "brain_widget", name = "controllers")`.

### Export the report (confirm first)

1. Confirm the sections with the user.
2. `tool("tool__module_interactive_script_run", name = "run_analysis")`: it
   builds the overview, then schedules the export and returns.
3. Poll `tool("tool__module_interactive_script_run", name = "export_status")`
   until "Export finished: <zip>".
4. Tell the user to click "Download" in the dialog "Download packaged
   report"; you cannot download it. To close the dialog instead:
   `tool("tool__shiny_ui_operate", action = "dismiss_modal")`.
