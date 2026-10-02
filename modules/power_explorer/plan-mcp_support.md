# Plan: MCP support for `power_explorer`

This plan is generated based on `agents/skills/build-module-mcp`

## Context

Power Explorer (`modules/power_explorer`) analyzes baseline-corrected power. People load a subject, choose electrodes, baseline, analysis windows and trial groups, press RAVE!, and read heatmaps, per-electrode statistics and models; they can also export data, save for group analysis, and generate an HTML report. The user asked for MCP support that follows `agents/skills/build-module-mcp/SKILL.md`, **without changing existing logic**.

Where it stands:
- The user's uncommitted batch edit (the same one as in 9 other modules) already turned "Load subject" into `ravedash::load_data_button()` plus script `load_data`, and the RAVE! observer into script `run_analysis`. `agents.yaml` already lists the `module_interactive_script_*` tools.
- Most inputs were registered in March ("Integrated agents"). Three gaps remain:
  - the module's own epoch loader (`build_epoch_loader`) registers nothing, so agents cannot pick an epoch;
  - 7 of the 8 heatmap-panel options per plot are unregistered;
  - the 5 output tab sets are unregistered.
- Seven buttons have no agent path. `run_analysis()` says "Analysis not run" only in toasts, which agents never see (`ravedash::show_notification` does not log).
- The per-electrode statistics exist only as a plain DT table. `get_results.R` prints just a truncated `str()`.
- The manual is marked "TODO: needs rewrite", and some of its facts are wrong. For example, its units are "decibel / % signal change / z-score", but the code has `% Change Power`, `% Change Amplitude`, `Decibel`, `z-score Power`, `z-score Amplitude`, `z-score Decibel`. There is no `test-mcp.R`.

**Decisions made with the user (2026-09-30):**
- **Baseline:** build on the uncommitted power_explorer edit, unchanged except for descriptions and return values. Edits in other modules are not touched.
- **Writes agents may run, always after confirming the settings with the user:** Export electrodes, Save for group analysis, and the HTML report.
- **"Cluster -> electrodes.csv" is for people only:**
  - no script;
  - its observer and the dialog's Save observer stay untouched;
  - the button stays registered with `writable = FALSE`;
  - `shiny_ui_operate` stays out of `agents.yaml`, because it can click any element.
  
  The cost: agents cannot close alerts or dialogs, so the user closes "Done with exporting!".
- **Print-only log lines are approved** inside `run_analysis()` and inside the moved Export and Save bodies, next to each message that only people see.
- **Test subject:** `demo/DemoSubject`, with writes.
- **Rules also go into descriptions.** Over MCP, the Ask/Plan/Execute modes don't gate tools. So the rules go into the script and input descriptions and the manual, not only into the system prompt.

**Step 0, right after approval:** save this plan as `modules/power_explorer/plan-mcp_support.md`.

**Visible UI changes: none.** There are no pipeline changes (`main.Rmd`, `make-power_explorer.R`, settings keys), no `register_output`, and no commits.

**Code that buttons run (the complete list):**
1. **Seven observer bodies move verbatim into scripts.** Each observer keeps its `bindEvent(...)` event and flags, and calls `server_tools$trigger_script("<name>")`. Checked: inside `trigger_script`'s `eval()`, `on.exit()` and `return()` behave as they do in the observer.
2. **Approved log lines:**
   - 6 in `run_analysis()`, one next to each `show_notification`;
   - 1 in the Export body ("No electrodes selected for export");
   - 2 in the Save body ("Could not save results").
3. **Lines whose values people never see:**
   - `load_data`'s summary line;
   - in the `run_analysis` script, a check of whether the results changed, plus a summary;
   - Export and Save get `started <- Sys.time()` as their first line and, as their last line, the folder this run wrote.

Nothing else changes inside `run_analysis()`, the moved bodies, or any other observer.

## Inventory

| Button (inputId) | Reads | Writes | Agent path |
|---|---|---|---|
| Load subject (`load_data_button`) | `loader_project_name`, `loader_subject_code`, `loader_epoch_name` (+ window, anchors), `loader_reference_name`, `loader_electrode_text` | `settings.yaml`, target `repository`; subject defaults if "Set as the default" is checked; `meta/epoch_single_trial_<epoch>.csv` if "Load first block as single trial" is checked | script `load_data` (exists): description, summary |
| RAVE! (footer) | electrodes or custom ROI, baseline, windows, factors, quick mode, export options | `settings.yaml`, pipeline cache (`shared/`, gitignored) | script `run_analysis` (exists): description, result, log lines |
| Assign all ROI levels to groups | `enable_custom_ROI`, `custom_roi_variable` | input `custom_roi_groupings` | new script `assign_roi_levels` |
| Clear groups | same | same | new `clear_roi_groups` |
| Cluster -> brain viewer | cluster table (Over Time heatmap) | viewer refresh | new `cluster_to_viewer` |
| Cluster -> ROI | cluster table | custom ROI inputs (`PE_Cluster`) | new `cluster_to_roi` |
| Export (`btn_export_electrodes`) | `electrodes_to_export`, `*_to_export`, ROI filter, analysis inputs | new `<subject>/power_explorer/pe_export_<time>/`; re-runs the analysis on those electrodes | new `export_electrodes` |
| Save! (`save_pipeline_for_group_analysis`) | `save_pipeline_for_group_analysis_label`, `replace_existing_group_anlysis_pipeline` | fork `<subject>/pipelines/power_explorer/power_explorer-<label>-<time>/` and registry; "Replace" deletes older forks with that label | new `save_for_group_analysis` |
| Generate Report (`btn_export_html_report`) | `exp_html_electrodes_to_include`, `exp_html_graphs` | `<subject>/reports/report-univariatePower_datetime-<time>_power_explorer/` (background job) | new `generate_report`, plus read-only `report_status` |
| Cluster -> electrodes.csv, then the dialog's Save | cluster table, dialog inputs | `meta/electrodes.csv` | **none, by decision** (the button is read-only) |
| Load Settings (file upload) | an uploaded YAML file | inputs | people only; agents set the inputs directly |
| Save settings, plot camera, meta-data download | none | browser download | people only; agents read results and outputs instead |
| Plot clicks, the "Manual threshold" dialog, the click table's Clear/Flag | clicks, row selection | labels, trial outliers | people only; agents use `electrode_text` and `electrode_statistics` |
| Double-click in the 3D viewer | the clicked electrode | runs the analysis on it | agents set `electrode_text`, then run `run_analysis` |

Plain outputs (`per_electrode_results_table`, `by_condition_by_trial_clicks`) are read with `shiny_query_ui` after their tab is activated. Registered outputs work with `shiny_output_result` even in hidden tabs.

## Changes

### 1. `R/aaa-presets.R`: register the epoch loader (tags unchanged)
In `build_epoch_loader()`'s `ui_func`, wrap each tag in `shidashi::register_input()`, with the same IDs and wording as ravedash's own `presets_loader_epoch()`:

| inputId | update | The description says |
|---|---|---|
| `loader_epoch_name` | `shiny::updateSelectInput(value=selected)` | the subject's epoch names; the choices load after the subject is chosen |
| `loader_epoch_name__trial_starts` / `__trial_ends` | `shiny::updateNumericInput` | seconds before the event (negative; default -1) / after it (default 2) |
| `loader_epoch_name__trial_starts_rel_to_event` / `__trial_ends_rel_to_event` | `shiny::updateSelectInput(value=selected)` | the anchor event: "Trial Onset" or an event of the epoch |
| `loader_epoch_name__load_single_trial` (**`writable = FALSE`**) | `shinyWidgets::updatePrettyCheckbox` | when checked, loading writes `meta/epoch_single_trial_<epoch>.csv`; only people may check it |

"Set as the default" stays unregistered, as it is in ravedash's epoch and reference presets.

### 2. `R/shared-ui.R`
- **`make_heatmap_control_panel()`:** register its 7 unregistered inputs:
  - `<prefix>_range_is_percentile`, `_scale_is_global`, `_scale_based_on_aw`, `_xlim`, `_ncol`, `_byrow`, `_show_window`;
  - update functions: `shiny::updateCheckboxInput`, `updateSliderInput`, `updateNumericInput`;
  - each description names its plot through a small prefix lookup:

    | Prefix | Plot |
    |---|---|
    | `bfot` | By Frequency › Over time |
    | `bfc` | By Frequency › Correlation |
    | `otbt` | Over Time › By Trial |
    | `otbe` | By Electrode › Over Time |
    | `bewot` | By Electrode › Waterfall over Time |
- **`make_by_frequency_tabset()`:** register `by_frequency_tabset`.

### 3. `R/module_html.R` (tags unchanged)
- **Tab sets.** Register the output card tab sets `brain_viewers`, `by_electrode_tabset`, `over_time_tabset` and `by_condition_tabset`, with `update = "shidashi::card_tabset_activate(value=title)"` as in `modules/reference_module/R/module_html.R`. Each description lists the tab titles.
- **Descriptions (text only)**, where the value format or the order matters:
  - `ui_analysis_settings`: JSON `[{"label","event","time":[a,b],"frequency_dd":"Select one","frequency":[lo,hi]}]`, 1–5 windows. A preset band in `frequency_dd` is copied into `frequency`. Identical or heavily overlapping windows stop the run.
  - `first_condition_groupings`, `second_condition_groupings`, `custom_roi_groupings`:
    - format: JSON `[{"label","conditions":[...]}]`;
    - where the choices come from;
    - a condition listed in two levels is dropped from the later level;
    - the second factor needs at least 2 levels that together cover the first factor's conditions exactly once.
  - `condition_variable`: changing it resets the first factor to "All Conditions", so set it first.
  - `baseline_window`, `baseline_scope`, `baseline_unit`, `quick_omnibus_only`, `enable_custom_ROI`, `custom_roi_variable`, `custom_roi_type`, and the export inputs: their real choices and effects.
  - `pes_select_mode`, `pes_selected_action`: they act on plot clicks, so they are for people only. "Manual threshold" opens a dialog that agents can neither fill in nor close.
  - Each read-only button names the script to run instead. `otbe_cluster_to_electrodes_csv` states the people-only rule. `file_load_settings` says agents set the inputs instead.

### 4. `R/loader.R`: `load_data`
- **Longer description:** the inputs by ID; what the script writes, including the single-trial epoch file; what it returns.
- **Summary as the last line.** It is wrapped in `tryCatch`, so a load never fails because of it:
  `"Loaded <project>/<subject>: epoch <name> (<n> trials, <start> to <end> s), reference <name>, electrodes <list>; conditions (<column>): …; events: …; frequencies <lo>-<hi> Hz"`.

### 5. `R/module_server.R`
- **`run_analysis` script:**
  - It gets a full description.
  - Its body becomes `last <- local_reactives$update_pes_plot; run_analysis(); <result>`.
  - The result is `"Analysis done (quick|full): electrodes …; windows …; baseline …; first factor …; second factor …; custom ROI …"`, built from `pipeline$get_settings()`. If `update_pes_plot` did not change, the result is `"Analysis did not run: see the warnings in output"`.
  - It always returns a string and never throws for this case, so people clicking RAVE! see nothing new.
- **`run_analysis()`:** one `ravepipeline::logger(<same text>, level = "warning")` line next to each of its 6 `show_notification` calls (approved).
- **New scripts, with the observer bodies moved verbatim:** `assign_roi_levels`, `clear_roi_groups`, `cluster_to_viewer`, `cluster_to_roi`, `export_electrodes`, `save_for_group_analysis`, `generate_report`.
  - **`export_electrodes`:**
    - it gets the log line and `started <- Sys.time()` as its first line;
    - its last line returns `local_data$results$data_for_export` only if that folder was created after `started`; otherwise it returns `"Nothing exported: see output"`. This matters because `run_analysis()` can return early and leave an old path in place.
    
    The description says:
    - confirm with the user first;
    - the inputs it reads;
    - what it writes: one `<project>_<subject>_eNNNN.csv` per electrode, plus `metadata.yaml`;
    - that it re-runs the analysis on the export electrodes, so the plots then show those electrodes;
    - that "Done with exporting!" stays open until the user closes it.
  - **`save_for_group_analysis`:**
    - it gets the 2 log lines and `started` as its first line;
    - its last line returns the fork folder registered after `started` (from `subject$list_pipelines("power_explorer")`, with policy `group_analysis` and this label); otherwise it returns `"Not saved: see output"`.
    
    The description says:
    - confirm the label and "Create new" or "Replace" first;
    - "Replace" deletes the older forks with that label;
    - it needs a completed `run_analysis`.
  - **`generate_report`:** the body moves verbatim. It ends in `return()`, so it has no result. The description says:
    - confirm with the user first;
    - the report is a background job;
    - people see "Report(s) scheduled", then "Report generated!";
    - agents poll `report_status`.
- **New read-only scripts:**
  - **`electrode_statistics`:**
    - It returns at most 100 lines: a header line, then one line per row of `omnibus_results$stats`.
    - The rows are `m(…)`, `t(…)`, `p(…)`, `p_fdr(…)` and `currently_selected`. Each line reads `"<row>: 13=…, 14=…"`, with values to 4 significant digits.
    - Before any run, it returns "No results yet: run `run_analysis` first."
  - **`report_status`:**
    - It gives the state of the latest report job from `ravepipeline::check_job()`: initialized, started, running, finished, or errored with its message.
    - When the job is finished, it also gives the newest report folder created after the job started.
    - It never calls `resolve_job()`, because its `auto_remove` would delete the job that the module's promise is waiting on.
  - **`pipeline_progress`:** copied from `modules/wavelet_module/R/module_server.R`. It returns `<target>: <progress>` lines plus the `targets::tar_meta` error messages, because a failing target shows only as "✖ <target> errored".
- **Untouched:**
  - the `otbe_cluster_to_electrodes_csv` and `do_write_clusters_to_columns` observers;
  - the file upload and the downloads;
  - the click, threshold and outlier observers;
  - the plot-option observers;
  - the double-click in the 3D viewer.

### 6. `modules/power_explorer/agents.yaml`
**Tools.** Keep the working tree's tool list, and add:
- `rave_3dviewer_get` (exploratory) and `rave_3dviewer_set` (executing), for the viewers `brain_viewer` and `brain_viewer_movies`;
- a comment saying why `shiny_ui_operate` is left out.

**System prompt (rewritten).** It covers:
- the module and its ID, the modes, the data flow, and a tools table with the manual first;
- the order:
  1. set the loader inputs, then run `load_data`;
  2. set the analysis inputs in dependency order (`condition_variable` before the groups, `enable_*` before its groups), and check each with `shiny_input_info`;
  3. run `run_analysis`;
  4. read the results with `electrode_statistics` and `shiny_output_result`, and with `pipeline_progress` after an error;
- **confirm with the user before** running `export_electrodes`, `save_for_group_analysis` or `generate_report`;
- **never write electrodes.csv:** point the user to "Cluster -> electrodes.csv" instead;
- what is for people only: plot clicks, outliers, thresholds, the file upload, downloads;
- alerts stay open until the user closes them;
- in Plan mode, ask the user to click instead.

### 7. Manual `agents/skills/rave-module/references/power_explorer.md` (rewrite)
Title "# Power Explorer Module Reference". The `TEMPLATE.md` headings stay unchanged, and every claim is checked against the code. It covers:
- **Prerequisite:** wavelet power, an epoch, and a reference.
- **Inputs:** the loader and analysis inputs, with their real choices and defaults (units, scopes, windows, factors, custom ROI modes, quick mode, movie maker).
- **Outputs:** per card and tab, including which ones are registered.
- **Procedures:**
  - compare two trial groups;
  - a two-factor design;
  - custom ROI;
  - clusters → ROI;
  - export (confirm first);
  - save for group analysis;
  - HTML report.
- **Caveats:**
  - settings are saved only when RAVE! runs;
  - quick mode shows only the statistics and the viewer;
  - `condition_variable` resets the groups;
  - duplicate conditions and overlapping windows;
  - auto re-calculation re-runs the analysis when the electrodes change;
  - the export re-runs the analysis on its electrodes;
  - single-trial loading writes an epoch file;
  - which actions are for people only.
- **Plain-R pipeline:** how UI values map to settings (e.g. `baseline_settings`, the custom ROI electrodes), and the quick and full target lists.
- **The MCP section.**

### 8. `modules/power_explorer/test-mcp.R` (new)
The helpers come from `modules/compatibility_rave1/test-mcp.R`.

**Settings at the top:**
- subject `demo/DemoSubject`: epoch `auditory_onset` (-1 to 2 s), reference `default`, electrodes `13-16,24`;
- analysis electrodes `14-16`;
- baseline -1 to 0 s, unit `% Change Power`;
- window HighGamma: 0-1 s, 70-150 Hz;
- groups Auditory (`drive_a`, `known_a`, `last_a`, `meant_a`) vs AudioVisual (the `*_av` conditions);
- export electrodes `14-15`;
- label `mcp_test`;
- `do_write <- TRUE`.

**Steps:**
1. **Protocol.**
   - The tools list has no `shiny_ui_operate`.
   - The manual loads.
   - All 12 scripts are listed.
2. **Load.**
   - Set the loader inputs, including the newly registered epoch inputs, then run `load_data`.
   - The result and the `settings.yaml` values are exact.
3. **Quick run.**
   - Set the inputs, then run `run_analysis`; the result starts with "Analysis done (quick)".
   - `settings.yaml` matches the inputs exactly.
   - `omnibus_results` was rebuilt after the test's start.
   - `electrode_statistics` equals the same lines, built in the test from `pipeline$read("omnibus_results")$stats`.
4. **Full run.**
   - The `by_condition_statistics` HTML contains the model formula.
   - `over_time_by_condition` comes back as an image.
5. **Error cases.** In each, nothing is refreshed and no dialog opens.
   - Two identical windows: the result is "did not run", and the warning is in `output`.
   - A first-factor level emptied by duplicates: "Insufficient Data".
6. **Second factor** (drive+known vs last+meant).
   - The settings are exact.
   - The statistics have Factor1×Factor2 rows.
7. **Custom ROI on `FSLabel`** (13-15, 16, 24).
   - `assign_roi_levels` gives 3 groups.
   - With "Group/Stratify", groups STG (13-15) and Other (16, 24) are saved with electrodes exactly `13-15` and `16,24`.
   - `clear_roi_groups` leaves one "All levels" group.
8. **Clusters.**
   - Sort by "Activity Correlation" with k = 2, then render `over_time_by_electrode`.
   - `cluster_to_roi` sets `custom_roi_variable` to `PE_Cluster`.
   - `cluster_to_viewer` runs.
9. **Forbidden button.**
   - `shiny_input_update` on `otbe_cluster_to_electrodes_csv` gives "read-only".
   - `shiny_ui_operate` gives `isError`.
   - The `electrodes.csv` checksum is unchanged.
10. **3D viewer.**
    - `rave_3dviewer_get` lists the controllers of `brain_viewer`.
    - Set one statistic from `controller_options` and read it back.
11. **Writes.** The test stops before this step if `do_write` is FALSE.
    - **Export.**
      - With empty electrodes: the log line appears, and nothing is written.
      - With `14-15` (collapsed frequency and time; raw trials that are in the groups):
        - the export folder is the only new folder, and it is fresh;
        - it holds exactly `demo_DemoSubject_e0014.csv`, `demo_DemoSubject_e0015.csv` and `metadata.yaml`;
        - `metadata.yaml` matches the inputs;
        - each CSV has 127 rows, one per trial of the two groups;
        - the values equal an independent ravecore computation: `prepare_subject_power_with_epochs` plus `power_baseline`, then the mean over 0-1 s × 70-150 Hz, to floating-point tolerance.
    - **Group save.**
      - With an empty label: the log line appears, and no fork is written.
      - `mcp_test` with "Create New": a fork plus a registry row (policy `group_analysis`); the fork's `settings.yaml` equals the module's.
      - "Replace existing": the older `mcp_test` fork is deleted.
    - **Report** ("Aggregate only", one graph).
      - Poll `report_status` until it says "finished" (at most 5 minutes).
      - A new report folder with an HTML file exists.

## Verification
0. The copy of this plan exists.
1. Every edited R file parses (`Rscript -e 'parse("<file>")'`).
2. **Catalog, offline.** Run `shidashi::init_app()`, then `shidashi:::mcp_harvest_module("power_explorer", root_path = normalizePath("."))`. It lists the `rave_3dviewer_*` tools and no `shiny_ui_operate`.
3. **Live run**, with the skill's recipe on port 17299:
   - **Back up** `settings.yaml`, `_targets.yaml`, `shared/` and `preferences/` to the scratchpad. `preferences/` is the plot-option cache that the user's own app shares.
   - **Start** the app, plus a scratchpad copy of `live-browser.js` that also reads a `click` file (a CSS selector).
   - **Run** `RAVE_TEST_PORT=17299 Rscript modules/power_explorer/test-mcp.R`, with screenshots.
   - **Person clicks**, each with a screenshot:
     - Load subject;
     - RAVE!, and RAVE! with identical windows, which shows the "Analysis not run" toast;
     - the four ROI and cluster buttons;
     - Export with no electrodes, which shows "Export not started"; then with `14-15`, which shows "Done with exporting!";
     - Save! with an empty label, which shows "Could not save results"; then "Replace" on `mcp_test`;
     - Generate Report, which ends with "Report generated!".
   - **Stop** the app, restore the four backups, and check that `git diff` shows only the intended files.
4. **Diff review** against the Done checklist:
   - no UI or pipeline change;
   - no second code path;
   - the old observer bodies are gone;
   - the people-only observers are unchanged.
5. **Report** in the agent file's format:
   - the inventory;
   - the approvals;
   - the files changed;
   - the test result and screenshots;
   - the restored files;
   - behaviour changes for people (none expected);
   - the subject files written (below);
   - the findings.

**Subject files written to `demo/DemoSubject`:**
- 2 export folders under `power_explorer/`: one from the test, one from the click;
- one `mcp_test` group-analysis fork, plus an updated `pipelines/pipeline-registry.csv` (the click replaces the test's fork);
- 2 report folders.

Nothing that already exists is overwritten.

## Found, not changed
- "or Mask file" (`loader_mask_file`) is not read anywhere.
- "Load Settings" (`update_all_settings()`) restores the baseline window and scope, the condition variable, the windows, the groups and the ROI variable. It does not restore the unit, the electrodes, the second-factor and custom-ROI switches, or the export options.
- The dialog's Save with "Overwrite" calls `utils::write.csv(x, file)`, which adds a row-number column to `electrodes.csv`.
- `get_omni_stat_row()` falls back to `omni_stats[ind[1,],]`, which errors because `ind` is a vector.

## Implementation notes (2026-09-30)

What differed from the plan, and why:
- **`generate_report`** also got one first line, `local_data$report_scheduled_at <- Sys.time()`, whose value people never see. The module's own promise calls `ravepipeline::resolve_job()` as soon as the report job ends, which deletes the job, so `report_status` finds a finished report by its new `report.html`.
- **The subject's pipeline folder is `<subject>/pipelines/`**, not `<subject>/rave/pipelines/`; the text above is corrected.
- **Quick mode exports nothing.** The quick branch of `run_analysis()` ignores `extra_names`, so `data_for_export` is never built. `export_electrodes` returns "Nothing exported", and its description, the manual, and the test say so.
- **`load_data` does not clear the results of an earlier run** in the same browser session (`local_data$results`), so `electrode_statistics` says so in its description.
- **"Replace existing" keeps the older saves** (see below); the test checks the deletion once ravecore lists the saves again, and the current behaviour until then.
- **Person clicks** (screenshots): RAVE! (and "Analysis not run" on identical windows), the four ROI and cluster buttons, Generate Report, and Load subject were clicked for real; Export and Save! only on their error paths ("Export not started", "Could not save results"), to write nothing more.

Found, not changed (in addition to the list above):
- **ravecore:** `RAVESubject$list_pipelines(all = TRUE)` always returns no rows: the `else` branch of `if (!all && ...)` replaces the registry with an empty table. `ravepipeline`'s `fork_to_subject(delete_old = TRUE)` builds its registry from that call, so it never deletes older saves and rewrites `pipelines/pipeline-registry.csv` with the header only (DemoSubject's was already header-only before the test).
- Stratified and interaction contrasts are never computed: `fe` is pasted into one string before `length(fe) > 1` is checked (target `across_electrode_statistics`, and the module's contrast choices).
- The export's "Custom ROI" filter is not implemented in `build_data_for_export` (`v` stays empty); found by reading the code, not run.
- Sizes: a group-analysis save is about 26 MB (it copies the module folder), and a report with one graph and aggregate plots only about 66 MB.

## Follow-up fixes (2026-09-30)

Rechecked, then fixed with the user's approval:
- **ravecore `list_pipelines(all = TRUE)`** returned no rows; the default `all = FALSE` was fine. Fixed in the source only, `../ravecore/R/class005-subject.R`: empty table only when no fork is found, dedupe only when `!all`. Not installed or committed; until a release, "Replace existing" keeps older saves.
- **Stale results after loading new data.** The data-loaded observer now also resets `update_pes_plot` and `update_cluster_table` and clears `local_data$results` and the cluster table, and `basic_checks()` asks for results. Outlier trials, electrode labels, and the threshold are cleared when the subject or epoch changes (the user's choice).
- **Stratified and interaction contrasts** are computed again: `fe` stays a vector of effect names, and `fe_formula` builds the model. `main.Rmd` changed, and `make-power_explorer.R` was regenerated with `ravepipeline::pipeline_render()`; only the `across_electrode_statistics` target differs.
- **Quick-mode export** works: `run_analysis()` honours `extra_names` in quick mode.
- **"Cluster -> electrodes.csv"** writes no row names.
- **`get_omni_stat_row()`** typo fixed.

The outlier behaviour the user asked about already exists: averages and statistics drop flagged trials at the next RAVE!, and the By Trial scatter keeps them as hollow dots while "Show Outliers" is on.

Left for the user's PR: the export's "Custom ROI" filter, the unused "or Mask file" input, and the partial "Load Settings".

## Flagged trials kept in the epoch (2026-10-02)

### Context

Flagged outlier trials (`local_data$trial_outliers_list`) are dropped from all averages, statistics, viewers and exports from the next RAVE! on. But they live only in the browser session:
- they start empty in every session ([module_server.R:53](modules/power_explorer/R/module_server.R#L53));
- "Load Settings" ignores them;
- the next RAVE! after reopening the module saves an empty list, so the flagged trials come back without notice.

Agents can't flag trials (it takes plot clicks), and nothing tells them which trials were left out.

Your ravecore working tree (uncommitted: `R/class004-epoch.R`, `R/aaa-constants.R`) adds the epoch column `ExcludedHint`:
- `epoch$excluded_trials` reads the marked trials;
- `epoch$exclude_trials(..., add = FALSE)` replaces the marks;
- `epoch$save()` renames the old file to `epoch_<name>_[YYYYmmdd_HHMMSS].csv`, writes `epoch_<name>.csv`, and writes or removes `epoch_<name>_OutlierRemoved.csv`.

Backups don't show up as epochs (the `epoch_names` pattern excludes `[...]`). The installed ravecore (0.1.1.17) doesn't have this column yet, and the source still says 0.1.1.17.

**Decisions made with you (2026-10-02):**
- The flagged trials stay in step with the epoch's `ExcludedHint`. People save them into the epoch once they're satisfied, so the flags persist across sessions.
- The save action is a link in the Click Details caption. Only people may save; agents ask the user to click it, with directions.
- Agents may flag and unflag trials through a new visible "Flagged trials" field above the Click Details table.
- The empty Click Details table also shows its caption and links, so a cleared list can be saved.
- Agents are told about excluded trials in the load and run summaries, the field description, the manual and `agents.yaml`.

**Not changed:** the pipeline (`main.Rmd`, `make-power_explorer.R`, settings keys; it still reads only `trial_outliers_list`), exports, "Load Settings", and other modules. No commits.

### Step 0, right after approval

- Add this plan as a new section, "Flagged trials kept in the epoch (2026-10-02)", to `modules/power_explorer/plan-mcp_support.md`.
- Update memory `power-explorer-mcp-scope.md` with the decisions above.

### Visible UI changes (each approved, 2026-10-02)

1. **New field** "Flagged trials (applied at RAVE!)" above the Click Details table (By Condition card → By Trial tab, right column). It's a `shiny::textInput` with placeholder `e.g. 3,17,40-42` and `updateOn = "blur"`, so it sends its value on blur or Enter, not on every keystroke.
2. **New link** "Save Flags to Epoch" (`ravedash::shiny_icons$save`), after "Flag Selected (requires re-RAVE)" in the caption. Each click fires `Shiny.setInputValue(..., {priority: 'event'})`, so clicking twice also works.
3. **Empty table:** the same caption, with all three links, also appears above "No Clicks / Outliers".
4. **New toasts:**
   - "Flags saved to epoch": the trials and the files written, and a note that the old file was kept as a backup.
   - "Flags not saved": the reason.
   - "Flagged trials" warnings: unreadable text is put back to the current list; trials not in the epoch are dropped.

**Behavior change:** trials the epoch marks `ExcludedHint` start out flagged, so they are left out from the first RAVE! after loading. No epoch has that column today, so existing subjects see no change.

### Changes

#### `R/shared-functions.R`: helpers (also in the pipeline's shared env, so tests can call them)
- `epoch_flagged_trials(epoch)`: `sort(as.integer(epoch$excluded_trials))`. With an older ravecore this is empty, because `$excluded_trials` is `NULL`.
- `same_epoch_trials(e1, e2)`: same `Trial`, `Block`, and `Time` (within 1e-3, like ravecore) after ordering by trial. `FALSE` if either is `NULL`.
- `save_trial_flags_to_epoch(repository, trials)`:
  1. Reads a fresh `ravecore::RAVEEpoch$new(repository$subject, repository$epoch_name)` from disk, so the loaded repository is never changed.
  2. Stops with "needs a newer ravecore" when `exclude_trials` is missing.
  3. Stops when the file's trials differ from the loaded epoch's, e.g. after an `_OutlierRemoved` save renumbered them or after an outside edit: "…changed since the data were loaded; load the data again, then save". Without this check, a second save could mark the wrong trials.
  4. Calls `$exclude_trials(as.integer(trials), add = FALSE)` and returns `$save()`'s paths.

#### `R/module_server.R`
- **`set_trial_flags(trials, refresh = TRUE)`**: the single place that sets the list.
  - Sorts the trials as unique integers and drops trials not in `component_container$data$repository$epoch$trials`, with a logger warning and a toast. Without loaded data, it skips this check.
  - Sets `local_data$trial_outliers_list` (`NULL` if empty).
  - Updates the field to `dipsaus::deparse_svec(trials)` only when the field shows something different.
  - With `refresh`, fires `update_by_condition_plot` and `outliers_updated`, as the flag links do now. The data-loaded observer passes `refresh = FALSE`, because it has just reset those triggers and cleared the results.
  - Storing integers also fixes the click path saving trial numbers as strings.
- **Field observer** (`input$flagged_trials`):
  - returns at once when the text already equals the current list (the echo of `set_trial_flags`);
  - parses with `dipsaus::parse_svec(text, sep = ",|;", connect = ":-")`;
  - non-blank text that doesn't parse (`"abc"`, `"1.5"`) warns and puts the field back; it never clears the list;
  - blank text clears the list; anything else goes to `set_trial_flags`.
- **"Flag Selected" / "Clear Selected" observers** (≈L2209–2250): the toggle and remove logic stays. Only the assignment lines (and their reactive triggers) become `set_trial_flags(...)`.
- **Data-loaded observer** (≈L1295–1306):
  - the electrode labels and the threshold keep the current rule (same subject and epoch);
  - the flags are reset with `set_trial_flags(epoch_flagged_trials(new_repository$epoch), refresh = FALSE)` unless the subject, epoch, **and** trials are the same (`same_epoch_trials(old, new)`). Unsaved flags of this session survive a reload of the same epoch, and a renumbered epoch starts over.
- **Click Details caption** (≈L2161–2186): moved into a helper used by both the empty and the non-empty table. The existing tags are copied as they are, and `' | '` plus the save link are added at the end.
- **Save observer** (`input$save_flags_to_epoch`), people only:
  - no script, and not registered: a link drawn inside the DT caption can't be registered, the same as the two links beside it;
  - calls `save_trial_flags_to_epoch(component_container$data$repository, local_data$trial_outliers_list)` inside `tryCatch`;
  - shows the success or failure toast, with a `ravepipeline::logger` line next to it.
- **`summarize_analysis()`** adds `; flagged trials (left out): 3,17` (or `none`), from `settings$trial_outliers_list`. The tests check summaries with `startsWith`, so adding to the end breaks nothing.
- **`run_analysis` script description**: also reads `flagged_trials`.

#### `R/module_html.R`
- In the By Trial tab's right column (`column(width = 5)`, ≈L1464), above `DT::dataTableOutput(ns('by_condition_by_trial_clicks'))`: `shidashi::register_input(textInput(...), inputId = "flagged_trials", update = "shiny::updateTextInput", description = ...)`.
- The description says:
  - the field holds the whole list (setting it replaces the list; empty clears it) and applies at the next RAVE!/`run_analysis`;
  - the trials are left out for every electrode and window;
  - trials not in the epoch are dropped;
  - it starts from the epoch's `ExcludedHint` when a new epoch loads;
  - unsaved flags are lost when another epoch loads or the app restarts;
  - **only people save**: ask the user to click "Save Flags to Epoch" above the Click Details table.

#### `R/loader.R`
- The `load_data` summary adds `; trials marked excluded in the epoch (ExcludedHint): 3,17` (or `none`), via `epoch_flagged_trials(repo$epoch)`.
- The description explains that these become the flagged trials unless this session already flagged trials of the same epoch.

#### `agents.yaml` (system prompt)
- **Rules:** agents may flag or unflag trials with `flagged_trials`. They NEVER save the flags into the epoch; they ask the user to click "Save Flags to Epoch" (By Condition card → By Trial tab, above Click Details).
- "For people only" becomes: plot clicks (labels, thresholds), the click table's links, saving flags to the epoch, …
- `shiny_ui_operate` stays out.

#### Manual `agents/skills/rave-module/references/power_explorer.md`
- **By Trial (L156–161):** describe the field, the save link, and where the flags start from.
- **New "Procedure — flag outlier trials"**: read per-trial values, set `flagged_trials`, run `run_analysis`, then ask the user to save. For per-trial values, first check `shiny_output_result` on `by_condition_tabset_clipboard` (TSV: Trial, Electrode, y, is_clean). If that returns no text, document `get_results` on target `omnibus_results`.
- **Caveats (L286–290, L301–303):** flags start from the epoch, the reload rule, and the people-only save.
- **Pipeline (L373–374):** `trial_outliers_list` comes from the field.
- **MCP "Configure and run":** add the flag step and the save rule.

#### `test-mcp.R`: new section "flagged trials (writes, then deletes, a test epoch!)"
- **Switch:** `do_flags <- TRUE`. The section goes before the `do_write` stop, so `do_write = FALSE, do_flags = TRUE` tests flags without exports, group saves or reports.
- **Header comment:** add the test epoch to the list of what the test writes. Change the `do_write` stop message to "Nothing else was written".
- **Setup (at the top, before loading):** write `meta/epoch_pe_flag_test.csv`, a copy of `auditory_onset` with `ExcludedHint` on trials 3 and 17. It's written at the top so the loader's epoch choices already list it.
- **Steps:**
  1. Load `pe_flag_test`. The summary lists 3,17, and `flagged_trials` is `"3,17"`.
  2. `run_analysis`. The summary lists 3,17. `omnibus_results$data` has no trial 3 or 17. In `data_with_outliers` they have `is_clean == FALSE`. settings.yaml has `trial_outliers_list` = 3,17.
  3. As an agent, set `"3,17,40-41"` and run again: the summary updates.
  4. `"abc"` is put back to the current list. `"3,9999"` becomes `"3"`. `""` clears the list.
  5. No script saves flags. The field description contains "Save Flags to Epoch". The test epoch file's md5 doesn't change through all agent steps.
  6. Call the shared-env `save_trial_flags_to_epoch(pipeline$read("repository"), c(5, 9))`. The file marks only 5 and 9, a backup exists, and `_OutlierRemoved` has n−2 rows with `OriginalTrial`. Saving `integer(0)` clears the marks and removes `_OutlierRemoved`.
  7. Rewrite the file without one trial: the save stops with "changed since the data were loaded".
  8. Rewrite the file directly (`write.csv`) with all trials and marks 5 and 9. The helper can't be used here: step 7 changed the trials on purpose. Load `auditory_onset` (the field shows `""`), then `pe_flag_test` (`"5,9"`): a new epoch starts from its marks.
  9. Load `auditory_onset` again for the later sections, delete `meta/epoch_pe_flag_test*`, and check that the `auditory_onset` epoch file and electrodes.csv are unchanged (md5).

### Verification

1. **Install your uncommitted ravecore**: `R CMD INSTALL "../ravecore"`. It still reports 0.1.1.17. Your own app on port 17283 keeps the old ravecore until you restart it.
2. **Before any live run:** copy `modules/power_explorer/settings.yaml` (it has uncommitted edits) to the scratchpad, and restore it from that copy afterwards.
3. **Start a test app** on port 17299: `ravedash::debug_modules(".", port = 17299, ...)`, with the cached headless Chromium per the live-testing notes. Then run `RAVE_TEST_PORT=17299 Rscript modules/power_explorer/test-mcp.R` with `do_write <- FALSE`, `do_flags <- TRUE`.
4. **Check early:**
   - the agent update round-trips through `updateOn = "blur"`. If it doesn't, drop `updateOn` and debounce the observer by 500 ms;
   - how `parse_svec` handles mixed text such as `"3,abc"`.
5. **People's path in the headless browser**, using a scratchpad copy of `live-browser.js` that reads a `click` file:
   1. Load the test epoch and run quick mode via MCP. Show the By Trial tab.
   2. Check that the field and the Odd = 1 rows show the marks.
   3. Set `"3,17,40"`, click `#power_explorer-save_flags_to_epoch`, and check the epoch file and the toast text.
   4. Set `""`: the empty table shows the caption. Click save again: the marks are cleared and `_OutlierRemoved` becomes a backup.
   5. Take screenshots of the new field and links for you to review.
   6. Delete `meta/epoch_pe_flag_test*`.
6. **Finish:**
   - stop the test app and browser;
   - restore settings.yaml from the copy;
   - `git status` shows only the intended files changed: `R/shared-functions.R`, `R/module_server.R`, `R/module_html.R`, `R/loader.R`, `agents.yaml`, the manual, `test-mcp.R`, `plan-mcp_support.md`.

### Implementation notes (2026-10-02)

Where the work differed from the plan:
- **ravecore:** the ravecore in your R library (built 2026-10-02 04:27 UTC) already has `ExcludedHint` (an earlier version of the uncommitted source). Its source was being edited during the work, and another Claude session had an app running on port 17299 with it loaded. So the uncommitted source was installed into a private scratchpad library instead, the test app ran on port 17311, and your library was not touched. The public API used (`$excluded_trials`, `$exclude_trials(..., add = FALSE)`, `$save()`) was the same in both versions.
- **Old ravecore:** tested against ravecore HEAD 9dadfec (before `ExcludedHint`), built from `git archive`. No flags are read, and saving stops with "needs a newer ravecore".
- **Field format:** the field shows consecutive trials as a range (`dipsaus::deparse_svec`), e.g. `42,6,5` becomes `5-6,42`.
- **Text checks:** text that is not trial numbers (any character besides digits, `,` `;` `-` `:` and spaces, or `NA` from `parse_svec`) is put back with a warning: `parse_svec` silently drops words (`3,abc` gives `3`).
- **Test trials:** the test marks trials 5 (`known_a`) and 15 (`last_av`), not 3 and 17: those are visual-only and in no trial group of the test. Before loading, the test chooses another subject and then DemoSubject again, so the open loader lists the test epoch (the loader lists a subject's epochs only when the subject changes).
- **Dropped check:** the planned check that the field description contains "Save Flags to Epoch" was dropped, because it tests text, not behavior. The existing exact script-list check already fails if a save script appears.
- **Agents' per-trial values:** agents can read them from `shiny_output_result(outputId = "by_condition_tabset_clipboard", max_chars = 100000)`. The output is the "copy data" button with the tab-separated table in `data-clipboard-text`, about 64 kB for 3 electrodes × 127 trials; the default `max_chars` cuts it at 10,000.
- **Verified:**
  - offline helper tests on a throwaway subject, with the new and the old ravecore;
  - the new `test-mcp.R` steps;
  - the people path with real clicks: one click makes one save and one backup; the second click saves the cleared list and backs up and removes `_OutlierRemoved`;
  - screenshots of the field, the links, and the empty table;
  - the full `test-mcp.R` with `do_write = FALSE` (see the final report).
