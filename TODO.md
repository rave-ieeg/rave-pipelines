# TODO

Last updated: 2026-10-02.

## Open from the shidashi PR #5 adaptation

All code steps are done (`register_output` and the standalone viewer moved to
shidashi, SVG capture, `stream_viz`, `npm run build`). Still to check by hand:

- [ ] Launch the app and test the output overlay icons
- [ ] Test the standalone viewer popout
- [ ] Test MCP `shiny_query_ui` SVG capture
- [ ] Test the `stream_viz` widget

## MCP support for modules

Each module's design and decisions are in `modules/<id>/plan-mcp_support.md`;
the agent manuals are in `agents/skills/rave-module/references/<id>.md`.

| Module | Status |
|---|---|
| notch_filter, reference_module, wavelet_module, custom_3d_viewer, compatibility_rave1, power_explorer | Done and committed (see their plans) |
| power_clust, voltage_clust, voltage_explorer, project_overview | Done and tested live on 2026-10-02; **not committed** |
| configure_rave, connectivity_viewer, electrode_localization, epoch_generator, generate_surface_atlas, group_3d_viewer, import_bids, import_lfp_native, import_signals, jupyterlab, standalone_report, standalone_viewer, stimpulse_finder, surface_reconstruction, trace_viewer, yael_preprocess | No MCP support yet (no `agents.yaml`) |

### Shared test helpers (2026-10-02)

Done:
- `agents/skills/build-module-mcp/test-common.R` holds the helpers every
  `test-mcp.R` used to copy (`mcp`, `tool`, `set_input`, `set_input_wait`,
  `run_script`, `operate`, `page_text`, `output_text`, `viewer_get`, ...).
  A test sets `module <- "<id>"` and loads them with one `source()` line.
- Tool arguments are R values now, not JSON text: `httr2` sends them as JSON
  and the app decodes them into the same R values (`I("a")` for a
  one-element array, `list()` for an empty one). All ten tests were changed.
- The skill's test step (`agents/skills/build-module-mcp/SKILL.md`, step 7)
  points to the new file.
- Re-run live after the change (port 17299):
  - power_clust, voltage_clust, project_overview, and custom_3d_viewer, end
    to end;
  - with their write switches off: voltage_explorer (`do_write`),
    compatibility_rave1 (`do_write`), wavelet_module (`do_apply`), and
    power_explorer (`do_write`, `do_flags`).

Open:
- [ ] Re-run `notch_filter` and `reference_module`. They were not re-run:
  notch_filter's script ends by applying the filter, and reference_module
  saves references into its test subject.

### Tooltips for people (2026-10-02)

shidashi 0.2.0.12 shows a registered input's `tooltip` when people hover over
it. By default the tooltip is the first sentence of the agent `description`.

Done:
- 114 inputs in the ten MCP modules have their own `tooltip`, written for
  people, where the description speaks to agents (script names, JSON
  formats, rules).
- Checked by rendering each module's loader and main UI offline and reading
  every tooltip; the 92 tooltips left at the default read fine for people.
- Compound inputs, card tab sets, the object list, and file uploads get no
  tooltip (shidashi adds none there).

Open:
- [ ] No module requires shidashi >= 0.2.0.12. With 0.2.0.11 or older,
  `register_input()` has no `tooltip` argument, so these modules stop with
  "unused argument (tooltip = ...)".
- [ ] ravedash presets still show agent wording to people. Examples: the
  "Create" button and name field of the "Create new subject" and "Create new
  project" dialogs (`presets_loader_subject`, `presets_import_setup_native`)
  say "...; only the user may create subjects."

### power_clust (Power Clustering)

Done:
- Registered `time_range`, `frequency_range`, `zeta_threshold`, `n_clusters`,
  `condition_groups`, and "Load Settings" (read-only).
- The output tab set got a stable `inputId` (`cluster_tabset`, approved),
  registered with `card_tabset_activate`.
- Scripts:
  - `load_data` and `run_analysis` got descriptions and summaries;
  - new read-only `cluster_summary` and `pipeline_progress`.
- `agents.yaml` (with `shiny_ui_operate` and the 3D viewer tools for
  `viewer`), the manual, and `test-mcp.R`.

Deviations from the plan, and solutions:
- After a new load the module keeps its old results in `local_data`, while
  the plots ask for a run. `cluster_summary` follows the plots: no results
  while `update_outputs` is empty or FALSE.
- Reloading identical data skips re-initialization, so the test checks "no
  results after loading" only on a freshly opened page.
- Errors reach agents at the end of `output`, after the logged code of the
  run: `Error in ...: <message>` for invalid inputs, `Possible issue:
  <message>` for a failing pipeline step. The manual and test say so.

Open:
- [ ] "or Mask file" (`loader_mask_file`) is not read anywhere.
- [ ] Tab title typo "Diagnosic plots" (visible; not changed).

### voltage_clust (ERP Clustering)

Done: the same as power_clust, without a frequency band. The baseline offers
only "Demean" and "Per trial and electrode" and uses only the windows.

Deviations from the plan, and solutions:
- **"Load subject" failed for everyone** (`unused argument (progress_quiet =
  TRUE)`). The argument dates from the old promise-based call; `set_script`
  runs the call synchronously, which forwards it to the targets run. You
  removed it.
- A failed load rewrote `_targets.yaml` (reporter lines); restored from HEAD.

Open:
- [ ] The test left the pipeline store and the 1.7 GB baseline cache
  (`data/cache`) in its demo/DemoSubject state, as a consistent pair. The
  next ds005953_01 load rebuilds both, which takes a while.
- [ ] The settings download is named `pipeline-power_clust-settings.yaml`.
- [ ] `main.Rmd` passes `frequency_range`, which its settings do not have,
  into `...` (unused).
- [ ] "or Mask file" is unused; "Diagnosic plots" typo.

### voltage_explorer (Voltage Explorer)

Done:
- Fixed the detrend select, which was registered as `detrend_method` instead
  of `remove_drift_method`.
- Registered the 13 plot options, `by_cond_channel_selector`, and six buttons
  and links (read-only, each naming its script).
- Moved six observer bodies verbatim into scripts: `inspect_filters`,
  `reset_signal_config`, `reset_crp_params`, `reset_plot_options`,
  `send_to_electrode_selector`, and `open_report_dialog`.
- The report: agents click the dialog's `do_generate_report` with
  `shiny_ui_operate` (its code untouched), after confirming with the user,
  then poll the new read-only `report_status`.
- New read-only `pipeline_progress`; a `run_analysis` result.
- `agents.yaml`, the manual, and `test-mcp.R`. One report was written to
  `demo/DemoSubject/reports/report-univariateVoltage_datetime-261001T233513_voltage_explorer/`.

Deviations from the plan, and solutions:
- **Preferences** live in RAVE's global store
  (`ravepipeline:::global_preferences("default")`, keys
  `voltage_explorer.*`), not in the module folder. Tests snapshot and restore
  those keys.
- After a run the module collapses "Signal Configurations". Agents set its
  inputs either way; people expand it (in the manual).
- To clear a numeric input, send `""`: JSON `null` is ignored.
- The test checks that the pipeline ran through target `data_placeholder`
  (cue "always"): the metrics are rebuilt only when their inputs change.

Open:
- [ ] A band-pass with one cutoff shows people a red "Coding Error"
  notification with the reason. This is ravedash's default for the Run
  Analysis observer; decide whether to keep it.
- [ ] `with_selector_filter_column()` prints the whole table on every viewer
  update (`print(erp_tbl)`).
- [ ] `DESCRIPTION` still has a placeholder Title and Description.
- [ ] `R/shared-filters.R` has another session's uncommitted filearray
  partition fix (see `BUGS.md`); it was not touched.

### project_overview (Project Overview)

Done:
- Registered `loader_project_name`, the output tab set `output_cardset`, and
  "Generate Report" (read-only).
- Scripts:
  - new `generate_report` (the observer body verbatim, plus a summary);
  - Export Report (`run_analysis`) got a description and the job-id line;
  - new read-only `export_status` and `pipeline_progress`.
- Clearer descriptions for the inputs.
- `agents.yaml`, the manual, and `test-mcp.R`. Nothing is written into the
  project.

Deviations from the plan, and solutions:
- **Tables in a hidden tab come back empty** from `shiny_output_result`.
  shidashi renders the output on the server, but DT draws nothing at zero
  size (`lazyRender`). Agents show the tab with `output_cardset` first. An
  `outputOptions(suspendWhenHidden = FALSE)` attempt was reverted.
- **"Export Report" hung forever, for people too.** `utils::zip()` in the
  background job printed a line per file into the job's stdout, a
  `callr::r_bg()` pipe that nothing drains.
  - Fixed in ravepipeline (commit `1cc1cec`, installed as 0.2.0.9):
    `start_job_callr` sends job stdout and stderr to `process_outputs.txt` in
    the job folder. A new test, `tests/testthat/test-jobs-child-output.R`,
    hangs without the fix and passes with it.
  - The module also zips quietly (`flags = "-r9Xq"`) while it serves an MCP
    call. shidashi 0.2.0.12 sets `SHIDASHI_USING_MCP` to "TRUE" during each
    MCP tool call, and the export job inherits it. The plan named the
    variable `SHIDASHI_MCP_ACTIVE`; shidashi shipped the other name.
- `export_status` compares normalized paths: the job reports
  `/private/var/...`, while `tempfile()` gives `/var/...`.

Re-checked on 2026-10-02, after both fixes were installed:
- `test-mcp.R` passed on a test app started without the variable. The
  export job's process had `SHIDASHI_USING_MCP=TRUE`, and its output had no
  zip "adding:" lines.

Open:
- [x] `settings.yaml` holds `subject_codes: DemoSubject, YAB` from a test
  run; restore it if that was not intended. (I will sanitize settings.yaml. do NOT report settings.yaml issues unless the input specs are changed, not the value. Otherwise the text is annoying)
- [x] The module's own `TODO.md` describes an older design, and
  `SKILL_draft.md` is a guide to writing `main.Rmd`. (leave it. The file is there no harm)
- [ ] Packaging warns "Could not fetch resource report_styles.css". The
  copied power_explorer reports under `build/module_reports/` link a
  stylesheet that is not next to them (This is a bug).
