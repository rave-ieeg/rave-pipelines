# TODO

Last updated: 2026-10-09.

## Open from the shidashi PR #5 adaptation

All code steps are done (`register_output` and the standalone viewer moved to
shidashi, SVG capture, `stream_viz`, `npm run build`). Still to check by hand:

- [ ] Launch the app and test the output overlay icons
- [ ] Test the standalone viewer popout
- [ ] Test MCP `shiny_query_ui` SVG capture
- [ ] Test the `stream_viz` widget

## Found in the 2026-10-09 regression test

Details are in `BUGS.md` (entries and "Test Results").

- [ ] Decide whether the interim `initialize_partition()` calls stay (ravecore
  `power_baseline`, voltage_explorer `filter_repository()`). They cost a few
  milliseconds, and users who install from CRAN still get filearray 0.2.3,
  which needs them.
- [ ] ravepipeline: report generation prints every environment variable, API
  keys included, to the app's console (`R/pipeline-report.R:253-254`;
  `BUGS.md`).
- [ ] threeBrain: the 3D viewer can keep stale electrode colors after a
  cluster update (power_clust, voltage_clust; `BUGS.md`).
- [ ] `rave_3dviewer_get` read back old controller values 2-3 s after
  `rave_3dviewer_set` while three test apps were running (e.g. Voxel Display
  still "normal" after setting "hidden"); the panel showed the new value at
  once. The tool note says about 0.5 s, so an agent that reads back at once
  may think its set failed.
  - Related, minor: threeBrain reports "Voxel Opacity" as -1 below 0.001 (on
    purpose, `PresetRaymarchingVoxels.js`), while its spec says 0 to 1.

## MCP support for modules

The agent manuals are in `agents/skills/rave-module/references/<id>.md`.
power_explorer, custom_3d_viewer, and compatibility_rave1 also keep their
plans in `modules/<id>/plan-mcp_support.md`.

| Module | Status |
|---|---|
| notch_filter, reference_module, wavelet_module, custom_3d_viewer, compatibility_rave1, power_explorer, power_clust, voltage_clust, voltage_explorer, project_overview, epoch_generator, yael_preprocess | Done and committed |
| electrode_localization | Phases 0-1 done and committed (2026-10-08); phases 2-4 open |
| configure_rave, connectivity_viewer, generate_surface_atlas, group_3d_viewer, import_bids, import_lfp_native, import_signals, jupyterlab, standalone_report, standalone_viewer, stimpulse_finder, surface_reconstruction, trace_viewer | No MCP support yet (no `agents.yaml`) |

### Shared test helpers (2026-10-02)

Every `test-mcp.R` loads `agents/skills/build-module-mcp/test-common.R` and
passes R values as tool arguments. All tests but two were re-run live after
that change.

- [ ] Re-run `notch_filter` and `reference_module`. They were not re-run:
  notch_filter's script ends by applying the filter, and reference_module
  saves references into its test subject.

### Tooltips for people (2026-10-02)

shidashi shows a registered input's `tooltip` when people hover over it. By
default the tooltip is the first sentence of the agent `description`.

- [ ] ravedash presets still show agent wording to people. Examples: the
  "Create" button and name field of the "Create new subject" and "Create new
  project" dialogs (`presets_loader_subject`, `presets_import_setup_native`)
  say "...; only the user may create subjects."

### power_explorer (Power Explorer)

Found in the 2026-10-09 test; not changed.

- [ ] "Waterfall over Time" repeats electrode numbers on its y-axis (16, 15,
  15, 14, 14 for electrodes 14-16), so the half-way gridlines carry an
  electrode's number. `yat <- pretty(y.seq)` gives fractional ticks for 2-4
  electrodes, and `labels = y.els[yat]` truncates them
  (`R/shared-graphing.R:1134-1141`). Fine with 5 (`pretty(1:5)` is `1:5`).
  Old code.
- [ ] Once, right after the quick custom-ROI run with "Group/Stratify
  results" (it switches `btp_panelvar` to `ROI_label`,
  `R/module_server.R:507`), the By Condition > By Trial plot logged "Error in
  if: argument is of length zero" (`plot_by_condition_by_trial`,
  `R/shared-graphing.R:2681`). People may briefly see an error there. It
  rendered fine afterwards; not reproduced, and which `if` failed is
  unknown.

### power_clust (Power Clustering)

- [ ] "or Mask file" (`loader_mask_file`) is not read anywhere.
- [ ] The visible tab title typo is fixed (`759e5a3`), but the
  `cluster_tabset` description still names the tab `'Diagnosic plots'`
  (`R/module_html.R:177`); the tab is "Diagnostic plots". An agent that
  sends the description's name leaves the card with no tab showing (shidashi
  deactivates every tab whose title does not match), and people see a
  "Cannot activate tab" toast.
- [ ] `test-mcp.R` never runs its "no results right after loading" check. It
  treats the page as reused when `n_clusters` exists (`test-mcp.R:204`), but
  `n_clusters` is static UI in the output card's footer
  (`R/module_html.R:228`). So the test prints "Reused page: skipping ..."
  even on a fresh page (seen 2026-10-09). A change that keeps old results
  after loading would go unnoticed.
- [ ] The run's error handler calls `logger(traceback(e))`
  (`R/module_server.R:206`). The same call failed in voltage_clust (see
  there); not seen failing here.

### voltage_clust (ERP Clustering)

- [ ] The settings download is named `pipeline-power_clust-settings.yaml`.
- [ ] `main.Rmd` passes `frequency_range`, which its settings do not have,
  into `...` (unused).
- [ ] "or Mask file" is unused; the `cluster_tabset` description has the
  same `'Diagnosic plots'` leftover (`R/module_html.R:154`).
- [ ] `test-mcp.R` never runs its "no results right after loading" check, as
  in power_clust (`test-mcp.R:200`; `n_clusters` is at
  `R/module_html.R:205`).
- [ ] The run's error handler could not log the traceback:
  `ravepipeline::logger(traceback(e), ...)` (`R/module_server.R:196`) logged
  "Cannot log the following call ... subscript out of bounds". This was
  probably during the test's deliberate failing run (2026-10-09). People
  still get the error toast; only the traceback for debugging is lost.

### project_overview (Project Overview)

- [ ] Found while checking the export, not changed:
  - ravepipeline renders with its own format object, so the YAML `output:`
    options of `report-export.Rmd` are ignored: `code_folding: none` became
    "hide".
  - Reports render with `clean = FALSE`, so each report folder keeps
    `intermediate/` with the `knit.md`. It is 50 KB in the export zip and
    18.8 MB in DemoSubject's voltage_explorer report.
  - The zip is 205 MB. Most of it is what the report links to: copies of 27
    module reports (309 MB unpacked) and the group brain viewer (80 MB).
  - The zip also carries module files such as `TODO.md`, `test-mcp.R`, and an
    old `report-export.html`. The `report` fork policy copies everything
    except `build`, `shared/user`, and `main.html`.

### epoch_generator (Generate Epoch)

- [ ] Once, the test page's Shiny session closed while idle after the person
  checks (no R or JS error logged). The same click sequence did not
  reproduce it.
- [ ] The "Epoch threshold" toast (it does not hide) covers the card header's
  download icon; people close it first. Not changed.
- [ ] The double-click fill keeps 4 significant digits (approved). For a
  small pulse on a large offset (e.g. counts near 8,388,000) that rounds the
  threshold onto the baseline; typing the value works. Not changed.
- [ ] Not changed, documented in the manual: the x-axis is in samples for 20000
  samples or fewer (the brush then fails), the MAT rate stays from the
  previous file, a block change re-selects a guessed file, a minimum duration
  of 0 lets staged onsets be staged twice, "Load existing epoch" drops
  conditions, "Save & Download" gives no feedback and does not refresh the
  list, transforms do not re-run the thresholding, and NSx/BrainVision files
  appear only as imported channels.
- [ ] The "Threshold" field uses `numericInput(updateOn = "blur")`, which
  shiny has only since 1.11.0 (the installed 1.14.0 has it). Nothing asks for
  that shiny: shidashi and ravedash require shiny >= 1.7, and
  `ravemanager::update_rave()` installs with `upgrade = FALSE`, so it keeps
  an older shiny. With an older shiny the module's page fails to build
  ("unused argument (updateOn = ...)").
- [ ] Suggestion: a line in build-module-mcp `SKILL.md` that MCP agents never
  see `system_prompt` (rules belong in descriptions and the manual's top).

### electrode_localization (Electrode Localization)

Plan: `~/.claude/plans/i-want-to-migrate-iridescent-lemon.md`.

- [ ] YAEL-reg3d misregisters yael_demo_001's CT_RAW + MRI_reference
  (skull-stripped, 9% non-zero): FastRigid (mattes or cc) and Rigid put 0-1
  of the 48 saved contacts on metal; the subject's ANTs transform puts 48/48.
  Rigid took ~1900 s offline (yael_preprocess notes say ~40 s on 2026-10-04).
  Not checked on a raw T1 with skull (no such image allowed).
- [ ] `save_electrode_table()` still writes `custom-data` under
  `raw_path/rave-imaging` (BIDS); not changed (only the loader was asked).
- [ ] Phases 2-4 (read-only voxel/atlas inspection, undo, refine): design
  round with the user.
- [ ] The default 3D view hides the CT behind the opaque sagittal slice
  (`BUGS.md`, "Electrode Localization hides the CT in its default 3D view").

### yael_preprocess (MRI/CT Preprocessing)

- [ ] "Start re-alignment" stays disabled after a failed start (pre-existing;
  people reload the page). Not changed.
- [ ] Both imports share one log file name when built in the same second; the
  CT import overwrites the T1 import's log. Not changed.
- [ ] Old `notes/surface_reconstruction.*` files stay on subjects; harmless.

## import_signals: NEV/NSx freeze (2026-10-15)

Plan: `~/.claude/plans/the-import-module-is-woolly-brook.md`.

- [ ] Release readNSx 0.1.1: the module's NEV preview calls
  `readNSx::read_nev_header()` (new). The release also reads NEV packets in
  chunks (excluded events cost nothing) and stores full spike waveforms.
- [ ] Release ravecore with `nev_cache_key()` (validation and import hash each
  NEV file once per session instead of up to six times).
