# Known bugs

## The 3D viewer can keep stale electrode colors after a cluster update

- **Found:** 2026-10-09, in `power_clust` and `voltage_clust`, during the live test in [Test Results](#test-results).
- **Versions:** threeBrain 1.3.0.66 (three.js r185, node materials), shidashi 0.2.0.17, ravedash 0.1.3.56; headless Chromium 1228 with the GPU through ANGLE Metal; macOS.
- **Status:** open. Seen twice; not reproduced on demand.

### Symptom

After a run, or a change of "# of clusters", some electrodes keep an old color in the 3D viewer. The legend, the table and the plots show the new clusters. Subject demo/DemoSubject, electrodes 13-16,24:
- **power_clust, k = 2:** all five electrodes are drawn orange (class 1), but 14 should be blue (class 2). After setting k = 3, 15 turned green as it should, and 14 stayed orange.
- **voltage_clust, k = 2:** four electrodes are orange and one is blue. It should be {13, 15} orange and {14, 16, 24} blue: 14 and 24 kept orange. Seen in 1 of 4 attempts.

### What is known

- R sends the right data: `brain_proxy$set_electrode_data(..., clear_first = TRUE, update_display = TRUE)` carries the correct clusters (power_clust `R/module_server.R:414`, voltage_clust `R/module_server.R:390`).
- The viewer's state is right too: for the stale electrodes, `state.displayValues` holds the new class, and the material color is the new one (#1874cd).
- Only the drawn color is stale:
  - `canvas.needsUpdate` and `material.needsUpdate` do not redraw it, and setting the same color again does not either;
  - setting another color and then the right one does;
  - re-selecting Display Data ("Loaded", then "Cluster") also fixes it.
- In voltage_clust, the stale electrodes were the ones whose class did not change between the last two cluster updates. The run updates the outputs while "# of clusters" still holds the previous k, then sets it to the suggested k.
- A guess that fits these observations: at some earlier update, the drawn (GPU) color fell behind the material's color, and later updates that leave the material's value unchanged are skipped. Not proven.
- The electrodes are sphere meshes with `MeshBasicNodeMaterial`.
  - three-brain-js `a5b5574` ("Fixed r185 electrode color not updated issue by introducing dynamic range") makes instance colors upload on every render (`DynamicDrawUsage`).
  - That covers contacts drawn as instanced meshes, not these per-mesh materials.

### Consequences

- People can read the wrong cluster membership off the brain until the display is refreshed. Results, tables and plots are not affected.
- The module tests cannot catch it: they only check the name of the Display Data controller.

### Not checked

- Other browsers (Safari, a visible Chrome window).
- Whether older threeBrain builds behave the same. The newest three-brain-js commit (`3e1f71d`, 2026-10-09) changes only NIfTI loading.

## Report generation prints every environment variable to the log

- **Found:** 2026-10-09, in the console output of two test apps, after they generated the `power_explorer` and `voltage_explorer` reports.
- **Versions:** ravepipeline 0.2.0.10. The code has been in ravepipeline since commit `412dc0a5` (2025-12-10).
- **Status:** fixed in ravepipeline 0.2.0.12 (commit `cbd548e`, 2026-10-09; not pushed as of 2026-10-09). 0.2.0.12 is installed in the main library.

### Fix

- `R/pipeline-report.R:254-264` now prints nothing when the report runs during an MCP call (`SHIDASHI_USING_MCP` is "true"; it is passed to the job at `:237-240`).
- Otherwise it prints only variables whose names start with `RAVE`, `SHIDASHI`, `DIPSAUS`, `RPYMAT`, `RETICULATE`, `RSTUDIO_PANDOC` or `TAR_` (any case).
- None of ravepipeline, ravecore, shidashi, dipsaus, rpymat or ravedash uses a variable under those prefixes whose name contains KEY, TOKEN, SECRET, PASS, AUTH or CRED.
- Left over, minor: a secret that a person stores under one of those prefixes themselves (for example `RAVE_..._TOKEN`) is still printed.

### Symptom and cause (before the fix)

- `pipeline_report_generate()` (ravepipeline `R/pipeline-report.R:253-254`) runs `cat("==== System environment ====")` and then `print(Sys.getenv())` inside the report job.
- The app relays the job's output to its console as `TRACE` lines.
- So the whole environment of the R process ends up there in plain text, including API keys and tokens (`*_API_KEY`, `*_TOKEN`).

### Consequences (before the fix)

- Every report, in any RAVE session, writes all environment variables wherever the session's output goes: a terminal, a server log, a log collector.
- On a RAVE server, anyone who can read its logs can read the credentials in the server's environment.
- Not checked: whether job output is also kept in files, such as job logs or the report folder.

### Fix suggested in the original report

Remove the two lines, which look like leftover debugging, or print only RAVE's own variables (`RAVE_*`).

# Test Results

Background for the entries above.

## 2026-10-09: filearray 0.2.3.1 and the analysis modules

**Setup:**
- **Versions:** filearray 0.2.3.1, ravecore 0.1.1.22, ravepipeline 0.2.0.10, ravedash 0.1.3.56, shidashi 0.2.0.17, threeBrain 1.3.0.66, ravetools 0.3.2.3, R 4.6.1, macOS with 10 cores (`max_worker` 10).
- **Live tests:**
  - Three test apps ran at the same time on separate ports, from an isolated copy of this repository (`git archive HEAD`).
  - Each module's `test-mcp.R` drove its module, with an attached headless Chromium (GPU through ANGLE Metal).
  - Screenshots of every output and 3D viewer were checked by eye.
- **Subjects:** demo/DemoSubject and YAEL/yael_demo_001; YAEL/CIT168 (a template) only for the Subject 3D Viewer's streamline analysis.

**filearray:** 0.2.3.1 fixes the partition race that made parallel jobs fail with "File size too small".
- Against the pre-fix 0.2.3, it went from 10 of 10 failed runs to 0 of 30, with a delay added in `write_header()`.
- filearray's own tests pass (979 passed, 3 skipped on CRAN).

### Analysis modules and viewers

All six module tests passed on their first run, and no app log has a partition or filearray error.

| Module | Subject | Analysis | Viewers |
|---|---|---|---|
| power_explorer | demo/DemoSubject | passed, including the export, two group-analysis saves and the report | correct, except the waterfall plot's labels (`TODO.md`) |
| power_clust | demo/DemoSubject | passed | [stale cluster colors](#the-3d-viewer-can-keep-stale-electrode-colors-after-a-cluster-update) |
| voltage_explorer | demo/DemoSubject | passed, including the report | correct |
| voltage_clust | demo/DemoSubject | passed | [stale cluster colors](#the-3d-viewer-can-keep-stale-electrode-colors-after-a-cluster-update), once |
| electrode_localization | YAEL/yael_demo_001 | 31 checks passed; agents' clicks on "Save to subject" refused | correct (see [CT behind the sagittal slice](#electrode_localization-ct-behind-the-sagittal-slice)) |
| custom_3d_viewer | demo/DemoSubject, YAEL/CIT168, YAEL/yael_demo_001 | passed, including the streamline analysis | correct |

Smaller findings are in `TODO.md`.

#### electrode_localization: CT behind the sagittal slice

The default view shows the sagittal slice but no CT metal. It was first listed as a bug and is withdrawn: the viewer draws it correctly.
- **Cause:** the camera looks from the right, and the demo's electrodes are all in the left hemisphere.
  - The module passes no camera to `brain$localize()` (`modules/electrode_localization/R/module_server.R:620-627`), so threeBrain's default `camera_pos = c(500, 0, 0)` (+x, right) applies.
  - All 48 yael_demo_001 contacts (AHL, LB, LC, LD) have tkrRAS x between -75.9 and -6.2.
  - The opaque sagittal slice at x = 0, which the module turns on (`:593-595`), therefore lies between the camera and all of the CT.
- **Checked live** (2026-10-09, same setup; camera set with `rave_3dviewer_set`):

  | Camera | Sagittal overlay | CT metal |
  |---|---|---|
  | right (default) | on | hidden |
  | left (`[-500, 0, 0]`) | on | shown, in front of the slice |
  | left | off | shown |
  | right | off | shown |
  | right | on again | hidden |

- **Effect:** in subjects whose electrodes are all on the left, people see no CT until they rotate the view or turn off the sagittal overlay. Results are not affected. Changing the default camera would be a visible UI change and is not planned.

Harmless, noted only:
- Console errors from threeBrain in headless Chromium, at page load ("getController") and while a viewer is rebuilt ("controllersRecursive", "dispatcherEnabled", ...). This is the known class of viewer-not-ready errors.
- Warnings "no non-missing arguments to min" from the targets `localize_data` (electrode_localization) and `loaded_brain_info` (custom_3d_viewer).
- electrode_localization's `load_data` rewrites `notes/electrode_localization.{json,yaml}` with the same content.

### Not covered

- Windows and network file systems, where rename and open behave differently.
- The preprocessing modules that write subject data through filearray (wavelet, notch filter, reference): they are not run on demo data.
