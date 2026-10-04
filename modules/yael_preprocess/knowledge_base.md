# yael_preprocess: technical knowledge base

Developer notes for the MCP migration of this module (title "MRI/CT
Preprocessing"). The goal-focused manual for people and agents is
`agents/skills/rave-module/references/yael_preprocess.md`.

Written 2026-10-04 from the code, with ravecore 0.1.1.18 (0.1.1.19 with the
import-history fix), ravedash 0.1.3.55, shidashi 0.2.0.14, threeBrain
1.3.0.65, ravepipeline 0.2.0.10. Line numbers refer to the tree before the
MCP migration, with the uncommitted batch edit (loader ->
`ravedash::load_data_button()` + sync `load_data` script; footer ->
`run_analysis` script); the migration moved the button observers into
scripts, so search by input or script name.

**Notation.** `<imaging_path>` = `subject$imaging_path`; `<raw_path>` =
`subject$preprocess_settings$raw_path`. The module's own code builds the
imaging folder as `file.path(<raw_path>, "rave-imaging")`, which is the same
folder (ravecore `R/class003-subject-internal.R:127`). New code uses
`subject$imaging_path`.

**Privacy.** Only `YAEL/yael_demo_001` may be opened for this module's work
and tests. Do not open the rendered `*.html` reports in this folder or old
objects in `shared/objects`: they may hold other subjects' data.

## Contents

* [Files](#files)
* [User decisions for agents](#user-decisions-for-agents)
* [Shared mechanics](#shared-mechanics)
* [1. Loading data](#1-loading-data)
* [2. Import](#2-import)
* [3. ACPC re-alignment](#3-acpc-re-alignment)
* [4. MRI preprocessing](#4-mri-preprocessing)
* [5. CT-MRI co-registration](#5-ct-mri-co-registration)
* [Reports](#reports)
* [External programs](#external-programs)
* [Downstream: electrode_localization](#downstream-electrode_localization)
* [Bugs and dead code](#bugs-and-dead-code)
* [Demo subject](#demo-subject)
* [MCP migration](#mcp-migration)

## Files

| File | Role |
|---|---|
| `R/aa.R` | module ID, `pipeline`, `check_data_loaded()` (needs all `check_result` fields) |
| `R/aaa-presets.R` | loader presets: project, subject (`allow_new = TRUE`), two sync buttons |
| `R/shared-functions.R` | `%OF%`, `FSL_COST_FUNCTIONS`, `path_is_valid()` (also loaded by the pipeline) |
| `R/loader.R` | loader UI and server: image pickers, upload dialog, `load_data` script, "Proceed" |
| `R/module_html.R` | main UI: left panel + four collapsed cards |
| `R/module_server.R` | main server: card logic, `run_command_pipeline()`, previews, ACPC |
| `main.Rmd` / `make-yael_preprocess.R` | pipeline: tool check, subject check, ACPC viewer, command generators |
| `report-coregistration.Rmd`, `report-normalization.Rmd`, `report-list.yaml` | quality reports |
| `module-ui.html` | page template; footer button label "Generate Reports" |
| `agents.yaml` | MCP tools, skills, and the in-dashboard chat's system prompt |
| `test-mcp.R` | live MCP test on `YAEL/yael_demo_001` (backs up and restores it) |

## User decisions for agents

Decided 2026-10-04. They go into descriptions and the manual only: no guard
code, no behaviour change for people.

* **Run from RAVE, after confirming each time:** both imports (~10 s); ANTs
  co-registration (1-2 min, by far the best algorithm, always recommended);
  NiftyReg (5-10 min). Agents never click MRI preprocessing's "Run from RAVE"
  and never run img_pipe, FSL, or AFNI from RAVE.
* **Everything else:** "Save & run by yourself". An agent with a terminal on
  the machine where RAVE runs may run the one-liner in a separate terminal,
  never inside RAVE (a RAVE job dies with RAVE); otherwise it gives the user
  the command.
* **MRI preprocessing:** recommend YAEL+recon-all; YAEL-only when FreeSurfer
  is not needed. When RAVE is local, check FreeSurfer version vs RAM:
  7.x needs 16 GB, 8.x needs 32 GB; if short, switch versions or YAEL-only.
* **Import = confirming the image** for the whole pipeline. Re-importing a
  different image invalidates all processing of the old one; DICOM import
  deleting `<imaging_path>/inputs/<TYPE>` is by design. Agents import a type
  only when it was never imported; otherwise they ask the user.
* **ACPC:** agents coach; the person places AC/PC; agents save only after the
  person confirms.
* **Same MR image for both steps:** MRI preprocessing input (`param_fs_infile`)
  and co-registration MR image (`param_coreg_mri`) must be identical:
  `MRI_RAW` when ACPC is not part of the pipeline, `MRI_acpc.nii.gz` when it
  is. Never `T1.nii` (even after preprocessing), never an ACPC image with a
  non-ACPC one (ACPC changes the orientation matrix). People may choose
  otherwise.
* **Order:** import, then ACPC when used; then MRI preprocessing and
  co-registration in either order or at the same time.
* **Reports:** agents generate them without asking.
* **Forbidden:** "Remove existing work" (`param_fs_fresh_start`), editing
  command-line paths (`cmd_*_path`), and `"[Upload]"` (person only anyway).
  The two inputs are registered `writable = FALSE`.
* Rules live in descriptions and the manual's first lines: MCP agents never
  see `agents.yaml`'s system prompt (it is for the in-dashboard chat).

## Shared mechanics

### Commands are generated, then run

The pipeline never runs an external program: every command target calls a
ravecore `cmd_run_*()` with `dry_run = TRUE` and returns a list with
`script`, `script_path`, `log_file`, `command`, `execute()` (and
`src_path`/`dest_path` etc.). A target that throws is caught into
`list(error = TRUE, condition = e)` (`main.Rmd:208-649`).

`input_params()` (`R/module_server.R:826-916`, debounced 300 ms) collects the
main inputs. The observer at `:1050-1083` (triggered by
`local_reactives$build_command` and `input_params()`) runs, on **every input
change**:

1. `pipeline$set_settings(params = params)`, so `settings.yaml` follows the
   inputs;
2. `pipeline$eval()` of `settings_path`, `settings`, `params`, `subject`,
   `cmd_tools`, `check_result`, `import_T1`, `import_CT`,
   `image_segmentation`, `morphmri_ants`, `coreg_flirt`, `coreg_niftyreg`,
   `coreg_ants`, `coreg_3dallineate`, `coreg_nipy` into
   `local_reactives$bash_scripts`.

Side effects of that evaluation, each time:
* `cmd_run_dcm2niix()` saves subject default `raw_<type>_path` (absolute
  path) in namespace `surface_reconstruction`
  (`<subject>/notes/surface_reconstruction.json|yaml`; ravecore
  `R/workflow-dcm2niix.R:170`).
* `morphmri_ants` always, and `image_segmentation` for YAEL programs, call
  `cmd_run_yael_preprocess()`, which runs `rpyANTs::ensure_template()` for
  each template (may download).
* `check_commandline_tools` may call `raveio_setopt()` (see loading).

### UI input -> `params` (settings.yaml)

| `params` key | Input | Note |
|---|---|---|
| `nii_t1` | `param_fs_infile` | file name in `<imaging_path>/inputs/MRI` |
| `nii_ct` | `param_coreg_ct` | file name in `<imaging_path>/inputs/CT` |
| `template_brain` | `mri_morph_template_subject` | input commented out: always NULL |
| `dcm2niix$merge/float/crop` | `param_dcm2niix_*` | |
| `segmentation` | `param_fs_prog`, `param_fs_fresh_start` | `program`, `flag`, `fresh_start` |
| `niftyreg$reference/reg_type/interp` | `param_coreg_mri`, `coreg_niftyreg_*` | target reads `params$nipy$reference` |
| `ants$reference/reg_type/aff_metric/syn_metric` | `param_coreg_mri`, `coreg_ants_*` | target reads `params$nipy$reference` |
| `flirt$reference/dof/cost/search/searchcost` | `param_coreg_mri`, `coreg_fsl_*` | `dof` parsed from "6 (rigid body)" |
| `nipy$...` | `param_coreg_mri`, `coreg_nipy_*` | `similarity` <- `coreg_nipy_cost` |
| `afni$reference` | `param_coreg_mri` | unused: AFNI target uses `nii_t1` |

All `reference` keys hold the same value, so the `nipy` cross-read is
harmless.

### Running a command from RAVE

`run_command_pipeline(cmd, wait, title, command, ...)`
(`R/module_server.R:367-518`), called by every "Run from RAVE" button:

1. clears error toasts; shows a modal (`title`; `verbatimTextOutput`
   `verbatim_log`; footer `dismiss_modal` "Running..." disabled);
2. `cmd$execute(dry_run = TRUE, backup = FALSE)` writes the script file;
3. `ravepipeline::start_job(method = "rs_job")` (callr outside RStudio;
   `ravepipeline/R/jobs.R:641`) runs `ravecore::cmd_execute(script,
   script_path, stdout = log, stderr = log, command)`; `command` defaults to
   `cmd$command` (`bash` or Rscript);
4. a promise polls every 1 s with `later`: reads the log into
   `local_data$current_logs`, renders it, checks
   `ravepipeline::resolve_job(job_id, timeout = 0)`;
5. on finish: `dismiss_modal` -> "Done" (error: "Gotcha"); sticky toast
   (class `dismissible`) "The command seems to have finished. However, RAVE
   does NOT know if the command finishes correctly." with download link
   `logfile_download`.

`local_data$current_command` holds `job_id` and `cmd` while running and is
cleared at the end; `local_reactives$logfile_path` keeps the last log path.
Clicking `dismiss_modal` (`:342-350`) clears `local_data$log_file`, the
`dismissible` toasts, and the modal. No exit status reaches R: success is
only visible in the log tail and the output files.

Native co-registration programs (NiftyReg, ANTs, img_pipe) on **Windows** run
synchronously instead: alert, write script, `source()` it with
`check_rpymat <- FALSE` (`:756-778`).

### "Save & run by yourself"

`btn_recon_copy` (`:609-644`) and `btn_coreg_copy` (`:659-713`):
`cmd$execute(dry_run = TRUE, backup = FALSE)` writes the script and returns
the one-liner, e.g. `'bash' '<imaging_path>/scripts/cmd-fs-recon.sh'` or
`'<Rscript>' --no-save --no-restore '<imaging_path>/scripts/...R'`
(ravecore `cmd_execute()`, `R/aaa-cmd.R:54-63`). A sticky toast shows it with
a copy button. Scripts `save_mri_preprocessing_script` and
`save_coregistration_script` also print and return it for agents.

### Other shared behaviour

* Raveio option `cmd_dry_run = TRUE` (`R/module_html.R:7`) removes
  `btn_recon_run` and `btn_coreg_run` from the page.
* All four cards start collapsed and are collapsed again on data load
  (`:108-111`). `basic_info` (`:171-216`, renderUI) holds action links
  `jump_import_dicom`, `jump_acpc_realign`, `jump_recon`,
  `jump_coregistration`; each expands one card and collapses the others
  (`:129-168`).
* Outputs in a collapsed card or a closed `<details>` (the "Terminal script"
  panels) do not render.
* File lists (`update_nii_t1()` `:219-250`, `update_nii_ct()` `:252-295`) run
  on data load, on the refresh controls, and after an ACPC save; **not** after
  an import job.
* Error helper `error_notification()` (`:4-20`) logs through
  `ravepipeline::logger_error_condition()`, so errors also print.
* A raw `shiny::observe()` prints debug state (`:65-70`).
* Log names are stamped when the command is built. Both imports are built in
  the same second, so they share one `log/log-dcm2niix-<time>.log`: the CT
  import overwrites the T1 import's log (seen live 2026-10-04).
* An empty select sends `""`, not NULL: "Start re-alignment" with no MRI
  listed passes the `length()` check and stops in target `viewer_acpc`
  ("Invalid file [...] for ACPC alignment.").
* When AFNI is missing, `cmd_tools$afni` is `NA` and the "Confirmation"
  dialog shows "AFNI: NA" (the agent summaries print "not found").

## 1. Loading data

UI: `R/loader.R:2-149`. Server: `:153-496`.

| Input | Widget | Notes |
|---|---|---|
| `loader_project_name`, `loader_subject_code` | ravedash presets | subject preset allows new codes |
| `loader_sync_from_recent_project_subject`, `loader_sync_from_import_project_subject` | preset sync buttons | second syncs from `import_signals` |
| `mri_path`, `ct_path` | select | choices below |
| `cmd_dcm2niix_path`, `cmd_fs_path`, `cmd_fsl_path`, `cmd_afni_path` | text | defaults `ravecore::cmd_*(error_on_missing = FALSE)` |

**Choices** (`:379-493`, on project/subject change): `"[Upload]"`, `" "`,
then:
* for BIDS projects, `.nii[.gz]` files under `raw_path2`, relative to it;
* every folder and `.nii[.gz]` under `<raw_path>` (recursive, relative,
  minus anything starting with `rave-imaging`).

A folder means a DICOM series. Preselection: first of
`rave-uploads/<TYPE>/...`, `*_T1w.nii*` / names with "MR" (MRI) or "CT" (CT);
before those, subject default `raw_<type>_path` in namespace
`yael_preprocess` (never set, see bugs); before all, `settings.yaml`
`path_mri`/`path_ct` when the settings' subject matches.

**Upload** (`register_file_uploader()`, `:160-265`): choosing `"[Upload]"`
opens a modal with `fancyFileInput` `<type>_upload` (fallback `fileInput`)
and Cancel `<type>_upload_dismiss`; raises `shiny.maxRequestSize` to 300 MB;
copies to `<raw_path>/rave-uploads/<TYPE>/<name>` (overwrite) and selects it.
Person only.

**`load_data` script** (`:274-358`, bound to `load_data_button`,
`alert_params`, no `dispatch_event`):
* `pipeline$set_settings(path_mri, path_ct, skip_recon = FALSE,
  skip_coregistration = FALSE, dcm2niix_path, freesurfer_path, fsl_path,
  afni_path, project_name, subject_code)`;
* `pipeline$run(names = "check_result", type = "vanilla", scheduler = "none")`;
* modal "Confirmation": messages, warnings, tool paths; footer
  `modalButton("Cancel")` and `loader_ready_btn2` "Proceed".
* Before the migration it returned nothing (messages only in the modal);
  now it also prints and returns them as labelled lines.

**"Proceed"** (`:360-375`): removes the modal, fires `data_changed`, saves
`project_name`/`subject_code` in the session. Only `load_data` may run
before this (skill rule), so agents click it with `shiny_ui_operate`.

**Pipeline targets:**
* `check_commandline_tools` -> `cmd_tools` (`main.Rmd:16-88`): normalizes
  each path; a valid non-default path is saved with
  `ravepipeline::raveio_setopt("dcm2niix_path" | "freesurfer_path" |
  "fsl_path" | "afni_path")`, i.e. **globally**. `flirt` =
  `<fsl>/bin/flirt`.
* `check_subject_data` -> `check_result` (`:98-167`): `fs_reconstructed` via
  `threeBrain::check_freesurfer_path(check_volume = TRUE)`; warnings for
  missing dcm2niix/FreeSurfer, missing MRI, existing FreeSurfer folder
  ("recon-all will ignore the imported T1 images"), invalid CT; messages
  always say a new reconstruction and co-registration will be made
  (`skip_*` are always FALSE). `path_mri`/`path_ct` become
  `file.path(<raw_path>, path_*)`; `path_temp` = imaging folder.
* Warnings never block "Proceed".

## 2. Import

Card "Import DICOM Folders or Nifti Images" (`R/module_html.R:38-113`).

| Input | Choices (default) | dcm2niix flag |
|---|---|---|
| `param_dcm2niix_merge` | Auto, Yes, No (Auto) | `-m y` / `-m n` / none |
| `param_dcm2niix_float` | Yes, No (Yes) | `-p y` / `-p n` (Philips precise float scaling) |
| `param_dcm2niix_crop` | Yes, No, Ignore (No) | `-x y` / `-x n` / `-x i` |

Buttons `btn_dcm2niix_run_t1` (`R/module_server.R:577-591`) and
`btn_dcm2niix_run_ct` (`:592-606`): error toast if the target errored, else
`run_command_pipeline(cmd, title = "Importing T1" | "Importing CT")`.
Previews `panel_import_T1`, `panel_import_CT` (`:1114-1130`).

Targets `import_T1` / `import_CT` (`main.Rmd:208-264`) call
`ravecore::cmd_run_dcm2niix(subject, src_path = check_result$path_mri|ct,
type, merge, float, crop, overwrite = TRUE)`. In ravecore
(`R/workflow-dcm2niix.R`):
* stops if `src_path` is missing or inside `<imaging_path>/inputs/<TYPE>`;
* **NIfTI source:** Rscript `scripts/cmd-import-<type>.R` copies to
  `<imaging_path>/inputs/<TYPE>/<TYPE>_RAW.nii[.gz]` and
  `<imaging_path>/derivative/<TYPE>_RAW.nii[.gz]` (overwrite, `copy.date =
  TRUE`: the copies keep the source's modification time);
* **DICOM folder:** bash `scripts/cmd-import-<type>.sh`
  (`inst/shell-templates/dcm2niix-mri-ct.sh`) runs
  `rm -r <imaging_path>/inputs/<TYPE>` (by design, see decisions), then
  dcm2niix into it (file names from the series); no derivative copy;
* log `log/log-dcm2niix-<yymmdd-HHMMSS>.log`.

## 3. ACPC re-alignment

Card "ACPC Re-alignment (Optional)" (`R/module_html.R:115-161`).

| Element | Notes |
|---|---|
| `acpc_3dviewer` | `threeBrain::threejsBrainOutput`; card body resizable, 500 px |
| `param_acpc_infile` | files in `inputs/MRI` minus names ending `ACPC.nii.gz` (case-sensitive, bug) |
| `param_acpc_refresh` | refresh both MRI lists |
| `param_acpc_start` | "Start re-alignment" (styled) |
| `acpc_results` | `htmlOutput`: instructions, then matrix + `acpc_save` |

**Start** (`R/module_server.R:1181-1210`, `error_wrapper = "notification"`):
stops "No MRI file specified" if empty; disables the start button; info
toast "Loading viewer..."; `pipeline$set_settings(acpc_infile)`;
`pipeline$eval("viewer_acpc")`. Target `viewer_acpc` (`main.Rmd:171-202`):
deletes and recreates `<imaging_path>/acpc-alignment/mri`, writes
`brain.nii.gz` (RNifti), builds `threeBrain::threeBrain(path =
acpc-alignment)`. `renderBrain` (`:1212-1229`) plots it with
`control_presets = "acpcrealign"` and re-enables the start button.

**Viewer workflow** (threeBrain
`inst/three-brain-js/src/js/controls/PresetACPCReAlign.js`): function
controllers "Click to Register/Reset AC" / "Confirm Updating AC", same for
PC, "Click to Set/Reset Rotation" / "Confirm Updating Rotation"; read-only
"AC (ScanRAS)", "PC (ScanRAS)". The viewer broadcasts `acpc_realign`;
`acpc_proxy$acpc_alignment` (threeBrain `R/class_proxy.R:835-886`) returns
`ac`, `ac_set`, `pc`, `pc_set`, `ras2acpc` (scanner RAS -> ACPC).
`acpc_results` (`:1233-1261`) shows: choose a file; set AC; set PC; then the
matrix and `acpc_save` "Save ACPC alignment results.".

Agents cannot press these controllers: `rave_3dviewer_set` sends
`setValue()` (`EnhancedGUI.js` `setFromDictionary`), which for a lil-gui
function controller replaces the callback instead of calling it (from code;
not tried live). They can read the viewer (`rave_3dviewer_get`) and the
`acpc_results` text. Decision: coach only.

**Save** (`:1270-1371`): stops if AC or PC unset; alert "Wait..."; writes
* `<imaging_path>/acpc-alignment/mri/brain_acpc.nii.gz` (`sform = ras2acpc
  %*% vox2ras`),
* `<imaging_path>/inputs/MRI/MRI_acpc.nii.gz` (`file.copy`, no overwrite,
  bug),
* `<imaging_path>/acpc-alignment/MR_Scanner_to_MR_ACPC.txt`,
* `<imaging_path>/derivative/MRI_acpc.nii.gz` (no overwrite, bug),
* `<imaging_path>/derivative/MR_Scanner_to_MR_ACPC.txt`,
* `<imaging_path>/derivative/conf-acpc.yaml`;

then alert "Done!" (stays open) and `update_nii_t1()`.

## 4. MRI preprocessing

Card "MRI Preprocessing" (`R/module_html.R:163-270`).

| Input | Notes |
|---|---|
| `mri_preview` | 3-plane plot of `inputs/MRI/<nii_t1>` |
| `param_fs_infile` | files in `inputs/MRI`; preselects subject default `nii_t1` (never set) |
| `param_fs_refresh` | action link "Refresh" |
| `param_fs_prog` | `selected = "recon-all"` is not a choice, so `recon-all -all` shows |
| `param_fs_fresh_start` | "Remove existing work before running the command (if applicable)" |
| `btn_recon_run` | conditionalPanel: only `recon-all -autorecon1`, `simple-import`; absent if `cmd_dry_run` |
| `btn_recon_copy` | "Save & run by yourself" |

Target `image_segmentation` (`main.Rmd:287-398`) stops "Please choose a Nifti
file under ..." when `nii_t1` is invalid, then by program:

| Program | ravecore | Script / log | Notes |
|---|---|---|---|
| `recon-all -all` / `-autorecon1` | `cmd_run_freesurfer_recon_all(args = flag, overwrite = fresh_start)` | `scripts/cmd-fs-recon.sh`, `log-recon-all-*` | `-all` 6-12 h; `-autorecon1` 8-10 min |
| `recon-all-clinical.sh` | `cmd_run_freesurfer_recon_all_clinical()` | `scripts/cmd-fs-recon-clinical.sh` | |
| `YAEL+recon-all` | `cmd_run_yael_preprocess(templates 09b, 09a, 09c, run_recon_all = TRUE)` | `scripts/cmd-yael-preprocess-full.R`, `log-yael-preprocess-*` | ANTs normalization, then recon-all on `inputs/MRI/MRI_RAW.nii.gz` with overwrite |
| `YAEL-only` | `cmd_run_yael_preprocess(template 09b, run_recon_all = FALSE)` | `scripts/cmd-yael-preprocess-simple.R` | ANTs surfaces + envelopes |
| `simple-import` | `cmd_run_r()` | temp script and log | copies T1 to `fs/mri/brain.<ext>`, `derivative/MRI_RAW.<ext>` |

recon-all template (`inst/shell-templates/recon-all-t1.sh`):
* fresh start: `rm -r <imaging_path>/fs`;
* copies the chosen T1 to `derivative/MRI_RAW<ext>` (so after preprocessing
  `MRI_acpc.nii.gz`, `derivative/MRI_RAW` holds the ACPC image;
  `simple-import` does the same); writes `derivative/conf-reconstruction.yaml`;
* symlink `<R_user_dir("ravecore","cache")>/FreeSurfer/<subject_code>` ->
  imaging folder (FreeSurfer dislikes spaces);
* if `fs/mri` exists: continues it (`recon-all -sid fs <flag>`), ignoring the
  chosen T1; else imports the T1;
* after: `T1.mgz` -> `fs/mri/T1.nii` and `derivative/T1.nii`,
  `derivative/transform-Norig.tsv`, `transform-Torig.tsv`,
  `fs/mri/rave_slices<ext>`.

YAEL template (`inst/shell-templates/yael-preprocess.R`): with
`raveio_getopt("max_mem") < 12`, MNI152 09b is replaced by 09a (message says
16 GB are needed). FreeSurfer version: `$FREESURFER_HOME/build-stamp.txt`
(e.g. `freesurfer-macOS-darwin_arm64-8.0.0-...`); RAM:
`ps::ps_system_memory()$total`.

## 5. CT-MRI co-registration

Card "Co-registration CT and T1" (`R/module_html.R:272-640`).

| Input | Choices (default) |
|---|---|
| `ct_preview` | 3-plane plot of `inputs/CT/<nii_ct>` |
| `param_coreg_ct` | files in `inputs/CT`; preselects subject default `nii_ct` (never set) |
| `param_coreg_refresh` | action link "Refresh" |
| `param_coreg_mri` | files in `derivative/` minus `ct_in_t1*`, `MRI*` first (includes `CT_RAW`) |
| `coreg_ct_program` | NiftyReg, ANTs, img_pipe, AFNI, FSL (NiftyReg) |
| `coreg_niftyreg_type`, `_interp` | rigid/affine/nonlinear (rigid); trilinear/cubic/nearest (trilinear) |
| `coreg_ants_type`, `_aff_metric`, `_syn_metric` | Rigid/DenseRigid/Affine/SyN (Rigid); mattes/meansquares/GC; mattes/meansquares/demons/CC |
| `coreg_nipy_reg_type`, `_interp`, `_cost`, `_optimizer` | rigid/affine; pv/tri; crl1/cc/cr/mi/nmi/slr; powell/steepest/cg/bfgs/simplex |
| `coreg_nipy_clean_source`, `_inverse_target`, `_precenter_source` | checkboxes, TRUE |
| `coreg_fsl_dof`, `_cost`, `_search`, `_searchcost` | "6 (rigid body)"/7/9/12; `FSL_COST_FUNCTIONS` (mutualinfo); 90/180 |
| `btn_coreg_run` | absent if `cmd_dry_run` |
| `btn_coreg_copy` | "Save & run by yourself" |

Per-program inputs live in conditionalPanels (`R/module_html.R:394-616`).

MR image resolution (targets `main.Rmd:421-624`): `derivative/<reference>`
if valid, else `inputs/MRI/<nii_t1>`. AFNI always uses `inputs/MRI/<nii_t1>`.
CT: `inputs/CT/<nii_ct>`, else error "Please choose a valid CT Nifti file".

| Program | ravecore | Script / log | Runs as |
|---|---|---|---|
| NiftyReg | `cmd_run_niftyreg_coreg()` | `scripts/cmd-nifty-coregistration.R`, `log-rave-nifty-coregistration.R.log` | Rscript job |
| ANTs | `cmd_run_ants_coreg()` | `scripts/cmd-ants-coregistration.R`, `log-rave-ants-coregistration.R.log` | Rscript job; installs ANTs if missing |
| img_pipe | `cmd_run_nipy_coreg()` | `scripts/cmd-rave-nipy-coregistration.R`, `log-rave-nipy-*` | Rscript job; configures python, pip-installs nipy |
| FSL | `cmd_run_fsl_flirt(overwrite = FALSE)` | `scripts/cmd-fsl-flirt.sh`, `log-flirt-*` | bash job |
| AFNI | `cmd_run_3dAllineate(overwrite = FALSE)` | `scripts/cmd-afni-3dallineate.sh`, `log-3dAllineate-*` | bash job |

Outputs in `<imaging_path>/coregistration/` (`CT_RAW`, `MRI_reference`,
`ct_in_t1`, `CT_IJK_to_MR_RAS.txt`, `CT_RAS_to_MR_RAS.txt`; NiftyReg
nonlinear: control points; ANTs: warps; FSL: `ct2t1.mat`; AFNI:
`afni_3dAllineate/`) and `<imaging_path>/derivative/`
(`conf-coregistration.yaml`, copies, `transform-ctIJK2mrRAS.txt`,
`transform-ctRAS2mrRAS.txt`).

## Reports

Footer "Generate Reports" -> script `run_analysis` (`R/module_server.R:74-78`)
-> `report_wizard$launch(subject, multiple = TRUE)`. The ravedash dialog
"Generate reports" registers `_report_label` (select, multiple) and
`_report_generate`. Each report runs as a background job
(`pipeline$generate_report`); toasts "Report(s) scheduled", then "Report
generated!" with the path and a "View report" link, or an error.

| Report | Entry | Reads |
|---|---|---|
| Coregistration Quality Report | `report-coregistration.Rmd` | `<imaging_path>/coregistration/` (underlay MRI, overlay CT, transform) |
| Normalization Quality Report | `report-normalization.Rmd` | `<imaging_path>/normalization/`, YAEL templates |

## External programs

| Program | Needed for | Found by |
|---|---|---|
| dcm2niix | DICOM import | `ravecore::cmd_dcm2niix()`, raveio option `dcm2niix_path` |
| FreeSurfer | recon-all, clinical, YAEL+recon-all | `cmd_freesurfer_home()`, option `freesurfer_path` |
| FSL | FLIRT | `cmd_fsl_home()`, option `fsl_path` |
| AFNI | 3dAllineate | `cmd_afni_home()`, option `afni_path` |
| ANTs (rpyANTs, python) | YAEL, ANTs co-registration | `rpyANTs::ants_available()` |
| NiftyReg (RNiftyReg) | NiftyReg co-registration | R package, always installed |
| nipy (python) | img_pipe | rpymat environment |

bash programs need a Unix shell (WSL2 on Windows).

## Downstream: electrode_localization

`modules/electrode_localization/R/loader.R:370-440` reads
`<imaging_path>/derivative/conf-coregistration.yaml`: with
`CT_IJK_to_MR_RAS` in its outputs it uses "CT (IJK) to MR (RAS) transform +
Raw CT"; with `ct2t1`, "FSL transform + Raw CT + MRI"; with only `ct_in_t1`,
"Re-sampled CT". A new co-registration therefore changes the basis of
electrodes localized before. That module can also run its own YAEL
preprocessing (`:663-676`), writing into the same imaging folder.

## Bugs and dead code

| # | Bug | Where | Status |
|---|---|---|---|
| 1 | ACPC re-save keeps the first `MRI_acpc.nii.gz`: `file.copy()` without `overwrite` (returns FALSE when the file exists); transforms and `conf-acpc.yaml` update | script `save_acpc` | fixed 2026-10-04: `overwrite = TRUE` |
| 2 | Import history saved under the old module ID `surface_reconstruction`, as an absolute path; the loader reads `yael_preprocess` and matches relative choices with `%OF%` | ravecore `R/workflow-dcm2niix.R`; `R/loader.R` | fixed in ravecore 0.1.1.19: namespace `yael_preprocess`, path relative to `<raw_path>` (or `raw_path2`) via `image_source_relpath()` |
| 3 | BIDS: images listed relative to `raw_path2` (`<bids>/sub-01`) but joined with `raw_path` (`<bids>/derivatives/rave/raw_dir/sub-01`) | `main.Rmd` `check_subject_data` | fixed 2026-10-04: `resolve_raw_image_path()` (`R/shared-functions.R`); see `BUGS.md` |
| 4 | ACPC input list filter `endsWith(choices, "ACPC.nii.gz")` misses `MRI_acpc.nii.gz` | `update_nii_t1()` | fixed 2026-10-04: case-insensitive `grepl()` |
| 5 | `param_dcm2niix_float` label repeats the merge label | `R/module_html.R` | fixed 2026-10-04: "Philips precise float scaling" |
| 6 | "Start re-alignment" stays disabled after a failed start (e.g. no MRI imported): the body disables it, and only the viewer's render re-enables it, which a failed start never triggers. People must reload the page; script `start_acpc_viewer` still works | script `start_acpc_viewer`, `output$acpc_3dviewer` | found live 2026-10-04; not fixed (changes a button function: needs approval) |

Dead code: `btn_dcm2niix_copy` observer (`R/module_server.R:533-576`) and
`btn_mri_morph_run` observer (`:787-824`) have no buttons;
`updateCheckboxInput("skip_recon")` (`R/loader.R:483-487`) targets a removed
input; subject defaults `nii_t1`/`nii_ct` (`:232`, `:265`) are never set;
`params$template_brain` is always NULL.

## Demo subject

`YAEL/yael_demo_001`, native format; raw folder
`~/rave_data/raw_dir/yael_demo_001`.

* `<raw_path>/rave-uploads/MRI/MRI_reference.nii.gz`: FreeSurfer-conformed T1
  (256^3, 0-255). `<raw_path>/rave-uploads/CT/CT_RAW.nii.gz`: 512x512x178,
  -1024..3071. Both added 2026-10-04 and byte-identical to the copies in
  `derivative/` and `coregistration/`.
* No `inputs/`, `log/`, `scripts/`, `acpc-alignment/`: the main-card file
  lists start empty until an import.
* Minimal `fs/` (aseg/aparc volumes, `brain.finalsurfs.mgz`, surfaces);
  `check_freesurfer_path()` is TRUE, so the load check warns about an
  existing FreeSurfer folder.
* An ANTs Rigid co-registration underlies `localization/`, `custom-data/`,
  and the electrode tables in `rave/meta`.
* `fs/mri/ct_in_t1.nii.gz` is byte-identical to the T1 (misnamed).
* `notes/surface_reconstruction.*` hold the import history (bug 2).
* Machine: FreeSurfer 8.0.0, 32 GB RAM, ANTs and NiftyReg available.

## MCP migration

Done 2026-10-04 (plan:
`~/.claude/plans/use-the-agents-skills-build-module-mcp-s-valiant-moler.md`).

* **Registered inputs** (wrappers only, plus hover tooltips): `mri_path`,
  `ct_path`; `cmd_*_path` (`writable = FALSE`); `param_dcm2niix_*`;
  `param_acpc_infile`, `param_acpc_refresh`; `param_fs_infile`,
  `param_fs_refresh`, `param_fs_prog`, `param_fs_fresh_start`
  (`writable = FALSE`); `param_coreg_ct`, `param_coreg_refresh`,
  `param_coreg_mri`, `coreg_ct_program`, all 16 per-program inputs; the four
  `jump_*` links in `basic_info`.
* **Button scripts** (observer bodies moved verbatim; each button runs its
  script; a printed summary appended): `import_t1`, `import_ct`,
  `start_acpc_viewer`, `save_acpc`, `save_mri_preprocessing_script`,
  `run_coregistration`, `save_coregistration_script`. `load_data` and
  `run_analysis` got descriptions; `load_data` prints what its dialog shows.
  MRI preprocessing's "Run from RAVE" (`btn_recon_run`) has no script.
* **New scripts:** `imaging_status`, `command_status` (read-only),
  `generate_coregistration_report`, `generate_normalization_report` (call
  the report wizard's own `generate()`).
* **Fixes:** bugs 1-5 above; the only visible text change is the float label.
* **`agents.yaml`:** wavelet's tool set plus `rave_3dviewer_get`; the
  `rave-module` skill's `set_inputs.R` and `run.R` are off (pipeline settings
  include the command-line paths). The system prompt is for the in-dashboard
  chat; MCP agents read the descriptions and the manual's first lines.
* **`test-mcp.R`** (demo only): backs up and restores the subject itself,
  verified by checksum; error cases first; `do_write <- FALSE` stops before
  the imports.
