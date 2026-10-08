# Electrode Localization Module Reference

The Electrode Localization module (`electrode_localization`) places a subject's
iEEG electrodes on the post-implant CT, co-registered to the pre-implant T1 MRI,
in a 3D viewer. People mark the electrodes; the module labels each one with
FreeSurfer atlas regions and saves the electrode table (coordinates in several
spaces, labels, electrode models) to the subject.

**Prerequisite:** a RAVE subject with a T1 MRI and a CT co-registered to it
(the MRI/CT Preprocessing module `yael_preprocess`, or the loader's own
'Preprocess' tab), or at least an MRI for 'Localize without CT'.

## Rules for agents (read first)

People keep their usual buttons. These rules are for agents only:

1. **People place the electrodes.** Marking, re-localizing, interpolating, and
   refining happen in the 3D viewer with the mouse and keyboard. Explain how
   (this manual), point the crosshair at places (`rave_3dviewer_set`, name
   `crosshair`), and read the results; do not move electrodes for people.
2. **Confirm the electrode plan before loading.** Script `load_data` writes the
   plan (`loader_plan`) into the subject's staged table
   (`meta/electrodes_unsaved.csv`), keeping positions already placed. Channels
   already in that table must stay in the plan, or loading fails (nothing is
   written).
3. **'Save to subject' is for people only.** Its button (`save_btn`, in the
   'Electrode table' dialog) refuses agents' clicks. Ask the person to review
   the table, tick the post-process options they need, and click it.
4. **Ask first** before 'Reset current group' (`action_reset_btn`; it removes
   the group's placed electrodes) and before changing a FreeSurfer label
   (`fsindex_label`, `fsindex_label_next`).
5. **Preprocessing from the loader** (`loader_preprocess_run`) only after the
   person dropped both images (agents cannot drop files) and confirmed; it
   replaces the subject's co-registration. For DICOM images, or to choose
   images from the subject's raw folder, use the MRI/CT Preprocessing module.

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: Choose the RAVE project and subject. The loader reads the subject's
co-registration and electrode table and fills in the rest.

Step 2: 'Image Selection' card:

* **'Data selector' tab** — **Localization method**: 'CT (IJK) to MR (RAS)
  transform + Raw CT' (RAVE's co-registration: YAEL-reg3d, ANTs, NiftyReg; the
  usual choice), 'FSL transform + Raw CT + MRI' (FSL flirt), 'Re-sampled CT'
  (a CT already resampled into the MRI), or 'Localize without CT' (MRI slices
  only). Then **Choose CT** (e.g. `CT_RAW.nii.gz`), **Choose raw MRI** (e.g.
  `MRI_reference.nii.gz`) and **Transform matrix** (`CT_IJK_to_MR_RAS.txt`, or
  `ct2t1.mat` for FSL), files in the subject's co-registration folder.
  'Refresh file list' reads the folder again.
* **'Preprocess' tab** (shown when the subject has no co-registered CT): drop a
  T1 MRI and a CT (NIfTI, `.nii` or `.nii.gz`) and click 'Run YAEL preprocess
  pipeline'. It imports both images, copies the MRI into place without
  processing it (simple-import: no surfaces or atlases yet), and co-registers
  the CT to the MRI with YAEL-reg3d (FastRigid, about a minute). A log window
  shows progress; click 'Done' when it is finished. A notification then gives a
  terminal command for YAEL + FreeSurfer recon-all on the same MRI (surfaces,
  atlas labels, MNI mapping): run it before saving the electrodes, because it
  rebuilds the FreeSurfer folder (including saved electrode shapes).

Step 3: 'Electrode Plan' card: one group per electrode (shaft, grid, or lead):

* **GroupLabel** — label prefix; contacts are named `<prefix>1`, `<prefix>2`, ...
* **Channels** — channel numbers, e.g. `1-12`. For an electrode model, list
  them in contact order (contact 1 first) and write `NAx2` for 2 unplugged
  contacts, e.g. `13-18,NAx2`.
* **Type** — `iEEG` or `ECoG` (each contact placed as a sphere), `Others`, or
  an electrode model (prototype), e.g. `SEEG-ADTECH-SD12R-SP05X-000` or
  `DBS-MEDTRONIC-3389`: the model's shape and spacing come with it.
* **Hemisphere** — `auto`, `left`, or `right`; limits the FreeSurfer labels to
  that hemisphere (`auto` decides per electrode).

The card's tools upload or download the plan as a CSV (`GroupLabel`, `Channels`,
`Type`, `Hemisphere`). Click 'Load subject'.

### 2. Analysis inputs

Left panel, 'Localization Plan Details':

* **Group buttons** — one per group (e.g. `LA.12.iEEG [1-12]`); click one to
  show it in the group table and the 3D viewer.
* **'Reset current group'** — removes the active group's placed electrodes so
  it can be placed again.
* **'Re-compute FreeSurfer labels'** — labels the active group's electrodes
  again at their current positions (labels picked by hand are dropped).
* **Group table** — one row per electrode (sphere groups) or per control point
  (electrode models), with **Label**, **scannerRAS** (T1 scanner RAS, mm),
  **MNI152** (affine, links to neurosynth), and **FSLabel**. Rows not placed yet
  are blank. For an electrode model, control points that are contacts show the
  contact and its label; fiducial markers (points on the shaft that are not
  contacts, e.g. the 2 markers of a directional DBS lead) show `(marker k)`.
* **Selecting a row** offers 'To re-localize this electrode, click here' (the
  next electrode placed replaces the row), and for sphere rows a **FreeSurfer
  Label** picker with 'Save & Next'.

3D viewer ('YAEL 3D Viewer', output `localization_viewer`), folder 'Electrode
Localization' of its control panel:

* **Edit Mode** — `CT/volume` (double-click a bright CT spot: the electrode
  snaps to the CT intensity), `MRI slice` (double-click on the MRI slices, for
  localizing without CT), `refine` (double-click an electrode to highlight it,
  then move it), or `disabled`.
* **Interp Size** — contacts to fill in between the two placed ends: a number
  (e.g. `10` for 12 contacts) or spacing x count (e.g. `5.0x11`: 11 steps of
  5 mm). Electrode models fill it in. **Interpolate** places them (refined to the
  CT); **Interpolate (no refine)** places them exactly evenly; **Extrapolate**
  continues past the last one.
* **Auto-Adjust Highlighted**, **Auto-Adjust All** (refine mode) — move the
  highlighted (or every) electrode to the nearby CT intensity peak; **Reset
  Highlighted** moves it back.
* **Register from Crosshair** (key `` ` ``) — places an electrode at the
  crosshair.
* **Brain Shift** and **Max Shift** — for surface (ECoG) electrodes: project
  them onto the brain surface when the CT shows them shifted (set
  automatically for surface groups).
* Keys in refine mode: `1`/`⇧1` move right/left, `2`/`⇧2` anterior/posterior,
  `3`/`⇧3` superior/inferior; `4`/`⇧4` rotate an electrode model's shaft.
* `- tkrRAS`, `- T1 RAS`, `- MNI305` show the highlighted electrode's position.

**'Stage & Show Electrode Table'** (the RAVE! button at the bottom) stages every
group (`meta/electrodes_unsaved.csv`, `geometry_unsaved.json`; the next load
starts from here) and opens the 'Electrode table' dialog.

### 3. Outputs

* **'Electrode table' dialog** — every electrode: label, location type,
  electrode model, FreeSurfer index and label, and its position in tkrRAS, T1
  RAS (scanner), MNI305, and MNI152. **Post-process options**: 'Surface mapping
  to inflated brain & fsaverage' (sphere coordinates for group analysis; needs
  FreeSurfer surfaces), 'Non-linear MNI152 coordinates' (needs RAVE's Python
  with ANTs and a YAEL normalization of the subject), 'Burn electrodes to T1w
  MRI' (writes a copy of the MRI with the electrodes drawn in). **'Save to
  subject'** writes:
  * `meta/electrodes.csv` (RAVE's electrode table; the previous one is kept as
    `electrodes_[<date>].csv`), also in the imaging folder's `localization/`;
  * `meta/electrodes_in_ct.csv` and `localization/sub-<code>_space-CT_electrodes.tsv`
    (CT voxel and RAS positions, when a CT is used);
  * BIDS `meta/sub-<code>_space-<system>_electrodes.tsv` and
    `_coordsystem.json`, also in `localization/`;
  * electrode model shapes in the FreeSurfer folder (`RAVE/geometry/*.json`),
    a copy for the 3D viewers (`custom-data`), and burn-in MRIs in
    `localization/` when chosen; then an HTML report.
  The staged files are removed after saving.
* **Electrode table columns** (`electrodes.csv`): `Coord_x/y/z` tkrRAS
  (FreeSurfer surface RAS; for shifted surface electrodes the projected
  position), `OrigCoord_x/y/z` (before the shift), `T1R/T1A/T1S` (T1 scanner
  RAS), `MNI305_x/y/z` and `MNI152_x/y/z` (affine unless the non-linear option
  ran), `FSIndex`/`FSLabel` (the label shown) and `FSLabel_<atlas>` for each
  atlas (`aparc_a2009s_aseg`, `aparc_aseg`, `aparc_DKTatlas_aseg`, `aseg`),
  `Label`, `LabelPrefix`, `Prototype`, `ContactOrder`, `Hemisphere`,
  `SurfaceElectrode`, `DistanceShifted`, `Sphere_x/y/z`, `MRVoxel_I/J/K`.

## Common procedures

Each recipe starts after loading the subject.

### Procedure — localize a depth electrode with an electrode model (sEEG, DBS)

The group table lists the model's control points (contacts, or `(marker k)`
for fiducial markers); 'Show/Hide localization instructions' under the table
gives the steps for the active group.

Step 1: Click the group's button. Edit Mode turns to `CT/volume`.
Step 2: Double-click the first control point on the CT, then the last.
Step 3: Interp Size is filled in from the model (e.g. `5.0x11`); click
'Interpolate'. The model fits through the points; the table shows the control
points' positions and labels.
Step 4 (directional leads): in refine mode, `4`/`⇧4` rotates the shaft.

### Procedure — localize sphere electrodes (iEEG, ECoG)

Step 1: Click the group's button; Edit Mode `CT/volume` (or `MRI slice`
without CT).
Step 2: Double-click each contact, or the two ends and set Interp Size to the
number of contacts in between, then 'Interpolate'.
Step 3 (ECoG grids): check Brain Shift/Max Shift (set automatically) so the
contacts project onto the brain surface.

### Procedure — refine or re-localize one electrode

Step 1: Edit Mode `refine`; double-click the electrode (it turns red), then
'Auto-Adjust Highlighted' or move it with `1`/`2`/`3` (with `⇧` for the other
direction). Labels follow the new position.
Step 2 (start over for one electrode): select its row in the group table,
click 'here' (re-localize), then double-click the right spot.

### Procedure — change an electrode's FreeSurfer label

Step 1: Select the electrode's row (sphere groups).
Step 2: Pick the label in 'FreeSurfer Label', click 'Save & Next'. The label
stays until 'Re-compute FreeSurfer labels'.

### Procedure — save the localization

Step 1: Click 'Stage & Show Electrode Table'.
Step 2: Review the table; tick the post-process options; click 'Save to
subject' (people only).

## Caveats

* FreeSurfer labels come from the subject's atlases (`aparc.a2009s+aseg`,
  `aparc+aseg`, `aparc.DKTatlas+aseg`, `aseg`): without FreeSurfer (e.g. right
  after the loader's simple-import) they read `Unknown`.
* Labels follow the electrodes: they are computed again when an electrode
  moves, and for electrode models at the fitted contact positions when
  staging. Only labels picked by hand stay, until 'Re-compute FreeSurfer
  labels', and only in the current session.
* The loader's preprocessing uses YAEL-reg3d FastRigid, a quick registration:
  check that the CT's bright contacts sit where the electrodes are; for a finer
  registration use the MRI/CT Preprocessing module.
* Loading with a plan that leaves out channels already in the staged or saved
  table fails with "... Please include them in the electrode plan."; nothing is
  written.
* Clicking a group right after loading may show nothing while the viewer still
  loads its images; click it again.
* Running YAEL + FreeSurfer recon-all after saving removes the saved electrode
  model shapes (`RAVE/geometry` in the FreeSurfer folder).

## Run the pipeline without the UI

The UI writes the plan and image choices into the pipeline settings; placing
electrodes needs the 3D viewer. Without the UI you can load the plan and read
the staged or saved tables:

```r
pipeline <- ravepipeline::pipeline("electrode_localization")
pipeline$set_settings(
  project_name = "demo",                  # RAVE project
  subject_code = "DemoSubject",           # subject
  localization_plan = list(               # the electrode plan (loader_plan)
    list(label = "LA", dimension = "1-12", type = "iEEG", hemisphere = "auto")
  ),
  transform_space = "no_ct",              # ijk2ras, fsl, resampled, or no_ct
  path_ct = NULL,                         # e.g. "{subject$imaging_path}/coregistration/CT_RAW.nii.gz"
  path_mri = NULL,                        # MRI the transform maps to
  path_transform = NULL                   # e.g. ".../coregistration/CT_IJK_to_MR_RAS.txt"
)
# Writes the plan into meta/electrodes_unsaved.csv (as 'Load subject' does)
plan_list <- pipeline$run("plan_list")

# Electrode table saved to the subject
ravecore::as_rave_subject("demo/DemoSubject")$get_electrode_table()
```

The target `localization_result_initial` stages a `localization_list` (set by
the module from the viewer), and `localization_result_final` saves to the
subject: both write subject files.

## Drive the module with MCP tools

Follow the rules at the top. Scripts: `load_data` (loader), then
`localization_status` (read-only, any time) and `run_analysis` (stages and
opens the 'Electrode table' dialog). A script's reply has `result` and
`output` (what it printed).

### Load data

* Subject: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`,
  then `loader_subject_code`; wait until `loader_ct_fname` has choices
  (`tool("tool__shiny_input_info", inputIds = list("loader_ct_fname", "loader_plan"))`).
  No choices: no co-registration yet (see rule 5, or the MRI/CT Preprocessing module).
* Plan: read `loader_plan` and confirm it with the user; to change it:
  `tool("tool__shiny_input_update", inputId = "loader_plan", value = "[{\"label\":\"LA\",\"dimension\":\"1-12\",\"type\":\"iEEG\",\"hemisphere\":\"auto\"}]")`.
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`. On
  failure the error is in the reply and an 'Error found!' alert stays open:
  `tool("tool__shiny_ui_operate", action = "close_alert2")`.
* Preprocess (after the person dropped the files and confirmed):
  `tool("tool__shiny_input_update", inputId = "loader_preprocess_run", value = "")`;
  poll `tool("tool__shiny_query_ui", css_selector = "#electrode_localization-dismiss_modal")`
  until it reads 'Done', then `tool("tool__shiny_ui_operate", action = "click", target = "dismiss_modal")`.

### Configure and run

* Overview: `tool("tool__module_interactive_script_run", name = "localization_status")`.
* Show a group: `tool("tool__shiny_input_update", inputId = "switch_plan_btn_2", value = "")`.
* Explain placing (procedures above). Point the viewer:
  `tool("tool__rave_3dviewer_set", outputId = "localization_viewer", name = "crosshair", data = "{\"position\":[-30,10,25]}")`
  (scanner RAS, as in the scannerRAS column); viewer settings, e.g.
  `name = "controllers", data = "{\"Edit Mode\":\"refine\"}"` when the person asks.
* Re-compute labels: `tool("tool__shiny_input_update", inputId = "action_reset_fslabel_btn", value = "")`.
* Stage: `tool("tool__module_interactive_script_run", name = "run_analysis")`,
  then ask the person to review and click 'Save to subject'. Close the dialog
  without saving: `tool("tool__shiny_ui_operate", action = "dismiss_modal")`.

### Inspect results

* Group table (shows the active group):
  `tool("tool__shiny_query_ui", css_selector = "#electrode_localization-group_table", transform_image = FALSE)`.
* Staged table in the dialog:
  `tool("tool__shiny_query_ui", css_selector = "#electrode_localization-electrode_table_preview", transform_image = FALSE)`.
* Picture of the viewer: `tool("tool__shiny_query_ui", css_selector = "#electrode_localization-localization_viewer")`;
  its settings: `tool("tool__rave_3dviewer_get", outputId = "localization_viewer", name = "controllers")`.
* After the person saved: `localization_status` (saved time, files).
