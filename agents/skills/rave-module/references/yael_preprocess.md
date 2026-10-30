# MRI/CT Preprocessing Module Reference

<!-- Technical details for developers: modules/yael_preprocess/knowledge_base.md -->

The MRI/CT Preprocessing module (`yael_preprocess`) prepares a subject's images
for electrode localization: it imports the pre-surgery T1 MRI and the
post-implant CT, optionally re-aligns the MRI to the AC-PC line, reconstructs or
normalizes the MRI (FreeSurfer, YAEL), and co-registers the CT to the MRI. The
Electrode Localization module then uses the results. The module is optional:
labs may use their own imaging pipeline (see https://yael.wiki).

**Prerequisite:** a RAVE subject (project and subject code), and its T1 MRI and
CT as DICOM folders or NIfTI files, either inside the subject's raw folder or
uploaded in the loader. DICOM images need `dcm2niix`; FreeSurfer is needed only
for surface reconstruction; `YAEL+recon-all`, `YAEL-only` and ANTs
co-registration use RAVE's Python environment; the `YAEL-native` commands and
YAEL-reg3d co-registration (the default) are built into RAVE and need no Python.

## Rules for agents (read first)

People keep their usual buttons. These rules are for agents only:

1. **Importing confirms the image.** Importing an MRI or CT tells RAVE to use
   that image for the rest of the pipeline. Importing another image later makes
   all processing of the old one invalid (a DICOM import deletes the previously
   imported images of that type, by design). Import an image type only when the
   subject has never had one imported (`imaging_status` tells). If one exists,
   do not import: ask the user to do it.
2. **What you may run from RAVE, after the user confirms each run:** the two
   imports (about 10 seconds), and CT-MRI co-registration with ANTs (1-2
   minutes), YAEL-reg3d (the default program, built into RAVE; Rigid under a
   minute, DenseRigid a few minutes) or NiftyReg (5-10 minutes). Nothing else:
   never click "Run from RAVE" in MRI Preprocessing, and never run img_pipe,
   FSL, or AFNI from RAVE.
   **Recommend co-registration programs in this order: ANTs (if `antspyx` installed) > YAEL-reg3d >
   FSL (if `FSLDIR` is set) > NiftyReg > img_pipe, AFNI** (offer ANTs first; YAEL-reg3d is an
   equal alternative). ANTs needs RAVE's Python (`rpymat`): when script
   `imaging_status` shows `ants=not available`, recommend YAEL-reg3d first.
   Skip programs that are not available (`imaging_status`: `ants`,
   `fsl_flirt`, `afni`). FSL runs only from its saved script (rule 3).
3. **Long steps run outside RAVE.** For MRI preprocessing (and any other
   program), click "Save & run by yourself": it saves a script in the subject's
   imaging folder and gives a one-line terminal command. If you have a terminal
   on the computer where RAVE runs, you may run that command in a separate
   terminal (on macOS: open Terminal and run it). Never run it inside RAVE: when
   RAVE closes, its jobs stop. Otherwise, give the user the command to run.
4. **Recommend YAEL+recon-all** for MRI preprocessing, or **YAEL-only** when
   FreeSurfer surfaces are not needed. When script `imaging_status` shows
   `ants=not available`, recommend the native entries, which need no Python:
   **YAEL-native+recon-all**, or without FreeSurfer **YAEL-native-mni152b**
   (**YAEL-native-mni152a** needs less memory). When RAVE runs on your computer,
   check the FreeSurfer version against the computer's memory: FreeSurfer 7.x
   needs 16 GB of RAM and 8.x needs 32 GB. If the memory is short, tell the user
   to switch FreeSurfer versions or to run an entry without FreeSurfer.
5. **ACPC re-alignment is the person's job.** You may choose the MRI, open the
   card, start the viewer, and explain each step; the person places AC and PC.
   Click "Save ACPC alignment results." only after the person says they are done
   and confirms.
6. **MRI preprocessing and co-registration use the same MR image.** "MRI file"
   in MRI Preprocessing and "MR image" in co-registration must be identical:
   the imported MRI (`MRI_RAW`) when ACPC re-alignment is not part of the
   pipeline, or `MRI_acpc.nii.gz` when it is (even though other images stay in
   the lists). Never use `T1.nii`, even after preprocessing finished, and never
   pair an ACPC image with a non-ACPC one: ACPC re-alignment changes the
   image's orientation matrix. (People may choose otherwise.)
7. **Order.** Import first, then ACPC re-alignment when it is used. After
   that, MRI preprocessing and co-registration may run in either order, or at
   the same time.
8. **Never** tick "Remove existing work before running the command", edit the
   command-line paths in the loader (they change RAVE's settings for every
   subject), or choose "[Upload]" (only a person can upload a file).
9. **RAVE cannot tell whether a command worked.** After a run, read the log
   (`command_status`) and check the results (`imaging_status`) before you
   report success.
10. **Quality reports** need no confirmation: generate them when useful
    (`generate_coregistration_report`, `generate_normalization_report`).

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: In the loader's "Data settings" card, choose the project and subject
("Project and Subject"). The sync buttons below copy the subject from your
recent work or from the Import Signals module.

Step 2: Choose the images, only if this subject's MRI or CT were never imported
("If you have already imported MRI and CT before, you may ignore these inputs"):
* **Raw MRI (DICOM folder or Nifti file)**: the pre-surgery T1 MRI.
* **Raw CT (DICOM folder or Nifti file)**: the post-implant CT.

The lists show every folder and NIfTI file in the subject's raw folder; a folder
means a DICOM series. To use a file from elsewhere, choose "[Upload]" and upload
a `.nii` or `.nii.gz` file.

Step 3 (optional): In "Command-line (optional)", check the paths to `dcm2niix`,
FreeSurfer, FSL, and AFNI. They are filled in when RAVE finds the programs.

Step 4: Click "Check data and command-line tools". The "Confirmation" dialog
lists what will run, warnings, and the program paths. Read the warnings, then
click "Proceed".

### 2. Analysis inputs

The main page has four cards, collapsed at first. Open one with the links under
"Actions:" in the "Subject Information" panel on the left. Each card's footer
shows the terminal script it would run ("Terminal script - ...").

**Import DICOM Folders or Nifti Images**: copies the chosen NIfTI file, or
converts the DICOM folder with `dcm2niix`, into the subject's imaging folder.
* The three `dcm2niix` options ("Merge 2D slices from same series regardless of
  echo, exposure, etc.": `Auto`; "Philips precise float scaling": `Yes`; "Crop
  3D acquisitions": `No`) matter only for DICOM folders; keep the defaults
  unless you know your scanner needs otherwise.
* "Run from RAVE (T1 MRI)" and "Run from RAVE (CT)" import the images chosen in
  the loader. A dialog shows the log; click "Done" when it finishes, then read
  the notice: RAVE does not know whether the command worked, so check the log.

**ACPC Re-alignment (Optional)**: re-orients the MRI so the anterior and
posterior commissures (AC, PC) define the axes.
* **MRI file (select one to be ACPC re-aligned)**: an imported MRI. Click the
  refresh button if the list is empty after an import.
* "Start re-alignment" opens the image in the viewer. Then, in the viewer's
  controls: "Click to Register/Reset AC", move the crosshair onto the AC,
  "Confirm Updating AC"; the same for PC; optionally "Click to Set/Reset
  Rotation" to adjust the inter-hemispheric plane, then "Confirm Updating
  Rotation". The text under the viewer guides each step.
* "Save ACPC alignment results." appears once AC and PC are set. It saves the
  aligned image as `MRI_acpc.nii.gz`.

**MRI Preprocessing**: reconstructs and/or normalizes the MRI.
* **MRI file (select one with the best quality)**: the imported MRI, or
  `MRI_acpc.nii.gz` after ACPC re-alignment. "Refresh" updates the list. The
  preview shows the chosen image.
* **Command** (the dropdown shows each name with its estimated time, and a gray
  description below; these are the input values):
  * `YAEL+recon-all` (shown as "YAEL-ants+recon-all", recommended): YAEL
    normalization to MNI templates with ANTs, then FreeSurfer reconstruction.
    Hours.
  * `YAEL-only` (shown as "YAEL-ants"): YAEL normalization with ANTs, without
    FreeSurfer, when surfaces are not needed.
  * `YAEL-native+recon-all` (shown as "YAEL-ravetools+recon-all"): normalization
    to the asymmetric MNI152b template with the native engine (no Python), then
    FreeSurfer reconstruction. Hours.
  * `YAEL-native-mni152b` (shown as "YAEL-ravetools MNI152b"): native
    normalization to the asymmetric and the symmetric 0.5 mm templates, without
    FreeSurfer. Up to 22 GB of memory.
  * `YAEL-native-mni152a` (shown as "YAEL-ravetools MNI152a"): the same with the
    1 mm templates. About 8 GB of memory. Both entries take minutes to tens of
    minutes per template, depending on the image.
  * `recon-all -all`: full FreeSurfer reconstruction, 6-12 hours.
  * `recon-all -autorecon1`: normalizes and skull-strips the MRI only, about 10
    minutes; enough to localize electrodes.
  * `recon-all-clinical.sh`: FreeSurfer's script for clinical images.
  * `simple-import`: copies the MRI into place without processing it.
* **Remove existing work before running the command (if applicable)**: FreeSurfer
  commands (`YAEL+recon-all` and `YAEL-native+recon-all` included) delete the
  subject's previous FreeSurfer folder first; YAEL commands remove the `tmp`
  folder under the imaging folder and calculate the normalization again. Without
  it, FreeSurfer continues an existing folder and ignores the chosen MRI, and
  YAEL keeps an existing normalization.
* "Save & run by yourself" saves the script and shows the terminal command to
  paste. "Run from RAVE" is offered only for `recon-all -autorecon1`,
  `YAEL-native-mni152b`, `YAEL-native-mni152a` and `simple-import`.

**Co-registration CT and T1**: aligns the CT to the MRI.
* **CT file**: the imported CT ("Refresh" updates the list). The preview shows
  it.
* **MR image**: the MRI to align to. Use the same image as "MRI file" in MRI
  Preprocessing: the imported original (`MRI_RAW`), or `MRI_acpc.nii.gz` after
  ACPC re-alignment. The list holds every image in the subject's derivative
  folder, including the CT copy and FreeSurfer's `T1`: pick the matching MR
  image. When the list is empty (e.g. after a DICOM import), the MRI chosen in
  MRI Preprocessing is used.
* **Program**: `YAEL-reg3d` (the default; built into RAVE, rigid only:
  "Registration type" `Rigid`, `DenseRigid` or `FastRigid`, "Cost function" MI
  or CC, "Interpolation"), `ANTs` (uses RAVE's Python), `NiftyReg` (always
  available), `img_pipe` (uses RAVE's Python), `AFNI` or `FSL` (need those
  programs and a `bash` terminal; on Windows, WSL2). Each program shows its
  own parameters; the defaults use rigid registration. Recommended order:
  ANTs ≥ YAEL-reg3d > FSL > NiftyReg > the rest; YAEL-reg3d first when RAVE's
  Python is not configured (rule 2).
* "Run from RAVE" runs it; "Save & run by yourself" saves the script and shows
  the terminal command.

**Generate Reports** (footer button): choose one or more reports in the
"Generate reports" dialog, then click "Generate reports". They are built in the
background.

### 3. Outputs

Everything is written into the subject's imaging folder (`subject$imaging_path`
in R):
* **Imported images** (`inputs/MRI`, `inputs/CT`), with copies in `derivative`.
* **ACPC re-alignment**: `MRI_acpc.nii.gz` (in `inputs/MRI` and `derivative`)
  and the scanner-to-ACPC transform.
* **MRI preprocessing**: the FreeSurfer folder (`fs`) with surfaces and the
  normalized `T1`, or the YAEL normalization.
* **Co-registration**: the CT resampled into MRI space and the CT-to-MRI
  transforms (`coregistration`). Electrode Localization reads these to place
  the electrodes. YAEL-reg3d saves the CT (`CT_RAW.nii.gz`) and the MRI
  (`MRI_reference.nii.gz`) at their original resolution.
* **Reports**: "Coregistration Quality Report" (CT overlaid on the MRI, for
  checking the alignment) and "Normalization Quality Report" (the subject's MRI
  against the MNI templates; needs YAEL normalization).
* **Logs and scripts**: `log` and `scripts` folders, one file per run.

## Common procedures

These recipes reuse the step-by-step guide.

### Procedure — Prepare a new subject for electrode localization

Step 1: Load the subject, choosing the raw MRI and CT ([Load data](#1-load-data)).
Step 2: In "Import DICOM Folders or Nifti Images", click "Run from RAVE (T1
MRI)", then "Run from RAVE (CT)". Check each log.
Step 3 (optional): ACPC re-alignment; then follow
[Procedure — After ACPC re-alignment](#procedure--after-acpc-re-alignment).
Steps 4 and 5 may run in either order, or at the same time (e.g. start the
terminal run of step 4, then do step 5 in RAVE).
Step 4: In "MRI Preprocessing", click "Refresh", choose the imported MRI
(`MRI_RAW`) and the command by rule 4 (`YAEL+recon-all`, or `YAEL-native+recon-all`
when `imaging_status` shows `ants=not available`), then "Save & run by
yourself" and run the command in a terminal
([Procedure — Run a long step in a terminal](#procedure--run-a-long-step-in-a-terminal)).
Step 5: In "Co-registration CT and T1", click "Refresh", choose the CT, the
same MR image as in step 4 (`MRI_RAW`), and the program by rule 2 (`ANTs`, or
`YAEL-reg3d` when `imaging_status` shows `ants=not available`), then "Run from
RAVE".
Step 6: Check the alignment ([Procedure — Check the co-registration](#procedure--check-the-co-registration)),
then open the Electrode Localization module.

### Procedure — After ACPC re-alignment

Step 1: Click "Refresh" in "MRI Preprocessing" and in "Co-registration CT and
T1".
Step 2: Choose `MRI_acpc.nii.gz` as "MRI file" in MRI Preprocessing and as "MR
image" in co-registration. Do not use the other images (including FreeSurfer's
`T1`): ACPC re-alignment changes the image's orientation matrix, so an ACPC
image and a non-ACPC image never belong together.

### Procedure — Run a long step in a terminal

Step 1: Set the card's inputs, then click "Save & run by yourself". The notice
shows where the script was saved and the one-line command.
Step 2: Open a terminal on the computer where RAVE runs, paste the command, and
keep the terminal open until it ends ("Done." at the end of the output).
Step 3: Back in RAVE, click "Refresh" in the cards that use the new files.

### Procedure — Check the co-registration

Step 1: After co-registration, click "Generate Reports", choose "Coregistration
Quality Report", and click "Generate reports".
Step 2: When the "Report generated!" notice appears, click "View report". The
CT's skull and electrode contacts should line up with the MRI. If not, run
co-registration again with other settings or another MR image.

### Procedure — Choose between FreeSurfer and YAEL-only

Step 1: Find the FreeSurfer version (the `build-stamp.txt` file in the
"FreeSurfer home" folder names it; the folder name often does too) and the
computer's memory.
Step 2: FreeSurfer 7.x needs 16 GB of RAM, 8.x needs 32 GB. With enough memory,
use `YAEL+recon-all`; otherwise switch FreeSurfer versions, or use `YAEL-only`.
When `imaging_status` shows `ants=not available`, use `YAEL-native+recon-all`
and `YAEL-native-mni152b` (or `YAEL-native-mni152a`) in their place.

## Caveats

* **Import once.** Importing another image later invalidates every result
  built on the earlier one; you must preprocess and co-register again. A DICOM
  import replaces the previously imported images of that type, including
  `MRI_acpc.nii.gz`.
* **RAVE does not know whether a command worked.** Read the log at the end of
  each run; errors from FreeSurfer, ANTs or YAEL-reg3d appear only there.
* **Lists do not refresh by themselves** after a run: click "Refresh" (or the
  refresh button in the ACPC card).
* **FreeSurfer continues an existing folder.** If the subject already has a
  FreeSurfer folder, `recon-all` works on it and ignores the chosen MRI unless
  "Remove existing work before running the command" is ticked, which deletes
  it.
* **A new co-registration replaces the old one.** Electrode positions
  localized on the old one may then need checking.
* **Long runs belong in a terminal.** A run started from RAVE stops when RAVE
  closes.
* **Command-line paths are global.** A path entered in the loader is saved to
  RAVE's settings for every subject.
* **First runs may install software.** ANTs co-registration installs ANTs when
  it is missing; img_pipe sets up its Python packages; YAEL downloads its
  templates. YAEL-reg3d installs nothing.
* **Memory.** YAEL's default MNI template needs 16 GB of RAM (it falls back to
  another template otherwise); FreeSurfer 8.x needs 32 GB. The native engine
  needs 32 GB for the 0.5 mm templates: on smaller computers, use
  `YAEL-native-mni152a`.
* **AFNI ignores "MR image"**: it always uses the MRI chosen in MRI
  Preprocessing.

## Run the pipeline without the UI

The pipeline builds the command scripts; running a script is a separate step.
The UI writes its inputs into `settings.yaml`: `path_mri` and `path_ct` are
relative to the subject's raw folder, and the cards' inputs go into `params`
(file names are relative to the imaging folder's `inputs/MRI`, `inputs/CT`,
and `derivative`).

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("yael_preprocess")

# Set inputs
pipeline$set_settings(
  project_name = "YAEL",                       # project
  subject_code = "yael_demo_001",              # subject
  path_mri = "rave-uploads/MRI/MRI_reference.nii.gz",  # raw MRI (file or DICOM folder)
  path_ct = "rave-uploads/CT/CT_RAW.nii.gz",           # raw CT
  params = list(
    nii_t1 = "MRI_RAW.nii.gz",                 # "MRI file" (inputs/MRI)
    nii_ct = "CT_RAW.nii.gz",                  # "CT file" (inputs/CT)
    dcm2niix = list(merge = "Auto", float = "Yes", crop = "No"),
    segmentation = list(program = "YAEL+recon-all", fresh_start = FALSE),
    # "MR image" (derivative); YAEL-reg3d, ANTs and NiftyReg read it from `nipy`
    nipy = list(reference = "MRI_RAW.nii.gz"),
    reg3d = list(reference = "MRI_RAW.nii.gz", reg_type = "Rigid",
                 cost = "mattes", interp = "trilinear"),
    ants = list(reference = "MRI_RAW.nii.gz", reg_type = "Rigid",
                aff_metric = "mattes", syn_metric = "mattes")
  )
)

# Upstream targets the module evaluates with every command
upstream <- c("settings_path", "settings", "params", "subject",
              "cmd_tools", "check_result")

# Build and run the imports. Each command has `script`, `script_path`,
# `log_file`, and `execute()`; a failed one has `error = TRUE` and `condition`
cmds <- pipeline$eval(names = c(upstream, "import_T1", "import_CT"))
cat(cmds$import_T1$script)
cmds$import_T1$execute()
cmds$import_CT$execute()

# Build the next commands once the imported files exist
cmds <- pipeline$eval(names = c(upstream, "image_segmentation", "coreg_ants",
                                "coreg_reg3d"))
cmds$coreg_ants$execute()   # ANTs co-registration (cmds$coreg_reg3d: YAEL-reg3d)

# A long step: save its script and get the one-line terminal command
cmds$image_segmentation$execute(dry_run = TRUE, backup = FALSE)
```

## Drive the module with MCP tools

Operate the live module the way a person does. Call tools as
`tool("tool__NAME", arg = value)`. A script's reply has `result` (its return
value) and `output` (what it printed). Follow the [rules](#rules-for-agents-read-first).

Script order: `load_data`, click "Proceed", `imaging_status`, then imports,
ACPC (coached, when used), then MRI preprocessing (saved script) and
co-registration in either order, then reports.

### Load data

* Set the subject: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "YAEL")`,
  then `inputId = "loader_subject_code"`.
* Only for a first import, choose the images: `inputId = "mri_path"` and
  `inputId = "ct_path"`. Ask the user which DICOM folder or NIfTI file in the
  subject's raw folder to use: agents cannot read the dropdown's choices, and
  `tool("tool__shiny_input_info", inputIds = list("mri_path", "ct_path"))`
  shows only the preselected value (`current_value`). `Rscript subject_info.R <project/subject>`
  shows whether a native MRI is imported already. Never `"[Upload]"`.
* `tool("tool__module_interactive_script_run", name = "load_data")`: its output
  lists the to-do items, warnings, and program paths. Tell the user the
  warnings.
* Check that the dialog is open (`tool("tool__shiny_query_ui", css_selector = "#yael_preprocess-loader_ready_btn2")`),
  then click "Proceed": `tool("tool__shiny_ui_operate", action = "click", target = "loader_ready_btn2")`.
* `tool("tool__module_interactive_script_run", name = "imaging_status")`: what
  is already imported, re-aligned, reconstructed, and co-registered, the
  programs found, the FreeSurfer version, and the computer's memory.

### Configure and run

* Open a card: `tool("tool__shiny_input_update", inputId = "jump_coregistration", value = "1")`
  (also `jump_import_dicom`, `jump_acpc_realign`, `jump_recon`). The value is
  ignored: the link is clicked.
* Imports (only if never imported; ask first): scripts `import_t1`,
  `import_ct`. Poll `command_status` until it reports the job finished, read
  the log tail, then close the log dialog:
  `tool("tool__shiny_ui_operate", action = "click", target = "dismiss_modal")`.
* Refresh the file lists: `tool("tool__shiny_input_update", inputId = "param_fs_refresh", value = "1")`
  (also `param_coreg_refresh`, `param_acpc_refresh`; the value is ignored).
* ACPC (coach the person): set `param_acpc_infile`, open the card
  (`jump_acpc_realign`), run `start_acpc_viewer`, then guide the person; read
  progress from `#yael_preprocess-acpc_results` with `shiny_query_ui`, or the
  viewer (`outputId = "acpc_3dviewer"`) with `rave_3dviewer_get`. After the
  person confirms: script `save_acpc`; close the "Done!" alert with
  `shiny_ui_operate` action `close_alert2`.
* MRI preprocessing: set `param_fs_infile` (`MRI_RAW`, or `MRI_acpc.nii.gz`
  with ACPC) and `param_fs_prog`, wait a moment, then script
  `save_mri_preprocessing_script`. Check `program` and `mri` in its output
  (the module rebuilds the script 0.3 s after an input change); `command` is
  the terminal command (rules 3 and 4). Do not run it when `fresh_start` is
  TRUE. With the box unticked, FreeSurfer continues an existing folder and YAEL
  keeps an existing normalization: when `imaging_status` shows either, ask the
  user first. A `freesurfer_folder` with `reconstruction=none` was not made by
  RAVE's recon-all, so ask the user where it came from: a folder from
  `simple-import` cannot be continued and needs "Remove existing work"; a
  FreeSurfer folder made elsewhere must not be removed.
* Co-registration (ask first): set `param_coreg_ct`, `param_coreg_mri` (the
  same image as `param_fs_infile`), `coreg_ct_program` (`ANTs`, `YAEL-reg3d`
  or `NiftyReg`, in the order of rule 2) and its parameters (`coreg_ants_type`, `coreg_ants_aff_metric`,
  `coreg_ants_syn_metric`; `coreg_reg3d_type`, `coreg_reg3d_cost`,
  `coreg_reg3d_interp`; `coreg_niftyreg_type`, `coreg_niftyreg_interp`),
  then script `run_coregistration`; poll `command_status`. Other programs:
  script `save_coregistration_script`. It may run while MRI preprocessing runs
  in a terminal.
* Reports (no need to ask): scripts `generate_coregistration_report` and
  `generate_normalization_report`. They run in the background; the report is
  ready when `imaging_status` shows "report.html ready" for a newer folder.
  People use "Generate Reports" (script `run_analysis`, a dialog with
  `_report_label` and `_report_generate`).

### Inspect results

* `imaging_status` after each step.
* Previews and script panels are plain outputs: open their card first, then
  read `#yael_preprocess-mri_preview`, `#yael_preprocess-ct_preview`,
  `#yael_preprocess-panel_coreg` with `shiny_query_ui`.
