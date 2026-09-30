# Data Tools Module Reference

The Data Tools module (`compatibility_rave1`) checks a subject's data files for
missing, broken, or inconsistent data ("Data integrity check"), and exports
epoched power or voltage data as MATLAB files that MATLAB, Python, and R can
read ("Export data"). Its third card, "Backward compatibility", converts a
subject for RAVE 1.0 modules; only people use it.

**Prerequisite:** None: any subject loads, even an incomplete or broken one.
Exporting power needs the Wavelet module first; exporting voltage needs the
Notch filter.

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: In the loader screen, choose the RAVE **Project** and **Subject**.
(Optional: use "Sync from ..." to copy the project and subject from a recently
used module.)
Step 2: Click **"Load subject"**. Nothing is checked while loading, so
incomplete or broken subjects load too.

After loading, the three cards on the right are collapsed. Open one with the
**Quick access** links on the left ("Data integrity check", "Backward
compatibility", "Export data"): each link opens its card and collapses the
other two.

### 2. Analysis inputs

**Data integrity check**

* **Data version** (`validation_version`) — `2` (default) checks the RAVE 2.0
  data format. `1` also checks the referenced (`ref/`) copies of the voltage,
  power, and phase data that the RAVE 1.0 format keeps. It only matters in
  `normal` mode.
* **Validation mode** (`validation_mode`) — `basic` checks only small files:
  the subject folders, the preprocess settings, and the meta tables
  (electrodes, references, epochs). `normal` (default) also reads the voltage,
  power, and phase data, and checks every epoch and reference table against
  the data, so it takes longer.

Click **"Validate subject"** to run the checks. It writes nothing. In `normal`
mode the alert "Validation in progress..." stays until the checks finish. The
checks come in parts (see `ravecore::validate_subject`):

| Part | What it checks |
|---|---|
| `paths` | The subject's folders exist: the root and `rave` folders, the raw data folders, and the `data`, `meta`, `reference`, and `preprocess` folders. The cache, FreeSurfer, notes, and pipeline folders are low priority ("minor"). |
| `preprocess` | Electrodes and blocks are set; the sample rate is valid; every channel is imported; the LFP and EKG channels are Notch-filtered; the LFP channels have the wavelet; at least one reference and one epoch exist; `meta/electrodes.csv` exists. |
| `meta` | `electrodes.csv` matches the preprocess settings; each reference table lists every electrode and its reference data exist; each epoch table has the right format and lists only imported blocks. |
| `voltage_data` (normal) | The preprocessed and Notch-filtered voltage exist, can be read, and have consistent lengths. |
| `power_phase_data` (normal) | Power and phase exist for every LFP channel, with the frequencies and time points the preprocess settings expect. |
| `epoch_tables` (normal) | No trial onset is later than the end of its block's recording. |
| `reference_tables` (normal) | Each reference's data are valid: the file exists, with the expected frequencies and time points. |

**Backward compatibility** (people only)

* **"Make this subject RAVE 1.0 compatible"** (`compatibility_do`) — converts
  the loaded subject so RAVE 1.0 modules can read it
  (`ravecore::rave_legacy_subject_format_conversion`). It first validates the
  subject and stops unless its data are imported, Notch-filtered, and
  wavelet-transformed, with valid voltage, power, and phase data. Then it
  rewrites subject files: an entry in each channel's voltage, power, and phase
  files, `meta/time_points.csv`, `data/cache/cached_reference.csv`, and the
  preprocess settings `preprocess/rave.yaml` (backed up first). Only people
  click it; agents never run it.

**Export data**

* **Data type** (`export_type`) — `power` (default; the wavelet power),
  `voltage` (the Notch-filtered voltage), or `raw-voltage` (the voltage
  without any processing or reference). `power` needs the wavelet on at least
  one channel, and `voltage` the Notch filter on at least one channel.
  Choosing `raw-voltage` sets **Reference name** to `noref`.
* **Electrode channels** (`export_electrode`) — e.g. `14-15` or `1-5,8`;
  blank (default) exports all channels. Channels that the subject does not
  have are dropped. At least one channel must have a valid reference in
  **Reference name** (any channel, for `raw-voltage`).
* **Reference name** (`export_reference`) — one of the subject's references,
  e.g. `default` or `noref`, applied to the data before the export.
* **Epoch name** (`export_epoch`) — the epoch (table of trial onsets) that
  cuts the data into trials.
* **Pre-onset** (`export_pre`) and **Post-onset** (`export_post`) — the trial
  window in seconds around each onset. Pre-onset must be negative (default
  `-1`), and post-onset positive (default `2`).

An invalid input shows a message under it, e.g. "Please choose a negative
number" or "No valid electrode channels chosen".

* **"Generate exports"** writes the export into a new folder of the subject,
  `rave/exports/rave-repository/export-<yymmddTHHMMSS>/`, never overwriting
  an old one. The alert "Exporting repository..." shows while it runs, then
  "Success!" gives the folder's path. With an invalid input it stops and
  shows the notification "Please correct the inputs before exporting data".
* **"Export & download"** is how people get the export as a zip file through
  the browser.

### 3. Outputs

* **Validation results** (`validation_check`, in the "Data integrity check"
  card) — one line per check: `<what was checked>... valid: yes`, or
  `valid: no` or `valid: N/A (skipped)` followed by `reason: <why>`. The lines
  are coloured by status: passed; minor or skipped; failed. A skipped check
  could not run, usually because an earlier check failed.
* **Export folder** — `summary.yaml` (project, subject, channels, reference,
  epoch, time window, sample rates, and the dimension names and shape of the
  data), `electrodes.csv` (electrode table), `reference.csv` (reference
  table), `with_epochs/epoch.csv` (trial table), and one MATLAB file per
  channel, `with_epochs/<power|voltage|raw_voltage>/ch<NNNN>.mat` (e.g.
  `ch0014.mat`). Each file holds `data` (dimensions as named in
  `summary.yaml`, e.g. Frequency × Time × Trial × Electrode for power),
  `electrode`, `trial_number`, `time_in_secs`, `reference_channels`,
  `reference` (the reference signal for the same trials, or 0 without a
  reference), `description`, and, for power, `frequency`.

## Common procedures

Recipes for the common tasks; each builds on the step-by-step guide.

### Procedure — Check a subject after preprocessing

Step 1: Load the subject (see [Step 1](#1-load-data)).
Step 2: Open **Data integrity check** (Quick access). Keep **Data version**
`2` and **Validation mode** `normal`; `basic` is a fast first look at the
folders, settings, and meta tables.
Step 3: Click **"Validate subject"** and read the results. Each failed check
names the file or step with the problem.

### Procedure — Export power around an epoch

Step 1: Load the subject; its wavelet must be done.
Step 2: Open **Export data** (Quick access). Set **Data type** `power` first,
then e.g. **Electrode channels** `14-15`, **Reference name** `default`,
**Epoch name** `auditory_onset`, **Pre-onset** `-1`, and **Post-onset** `2`.
Step 3: Click **"Generate exports"**. The "Success!" alert gives the folder.

### Procedure — Check a subject converted for RAVE 1.0

Step 1: The user converts the subject with **"Make this subject RAVE 1.0
compatible"** (people only).
Step 2: Validate with **Data version** `1` and **Validation mode** `normal`.

## Caveats

* **Loading checks nothing.** Any subject loads; only "Validate subject"
  checks it.
* **Collapsed cards.** After loading, all three cards are collapsed. A
  collapsed card's results are not drawn until the card opens.
* **`normal` validation is slow** on large subjects: it reads all voltage,
  power, and phase data.
* **Data version `1`** changes only the `normal` checks of the voltage, power,
  and phase data.
* **Exports add up.** Every "Generate exports" writes a new folder. A power
  export holds frequencies × time points × trials for each channel, plus the
  reference signal for the same trials: channels 14-15 of `demo/DemoSubject`
  (16 frequencies, 301 time points, 287 trials, reference `default`) take
  42 MB.
* **Epoch tables must match the subject.** An epoch table that lists blocks
  the subject does not have fails its `meta` check.
* **RAVE 1.0 conversion is for people only.** The Wavelet module back-ports
  its results itself; when that fails, its alert "Wavelet done, but..." sends
  users to this module to validate the subject and convert it by hand.

## Run the pipeline without the UI

The pipeline has one target, `subject`, which loads the subject. The buttons
then call ravecore directly:

```r
# Load the subject (the only pipeline target)
pipeline <- ravepipeline::pipeline("compatibility_rave1")
pipeline$set_settings(
  project_name = "demo",          # RAVE project
  subject_code = "DemoSubject"    # subject
)
pipeline$run("subject")
subject <- pipeline$read("subject")

# "Validate subject": Data version 2, Validation mode normal. Each check has
# `valid` (TRUE, FALSE, or NA for skipped), `severity`, `description`, and
# `message`
results <- ravecore::validate_subject(
  subject = subject$subject_id, method = "normal", version = 2)
results$preprocess$has_wavelet$valid

# "Generate exports": power of channels 14-15, 1 s before to 2 s after each
# onset of epoch `auditory_onset`, with reference `default`
repository <- ravepipeline::with_rave_parallel({
  ravecore::prepare_subject_power_with_epochs(
    subject = subject,
    electrodes = c(14, 15),
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    reference_name = "default"
  )
})
export_folder <- repository$export_matlab()
```

For voltage, use `ravecore::prepare_subject_voltage_with_epochs()` with the
same arguments; for raw voltage, `ravecore::prepare_subject_raw_voltage_with_epochs()`
without `reference_name`.

## Drive the module with MCP tools

Operate the live module the way a person does. Each step names an MCP tool;
call it as `tool("tool__NAME", arg = value)`. The module registers four
interactive scripts: `load_data`, `run_analysis` (the "Validate subject"
button), `validation_results` (read-only), and `generate_exports` (the
"Generate exports" button). No script or tool converts a subject to RAVE 1.0:
only the user may do that.

**Run `load_data` first; then validate and export in any order.** Validation
writes nothing: run it without asking. **Always show the user the export
settings and get their confirmation before running `generate_exports`.**

A script's reply has `result` (its return value) and `output` (what it
printed while it ran).

### Load data

* Set the project: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`
* Set the subject: `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "DemoSubject")`
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`. The
  `result` lists the electrodes, the Notch-filtered ones and those with the
  wavelet, and the epoch and reference names (the export choices).

### Validate the subject

1. Show the user the card (optional): `tool("tool__shiny_input_update", inputId = "quickaccess_data_integrity", value = "1")`.
   The value is ignored: the link is clicked.
2. Set the data version: `tool("tool__shiny_input_update", inputId = "validation_version", value = "2")`
3. Set the mode: `tool("tool__shiny_input_update", inputId = "validation_mode", value = "normal")`
4. Check both: `tool("tool__shiny_input_info", inputIds = ["validation_version", "validation_mode"])`
5. Validate: `tool("tool__module_interactive_script_run", name = "run_analysis")`.
   Its `output` logs each check (`<what was checked>... valid: yes`, or
   `valid: no` and `reason: <why>`), but only the last 3000 characters: read
   the complete results in the next step.
6. Read the results: `tool("tool__module_interactive_script_run", name = "validation_results")`.
   The first line counts the checks by status; then one line per check that
   did not pass, `[failed|minor|skipped] <part>/<check>: <what was checked> - <reason>`;
   the last line lists the passed checks.

### Export data

1. Show the user the card (optional): `tool("tool__shiny_input_update", inputId = "quickaccess_export", value = "1")`
2. Set the data type first, since it resets the reference and epoch choices:
   `tool("tool__shiny_input_update", inputId = "export_type", value = "power")`
3. Set the rest:
   * `tool("tool__shiny_input_update", inputId = "export_electrode", value = "14-15")`
   * `tool("tool__shiny_input_update", inputId = "export_reference", value = "default")`
   * `tool("tool__shiny_input_update", inputId = "export_epoch", value = "auditory_onset")`
   * `tool("tool__shiny_input_update", inputId = "export_pre", value = "-1")`
   * `tool("tool__shiny_input_update", inputId = "export_post", value = "2")`
4. Check them: `tool("tool__shiny_input_info")`.
5. **Show the user the settings and get their confirmation.**
6. Export: `tool("tool__module_interactive_script_run", name = "generate_exports")`.
   The `result` is the export folder; tell the user. If an input is invalid,
   the call fails with "Please correct the inputs before exporting data" and
   nothing is written: check each input against its description
   (`shiny_input_info`). The rule it breaks shows under the input; read it
   with e.g.
   `tool("tool__shiny_query_ui", css_selector = ".shiny-input-container:has(#compatibility_rave1-export_pre)")`
   ("Pre-onset Please choose a negative number").
7. The "Success!" alert stays until the user closes it; you cannot close it.
   "Export & download" is for the user to click.

### Inspect results

* Validation: script `validation_results` (step 6 above). To look at the card
  itself, open it first (`quickaccess_data_integrity`); it then shows the
  latest results, even of a validation that ran while it was collapsed:
  `tool("tool__shiny_query_ui", css_selector = "#compatibility_rave1-validation_check")`.
* Export: the `generate_exports` result, or the alert:
  `tool("tool__shiny_query_ui", css_selector = ".swal-overlay--show-modal .swal-modal")`.

### RAVE 1.0 conversion

Never run it. The button `compatibility_do` is read-only for agents
(`shiny_input_update` refuses it), and no tool can click it. If the user asks,
open the card, `tool("tool__shiny_input_update", inputId = "quickaccess_compatibility", value = "1")`,
and ask them to click **"Make this subject RAVE 1.0 compatible"** themselves.
Afterwards, validate with `validation_version` `1`.
