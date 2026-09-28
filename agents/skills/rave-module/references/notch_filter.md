# Notch Filter Module Reference

The Notch Filter module removes electric line noise (typically 60 Hz in the
Americas or 50 Hz elsewhere, plus its harmonics) from imported iEEG/LFP voltage
signals. It writes the cleaned signals back to the subject and shows before/after
Welch periodograms so you can confirm the noise peaks are gone.

**Prerequisite:** Import the subject's raw signals first with the **Import Signal
Data** module (`import_signals`). At least one LFP electrode must be imported
before this module can load the subject. (`import_signals` supersedes the legacy
"Native Standard" / `import_lfp_native` and "BIDS Standard" / `import_bids`
importers.)

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: In the loader screen, choose the RAVE **Project** and **Subject** you
want to filter. (Optional: use "Sync from ..." to copy the project/subject from a
recently used module.)
Step 2: Click **"Load subject"**. The module loads the imported electrodes and
shows the main panel. If loading fails, the most common cause is that the signals
were never imported (see Prerequisite).

### 2. Analysis inputs

The filter settings are in the **"Frequencies and bandwidths"** card on the left:

* **Base frequency (Hz)** — the fundamental line-noise frequency. Default `60`
  (Americas). Use `50` for Europe/Africa/most of Asia.
* **x (Times)** — comma-separated multiples of the base frequency to remove, i.e.
  which harmonics. Default `1,2,3` removes 60, 120, and 180 Hz.
* **+- Bandwidth (Hz)** — comma-separated half-widths, one per multiple. Default
  `1,2,2` gives ±1 Hz at 60 Hz, ±2 Hz at 120 Hz, ±2 Hz at 180 Hz. Must have the
  same length as "x (Times)".
* **Additional channel types** — optionally also filter `Spike` or `Auxiliary`
  channels. LFP macro-channels are always filtered and cannot be removed here.
  Adding other types is **discouraged**: in almost all cases you should notch
  only LFP macro-channels, so leave this empty unless you have a specific reason.

Below the inputs, a live **preview** lists the exact bands that will be removed
(e.g. "Filter 1: 59.0Hz - 61.0Hz"). Check it before applying.

Click **"Apply Notch filters"** to run. This does not filter immediately: it
first validates the settings and opens a confirmation dialog listing the subject,
the electrode channels, and the frequency bands. Click **"Confirm"** to apply and
save, or **"Cancel"** to go back.

The **"Inspection"** card controls what the diagnostic plot shows (it does not
change the filter):

* **Block** / **Electrode** — pick which recording block and electrode to inspect.
* **Previous / Next** — step through electrodes.
* **Window length (seconds)** — Welch periodogram window. Default `2`.
* **Frequency limit** — maximum frequency shown. Default `300` Hz.
* **Number of histogram bins** — FFT histogram bins. Default `60`.

### 3. Outputs

* **Notch - Inspect signals** (`signal_plot`) — a multi-panel diagnostic for the
  selected block/electrode: the full voltage trace over time, two Welch
  periodograms (linear and log-frequency) overlaying the **Original** (black) and
  **Filtered** (red) spectra, and a voltage histogram. After filtering, the red
  curve should dip at the base frequency and its harmonics while the rest of the
  spectrum tracks the black curve. Use Block/Electrode and the Previous/Next
  buttons to spot-check several channels.
* **Download as PDF** — exports the diagnostic periodograms for every electrode
  and block to a PDF.
* **Diagnostic report** — after a successful apply, a "Notch Filter Diagnostic
  Plots" report is generated in the background; open it from the module header's
  report menu.

## Common procedures

Short recipes for the most common tasks.

### Procedure — Apply a standard 60 Hz notch (Americas)

Step 1: Load the subject (see [Step 1](#1-load-data)).
Step 2: Set **Base frequency** = `60`, **x (Times)** = `1,2,3`, **+- Bandwidth** =
`1,2,2` (removes 60/120/180 Hz).
Step 3: Confirm the preview bands, click **"Apply Notch filters"**, review the
dialog, and click **"Confirm"**.
Step 4: Inspect a few electrodes in the plot to confirm the peaks are gone.

### Procedure — Apply a 50 Hz notch (Europe / Asia)

Steps 1-4: reuse [Procedure — Apply a standard 60 Hz notch](#procedure--apply-a-standard-60-hz-notch-americas),
but set **Base frequency** = `50` (removes 50/100/150 Hz).

### Procedure — Inspect a specific electrode/block

Step 1: In the **Inspection** card, choose the **Block** and **Electrode**.
Step 2: Adjust **Window length**, **Frequency limit**, and **Number of histogram
bins** to zoom in on the noise bands.
Step 3: Use **Previous / Next** to compare neighboring electrodes.

### Procedure — Export the diagnostic PDF

Step 1: Set the Inspection parameters you want reflected in the plots.
Step 2: Click **"Download as PDF"**. The file is named
`{project}-{subject}-Notch_filter_diagnostic_plots.pdf`.

### Procedure — Re-run the filter with different settings

Step 1: Change the filter inputs.
Step 2: Click **"Apply Notch filters"** and **"Confirm"** again. Re-running
overwrites the previously filtered signals (see Caveats).

## Caveats

* **Import first.** The module cannot load a subject until its raw LFP signals are
  imported via the **Import Signal Data** module (`import_signals`). The legacy
  `import_lfp_native` / `import_bids` importers also produce compatible data.
* **Notch only LFP channels.** LFP macro-channels are always filtered. Adding
  `Spike` or `Auxiliary` channel types is **discouraged** — in almost all cases
  you should notch only LFP macro-channels. Only include other types if you have
  a specific reason to filter them.
* **Bandwidth length must match Times.** "+- Bandwidth" needs exactly as many
  values as "x (Times)"; each lower bound must be below its upper bound.
* **Re-running overwrites.** Applying the filter again replaces the previously
  filtered data and updates the subject's `notch_filtered` flag. Filtering is not
  cumulative.
* **Match the frequency range to your analysis.** The defaults cover roughly
  0-200 Hz, which suits most analyses. For high-frequency oscillations (HFO), add
  higher harmonics so line noise is removed up to ~500 Hz.
* **Apply is destructive.** "Confirm" writes filtered signals into the subject's
  data directory. When driving via MCP, always ask the user before applying.

## Run the pipeline without the UI

The UI does not store the base frequency / times / bandwidth directly. It first
converts them to explicit band edges and writes those to `settings.yaml`:

```
center = base_freq * times
notch_filter_lowerbound = center - bandwidth
notch_filter_upperbound = center + bandwidth
```

So the UI defaults (base `60`, times `1,2,3`, bandwidth `1,2,2`) become
lowerbound `c(59, 118, 178)` and upperbound `c(61, 122, 182)`.

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("notch_filter")

# Set inputs (band edges are explicit, not base/times/bandwidth)
pipeline$set_settings(
  project_name = "test2",                     # RAVE project
  subject_code = "DemoSubject",               # subject to filter
  notch_filter_lowerbound = c(59, 118, 178), # lower edges (Hz): 60/120/180 Hz bands
  notch_filter_upperbound = c(61, 122, 182), # upper edges (Hz)
  channel_types = "LFP"                      # LFP is always filtered
)

# Apply the notch filters and write cleaned signals to the subject
apply_notch <- pipeline$run("apply_notch")

# Optional: generate the diagnostic periodograms (PDF)
diagnostic_plots <- pipeline$run("diagnostic_plots")
```

## Drive the module with MCP tools

Operate the live module as a user would, following the step-by-step guide. Each
step names an MCP tool; call it as `tool("tool__NAME", arg = value)`. The module
registers three interactive scripts: `load_data`, `run_analysis` (validate and
open the confirmation dialog), and `apply_notch_filter` (apply and save).

**Run these scripts in this exact order: `load_data` → `run_analysis` →
`apply_notch_filter`.** Only `load_data` is available until the data are loaded;
the other two need a loaded subject. Never skip `run_analysis`: it validates the
filter parameters and opens the confirmation dialog. `apply_notch_filter` will
still run if you skip `run_analysis`, but then the parameters are never reviewed
before they are written to the subject.

### Load data

* Set the project: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "test2")`
* Set the subject: `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "DemoSubject")`
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`

### Configure, then validate, then apply

1. Inspect the current inputs: `tool("tool__shiny_input_info")`
2. Set the base frequency: `tool("tool__shiny_input_update", inputId = "notch_filter_base_freq", value = "60")`
3. Set the harmonics (times): `tool("tool__shiny_input_update", inputId = "notch_filter_times", value = "1,2,3")`
4. Set the bandwidths: `tool("tool__shiny_input_update", inputId = "notch_filter_bandwidth", value = "1,2,2")`
5. **Validate (required, do not skip):** `tool("tool__module_interactive_script_run", name = "run_analysis")` —
   checks the parameters and opens the confirmation dialog.
6. **Apply and save (ask the user first):** `tool("tool__module_interactive_script_run", name = "apply_notch_filter")`

### Inspect results

* Render a registered output: `tool("tool__shiny_output_result", outputId = "signal_plot")`
* Change the inspected electrode: `tool("tool__shiny_input_update", inputId = "electrode", value = "14")`

Read any shidashi-registered output with `tool__shiny_output_result` (here,
`signal_plot`). The band preview and the "Download as PDF" link are plain UI
elements, not shidashi-registered outputs, so read the preview with
`tool("tool__shiny_query_ui", css_selector = "#notch_filter-notch_filter_preview")`.

---

For implementation details (pipeline targets, settings schema, data layout), read
the module source with the `rave-module` skill: `main.Rmd`, `R/module_server.R`,
and `make-notch_filter.R`.
