# Wavelet Module Reference

The Wavelet module runs the Morlet (continuous) wavelet transform on a subject's
Notch-filtered iEEG/LFP voltage signals, producing the time-frequency **power**
and **phase** that downstream RAVE 2.0 modules (e.g. Power Explorer) analyze. It
also back-ports the results so that RAVE 1.0 modules can read them.

**Prerequisite:** Apply the **Notch Filter** module (`notch_filter`) first. The
module only loads electrodes whose signals are Notch-filtered (LFP, EKG, and
Audio channels); if there are none, it refuses to load the subject.

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
Step 2: Click **"Load subject"**. The module reads the Notch-filtered
electrodes and their sample rates, then shows the main panel. If loading fails,
the most common cause is that the Notch filter was never applied.

If the subject already has a wavelet from this module, loading restores that
run's power sample rate, down-sample factor, and precision into the inputs.

### 2. Analysis inputs

The wavelet settings are in the **"Wavelet settings"** card on the left.

**Basic configurations**

* **Power sample rate (Hz)** (`target_sample_rate`) — the sample rate that the
  wavelet coefficients are down-sampled to before saving. Default `100`. Must be
  greater than `1`.
* **Down-sample before wavelet** (`pre_downsample`) — factor applied to the
  voltage before the wavelet, to reduce computation. The choices are powers of
  two derived from the subject's sample rate and the power sample rate; `1`
  means no down-sampling. Changing the power sample rate resets the choices.
* **Use single float precision to speed up** (`precision`) — when checked, the
  wavelet is computed in single (float) precision, which is faster; unchecked
  (default), in double precision.

**Frequency & cycle**

* **Select method to generate wavelet parameters** (`use_preset`) —
  `Builtin tool` (default) builds the frequency and cycle table from the three
  sliders below; `Upload preset` reads a CSV file instead.
* **Frequency range** (`freq_range`) — lowest and highest frequency of the
  table, e.g. `[2, 200]` Hz. Once the subject is loaded, the slider stops at the
  Nyquist frequency of the down-sampled signal: sample rate / 2 / down-sample
  factor.
* **Frequency step size** (`freq_step`) — spacing between frequencies, e.g. `2`
  Hz gives 2, 4, 6, ... Hz.
* **Wavelet cycles** (`cycle_range`) — number of Morlet cycles at the lowest and
  highest frequency, e.g. `[3, 20]`. In between, log(cycles) grows linearly with
  log(frequency), rounded to whole cycles. Fewer cycles give better time
  resolution; more cycles give better frequency resolution. The lower value must
  be at least `2`.
* **Upload** (`preset_upload`, only when `use_preset` is `Upload preset`) — a
  CSV file with the columns `Frequency` and `Cycles`, one row per frequency.

Click **"Run wavelet"** to continue. It does not run the wavelet yet: it checks
the inputs, builds the kernel table, and opens a **confirmation dialog** that
lists the subject, frequencies, cycle counts, precision, and the steps that will
run. The dialog has three buttons:

* **"Confirm"** runs the wavelet in the RAVE app's own R process. The whole app
  waits until the wavelet finishes.
* **"Confirm and run in background"** runs the wavelet in a separate R process.
  The app stays responsive (e.g. in other browser tabs), while this tab shows a
  progress alert until the wavelet finishes.
* **"Cancel"** closes the dialog.

When the wavelet finishes, an alert says "Done! Please feel free to close this
dialogue".

### 3. Outputs

* **Wavelet kernel — figure** (`kernel_plot`) — the Morlet wavelet kernels for
  the chosen frequencies and cycles, at the subject's LFP sample rate. Use it to
  check the time-frequency trade-off before running.
* **Wavelet kernel — table** (`kernel_table`) — the `Frequency` / `Cycles`
  table that will be used. Double-click the card (or use the flip tool) to
  switch between the figure and the table. **Download kernel parameters** saves
  it as a CSV file.

## Common procedures

Short recipes for the most common tasks.

### Procedure — Run a standard wavelet (2-200 Hz)

Step 1: Load the subject (see [Step 1](#1-load-data)).
Step 2: Set **Power sample rate** to `100`, **Down-sample before wavelet** to
`1`, and leave **precision** unchecked.
Step 3: Keep `use_preset` at **Builtin tool**; set **Frequency range** to
`[2, 200]`, **Frequency step size** to `2`, and **Wavelet cycles** to `[3, 20]`.
Step 4: Click **"Run wavelet"**, review the confirmation dialog, and click
**"Confirm"** (or **"Confirm and run in background"**).

### Procedure — Speed up a large subject

Steps 1-3: reuse [Procedure — Run a standard wavelet](#procedure--run-a-standard-wavelet-2-200-hz).
Step 4: Check **Use single float precision**, and/or set **Down-sample before
wavelet** to `2` to halve the sample rate before the wavelet.
Step 5: Click **"Run wavelet"**, then **"Confirm and run in background"** so the
app stays responsive while the wavelet runs.

### Procedure — Use a custom frequency/cycle table

Step 1: Load the subject.
Step 2: Set `use_preset` to **Upload preset** and upload a CSV file with the
columns `Frequency` and `Cycles`.
Step 3: Check the table in the **Wavelet kernel** output, then click **"Run
wavelet"** and confirm.

## Caveats

* **Notch first.** The module cannot load a subject until its signals are
  Notch-filtered by the **Notch Filter** module (`notch_filter`).
* **Applying overwrites.** Either Confirm button first deletes the previous
  wavelet files of the loaded electrodes, then writes power, phase, and voltage
  files for each electrode, `meta/frequencies.csv`, and
  `meta/reference_noref.csv`. It also deletes `meta/time_points.csv`, clears
  cached data, regenerates the subject's common-average reference signals
  (`ref_*` of more than one channel), and replaces
  `[subject]/pipelines/wavelet_module` after backing it up. A failed run leaves
  those electrodes without wavelet results.
* **Power sample rate must exceed 1.** `target_sample_rate <= 1` is rejected.
* **Frequency ceiling.** The top of **Frequency range** cannot exceed the
  Nyquist frequency of the (down-sampled) signal; the slider caps itself after
  the subject loads and whenever the down-sample factor changes.
* **Cycles.** The pipeline rejects any cycle count of `1` or less, so the lower
  value of **Wavelet cycles** must be at least `2`.
* **Long-running.** The wavelet can take a long time. "Confirm" makes the whole
  app wait; "Confirm and run in background" keeps the app responsive, but this
  tab shows the progress alert until the wavelet finishes.

## Run the pipeline without the UI

The UI turns the three frequency and cycle sliders into an explicit kernel
table (`kernel_table`, with columns `Frequency` and `Cycles`) before it writes
`settings.yaml`. The code below builds the same table.

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("wavelet_module")

# The kernel table that the "Builtin tool" builds: log(cycles) grows linearly
# with log(frequency), then cycles are rounded
freq_range  <- c(2, 200)   # Frequency range (Hz)
freq_step   <- 2           # Frequency step size (Hz)
cycle_range <- c(3, 20)    # cycles at the lowest and highest frequency
frequency <- seq(freq_range[1], freq_range[2], by = freq_step)
cycles <- round(exp(
  (log(cycle_range[2]) - log(cycle_range[1])) /
    (log(freq_range[2]) - log(freq_range[1])) *
    (log(frequency) - log(freq_range[1])) + log(cycle_range[1])
))

# Set inputs
pipeline$set_settings(
  project_name       = "demo",          # RAVE project
  subject_code       = "DemoSubject",   # subject (must be Notch-filtered)
  kernel_table       = data.frame(Frequency = frequency, Cycles = cycles),
  target_sample_rate = 100,             # power sample rate (Hz), > 1
  pre_downsample     = 1,               # down-sample factor before the wavelet
  precision          = "double"         # "double" or "float"
)

# Check the kernel table, then run the wavelet and save it to the subject
pipeline$run("kernels")
pipeline$run("wavelet_params")

# What the Confirm buttons do next: clear cached data, then save a copy of the
# pipeline into the subject, backing up the previous copy
pipeline$run(names = c("subject", "clear_cache"), shortcut = TRUE)
subject <- pipeline$read("subject")
fork_path <- file.path(subject$pipeline_path, pipeline$pipeline_name)
if (file.exists(fork_path)) {
  ravecore::backup_file(fork_path, remove = TRUE)
}
pipeline$fork(fork_path)
```

## Drive the module with MCP tools

Operate the live module the way a person does, following the step-by-step
guide. Each step names an MCP tool; call it as `tool("tool__NAME", arg =
value)`. The module registers three interactive scripts: `load_data`,
`run_analysis` (the "Run wavelet" button), and `pipeline_progress`
(read-only). The wavelet itself starts only from the confirmation dialog's
buttons, which you click with `shiny_ui_operate`: the same code runs as when a
person clicks them.

**Run the steps in this exact order: `load_data`, set the inputs,
`run_analysis`, ask the user, click a Confirm button, then follow the run until
it is done.** Only `load_data` works before the data are loaded.

A script's reply has `result` (its return value) and `output` (what it printed
while it ran). `run_analysis` finishes even when it fails, so always read its
`output`.

### Load data

* Set the project: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`
* Set the subject: `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "DemoSubject")`
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`. The
  `result` names the Notch-filtered electrodes and the sample rate.

### Configure, then validate, then run

1. Inspect the inputs: `tool("tool__shiny_input_info")`
2. Keep the builtin generator: `tool("tool__shiny_input_update", inputId = "use_preset", value = "Builtin tool")`
   (`Upload preset` needs a CSV upload, which an agent cannot do.)
3. Set the power sample rate first: `tool("tool__shiny_input_update", inputId = "target_sample_rate", value = "100")`
   (it resets the `pre_downsample` choices).
4. Set the down-sample factor: `tool("tool__shiny_input_update", inputId = "pre_downsample", value = "1")`
5. Set the precision: `tool("tool__shiny_input_update", inputId = "precision", value = "false")`
6. Set the frequency range: `tool("tool__shiny_input_update", inputId = "freq_range", value = "[2, 200]")`
7. Set the frequency step: `tool("tool__shiny_input_update", inputId = "freq_step", value = "2")`
8. Set the cycles: `tool("tool__shiny_input_update", inputId = "cycle_range", value = "[3, 20]")`
9. **Check the inputs and open the dialog (required, do not skip):**
   `tool("tool__module_interactive_script_run", name = "run_analysis")`. Read
   its `output`:
   * "Error found! There are some invalid inputs...": an input is invalid,
     e.g. `target_sample_rate` of 1 or less. Check the values with
     `shiny_input_info`.
   * "kernels errored": the pipeline rejected the kernel table. Run
     `tool("tool__module_interactive_script_run", name = "pipeline_progress")`;
     its `kernels` line gives the reason.
   * "kernels completed": the dialog opens right after the call. Check that it
     is open, `tool("tool__shiny_query_ui", css_selector = "#wavelet_module-wavelet_confirm_btn")`,
     and show the user what it lists: `tool("tool__shiny_query_ui", css_selector = ".modal-body")`.

   People see errors as a notification. Remove it with
   `tool("tool__shiny_ui_operate", action = "remove_notification", target = "wavelet_module-error_notif")`.
10. **Ask the user before starting the wavelet**: it is slow and overwrites the
    subject's previous wavelet results (see [Caveats](#caveats)). The user can
    also click the dialog's buttons themselves.
11. Start it in the background: `tool("tool__shiny_ui_operate", action = "click", target = "wavelet_confirm_btn2")`
    ("Confirm and run in background"). Click "Confirm" (`target = "wavelet_confirm_btn"`)
    only if the user asks: it runs in the app's R process, so the app, and your
    next tool call, wait until the wavelet finishes.
12. If the user declines, close the dialog: `tool("tool__shiny_ui_operate", action = "dismiss_modal")`.

### Inspect results

* Kernel figure and table: `tool("tool__shiny_output_result", outputId = "kernel_plot")`
  and `tool("tool__shiny_output_result", outputId = "kernel_table")`.
* Follow a run: `tool("tool__module_interactive_script_run", name = "pipeline_progress")`,
  e.g. every 30 seconds. It shows `wavelet_params: dispatched` while the
  wavelet runs, then `completed` (or `errored`, with the reason), then
  `subject` and `clear_cache` while the module finishes saving.
* Read the alert: `tool("tool__shiny_query_ui", css_selector = ".swal-overlay--show-modal .swal-modal")`.
  * "Done! Wavelet has finished. Trying to make data compatible with RAVE 1.0
    modules...": still saving; keep polling.
  * "Done! Please feel free to close this dialogue": finished and saved.
  * "Wavelet done, but...": the wavelet is saved, but the RAVE 1.0 back-port
    failed; the message says why.
  * "Errors": the wavelet failed; the message says why.

  The final alert stays until someone clicks its button:
  `tool("tool__shiny_ui_operate", action = "click", target = ".swal-overlay--show-modal .swal-button")`.
* Tell the user something in the app: `tool("tool__shiny_ui_operate", action = "show_notification", message = "...")`;
  remove it with `tool("tool__shiny_ui_operate", action = "remove_notification", target = "wavelet_module-agent_notification")`.
