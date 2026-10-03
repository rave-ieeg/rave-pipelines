# Voltage Explorer Module Reference

Voltage Explorer (module ID `voltage_explorer`) analyzes event-related
voltage. It filters the signals, aligns the trials to an event, averages them
by condition group, and estimates each electrode's canonical response (CRP),
with metrics such as amplitude, variance explained, signal-to-noise ratio,
t-statistic, duration, and onset. Results are shown as figures by electrode,
by trial, and for one channel, as a results table, and in a 3D viewer; an
HTML report can be saved into the subject.

**Prerequisite:** the subject needs Notch-filtered voltage (run the Notch
filter module first), an epoch, and a reference. Only LFP electrodes are
analyzed.

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: Choose the project and the subject.
Step 2: Choose the epoch and its trial window (seconds before and after the
event), the reference, and the electrodes to load (e.g. `13-16,24`).
Step 3: Click "Load subject".

### 2. Analysis inputs

Card "Signal Configurations" (the advanced fields show after clicking the
card's "show advanced" toggle). The signals go through these steps in order:
drift removal, down-sampling before the filters, the filters, down-sampling
after the filters, and the baseline.

* **Enable low/high/band-pass filter** — off by default. Then:
  * **Type** — `Low-pass`, `High-pass`, or `Band-pass`.
  * **Method** (advanced) — `FIR (least squares)` (default), `FIR (Kaiser)`,
    `FIR (Parks-McClellan)`, `IIR (Butterworth)`, `Chebyshev I`,
    `Chebyshev II`, or `Elliptic`.
  * **Cutoff freq (Hz)** — the cutoff of a low- or high-pass filter, or one
    edge of a band-pass; the second box (band-pass only) is the other edge.
    Cutoffs must lie between 0 and the Nyquist frequency after the first
    down-sampling. Example: band-pass `1` and `30`.
* **Enable band-stop filter** — off by default; **Stopband frequencies (Hz)**
  are ranges such as `59-61, 119-121, 179-181`. A range beyond the Nyquist
  frequency is skipped.
* **Inspect combined filter** — plots the frequency response of the enabled
  filters in a dialog.
* **Enable baseline correction** — on by default; **Baseline window
  (seconds)**, e.g. `-1` to `0`, within the trial window.
* **Remove drifts** (advanced) — `None`, `Detrend`, `Center (demean)`, or
  `Detrend + Center`.
* **Automatic down-sample** (advanced, before and after the filters) — on by
  default. The automatic factor before the filters keeps at least 3 times the
  highest cutoff (at least 300 Hz) of the sample rate, or 1000 Hz without a
  pass filter; after the filters it keeps at least 4 times the highest cutoff
  (at least 50 Hz), 200 Hz without a pass filter, and no down-sampling after
  a high-pass filter. Turn it off to type a **decimation factor**. The
  effective sample rates show under each box.
* **Reset to defaults** — a band-pass FIR (least squares) filter from 1 to 30
  Hz, no band-stop, baseline on, `Detrend + Center`, and automatic
  down-sampling.

Card "Condition Groups":

* **Analysis event** — `Trial Onset` or an event of the epoch; after the
  filters and the baseline, the trials are aligned so that this event is at
  time 0.
* **Group** (1 to 15) — a **Group label** and its **Conditions**. Loading data
  restores the saved groups, or one group "All Conditions".

Card "Electrode selector" — **electrode_text**: which electrodes the
by-electrode figures draw (e.g. `13-15`). Every loaded LFP electrode is
analyzed either way.

Card "CRP Parameters":

* **Response window (seconds)** — the window after the event in which the
  response duration is estimated; the response starts after its start and
  ends before its end. Default: 0.01 s (or the epoch start) to the end of the
  epoch.
* **Remove artifacts** — on by default: trials flagged as artifacts are left
  out when estimating the canonical response.
* **Onset detection border** (advanced) — the earliest time the onset scan may
  reach: `Earliest possible`, `Event onset (0 s)`, `Detection start
  (t_start)`, or `Disabled (no onset)` (the remembered default).
* **step** (advanced) — the step, in samples, between candidate durations;
  default 5.
* **Threshold (%)** (advanced) — the share of the peak mean projection used
  for the duration bounds; default 98.
* **Channel filter** — rows of *metric*, *operator* (`AND`/`OR`, applied left
  to right; the first is ignored), *criteria* (`v = T1`, `|v| < T1`,
  `|v| >= T1`, `v < T1`, `v >= T1`, `v in [T1, T2]`, `v not in [T1, T2]`), and
  *threshold* (`T1` or `T1, T2`). A metric is one results-table column such
  as `t_proj (A)`, or `all:<metric>` / `any:<metric>` (every / at least one
  group passes). **Send to electrode selector** writes the passing electrodes
  into the electrode selector and marks them in the viewer and the table.
* **Reset to defaults** — the remembered defaults of the advanced parameters,
  artifacts removed, and the full response window.

Card "Plot Options" (they apply at once, without re-running): the plotted
**time window** (start and end; empty means the whole epoch), **Rendering**
of the by-electrode figures (`Stacked lines` or `Heatmap`), **Condition
colors** and **Heatmap colors**, **Max** (a percentile of the absolute values
when **Max is %** is checked, default 99, otherwise µV), **Text size**,
**Channel** labels (`number`, `short`, `label`, `full`), **Sort trials by**
(`stimuli`: grouped by condition, or `trial`), **Onset mark (s)**, **Show CRP
decoration**, and **Scale canonical to µV**.

Click **Run Analysis** (footer) to run the analysis with these inputs.

### 3. Outputs

* **Overall › Results Viewer** — the 3D viewer: each electrode's CRP metrics
  per group; electrodes that fail the channel filter are hidden.
* **Overall › Results Table** — one row per electrode (number and label), one
  column per metric and group: `al_p` (mean amplitude, µV), `expl_var` (R²,
  variance explained), `SNR` (canonical vs. residual signal-to-noise),
  `t_proj` (t-statistic on trial projections), `tau` (response duration, s),
  and `onset` (response onset, s; absent when onset detection is disabled).
* **By Electrode** — `Mean Voltage` (per group, all electrodes), `Canonical
  Representations` (the canonical response per electrode and group), and both
  as overlays (one panel per electrode, the groups overlaid).
* **By Trial** — `α' by Electrode`, `SNR by Electrode`, and `R² by Electrode`:
  heatmaps of a CRP parameter by trial and electrode.
* **Single Channel Results** — for the channel chosen in the footer (or by
  double-clicking an electrode in the viewer): `Over Time` (every trial with
  the mean on top), `By Trial (Lines)`, and `By Trial (Heatmap)`.
* **Generate Report** (card "Export Configurations") — opens a dialog,
  pre-filled from the plot options and the electrode selector, whose
  "Generate report" writes an HTML report into the subject's reports folder
  in the background.

## Common procedures

### Procedure — compare the ERPs of two condition groups

Step 1: Load the subject.
Step 2: Turn on the band-pass filter, e.g. 1 to 30 Hz, and keep the baseline,
e.g. `-1` to `0`.
Step 3: Make two groups, e.g. "A" (`drive_a`, `known_a`, `last_a`, `meant_a`)
and "AV" (`drive_av`, `known_av`, `last_av`, `meant_av`).
Step 4: Click "Run Analysis"; compare the groups in "Mean Voltage (Overlay)",
and the metrics in the results table.

### Procedure — keep the electrodes with a strong response

Steps 1-4: reuse [Procedure — compare the ERPs of two condition groups](#procedure--compare-the-erps-of-two-condition-groups).
Step 5: Add a channel-filter row, e.g. `any:t_proj`, `|v| >= T1`, `3`.
Step 6: Click "Send to electrode selector": the figures, the table, and the
viewer keep the passing electrodes.

### Procedure — HTML report

Steps 1-4: as above. Set the plot options the report should use.
Step 5: Click "Generate Report", check the dialog, and click "Generate
report". A notification says when the report is ready, with a link.

## Caveats

* Invalid filter settings (a band-pass with one cutoff, a cutoff above the
  Nyquist frequency) stop the run before anything is computed; people see a
  red notification, "Invalid filter settings", that says what to change.
* After a run, the module collapses the card "Signal Configurations"; click
  its "+" to see the filters again (agents can set its inputs either way).
* The plot options (except the time window, the onset mark, and the CRP
  decoration) and the advanced CRP parameters are remembered as preferences,
  shared by every session of this module.
* Reset to defaults of the signal card sets a band-pass filter: it is not the
  "filters off" state the card starts with.
* With auto re-calculation on, changing the electrode selector or the groups
  runs the analysis again.
* The report dialog shows the current plot options; changing them in the
  dialog does not change the preferences.
* Only the report writes into the subject.

## Run the pipeline without the UI

The UI turns the signal card into a list `filter_configurations` (in
processing order: `detrend`/`demean`, `decimate` with `by`, the filters with
`high_pass_freq`/`low_pass_freq`, `decimate`, and `baseline` with `windows`),
and saves the groups (`condition_groups`), the event (`analysis_event`), and
the CRP inputs (`crp_detection_window`, `crp_remove_artifacts`,
`crp_time_step`, `crp_threshold_quantile`, `crp_onset_border`).

```r
pipeline <- ravepipeline::pipeline("voltage_explorer")

pipeline$set_settings(
  project_name = "demo", subject_code = "DemoSubject",
  epoch_choice = "auditory_onset",                   # epoch
  epoch_choice__trial_starts = -1, epoch_choice__trial_ends = 2,
  reference_name = "default", loaded_electrodes = "13-16,24",
  filter_configurations = list(                      # processing steps, in order
    list(type = "detrend"), list(type = "demean"),
    list(type = "decimate", by = 2),
    list(type = "firls", high_pass_freq = 1, low_pass_freq = 30),
    list(type = "decimate", by = 5),
    list(type = "baseline", windows = c(-1, 0))
  ),
  analysis_event = "Trial Onset",                    # time 0
  condition_groups = list(
    list(label = "A", conditions = c("drive_a", "known_a")),
    list(label = "AV", conditions = c("drive_av", "known_av"))
  ),
  crp_detection_window = c(0.01, 1),                 # response window (s)
  crp_remove_artifacts = TRUE
)

pipeline$run(c("crp_results", "erp_results_for_viewer", "data_by_channel_condition"))
metrics <- pipeline$read("erp_results_for_viewer")    # one row per electrode

# the module's plot of the mean voltage by group (a method in the pipeline's
# shared environment, so call it from there)
env <- pipeline$shared_env()
env$plot.data_by_channel_condition(pipeline$read("data_by_channel_condition"))
```

## Drive the module with MCP tools

Operate the live module the way a person does. Each step names an MCP tool;
call it as `tool("tool__NAME", arg = value)`. The module registers these
interactive scripts: `load_data` ("Load subject"), `run_analysis` ("Run
Analysis"), `inspect_filters`, `reset_signal_config`, `reset_crp_params`,
`reset_plot_options`, `send_to_electrode_selector`, `open_report_dialog`, and
the read-only `report_status` and `pipeline_progress`.

**Run `load_data` first, then set the inputs, then `run_analysis`.** Only
the report writes into the subject: **always confirm its settings with the
user first.**

A script's reply has `result` (its return value) and `output` (what it
printed while it ran).

### Load data

* `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`
* `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "DemoSubject")`
* `tool("tool__shiny_input_update", inputId = "loader_epoch_name", value = "auditory_onset")`,
  with `loader_epoch_name__trial_starts` (`"-1"`) and
  `loader_epoch_name__trial_ends` (`"2"`)
* `tool("tool__shiny_input_update", inputId = "loader_reference_name", value = "default")`
* `tool("tool__shiny_input_update", inputId = "loader_electrode_text", value = "13-16,24")`
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`. The
  `result` gives the trials, the time window, the LFP electrodes, the sample
  rate, the conditions with their trial counts, and the events.

Values round-trip through the browser: check them with
`tool("tool__shiny_input_info", inputIds = [...])`. A select whose choices
are still loading ignores the update; send it again.

### Configure and run

* Filters (set the switch first; its fields exist only while it is on):
  * `tool("tool__shiny_input_update", inputId = "passing_filter_enabled", value = "true")`
  * `tool("tool__shiny_input_update", inputId = "passing_filter_type", value = "band_pass")`
    (`low_pass`, `high_pass`, `band_pass`)
  * `tool("tool__shiny_input_update", inputId = "passing_filter_method", value = "firls")`
    (`firls`, `fir`, `fir_remez`, `iir`, `cheby1`, `cheby2`, `ellip`)
  * `tool("tool__shiny_input_update", inputId = "passing_freq1", value = "1")` and
    `passing_freq2` (`"30"`); an empty string (`""`) clears a cutoff
  * `bandstop_filter_enabled`, then `bandstop_filter_ranges` (`"59-61, 119-121"`)
  * `tool("tool__shiny_input_update", inputId = "remove_drift_method", value = "detrend+demean")`
    (`none`, `detrend`, `demean`, `detrend+demean`)
  * `enable_baseline_method`, then `baseline_window` (`"[-1, 0]"`)
  * `pre_downsample_factor_auto` / `post_downsample_factor_auto`; with
    `false`, `pre_downsample_factor` / `post_downsample_factor`
* Optional: `tool("tool__module_interactive_script_run", name = "inspect_filters")`
  opens the "Filter Inspector" dialog; picture it with
  `tool("tool__shiny_query_ui", css_selector = "#voltage_explorer-filter_inspector_plot")`
  and close it with `tool("tool__shiny_ui_operate", action = "dismiss_modal")`.
  With no filter enabled it fails with "Filter inspector will not launch".
* `tool("tool__shiny_input_update", inputId = "analysis_event", value = "Trial Onset")`
* `tool("tool__shiny_input_update", inputId = "condition_groups", value = "[{\"label\": \"A\", \"conditions\": [\"drive_a\", \"known_a\"]}, {\"label\": \"AV\", \"conditions\": [\"drive_av\", \"known_av\"]}]")`
* CRP: `crp_detection_window` (`"[0.01, 1]"`), `crp_remove_artifacts`
  (`"true"`), and the advanced `crp_onset_border`, `crp_time_step`,
  `crp_threshold_quantile`.
* Run: `tool("tool__module_interactive_script_run", name = "run_analysis")`.
  * On success the `result` is "Analysis done: ..." with the electrodes, the
    filters as saved, the event, the groups, and the response window.
  * Invalid filters make the call fail with the reason, e.g. "A band-pass
    filter requires two cutoff frequencies. Please enter both, or choose a
    low-pass or high-pass filter if only one cutoff is needed."; nothing
    runs, and people see the same reason in a notification.
  * "Analysis did not run" means a pipeline step failed: the end of `output`
    gives the error (people see a red notification), and
    `tool("tool__module_interactive_script_run", name = "pipeline_progress")`
    lists each step.
* Reset scripts (`reset_signal_config`, `reset_crp_params`,
  `reset_plot_options`) do what the cards' "Reset to defaults" links do.

### Inspect results

* The metrics: `tool("tool__shiny_output_result", outputId = "crp_viewer_table")`
  returns the table's data as text (one row per electrode).
* Figures (registered outputs, readable even in hidden tabs):
  `figure_data_by_channel_condition`, `figure_data_crp_by_channel`,
  `figure_data_by_channel_condition_overlay`, `figure_data_crp_by_channel_overlay`,
  `figure_data_crp_param_alpha_prime`, `figure_data_crp_param_snr`,
  `figure_data_crp_param_expl_var`, and, for the channel in
  `by_cond_channel_selector`, `figure_data_by_trial_channel_condition_butterfly`,
  `figure_data_by_trial_channel_condition_multiline`,
  `figure_data_by_trial_channel_condition_heatmap`. Example:
  `tool("tool__shiny_output_result", outputId = "figure_data_by_channel_condition")`.
* One channel: `tool("tool__shiny_input_update", inputId = "by_cond_channel_selector", value = "14")`.
* Fewer electrodes in the figures: set `electrode_text` (e.g. `"13-15"`), or
  set `crp_channel_filter`, e.g.
  `[{"name": "any:t_proj", "operator": "or", "criteria": "abs_gte", "threshold": "3"}]`,
  and run `send_to_electrode_selector`; its `result` lists the electrodes it
  sent.
* Plot options: set them like any input (see their descriptions); most are
  remembered as preferences, so tell the user.
* 3D viewer (`outputId = "brain_viewer"`):
  `tool("tool__rave_3dviewer_get", outputId = "brain_viewer", name = "controllers")`.
  Check a controller's choices with `name = "controller_options"` before
  `rave_3dviewer_set`. The viewer reports its camera only after the user drags
  it.

### Generate the HTML report (confirm first)

1. Set the plot options and `electrode_text` the report should use, and show
   them to the user. **Get their confirmation.**
2. `tool("tool__module_interactive_script_run", name = "open_report_dialog")`.
   The `result` gives the electrodes and plot range the dialog shows.
3. Check that the dialog is open:
   `tool("tool__shiny_query_ui", css_selector = "#voltage_explorer-do_generate_report")`.
4. `tool("tool__shiny_ui_operate", action = "click", target = "do_generate_report")`.
5. Poll `tool("tool__module_interactive_script_run", name = "report_status")`
   until it says "Report finished: <path>"; tell the user the path.

To cancel instead, close the dialog with `dismiss_modal`.

### For people only

Plot clicks, the channel ‹ › buttons (set `by_cond_channel_selector`
instead), and "Set as the default" in the loader.
