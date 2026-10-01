# Power Explorer Module Reference

Power Explorer analyzes the baseline-corrected power (wavelet spectrogram) of
iEEG electrodes across trial groups. It plots power over time and frequency,
tests each electrode's response against baseline and between groups, fits a
model across electrodes, shows the results on a 3D brain, and exports data for
analyses elsewhere and for group analysis.

**Prerequisite:** the subject needs power data from the Wavelet module, an
epoch table, and a reference table (see the Epoch and Reference modules).

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: Choose the RAVE project and subject in the "Data Selection" card.
Step 2: Choose the epoch ("Epoch name") and the trial window: "Pre" is the
trial start in seconds (default -1; it must be negative for a baseline window
before the event) and "Post" the trial end (default 2), each relative to an
"anchor to event" (default "Trial Onset"). The default anchor event fits in 
the most cases. "Set as the default" saves the
epoch as the subject's default. "Load first block as single trial" 
(rarely used) writes a new epoch file
`meta/epoch_single_trial_<epoch>.csv` into the subject.
Step 3: Choose the reference and the electrodes to load, e.g. `13-16,24`.
The "or Mask file" input is not used.
Step 4: Click the "Load subject" button to load the data.

### 2. Analysis inputs

The inputs are on the left panel, from top to bottom. Nothing is computed
until you click **RAVE!** (bottom right); the run saves the inputs to the
pipeline settings and refreshes the outputs.

* **Select Electrodes** — the electrodes to analyze, a subset of the loaded
  ones. The category selectors pick electrodes by a column of the subject's
  electrodes.csv. Default: the last saved selection, or the first loaded
  electrode. Example: `14-16`.
* **Custom ROI** (off by default) — analyze electrodes by region instead of
  by number. When on, it replaces Select Electrodes.
  * **ROI Variable** — a column of electrodes.csv with 2 to n-1 distinct
    values among the loaded electrodes, e.g. `FSLabel`.
  * **How to use ROI** — "Filter only" (default) only limits the
    electrodes. "Group/Stratify results" and "Interaction model", which the
    pipeline treats the same way, also split each analysis window by ROI
    group (labelled `<ROI>_<window>`) and add the ROI as a factor of the
    across-electrode model.
  * **ROI Group** — 1 to 15 groups, each with a label and the ROI values
    (categories) it holds. "Assign all ROI levels to groups" makes one group
    per value; "Clear groups" makes one group "All levels" with every value.
* **Save/Load Settings** — "Load Settings" reads a YAML file that "Save
  settings" downloaded. The saved settings are those of the last RAVE! run.
* **Baseline**
  * **Baseline unselected electrodes** (off by default) — also baselines the
    loaded electrodes that are not selected, so that the per-electrode
    statistics and the 3D viewer cover them (slower).
  * **Window** — the baseline window in seconds, relative to each trial's
    anchor event, e.g. `[-1, 0]`.
  * **Baseline Scope** — which units get their own baseline:
    "Per frequency, trial, and electrode" (default; each trial, frequency and
    electrode), "Across trials (aka global baseline)" (one baseline per
    frequency and electrode, shared by the trials), "Across trials and
    electrodes" (one per frequency), "Across electrodes only" (one per trial
    and frequency, shared by the electrodes).
  * **Unit of Analysis** — how power `z` is compared with the baseline power
    `z0`: "% Change Power" (default; `(z / mean(z0) - 1) × 100`), "% Change
    Amplitude" (the same on `sqrt(z)`), "Decibel"
    (`10 × (log10(z) - mean(log10(z0)))`), "z-score Power"
    (`(z - mean(z0)) / sd(z0)`), "z-score Amplitude" (the same on `sqrt(z)`),
    "z-score Decibel" (the same on `log10(z)`). Common options working well are
    z-scored amplitude (less-skewed, the number may be comparable across electrodes) 
    and decibel (more normal distributed, less subject to noisy trials). Other
    methods such as power-based baseline are often skewed; percentage
    change may increase the number of high leverage points; z-scored decibel results 
    is subject to frequency. However, the pros and cons are relative, and different
    researchers might have different conventions, so strictly speaking none of them
    are wrong. 
* **Analysis Windows**
  * **Just get univariate stats + 3dViewer (fast)** (off by default) — quick
    mode: computes only the per-electrode statistics and the electrode-by-time
    data.
  * **Analysis Window** — 1 to 5 windows, each with a Label, an Event
    ("Trial Onset" or an event column of the epoch), a Time range in seconds,
    and a Frequency range in Hz. "Choose preset band" fills the frequency
    range (e.g. "high gamma (70-150)"). Example: `HighGamma`, "Trial Onset",
    0 to 1 s, 70 to 150 Hz.
* **First Trial Factor**
  * **Condition Variable** — the epoch column that holds the trial
    conditions (columns whose name contains "Condition"; default
    "Condition").
  * **Trial Group** — 1 to 15 levels, each with a label and its conditions.
    Default after loading: the saved groups, or one group "All Conditions".
    Example: `Auditory` = `drive_a, known_a, last_a, meant_a` and
    `AudioVisual` = `drive_av, known_av, last_av, meant_av`.
* **Second Trial Factor** (off by default) — 2 to 15 levels; its choices are
  the conditions used in the first factor, and each should be in exactly one
  level. Example: `drive_known` = `drive_a, drive_av, known_a, known_av` and
  `last_meant` = the `last_*` and `meant_*` conditions.
* **Global plot options** — "Calculate electrode over time (movie maker)"
  (off by default) computes the data of the Movie Maker viewer; the line and
  heatmap palettes.
* **Save for Group Analysis**, **Export electrodes to csv**, **Export HTML
  Report** — see the procedures below.

### 3. Outputs

The output cards are on the right. Each card with tabs has a puzzle-piece
icon, which shows the tab's plot options, and a camera icon, which downloads
the plot.

* **Brain Viewers**
  * **Results Viewer** (`brain_viewer`) — the 3D brain; each statistic of
    the per-electrode table (and the clusters, `PE_Cluster`) can be shown on
    the electrodes. Double-clicking an electrode analyzes that electrode.
  * **Movie Maker** (`brain_viewer_movies`) — animates each electrode's value
    over time; needs "Calculate electrode over time (movie maker)".
* **By Electrode**
  * **Over Time** (`over_time_by_electrode`) — a heatmap of electrodes by
    time (power averaged over the window's band) for each trial group and
    window. "How to sort electrodes" other than "Electrode #" orders the
    electrodes by similarity and cuts them into "# Clusters" clusters
    (labelled `C1`, `C2`, …); "Cluster -> brain viewer", "Cluster -> ROI",
    "Cluster -> clipboard", and "Cluster -> electrodes.csv" use them.
  * **Waterfall over Time** (`waterfall_by_electrode_plot`) — the same data as
    stacked lines, one row per electrode.
  * **By Condition** (`per_electrode_statistics_mean`, `…_tstat`, `…_fdrp`)
    — per electrode, the mean, t-statistic, and p-value of the statistic
    chosen in "Data group to display". Clicks label electrodes or set
    thresholds ("Select mode"); "Actions" re-analyzes the labelled
    electrodes or sends them to the export.
  * **Tabular Results** (`per_electrode_results_table`) — the per-electrode
    statistics as a table, with options to hide columns and add electrode
    meta data.
  * **Custom Plot** (`by_electrode_custom_plot`) — plots one per-electrode
    statistic against another (or against the electrode number).
* **By Frequency**
  * **Over time** (`by_frequency_over_time`) — frequency by time heatmaps
    (averaged over trials and electrodes) for each trial group and window,
    with the analysis window outlined.
  * **Correlation** (`by_frequency_correlation`) — frequency by frequency
    Pearson correlation of the trial-level responses in each window.
* **Over Time**
  * **By Condition** (`over_time_by_condition`) — the power time course of
    each trial group; "Plot type" combines conditions, events, both, or
    neither.
  * **By Trial** (`over_time_by_trial`) — trials by time heatmaps.
* **By Condition**
  * **By Trial** (`by_condition_by_trial`) — one value per trial (the mean
    over the window's time and band) by trial group, with a table of clicked
    points. "Flag Selected (requires re-RAVE)" marks the selected trials as
    outliers, which the next run leaves out of the statistics.
  * **Overall model test** (`by_condition_statistics`) — the ANOVA table and
    formula of the model across the selected electrodes.
  * **Conditions vs. Baseline** (`by_condition_statistics_emmeans`) — each
    group's estimated mean against 0, i.e. against the baseline.
  * **Pairwise comparisons** (`by_condition_statistics_contrasts`) — all
    pairwise contrasts between the groups.

How the statistics are computed:

* **Per trial**, the value is the mean baseline-corrected power over the
  window's frequency band and time range, after aligning each trial to the
  window's event.
* **Per electrode** (the "By Condition" plots and "Tabular Results"), a
  linear model of these values on the factors that vary (the trial groups,
  and the window when there are several, with a random intercept per trial)
  gives rows `m(<group>)`, `t(<group>)`, `p(<group>)`: the estimated mean,
  its t-statistic, and its p-value against 0, for `overall` and each group;
  contrasts `A - B` get `m(...)`, `t(...)`, and `p_fdr(...)` (FDR-adjusted
  within the electrode). Flagged outlier trials are left out.
* **Across electrodes** (the "By Condition" card), a linear (mixed) model of
  the selected electrodes' trial values on the factors that vary (first
  factor, second factor, ROI, window), with random intercepts for block and
  electrode when they vary (and for trial when there are several windows).

## Common procedures

These recipes reuse the step-by-step guide.

### Procedure — compare two trial groups

Step 1: Load the subject (step-by-step guide, section 1).
Step 2: Select the electrodes, e.g. `14-16`.
Step 3: Set the baseline (e.g. -1 to 0 s, "% Change Power") and one
analysis window (e.g. "Trial Onset", 0 to 1 s, 70 to 150 Hz).
Step 4: In "First Trial Factor", make two groups, e.g. `Auditory` and
`AudioVisual`, each with its conditions.
Step 5: Click RAVE!. Read the per-electrode results in "By Electrode" ›
"By Condition" and "Tabular Results", and the model across electrodes in the
"By Condition" card.

### Procedure — two-factor design

Steps 1-4: reuse [Procedure — compare two trial groups](#procedure--compare-two-trial-groups).
Step 5: Check "Second Trial Factor" and make at least two levels that
together hold every condition of the first factor once (e.g. `drive_known`
and `last_meant`).
Step 6: Click RAVE!. The statistics now have a cell for each combination of
the two factors.

### Procedure — analyze regions (custom ROI)

Steps 1-4: reuse [Procedure — compare two trial groups](#procedure--compare-two-trial-groups).
Step 5: Check "Custom ROI", choose an ROI Variable (e.g. `FSLabel`), and
click "Assign all ROI levels to groups", or make the groups by hand (e.g.
`STG` = `ctx_lh_G_temp_sup-Lateral`).
Step 6: Choose "How to use ROI": "Filter only" to analyze just those
electrodes, or "Group/Stratify results" to also compare the regions.
Step 7: Click RAVE!.

### Procedure — cluster electrodes by their time course

Steps 1-5: reuse [Procedure — compare two trial groups](#procedure--compare-two-trial-groups).
Step 6: In "By Electrode" › "Over Time", open the options (puzzle-piece
icon) and set "How to sort electrodes" (e.g. "Activity Correlation") and
"# Clusters".
Step 7: "Cluster -> brain viewer" shows the clusters on the 3D brain;
"Cluster -> ROI" turns on the custom ROI with variable `PE_Cluster`, whose
groups can then be analyzed with RAVE!. "Cluster -> electrodes.csv" saves
the clusters as a column `PE_CLUST_<name>` of the subject's electrodes.csv
(a dated backup is kept unless "Overwrite" is checked).

### Procedure — export data for analyses elsewhere

Steps 1-5: reuse [Procedure — compare two trial groups](#procedure--compare-two-trial-groups).
Step 6: In "Export electrodes to csv", enter the electrodes (e.g. `14-15`),
optionally an ROI filter (a column of electrodes.csv and its values to
keep), and how to export frequency, time, and trials ("Collapsed" averages
over the analysis window or over the trials of each group).
Step 7: Click "Export". The module re-runs the analysis on the export
electrodes and writes a new folder
`<subject>/power_explorer/pe_export_<time>/` with one CSV per electrode
(`<project>_<subject>_eNNNN.csv`: columns for the remaining dimensions, the
value, the analysis window, and the trial labels) and `metadata.yaml` (the
baseline, unit, windows, reference, and electrode table). An alert shows the
folder.

### Procedure — save results for group analysis

Steps 1-5: reuse [Procedure — compare two trial groups](#procedure--compare-two-trial-groups).
Step 6: In "Save for Group Analysis", keep "Create New" and enter a label,
e.g. `compare_A_and_B_pct_signal_change`; use the same label for every
subject. To replace an earlier save, choose its label instead.
Step 7: Click "Save!". The module saves a copy of the pipeline, with its
settings and the results, into
`<subject>/pipelines/power_explorer/power_explorer-<label>-<time>/`.

### Procedure — HTML report

Steps 1-5: reuse [Procedure — compare two trial groups](#procedure--compare-two-trial-groups).
Step 6: In "Export HTML Report", choose "Aggregate only" or "Aggregate +
individual" (adds each selected electrode) and the graphs to include.
Step 7: Click "Generate Report". The report is written in the background
into `<subject>/reports/report-univariatePower_datetime-<time>_power_explorer/report.html`;
a notification with a link appears when it is done.

## Caveats

* The inputs reach the pipeline (and "Save settings") only when RAVE! runs.
* Quick mode refreshes only the "By Electrode" card (except "Waterfall over
  Time") and the 3D viewer; the other plots keep the last full run.
* Changing "Condition Variable" resets the first factor to "All Conditions".
* A condition in two levels of a factor is kept only in the first level (a
  warning says so); if that leaves a level empty, the run stops ("Insufficient
  Data"). Analysis windows with the same event and nearly the same time and
  frequency ranges stop the run ("Analysis not run"); a smaller overlap only
  warns.
* With auto re-calculation on, changing the selected electrodes re-runs the
  analysis.
* "Custom ROI" replaces "Select Electrodes" while it is on.
* "Export" re-runs the analysis on the export electrodes, so the plots then
  show those electrodes while "Select Electrodes" keeps its value. It writes
  nothing in quick mode: uncheck "Just get univariate stats + 3dViewer
  (fast)" first.
* The export's ROI filter works with columns of electrodes.csv; its "Custom
  ROI" choice is not implemented in the export step.
* "Save for Group Analysis" with an earlier label asks to delete the older
  saves with that label, but they are kept for now: ravecore's
  `list_pipelines(all = TRUE)`, which the save uses to find them, returns no
  rows. The newest save is the one that "Create new / Replace existing"
  lists.
* Reports with all graphs and individual electrodes can take tens of MB.
* "Which contrasts to display?" offers only "All-possible pairwise" after a
  run.
* For AI agents: plot clicks (labels, thresholds, trial outliers), the
  "Manual threshold" dialog, loading settings from a file, downloads, and
  "Cluster -> electrodes.csv" are for people only.

## Run the pipeline without the UI

Reproduce the analysis in plain R, without the front-end, via the RAVE
pipeline. The settings are those in the module's `settings.yaml`; keys not set
below keep their saved values.

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("power_explorer")

# Loader settings (what "Load subject" saves), then load the data
pipeline$set_settings(
  project_name = "demo",                  # RAVE project
  subject_code = "DemoSubject",           # subject code
  epoch_choice = "auditory_onset",        # epoch table
  epoch_choice__trial_starts = -1,        # trial start (s), relative to the anchor event
  epoch_choice__trial_ends = 2,           # trial end (s)
  reference_name = "default",             # reference table
  loaded_electrodes = "13-16,24"          # electrodes to load
)
pipeline$run("repository")

# Analysis settings (what RAVE! saves)
pipeline$set_settings(
  analysis_electrodes = "14-16",          # electrodes to analyze
  baseline_settings = list(
    window = list(c(-1, 0)),              # baseline window(s), seconds
    scope = "Per frequency, trial, and electrode",
    unit_of_analysis = "% Change Power"
  ),
  analysis_settings = list(               # one list per analysis window
    list(label = "HighGamma", event = "Trial Onset", time = c(0, 1),
         frequency_dd = "Select one", frequency = c(70, 150))
  ),
  condition_variable = "Condition",       # epoch column with the conditions
  first_condition_groupings = list(       # levels of the first factor
    list(label = "Auditory",
         conditions = c("drive_a", "known_a", "last_a", "meant_a")),
    list(label = "AudioVisual",
         conditions = c("drive_av", "known_av", "last_av", "meant_av"))
  ),
  enable_second_condition_groupings = FALSE,
  enable_custom_ROI = FALSE
)

# Build the statistics and a plot's data
results <- pipeline$run(c("omnibus_results", "across_electrode_statistics",
                          "over_time_by_condition_data"))
results$omnibus_results$stats           # statistics by electrode
results$across_electrode_statistics$aov # the model across electrodes

# Reuse a module helper (from the shared env) to plot a result
env <- pipeline$shared_env()
env$plot_over_time_by_condition(
  results$over_time_by_condition_data,
  plot_options = env$pe_graphics_settings_cache$get(
    "over_time_by_condition_plot_options")
)
```

How RAVE! converts the inputs before it saves them:

* `baseline_window`, `baseline_scope`, and `baseline_unit` become
  `baseline_settings` (`window` is a list of windows).
* `electrode_text` becomes `analysis_electrodes`. With the custom ROI on, it
  is replaced by the electrodes whose ROI value is in the groups, and each
  group of `custom_roi_groupings` gets their `electrodes`.
* Duplicated conditions are dropped from the later levels of each factor.
* `time_censor` is always off; `trial_outliers_list` holds the flagged
  trials.
* Quick mode builds `over_time_by_electrode_data`, `omnibus_results`, and
  `by_electrode_similarity_data`. A full run builds `analysis_settings_clean`,
  `baseline_settings`, `baselined_power`, `analysis_groups`,
  `pluriform_power`, `by_frequency_over_time_data`,
  `by_frequency_correlation_data`, `over_time_by_trial_data`,
  `over_time_by_electrode_data`, `by_electrode_similarity_data`,
  `omnibus_results`, `over_time_by_condition_data`, and
  `across_electrode_statistics`, plus `over_time_by_electrode_dataframe` for
  the movie maker. "Export" adds `data_for_export` (the export folder), and
  "Save!" builds `data_for_group_analysis`.

## Drive the module with MCP tools

Operate the live module the same way a user would, following the
step-by-step guide above. Each step names an MCP tool; call it as
`tool("tool__NAME", arg = value)`. Values are JSON text: `"14-16"`,
`"[-1,0]"`, `"true"`, or an array of objects for groups and windows.

Run the scripts in this order: `load_data`, then set the analysis inputs,
then `run_analysis`, then read the results. Scripts that write into the
subject folder (`export_electrodes`, `save_for_group_analysis`,
`generate_report`) come last, and only after the user confirms their
settings. "Cluster -> electrodes.csv" has no script: only the user may use
it.

A script's reply has `result` (its return value) and `output` (what it printed
while it ran). `run_analysis` finishes even when invalid inputs stop the
analysis: its result is then "Analysis did not run", and `output` says why.
A failing pipeline step makes the script fail with the step's error (e.g.
"No electrode selected" when no loaded electrode is selected); script
`pipeline_progress` lists the state and error of each step.

Inputs round-trip through the browser: after each `shiny_input_update`,
check the value with `shiny_input_info` before going on. A select whose
choices are still loading (e.g. the subject list, or the conditions after
loading) keeps its old value; send the update again.

### Load data

* Set the project: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`
* Set the subject: `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "DemoSubject")`
* Set the epoch and trial window: `inputId = "loader_epoch_name"` (`"auditory_onset"`),
  `"loader_epoch_name__trial_starts"` (`"-1"`), `"loader_epoch_name__trial_ends"` (`"2"`)
* Set the reference and electrodes: `inputId = "loader_reference_name"` (`"default"`),
  `"loader_electrode_text"` (`"13-16,24"`)
* Load the data: `tool("tool__module_interactive_script_run", name = "load_data")`.
  The result lists the trials, electrodes, conditions (with trial counts),
  events, and the time and frequency ranges.

### Configure and run

* Inspect the current inputs: `tool("tool__shiny_input_info")`
* Electrodes: `tool("tool__shiny_input_update", inputId = "electrode_text", value = "14-16")`
* Baseline: `inputId = "baseline_window"` (`"[-1,0]"`), `"baseline_scope"`
  (`"Per frequency, trial, and electrode"`), `"baseline_unit"` (`"% Change Power"`)
* Analysis windows: `tool("tool__shiny_input_update", inputId = "ui_analysis_settings",
  value = '[{"label":"HighGamma","event":"Trial Onset","time":[0,1],"frequency_dd":"Select one","frequency":[70,150]}]')`
* Trial groups: set `condition_variable` first (it resets the groups), then
  `tool("tool__shiny_input_update", inputId = "first_condition_groupings",
  value = '[{"label":"Auditory","conditions":["drive_a","known_a","last_a","meant_a"]},{"label":"AudioVisual","conditions":["drive_av","known_av","last_av","meant_av"]}]')`
* Second factor (optional): `enable_second_condition_groupings` (`"true"`),
  then `second_condition_groupings` (same format)
* Custom ROI (optional): `enable_custom_ROI` (`"true"`), `custom_roi_variable`
  (e.g. `"FSLabel"`), `custom_roi_type`, then `custom_roi_groupings`
  (`[{"label":"STG","conditions":["ctx_lh_G_temp_sup-Lateral"]}]`) or script
  `assign_roi_levels` (`clear_roi_groups` makes one "All levels" group)
* Quick mode: `quick_omnibus_only` (`"true"` or `"false"`)
* Run the analysis: `tool("tool__module_interactive_script_run", name = "run_analysis")`.
  The result starts with "Analysis done (quick)" or "Analysis done (full)"
  and lists the settings used.
* Clusters (optional): set `otbe_yaxis_sort` (e.g. `"Activity Correlation"`)
  and `otbe_yaxis_cluster_k` (e.g. `"2"`), draw the heatmap with
  `tool("tool__shiny_output_result", outputId = "over_time_by_electrode")`,
  then run script `cluster_to_viewer` or `cluster_to_roi`.
* Export (ask the user first): set `quick_omnibus_only` to `"false"`
  (nothing is exported in quick mode), `electrodes_to_export` (`"14-15"`),
  `frequencies_to_export`, `times_to_export`, `trials_to_export`, and the
  optional ROI filter, then run script `export_electrodes`. Its result is the
  export folder. The "Done with exporting!" alert stays until the user closes
  it.
* Save for group analysis (ask the user first): set
  `save_pipeline_for_group_analysis_label` (and
  `replace_existing_group_anlysis_pipeline` to replace a save), then run
  script `save_for_group_analysis`. Its result is the saved folder.
* HTML report (ask the user first): set `exp_html_electrodes_to_include`
  and `exp_html_graphs`, run script `generate_report`, then run
  `report_status` until it says "Report finished: <path>".

### Inspect results

* Per-electrode statistics as text: `tool("tool__module_interactive_script_run", name = "electrode_statistics")`
* The model across electrodes: `tool("tool__shiny_output_result", outputId = "by_condition_statistics", transform_image = false)`;
  also `by_condition_statistics_emmeans` and `by_condition_statistics_contrasts`
* Plots: `tool("tool__shiny_output_result", outputId = "over_time_by_condition")`;
  also `by_frequency_over_time`, `by_frequency_correlation`, `over_time_by_trial`,
  `over_time_by_electrode`, `waterfall_by_electrode_plot`,
  `per_electrode_statistics_mean`, `per_electrode_statistics_tstat`,
  `per_electrode_statistics_fdrp`, `by_electrode_custom_plot`, `by_condition_by_trial`
* Show the user a tab: `tool("tool__shiny_input_update", inputId = "by_electrode_tabset", value = "Tabular Results")`
  (also `brain_viewers`, `over_time_tabset`, `by_condition_tabset`,
  `by_frequency_tabset`). A plain output such as the table can be read only
  while its tab shows: `tool("tool__shiny_query_ui", css_selector = "#power_explorer-per_electrode_results_table", transform_image = false)`
* 3D viewers: `tool("tool__rave_3dviewer_get", outputId = "brain_viewer", name = "controllers")`;
  change one with `tool("tool__rave_3dviewer_set", outputId = "brain_viewer", name = "controllers", data = ...)`
  after checking its choices with `name = "controller_options"`
