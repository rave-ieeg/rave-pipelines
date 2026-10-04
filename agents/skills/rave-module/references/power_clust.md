# Power Clustering Module Reference

Power Clustering (module ID `power_clust`) groups electrodes whose
baseline-corrected power responses look alike. It averages power over a
frequency band and over the trials of each condition group, compares the
electrodes' response curves, and builds a hierarchical clustering tree;
silhouette scores suggest the number of clusters. The clusters are shown as
plots, a table, and colors in the 3D viewer.

**Prerequisite:** the subject needs wavelet power (run the Wavelet module
first), an epoch, and a reference.

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
event; by default -1 and 2), the reference, and the electrodes to load (e.g.
`13-16,24`). "Set as the default" saves the epoch or reference as the
subject's default.
Step 3: Click "Load subject".

### 2. Analysis inputs

Card "Configure Analysis":

* **Time range (relative to event start)** — the analysis window `[start,
  end]` in seconds after each group's start event; the bounds are the loaded
  trial window. Each group is analyzed from `start` until `end` after its
  start event, or until its finish event. Example: `0` to `1`.
* **Frequency range** — the band in Hz whose power is averaged; the bounds are
  the subject's wavelet frequencies. It must contain at least one wavelet
  frequency. Example: `70` to `150` (high gamma).
* **Zeta threshold** — 0.05 to 0.95, default 0.5. Each group's electrode
  similarity matrix is factorized (non-negative matrix factorization),
  trying ranks from the number of the group's conditions (at most the number
  of electrodes) down to 2; the first rank whose degeneracy score (zeta) is
  below the threshold is kept. A lower threshold tends to keep fewer
  components.
* **Load Settings / Download Settings** — load the analysis settings from a
  YAML file, or download them.

Card "Baseline Settings":

* **Baseline windows** — one or more windows in seconds, e.g. `-1` to `0`.
* **Unit of analysis** — `Decibel`, `% Change Power`, `% Change Amplitude`,
  `z-score Decibel`, `z-score Power`, or `z-score Amplitude`.
* **Scope** — `Per frequency, trial, and electrode`, `Across electrode`,
  `Across trial`, or `Across trial and electrode`.

Card "Create Condition Contrast" (1 to 40 groups):

* **Name** — the group's label (empty becomes `group01`, `group02`, ...).
* **Conditions** — the epoch conditions in the group. A group without a
  valid condition is skipped.
* **Start event** — `Trial Onset` or an event of the epoch.
* **Finish event** — `[Analysis end]` (use the end of the time range), an
  event, or `Trial Onset`.

Loading data resets the groups to the saved ones, or to one group "All
Conditions".

Click **Run Analysis** (footer). The module saves the inputs, applies the
baseline, and clusters the electrodes on their responses across all groups;
then it sets **# of clusters** to the suggested number.

### 3. Outputs

* **3D Viewer** — the loaded electrodes, colored by cluster ("Cluster") after
  a run.
* **Channel time-series** — a heatmap of each electrode's average response
  (rows) over time (columns), the groups side by side; the rows are ordered by
  cluster and marked with the cluster colors.
* **Diagnostic plots** — the dendrogram with boxes around the clusters, the
  silhouette score for each number of clusters (a dashed line marks the
  current one; click a point to choose it), and the mean response of each
  cluster per group.
* **Clustering table** — each cluster's number of channels and its channels.
* **# of clusters** (footer of the output card) — the number of clusters k.
  Changing it cuts the same tree again, without re-running.

A higher silhouette score means better separated clusters; the suggested k
has the highest score.

## Common procedures

### Procedure — cluster electrodes on two condition groups

Step 1: Load the subject (step-by-step guide, section 1).
Step 2: Set the time range, the frequency band, and the baseline.
Step 3: Make two groups, e.g. "Auditory" (`drive_a`, `known_a`, `last_a`,
`meant_a`) and "AudioVisual" (`drive_av`, `known_av`, `last_av`, `meant_av`),
both from `Trial Onset` to `[Analysis end]`.
Step 4: Click "Run Analysis", then read the clustering table.

### Procedure — choose the number of clusters

Steps 1-4: reuse [Procedure — cluster electrodes on two condition groups](#procedure--cluster-electrodes-on-two-condition-groups).
Step 5: Compare the silhouette scores in "Diagnostic plots"; set "# of
clusters" to another k and look at the dendrogram, the mean responses, and
the 3D viewer.

## Caveats

* The inputs are saved to the pipeline only when the analysis runs.
* Every run sets "# of clusters" back to the suggested k.
* A frequency band without any wavelet frequency stops the run with
  "Frequency range is too narrow" in an "Error found!" alert, which stays
  until it is closed.
* A time range that ends at or before the start event fails the pipeline
  ("Analysis time duration is too narrow"); a red notification shows the
  error, and the plots ask for a run.
* After loading new data, the plots ask for a run again.
* The module writes nothing into the subject (only "Set as the default"
  changes the subject's default epoch or reference).

## Run the pipeline without the UI

The UI saves its inputs under these names in `settings.yaml`: the time range
as `analysis_window`, the baseline card as `baseline__windows`,
`baseline__unit_of_analysis`, and `baseline__global_baseline_choice`, and
the groups as `condition_groups`.

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("power_clust")

# Set inputs
pipeline$set_settings(
  project_name = "demo",                  # project
  subject_code = "DemoSubject",           # subject
  epoch_choice = "auditory_onset",        # epoch
  epoch_choice__trial_starts = -1,        # trial window start (s)
  epoch_choice__trial_ends = 2,           # trial window end (s)
  reference_name = "default",             # reference
  loaded_electrodes = "13-16,24",         # electrodes to load
  analysis_window = c(0, 1),              # time range (s from each group's start event)
  frequency_range = c(70, 150),           # band (Hz)
  zeta_threshold = 0.5,                   # zeta threshold
  baseline__windows = list(list(window_interval = c(-1, 0))),
  baseline__unit_of_analysis = "Decibel",
  baseline__global_baseline_choice = "Per frequency, trial, and electrode",
  condition_groups = list(
    list(group_name = "Auditory", group_conditions = c("drive_a", "known_a"),
         group_start_event = "Trial Onset", group_finish_event = "[Analysis end]")
  )
)

# Build the clustering and read the results
pipeline$run(c("clustering_tree", "clustering_index"))
clustering_tree <- pipeline$read("clustering_tree")    # $cluster_object is an hclust
clustering_index <- pipeline$read("clustering_index")  # $scores (k, silhouette), $suggested$k
groups <- pipeline$read("combined_group_results")      # $electrode_channels, response curves

# Clusters at the suggested k, and the module's channel plot
clusters <- stats::cutree(clustering_tree$cluster_object, k = clustering_index$suggested$k)
env <- pipeline$shared_env()
env$diagnose_cluster(cluster_result = clustering_tree,
                     k = clustering_index$suggested$k,
                     combined_group_results = groups)
```

## Drive the module with MCP tools

Operate the live module the way a person does. Each step names an MCP tool;
call it as `tool("tool__NAME", arg = value)`. The module registers four
interactive scripts: `load_data` ("Load subject"), `run_analysis` ("Run
Analysis"), and the read-only `cluster_summary` and `pipeline_progress`.

**Run `load_data` first, then set the analysis inputs, then `run_analysis`.**
Confirm the condition groups with the user unless they named them. The
module writes nothing into the subject.

A script's reply has `result` (its return value) and `output` (what it
printed while it ran).

### Load data

* `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`
* `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "DemoSubject")`
* `tool("tool__shiny_input_update", inputId = "loader_epoch_name", value = "auditory_onset")`
* `tool("tool__shiny_input_update", inputId = "loader_epoch_name__trial_starts", value = "-1")`
  and `loader_epoch_name__trial_ends` (`"2"`)
* `tool("tool__shiny_input_update", inputId = "loader_reference_name", value = "default")`
* `tool("tool__shiny_input_update", inputId = "loader_electrode_text", value = "13-16,24")`
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`. The
  `result` gives the trials, the electrodes, the conditions with their trial
  counts, the events, and the frequency range: the choices of the analysis
  inputs.

Values round-trip through the browser: check them with
`tool("tool__shiny_input_info", inputIds = [...])` before loading. A select
whose choices are still loading (e.g. the subject after the project)
ignores the update; send it again.

### Configure and run

* `tool("tool__shiny_input_update", inputId = "time_range", value = "[0, 1]")`
* `tool("tool__shiny_input_update", inputId = "frequency_range", value = "[70, 150]")`
* `tool("tool__shiny_input_update", inputId = "zeta_threshold", value = "0.5")`
* `tool("tool__shiny_input_update", inputId = "baseline_choices__windows", value = "[{\"window_interval\": [-1, 0]}]")`
* `tool("tool__shiny_input_update", inputId = "baseline_choices__unit_of_analysis", value = "Decibel")`
* `tool("tool__shiny_input_update", inputId = "baseline_choices__global_baseline_choice", value = "Per frequency, trial, and electrode")`
* Groups, one object per group:
  `tool("tool__shiny_input_update", inputId = "condition_groups", value = "[{\"group_name\": \"Auditory\", \"group_conditions\": [\"drive_a\", \"known_a\"], \"group_start_event\": \"Trial Onset\", \"group_finish_event\": \"[Analysis end]\"}]")`
* Check them: `tool("tool__shiny_input_info")`.
* Run: `tool("tool__module_interactive_script_run", name = "run_analysis")`.
  * On success, the `result` starts with "Clustering done: ..." (the settings
    used), then gives the clusters at the suggested k and the silhouette
    score per k.
  * "Clustering did not run" means it failed. The end of `output` gives the
    error, after the code of the run:
    * an invalid input logs `Error in ...: <message>`, e.g. "Frequency range
      is too narrow", and also opens an "Error found!" alert: after reading
      the error, close it with
      `tool("tool__shiny_ui_operate", action = "close_alert2")`;
    * a failing pipeline step logs "✖ <step> errored", then
      `Possible issue: <message>`, e.g. "Analysis time duration is too
      narrow"; people see a red notification, and
      `tool("tool__module_interactive_script_run", name = "pipeline_progress")`
      lists each step with the error of the failed one.

### Inspect results

* Clusters: `tool("tool__module_interactive_script_run", name = "cluster_summary")`:
  the k and the electrodes, one line per cluster (`cluster 1 (n=2): 13,15`),
  and the silhouette score per k. After `load_data` or a failed run it says
  "No results yet".
* Another number of clusters, without re-running:
  `tool("tool__shiny_input_update", inputId = "n_clusters", value = "3")`, then
  `cluster_summary` again. The plots, the table, and the viewer follow.
* The plots and the table are not registered outputs. Show their tab first,
  then picture them:
  * `tool("tool__shiny_input_update", inputId = "cluster_tabset", value = "Diagnostic plots")`
    (tabs: `Channel time-series`, `Diagnostic plots`, `Clustering table`)
  * `tool("tool__shiny_query_ui", css_selector = "#power_clust-cluster_mean_plot")`
    (also `cluster_dendrogram_plot`, `cluster_silhouette_plot`,
    `channel_cluster_timeseries`, and the table `cluster_table`)
* 3D viewer (`outputId = "viewer"`):
  `tool("tool__rave_3dviewer_get", outputId = "viewer", name = "controllers")`.
  Check a controller's choices with `name = "controller_options"` before
  changing it with `tool("tool__rave_3dviewer_set", ...)`. The viewer reports
  its camera only after the user drags it.

### For people only

"Load Settings" (a file upload; agents set the inputs instead), "Download
Settings", clicks on the silhouette plot (set `n_clusters` instead), and "Set
as the default" in the loader.
