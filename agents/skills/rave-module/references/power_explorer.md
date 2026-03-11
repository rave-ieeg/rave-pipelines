# power_explorer Module Reference

Explores baseline-corrected power/amplitude data from iEEG spectrograms. Supports multi-factorial trial groupings, custom ROI analysis, electrode clustering, 3D brain visualization with time-course animations (movie maker), univariate statistics, and data export for group analysis.

---

## 1. Pipeline Interface

This module uses `ravepipeline` for computation. The pipeline definition is in `main.Rmd`, with the interpreted make file at `make-power_explorer.R` showing target dependencies.

### 1.1 Prerequisites

| Requirement | Description |
|-------------|-------------|
| **Wavelet-transformed power data** | Power data must be generated via `wavelet_module` ("Wavelet Time Frequency"). |
| **Epoch file** | At least one epoch file must exist for the subject. |
| **Reference scheme** | A reference scheme must be defined (typically via `reference_module`). |
| **Valid project/subject** | A RAVE project and subject must exist. |

#### Loader Components

The loader UI (`R/loader.R`, `R/aaa-presets.R`) collects:

| Component | Purpose |
|-----------|---------|
| `loader_project` | Project name selector |
| `loader_subject` | Subject code selector |
| `loader_epoch` | Epoch file selector with default option |
| `loader_reference` | Reference scheme selector with default option |
| `loader_electrodes` | Electrode numbers/ranges to load |
| `loader_mask_file` | Optional mask file for electrode selection |
| `loader_3d_viewer` | Preview brain with electrode loading status |

On successful load, the pipeline runs `repository` target and fires `data_changed` event.

---

### 1.2 Pipeline Settings

Settings are stored in `settings.yaml`. Access via `pipeline$get_settings()` and update via `pipeline$set_settings()`.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `project_name` | string | `"demo"` | RAVE project name |
| `subject_code` | string | `"DemoSubject"` | Subject identifier |
| `loaded_electrodes` | string | `"13-16,24"` | Electrodes loaded into memory |
| `analysis_electrodes` | string | `"14"` | Electrodes for analysis (subset of loaded) |
| `epoch_choice` | string | `"auditory_onset"` | Selected epoch file name |
| `reference_name` | string | `"default"` | Reference scheme name |
| `epoch_choice__trial_starts` | numeric | `-1` | Trial start time relative to event (seconds) |
| `epoch_choice__trial_ends` | numeric | `2` | Trial end time relative to event (seconds) |
| `epoch_choice__load_single_trial` | logical | `FALSE` | Load single trial mode (for event-locked analysis) |
| `condition_variable` | string | `"Condition"` | Column name for trial conditions |

#### baseline_settings

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `window` | list | `[[-0.8, -0.5]]` | Baseline time window(s) |
| `scope` | string | `"Per frequency, trial, and electrode"` | Baseline normalization scope |
| `unit_of_analysis` | string | `"decibel"` | Unit: decibel, % signal change, z-score, etc. |

#### analysis_settings

Nested list (indexed by position), each containing:

| Key | Type | Description |
|-----|------|-------------|
| `label` | string | Name for the analysis window (e.g., "VisStart") |
| `event` | string | Event column to align to (e.g., "Trial Onset") |
| `time` | numeric[2] | Time window [start, end] in seconds |
| `frequency` | numeric[2] | Frequency band [low, high] in Hz |

#### first_condition_groupings / second_condition_groupings

Nested list for factorial trial groupings:

| Key | Type | Description |
|-----|------|-------------|
| `label` | string | Factor level label (e.g., "AV", "A", "V") |
| `conditions` | string[] | Trial types in this group |

#### custom_roi_groupings

| Key | Type | Description |
|-----|------|-------------|
| `label` | string | ROI group label |
| `conditions` | string[] | ROI categories from electrodes.csv |
| `electrodes` | string | Electrodes in this ROI (computed) |

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `enable_custom_ROI` | logical | `FALSE` | Enable custom ROI filtering |
| `custom_roi_variable` | string | `"none"` | Column from electrodes.csv for ROI |
| `custom_roi_type` | string | `"Filter only"` | How to use ROI: "Filter only", "Group/Stratify results", "Interaction model" |
| `enable_second_condition_groupings` | logical | `FALSE` | Enable second trial factor |
| `omnibus_includes_all_electrodes` | logical | `FALSE` | Baseline all loaded electrodes for 3D viewer |

#### Export settings

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `electrodes_to_export` | string | `""` | Electrodes to export |
| `electrodes_to_export_roi_name` | string | `"none"` | ROI filter for export |
| `frequencies_to_export` | string | `"Collapsed, Analysis window(s) only"` | How to export frequency |
| `times_to_export` | string | `"Raw, All available times"` | How to export time |
| `trials_to_export` | string | `"Raw, Only trials used in grouping factors"` | How to export trials |

---

### 1.3 Key Targets

Main targets defined in `main.Rmd`:

| Target | Export | Dependencies | Description |
|--------|--------|--------------|-------------|
| `check_load_power` | `repository` | project_name, subject_code, loaded_electrodes, epoch_choice, reference_name | Loads subject power data repository |
| `check_requested_electrodes` | `requested_electrodes` | repository, analysis_electrodes | Validates analysis electrodes |
| `check_analysis_settings` | `analysis_settings_clean` | repository, analysis_settings, baseline_settings, first_condition_groupings, etc. | Validates and cleans settings |
| `calculate_baseline` | `baselined_power` | repository, baseline_settings, requested_electrodes | Applies baseline correction |
| `build_trial_details` | `trial_details` | repository, first_condition_groupings, condition_variable | Maps trials to factor levels |
| `build_analysis_groups` | `analysis_groups` | trial_details, first_condition_groupings, second_condition_groupings | Creates analysis group structures |
| `build_by_frequency_over_time_data` | `by_frequency_over_time_data` | baselined_power, analysis_settings_clean, analysis_groups | Time-frequency heatmap data |
| `build_over_time_by_electrode_data` | `over_time_by_electrode_data` | baselined_power, analysis_settings_clean, analysis_groups | Electrode × time heatmap data |
| `build_over_time_by_condition_data` | `over_time_by_condition_data` | baselined_power, analysis_settings_clean, analysis_groups | Condition time-course data |
| `build_omnibus_results` | `omnibus_results` | multiple | Univariate statistics (t-tests, p-values per electrode) |
| `build_across_electrode_statistics` | `across_electrode_statistics` | omnibus_results | Statistics aggregated across electrodes |
| `build_data_for_export` | `data_for_export` | multiple | Prepared data for CSV export |

> See `make-power_explorer.R` for detailed target dependency tree and implementation.

#### Target Details

**`repository`** (loader target):
- Loads subject power data via `ravecore::prepare_subject_power_with_epochs()`
- Validates epoch and reference exist
- Returns: `RAVEPowerRepository` object

**`baselined_power`** (main computation):
- Applies baseline normalization via `ravecore::power_baseline()`
- Supports multiple baseline scopes: per-trial, per-electrode, per-frequency, or combinations
- Returns: `FileArray` with baselined data (Frequency × Time × Trial × Electrode)

**`omnibus_results`**:
- Computes per-electrode univariate statistics
- Includes t-tests, mean power, FDR-corrected p-values
- Returns: `list(stats = data.frame, data = list)`

#### When to Run Each Target

| Situation | Targets to Run |
|-----------|----------------|
| Load subject data | `repository` |
| Validate settings | `analysis_settings_clean`, `analysis_groups` |
| Full analysis | `omnibus_results` (pulls all dependencies) |
| Quick stats only | `omnibus_results`, `over_time_by_electrode_data`, `by_electrode_similarity_data` |

---

## 2. Module UI Interface

The UI is built with `ravedash` and `shidashi`. Module UI logic is in `R/module_html.R`, `R/module_server.R`, and `R/loader.R`. The loader ensures prerequisites are met before showing main content.

### 2.1 Input Components

Input IDs below are without the module namespace prefix (i.e., `"baseline_window"` not `"power_explorer--baseline_window"`).

#### Electrode Selector

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `electrode_text` | textInput | "Electrodes" | — | Analysis electrodes (subset of loaded) |

#### Custom ROI Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `enable_custom_ROI` | checkboxInput | "Custom ROI" | `FALSE` | Enable ROI-based electrode filtering |
| `custom_roi_variable` | selectInput | "ROI Variable" | `character(0)` | Column from electrodes.csv |
| `custom_roi_type` | selectInput | "How to use ROI" | `"Filter only"` | Filter, Group/Stratify, or Interaction |
| `custom_roi_groupings` | compoundInput2 | "ROI Group" | — | ROI grouping definitions |

#### Baseline Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `omnibus_includes_all_electrodes` | checkboxInput | "Baseline unselected electrodes..." | `FALSE` | For 3D viewer inclusion |
| `baseline_window` | sliderInput | "Window" | `[0, 1]` | Baseline time window (seconds) |
| `baseline_scope` | selectInput | "Baseline Scope" | (first option) | Normalization scope |
| `baseline_unit` | selectInput | "Unit of Analysis" | (first option) | decibel, % signal change, z-score |

#### Analysis Windows Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `quick_omnibus_only` | checkboxInput | "Just get univariate stats..." | `FALSE` | Fast mode: skip full analysis |
| `ui_analysis_settings` | compoundInput2 | "Analysis Window" | — | Multi-window settings (1-5 windows) |

Each analysis window contains:
- `label`: Window name
- `event`: Event to align to
- `time`: Time range slider
- `frequency_dd`: Preset frequency band dropdown
- `frequency`: Frequency range slider

#### Trial Groupings Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `condition_variable` | selectInput | "Condition Variable" | `"Condition"` | Column for trial types |
| `first_condition_groupings` | compoundInput2 | "Trial Group" | — | First factor levels (1-15 groups) |
| `enable_second_condition_groupings` | checkboxInput | "Second Trial Factor" | `FALSE` | Enable 2-way factorial design |
| `second_condition_groupings` | compoundInput2 | "Trial Group" | — | Second factor levels |

#### Global Plot Options

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `do_over_time_by_electrode_dataframe` | checkboxInput | "Calculate electrode over time (movie maker)" | `FALSE` | For 3D animation |
| `gpo_lines_palette` | selectInput | "Lines/Points palette" | — | Color palette for line plots |
| `gpo_heatmap_palette` | selectInput | "Heatmap palette" | — | Color palette for heatmaps |

#### Export Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `electrodes_to_export` | textInput | "Electrodes to export" | `""` | Electrode range for export |
| `electrodes_to_export_roi_name` | selectInput | "Add ROI filter" | `"none"` | ROI filter |
| `frequencies_to_export` | selectInput | "How to export frequency" | (first option) | Collapsed or raw |
| `times_to_export` | selectInput | "How to export time" | `"Raw, All available times"` | Collapsed or raw |
| `trials_to_export` | selectInput | "How to export trial" | `"Raw, Only trials used..."` | Collapsed or raw |
| `btn_export_electrodes` | actionButton | "Export" | — | Triggers CSV export |

#### Group Analysis Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `replace_existing_group_anlysis_pipeline` | selectInput | "Create new / Replace existing" | `"Create New"` | Pipeline handling |
| `save_pipeline_for_group_analysis_label` | textInput | "Label for results" | — | Export label |
| `save_pipeline_for_group_analysis` | actionButton | "Save!" | — | Save for group analysis |

---

### 2.2 UI to Pipeline Mapping

#### Input Validation

Validation rules applied before pipeline execution:

| Input | Rule |
|-------|------|
| `electrode_text` | Valid electrode numbers within loaded range |
| `first_condition_groupings` | No duplicate conditions across levels |
| `second_condition_groupings` | If enabled, must include all conditions from first factor |
| `ui_analysis_settings` | No identical or heavily overlapping windows |

#### Baseline Scope Options

| Display Name | Pipeline Value |
|--------------|---------------|
| "Per frequency, trial, and electrode" | per-trial baseline per frequency and electrode |
| "Per trial and electrode" | collapse frequency for baseline |
| "Per frequency and electrode" | global trial baseline |
| "Per electrode" | global across trials and frequencies |

#### Unit of Analysis Options

| Display Name | Method |
|--------------|--------|
| "decibel" | dB transformation: 10 × log10(power/baseline) |
| "% signal change" | (power - baseline) / baseline × 100 |
| "z-score" | (power - baseline) / sd(baseline) |

---

### 2.3 Pipeline Execution Flow

#### Stage 0: Load Repository

- **Goal**: Load subject power data and validate prerequisites
- **Trigger**: "Load subject" button in loader
- **Targets**: `repository`
- **Result**: If successful, fires `data_changed` event → `check_data_loaded()` validates → main UI shown

```r
pipeline$run(names = "repository", scheduler = "none", type = "smart", async = FALSE)
```

#### Stage 1: Run Analysis

- **Goal**: Compute baseline correction, trial groupings, statistics, and visualization data
- **Trigger**: "RAVE!" button clicked (bottom-right)
- **Targets**: Multiple (depends on `quick_omnibus_only` setting)

Quick mode (`quick_omnibus_only = TRUE`):
```r
pipeline$run(
  names = c("over_time_by_electrode_data", "omnibus_results", "by_electrode_similarity_data"),
  scheduler = "none", type = "smart", async = FALSE
)
```

Full analysis:
```r
pipeline$run(
  names = c(
    "analysis_settings_clean", "baseline_settings", "baselined_power",
    "analysis_groups", "pluriform_power",
    "by_frequency_over_time_data", "by_frequency_correlation_data",
    "over_time_by_trial_data", "over_time_by_electrode_data",
    "by_electrode_similarity_data", "omnibus_results",
    "over_time_by_condition_data", "across_electrode_statistics"
  ),
  scheduler = "none", type = "smart", async = FALSE
)
```

After completion:
- Updates `local_data$results` with all computed targets
- Triggers UI output updates via reactive flags

---

### 2.4 Outputs and Visualizations

#### Brain Viewers Tab

| Output ID | Type | Content |
|-----------|------|---------|
| `brain_viewer` | threejsBrainOutput | 3D brain with electrode results (t-values, means, clusters) |
| `brain_viewer_movies` | threejsBrainOutput | 3D brain for time-course animations |

**Triggering reactives:**
- `local_reactives$update_3dviewer` — updates after analysis or cluster changes

#### By Electrode Tab

| Output ID | Type | Content |
|-----------|------|---------|
| `over_time_by_electrode` | plotOutput2 | Heatmap: Electrode × Time, colored by power |

**Triggering reactives:**
- `local_reactives$update_over_time_by_electrode_plot`
- Electrode sorting and clustering options

**Options:**
- Electrode sorting: by number, activity correlation, coordinate distance, ROI
- Clustering: k-means with adjustable number of clusters
- Export clusters to 3D viewer or electrodes.csv

#### By Electrode > By Condition Tab

| Output ID | Type | Content |
|-----------|------|---------|
| `by_electrode_custom_plot` | plotOutput2 | Per-electrode statistics by condition |

**Triggering reactives:**
- `local_reactives$update_by_electrode_custom_plot`
- `input$per_electrode_statistics_chooser`

#### Over Time Tab

| Output ID | Type | Content |
|-----------|------|---------|
| Multiple heatmaps | plotOutput2 | Frequency × Time heatmaps per condition |
| Line plots | plotOutput2 | Time-course traces with confidence bands |

**Triggering reactives:**
- `local_reactives$update_over_time_plot`
- Plot option panels for color range, line styles

#### By Condition Tab

| Output ID | Type | Content |
|-----------|------|---------|
| `by_condition_by_trial` | plotOutput2 | Trial-level scatter/bar plots with statistics |

**Triggering reactives:**
- `local_reactives$update_by_condition_plot`
- Click interactions for trial outlier marking

---

## 3. Data Export and Reports

### 3.1 Reading Pipeline Results in R

#### Using ravepipeline

```r
# Load pipeline (use temporary = TRUE to avoid registry conflicts)
pipeline <- ravepipeline::pipeline("power_explorer", temporary = TRUE)

# Read omnibus results
omnibus <- pipeline$read("omnibus_results")
# Returns: list(
#   stats = data.frame (electrode × statistic),
#   data = list (by-condition data)
# )

# Read baselined power
baselined <- pipeline$read("baselined_power")
# Returns: FileArray (Frequency × Time × Trial × Electrode)

# Read analysis groups
groups <- pipeline$read("analysis_groups")
# Returns: list of group definitions with trials

# Read over-time data for plotting
otbe <- pipeline$read("over_time_by_electrode_data")
```

#### Using ravecore (High-Level)

```r
# Load subject repository directly
subject <- ravecore::as_rave_subject("demo/DemoSubject")
repository <- ravecore::prepare_subject_power_with_epochs(
  subject = subject,
  electrodes = 1:20,
  epoch_name = "auditory_onset",
  reference_name = "default",
  time_windows = c(-1, 2)
)

# Access power data
repository$power$data_list  # List of electrode arrays
repository$epoch$table      # Epoch/trial table
repository$subject$power_sample_rate
```

#### File Locations

| Data | Location |
|------|----------|
| Power data | `{rave_data}/data/{project}/{subject}/rave/preprocess/power/{block}` |
| Epoch files | `{rave_data}/data/{project}/{subject}/rave/meta/epoch_*.csv` |
| Reference schemes | `{rave_data}/data/{project}/{subject}/rave/meta/reference_*.csv` |
| Electrode metadata | `{rave_data}/data/{project}/{subject}/rave/meta/electrodes.csv` |

---

### 3.2 Built-in Export Functions

#### CSV Export

| Output ID | `btn_export_electrodes` |
|-----------|------------------------|
| Format | CSV |
| Filename | `{project}-{subject}-power_export_{timestamp}.csv` |

**Contents:**
- Baseline-corrected power values
- Configurable frequency/time/trial dimensions
- ROI filtering options

**Export Options:**
- Frequencies: Collapsed (averaged) or Raw (all)
- Times: Collapsed, Analysis window(s) only, or All available
- Trials: Collapsed by grouping factors, or Raw

#### Save for Group Analysis

| Output ID | `save_pipeline_for_group_analysis` |
|-----------|-----------------------------------|
| Location | Subject's pipeline directory |
| Format | RAVE pipeline fork |

Saves the current pipeline state with:
- All current settings
- Computed results
- Label for identification

---

### 3.3 Available Reports

From `report-list.yaml`:

| Report Name | Entry File | Label |
|-------------|------------|-------|
| `univariatePower` | `report-univariate.Rmd` | "Univariate Power Spectrogram Analysis" |

#### Report: univariatePower

**Content:**
- Summary of analysis settings
- Baseline parameters
- Per-electrode statistics tables
- Visualization plots

**How to Generate:**

1. **Manual**: Click "Generate reports" button in module header
2. **Programmatic**:

```r
pipeline <- ravepipeline::pipeline("power_explorer", temporary = TRUE)
ravedash::report_wizard$generate(
  subject = pipeline$read("repository")$subject,
  report_name = "univariatePower"
)
```

**Parameters:**
- Uses current pipeline settings
- Requires completed analysis (omnibus_results)
