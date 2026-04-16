# notch_filter Module Reference

Removes electric line noise (e.g., 60 Hz and harmonics) from iEEG signals using notch filters. Displays Welch periodograms for before/after comparison and generates diagnostic reports.

---

## 1. Pipeline Interface

This module uses `ravepipeline` for computation. The pipeline definition is in `main.Rmd`, with the interpreted make file at `make-notch_filter.R` showing target dependencies.

### 1.1 Prerequisites

| Requirement | Description |
|-------------|-------------|
| **Imported LFP signals** | Raw signals must be imported via `import_lfp_native` ("Import Signals → Native Structure") module. The subject's `data_imported` flag must be `TRUE` for at least one electrode. |
| **Valid project/subject** | A RAVE project and subject must exist. |

#### Loader Components

The loader UI (`R/loader.R`, `R/aaa-presets.R`) collects:

| Component | Purpose |
|-----------|---------|
| `loader_project` | Project name selector |
| `loader_subject` | Subject code selector |
| `loader_sync1` | Sync from "Import Signals → Native Structure" module |
| `loader_sync2` | Sync from recent project/subject selection |

On successful load, the pipeline runs `imported_electrodes` target to validate data availability.

---

### 1.2 Pipeline Settings

Settings are stored in `settings.yaml`. Access via `pipeline$get_settings()` and update via `pipeline$set_settings()`.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `project_name` | string | `"test"` | RAVE project name |
| `subject_code` | string | `"DemoSubject"` | Subject identifier |
| `notch_filter_lowerbound` | numeric[] | `[59, 118, 178]` | Lower bounds of notch bands (Hz) |
| `notch_filter_upperbound` | numeric[] | `[61, 122, 182]` | Upper bounds of notch bands (Hz) |
| `channel_types` | string[] | `["LFP"]` | Channel types to filter (LFP always included) |
| `background` | string | `"#ffffff"` | Background color for plots |
| `diagnostic_plot_path` | string | `"{subject$note_path}/notch_filter.pdf"` | Output path for diagnostic PDF (glue template) |
| `diagnostic_plot_params` | object | See below | Parameters for diagnostic visualizations |

#### diagnostic_plot_params

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `window_length` | numeric | `2` | Welch periodogram window length (seconds) |
| `max_frequency` | numeric | `300` | Maximum frequency to display (Hz) |
| `histogram_bins` | numeric | `60` | Number of FFT histogram bins |
| `background` | string | `"#ffffff"` | Plot background color |
| `foreground` | string | `"#212529"` | Plot foreground color |
| `font_size` | numeric | `2.0` | Font size multiplier |
| `quiet` | logical | `FALSE` | Suppress progress messages |

---

### 1.3 Key Targets

Main targets defined in `main.Rmd`:

| Target | Export | Dependencies | Description |
|--------|--------|--------------|-------------|
| `load_subject` | `subject` | project_name, subject_code | Creates `RAVESubject` instance |
| `check_imported_electrodes` | `imported_electrodes` | subject | Validates imported signals exist; returns electrode IDs |
| `check_filter_settings` | `filter_settings` | notch_filter_lowerbound, notch_filter_upperbound | Validates lb/ub pairing; returns `list(lb, ub, domain)` |
| `check_electrode_channels_to_filter` | `channels_to_apply_filters` | imported_electrodes, channel_types, subject | Filters electrode list by channel type |
| `apply_Notch_filters` | `apply_notch` | subject, imported_electrodes, filter_settings, channels_to_apply_filters | **Main computation**: applies notch filters, writes to H5 |
| `generate_diagnostic_plots` | `diagnostic_plots` | subject, imported_electrodes, diagnostic_plot_params, diagnostic_plot_path | Generates diagnostic PDF |

> See `make-notch_filter.R` for detailed target dependency tree and implementation.

#### Target Details

**`apply_notch`** (main computation):
- Reads raw signals from `raw/{block}` in each electrode H5 file
- Applies `ravetools::notch_filter()` in parallel via `ravepipeline::lapply_jobs()`
- Writes filtered signals to `notch/{block}` in each H5 file
- Updates subject preprocess settings: `notch_filtered = TRUE`
- Returns: `list(electrodes, filter_applied, bounds, timestamp)`

**`diagnostic_plots`**:
- Generates PDF with Welch periodograms (raw vs. filtered)
- Skips if `diagnostic_plot_path` is empty
- Returns: PDF file path or `FALSE` if skipped

#### When to Run Each Target

| Situation | Targets to Run |
|-----------|----------------|
| Validate settings before applying | `filter_settings`, `channels_to_apply_filters` |
| Apply notch filters | `apply_notch` |
| Generate diagnostic PDF | `diagnostic_plots` |
| Full pipeline | All targets (or just `apply_notch` which depends on others) |

---

## 2. Module UI Interface

The UI is built with `ravedash` and `shidashi`. Module UI logic is in `R/module_html.R`, `R/module_server.R`, and `R/loader.R`. The loader ensures prerequisites are met before showing main content.

### 2.1 Input Components

Input IDs below are without the module namespace prefix (i.e., `"electrode"` not `"notch_filter--electrode"`).

#### Filter Settings Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `notch_filter_base_freq` | numericInput | "Base frequency (Hz)" | `60` | Min: 1, Step: 1 |
| `notch_filter_times` | textInput | "x (Times)" | `"1,2,3"` | Comma-separated multipliers |
| `notch_filter_bandwidth` | textInput | "+- Bandwidth (Hz)" | `"1,2,2"` | Comma-separated half-widths |
| `notch_filter_channel_types` | selectInput (multi) | "Additional channel types" | `character(0)` | Choices: "Spike", "Auxiliary" |
| `notch_filter_btn` | actionButton | "Apply Notch filters" | — | Triggers filter application |

#### Preview Output

| outputId | Type | Description |
|----------|------|-------------|
| `notch_filter_preview` | uiOutput | Shows computed filter bands (e.g., "Filter 1: 59.0Hz - 61.0Hz") |

#### Inspection Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `block` | selectInput | "Block" | — | Data block to inspect |
| `electrode` | selectInput | "Electrode" | — | Electrode number |
| `previous_electrode` | actionButton | "Previous" | — | Navigate electrodes |
| `next_electrode` | actionButton | "Next" | — | Navigate electrodes |
| `pwelch_winlen` | sliderInput | "Window length (seconds)" | `2` | Range: 0-4, Step: 0.1 |
| `pwelch_freqlim` | sliderInput | "Frequency limit" | `300` | Range: 20-1000 (max updates dynamically) |
| `pwelch_nbins` | sliderInput | "Number of histogram bins" | `60` | Range: 20-200, Step: 5 |

#### Download

| outputId | Type | Label |
|----------|------|-------|
| `download_as_pdf` | downloadLink | "Download as PDF" |

---

### 2.2 UI to Pipeline Mapping

#### Input Validation

Validation rules (via `shinyvalidate::InputValidator`):

| Input | Rule |
|-------|------|
| `notch_filter_base_freq` | Must be a positive number |
| `notch_filter_times` | Comma/space-separated numerics |
| `notch_filter_bandwidth` | Comma/space-separated numerics; length must match `times` |

#### Frequency Conversion Formula

The UI uses a simplified input format that gets converted to pipeline settings:

```
center_frequencies = base_freq × times
notch_filter_lowerbound = center_frequencies - bandwidth
notch_filter_upperbound = center_frequencies + bandwidth
```

**Example:**

| UI Input | Value |
|----------|-------|
| `base_freq` | 60 |
| `times` | "1,2,3" → [1, 2, 3] |
| `bandwidth` | "1,2,2" → [1, 2, 2] |

| Pipeline Setting | Calculation | Result |
|------------------|-------------|--------|
| `notch_filter_lowerbound` | [60×1-1, 60×2-2, 60×3-2] | [59, 118, 178] |
| `notch_filter_upperbound` | [60×1+1, 60×2+2, 60×3+2] | [61, 122, 182] |

#### Channel Types

- LFP channels are **always included**
- Additional types from `notch_filter_channel_types` are appended
- Combined list written to `channel_types` setting

---

### 2.3 Pipeline Execution Flow

#### Stage 0: Load Repository

- **Goal**: Load subject data repository and validate prerequisites
- **Trigger**: "Load subject" button in loader
- **Targets**: `imported_electrodes`
- **Result**: If successful, fires `data_changed` event → `check_data_loaded()` validates → main UI shown

```r
pipeline$run(names = "imported_electrodes", as_promise = FALSE)
```

#### Stage 1: Validation

- **Goal**: Validate filter parameters before applying
- **Trigger**: "Apply Notch filters" button clicked
- **Targets**: `filter_settings`, `channels_to_apply_filters`
- **Result**: Shows confirmation dialog with subject ID, electrodes, and filter frequencies

```r
pipeline$run(
  names = c("filter_settings", "channels_to_apply_filters"),
  scheduler = "none", type = "vanilla",
  async = FALSE, as_promise = TRUE
)
```

User clicks "Confirm" to proceed or "Cancel" to abort.

#### Stage 2: Application

- **Goal**: Apply notch filters and persist results
- **Trigger**: "Confirm" button clicked in dialog
- **Targets**: `apply_notch`
- **Result**: Filtered data written to H5 files, diagnostic report auto-generated, UI refreshed

```r
pipeline$run(
  names = "apply_notch",
  scheduler = "none", type = "smart",
  async = FALSE, as_promise = TRUE
)
```

After completion:
- Updates `diagnostic_plot_params` from current UI values
- Forks pipeline to subject's data path
- Generates diagnostic report in background

---

### 2.4 Outputs and Visualizations

#### Primary Output: signal_plot

| Property | Value |
|----------|-------|
| Output ID | `signal_plot` |
| Type | `plotOutput` |
| Function | `diagnose_notch_filters()` from `R/shared-diagnose_plot.R` |
| Content | Side-by-side Welch periodograms (raw vs. filtered) |

**Triggering reactives:**
- `local_reactives$refresh` — triggers on data load or filter application
- `input$block`, `input$electrode` — electrode/block selection changes
- `input$pwelch_winlen`, `input$pwelch_freqlim`, `input$pwelch_nbins` — visualization param changes

**Parameters:**
- `blocks`: from `input$block`
- `electrodes`: from `input$electrode`
- `max_freq`: from `input$pwelch_freqlim`
- `winlen`: from `input$pwelch_winlen`
- `nbins`: from `input$pwelch_nbins`
- `bg`, `fg`: from current Shiny theme via `ravedash::current_shiny_theme()`

#### Preview Output: notch_filter_preview

| Property | Value |
|----------|-------|
| Output ID | `notch_filter_preview` |
| Type | `uiOutput` |
| Content | HTML list of filter bands to apply |

**Triggering reactives:**
- `input$notch_filter_base_freq`, `input$notch_filter_times`, `input$notch_filter_bandwidth`

Example output: "Filter 1: 59.0Hz - 61.0Hz", "Filter 2: 118.0Hz - 122.0Hz"

#### UI State Changes

- "Filter settings" card auto-collapses after successful filter application
- Input fields are populated with previously saved parameters when `ravedash::watch_data_loaded()` fires

---

## 3. Data Export and Reports

### 3.1 Reading Pipeline Results in R

#### Using ravepipeline

```r
# Load pipeline (use temporary = TRUE to avoid registry conflicts)
pipeline <- ravepipeline::pipeline("notch_filter", temporary = TRUE)

# Read apply_notch result
result <- pipeline$read("apply_notch")
# Returns: list(
#   electrodes = integer vector,
#   filter_applied = logical vector,
#   bounds = list(lb = numeric, ub = numeric),
#   timestamp = POSIXct
# )

# Read other targets
subject <- pipeline$read("subject")
imported_electrodes <- pipeline$read("imported_electrodes")
filter_settings <- pipeline$read("filter_settings")
```

#### Using ravecore (High-Level)

```r
# Load subject via ravecore
subject <- ravecore::RAVESubject$new(
  project_name = "demo",
  subject_code = "YAB"
)

# Access preprocess settings
subject$preprocess_settings$notch_filtered
subject$preprocess_settings$notch_params
```

#### Direct H5 File Access (via ieegio)

Filtered signals are stored in electrode H5 files:

```r
# Path to electrode file
h5_path <- file.path(subject$preprocess_path, "voltage", "electrode_1.h5")

# Read filtered signal for a block
filtered_signal <- ieegio::io_read_h5(h5_path, "notch/block001")

# Read raw signal for comparison
raw_signal <- ieegio::io_read_h5(h5_path, "raw/block001")

# List available data paths
ieegio::io_h5_names(h5_path)
```

#### File Locations

| Data | Location |
|------|----------|
| Raw signals | `{preprocess_path}/voltage/electrode_{n}.h5` → `raw/{block}` |
| Filtered signals | `{preprocess_path}/voltage/electrode_{n}.h5` → `notch/{block}` |
| Preprocess settings | `{preprocess_path}/rave.yaml` |
| Diagnostic PDF | `{subject$note_path}/notch_filter.pdf` (default) |

---

### 3.2 Built-in Export Functions

#### PDF Download Handler

| Property | Value |
|----------|-------|
| Output ID | `download_as_pdf` |
| Filename | `{project_name}-{subject_code}-Notch_filter_diagnostic_plots.pdf` |
| Format | PDF |

**Contents:**
- Welch periodograms for all imported electrodes across all blocks
- Before (raw) and after (notch-filtered) comparison
- Uses current UI settings for visualization parameters

**Process:**
1. Updates `diagnostic_plot_params` in pipeline settings with current UI values
2. Runs targets: `diagnostic_plots`
3. Copies generated PDF to download

---

### 3.3 Available Reports

From `report-list.yaml`:

| Report Name | Entry File | Label |
|-------------|------------|-------|
| `diagnostics` | `report-diagnostics.Rmd` | "Notch Filter Diagnostic Plots" |

#### Report: diagnostics

**Content:**
- HTML report with Welch periodograms
- Shows before/after comparison for all electrodes and blocks
- Includes filter parameters used

**How to Generate:**

1. **Automatic**: Generated in background after successful filter application
2. **Manual**: Click "Generate reports" button in module header
3. **Programmatic**:

```r
pipeline <- ravepipeline::pipeline("notch_filter", temporary = TRUE)
ravedash::report_wizard$generate(
  subject = pipeline$read("subject"),
  report_name = "diagnostics"
)
```

**Parameters:**
- Uses `diagnostic_plot_params` from pipeline settings
- Output location configurable via `diagnostic_plot_path`

---

## Typical Workflow

### Via UI

1. **Load subject**
   - Select project and subject in loader
   - Verify "imported electrodes" count shown

2. **Configure filter parameters**
   - Set base frequency (60 Hz for US, 50 Hz for Europe)
   - Set harmonics via "Times" (e.g., "1,2,3" for fundamental + 2 harmonics)
   - Set bandwidths (e.g., "1,2,2" for ±1 Hz at 60 Hz, ±2 Hz at harmonics)
   - Optionally include additional channel types (Spike, Auxiliary)

3. **Apply filters**
   - Click "Apply Notch filters"
   - Review confirmation dialog (subject, electrodes, frequencies)
   - Click "Confirm" to execute

4. **Verify results**
   - Inspect Welch periodograms in the Inspection panel
   - Navigate electrodes with Previous/Next buttons
   - Adjust visualization parameters (window length, frequency limit)

5. **Export**
   - Click "Download as PDF" for diagnostic plots
   - Reports auto-generated; access via "Generate reports" button

### Via Scripts

```bash
# Read current settings
Rscript set_inputs.R notch_filter

# Update settings
Rscript set_inputs.R notch_filter '{"project_name":"demo","subject_code":"YAB"}'
Rscript set_inputs.R notch_filter '{"notch_filter_lowerbound":[59,118,178],"notch_filter_upperbound":[61,122,182]}'

# Check target status
Rscript get_targets.R notch_filter

# Run filters
Rscript run.R notch_filter --targets=apply_notch

# Generate diagnostics
Rscript run.R notch_filter --targets=diagnostic_plots

# Read results
Rscript get_results.R notch_filter --target=apply_notch
```

---

## Notes

- **Line noise frequencies:**
  - 60 Hz (Americas, parts of Asia): bounds `[59,118,178]` / `[61,122,182]`
  - 50 Hz (Europe, Africa, most of Asia): bounds `[49,98,148,198]` / `[51,102,152,202]`
  - Typically the range is 0-200 Hz because most analyses focus on frequencies no more than 200 Hz. However, some HFO requires analysis up to 500 Hz


- **Array validation:** Lower/upper bound arrays must have equal length, with each lower bound strictly less than the corresponding upper bound.

- **Parallel processing:** The `apply_notch` target uses `ravepipeline::lapply_jobs()` for parallel electrode processing.

- **Idempotent application:** Re-running the filter overwrites previous filtered data. The `notch_filtered` flag is updated in preprocess settings.
