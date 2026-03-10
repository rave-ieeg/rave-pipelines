# notch_filter Module Reference

Removes electric line noise (e.g. 60 Hz and harmonics) from iEEG signals using notch filters.

## settings.yaml Keys

| Key | Type | Description | Example |
|-----|------|-------------|---------|
| `subject_code` | string | Subject identifier | `"DemoSubject"` |
| `project_name` | string | RAVE project name | `"test"` |
| `notch_filter_lowerbound` | numeric[] | Lower bounds of notch bands (Hz) | `[59, 118, 178]` |
| `notch_filter_upperbound` | numeric[] | Upper bounds of notch bands (Hz) | `[61, 122, 182]` |
| `channel_types` | string | Signal type to filter | `"LFP"` |
| `diagnostic_plot_path` | string | Output path for diagnostic PDF (glue template) | `"{subject$note_path}/notch_filter.pdf"` |
| `diagnostic_plot_params` | object | Diagnostic plot parameters (see below) | |
| `background` | string | Background color | `"#ffffff"` |

### diagnostic_plot_params

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `window_length` | numeric | 2 | Welch periodogram window length (seconds) |
| `max_frequency` | numeric | 300 | Maximum frequency to display (Hz) |
| `histogram_bins` | numeric | 60 | Number of histogram bins |
| `background` | string | `"#ffffff"` | Plot background color |
| `foreground` | string | `"#212529"` | Plot foreground color |
| `font_size` | numeric | 2.0 | Font size multiplier |
| `quiet` | logical | no | Suppress progress messages |

## Pipeline Targets

| Target | Dependencies | Description |
|--------|-------------|-------------|
| `settings_path` | — | Watches `settings.yaml` for changes |
| `settings` | `settings_path` | Loads settings from YAML |
| `subject_code` | `settings` | Extracts subject code |
| `project_name` | `settings` | Extracts project name |
| `notch_filter_lowerbound` | `settings` | Extracts lower bounds |
| `notch_filter_upperbound` | `settings` | Extracts upper bounds |
| `diagnostic_plot_path` | `settings` | Extracts diagnostic plot path |
| `diagnostic_plot_params` | `settings` | Extracts diagnostic plot parameters |
| `channel_types` | `settings` | Extracts channel types |
| `background` | `settings` | Extracts background color |
| `subject` | `project_name`, `subject_code` | Creates RAVESubject instance |
| `imported_electrodes` | `subject` | Validates imported electrodes exist |
| `filter_settings` | `notch_filter_lowerbound`, `notch_filter_upperbound` | Validates and structures filter bands |
| `channels_to_apply_filters` | `imported_electrodes`, `channel_types`, `subject` | Determines which electrodes to filter based on type |
| `apply_notch` | `subject`, `imported_electrodes`, `filter_settings`, `channels_to_apply_filters` | Applies notch filters to electrode H5 files |
| `diagnostic_plots` | `subject`, `diagnostic_plot_path`, `diagnostic_plot_params`, `imported_electrodes` | Generates Welch periodogram diagnostic PDF |

## Typical Workflow

1. Set subject and filter parameters: `set_inputs.R notch_filter --subject_code=YAB --project_name=demo`
2. Check target status: `get_targets.R notch_filter`
3. Run the filter: `run.R notch_filter --targets=apply_notch`
4. Generate diagnostics: `run.R notch_filter --targets=diagnostic_plots`
5. Read results: `get_results.R notch_filter --target=apply_notch`

## Notes

- The `apply_notch` target writes filtered signals to `notch/{block}` in each electrode's H5 file and marks electrodes as notch-filtered in preprocess settings.
- Lower/upper bound arrays must have equal length, with each lower bound strictly less than the corresponding upper bound.
- For 60 Hz line noise: use bounds `[59,118,178]` / `[61,122,182]` (fundamental + 2 harmonics).
- For 50 Hz line noise: use bounds `[49,98,148]` / `[51,102,152]`.
