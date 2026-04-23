# project_overview Module Redesign

## Overview

Replace the current Quarto/iframe-based project overview with a Shiny-native module. Pipeline writes RDS data under the module's `cache/{project}/` directory. Output panel uses two cardsets (group-level and subject-level) with dynamic footer controls. Export copies cache to `build/`, renders HTML alongside RDS, rewires links to relative paths, and zips.

**Current**: Loader → Quarto website build → iframe display → zip export
**New**: Loader (project + subjects + section config) → Pipeline (RDS snapshots to cache/) → Cardset output panel (DT tables + standalone links) → Export (build/ with index.html + relative links)

## Directory Structure

### Cache (pipeline output, under module dir)

```
cache/{project_name}/
  group/
    snapshot.rds              # group-level summary data
    group_viewer.html         # merged brain viewer (threeBrain)
    electrode_coverage.rds    # FSLabel × subjects matrix
  subjects/
    {subject_code}/
      snapshot.rds            # subject metadata (electrodes, epochs, refs, validation)
      viewer.html             # individual brain viewer
      reports/
        manifest.rds          # data.frame of discovered reports for this subject
```

### Build (export output, same structure + rendered HTML + copied reports)

```
build/{project_name}/
  index.html                  # rendered overview with relative links
  group/
    group_viewer.html
    electrode_coverage.html   # rendered DT table from RDS
    subjects_summary.html     # rendered summary table
  subjects/
    {subject_code}/
      viewer.html
      validation.html         # rendered validation table
      reports/
        {report_dir}/report.html  # copied from subject$report_path
```

## Implementation Steps

### Phase 1: Loader UI

- [ ] **1. Rewrite `loader_html()` in R/loader.R**
  - `ravedash::input_card("Project")`: single-project selector
  - `ravedash::input_card("Subjects")`: subject multi-selector (default all), template brain selector, clear-cache checkbox
  - `ravedash::input_card("Group-Level Sections")`: checkboxes for `group_viewer`, `electrode_coverage`, `subjects_metadata`
  - `ravedash::input_card("Subject-Level Sections")`: checkboxes for `module_reports` (+ module filter multi-selector), `native_viewer`, `validation`
  - "Generate Report" button in footer

- [ ] **2. Rewrite `loader_server()` in R/loader.R**
  - Observer: project change → `updateSelectInput` for subjects (from `project$subjects()`)
  - Observer: subjects change → scan for available module reports → populate module-filter choices
  - "Generate Report" button → save settings to `settings.yaml`, run `pipeline$run_as_task()`

### Phase 2: Pipeline Targets (RDS to cache/)

- [ ] **3. Rewrite `main.Rmd`** — 3 pipeline targets:
  - `validate_inputs`: validate project_name, subject_codes, section config
  - `generate_snapshots`: per-subject `snapshot_subject()` → `cache/{project}/subjects/{sub}/snapshot.rds` + `viewer.html`; group `snapshot_project()` → `cache/{project}/group/group_viewer.html` + `electrode_coverage.rds` + `snapshot.rds`
  - `collect_report_manifest`: scan `subject$report_path` per subject → per-subject `manifest.rds`

- [ ] **4. Update R/shared-scripts.R**
  - Remove `generate_website()` (all Quarto logic)
  - Keep `ensure_brain_template()`
  - Add `discover_existing_reports(subject, module_filter = NULL)` — scans `subject$report_path`, returns data.frame (report_name, module, timestamp, file_path)
  - Add `get_cache_dir(project_name)` — returns `file.path(pipeline$pipeline_path, "cache", project_name)`

- [ ] **5. Update R/shared-column-builders.R**
  - Parameterize `cache_root`: accept as argument instead of global
  - `read_subject_snapshot(subject_code, cache_dir)` reads from `cache_dir/subjects/{subject_code}/snapshot.rds`
  - Keep `snapshot_subject()`, `snapshot_project()`, `subjects_columns`, column builders

### Phase 3: Output Panel (Two Cardsets)

- [ ] **6. Rewrite R/module_html.R** — two cardsets, no iframe:
  - **Group cardset** (`ravedash::output_cardset`): tabs for `3D Viewer` (link button), `Electrode Coverage` (DT), `Subjects Summary` (DT). Footer: `uiOutput` for tab-specific controls.
  - **Subject cardset** (`ravedash::output_cardset`): tabs for `3D Viewer` (link), `Module Reports` (DT), `Validation` (DT). Footer: `uiOutput` with **subject selector** + tab-specific controls.
  - Only include tabs for sections enabled in config.

- [ ] **7. Rewrite R/module_server.R**
  - Read validated config and cache data on `ravedash::watch_data_loaded()`
  - **Group cardset footer**: `renderUI` switches on `input$group_cardset` — "Open in new window" button for viewer tab, filter controls for table tabs
  - **Group tab renderers**: 3D Viewer → link `?module=standalone_report&snapshot=1&...`; Electrode Coverage → `DT::renderDataTable()`; Subjects Summary → `DT::renderDataTable()` using `subjects_columns`
  - **Subject cardset footer**: `renderUI` with subject selector (`selectInput`) + `switch(input$subject_cardset, ...)` for tab-specific controls
  - **Subject tab renderers**: 3D Viewer → link to subject viewer; Module Reports → `DT::renderDataTable()` from `manifest.rds` with clickable links; Validation → `DT::renderDataTable()` from `snapshot$validation`

- [ ] **8. Update R/aa.R**
  - `check_data_loaded()`: check for `cache/{project}/group/snapshot.rds` instead of `build/_site/index.html`
  - Remove `addResourcePath` / `static_path_added` / iframe logic

### Phase 4: Export (build/ folder)

- [ ] **9. Create R/export-helpers.R**
  - `consolidate_export(project_name, cache_dir, build_dir, sections_config)`:
    1. Copy `cache/{project}/` → `build/{project}/`
    2. Copy report HTMLs from `subject$report_path` → `build/.../reports/{report_dir}/`
    3. Render static HTMLs from RDS (electrode_coverage.html, subjects_summary.html, validation.html)
    4. Render `index.html` via `rmarkdown::render()` on `report-export.Rmd`
    5. Zip `build/{project}/`
  - `generate_static_table(rds_path, output_html)` — renders RDS as self-contained HTML with DT

- [ ] **10. Create report-export.Rmd**
  - Lightweight template for export `index.html`
  - Group sections: link to `group/group_viewer.html`, embedded electrode coverage DT, subjects summary
  - Subject sections: per-subject links to viewers, reports, validation tables
  - All relative paths

- [ ] **11. Export handler in R/module_server.R**
  - "Export Report" button → run `consolidate_export()` in background job
  - On completion → download modal with `downloadHandler` for the zip

### Phase 5: Cleanup

- [ ] **12. Delete old files**
  - `project_page_template.qmd`
  - `report-project.rmd`
  - `report_styles.css`

- [ ] **13. Update DESCRIPTION**
  - Remove `quarto` from Imports
  - Ensure `DT`, `threeBrain`, `htmltools`, `data.table`, `rmarkdown` are listed

- [ ] **14. Update module-ui.html**
  - Remove `resizeIframe()` JS function (no more iframes)
  - Keep navbar, loader/main toggle, "Export Report" footer

- [ ] **15. Update settings.yaml**
  ```yaml
  project_name: "demo"
  subject_codes: []
  template_subject: "fsaverage_inCIT168"
  use_cache: yes
  group_sections:
    group_viewer: true
    electrode_coverage: true
    subjects_metadata: true
  subject_sections:
    module_reports: true
    module_filter: []
    native_viewer: true
    validation: true
  ```

## Reference Patterns

- `reference_module/R/module_html.R` (lines 95-150): `card_tabset` with dynamic footer via `uiOutput`
- `reference_module/R/module_server.R`: `renderUI` footer that switches on `input$cardset_id`
- `power_explorer/R/module_server.R` (lines ~2589-2610): standalone_report URL construction
- `standalone_report/R/loader.R` (lines 38-55): snapshot mode; (lines 67-84): report discovery
- `power_explorer/R/module_html.R` (lines 516-560): `output_cardset` with tools

## Verification

1. **Rebuild pipeline**: Knit `main.Rmd` or run `build_pipeline(make_file = "make-project_overview.R")` to regenerate `make-project_overview.R` with new target names
2. Loader: select project → subjects populate → configure sections → generate
3. Cache: `cache/{project}/group/` and `cache/{project}/subjects/{sub}/` populated with RDS + HTML
4. Group cardset: viewer link opens new tab, DT tables interactive, footer adapts per tab
5. Subject cardset: subject selector in footer switches data, reports with working links, validation table
6. Export: `build/` mirrors cache + rendered HTMLs + copied reports + `index.html` with relative links. Zip works offline.
7. Edge cases: no subjects, no module reports, unchecked sections → tabs absent

## Architecture Note

- **Viewer HTML files** are saved to `project$group_path("project_overview")/snapshot/` (the project's data repository) so that `standalone_report` module can serve them via its snapshot mode URL pattern
- **RDS snapshots** are saved to `cache/{project}/` under the module directory for fast local access
- This split is because standalone_report reads from `project$group_path("project_overview")/snapshot/{path}` and these paths are not configurable
