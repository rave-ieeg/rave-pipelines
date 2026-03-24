# custom_3d_viewer Module Reference

Views a subject brain with cortical surfaces overlaid by MRI slices and electrodes. Electrodes can be colored by uploaded data tables (CSV/FST), animated over time, and mapped to template brains. Supports drag-and-drop of NIfTI volumes, FreeSurfer surfaces, and color maps. The 3D viewer is powered by the `threeBrain` R package (https://dipterix.org/threeBrain/) and `three-brain-js` JavaScript engine (https://github.com/dipterix/three-brain-js).

---

## 1. Pipeline Interface

This module uses `ravepipeline` for computation. The pipeline definition is in `main.rmd`, with the interpreted make file at `make-custom_3d_viewer.R` showing target dependencies.

### 1.1 Prerequisites

| Requirement | Description |
|-------------|-------------|
| **FreeSurfer reconstruction** | Folder must exist at `<rave_dir>/raw_dir/<subject>/rave-imaging/fs`. Required for cortical surfaces, volumes, and coordinate transforms. |
| **Valid project/subject** | A RAVE project and subject must exist. The project can be set to `[Auto]` to auto-detect. |
| **Electrode coordinates** (if visualizing electrodes) | An electrode table with `Coord_x`, `Coord_y`, `Coord_z`, `Electrode`, and `Label` columns. Can come from the subject's `electrodes.csv` or be uploaded in various coordinate systems (see Section 4 for coordinate system details). |

#### Loader Components

The loader UI (`R/loader.R`, `R/aaa-presets.R`) collects:

| Component | Purpose |
|-----------|---------|
| `loader_project` | Project name selector |
| `loader_subject` | Subject code selector |
| `loader_electrode_source` | Source of electrode coordinates (meta directory or file upload in various coordinate systems) |
| `loader_electrode_tbl_upload` | CSV file upload for custom electrode coordinates |
| `loader_volume_types` | Additional volume overlays to load (e.g., `aparc.DKTatlas+aseg`, `aparc.a2009s+aseg`). Atlas/parcellation files are sourced from `rave-imaging/fs/mri/` (`.nii`, `.nii.gz`, or `.mgz`). |
| `loader_surface_types` | Additional surface types to load (e.g., `smoothwm`, `inflated`, `white`, `pial-outer-smooth`) |
| `loader_annot_types` | Additional surface annotations/measurements |
| `loader_use_spheres` | Whether to use sphere contacts instead of prototypes |
| `loader_override_radius` | Override contact radius (requires sphere contacts enabled) |
| `loader_use_template` | Whether to use template brain (N27) |

#### Electrode Coordinate Systems

When uploading electrode coordinates, the following coordinate systems are supported:

| Source | Coordinate System | Required Columns |
|--------|-------------------|------------------|
| Subject meta directory | tkrRAS (auto) | `Coord_x`, `Coord_y`, `Coord_z`, `Electrode`, `Label` |
| File upload - auto | tkrRAS | `Coord_x`, `Coord_y`, `Coord_z` (or `x`, `y`, `z`), `Electrode`, `Label` |
| File upload - Scanner RAS | ScannerRAS | `x`, `y`, `z`, `name` or `Label` |
| File upload - tk-registered (FreeSurfer) RAS | tkrRAS | `x`, `y`, `z`, `name` or `Label` |
| File upload - MNI152 RAS | MNI152 | `x`, `y`, `z`, `name` or `Label` |

See Section 4 (Coordinate Systems Reference) for details on tkrRAS, ScannerRAS, and MNI152.

Required: `Electrode` (integer index; may be auto-generated for file uploads but must be present and stable for all data linkage). Optional: `Radius` (contact radius in mm).

---

### 1.2 Pipeline Settings

Settings are stored in `settings.yaml`. Access via `pipeline$get_settings()` and update via `pipeline$set_settings()`.

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `project_name` | string | `"[Auto]"` | RAVE project name (`[Auto]` for auto-detection) |
| `subject_code` | string | `"DemoSubject"` | Subject identifier |
| `coordinate_sys` | string | `""` | Coordinate system used for electrodes: `"tkrRAS"`, `"ScannerRAS"`, `"MNI152"`, `"MNI305"` |
| `surface_types` | string[] | `["smoothwm", "inflated", "white", "pial-outer-smooth"]` | Surface types to load (pial and sphere.reg are always loaded) |
| `overlay_types` | string | `"aparc+aseg"` | Atlas/volume overlay type |
| `annot_types` | string/null | `null` | Surface annotations to load |
| `use_spheres` | logical | `FALSE` | Use sphere contacts instead of prototype geometry |
| `override_radius` | numeric/NA | `NA` | Override electrode contact radius (mm) |
| `use_template` | logical | `FALSE` | Use template brain (merge individual brain onto N27) |
| `data_source` | string | `"Uploads"` | Data source for electrode values: `"Uploads"`, `"Saved pipelines/modules"`, `"None"` |
| `uploaded_source` | string | `""` | Filename of the uploaded data (relative to subject imaging uploads folder) |
| `data_source_project` | string | `""` | Project name for saved pipeline data source |
| `data_source_pipeline` | string | `""` | Pipeline identifier for saved pipeline data source |
| `data_source_pipeline_target` | string | `""` | Target variable name in the saved pipeline |
| `controllers` | list | `[]` | Viewer controller states (saved and restored) |
| `main_camera` | list | `[]` | Camera state: `position` (xyz), `zoom`, `up` (xyz) |
| `shiny_outputId` | string | `""` | Shiny outputId for viewer readiness signal |

---

### 1.3 Key Targets

Main targets defined in `main.rmd`:

| Target | Export | Dependencies | Description |
|--------|--------|--------------|-------------|
| `get_valid_project_name` | `loaded_brain` | subject_code, project_name, overlay_types, surface_types, annot_types, use_spheres, override_radius, coordinate_sys, use_template | Loads the brain object with surfaces, volumes, and electrodes |
| `render_initial_viewer` | `initial_brain_widget` | loaded_brain, shiny_outputId, controllers, main_camera | Renders the initial 3D viewer widget (no electrode data) |
| `find_data_path` | `path_datatable` | loaded_brain, data_source, uploaded_source, data_source_project, data_source_pipeline, data_source_pipeline_target | Resolves the path to the electrode value data |
| `load_data_table` | `brain_with_data` | loaded_brain, path_datatable, data_source | Loads electrode values from the data table into the brain |
| `render_viewer` | `brain_widget` | brain_with_data, shiny_outputId, controllers, main_camera | Renders the final 3D viewer widget with electrode data |

> See `make-custom_3d_viewer.R` for detailed target dependency tree and implementation.

#### Target Details

**`loaded_brain`** (main brain loader):
- Creates a `RAVESubject`, then calls `ravecore::rave_brain()` to load FreeSurfer surfaces/volumes
- Optionally merges onto template brain via `threeBrain::merge_brain()`
- Sets electrode positions from the uploaded/meta electrode table via `brain$set_electrodes()`
- Returns: `list(brain, subject_code, project_name, electrode_table, surface_types, overlay_types, ...)`

**`brain_with_data`** (electrode value assignment):
- Reads data from CSV/FST file (Uploads mode) or from a saved pipeline target
- Data must be a data.frame with `Electrode` column; additional columns become display variables
- If `Time` column exists, handles animation data (aggregates duplicate time-electrode pairs)
- Calls `brain$set_electrode_values(loaded_datatable)` to assign values
- Returns: `list(brain, variables)` where `variables` are the available display data column names

**`brain_widget`** (final viewer render):
- Calls `brain$plot()` with saved controller states and camera position
- Restores background color, zoom level, camera position/up vectors
- Injects custom JavaScript to signal Shiny when viewer is ready

#### When to Run Each Target

| Situation | Targets to Run |
|-----------|----------------|
| Load/reload brain model | `loaded_brain`, `initial_brain_widget` |
| Update electrode data display | `path_datatable`, `brain_widget` |
| Full re-render with data | `path_datatable`, `brain_widget` |

---

## 2. Module UI Interface

The UI is built with `ravedash` and `shidashi`. Module UI logic is in `R/module_html.R`, `R/module_server.R`, and `R/loader.R`. The loader ensures prerequisites are met before showing main content.

### 2.1 Input Components

Input IDs below are without the module namespace prefix (i.e., `"data_source"` not `"custom_3d_viewer--data_source"`).

#### Data Selector Section

| inputId | Type | Label | Default | Notes |
|---------|------|-------|---------|-------|
| `data_source` | selectInput | "Data source" | `"Uploads"` | Options: "Uploads", "None" |
| `uploaded_source` | selectInput | "Select an uploaded data" | — | FST/CSV files in subject's uploads folder |
| `uploaded_file` | fancyFileInput | "Upload csv/fst/xlsx table" | — | Shown when `[New Uploads]` is selected |
| `download_template_btn` | actionLink | "Show/Download a template table" | — | Shows template data examples |
| `data_source_project` | selectInput | "Select a project" | — | For "Saved pipelines" mode |
| `data_source_pipeline` | selectInput | "Select a saved pipeline" | — | For "Saved pipelines" mode |
| `data_source_pipeline_target` | selectInput | "Select a target variable" | — | For "Saved pipelines" mode |
| `viewer_reset` | actionLink | "Reset controller option" | — | Resets viewer controllers and re-generates |

#### Viewer Status Panel

| outputId | Type | Description |
|----------|------|-------------|
| `viewer_status` | uiOutput | Shows surface type, anatomical clip plane, camera zoom, and selected electrode info |
| `viewer_selected_data` | plotOutput | Time-series plot of selected electrode data (flip side of status panel) |

#### Main Viewer

| outputId | Type | Description |
|----------|------|-------------|
| `viewer` | threejsBrainOutput | The 3D brain viewer, occupies 9/12 columns |

---

### 2.2 UI to Pipeline Mapping

#### Electrode Value Table Formats

Three template types are supported:

| Template | Required Columns | Description |
|----------|-----------------|-------------|
| Simple property | `Electrode`, + value column(s) | Single value per electrode (e.g., t-value, p-value) |
| Multiple properties | `Electrode`, + multiple value columns | Multiple data columns; `NA` for missing values |
| Animation | `Electrode`, `Time`, + value column(s) | Time-series data for animation; `Time` in seconds |

All tables uploaded are converted to FST format and stored in the subject's imaging custom-data folder.

#### Data Source Resolution

| Data Source | How Path is Resolved |
|-------------|---------------------|
| `Uploads` | `<subject_imaging_path>/custom-data/<uploaded_source>` |
| `Saved pipelines/modules` | Reads specified target from a previously saved pipeline directory |
| `None` | No electrode data loaded |

---

### 2.3 Pipeline Execution Flow

#### Stage 0: Load Brain Model

- **Goal**: Load brain surfaces, volumes, and electrode positions
- **Trigger**: "Load subject" button in loader
- **Targets**: `loaded_brain`, `initial_brain_widget`
- **Result**: If successful, fires `data_changed` event → `check_data_loaded()` validates → main UI shown

```r
pipeline$run(
  as_promise = TRUE,
  names = c("loaded_brain", "initial_brain_widget"),
  scheduler = "none", type = "vanilla", callr_function = NULL
)
```

#### Stage 1: Generate Viewer with Data

- **Goal**: Load electrode value data and render the 3D viewer
- **Trigger**: "Re-generate the viewer" link or "RAVE!" button (bottom-right)
- **Targets**: `path_datatable`, `brain_widget`
- **Result**: 3D viewer updated with electrode colors/animation

```r
pipeline$run(
  as_promise = FALSE,
  scheduler = "none", type = "vanilla", callr_function = NULL,
  names = c("path_datatable", "brain_widget")
)
```

The viewer preserves controller states and camera position across regenerations by reading them from the `brain_proxy` and saving back to pipeline settings.

---

### 2.4 Outputs and Visualizations

| Output ID | Type | Description | Data Source |
|-----------|------|-------------|-------------|
| `viewer` | threejsBrainOutput | Interactive 3D brain viewer | `brain_widget` target |
| `viewer_status` | uiOutput | Viewer metadata: surface type, crosshair MNI coordinates, camera zoom, selected electrode info | `brain_proxy` reactive |
| `viewer_selected_data` | plotOutput | Time-course plot or cumulative category count for double-clicked electrode | Electrode data from FST table |

#### Interactive Features

- **Single-click electrode**: Selects and highlights an electrode; shows electrode info in status panel
- **Double-click electrode**: Loads electrode value data and displays time-course plot (flip panel)
- **Click on time-course plot**: Sets the viewer animation time to the clicked time point
- **Brain proxy**: `threeBrain::brain_proxy("viewer")` provides reactive access to camera, controllers, and mouse events

---

## 3. The 3D Viewer: Features and Controllers

The 3D viewer is a WebGL-based interactive viewer with a control panel on the right side. Controllers are organized into folders.

### 3.1 Controller Reference

Controllers can be set programmatically via `brain$plot(controllers = list(...))` or via `brain_proxy$set_controllers(list(...))`.

#### Default / General

| Controller | Type | Values | Description |
|------------|------|--------|-------------|
| `Background Color` | color | hex string (e.g., `"#FFFFFF"`) | Viewer background color |
| `Camera Position` | select | `"[free rotate]"`, `"right"`, `"left"`, `"anterior"`, `"posterior"`, `"superior"`, `"inferior"` | Preset camera angles |
| `Display Coordinates` | boolean | `TRUE`/`FALSE` | Show coordinate display overlay |
| `Record` | button | — | Start screen recording |
| `Show Panels` | boolean | `TRUE`/`FALSE` | Show/hide side slice panels |

#### Volume Settings

| Controller | Type | Values | Description |
|------------|------|--------|-------------|
| `Slice Mode` | select | `"canonical"`, `"column-row-slice"` | How MRI slices are displayed |
| `Slice Brightness` | numeric | 0–1 | Brightness of MRI slice overlays |
| `Coronal (P - A)` | numeric | — | Coronal slice position (posterior to anterior) |
| `Axial (I - S)` | numeric | — | Axial slice position (inferior to superior) |
| `Sagittal (L - R)` | numeric | — | Sagittal slice position (left to right) |
| `Overlay Coronal` | boolean | `TRUE`/`FALSE` | Show coronal slice overlay on 3D view |
| `Overlay Axial` | boolean | `TRUE`/`FALSE` | Show axial slice overlay on 3D view |
| `Overlay Sagittal` | boolean | `TRUE`/`FALSE` | Show sagittal slice overlay on 3D view |
| `Frustum Near` | numeric | 0–1 | Near clipping plane distance |
| `Frustum Far` | numeric | 0–1 | Far clipping plane distance |
| `Voxel Display` | select | `"hidden"`, `"normal"`, `"anat. slices"` | How volume voxels are displayed |
| `Voxel Opacity` | numeric | 0–1 | Opacity of voxel display |
| `Voxel Min` | numeric | — | Minimum voxel value threshold |
| `Voxel Max` | numeric | — | Maximum voxel value threshold |
| `Voxel Label` | string | — | Current voxel label at crosshair |

#### Surface Settings

| Controller | Type | Values | Description |
|------------|------|--------|-------------|
| `Surface Material` | select | `"MeshPhysicalMaterial"`, `"MeshLambertMaterial"` | Surface lighting model |
| `Surface Type` | select | `"pial"`, `"white"`, `"smoothwm"`, `"inflated"`, `"pial-outer-smooth"` | Active surface type |
| `Clipping Plane` | boolean | `TRUE`/`FALSE` | Enable surface clipping |
| `Left Hemisphere` | select | `"normal"`, `"hidden"`, `"wireframe"` | Left hemisphere display mode |
| `Right Hemisphere` | select | `"normal"`, `"hidden"`, `"wireframe"` | Right hemisphere display mode |
| `Left Opacity` | numeric | 0–1 | Left hemisphere opacity |
| `Right Opacity` | numeric | 0–1 | Right hemisphere opacity |
| `Left Mesh Clipping` | boolean | — | Enable left hemisphere mesh clipping |
| `Right Mesh Clipping` | boolean | — | Enable right hemisphere mesh clipping |
| `Surface Color` | select | `"none"`, `"vertices"`, `"sync from voxels"`, `"sync from electrodes"` | How surface is colored |
| `Blend Factor` | numeric | 0–1 | Blend factor for electrode-to-surface color mapping |
| `Sigma` | numeric | — | Gaussian spread for electrode-to-surface mapping |
| `Decay` | numeric | — | Distance decay for electrode-to-surface mapping |
| `Range Limit` | numeric | — | Maximum distance for electrode-to-surface mapping |

#### Electrode Settings

| Controller | Type | Values | Description |
|------------|------|--------|-------------|
| `Visibility` | select | `"all visible"`, `"threshold only"`, `"hide inactives"`, `"hidden"` | Electrode visibility filter |
| `Electrode Shape` | select | `"prototype+sphere"`, `"sphere"`, `"prototype"` | Contact rendering style |
| `Outlines` | boolean | — | Show electrode outlines |
| `Text Scale` | numeric | — | Electrode label text size |
| `Text Visibility` | boolean | — | Show/hide electrode text labels |

#### Electrode Data & Display

| Controller | Type | Values | Description |
|------------|------|--------|-------------|
| `Display Data` | select | (available data column names) | Which data variable colors the electrodes |
| `Display Range` | string | `"min,max"` | Color range for continuous data |
| `Threshold Data` | select | (available data column names) | Variable used for thresholding |
| `Threshold Range` | string | `"min,max"` | Threshold range |
| `Threshold Method` | select | — | How thresholding is applied |
| `Additional Data` | select | — | Secondary data overlay |

#### Electrode Mapping (Template)

| Controller | Type | Values | Description |
|------------|------|--------|-------------|
| `Map Electrodes` | boolean | `TRUE`/`FALSE` | Enable mapping to template brain |
| `Surface Mapping` | select | `"sphere.reg"`, `"mni305"`, `"mni305+shift"`, `"mni305.affine"`, `"no mapping"` | Surface mapping method |
| `Volume Mapping` | select | `"mni305"`, `"mni305.affine+fix.target"`, `"mni305.affine"`, `"sphere.reg"`, `"no mapping"` | Volume mapping method |
| `Projection Threshold` | numeric | 0–30 | Distance threshold for surface projection |

#### Animation

| Controller | Type | Values | Description |
|------------|------|--------|-------------|
| `Show Legend` | boolean | `TRUE`/`FALSE` | Show color legend |
| `Show Time` | boolean | `TRUE`/`FALSE` | Show current time stamp |
| `Time` | numeric | — | Current animation time (seconds) |
| `Speed` | string | — | Playback speed multiplier |
| `Highlight Box` | boolean | `TRUE`/`FALSE` | Show highlight box around selected electrode |
| `Info Text` | string | — | Custom info text overlay |

---

### 3.2 Keyboard Shortcuts

Mouse controls: left-drag = free arcball rotation; right-drag = pan; scroll = zoom. Hold `Shift` / `Ctrl`/`Cmd` / `Alt` during left-drag to constrain rotation to vertical / horizontal / roll axis respectively.

Localization keys (`` ` ``, `1`–`4`) are only active inside the electrode localization module.

| Key | Category | Description |
|-----|----------|-------------|
| `p` | Display | Toggle side panel |
| `z` | Display | Zoom out |
| `Z` | Display | Zoom in |
| `m` | Slice | Cycle slice mode: `canonical` → `line-of-sight` → `snap-to-electrode` → `column-row-slice` |
| `Shift+C` | Slice | Toggle coronal slice overlay |
| `Shift+A` | Slice | Toggle axial slice overlay |
| `Shift+S` | Slice | Toggle sagittal slice overlay |
| `e` | Slice | Move coronal slice +1 mm |
| `E` | Slice | Move coronal slice −1 mm |
| `q` | Slice | Move axial slice +1 mm |
| `Q` | Slice | Move axial slice −1 mm |
| `w` | Slice | Move sagittal slice +1 mm |
| `W` | Slice | Move sagittal slice −1 mm |
| `[` | Surface | Cycle left hemisphere material |
| `]` | Surface | Cycle right hemisphere material |
| `Shift+[` | Surface | Cycle left hemisphere opacity |
| `Shift+]` | Surface | Cycle right hemisphere opacity |
| `Shift+,` (`<`) | Surface | Cycle left hemisphere mesh clipping |
| `Shift+.` (`>`) | Surface | Cycle right hemisphere mesh clipping |
| `k` | Surface | Cycle surface color type (none → vertices → sync from electrodes → …) |
| `Shift+P` | Surface | Cycle surface types (pial → white → inflated → …) |
| `Shift+M` | Surface | Cycle surface material / lighting model |
| `c` | Surface | Change clipping plane |
| `a` | Volume | Change displayed voxel volume |
| `l` | Volume | Cycle voxel / atlas display mode |
| `.` | Electrode | Select next electrode |
| `,` | Electrode | Select previous electrode |
| `v` | Electrode | Cycle electrode visibility |
| `Shift+V` | Electrode | Toggle electrode label visibility |
| `o` | Electrode | Cycle electrode outline options |
| `Shift+O` | Electrode | Cycle electrode shapes |
| `s` | Animation | Play / pause animation |
| `d` | Animation | Cycle animation clips / values forward |
| `D` | Animation | Cycle animation clips / values backward |
| `t` | Animation | Sync threshold with current animation clip name |
| `r` | Tools | Enable ruler |
| `R` | Tools | Reset ruler |
| `Cmd+Opt+Shift+D` | Dev | Toggle debug mode |
| `Cmd+Opt+Shift+H` | Dev | Toggle hidden gems / special features |
| `` ` `` | Localization | Register electrode from crosshair |
| `1` / `Shift+1` | Localization | Adjust electrode R (right) / L (left) |
| `2` / `Shift+2` | Localization | Adjust electrode A (anterior) / P (posterior) |
| `3` / `Shift+3` | Localization | Adjust electrode S (superior) / I (inferior) |
| `4` / `Shift+4` | Localization | Rotate electrode shaft (quaternion) |

---

### 3.3 Drag & Drop

Files are dragged onto the **drag-and-drop drop zone** in the control panel sidebar (not directly onto the 3D canvas — dropping on the canvas causes a browser error). For a full tutorial, see [rave.wiki: Drag & Drop](https://rave.wiki/posts/image_widgets/viewer401.html).

| File Type | Formats | What Happens |
|-----------|---------|-------------|
| **NIfTI volumes** | `.nii`, `.nii.gz`, `.mgz` | Loaded as additional volume overlay with visibility, opacity, color mode, and clipping controls |
| **FreeSurfer surfaces** | `.pial`, `.white`, `.inflated`, surface files | Loaded as additional surface with visibility, opacity, and color controls |
| **Surface annotations** | FreeSurfer annotation files | Loaded as surface color overlays |
| **Atlas volumes** | `.nii`, `.nii.gz`, `.mgz` (discrete) | Loaded as atlas; can also generate surface objects from atlas labels |
| **Tractography / DTI fibers** | `.tck`, `.trk`, `.tt` | Rendered as 3D fiber tract streamlines |
| **Electrode coordinates** | `.csv` with coordinate columns | Adds or updates electrode positions |
| **Electrode value tables** | `.csv` with `Electrode` column | Updates electrode display data for coloring/animation |
| **Electrode color maps** | `*_colormap.csv` | Updates electrode color palettes (see format below) |
| **Surface color maps** | `surface_color.csv` (dragged with surfaces) | Assigns colors to drag-dropped surfaces (see format below) |
| **Viewer state JSON** | `.json` | Programmatically sets all viewer controller states (camera, display, thresholds, etc.) |
| **State transition file** | `.json` (transition format) | Animates between viewer states over time |

Once a file is dragged in, a "Drag & Drop" folder appears in the control panel with per-file controllers: (the following options are for volumes and maybe surfaces & tracks. NOT for electrodes)

- **Visibility**: Show/hide the dragged item
- **Opacity**: Transparency control
- **Color Mode**: `"single color"`, `"continuous"`, `"discrete"` (for volumes)
- **Color Map**: Choose from built-in continuous or discrete color lookup tables (for surfaces & volumes). **This does NOT affect electrode color palettes** — electrode palettes are changed via `*_colormap.csv` drag & drop (see Electrode Colormap CSV Format below).
- **Value Clipping**: Min/max thresholds for continuous volumes

> **Clear all**: Buttons to clear all dragged volumes or surfaces are provided at the top of the Drag & Drop folder.

#### Electrode Coordinate CSV Format

To override or supply electrode positions via drag & drop, prepare a CSV with at least `Electrode`, `Label`, and coordinate columns. The coordinate interpretation depends on how the file is loaded (auto-detect uses `tkrRAS`; alternatively select Scanner RAS, tkrRAS, or MNI152 mode in the loader). Optional columns (`SignalType`, `Hemisphere`, `LabelPrefix`) are preserved in the electrode table.

```
Electrode,Coord_x,Coord_y,Coord_z,Label,SignalType,Hemisphere,LabelPrefix
13,-42.3,12.1,-8.5,GRID01,LFP,left,GRID
14,-45.1,14.7,-6.2,GRID02,LFP,left,GRID
15,-47.8,17.3,-4.0,GRID03,LFP,left,GRID
16,-50.2,19.8,-1.7,GRID04,LFP,left,GRID
24,-38.6,22.4,2.1,DEPTH01,LFP,left,DEPTH
```

> Use column names `x`/`y`/`z` as aliases for `Coord_x`/`Coord_y`/`Coord_z`. Use `Channel` as an alias for `Electrode`.

#### Electrode Value CSV Format

To color electrodes by one or more static values, prepare a CSV with an `Electrode` column plus any number of value columns. Use `NA` for missing values. Discrete (categorical) and continuous columns can be mixed.

```
Subject,Electrode,GroupLabel,tValue
Demo,13,GroupA,1.23
Demo,14,GroupB,-0.87
Demo,15,GroupA,2.45
Demo,16,GroupB,NA
Demo,24,GroupA,-1.10
```

In the viewer, each non-`Electrode`/`Subject` column appears as a selectable variable in `Display Data` and `Threshold Data`.

#### Electrode Animation CSV Format

To animate electrode colors over time, add a `Time` column (seconds). Each electrode-time pair is one row. Duplicate electrode-time pairs are averaged automatically.

```
Subject,Electrode,Time,Amplitude
Demo,13,0.1,0.82
Demo,13,0.2,-0.34
Demo,13,0.5,1.56
Demo,14,0.1,-1.02
Demo,14,0.2,0.71
Demo,14,0.5,0.09
Demo,15,0.1,1.44
Demo,15,0.2,-0.88
Demo,15,0.5,2.01
Demo,16,0.1,0.33
Demo,16,0.2,-0.52
Demo,16,0.5,0.77
Demo,24,0.1,-0.65
Demo,24,0.2,1.18
Demo,24,0.5,-1.33
```

Use the `Time` controller or the animation play button to scrub through time steps.

#### Electrode Colormap CSV Format

In the RAVE module UI, drag-and-drop of a `*_colormap.csv` file is the supported way to customize electrode color palettes. To do so, prepare a CSV file whose name ends with `_colormap.csv`. The first row must contain a single column name matching the variable to color (e.g., `LabelPrefix`). Each subsequent row is a hex color value. The colors are assigned in order to the unique levels of the variable.

Example `my_colormap.csv`:
```
LabelPrefix
#FF0000
#00FF00
#0000FF
#FFFF00
```

#### Surface Color CSV Format

To assign custom colors to drag-dropped FreeSurfer surfaces, prepare a CSV file with exactly two columns (case-sensitive):

| Column | Description |
|--------|-------------|
| `Filename` | Surface filename **without** extension (e.g., `lh.pial` not `lh.pial.gii`) |
| `Color` | Hex color string (e.g., `#FF0000`) |

Drag and drop the surface files **together with** this `surface_color.csv` file onto the viewer.

#### Viewer State JSON (Drag & Drop)

Save the current viewer state as JSON, then drag and drop the JSON file onto the viewer to restore controller settings programmatically. This is useful for:
- Reproducing exact viewer configurations across sessions
- Setting `Voxel Min`/`Voxel Max` thresholds programmatically
- Configuring camera position, display data, and all controller states at once

See the [rave.wiki viewer drag & drop tutorial](https://rave.wiki/posts/image_widgets/viewer401.html) for the JSON format specification.

---

### 3.4 Surface Color Mapping from Electrodes

To project electrode values onto the brain surface:

1. Load a brain with electrode values assigned
2. In the **Surface Settings** panel, set `Surface Color` to `"sync from electrodes"` (or press `k` to cycle)
3. Adjust mapping parameters:
   - **Blend Factor**: How strongly electrode colors mix with the surface (0–1)
   - **Sigma**: Gaussian spread of the color mapping from electrode positions
   - **Decay**: Distance decay rate
   - **Range Limit**: Maximum distance (mm) for the mapping to apply

To hide electrode contacts and show only the mapped values, set `Visibility` to `"hidden"` in the Electrode Settings panel.

```r
brain$plot(
  controllers = list(
    "Surface Color" = "sync from electrodes",
    "Blend Factor" = 0.57,
    "Decay" = 0.05,
    "Range Limit" = 6.0,
    "Visibility" = "hidden"
  )
)
```

---

## 4. Coordinate Systems Reference

RAVE always uses RAS (right-anterior-superior) coordinate convention. The `electrodes.csv` file contains multiple coordinate columns with different meanings. Software that uses LPS convention (e.g., AFNI, ITK, BrainLab) requires flipping the sign of the first two coordinates.

### 4.1 Electrode Coordinate Columns

| Column(s) | Coordinate System | Description | Availability |
|-----------|-------------------|-------------|--------------|
| `T1R`, `T1A`, `T1S` | Scanner RAS (native T1 space) | Native MRI scanner coordinates — always consistent regardless of preprocessing pipeline | Always available |
| `Coord_x`, `Coord_y`, `Coord_z` | tkrRAS (FreeSurfer surface RAS) | FreeSurfer tk-registered coordinates, aligned to `*h.pial` surfaces. This is the primary coordinate system used by the 3D viewer. | Requires FreeSurfer reconstruction |
| `OrigCoord_x`, `OrigCoord_y`, `OrigCoord_z` | tkrRAS (pre-shift) | Original coordinates before brain-shift correction. Equals `Coord_xyz` if no shift correction was applied. | Set during localization |
| `MNI152_x`, `MNI152_y`, `MNI152_z` | MNI152 RAS | Template-space coordinates. Accuracy depends on preprocessing: YAEL + recon-all = nonlinear (most accurate), recon-all only = affine, simple-import = unavailable. | Depends on preprocessing |
| `MNI305_x`, `MNI305_y`, `MNI305_z` | MNI305 RAS | Older MNI template space (Talairach-like). Computed from FreeSurfer's `talairach.xfm`. | Requires FreeSurfer reconstruction |
| `MRVoxel_*` | Voxel indices | Internal use only — do not rely on these for analysis. | Internal |

### 4.2 Coordinate Transforms

To convert between Scanner RAS (T1) and tkrRAS (FreeSurfer surface coordinates), use the brain object's transform matrices:

```r
brain <- ravecore::rave_brain(
  subject = ravecore::RAVESubject$new(
    project_name = "[Auto]",
    subject_code = "<subject>"
  )
)

# T1 Scanner RAS --> FreeSurfer tkrRAS
# tkrRAS = Torig %*% solve(Norig) %*% T1_RAS
xfm <- brain$Torig %*% solve(brain$Norig)

# Apply: scanner_ras is a 3-element vector
tkr_ras <- as.vector(xfm %*% c(scanner_ras, 1))[1:3]

# Inverse: tkrRAS --> Scanner RAS
inv_xfm <- solve(xfm)
scanner_ras <- as.vector(inv_xfm %*% c(tkr_ras, 1))[1:3]
```

### 4.3 MNI152 Coordinate Accuracy

The accuracy of `MNI152_xyz` coordinates depends on how the subject was preprocessed:

| Preprocessing Pipeline | MNI152 Method | Accuracy |
|------------------------|---------------|----------|
| **YAEL + recon-all** | Nonlinear (ANTs SyN) | Best — recommended for group analysis |
| **recon-all only** | Affine (Talairach transform) | Moderate — uses FreeSurfer's linear registration |
| **Simple import** | Not available | No MNI152 coordinates generated |

For group viewers, electrodes are displayed in MNI space. The template brain is selected during subject loading. Use nonlinear normalization (YAEL) for best spatial accuracy across subjects.

---

## 5. Imaging File Structure

### 5.1 FreeSurfer Folder Structure

The FreeSurfer reconstruction must be located at:
```
~/rave_data/raw_dir/<subject>/rave-imaging/fs/
```

Required contents for the 3D viewer:

| Path (under `fs/`) | Purpose |
|---------------------|---------|
| `mri/T1.mgz` | Anatomical MRI (used for slice overlays) |
| `mri/aparc+aseg.mgz` | Atlas parcellation (required for atlas overlays and electrode labeling) |
| `surf/lh.pial`, `surf/rh.pial` | Pial surfaces |
| `surf/lh.white`, `surf/rh.white` | White matter surfaces |
| `surf/lh.sphere.reg`, `surf/rh.sphere.reg` | Sphere registration (for template mapping) |

If FreeSurfer was run externally (not via RAVE), copy the output folder into `rave-imaging/fs` **before** launching RAVE modules. Atlas labels will not be available until `aparc+aseg.mgz` is present.

### 5.2 Volume Underlay Priority

The viewer selects the MRI underlay image using this priority chain (first found wins):

| Priority | Filename | Extensions checked |
|----------|----------|--------------------|
| 1 | `rave_slices` | `.nii`, `.nii.gz`, `.mgz` |
| 2 | `brain.finalsurfs` | `.nii`, `.nii.gz`, `.mgz` |
| 3 | `brain` | `.nii`, `.nii.gz`, `.mgz` |
| 4 | `T1` | `.nii`, `.nii.gz`, `.mgz` |

Files are searched in the `fs/mri/` directory. To use a custom T1 underlay, copy your volume to the `fs/mri/` folder and rename it to `rave_slices.nii` or `rave_slices.nii.gz`.


---

## 6. Electrode Geometry and Prototypes

### 6.1 Available Prototypes

List all available electrode geometry prototypes:

```r
threeBrain::list_electrode_prototypes()
```

Major manufacturers and naming conventions:

| Manufacturer | ID Pattern | Example |
|-------------|------------|---------|
| AdTech | `SEEG-ADTECH-*` | Various depth electrode models |
| DIXI | `SEEG-DIXI-*` | DIXI Medical sEEG electrodes |
| PMT | `SEEG-PMT-2102-XX-SPXXX` | PMT Corporation depth electrodes |
| NeuroOne / Zimmer | `SEEG-ZIMMER-NSEEGXXXX-RF` | NeuroOne sEEG electrodes |

When the electrode prototype is not available, the viewer falls back to sphere contacts. Use `loader_use_spheres = TRUE` to force sphere rendering, or `loader_override_radius` to set a custom radius.

---

## 7. Common Workflows

### Workflow 1: Visualize Electrode Locations

1. Load subject in the loader (select project, subject)
2. Electrode coordinates auto-detected from `electrodes.csv`
3. Click "Load subject"
4. The viewer shows electrodes on the brain surface

### Workflow 2: Color Electrodes by Uploaded Data

1. Load subject (Workflow 1)
2. In "Data selector", ensure "Uploads" is selected
3. Select `[New Uploads]` and upload a CSV with `Electrode` + value columns
4. Click "Re-generate the viewer"
5. In the viewer control panel, use `Display Data` to select the variable
6. Adjust `Display Range` for continuous data

### Workflow 3: Animate Electrode Time-Series

1. Prepare a CSV with `Subject`, `Electrode`, `Time`, and value columns
   - **`Subject`** must match the loaded subject code exactly
   - **`Electrode`** numbers must match those in the electrode table
   - **`Time`** must contain valid numeric values (seconds)
   - If any of these are missing or mismatched, the affected electrodes will not render with color
2. Upload and re-generate viewer
3. Use the animation controls (play/pause, speed) in the viewer
4. Double-click an electrode to see its time-course in the status panel

### Workflow 4: Add Volume Overlays via Drag & Drop

1. Drag a NIfTI file (`.nii`, `.nii.gz`) onto the 3D viewer canvas
2. A "Drag & Drop" folder appears in controls with per-file settings
3. Adjust visibility, opacity, color mode, and value clipping
4. To remove, use "Clear all volumes" button

### Workflow 5: Visualize DTI Fiber Tracts

1. Drag a tractography file (`.tck`, `.trk`, or `.tt`) onto the 3D viewer canvas
2. Fiber streamlines are rendered in 3D
3. Adjust visibility and opacity in the "Drag & Drop" folder

### Workflow 6: Map Electrodes to Template Surface

1. In the viewer control panel, open "Electrode Mapping"
2. Enable `Map Electrodes`
3. Select surface mapping method (e.g., `sphere.reg`) and volume mapping method (e.g., `mni305`)
4. Electrodes are projected onto the template brain surface

### Workflow 7: Selectively Show/Hide Electrodes

Use the threshold mechanism to show only a subset of electrodes:

1. Prepare a CSV with columns: `Subject`, `Electrode`, `Selected` (values: `yes` or `no`)
2. Upload and re-generate the viewer
3. Set **`Threshold Data`** to `Selected`
4. Set **`Threshold Range`** to `yes`
5. Set **`Visibility`** to `"threshold only"` (or press `v` to cycle visibility modes)
6. Only electrodes where `Selected == "yes"` will be visible

This works with any categorical or numeric variable — the threshold mechanism filters electrodes by the value range you specify.

### Workflow 8: Group Viewer (Multi-Subject on Template)

1. In the loader, enable `Use template brain`
2. Electrodes are mapped to the template (N27) brain using MNI coordinates
3. For multi-subject data, the uploaded CSV must include a `Subject` column
4. **Note**: sEEG electrodes appearing outside the brain surface is expected — depth electrodes are implanted in deep structures
5. For best spatial accuracy, use nonlinear MNI152 normalization (YAEL + recon-all preprocessing)
6. A script for visualizing electrodes on template brains with external MNI coordinates is available at: https://github.com/rave-ieeg/rave-gists/blob/main/visualize-mni-in-template-brain.R

### Workflow 9: Export Viewer

- **From the viewer**: Use the "Record" button for screenshots/video. Video filenames are browser-controlled and cannot be specified from RAVE.
- **Programmatically**: Use `threeBrain::save_brain(widget, "output.html")` to save as standalone HTML
- **Electrode table export**: The viewer supports downloading electrode coordinates as CSV via control panel

---

## 8. Troubleshooting & FAQ

### Electrodes Not Visible on CT Volume

**Problem**: CT overlay is loaded but electrodes are not visible (appear hidden behind the threshold).

**Solution**: The default `Voxel Min` threshold (3000) may be too high for your CT scan. Lower the **`Voxel Min`** value in the **Volume Settings** controller until electrodes appear. **Do NOT change `Voxel Max`** — always keep it at the maximum value for localization accuracy.

### Animation Data Not Showing Colors

**Problem**: Uploaded time-series data but electrodes remain gray / no color animation.

**Checklist**:
1. Ensure the CSV contains **`Subject`**, **`Electrode`**, and **`Time`** columns
2. The `Subject` value must exactly match the loaded subject code (case-sensitive)
3. The `Electrode` numbers must match those in the subject's electrode table
4. `Time` values must be valid numbers (seconds), not strings
5. Check for missing/NA values in the data column

### FreeSurfer Atlases Not Available

**Problem**: Atlas overlays (aparc+aseg) are not showing or electrode labels are empty.

**Solution**: Ensure `aparc+aseg.mgz` exists at `~/rave_data/raw_dir/<subject>/rave-imaging/fs/mri/aparc+aseg.mgz`. If FreeSurfer was run externally, copy the output directory into the `rave-imaging/fs` path before launching RAVE modules.

### Electrodes Outside Brain in Group Viewer

**Problem**: In the group/template viewer, some electrodes appear floating outside the brain surface.

**Explanation**: This is **expected behavior** for sEEG (depth) electrodes — they are implanted in deep brain structures and may not project onto the cortical surface. Subdural grid/strip electrodes should appear on or near the surface.

For better spatial accuracy in group analysis, use nonlinear MNI152 normalization (YAEL + recon-all preprocessing) rather than affine transforms.

### Custom T1 Underlay Not Showing

**Problem**: Want to use a specific T1 image as the MRI underlay, but a different volume is displayed.

**Solution**: The viewer uses a priority chain (see Section 5.2). To force your custom T1, copy it to the `fs/mri/` folder and rename it to `rave_slices.nii` or `rave_slices.nii.gz`. This has the highest priority and will be used as the underlay.

### Voxel Min/Max Cannot Be Set Programmatically via UI

**Problem**: Need to set exact Voxel Min/Max values but the UI slider is imprecise.

**Solution**: Save a viewer state JSON file with the desired values and drag & drop it onto the viewer. See Section 3.3 (Viewer State JSON) and the [rave.wiki tutorial](https://rave.wiki/posts/image_widgets/viewer401.html) for the JSON format.

### How Do I Change the Color Palette for Electrodes?

**Problem**: Want to change the colors used to display electrode values.

**Solution**: Drag and drop a `*_colormap.csv` file onto the viewer (see Section 3.3 — Electrode Colormap CSV Format for the file format). Two common misconceptions:

- The **`Color Map`** selector that appears after dragging a NIfTI volume or surface into the viewer applies **only to that dragged file** — it has no effect on electrode color palettes.
- The **`brain_proxy`** object (used internally by the RAVE module) is not accessible to end users in the module UI.

---

## 9. Community References

Sources for these notes (from [RAVE GitHub Discussions](https://github.com/orgs/rave-ieeg/discussions)):

- **Drag & drop tutorial**: https://rave.wiki/posts/image_widgets/viewer401.html
- **MNI template visualization gist**: https://github.com/rave-ieeg/rave-gists/blob/main/visualize-mni-in-template-brain.R
- **Selective electrode visibility**: Discussion #128
- **Animation data requirements**: Discussion #132
- **Coordinate column meanings**: Discussions #57, #143
- **Electrode colormap CSV format**: Discussion #109
- **Surface color CSV format**: Discussion #101

---

## 10. Programmatic Usage (R)

### 10.1 Creating a Brain and Rendering

```r
# Load a RAVE subject brain
subject <- ravecore::RAVESubject$new(project_name = "demo", subject_code = "DemoSubject")
brain <- ravecore::rave_brain(subject)

# Or create directly with FreeSurfer path
brain <- threeBrain::freesurfer_brain2(
  fs_subject_folder = "~/rave_data/raw_dir/DemoSubject/rave-imaging/fs",
  subject_name = "DemoSubject"
)

# Add additional surfaces
brain$add_surface("smoothwm")
brain$add_surface("inflated")
brain$add_surface("sphere.reg")

# Add atlas overlays
brain$add_atlas("aparc+aseg")
brain$add_atlas("aparc.DKTatlas+aseg")

# Plot
brain$plot()
```

### 10.2 Setting Electrode Coordinates

```r
# From subject meta directory (electrodes.csv with tkrRAS coordinates)
brain$set_electrodes("path/to/electrodes.csv")

# From a data.frame with custom coordinate system
electrode_table <- data.frame(
  Electrode = 1:5,
  Coord_x = c(-30.5, -28.2, -25.9, -23.6, -21.3),
  Coord_y = c(-10.1, -8.4, -6.7, -5.0, -3.3),
  Coord_z = c(15.2, 17.8, 20.4, 23.0, 25.6),
  Label = paste0("G", 1:5),
  Radius = rep(1.5, 5)  # optional
)

# With coordinate system specification
brain$set_electrodes(electrode_table, coord_sys = "tkrRAS")
# Other options: "scannerRAS", "MNI305", "MNI152"

# Use sphere contacts instead of prototype geometry
brain$set_electrodes(electrode_table, priority = "sphere")
```

### 10.3 Coloring Electrodes by Values

```r
# Simple: one value per electrode
electrode_values <- data.frame(
  Electrode = 1:5,
  tValue = c(2.3, -1.1, 3.5, 0.2, -0.8),
  PValue = c(0.02, 0.27, 0.001, 0.84, 0.42),
  Category = c("A", "A", "B", "B", "C")
)
brain$set_electrode_values(electrode_values)

# With custom color palettes
brain$plot(
  palettes = list(
    tValue = c("#053061", "#2166AC", "#F7F7F7", "#B2182B", "#67001F"),
    PValue = colorRampPalette(c("red", "yellow", "gray", "white"), bias = 3)(101),
    Category = c("orange", "dodgerblue3", "darkgreen")
  ),
  value_ranges = list(
    PValue = c(0, 1)
  ),
  controllers = list(
    "Display Data" = "tValue"
  )
)
```

### 10.4 Animation (Time-Series Data)

```r
time <- seq(-1, 2, length.out = 100)
animation_data <- data.frame(
  Subject = "DemoSubject",
  Electrode = rep(c(1, 2, 3), each = 100),
  Time = rep(time, 3),
  Amplitude = c(sin(time * 2 * pi), sin(time * 2 * pi + 1), cos(time * 2 * pi))
)

brain$set_electrode_values(animation_data)

brain$plot(
  controllers = list(
    "Display Data" = "Amplitude",
    "Show Time" = TRUE,
    "Speed" = "0.5"
  )
)
```

### 10.5 Custom Electrode Color Maps

Color palettes can be specified per variable:

```r
brain$plot(
  palettes = list(
    # Continuous: vector of colors (will be interpolated)
    ContinuousVar = c("#0000FF", "#FFFFFF", "#FF0000"),

    # Discrete: one color per factor level
    DiscreteVar = c("red", "green", "blue", "orange"),

    # Named palette from ravebuiltins (if available)
    Response = ravebuiltins::get_heatmap_palette("Spectral")
  ),
  value_ranges = list(
    ContinuousVar = c(-3, 3)  # symmetric range
  )
)
```

In the viewer UI, the `Display Data` controller selects which variable colors the electrodes. The `Display Range` controller (format `"min,max"`) clips the color range.

**Via drag-and-drop**: Drop a CSV file onto the viewer with columns matching variable names and hex color values to update the color map in real time.

### 10.6 Using Template Brains (Multi-Subject)

```r
brain <- ravecore::rave_brain(subject)

# Merge onto template (N27 by default)
template <- threeBrain::merge_brain(brain, template_surface_types = c("pial", "inflated"))

# The template object contains the mapped electrodes
template$plot()

# Set electrode values on the template
template$set_electrode_values(data.frame(
  Subject = "DemoSubject",
  Electrode = 1:5,
  Value = rnorm(5)
))
```

### 10.7 Exporting the Viewer

```r
# Save as standalone HTML file
widget <- brain$plot()
threeBrain::save_brain(widget, "~/Downloads/viewer.html")

# Or using htmlwidgets
htmlwidgets::saveWidget(widget, "~/Downloads/viewer.html", selfcontained = TRUE)
```

### 10.8 Using Brain Proxy in Shiny

> **Scope**: `brain_proxy` is an **internal server-side object** created and managed within the RAVE module's own Shiny server code. It is **not accessible to end users** through the module UI. Do not suggest `proxy$...` methods in response to user questions about changing viewer settings interactively — users cannot call these functions. 

```r
# In server function:
proxy <- threeBrain::brain_proxy("viewer")

# Read current state
proxy$main_camera       # camera position, zoom, up
proxy$surface_type      # current surface type
proxy$controllers       # all controller values
proxy$plane_position    # slice positions
proxy$mouse_event_double_click  # last double-click event

# Set controllers reactively
proxy$set_controllers(list(
  "Display Data" = "tValue",
  "Surface Type" = "inflated",
  "Time" = 0.5
))

# Update electrode data dynamically
proxy$set_electrode_data(
  data = new_data_frame,
  palettes = list(NewVar = c("blue", "red")),
  value_ranges = list(NewVar = c(-5, 5))
)

# Set electrode color palette
proxy$set_electrode_palette(colors = c("blue", "white", "red"), variable = "tValue")

# Change background
proxy$set_background(col = "#000000")
```

---

### 10.9 Reading Pipeline Results in R

```r
# Using ravepipeline
pipeline <- ravepipeline::pipeline("custom_3d_viewer", temporary = TRUE)

# Read the brain object
loaded_brain <- pipeline$read("loaded_brain")
# Returns: list(brain, subject_code, project_name, electrode_table, ...)

# Read the brain with data
brain_with_data <- pipeline$read("brain_with_data")
# Returns: list(brain, variables)

# Read the viewer widget
brain_widget <- pipeline$read("brain_widget")
# Returns: htmlwidget that can be displayed in Viewer or saved to HTML
```

#### Electrode Value File Locations

Uploaded electrode value files are stored as FST format at:
```
<subject_imaging_path>/custom-data/<filename>.fst
```

Where `<subject_imaging_path>` is resolved by `ravecore::RAVESubject$imaging_path`.
- **Underlay image priority**: Discussion #111
- **CT threshold troubleshooting**: Discussion #116
- **Electrode alignment troubleshooting**: Discussion #125
- **FreeSurfer folder structure**: Discussion #118
- **Tractography file support**: Discussion #144
- **Viewer state JSON**: Discussions #145, #146
- **Group viewer / template brain**: Discussions #120, #132
- **Electrode prototypes**: Discussion #104
- **Recalculate labels**: Discussion #117
