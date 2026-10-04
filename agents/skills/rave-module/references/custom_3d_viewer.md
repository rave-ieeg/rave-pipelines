# Subject 3D Viewer Module Reference

Shows a subject's brain in 3D: cortical surfaces, MRI slices, atlases (volumes),
fiber tracts (streamlines), and electrodes. The electrodes can be colored by a
table of values, animated over time. A quick analysis measures how much each
fiber-tract bundle overlaps chosen regions (electrodes, surfaces, volumes). The
viewer is `threeBrain` (https://dipterix.org/threeBrain/).

**Prerequisite:** A FreeSurfer reconstruction in the subject's imaging folder
(`~/rave_data/raw_dir/<subject>/rave-imaging/fs`). Electrodes need the subject's
electrode table (`electrodes.csv`) or an uploaded coordinate table.

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: Choose the RAVE project and subject in the loader screen.

Step 2: Choose where the electrode coordinates come from ("Select a source of
electrode coordinates"). The default, "Subject meta directory -
electrodes.csv", uses the subject's electrode table. The "File upload - ..."
choices need an uploaded table (csv or tsv, case-sensitive columns):

| Choice | Coordinates | Columns |
|---|---|---|
| File upload - auto | FreeSurfer tkrRAS, otherwise T1 scanner RAS  | `Electrode`, `Label`, and `Coord_x`, `Coord_y`, `Coord_z` (for tkrRAS), or `T1R`, `T1A`, `T1S` for scanner RAS ; optional `Radius` |
| File upload - Scanner RAS | T1 scanner RAS | `name` or `Label`, `x`, `y`, `z`; optional `Electrode`, `Radius` |
| File upload - tk-registered (FreeSurfer) RAS | FreeSurfer tkrRAS | `name` or `Label`, `x`, `y`, `z`; optional `Electrode`, `Radius` |
| File upload - MNI152 RAS | MNI152 | `name` or `Label`, `x`, `y`, `z`; optional `Electrode`, `Radius` |

Typically "Subject meta directory - electrodes.csv" should be used: it will pull the "electrodes.csv" from subject directory, a file automatically generated from electrode localization module in RAVE. The file contains all the information. 

Step 3: Optional extras (the choices depend on the subject):

* **Additional volumes**: atlases or other volumes from `fs/mri`, e.g. `aparc+aseg`.
* **Additional surface types**: e.g. `smoothwm`, `inflated`, `white`,
  `pial-outer-smoothed`. The `pial` surface (and `sphere.reg`, when present) is
  always loaded, `inflated` is encouraged if the electrodes of interests are near cortical surfaces.
* **Additional surface annotations/measurements**: e.g. `label/aparc.a2009s.annot`.
* **Additional streamlines**: fiber-tract bundles, e.g. `alic/*` (a group).
  The quick analysis needs at least one.
* **Use spheres contacts**: draw contacts as spheres instead of electrode
  device shapes (prototypes).
* **Override contact radius** (mm, 0 to 10): the sphere radius; a value above 0
  also checks "Use spheres contacts". For sEEG (depth electrodes), radius of 1mm is proper; for
  sparse ECoG (surface electrodes), 2mm is good; for small high-density electrodes, calculate their pitch (distance between electrodes) and divide by three.
* **Use template brain**: also load the template brain, very rarely used unless the subject does not have brain images and coordinates are already MNI.

Step 4: Click the "Load subject" button. The viewer shows the brain and
electrodes, without electrode values. Loading resets the viewer's saved
controllers and camera.

### 2. Analysis inputs

**Electrode value selector** (left panel):

* **Data source** — "Uploads" (a table uploaded to the subject) or "None" (no
  values). Default: the last saved choice, usually "Uploads".
* **Select an uploaded data** — the subject's uploaded tables (`.csv`, `.fst`),
  newest first. "[New Uploads]" shows the upload box ("Upload csv/fst/xlsx
  table") and the "Show/Download a template table" link. An upload must have an
  `Electrode` column; it is saved as `.fst` in the subject's
  `rave-imaging/custom-data` folder, and a notification asks to regenerate the
  viewer.
* **Re-generate the viewer** (link) or **Re-generate & Visualize** (bottom-right
  button): saves the data choice and the viewer's current settings, then
  regenerates the viewer with the values.
* **Reset controller option** (link): regenerates the viewer with default
  controllers and camera.

The value table:

* `Electrode` (required): the electrode numbers of the electrode table.
* `Subject` (optional): only the rows of the loaded subject are used.
* `Time` (optional, seconds): animates the values. Rows with the same electrode
  and time (to 0.01 s) are merged; `Trial`, `Frequency`, and `Block` columns are
  then ignored.
* Every other column is a variable to display: numbers are continuous, text is
  categorical. `NA` marks missing values.

The template dialog offers three examples: "Simple property" (one value per
electrode), "Multiple properties" (several columns), and "Animation" (with
`Time`).

**Viewer controllers** (the control panel on the right of the viewer). Folders
and the commonly used controllers, as the viewer reports them (choices marked
"loaded" depend on the brain and data):

| Folder | Controller | Type | Choices or range |
|---|---|---|---|
| Default | `Background Color` | color | `#RRGGBB` |
| Default | `Camera Position` | option | `[free rotate]`, `[lock]`, `right`, `left`, `anterior`, `posterior`, `superior`, `inferior` (a direction moves the camera once, then shows `[free rotate]` again) |
| Default | `Display Coordinates` | boolean | the orientation compass |
| Default | `Focus Object Type` | option | `all`, `surface mesh`, `2D slice`, `3D voxel`, `streamline` (what focus mode picks) |
| Default | `Record` | boolean | records a video that the browser downloads |
| Volume Settings | `Show Panels` | boolean | the three slice panels |
| Volume Settings | `View Layout` | option | `3dview`, `sliceview-flat`, `sliceview-axial`, `sliceview-coronal`, `sliceview-sagittal`, `sliceview-twobytwo` |
| Volume Settings | `Slice Mode` | option | `canonical`, `line-of-sight`, `snap-to-electrode`, `column-row-slice` |
| Volume Settings | `Slice Brightness`, `Slice Contrast` | number | -1 to 1 |
| Volume Settings | `Sagittal (L - R)`, `Coronal (P - A)`, `Axial (I - S)` | number | -128 to 128 (crosshair, tkrRAS mm) |
| Volume Settings | `Crosshair tkrRAS`, `Crosshair ScanRAS`, `Affine MNI152` | text | `"x, y, z"` (crosshair in that space) |
| Volume Settings | `Overlay Coronal`, `Overlay Axial`, `Overlay Sagittal` | boolean | the slice in the 3D view |
| Volume Settings | `Voxel Type` | option | `none` and the loaded volumes, e.g. `aparc_aseg` |
| Volume Settings | `Voxel Display` | option | `hidden`, `normal`, `side camera`, `main camera`, `anat. slices` (hidden while `Voxel Type` is `none`) |
| Volume Settings | `Voxel Opacity` | number | 0 to 1 |
| Volume Settings | `Voxel Label` | text | labels to show, e.g. `"4,5,6-7"` |
| Surface Settings | `Surface Type` | option | loaded surfaces, e.g. `pial`, `smoothwm` |
| Surface Settings | `Surface Material` | option | `MeshPhysicalMaterial`, `MeshLambertMaterial` |
| Surface Settings | `Clipping Plane` | option | `disabled`, `axial`, `coronal`, `sagittal` |
| Surface Settings | `Left Hemisphere`, `Right Hemisphere` | option | `normal`, `mesh clipping x 0.3`, `mesh clipping x 0.1`, `wireframe`, `hidden` |
| Surface Settings | `Left Opacity`, `Right Opacity` | number | 0.1 to 1 |
| Surface Settings | `Surface Color` | option | `vertices`, `sync from voxels`, `sync from electrodes`, `none` |
| Surface Settings | `Blend Factor` | number | 0 to 1 |
| Surface Settings | `Sigma`, `Decay`, `Range Limit` | number | 0 to 10, 0.05 to 1, 1 to 30 (painting electrodes onto the surface) |
| Surface Settings | `Surface Color Data`, `Surface Threshold Data` | option | `[none]`, the loaded annotations, `[custom measurement]`, `[custom annotation]` |
| Tractography Settings | `Streamline Display` | option | `all`, `main camera`, `side panels` |
| Tractography Settings | `Streamline Width`, `Streamline Opacity` | number | 0 to 1.5, 0 to 1 |
| Tractography Settings | `Show all (<group>)`, `Show: <group>/<bundle>` | boolean | one per loaded group and bundle |
| Electrode Settings | `Visibility` | option | `all visible`, `threshold only`, `hide inactives`, `hidden` |
| Electrode Settings | `Electrode Shape` | option | `prototype+sphere`, `prototype`, `contact-only` |
| Electrode Settings | `Outlines` | option | `auto`, `on`, `active only`, `off` |
| Electrode Settings | `Electrode Text` | option | `None`, `channel_numbers`, `label_prefix`, `device_name` |
| Electrode Settings | `Text Scale` | number | 1 to 6 |
| Electrode Settings | `Map Electrodes` | boolean | map to the template brain (with `Surface Mapping`, `Volume Mapping`) |
| Data Visualization | `Display Data` | option | `[None]`, `[Subject]`, and the table's variables |
| Data Visualization | `Display Range` | text | `"low,high"`; `""` for the data's range |
| Data Visualization | `Threshold Data`, `Additional Data` | option | as `Display Data` |
| Data Visualization | `Threshold Range` | text | `"low,high"` (numbers) or `"A\|B"` (categories) |
| Data Visualization | `Threshold Method` | option | `v = T1`, `\|v\| < T1`, `\|v\| >= T1`, `v < T1`, `v >= T1`, `v in [T1, T2]`, `v not in [T1,T2]` |
| Data Visualization | `Inactive Color` | color | electrodes without values |
| Data Visualization | `Play/Pause`, `Speed`, `Time` | boolean, option, number | animation; `Speed` 0.01 to 5 (shown as `x 1`), `Time` within the data's times |
| Data Visualization | `Show Legend`, `Show Time`, `Highlight Box`, `Info Text` | boolean | overlays |

Buttons in the panel include "Reset Canvas" (camera), "Reset Slice Canvas",
"Screenshot" (downloads a PDF), and "Download GLTF". Agents list every
controller, with its choices and range, with
`rave_3dviewer_get(name = "controller_options")`.

**Mouse and keyboard.** Left-drag rotates, right-drag pans, scrolling zooms.
Click an electrode to highlight it; double-click it to show its values in
"Viewer status". Hold `f` and click to focus any object (surface, slice,
volume, streamline). Keys (the mouse must be over the viewer):

| Key | Action | Key | Action |
|---|---|---|---|
| `p` | toggle the slice panels | `z` / `⇧Z` | zoom out / in |
| `l` | cycle view layouts | `m` | cycle slice modes |
| `e` / `E`, `q` / `Q`, `w` / `W` | move the coronal, axial, sagittal slice | `⇧C` / `⇧A` / `⇧S` | show the coronal / axial / sagittal slice in 3D |
| `[` / `]` | left / right hemisphere style | `⇧[` / `⇧]` | left / right hemisphere opacity |
| `⇧<` / `⇧>` | left / right mesh clipping | `c` | cycle the clipping plane |
| `⇧P` | cycle surface types | `⇧M` | cycle surface materials |
| `k` | cycle surface color | `a` | cycle volumes (`Voxel Type`) |
| `⇧L` | cycle volume display | `.` / `,` | next / previous electrode |
| `v` | cycle electrode visibility | `⇧V` | toggle electrode labels |
| `o` | cycle outlines | `⇧O` | cycle electrode shapes |
| `s` | play / pause | `d` / `D` | next / previous data variable |
| `t` | threshold by the displayed variable | `f` / `⇧F` | focus mode / cycle focus types |

**Quick analysis** (left panel, "Quick analysis" card):

* **Object type** — "Electrode", "Mesh surface", "3D volume", or "Streamlines",
  then the object: an electrode ("[Double-click electrode]" uses the one last
  double-clicked), a surface and hemisphere (e.g. `pial [lh]`), a volume
  ("[Current active overlay]" or a loaded volume, e.g. `aparc_aseg`), or a
  streamline group or bundle ("[Current active streamlines]", `alic/*`, or one
  bundle). A preview line shows the object.
* **Add object** — adds it to "Choose & sort objects" (drag to sort; remove
  with its button).
* **Analysis type** — "Streamline collision detection".
* **Configure & Run...** — opens a dialog: "Mode for ROI (volume/surface/electrode)
  objects" (`auto`, `volume`, `pointcloud`, `surface`) and "Radius (mm)"; the
  dialog starts with the last saved values. Its "Run" button runs the analysis,
  saves the objects and parameters, and closes the dialog when done. It needs at
  least one region (electrode, surface, or volume) and one streamline bundle.

### 3. Outputs

* **RAVE 3D Viewer** — the brain, slices, electrodes, and tracts, with the
  control panel, the color legend, and the time.
* **Viewer status** — "Surface Type" and "Camera Zoom" (as of the last mouse
  drag). After double-clicking an electrode: the electrode (with a link to its
  MNI152 location), the displayed variable, and its value (or the number of
  values).
* **Time series** (the back of "Viewer status"; click "Click here to toggle
  visualization for time-series data") — the double-clicked electrode's values
  over time. Clicking the plot moves the viewer to that time.
* **Analysis results** — the quick analysis table: each streamline bundle, the
  share of its lines that overlap the regions ("Overlap (%)"), and its number
  of lines.

## Common procedures

Recipes for common goals. Each starts after the data are loaded (step 1).

### Procedure — Color electrodes by a table of values

Step 1: In "Select an uploaded data", choose a table, or choose "[New Uploads]"
and upload one (see the value table in step 2 of the guide).
Step 2: Click "Re-generate the viewer" (or "Re-generate & Visualize").
Step 3: In the control panel, choose the variable in `Display Data`; for
numbers, set the color range in `Display Range`, e.g. `-2,2`.

### Procedure — Animate values over time

Steps 1-2: reuse [Procedure — Color electrodes by a table of values](#procedure--color-electrodes-by-a-table-of-values)
with a table that has `Time`.
Step 3: Play with `Play/Pause` (key `s`), set `Speed`, or move `Time`.
Double-click an electrode, then flip "Viewer status" to see its time series.

### Procedure — Show only some electrodes

Step 1: Add a column to the value table, e.g. `Selected` with `yes`/`no`, and
regenerate the viewer.
Step 2: Set `Threshold Data` to `Selected`, `Threshold Range` to `yes`, and
`Visibility` to `threshold only`.

### Procedure — Paint electrode values onto the cortex

Step 1: Color the electrodes (first procedure).
Step 2: Set `Surface Color` to `sync from electrodes`; adjust `Blend Factor`,
`Sigma`, `Decay`, and `Range Limit` (mm).
Step 3: Optionally hide the contacts: `Visibility` `hidden`.

### Procedure — Show an atlas

Step 1: Load the subject with the atlas in "Additional volumes", e.g.
`aparc+aseg`.
Step 2: Set `Voxel Type` to the atlas (e.g. `aparc_aseg`), then `Voxel Display`
(e.g. `normal`, or `side camera` for the slice panels only) and
`Voxel Opacity`.
`Voxel Label` limits the labels shown, e.g. `17,53` for the hippocampi.

### Procedure — Show fiber tracts

Step 1: Load the subject with the bundles in "Additional streamlines", e.g.
`alic/*`.
Step 2: Turn groups or bundles on and off with `Show all (<group>)` and
`Show: <group>/<bundle>`; adjust `Streamline Width` and `Streamline Opacity`.

### Procedure — Which tracts pass near an electrode

Steps 1-2: reuse [Procedure — Show fiber tracts](#procedure--show-fiber-tracts).
Step 3: Object type "Electrode", choose the electrode, "Add object".
Step 4: Object type "Streamlines", choose a group (e.g. `alic/*`), "Add object".
Step 5: "Configure & Run...", set the radius, click "Run". Read "Analysis results".

### Procedure — Add files by drag and drop

Drop files onto the "Drag files here" box in the control panel (Custom Geometry
Settings): volumes (`nii`, `mgz`), surfaces (FreeSurfer, `gii`, `stl`),
streamlines (`trk`, `tck`, `tt`), electrode coordinates (`csv`), and color
maps (`csv`, `tsv`). Each file gets its own visibility and opacity controllers
in that folder. Dropped files stay in the browser; the module does not save
them.

### Procedure — Reset the view

Click "Reset controller option": every controller and the camera return to
their defaults, and the viewer regenerates. "Reset Canvas" in the control panel
resets only the camera.

### Procedure — Save a picture or the viewer

"Screenshot" in the control panel downloads a PDF of the view; `Record` records
a video. To save the viewer as a web page, use
`threeBrain::save_brain()` (see [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)).

## Caveats

* **What regenerating keeps.** "Re-generate" saves and restores these
  controllers: slices and panels (`Show Panels`, `Slice Mode`, `Slice
  Brightness`, the slice positions and overlays, `Display Coordinates`),
  surfaces (`Surface Type`, `Surface Material`, `Clipping Plane`, hemisphere
  styles, opacities, and mesh clipping, `Surface Color`, `Blend Factor`,
  `Sigma`, `Decay`, `Range Limit`), electrodes (`Visibility`, `Electrode
  Shape`, `Outlines`, `Text Scale`, `Surface Mapping`, `Volume Mapping`), data
  (`Display Data`, `Display Range`, `Threshold Data`, `Threshold Range`,
  `Threshold Method`, `Additional Data`, `Show Legend`, `Show Time`, `Highlight
  Box`, `Info Text`, `Time`), `Voxel Min`, `Voxel Max`, `Voxel Label`, and the
  background. Everything else (e.g. `Voxel Type`, `Camera Position`) returns to
  its default.
* **The camera** is saved as of the last mouse drag or zoom; regenerating
  restores that camera, not a preset view chosen afterwards.
* **Loading** a subject, and "Reset controller option", clear the saved
  controllers and camera.
* **The "here" link** in the upload notification regenerates the viewer
  without saving the current controllers and camera; "Re-generate the viewer"
  saves them first.
* **Values do not show** when the table's `Electrode` numbers are not in the
  electrode table, when its `Subject` differs from the loaded subject, or when
  `Time` is not numeric.
* **Quick analysis with a volume** as a region fails ("as_ieegio_streamlines"
  error; the dialog stays open with an error notification). Electrode regions
  work.
* **Viewer status**: the MNI152 link of "Anat. Clip Plane" stays empty.
* **MRI slices** use the first image found in `fs/mri`: `rave_slices`,
  `brain.finalsurfs`, `synthSR.norm`, `synthSR`, `brain`, `brainmask`,
  `brainmask.auto`, `T1` (`.nii`, `.nii.gz`, or `.mgz`). To show your own
  image, save it as `fs/mri/rave_slices.nii.gz`.
* **Depth electrodes on a template brain** may sit outside the cortical
  surface: they are in deep structures.

## Run the pipeline without the UI

The viewer is a pipeline: set the loader and data choices, then build the
widget. `set_settings()` saves to the pipeline's `settings.yaml`, which the
module reads when it opens.

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("custom_3d_viewer")

# Set inputs (one per line; comment each so the mapping is clear)
pipeline$set_settings(
  project_name     = "demo",                    # RAVE project
  subject_code     = "DemoSubject",             # subject
  overlay_types    = "aparc+aseg",              # "Additional volumes"
  surface_types    = "pial-outer-smoothed",     # "Additional surface types"
  annot_types      = character(0),              # surface annotations
  streamline_types = character(0),              # fiber tracts, e.g. "alic/*"
  use_spheres      = FALSE,                     # "Use spheres contacts"
  override_radius  = NA,                        # sphere radius (mm); NA: from the table
  use_template     = FALSE,                     # "Use template brain"
  data_source      = "Uploads",                 # color electrodes by an upload
  uploaded_source  = "electrode_value-14.fst",  # its name in rave-imaging/custom-data
  controllers      = list("Display Data" = "Amplitude",
                          "Background Color" = "#000000"),
  main_camera      = list()                     # default camera
)

# Build a target and read its result
info   <- pipeline$run("loaded_brain_info")     # brain, electrode table, types
widget <- pipeline$run("brain_widget")          # the 3D viewer (htmlwidget)

# Optional: save the viewer as a web page
threeBrain::save_brain(widget, path = "viewer.html")
```

Other targets: `initial_brain_widget` (the viewer without values),
`path_datatable` (the value table's file), and `brain_with_data` (the brain and
its `variables`). The quick analysis is target
`analysis_results_streamline_collision_detection`; its objects and parameters
are settings `analysis_objects` and `analysis_inputs_streamline_collision_detection`.

## Drive the module with MCP tools

Operate the live module as a user would. The order is fixed: `load_data`
first; then the data table; then `run_analysis`; then the viewer tools. Every
script except `load_data` needs the data loaded.

A script's reply has `result` (its return value) and `output` (what it printed
while it ran). `load_data` and `run_analysis` print the pipeline's progress;
a failed step shows as `✖ <target> errored` in `output`.

### Load data

* Project and subject: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "demo")`,
  then `inputId = "loader_subject_code"` (wait until the subject choices load)
* Electrode source: `inputId = "loader_electrode_source"`,
  `value = "Subject meta directory - electrodes.csv"`. Other choices need the
  user to upload a table.
* Extras, as JSON arrays (the choices refresh when the subject changes):
  `loader_volume_types` (e.g. `["aparc+aseg"]`), `loader_surface_types`,
  `loader_annot_types`, `loader_streamline_types` (e.g. `["alic/*"]`); `[]` for
  none
* `loader_use_spheres`, `loader_use_template` (`true`/`false`),
  `loader_override_radius` (mm)
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`.
  The viewer is ready when `rave_3dviewer_get(outputId = "viewer", name = "controllers")`
  answers without an error. After loading another subject, the old viewer's
  values stay until the new viewer reports: wait until they change (e.g.
  `Subject`, or the `Show: ...` controllers of the new streamlines).

### Configure and run

* `data_source`: `"Uploads"` or `"None"`; `uploaded_source`: an existing
  upload, e.g. `"values.fst"`. To use a new table, ask the user to upload it
  ("[New Uploads]"); agents cannot upload files.
* Regenerate: `tool("tool__module_interactive_script_run", name = "run_analysis")`
  (the "Re-generate & Visualize" button). It saves the data choice and the
  viewer's controllers first. When the table loaded, `Display Data` offers its
  variables (`rave_3dviewer_get(name = "controller_options", args = '{"names": ["Display Data"]}')`).
* Reset: `name = "reset_viewer"` (the "Reset controller option" link). Ask the
  user first: it drops their viewer settings.
* Quick analysis (ask the user before running it):
  1. `object_selector` (`"Electrode"`, `"Mesh surface"`, `"3D volume"`,
     `"Streamlines"`), then its object input: `object_selector_electrode`
     (e.g. `"14"`), `object_selector_surface` (e.g. `"pial [lh]"`),
     `object_selector_volume` (e.g. `"aparc_aseg"`), or
     `object_selector_streamlines` (e.g. `"alic/*"`).
  2. Wait until the preview `tool("tool__shiny_output_result", outputId = "object_selector_text")`
     shows that object, then `name = "add_object"`; it returns the object's
     label. Repeat for each object. `object_selector_list` holds the added
     keys: send the keys without one to remove it, or `[]` to clear.
  3. `analysis_selector` (`"streamline_collision_detection"`), then
     `name = "open_analysis"`. Check the dialog:
     `tool("tool__shiny_query_ui", css_selector = "#custom_3d_viewer-analysis_param_run")`.
     Its mode and radius show the last saved values; agents cannot change them.
  4. Click its "Run": `tool("tool__shiny_ui_operate", action = "click", target = "analysis_param_run")`.
     The dialog closes when the analysis is done. If it stays open, the analysis
     failed: people see an error notification (see Caveats).

### Control the 3D viewer

Both tools take `outputId = "viewer"`. `args` and `data` are JSON objects.

* Read: `tool("tool__rave_3dviewer_get", outputId = "viewer", name = "controllers")`;
  `args = '{"names": ["Surface Type"]}'` picks some.
* What a controller accepts: `name = "controller_options"` (type, choices or
  range; hidden controllers apply once a related one is set).
* Change controllers, several at once:
  `tool("tool__rave_3dviewer_set", outputId = "viewer", name = "controllers", data = '{"Voxel Type": "aparc_aseg", "Voxel Display": "normal", "Left Opacity": 0.4}')`.
  Values are checked first (an error changes nothing); read them back after
  about 0.5 s. Standard views: `'{"Camera Position": "left"}'`.
* Camera: `name = "camera"`, `data = '{"position": [0, 500, 0], "up": [0, 0, 1], "zoom": 1.5}'`.
  The viewer does not report this back: check the view with a picture,
  `tool("tool__shiny_query_ui", css_selector = "#custom_3d_viewer-viewer")`.
* Crosshair: `name = "crosshair"`, `data = '{"position": [-40, 10, 20], "space": "MNI152"}'`
  (`space`: `scanner` by default, `tkrRAS`, `MNI305`, `MNI152`, `CRS`). Read it
  with `rave_3dviewer_get(name = "crosshair", args = '{"space": "all"}')`.
* The object the user clicked, double-clicked, or focused (e.g. an electrode's
  number and coordinates): `rave_3dviewer_get(name = "selected")`.
* Changes are not saved by themselves: `run_analysis` keeps the controllers
  listed in Caveats; `reset_viewer` and `load_data` drop them.

### Inspect results

* Registered outputs: `tool("tool__shiny_output_result", outputId = "analysis_results")`
  (the quick analysis table) and `outputId = "object_selector_text"` (the
  object preview).
* The viewer status panel: `tool("tool__shiny_query_ui", css_selector = "#custom_3d_viewer-viewer_status")`.
* The viewer's state: `rave_3dviewer_get` (above).
