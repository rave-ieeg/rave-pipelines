# Plan: MCP support for custom_3d_viewer + `rave_3dviewer_get` / `rave_3dviewer_set`

## Context

`custom_3d_viewer` ("Subject 3D Viewer") cannot be operated by agents yet:
- it has only the `load_data` / `run_analysis` scripts;
- none of its inputs are registered;
- its manual is marked "This file needs rewritten".

The user wants two things:
1. The normal conversion from `agents/skills/build-module-mcp/SKILL.md`.
2. Two new MCP tools, `rave_3dviewer_get(outputId, name, args = list())` and `rave_3dviewer_set(outputId, name, data)`. They read and change a live 3D viewer through `threeBrain::brain_proxy(outputId, session)`, and validate `args` / `data` for each `name`. Other modules with 3D viewers will use these tools later, so nothing in them is specific to custom_3d_viewer.

**Step 0, right after approval:** save this plan as `modules/custom_3d_viewer/plan-mcp_support.md`, as the user asked. Plan mode allows edits only to the plan file.

The installed threeBrain proxy (1.3.0.61, source `../threeBrain`) has gaps that make such tools unreliable:
- `set_display_data()` and `set_focused_electrode()` do nothing in the JS.
- After many changes, the viewer's controller values are not sent back to R. This covers crosshair moves made by clicking a slice, and every controller whose handler never broadcasts.
- Controller choices and ranges never reach R.
- The bindings `background`, `surface_type`, `display_variable` and `side_display` read inputs the JS never sends.
- `plane_position` is documented as reactive but is isolated.

**Decisions made with the user (2026-09-29)**
- **Tool shape.** The tools use property names: `name` picks a group, and `args` (get) or `data` (set) is a JSON object validated for that group.
  - `outputId` is a required argument of both tools, so they work for any module's viewer.
  - The crosshair's default space is `scanner`, for both get and set.
  - The JS sends controller values and specs back with a 500 ms debounce.
- **threeBrain.** Fix it as well (`../threeBrain` R proxy and JS driver, then rebuild the JS and reinstall), with two limits:
  - **Keep the camera report as it is.** The viewer reports `main_camera` only after a mouse drag. Agent camera changes are not reported back, and Re-generate keeps restoring the last dragged position.
  - **`set_values()` is deprecated.** It is not exposed in the tools and not changed in threeBrain.
- **Quick analysis.** The conversion includes the Quick analysis card.
- **Approval.** `rave_3dviewer_set` needs no approval per call (category `executing`). `rave_3dviewer_get` is `exploratory`.

**Visible UI changes: none.** The rave-pipelines changes are only `register_input` wrappers, scripts whose bodies are moved verbatim, `agents.yaml`, the manual, and a test.

**Behaviour changes people will see.** These come from the threeBrain fixes; the user accepted them, and they will be listed again in the final report:
1. In custom_3d_viewer, "Viewer status" shows the real Surface Type instead of always "pial".
2. In reference_module, `set_display_data("Value")` at `module_server.R:2330` starts working. It re-selects the value already shown, so no visible change is expected.
3. The viewer sends controller changes to R more often (debounced), so the custom_3d_viewer Quick analysis preview refreshes more often.

**No commits** in either repo unless the user asks. The existing uncommitted edits are kept:
- rave-pipelines: the custom_3d_viewer `loader.R`, `module_server.R` and `agents.yaml`, `tests/testthat/test-agent-tools.R`, and the manual.
- threeBrain: `ViewerWrapper.js`, `CHANGELOG.md`, and the `dist` files.

## Part A — threeBrain fixes

The JS source lives in the nested repo `inst/three-brain-js`, on branch `migrate-webgpu`, under `src/js`. The R code is in the outer repo on `master`.

**JS**
1. **`drivers/RShinyDriver.js` `driveDisplayData`.** Use `this.app.controllerGUI`; `this.controllerGUI` is undefined and causes a TypeError. Read the GUI through `this.app` every time, because `ViewerApp.updateControllers` rebuilds it on each `updateData`.
2. **`RShinyDriver.js` case `focused_electrode`.** Accept the keys R sends (`subject_code`, `electrode`) as well as `subjectCode` / `electrodeNumber`, then call `driveChooseElectrode`.
3. **Stale `controllers` input.**
   - `core/EnhancedGUIController.js` `_runChange` is the single path every handler call takes (user edits, `setValue`, `fire`). It will emit a GUI-level event through the shared folder dispatcher (`EnhancedGUI.js` `__eventDispather`).
   - `RShinyDriver.rebindControlCenter`, which already rebinds after every `updateData`, listens for that event and sends `controllers` with a 500 ms trailing debounce, reusing `_onControllersUpdated`.
   - `core/ViewerControlCenter.js` `_onSetSliceCrosshair` (slice clicks write values directly) triggers the same debounced send.
   - Playback `Time` writes skip handlers, so they stay unsent (no flood).
   - Existing `broadcast()` calls are unchanged.
4. **Controller specs to R.** Add a new input `<outputId>_controller_specs`. Each entry holds `name`, `folder` (`_folder._fullPaths.join(">")`), `type` (`_type`), `choices` (`_names`), `values` (`_values`), `min`, `max`, `step`, `hidden` and `disabled`.
   - It is sent from `rebindControlCenter` on `updateData.end`.
   - It is also re-sent (500 ms debounce) when the GUI structure changes: from `EnhancedGUI.addController`, `EnhancedGUIController.destroy`, `_rebuild` (min/max/step/options), `show`/`hide` and `enable`/`disable`.
5. **Not changed:**
   - camera announcements (user decision);
   - `set_values`/`add_clip`;
   - `interval`/`linegraph` types in `_onDriveController` (the tool reports those controllers as not settable);
   - the `threejs_brain.js:37` `show_modal = TRUE` edge case (reported only).

**R (`R/class_proxy.R`)**
6. **Controller specs.** Add an active binding `controller_specs` and a method `get_controller_specs()` (isolated), following `controllers` / `get_controllers()`. Both return a list keyed by controller name.
7. **Bindings that read unsent inputs.** They now read the `controllers` input, with the same defaults as today:

   | Binding | Controller |
   |---|---|
   | `background` | `Background Color` |
   | `surface_type` | `Surface Type` |
   | `display_variable` | `Display Data` |
   | `side_display` | `Show Panels` |

   `sync` is left as is.
8. **`plane_position`.** Make it reactive as documented: read the `controllers` input instead of the isolated `get_controllers()`.
9. **Housekeeping.** Update the roxygen docs, then run `devtools::document()` to regenerate `man/ViewerProxy.Rd`. Bump `DESCRIPTION` from 1.3.0.61 to 1.3.0.62. Add a `Shiny Proxy:` category to `NEWS.md` under "threeBrain 1.4.0", using the existing bullet style.

**Build.** `cd inst/three-brain-js && npm run build` (webpack; `node_modules` exists), then `cp -r inst/three-brain-js/dist inst/threeBrainJS`. These are the JS steps of the root `build.sh`. The root `build.sh` itself is not run, because it also rewrites `CHANGELOG.md` and builds a tarball.
- While developing: `R CMD INSTALL -l $SCRATCH/rlib ../threeBrain`. The prototype and the dashboard test put `$SCRATCH/rlib` first on the library path, so the user library is untouched until the fixes are verified.
- At the end: install into the user library (approved). The user's app on port 17283 needs a restart to use it.

threeBrain has no `tests/`, and none will be added. R-level checks use a scratch script with `shiny::MockShinySession` and `session$setInputs(viewer_controllers = ..., viewer_controller_specs = ...)`.

## Part B — Prototype in a scratchpad Shiny app (the user's DemoSubject snippet)

This is a throwaway harness in `$SCRATCH/proto/`, on port **17298**; the user's app on 17283 and the dashboard test on 17299 are untouched. It runs with `.libPaths(c("$SCRATCH/rlib", .libPaths()))`.

**`app.R`**
- **Brain:**
  - `brain <- ravecore::rave_brain("demo/DemoSubject")`
  - `brain$set_electrode_values(data.frame(Electrode = 14, Time = seq(-1,1,by=0.01), Value = sin(seq(-1,1,by=0.01)*30)*10 + rnorm(201)))`
  - `brain$add_atlas("aparc+aseg")`
- **Viewer:** UI `threeBrain::threejsBrainOutput("viewer")`, server `renderBrain(brain$plot(show_modal = FALSE))`.
- **Tools:** it sources the draft `agents/tools/rave_3dviewer.R` and builds each tool with `rave_3dviewer_get(session = session)[[1]]`, as `tests/testthat/test-agent-tools.R` does. Calls pass `outputId = "viewer"`.
- **Command loop:** it polls `cmd.R` every 300 ms, evaluates it in the session, and writes the printed result (or `ERROR: ...`) to `out.txt`.

**Browser:** a scratchpad copy of `agents/skills/build-module-mcp/live-browser.js`.
- It keeps Playwright attached so WebGL renders; a `shot` file triggers a screenshot.
- It adds a `mouse.json` command (drag or click at x,y, or a key press) to act like a person rotating the view or clicking a slice.

**Order**
1. Reproduce each threeBrain gap on the current build (baseline).
2. Apply Part A and rebuild.
3. Re-run every scenario:
   - each get and set `name`;
   - each validation error;
   - read-back after programmatic changes and after mouse changes;
   - the camera staying unreported after `set` (expected);
   - a screenshot after each set.

## Part C — The tools: `agents/tools/rave_3dviewer.R` (new)

Two `shidashi::mcp_wrapper(function(session) ellmer::tool(...))` objects live in one file, following `agents/tools/shiny_ui_operate.R`:
- the module session is bound through the closure;
- errors use `stop()`, which the MCP layer turns into `isError`;
- replies are strings or plain lists;
- reads are wrapped in `shiny::isolate()`;
- the generator body never touches live state, because the catalog harvest runs it on a `MockShinySession`.

Helpers get a `rave_3dviewer_` prefix, since tool files are sourced into the module env before the module's own files.

**Reusable by other modules.** The tools hold nothing specific to custom_3d_viewer.
- A module adopts them by listing them in its `agents.yaml` and naming its viewer's `outputId` in its manual and system prompt. For custom_3d_viewer the `outputId` is `viewer`; other examples are power_explorer's `brain_viewer` and electrode_localization's `localization_viewer`.
- The names are a registry: each `name` has a handler plus its argument checks. A module that needs another name later (e.g. `localization_table`) adds one entry. The file header and the build-module-mcp skill explain how.

**`outputId`** (required, both tools): the viewer's output ID without the module prefix. A prefixed ID (`custom_3d_viewer-viewer`) is stripped, as in `shiny_ui_operate`.
- It must be a rendered threeBrain viewer, i.e. its `<outputId>_controllers` input exists in the module session.
- Otherwise the error lists the rendered viewers found (every `*_controllers` input), or says that none has rendered yet ("run `load_data` and wait for the viewer").
- The proxy is `threeBrain::brain_proxy(outputId, session = session)`.

**`args` and `data` encoding.** Both are JSON strings, as in shidashi's `shiny_input_update`.
- They are decoded with `jsonlite::fromJSON(simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)`.
- An already-decoded list is also accepted.
- Each must be a named object. `args` is optional and defaults to `{}`.
- A key that the `name` does not take is an error naming the keys it does take.

**`rave_3dviewer_get(outputId, name, args = list())`**

| `name` | `args` | Returns |
|---|---|---|
| `controllers` | `names` (optional): an array of labels to return; an unknown label is an error with close matches | `{outputId, controllers: {<label>: <value>}}`, flat |
| `controller_options` | `names` (optional), as above | For each non-hidden controller: `type`, and `choices` or `min`/`max`/`step`. Buttons are left out; `interval`/`linegraph` are marked "not settable". Taken from `get_controller_specs()`. |
| `camera` | none | `position`, `up`, `zoom`, with a note: "reported when the user last dragged or zoomed with the mouse; changes by `rave_3dviewer_set`, 'Camera Position' presets or the keyboard are not reported" |
| `crosshair` | `space`: `scanner` (default), `tkrRAS`, `MNI305`, `MNI152`, `CRS`, or `all` (every space) | The crosshair position in that space, via `get_crosshair_position(space)`. Errors if the viewer has no slice panels, or if one space other than `tkrRAS` is asked for before the viewer has reported its subject. With `all`, it returns the spaces available and says which are missing. |
| `selected` | none | The last click, double-click and focus (F + click) events. For each: object name, type, electrode number, subject, positions (tkrRAS, scanner, MNI305/MNI152 where the event has them), and the data clip and time shown. |

**`rave_3dviewer_set(outputId, name, data)`**

`name` is one of `controllers`, `camera` or `crosshair`.

- **`controllers`.** `data` is `{"<label>": value, ...}`, and each value is checked against the live specs.
  - Unknown label: error listing close matches (`agrep`). Hidden or disabled controller: error.
  - Checks by type:

    | Type | Accepted value |
    |---|---|
    | `boolean` | `true` / `false` only |
    | `number` | finite and within `[min, max]`; out of range is an error, not clamped |
    | `option` | one of `choices` / `values` (e.g. `Speed` takes numbers) |
    | `color` | `#RRGGBB`, `#RGB`, or an R color name converted to hex |
    | `string` | a scalar; a numeric pair becomes `"lo,hi"` for the `* Range` controllers, and a numeric triple becomes `"x, y, z"` for `Crosshair tkrRAS`, `Crosshair ScanRAS` and `Affine MNI152` |
    | buttons, `interval`, `linegraph` | not settable |

  - `Record` is refused, because it starts a browser download; ask the user instead.
  - Keys are sent in dependency order in one `set_controllers()` call. These go first: Display Data, Threshold Data, Voxel Type, Surface Color Data, Surface Threshold Data, Left/Right Hemisphere, View Layout, Surface Type.
  - If threeBrain has no specs (an older install), labels must appear in `get_controllers()`, the type is inferred from the current value, and the reply says that choices and ranges were not checked.
- **`camera`.** `data` is `{"position": [x,y,z], "up": [x,y,z], "zoom": z}` with at least one key.
  - `position` and `up` are finite, length 3 and non-zero.
  - `up` is allowed only together with `position`, and must not be parallel to it; the error suggests `[0,1,0]` for views along z.
  - `zoom` is finite and within `[0.5, 40]`, the UI's limits.
  - Calls `set_camera()` / `set_zoom_level()`.
  - The reply says the viewer does not report this back: confirm with a picture instead, and note that Re-generate restores the last dragged position. The tool description points to `{"Camera Position": "left"}` for standard views.
- **`crosshair`.** `data` is `{"position": [x,y,z], "space": "scanner"|"tkrRAS"|"MNI305"|"MNI152"|"CRS"}`, with `space` defaulting to `scanner`.
  - `position` is finite and length 3.
  - The slice controllers must exist.
  - A space other than tkrRAS needs `current_subject`. This is an error, unlike the proxy's silent fallback.
  - Calls `set_crosshair_position(position, space)`.
- **Every reply** gives the normalized values sent, plus "the viewer applies them after this call returns; read them back with `rave_3dviewer_get` (controller values arrive within about 500 ms), or look with `shiny_query_ui(css_selector = '#<module>-<outputId> canvas')`".

**Schema mirror.** Entries for both tools go in `agents/tool-schema.yaml`, under the comment "Mirrors agents/tools/rave_3dviewer.R: keep them in sync".

**Unit tests** go in `tests/testthat/test-agent-tools.R`, appended after the user's uncommitted edits. The new file is sourced like the others, and `fake_session()` is extended to accept named input values. They cover:
- `outputId`: a valid ID, a module-prefixed ID, and an unknown or unrendered ID (the error lists the rendered viewers, or says none has rendered);
- each get `name` and its `args` (`names` filter; crosshair `space` default `scanner`, one space, and `all`), from fake `viewer_controllers` / `viewer_controller_specs` / `viewer_main_camera` / `viewer_current_subject` / mouse inputs;
- each set `name` sending the right `threeBrain-RtoJS-custom_3d_viewer-viewer` payload, with keys in dependency order and the crosshair's default space `scanner`;
- each validation error: unknown `args` key, unknown label with suggestions, out of range, bad option, bad color, non-object `args` / `data`, parallel `up`, zoom out of range, crosshair without slices or without a subject;
- the fallback when specs are missing.

## Part D — custom_3d_viewer conversion

**Inventory**

| Button / action | Reads | Writes | Agent path |
|---|---|---|---|
| Load subject (loader) | `loader_project_name`, `loader_subject_code`, `loader_electrode_source` (+ file upload), `loader_volume_types`, `loader_surface_types`, `loader_annot_types`, `loader_streamline_types`, `loader_use_spheres`, `loader_override_radius`, `loader_use_template` | settings.yaml, `data/suggested_electrode_table`, targets `loaded_brain_info`, `initial_brain_widget` | script `load_data` (exists) |
| Re-generate the viewer / Re-generate & Visualize | `data_source`, `uploaded_source`, viewer controllers + camera | settings.yaml, targets `path_datatable`, `brain_widget` | script `run_analysis` (exists) |
| Reset controller option (`viewer_reset`) | `data_source`, `uploaded_source` | settings.yaml (controllers and camera cleared); regenerates | **new script `reset_viewer`** (observer body moved verbatim; the link runs it) |
| Upload table (`uploaded_file`) | a file on the user's computer | subject `rave-imaging/custom-data/<name>.fst` | people only (file picker); agents pick an existing upload in `uploaded_source` |
| "here" link in the upload toast | — | `regenerate_viewer()` | unchanged; agents use `run_analysis` |
| Show/Download a template table | — | dialog with a download | unchanged; the manual gives the table format |
| Add object (`object_selector_add`) | `object_selector`, `object_selector_electrode/_surface/_volume/_streamlines`, viewer double-click, controllers | `object_selector_list` | **new script `add_object`** (body moved verbatim) |
| Configure & Run... (`analysis_configure`) | `analysis_selector` | opens the analysis dialog | **new script `open_analysis`** (body moved verbatim) |
| Dialog "Run" (`analysis_param_run`) | `analysis_selector`, dialog inputs, `object_selector_list` | settings (`analysis_objects`, `analysis_inputs_*`), analysis target, `analysis_results` | agents click it with `shiny_ui_operate` after `open_analysis`; dialog inputs are not agent-settable, so runs use the values the dialog shows |
| Flip viewer status; time-series plot click | — | display only; `Time` controller | unchanged; agents set `Time` with `rave_3dviewer_set` |

**Register inputs.** Each gets a `shidashi::register_input` wrapper; the tags are unchanged.
- **`loader.R`:**
  - `loader_electrode_source` (select);
  - multi-selects `loader_volume_types`, `loader_surface_types`, `loader_annot_types` and `loader_streamline_types`, with `shiny::updateSelectInput(value=selected)` and JSON arrays;
  - `loader_use_spheres` and `loader_use_template` (`shiny::updateCheckboxInput`);
  - `loader_override_radius` (`shiny::updateNumericInput`).

  The presets already register the project and subject inputs.
- **`module_html.R`:**
  - `data_source` and `uploaded_source` (select);
  - `object_selector` (`shiny::updateRadioButtons(value=selected)`);
  - `object_selector_electrode`, `_surface`, `_volume` and `_streamlines` (`shiny::updateSelectizeInput(value=selected)`);
  - `object_selector_list` (`shiny::updateSelectizeInput(value=selected)`): send the current keys minus the one to remove, or `[]` to clear;
  - `analysis_selector` (select).

  The hidden `data_source_*` inputs, whose choice is commented out, are skipped.
- **Risk to check live.** The four object selectors are filled with `server = TRUE`, so `shiny_input_update` may fail to select an option the browser hasn't loaded. If it does, stop and ask; no module change happens without approval.

**Scripts** (`module_server.R`): `reset_viewer`, `add_object` and `open_analysis`.
- Each has a description giving the button it mirrors, the inputs it reads, what it writes, and the next step. For example, after `open_analysis`: check `#custom_3d_viewer-analysis_param_run` with `shiny_query_ui`, ask the user, then click it with `shiny_ui_operate`.
- The observers keep their `error_wrapper` and now call `server_tools$trigger_script()`.
- The `load_data` and `run_analysis` bodies are unchanged.

**`agents.yaml`** (keeping the user's uncommitted edits):
- Add these tools:

  | Tool | Category | Modes |
  |---|---|---|
  | `rave_3dviewer_get` | exploratory | Plan, Execute |
  | `rave_3dviewer_set` | executing | Plan, Execute |
  | `shiny_ui_operate` | destructive | Execute |

- The system prompt covers:
  - the viewer's `outputId`, `viewer`, which both viewer tools need;
  - the data flow: load, then pick data, then `run_analysis`, then adjust the view with `rave_3dviewer_set`;
  - the script order;
  - what survives Re-generate: the synced controller list, the background, and the last dragged camera;
  - asking before running the quick analysis;
  - confirming views with a `shiny_query_ui` picture of the canvas;
  - a pointer to the manual.

**Manual** `agents/skills/rave-module/references/custom_3d_viewer.md`: rewritten from `TEMPLATE.md`, with its headings unchanged; the "needs rewritten" marker is dropped.
- Verified domain content is kept inside those headings:
  - the controller reference and file formats go under "2. Analysis inputs";
  - the workflows become "Procedure — …" sections;
  - the FAQ becomes Caveats;
  - programmatic use goes under "Run the pipeline without the UI";
  - a "Control the 3D viewer" subsection goes under "Drive the module with MCP tools".
- Every claim is checked against the code. Stale ones are fixed or dropped: the `loaded_brain` target name, the `custom_3d_viewer--` prefix, and "never suggest `proxy$...`".
- It includes what persists across `run_analysis`, and what `reset_viewer` and `load_data` clear.

**`modules/custom_3d_viewer/test-mcp.R`** (new). It reuses the helpers from `modules/wavelet_module/test-mcp.R`: `mcp`, `tool`, `input_info`, `wait_input`, `set_input`, `set_input_wait`, `run_script`, `operate`, `on_page`. The subject `demo/DemoSubject` is named at the top. It runs:
1. Load with the defaults.
2. Set `data_source = "Uploads"` and `uploaded_source = "electrode_value-14.fst"`; this is an existing upload, so no subject file is written.
3. Run `run_analysis`.
4. Call every get `name` with `outputId = "viewer"` (including crosshair `space` = default, `MNI152` and `all`), and every set `name` with read-back. For the camera there is no read-back: the reply and a screenshot are checked instead. Also check a wrong `outputId`, whose error lists `viewer`.
5. Error cases: the reply has `isError`, and read-back shows nothing changed.
6. Run `reset_viewer`.
7. Quick analysis:
   1. Add electrode 14 and a `default/*` streamline bundle with `add_object`.
   2. Run `open_analysis`.
   3. Click `analysis_param_run`.
   4. Read `analysis_results`.

   If DemoSubject has no streamlines, stop and ask which subject to use.

The test writes only settings.yaml (backed up and restored) and the module's own target store.

## Part E — Docs that list agent tools

- `agents/skills/rave-module/SKILL.md` tools table: add both tools, plus `shiny_ui_operate`, which is missing today.
- `agents/skills/build-module-mcp/SKILL.md` "How agents reach a module": add the viewer tools, and explain how a module adopts them: enable them in `agents.yaml`, and give the viewer's `outputId` in the manual and system prompt.
- `.github/agents/rave-module-mcp.agent.md`: add the same one-line mention.

## Verification

0. **Plan copy.** `modules/custom_3d_viewer/plan-mcp_support.md` exists and matches this plan.
1. **threeBrain.**
   - `npm run build` succeeds and the bundle is copied.
   - `devtools::document()` succeeds.
   - `R CMD INSTALL -l $SCRATCH/rlib` succeeds.
   - The MockShinySession script passes: the `controller_specs` binding and the remapped bindings.
   - The Part B prototype shows each fix: `set_display_data` and `set_focused_electrode` work; `controllers` is fresh after a Background Color change, a Voxel Display change and a slice click; specs arrive and update after `set_electrode_data`. Screenshots and the browser console (no TypeError) are checked.
2. **rave-pipelines unit level.**
   - Every edited R file parses (`Rscript -e 'parse("<file>")'`).
   - `Rscript -e 'testthat::test_file("tests/testthat/test-agent-tools.R")'` passes.
   - The catalog lists `tool__rave_3dviewer_get` (readOnlyHint) and `tool__rave_3dviewer_set` (neither hint) for custom_3d_viewer (`shidashi:::mcp_harvest_module` / `mcp_build_catalog`).
3. **Live.**
   - Back up `modules/custom_3d_viewer/settings.yaml` to the scratchpad.
   - Start the dashboard with `R_LIBS=$SCRATCH/rlib Rscript -e 'ravedash::debug_modules(".", port = 17299, launch_browser = FALSE, as_job = FALSE)'`, then run `node agents/skills/build-module-mcp/live-browser.js "http://127.0.0.1:17299/?module=custom_3d_viewer" $SCRATCH`.
   - Run `RAVE_TEST_PORT=17299 Rscript modules/custom_3d_viewer/test-mcp.R`, with screenshots after the set calls.
   - Click each converted button as a person would (Reset controller option, Add object, Configure & Run..., Run), with screenshots.
   - Check that `shiny_query_ui` returns a picture of the canvas.
   - Stop the app and restore settings.yaml from the backup.
4. **Diff review.** No visible UI change, no pipeline change, and no change inside functions that buttons call. Only the moved observer bodies are touched.
5. **Final.** Install threeBrain 1.3.0.62 into the user library, then report:
   - the changed files in both repos;
   - the behaviour changes above;
   - the threeBrain findings left open: `set_values`, `interval` controllers, the `show_modal = TRUE` driver edge case, and custom_3d_viewer's status panel reading the no-longer-existing `Intersect MNI305` controller (its MNI152 link is always blank);
   - that nothing was committed.
