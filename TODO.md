# Plan: Adapt rave-pipelines to shidashi PR #5

Align rave-pipelines with shidashi PR #5 which moves `register_output` and `standalone_viewer` into shidashi (removing them from ravedash), adds `stream_viz` D3 htmlwidget with binary streaming, and adds SVG-to-PNG capture for MCP. The old ravedash functions will no longer exist — this is not deprecation, it's removal. Comment old code instead of deleting.

---

## Phase 1: JavaScript — src/index.js (4 additions, 1 update)

- [x] Step 1.1: Add `fetchStreamData(id)` method after `_escapeAttr()` (~L936)
- [x] Step 1.2: Update MCP `query_ui` handler to use SVG capture (~L1713-L1775)
- [x] Step 1.3: Add `shidashi.set_shiny_input` handler in `_register_shiny()`
- [x] Step 1.4: Add `shidashi.register_output_widgets` handler in `_register_shiny()`
- [x] Step 1.5: Update `launchStandaloneViewer()` URL scheme to include `token`

## Phase 2: SCSS — src/shidashi.scss

- [x] Step 2.1: Add `.shidashi-output-widget-*` styles
- [x] Step 2.2: Comment out old `.ravedash-output-widget-*` styles (~L2410-2460)

## Phase 3: SVG Capture — src/canvas-capture.js

- [x] Step 3.1: Add `captureSVG(svgEl)` export

## Phase 4: Standalone Viewer Simplification

- [x] Step 4.1: Update `modules/standalone_viewer/R/loader.R`
- [x] Step 4.2: Update `modules/standalone_viewer/server.R`
- [x] Step 4.3: Update `modules/standalone_viewer/module-ui.html`

## Phase 5: register_output Migration (11 modules, 34 server + 27 UI calls)

**API change:**
- **Old (ravedash, being removed):**
  - UI: `ravedash::output_gadget_container(plotOutput(ns("id")))`
  - Server: `ravedash::register_output(outputId="id", render_function=renderPlot({...}), output_type="type")`
- **New (shidashi):**
  - UI: bare `plotOutput(ns("id"))` (no wrapper)
  - Server: `shidashi::register_output(renderPlot({...}), outputId="id", description="...", download_type="image")`

**output_type → download_type mapping:** csv→data, no-download→no-download, threeBrain→threeBrain, plots→image

- [x] Step 5.1: notch_filter (1 UI + 1 server)
- [x] Step 5.2: reference_module (3 UI + 3 server)
- [x] Step 5.3: custom_3d_viewer (2 UI + 2 server)
- [x] Step 5.4: electrode_localization (1 UI + 1 server)
- [x] Step 5.5: power_explorer (10 UI + 16 server)
- [x] Step 5.6: group_3d_viewer (1 UI + 1 server)
- [x] Step 5.7: epoch_generator (2 UI + 2 server)
- [x] Step 5.8: wavelet_module (2 UI + 2 server)
- [x] Step 5.9: generate_surface_atlas (1 UI + 1 server)
- [x] Step 5.10: connectivity_viewer (3 UI + 2 server)

## Phase 6: server.R Template Update

- [x] Step 6.1: Add `shidashi::stream_init(session)` after `shidashi::register_session_id(session)`

## Phase 7: Build & Verify

- [x] `npm run build`
- [ ] Launch app and test overlay icons
- [ ] Test standalone viewer popout
- [ ] Test MCP query_ui SVG capture
- [ ] Test stream_viz widget

---

## Decisions
- Comment old ravedash code, don't delete (for debugging)
- `ravedash::register_output` and `output_gadget_container` are being removed from ravedash
- `ravedash::plotOutput2` is kept as-is — normal output function
- SVG capture goes in `canvas-capture.js` where capture logic lives
- No `stream_viz` demo module in rave-pipelines