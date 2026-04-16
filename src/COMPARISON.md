# src/ Comparison: rave-pipelines vs shidashi PR #5

Compared against: [dipterix/shidashi#5](https://github.com/dipterix/shidashi/pull/5)
("Adds stream_viz, output widgets, etc" — `streamviz` → `main`)

Date: 2025-07-05 (updated 2026-04-06 with thorough re-comparison)

---

## Summary

The rave-pipelines `bslib-migration` branch `src/` folder is a **strict
superset** of shidashi PR #5's `bslib-bare/src/`.

A line-by-line comparison of the shidashi `streamviz` branch's
`index.js` (~2000 lines) against rave-pipelines' `src/index.js` (2250
lines) identified **one functional difference**: the generic
`data-shidashi-action="shidashi-button"` click handler needed three
changes to match the PR's behavior.

The `stream_viz` htmlwidget is delivered by the shidashi R package
(`inst/htmlwidgets/`), NOT by the template's `src/` folder — no
stream-viz JS/CSS is needed here.

---

## Updates Applied

### 1. Disabled-class guard on shidashi-button handler

PR #5 introduces `card_recalculate_badge()`, a button-style badge using
`data-shidashi-action="shidashi-button"` with class `disabled` toggled
by `enable_recalculate_badge()` / `disable_recalculate_badge()`.

Added `shidashiDataBtn.classList.contains('disabled')` check to prevent
clicks on disabled badges from firing.

### 2. Configurable event type via `data-shidashi-type`

Previously the handler hardcoded `broadcastEvent('button.click', ...)`.
The PR allows buttons to specify a custom event type via
`data-shidashi-type` (e.g. `"run_analysis"`), which is collected into
`eventData.type` by the existing attribute loop.

Added `eventData.type = eventData.type || 'button.click'` to default
the type, and changed the broadcast call to use
`broadcastEvent(eventData.type, eventData)`.

This is critical for `card_recalculate_badge()` which sets
`data-shidashi-type="run_analysis"` — without this, R's
`get_event("run_analysis")` would never fire.

### 3. Dynamic button support via `data-shidashi-dynamic`

Added `if (eventData.dynamic === "true") { eventData.message = Date.now(); }`
so buttons with `data-shidashi-dynamic="true"` generate a unique
timestamp per click. This ensures Shiny registers repeated clicks as
distinct input change events (otherwise identical payloads are
deduplicated by `onInputChange`).

---

## Feature Comparison: index.js

| Feature (from PR #5)                  | rave-pipelines | shidashi PR #5 | Notes |
|---------------------------------------|:-:|:-:|-------|
| `fetchStreamData()` method            | ✅ | ✅ | rave-pipelines adds `uint8` and `int16` types |
| `set_shiny_input` handler             | ✅ | ✅ | Identical — sets input with optional `priority: 'event'` |
| `register_output_widgets` handler     | ✅ | ✅ | Injects download/popout overlay icons on registered outputs |
| `_captureSVG` / SVG rasterisation     | ✅ | ✅ | rave-pipelines has richer impl in `canvas-capture.js` |
| SVG handling in `query_ui`            | ✅ | ✅ | rave-pipelines delegates to `captureVisualContent()` (async, multi-element) |
| `register_module_token` handler       | ✅ | ✅ | Identical |
| `update_chat_status` handler          | ✅ | ✅ | Identical |
| `toggle_stop_button` handler          | ✅ | ✅ | Identical |
| `standalone_viewer` popout URL        | ✅ | ✅ | rave-pipelines: `launchStandaloneViewer()`; PR: inline in `register_output_widgets` |
| Disabled guard on shidashi-button     | ✅ | ✅ | **Added** by this update |
| `data-shidashi-type` event type       | ✅ | ✅ | **Added** by this update — `eventData.type || 'button.click'` |
| `data-shidashi-dynamic` timestamps    | ✅ | ✅ | **Added** by this update — `Date.now()` for unique events |

## Feature Comparison: shidashi.scss

| CSS Feature                              | rave-pipelines | shidashi PR #5 |
|------------------------------------------|:-:|:-:|
| `.shidashi-output-widget-container`      | ✅ | ✅ |
| `.shidashi-output-widget-wrapper:hover`  | ✅ | ✅ |
| `.shidashi-output-widget-icon`           | ✅ | ✅ |
| Drawer styles                            | ✅ | ✅ |
| Chatbot stop button styles               | ✅ | ✅ |
| Resize handle styles (v+h)               | ✅ | ✅ |
| Dark-mode tweaks                         | ✅ | ✅ |

No SCSS changes needed — all sections identical between both branches.

## Helper Files (no changes needed)

| File                   | Status |
|------------------------|--------|
| `output-bindings.js`   | Identical to shidashi |
| `chat-helpers.js`      | Identical to shidashi |
| `iframe-manager.js`    | Identical to shidashi |
| `sidebar.js`           | Identical to shidashi |
| `canvas-capture.js`    | rave-pipelines only (superset of shidashi's inline `_captureCanvas`/`_captureSVG`) |

## Identical Sections (confirmed line-by-line)

- `ensureShiny`, `bindAll`, `unbindAll`
- `fromLocalStorage`, `cleanLocalStorage`, `_setSharedId`, `_setPrivateId`, `broadcastSessionData`
- `_reportActiveModule`
- `broadcastEvent`, `registerListener`
- `_col2Hex`, `_reportTheme`, `isDarkMode`, `_updateThemeIcon`, `asLightMode`, `asDarkMode`
- `click`, `triggerResize`
- `tabsetAdd`, `tabsetRemove`, `tabsetActivate`
- `_cardOperate`, `_updateCardIcon`, `_updateMaximizeIcon`, `toggleCard2`, `flipBox`
- `createNotification`, `clearNotification`, `_getToastContainer`
- `setProgress`
- `scrollTop`, `matchSelector`, `shinyHandler`, `_escapeHtml`, `_escapeAttr`
- `_initResizeHandles` (vertical + horizontal)
- `drawerOpen`, `drawerClose`, `drawerToggle`
- `_bindCardTools` (card widgets, chat-toggle, flip-box dblclick)
- All shared Shiny handlers (click, box_flip, card_tabset_*, cardwidget, card2widget,
  add/remove_attribute, drawer_*, activate_drawer_tab, register_module_token,
  set_shiny_input, register_output_widgets, update_chat_status, init_chat_stop_button,
  toggle_stop_button, ask_user, init_chat_code_copy, show_notification,
  clear_notification, set_progress, make_scroll_fancy, cache_session_input,
  get_theme, reset_output, shutdown_session, open_iframe_tab, set_html,
  accordion, add_class, remove_class, open_url)
- Storage listener, sidebar nav, tab change listener, initial tab reporting
- hljs initialization, bootstrap init sequence

## Files NOT needed in src/

| Component          | Location in shidashi         | Why not in src/ |
|--------------------|------------------------------|-----------------|
| `stream_viz` widget | `inst/htmlwidgets/`          | Delivered by R package via htmlwidgets, not template source |
| `stream_main.js`   | `inst/stream-viz-src/`       | Separate esbuild bundle for the htmlwidget; has own package.json |
| R-side APIs         | `R/stream.R`, `R/card-badge.R`, etc. | Pure R code in the shidashi package |

## rave-pipelines extras (not in shidashi)

These are rave-specific additions in `src/index.js` that go beyond shidashi:

- Constructor RAVE state: `_moduleId`, `_raveId`, `_active_module`, `_bodyClasses`, `variableBodyClasses`
- `shinySetInput()` with `children` cross-iframe forwarding
- `openURL()` with toast notification (vs shidashi's simple `openUrl()`)
- `launchStandaloneViewer()` — constructs `?module=standalone_viewer&outputId=...&token=...`
- `addClass()` / `removeClass()` with body class tracking
- `setInnerHtml()` helper
- `notifyParent()`, `notifyIframes()`, `resumeStatus()` — cross-frame integration
- `captureVisualContent()` — unified async capture (SVG + multi-canvas compositing + data-URI img)
- `.rave-button[rave-action]` click handler with JSON parsing + iframe broadcast
- `.shidashi-button[shidashi-action]` class-based button handler
- `.toggle-advance-options` handler
- `.ravedash-output-widget[data-type="standalone"]` click handler
- Ctrl+Enter → `run_analysis` keyboard shortcut
- Internal event system for `set_current_module` → standalone viewer link updates
- `_dismissPreloader()` called on init + shiny:connected
- `hide_header` / `show_header` handlers with parent notification
- Enhanced `set_current_module` handler with RAVE state tracking
- Enhanced `card()` method accepting `{selector, method}` object
- `fetchStreamData()` extra types: `uint8`, `int16`
- `shiny:idle` handler to auto-hide `.hide-on-shiny-idle` toasts
- `_initBackToTop()` with BS3→BS5 migration for ravedash
