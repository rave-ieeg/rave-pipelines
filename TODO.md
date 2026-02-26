# Migration Plan: AdminLTE3 → bslib/Bootstrap 5

Reference template: `unused/bslib-bare/` (copied from `shidashi` bslib-migration branch)

## Overview

Migrate rave-pipelines from AdminLTE3 + Bootstrap 4 (webpack) to bslib + Bootstrap 5 (esbuild).
RAVE-specific customizations (extra navbar buttons, `frontpage()`, Jupyter handlers,
shutdown button, `RAVEPipeline` global name) must be preserved.

---

## Tasks

### 1. HTML Views (`views/`)

- [x] **header.html** — `suppressDependencies("bootstrap")` → `shidashi::bslib_dependency()`;
  replace versioned `shidashi.js` script tag with `shidashi/css/shidashi.css` +
  `shidashi/js/index.js`; add Font Awesome 5 CDN link
- [x] **footer.html** — `$(document).on(...)` → `document.addEventListener(...)`;
  `RAVEPipeline.registerShidashi` → `Shidashi.registerShidashi`
- [x] **404.html** — `sidebar-mini` removed; `wrapper`/`content-wrapper` → `shidashi-wrapper`/`shidashi-content`;
  BS4 classes → BS5 (`float-sm-right` → `float-sm-end`, `error-page` → `d-flex`)
- [x] **500.html** — Same pattern as 404
- [x] **card.html** — `collapsed-card` → `shidashi-collapsed`; `<h3>` → `<h5>`
- [x] **card2.html** — `collapsed-card` → `shidashi-collapsed`; `<h3>` → `<h5>`;
  `data-widget="chat-pane-toggle"` → `data-shidashi-action="chat-toggle"`
- [x] **card-tabset.html** — `pt-1` → `pt-0`; `<h4>` → `<h5>`;
  `data-toggle` → `data-bs-toggle`; remove lorem ipsum example tabs
- [x] **accordion-item.html** — Remove extra `'card'` class; simplify header classes;
  `data-toggle` → `data-bs-toggle`; `data-parent` → `data-bs-parent`;
  fix `aria-expanded` logic; fix `style_body` bug (was reading `style_header`)
- [x] **menu-item.html** — `nav-item` → `shidashi-nav-item`; `nav-link nav-leaf` → `shidashi-nav-link`;
  `<p>` → `<span>`
- [x] **menu-item-dropdown.html** — `nav-item` → `shidashi-nav-item shidashi-nav-group`;
  all nav classes → `shidashi-nav-*`; `<p>` → `<span>`; arrow icon class update
- [x] **preview.html** — `layout-top-nav` removed; AdminLTE wrappers → `shidashi-*`;
  `col-xs-12` → `col-12`
- [x] **info-box.html** — No functional change needed (already matches target)

### 2. Main Layout (`index.html`)

- [x] Replace AdminLTE layout with bslib structure:
  - `wrapper` → `shidashi-wrapper`
  - Sidebar: `<aside class="main-sidebar ...">` → `<nav class="shidashi-sidebar ...">`
  - Content: `<div class="content-wrapper iframe-mode" data-widget="iframe">` →
    `<div class="shidashi-content" data-shidashi-widget="iframe-manager">`
  - Tab bar: AdminLTE iframe plugin → `shidashi-tab-bar` structure
  - All `data-widget` → `data-shidashi-*` attributes
  - `ml-auto` → `ms-auto` (BS5)
  - **Preserve** RAVE buttons: toggle_loader, citation_information, shutdown
  - **Preserve** `{{ frontpage() }}` in the empty tab content
  - Remove hidden footer and control-sidebar

### 3. R Files

- [x] **R/common.R** — Update `nav_class()` to `shidashi-header` classes;
  update `body_class()` to remove AdminLTE-specific classes;
  add `sidebar_class()` function; keep RAVE-specific `frontpage()`
- [x] **ui.R** — Keep `shidashi::adminlte_ui()` (same function used by bslib template)
- [x] **server.R** — No changes needed (module routing is framework-agnostic)

### 4. JavaScript Source (`src/`)

- [x] Replace `src/index.js` with bslib-bare version (new `ShidashiApp` class with
  BS5 Toast notifications, `data-shidashi-*` delegation, `Sidebar`/`IFrameManager` imports)
- [x] Add `src/sidebar.js` — custom sidebar toggle/treeview/search/active-state
- [x] Add `src/iframe-manager.js` — custom iframe tab management
- [x] Replace `src/scss/shidashi.scss` → `src/shidashi.scss` (top-level, bslib theme)
- [x] Remove old files: `src/js/AdminLTE/` (16 files), `src/js/class-shidashi.js`,
  `src/js/common.js`, `src/js/scrollbars.min.js`, `src/js/shiny-clipboard.js`,
  `src/js/shiny-progress.js`, `src/scss/AdminLTE/` (entire directory)
- [x] Remove `src/build.R` (was webpack + version stamping; no longer needed)

### 5. Build Tooling

- [x] Replace `webpack.config.js` with `esbuild.config.mjs`
- [x] Update `package.json`:
  - Remove: `admin-lte`, `bootstrap@^4`, `highlight.js`, `overlayscrollbars`,
    webpack devDeps (`css-loader`, `exports-loader`, `imports-loader`, `sass-loader`,
    `scss`, `style-loader`, `webpack`, `webpack-cli`)
  - Add: `bootstrap@^5.3.3`, keep `clipboard`
  - Add devDeps: `esbuild`, `esbuild-sass-plugin`, `sass`
  - Update scripts: `build` → `node esbuild.config.mjs`
- [x] Delete `webpack.config.js`

### 6. Static Assets (`www/`)

- [x] Remove `www/bootstrap/` (BS4 bundle — now provided by bslib R package)
- [ ] Consider removing `www/highlightjs/` if no longer needed
- [x] Keep `www/shidashi/img/`
- [x] Rebuild `www/shidashi/js/` and `www/shidashi/css/` via `npm run build`

### 7. Verification

- [x] `npm install && npm run build` succeeds
- [ ] `Rscript -e 'shidashi::render(".")'` succeeds
- [ ] Launch app, verify: sidebar, iframe tabs, theme toggle, card operations, RAVE buttons

---

## Key Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Global JS lib name | `Shidashi` → change from `RAVEPipeline` | Align with upstream template; footer.html updated to match |
| highlight.js | Keep `www/highlightjs/` for now | May be used by modules; remove later if confirmed unused |
| `src/build.R` | Remove | Version stamping no longer needed; esbuild handles build |
| `shidashi::adminlte_ui()` | Keep as-is | bslib template also uses this function name |
