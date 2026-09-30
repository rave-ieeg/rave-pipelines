# Plan: MCP support for `compatibility_rave1` (Data Tools)

## Context

The Data Tools module (`compatibility_rave1`) has three jobs: check a subject's files ("Data integrity check"), convert a subject to the RAVE 1.0 format ("Backward compatibility"), and export epoched data for MATLAB, Python and R ("Export data"). Agents can barely use it today:
- `load_data` and `run_analysis` (the "Validate subject" button) exist, but have no descriptions;
- none of its own inputs are registered (the ravedash presets already register `loader_project_name` and `loader_subject_code`);
- "Generate exports" is a plain observer;
- validation results exist only in a `uiOutput`, inside a card that loading collapses;
- there is no `agents.yaml`, manual, or `test-mcp.R`.

The user asked for MCP access that follows `agents/skills/build-module-mcp/SKILL.md` (and `.github/agents/rave-module-mcp.agent.md`).

**Decisions made with the user (2026-09-30):**
- **Scope.** Agents get the integrity check and data export only.
- **Validation** writes nothing, so agents run it without asking.
- **Export.** Agents always confirm the export settings with the user before running it.
- **RAVE 1.0 conversion is forbidden to agents.** It is rarely used, and the user does it by hand.
  - It gets no script, and its observer stays exactly as it is.
  - The button is registered with `writable = FALSE`: agents see it, but `shiny_input_update` refuses it.
  - `shiny_ui_operate` is left out of this module. It clicks any element, and a call goes only to an open module that offers the tool (`mcp_resolve_module` in shidashi's `mcp-module.R`). So no tool can reach the button. The cost: agents can't close alerts, so the user closes the export's "Success!" alert.
- **Test subjects.** Exports go to `demo/DemoSubject`. `demo/KC` is only loaded, to show failing checks.

**Rules also live in the descriptions.** Over MCP, the Ask/Plan/Execute modes don't gate tools; shidashi's `mcp-handler.R` leaves confirmation to the agent's own chat. So the rules also go into the script and input descriptions and the manual, not only into the `agents.yaml` system prompt.

**Step 0, right after approval:** save this plan as `modules/compatibility_rave1/plan-mcp_support.md`. Plan mode allows edits only to the plan file.

**Visible UI changes: none.** No pipeline changes, no `register_output`.

**Code that buttons run.**
- The "Generate exports" observer body moves verbatim into a script, and the button then runs that script.
- Two scripts get one extra last line whose value people never see: `load_data` returns a summary, and `generate_exports` returns the export folder. The skill asks for this ("return a short summary"), and wavelet's `load_data` already does it.
- Nothing else that a button runs changes. The conversion observer and `export_repository()` stay untouched.

**No commits.** The user's uncommitted edits in other modules are not touched.

## Inventory

| Button / action | Reads | Writes | Agent path |
|---|---|---|---|
| Load subject | `loader_project_name`, `loader_subject_code` | `settings.yaml`, target `subject` | script `load_data` (exists): add a description and a summary result |
| Validate subject (`run_analysis_button`) | `validation_version`, `validation_mode` | nothing; results go to `validation_check` | script `run_analysis` (exists): add a description |
| (none) | latest validation results | nothing | **new read-only script `validation_results`** |
| Make this subject RAVE 1.0 compatible (`compatibility_do`) | loaded subject | subject files | **none, by decision.** Registered read-only, so agents see it and tell the user to click it. |
| Generate exports (`export_do`) | `export_type`, `export_electrode`, `export_reference`, `export_epoch`, `export_pre`, `export_post` | new folder `rave/exports/rave-repository/export-<time>` | **new script `generate_exports`**; the button runs it |
| Export & download (`export_download_do`) | same | zip downloaded by the browser | people only: agents run `generate_exports` and give the folder path |
| Quick access links (`quickaccess_*`) | none | expand one card, collapse the others | registered, so `shiny_input_update` clicks them |
| Loader "Sync from ..." | none | loader inputs | not needed: agents set the loader inputs |

**Gaps, and the way around each without a UI or pipeline change:**
1. Validation results are a plain `uiOutput` in a card that is collapsed after loading, and hidden outputs don't update. `register_output` would add visible buttons. → A read-only script, `validation_results`, plus clickable Quick access links for showing the card to the user.
2. "Export & download" is a browser download. → Agents run `generate_exports` and report the folder. A user who wants a zip clicks the button.
3. A bad export input fails with "Please correct the inputs before exporting data", which doesn't say which input. → Each input's description states its rule, and the messages under the inputs can be read with `shiny_query_ui` (to be checked live).

## Changes

### 1. `modules/compatibility_rave1/R/module_html.R`: register inputs (tags unchanged)

Each input below gets a `shidashi::register_input(<tag>, inputId, update, description)` wrapper. Read-only registration follows ravedash's presets (e.g. `preset-loader-subject.R`).

| inputId | `update` | The description says |
|---|---|---|
| `quickaccess_data_integrity`, `quickaccess_compatibility`, `quickaccess_export` | `shiny::updateActionLink` | Expands that card and collapses the other two. Open a card before reading its outputs with `shiny_query_ui`, because hidden outputs don't update. |
| `validation_version` | `shiny::updateSelectInput(value=selected)` | `2` (default) checks the RAVE 2.0 files; `1` checks the RAVE 1.0 files that the conversion adds. |
| `validation_mode` | same | `basic` checks small files only (folders, preprocess settings, meta tables). `normal` (default) also checks the voltage, power and phase data, epochs and references, and is slower. |
| `compatibility_do` (**`writable = FALSE`**) | `dipsaus::updateActionButtonStyled` | "Make this subject RAVE 1.0 compatible" rewrites the subject's files for RAVE 1.0 modules. Only the user may click it; agents never run the conversion, and point the user to this button instead. |
| `export_type` | `shiny::updateSelectInput(value=selected)` | `power` (needs the wavelet), `voltage` (needs Notch), or `raw-voltage`. Set it first: it resets `export_reference` (only `noref` for raw-voltage) and `export_epoch`. |
| `export_electrode` | `shiny::updateTextInput` | Channels, e.g. `14-15`; blank exports all. At least one channel must be valid for the chosen reference. |
| `export_reference`, `export_epoch` | `shiny::updateSelectInput(value=selected)` | One of the subject's reference or epoch names; `load_data` lists them. |
| `export_pre`, `export_post` | `shiny::updateNumericInput` | Seconds around each onset. Pre must be negative (default -1); post must be positive (default 2). |

These are deliberately **not** registered:
- `export_do`. `shiny_input_update` can click any registered, writable button, but that click returns neither the folder nor the error. Agents run the script `generate_exports` instead.
- `export_download_do`, the download button.
- The Validate button, which script `run_analysis` covers.

### 2. `modules/compatibility_rave1/R/loader.R`: `load_data`
- **Description.** It says what the script loads, that any subject loads (even a broken one, since checking such subjects is this module's job), and what it returns.
- **Summary as the last line**, as in `modules/wavelet_module/R/loader.R`: `"Loaded <project>/<subject>: electrodes …; Notch-filtered …; wavelet …; epochs: …; references: …"`. The epoch and reference names are the export choices. The summary is wrapped in `tryCatch`, so a broken subject still loads for people.

### 3. `modules/compatibility_rave1/R/module_server.R`: scripts

These follow the build-module-mcp "Script pattern".

- **`run_analysis`**: gets a `description`; the body is unchanged. The description says:
  - it does the same as "Validate subject" and reads `validation_version` and `validation_mode`;
  - it writes nothing, so there is no need to ask;
  - `normal` mode reads all the data and shows "Validation in progress..." while it runs;
  - the results are read with `validation_results`.

  What `output` shows is filled in after the live run.
- **`validation_results`** (new, read-only):
  - It reads `local_reactives$validation_results` and walks the same keys and items as `output$validation_check`.
  - It returns at most 100 lines:
    - the count of checks by status;
    - one line `[failed|minor|skipped] <part>/<check>: <description> — <reason>` for each check that did not pass;
    - a last line listing the checks that passed.
  - Statuses follow the card's colours: `valid` TRUE is passed, NA is skipped, and FALSE is minor if `severity == "minor"`, otherwise failed. ravecore marks only low-priority paths as minor (cache, FreeSurfer, notes, pipelines).
  - Before any validation has run, it returns "No validation results yet: run `run_analysis` first."
- **`generate_exports`** (new): the body of the `input$export_do` observer, moved verbatim, plus a last line `path`, so the script returns the export folder. The observer keeps `error_wrapper = "notification"` and calls `server_tools$trigger_script("generate_exports")`. The description says:
  - it does the same as "Generate exports";
  - **always confirm the export settings with the user first**;
  - the inputs it reads, and the error when one is invalid (nothing is written then);
  - the folder layout: `summary.yaml`, `electrodes.csv`, `reference.csv`, `with_epochs/epoch.csv`, `with_epochs/<type>/chNNNN.mat`;
  - it creates a new folder each time, and never overwrites;
  - it ends with a "Success!" alert that stays until the user closes it.
- The `input$compatibility_do` observer, `export_repository()` (shared with the download button) and `expand_card()` stay unchanged.

### 4. `modules/compatibility_rave1/agents.yaml` (new)

Copy `modules/notch_filter/agents.yaml`, without `shiny_ui_operate`. The system prompt covers:
- the module and its ID, the mode capabilities, the data flow, and a tools table (manual first);
- the order: `load_data`, then either task:
  - validate: set `validation_version` and `validation_mode`, run `run_analysis`, then `validation_results`; no need to ask;
  - export: set `export_type` first, then the other inputs, and check them with `shiny_input_info`. **Always confirm the settings with the user**, then run `generate_exports` and report the folder it returns;
- **never convert to RAVE 1.0**: if the user asks, point them to the "Backward compatibility" card's button (open it with `quickaccess_compatibility`), and suggest validating with Data version 1 afterwards;
- that the cards collapse after loading, so agents open one with its Quick access link to show it to the user;
- that alerts ("Success!") stay open until the user closes them;
- that "Export & download" is for the user to click;
- that in Plan mode, agents ask the user to click instead.

### 5. Manual `agents/skills/rave-module/references/compatibility_rave1.md` (new)

Title "# Data Tools Module Reference", built from `TEMPLATE.md` with its headings unchanged. The content comes from the code and the ravecore docs for `validate_subject`, `prepare_subject_*_with_epochs` and `export_matlab`. It covers:
- the three cards and their inputs;
- what each group of checks covers;
- the export folder layout;
- procedures:
  - check a subject after preprocessing;
  - export power around an epoch, confirming the settings with the user first;
  - check a subject that the user converted by hand, with Data version 1;
- caveats, including:
  - the conversion is for people only;
  - after a failed back-port, the wavelet module's "Wavelet done, but..." alert sends users to this module;
- the plain-R equivalents for validation and export: the pipeline only loads `subject`, and each button calls ravecore directly;
- the MCP section.

### 6. `modules/compatibility_rave1/test-mcp.R` (new)

The helpers come from `modules/wavelet_module/test-mcp.R`, without `operate`. The subjects are named at the top, and setting `do_write <- FALSE` stops the test before the export. Nothing converts a subject. The steps:
1. **Protocol.** The tools and meta tools respond, and the manual loads.
2. **Failing checks on `demo/KC`** (not imported; nothing written):
   - Load it and run a `basic` validation.
   - `validation_results` equals a direct `ravecore::validate_subject(verbose = FALSE)`: same counts and same failing checks, exactly.
3. **The conversion button is locked** (still on demo/KC, where a click would show "Converting in progress", then "Conversion failed"):
   - `shiny_input_info` shows `compatibility_do` with `writable: false`;
   - `shiny_input_update` on it returns `isError` ("read-only");
   - `tool__shiny_ui_operate` aimed at this module returns `isError` ("no open module offers it");
   - no alert appears.
4. **Main subject `demo/DemoSubject`** (passes all 35 checks; epochs `auditory_onset` with 287 trials and `UTO_PAV069_5modality_ALL`; references `default` and `noref`):
   - Load it; the result lists those epochs and references.
   - Open the card and run a `normal` validation. `validation_results` equals the direct computation, and the card shows the results (`shiny_query_ui`).
5. **Exports** (writes one new folder, about 20 MB):
   - Setting `raw-voltage` resets `export_reference` to `noref`.
   - A bad `export_pre` gives `isError`, no new folder, and no alert.
   - Valid inputs (power, channels `14-15`, `auditory_onset`, `default`, -1 to 2 s):
     - the returned folder is the only new folder, and it is fresh;
     - the `summary.yaml` fields match exactly;
     - `ch0014.mat` and `ch0015.mat` match the inputs: 287 trials; times from -1 to 2 s at 100 Hz; the 16 frequencies in `meta/frequencies.csv`;
     - the "Success!" alert shows the same path.

## Verification
0. The plan copy exists.
1. Every edited R file parses (`Rscript -e 'parse("<file>")'`).
2. **Catalog.** `shidashi:::mcp_harvest_module("compatibility_rave1")` lists the module's tools without `shiny_ui_operate`, and `module_interactive_script_list` lists the four scripts: `load_data`, `run_analysis`, `validation_results`, `generate_exports`.
3. **Live** (the skill's recipe):
   - Back up `modules/compatibility_rave1/settings.yaml` and the tracked `_targets.yaml` to the scratchpad.
   - Start `ravedash::debug_modules(".", port = 17299, …)`, and run a scratchpad copy of `live-browser.js` on `?module=compatibility_rave1`. The copy adds a `click` control file (a CSS selector) so that buttons can be clicked as a person would; the repo file is not changed.
   - Run `RAVE_TEST_PORT=17299 Rscript modules/compatibility_rave1/test-mcp.R`, taking screenshots after the validation and the export alert.
   - **Person clicks** on demo/DemoSubject:
     - close the test's "Success!" alert with its button;
     - "Generate exports" with a bad input shows the error notification;
     - with valid inputs, it shows "Success!" and adds one more folder (about 20 MB);
     - "Validate subject" still shows the results.
   - Stop the app, restore both files, and check that `git diff` shows only the intended files.
4. **Diff review** against the Done checklist:
   - no UI or pipeline change;
   - the conversion observer is unchanged;
   - no second code path, and the old export observer body is gone.
5. **Report:**
   - changed files;
   - behaviour changes for people (none expected);
   - the subject files written: two export folders in demo/DemoSubject, the first of which creates its `rave/exports/`;
   - the findings below.

## Found, not changed
- ravecore's `rave_legacy_subject_format_conversion()` reads `data/voltage/<ch>.h5` for every channel, including the non-LFP channels it says it ignores. On `test2/DemoSubject`, whose Auxiliary channel 24 has no data files, it would fail, possibly after writing to channels 13-16.
- `test2/DemoSubject`'s `epoch_auditory_onset.csv` lists blocks 010-012, which test2 doesn't have. Its epoch check fails with "… not imported: NA": the message shows `NA` instead of the missing blocks.
- `DT::DTOutput("validation_table")` (the back of the Data integrity flip box) has no `output$validation_table`, so that side is blank.
