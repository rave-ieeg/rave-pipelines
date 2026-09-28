# Reference Channels Module Reference

The Reference Channels module (`reference_module`) re-references iEEG voltage
signals. You split the channels into groups and give each group one reference
scheme: no reference, common average (CAR), white-matter, or bipolar. The result
is a named reference profile (`[subject]/meta/reference_<name>.csv`) that later
modules load by its "Reference name".

**Prerequisite:** Run the **Notch Filter** module (`notch_filter`) first; the
module refuses to load a subject whose signals are not notch-filtered. Wavelet is
optional; only the "Reference signal" power heatmap uses it.

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Run the pipeline without the UI](#run-the-pipeline-without-the-ui)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: In the loader screen, choose the RAVE **Project** and **Subject**. (Optional:
use "Sync ..." to copy them from a recently used module or from the Wavelet
module.)
Step 2: Under **Presets**, choose where to start ("Create from existing reference
profile"):
* `[Blank profile]` starts fresh. Channels are grouped by their electrode label
  with trailing digits removed (labels `LA1`...`LA12` form group `LA`). Channels
  whose `LocationType` is `sEEG` start with a bipolar preset.
* An existing profile name (e.g. `default`, `noref`) loads that profile's
  groups and references, so you can edit them.

Step 3: Click **"Load subject"**.

### 2. Analysis inputs

Work top to bottom on the left panel: first the groups, then each group's reference.

**"Electrode groups" card**

* **Group name / Electrodes** (one row per group; add or remove rows with the
  `+`/`-` buttons) — a group lists the channels *to be referenced*. The channels
  they are referenced *to* may lie outside the group (group `LA` = `1-11` may use
  channel `12`, or the average of `1-100`). Rules: no channel in two groups, only
  LFP channels, and unique names. Electrodes accept numbers and ranges, e.g.
  `1-10,15`.
* **"Set groups"** — validates and applies the groups, then opens the
  "Reference settings" card. It **resets the reference of every group**.

**"Reference settings" card**

* **Group name** — the group to configure.
* **Reference type** — `No Reference`, `Common Average Reference`,
  `White-matter Reference`, or `Bipolar Reference`. Default is the group's
  current type.
* **Reference to** (common average / white-matter only) — an existing reference
  signal, named after the channels it averages (e.g. `ref_1-16,18-20`), or
  `[new reference]` to create one:
  * Type the channels to average (e.g. `1-16,18-20`) and click **"Generate"**,
    then **"Confirm"**. This creates the signal and selects it.
  * Or leave the box blank and click **"Calculate from least anti-correlation"**.
    This opens the CARLA dialog, which picks the channels whose average is least
    anti-correlated with the rest. Its settings:
    * Channels to include: default all LFP channels.
    * Epoch: default the first epoch.
    * Start / End time: default `0.01` to `1` s after onset.
    * Bootstrap samples: default `100`.
    * Minimum size: default blank, meaning automatic.
    * Options "Modified CARLA", "Sensitive", and "Absolute data": all on by default.

    **"Calculate"** fills the channel box; then click **"Generate"** as above.
* **"Open Bipolar reference editor"** (bipolar only) — a table where each channel
  references the next channel of the group, in electrode-number order. The last
  channel of the group gets `noref` (it is typically outside the brain).
  Double-click a Reference cell to change it: enter a channel number, an existing
  `ref_*` signal, `n` for no reference, or leave it blank for an empty reference.
* **"Confirm changes & visualize"** — applies the group's settings to the
  reference table. Repeat for every group.

Save from the **"Preview & Export"** tab: enter a **Reference name** (letters,
digits, underscore) and click **"Generate & save"**. The line above the button
says whether this creates a new profile or overwrites an existing one.

### 3. Outputs

* **Group inspection** (`reference_plot_signals`) — voltage traces of the
  selected group. Original traces are blue and referenced traces gray, overlaid.
  For common average, the top row `REF` (orange) is the reference signal, and
  channels inside the average are green. Channels with an empty reference are
  red. Controls: Session block, Start time, Plot duration, Vertical spacing, Hide
  electrodes, and Signal type (both, original only, referenced only). Good
  referencing removes noise shared across channels. A flat, spiky, or drifting
  trace is a candidate to exclude from the reference.
* **Electrode details** (`reference_plot_electrode`) — diagnostics for one
  channel: the trace, and Welch periodograms of the raw (gray) and referenced
  (blue) signal. Step through channels with Previous / Next. Look for line-noise
  peaks or broadband power that referencing should reduce.
* **Reference signal** (`reference_plot_heatmap`) — baselined power (trials ×
  time) of the group's common-average or white-matter reference signal around an
  epoch. A reference with strong task-evoked power carries signal into every
  channel; consider other channels, e.g. from CARLA.
* **Preview & Export** (`reference_table_preview`) — the reference table: one row
  per channel with Electrode, Group, Reference, and Type.
* **3D viewer** (under "Reference settings") — the selected group on the brain:
  [1] Valid (blue), [2] Within CAR (green), [3] Ignored, i.e. empty reference
  (red), [4] Other channels (gray).

## Common procedures

Recipes for the common references. In each one, "apply the group" means: choose it
in **Group name**, set **Reference type** (and **Reference to**), then click
**"Confirm changes & visualize"**.

**Excluded ("bad") channels.** Channels the user considers bad are *excluded from
the reference*, not removed: they stay in the table and are still referenced, so
they keep a chance to show physiology (a channel may look bad yet respond well,
and channels with special responses are often excluded only to keep their
artifacts out of other channels). The procedures below show how to leave them out
of the reference signals.

### Procedure — Bipolar reference by electrode label

Step 1: Load the subject with `[Blank profile]` (see [Load data](#1-load-data)).
The groups are the electrode labels without trailing digits: one group per shaft,
strip, or grid.
Step 2: If channels are excluded, take them out of their groups and put them in a
group of their own (e.g. `Excluded`). The chain then skips them: with `17`
excluded from `16-20`, the group becomes `16,18-20` and `16` references `18`.
Step 3: Click **"Set groups"**.
Step 4: Apply every label group with type `Bipolar Reference` (optionally review
the chain in "Open Bipolar reference editor"). Apply the `Excluded` group with
`No Reference`, so the excluded channels get `noref`.
Step 5: In "Preview & Export", name the profile (e.g. `bipolar`) and click
**"Generate & save"**.

Result for `16-20` with `17` excluded: 16→`ref_18`, 17 `noref`, 18→`ref_19`,
19→`ref_20`, 20 `noref`.

### Procedure — Custom groupings

Groups need not follow the labels. Edit the rows before **"Set groups"**, e.g.
`LA` = `1-5`, `LB` = `6-10`, `LC` = `11-15`, `LD` = `16-20`, or split a long
shaft into grey- and white-matter parts. Then apply each group as in the other
procedures. Remember that a group is the set of channels being referenced, not the
channels they are referenced to.

### Procedure — Common average of all channels but the excluded ones

Step 1: Load the subject. Choose one grouping (excluded channels stay in their
groups):
* **One group**: replace the rows with a single group `CAR` containing every LFP
  channel.
* **From `noref`**: load the `noref` profile (if the subject has one), which puts
  every channel in one group, and rename that group to `CAR`.
* **Keep the label groups**: keep one group per label and give every group the
  same reference signal in Step 4 (the average may include channels outside each
  group).

Step 2: Click **"Set groups"**.
Step 3: Choose the first group, type `Common Average Reference`, **Reference to**
`[new reference]`. Enter all LFP channels except the excluded ones (e.g.
`1-16,18-20` when `17` is excluded), click **"Generate"**, then **"Confirm"**.
Step 4: Click **"Confirm changes & visualize"**. For the remaining groups, choose
the same `ref_*` signal in **Reference to** and confirm.
Step 5: Save (e.g. `car`).

Every channel, the excluded ones included, is referenced to `ref_1-16,18-20`.

### Procedure — Common average from CARLA

Steps 1-2: reuse [Procedure — Common average of all channels but the excluded ones](#procedure--common-average-of-all-channels-but-the-excluded-ones).
Step 3: On the first group, choose `Common Average Reference` and `[new reference]`,
and leave the channel box blank. Click **"Calculate from least anti-correlation"**.
In the dialog, remove the excluded channels from "Channels to include", check the
epoch, and click **"Calculate"**. Then click **"Generate"** and **"Confirm"**.
Steps 4-5: as above.

### Procedure — Mixed reference types

Apply each group with its own type. For example, sEEG shafts with
`Bipolar Reference` and a grid with `Common Average Reference`. Groups left
untouched keep the blank profile's default (usually `No Reference`).

### Procedure — Edit an existing reference profile

Step 1: Load the subject with the profile's name under **Presets**.
Step 2: Click **"Set groups"**. This rebuilds the table, so **apply every group
again**, including groups you are not changing.
Step 3: Save under the same name (overwrite) or a new one.

## Caveats

* **Notch first.** The subject must be notch-filtered.
* **Groups are the referenced channels.** The reference channels can be anywhere
  (outside the group, or an average of any channels).
* **Exclude from the reference, do not remove.** Leave excluded channels out of
  reference signals (CAR channel lists, CARLA candidates) and out of bipolar
  chains, but keep a reference for them (`noref` or the common average).
* **Empty reference vs `noref`.** Channels with an empty reference are skipped
  when later modules load the data (by default). `noref` keeps the channel,
  unreferenced.
* **"Set groups" resets everything.** It rebuilds the table from the blank
  profile and clears every group's reference, even when you loaded an existing
  profile. Apply every group after it.
* **Bipolar order.** The chain follows electrode numbers within the group, and the
  last channel gets `noref`. Check that numbering follows the contacts along the
  shaft, or edit the table.
* **Generating a reference writes to the subject.** It creates
  `[subject]/reference/ref_<channels>.h5` and clears the subject's cached data.
  Generating the same channels again replaces the file.
* **Saving overwrites** a profile with the same name.
* **CARLA needs a proper epoch.** The subject needs an epoch whose `Block` names
  match its recording blocks. CARLA was designed for evoked responses (e.g.
  CCEP); adjust the time window for other tasks. Bootstrapping makes the result
  vary slightly between runs.
* **Blank-profile groups come from `Label`** (trailing digits removed), not from
  the `LabelPrefix` column of `electrodes.csv`.
* **Error dialogs from scripts.** When a script run through MCP fails while it
  shows a progress dialog, an "Error found!" dialog stays open in the browser
  until someone clicks **"Confirm"**.

## Run the pipeline without the UI

The UI converts its inputs before writing `settings.yaml`:
* The groups become `electrode_group`, a list of `list(name, electrodes)`.
* Each applied group becomes one item of `changes`, with fields `group_name`,
  `electrodes`, `reference_type`, and `reference_signal`. For common average and
  white-matter, `reference_signal` is one `ref_*` name. For bipolar, it is one
  reference per channel, in the row order of the `reference_group` target.
* The CARLA dialog becomes `carla_params`.
* Creating a signal and saving the profile are not pipeline targets: use
  `ravecore::generate_reference()` and write the table yourself.

```r
# Load the pipeline
pipeline <- ravepipeline::pipeline("reference_module")

# Subject and groups (one group with every LFP channel; 17 stays in it)
pipeline$set_settings(
  project_name = "test@bids:ds005953",  # RAVE project
  subject_code = "01",                  # subject
  reference_name = "[Blank profile]",   # or an existing profile to start from
  electrode_group = list(
    list(name = "CAR", electrodes = "1-20")
  ),
  changes = list()                      # no group applied yet
)
reference_group <- pipeline$run("reference_group")
subject <- pipeline$read("subject")

# Optional: find the CAR channels with CARLA (17 excluded from the candidates)
pipeline$set_settings(carla_params = list(
  epoch_name = "status_good",   # epoch to use
  time_window = c(0.01, 1),     # seconds after onset
  electrodes = "1-16,18-20",    # candidate channels
  n_bootstrap = 100,
  virtual_reference = TRUE,     # "Modified CARLA"
  sensitive = TRUE,
  min_size = NULL,              # automatic
  absolute_rank = TRUE          # "Absolute data"
))
carla_fit <- pipeline$run("carla_fit")

# Create the reference signal file ref_<channels>.h5
ravecore::generate_reference(subject = subject$subject_id,
                             electrodes = carla_fit$channels)
ref_name <- sprintf("ref_%s", dipsaus::deparse_svec(carla_fit$channels))

# Apply the group: every channel (17 included) references the average
pipeline$set_settings(changes = list(list(
  group_name = "CAR",
  electrodes = "1-20",
  reference_type = "Common Average Reference",
  reference_signal = ref_name
)))
reference_updated <- pipeline$run("reference_updated")

# Save the profile (what "Generate & save" does)
reference_updated <- reference_updated[order(reference_updated$Electrode), ]
utils::write.csv(reference_updated,
                 file.path(subject$meta_path, "reference_car.csv"),
                 row.names = FALSE)
```

For bipolar, `reference_signal` holds one entry per channel, in the order the
group's channels appear in `reference_group`. For example:

```r
channels <- reference_group$Electrode[reference_group$Group == "LA"]
reference_signal <- c(sprintf("ref_%d", channels[-1]), "noref")  # last: noref
```

## Drive the module with MCP tools

Operate the live module the same way a user would, following the step-by-step
guide. Each step names an MCP tool; call it as `tool("tool__NAME", arg = value)`.

**Talk to the user first (interactive mode).**
* Before `update_electrode_group`, show the user the groups you are about to set
  (names and channels) and ask them to confirm. Users often want groupings other
  than the defaults.
* Before building a reference, pause and ask which channels are bad (to exclude
  from the reference). The module works with channel numbers; if the user gives
  electrode labels, ask for the numbers.
* Ask before `save_reference` overwrites an existing name.
* Skip these questions only when the request already answers them, or the user
  asks for a fully automated run.

**Excluded channels are left out of the reference, not removed.**
* Common average: leave them out of `reference_channels_new`, e.g. `1-16,18-20`
  with `17` excluded. Keep them in the groups: they are referenced too.
* CARLA: put the candidates (all LFP channels but the excluded ones) in
  `reference_channels_new` before running `estimate_carla`.
* Bipolar: move them into a group of their own (e.g. `Excluded`) and apply it with
  `No Reference`; the other groups' chains then skip them.

**Run the scripts in this order:** `load_data` → `update_electrode_group` → then
for each group: (`estimate_carla` →) (`generate_reference` →)
`update_group_reference` → and finally `save_reference`.
* `update_electrode_group` resets every group, so run it once, before configuring
  the groups.
* `update_group_reference` applies only the group chosen in `group_name`: run it
  once per group.

**Inputs round-trip through the browser.**
* After `shiny_input_update`, confirm the new value with
  `tool("tool__shiny_input_info", inputIds = list("<id>"))`: check `exists` and
  `current_value` before running a script.
* Choosing a group in `group_name` resets `reference_type` to that group's
  current type. Set the type only after `group_name` shows the new group.
* Some inputs only exist in certain states:
  * `reference_channels`: while the type is common average or white-matter.
  * `reference_channels_new`: while `reference_channels` is `[new reference]`.
  * `preview_save_name`: while the output tab `Preview & Export` is active.

**Leave the dialog buttons to people.** `reference_channels_btn` (opens the CARLA
or confirmation dialog) and `bipolar_btn` (the table editor) are for people; use
the scripts instead.

| Input ID | Value |
|---|---|
| `loader_project_name` | project, e.g. `test@bids:ds005953` |
| `loader_subject_code` | subject code, e.g. `01` |
| `loader_reference_name` | `[Blank profile]`, or an existing profile name |
| `electrode_group` | JSON array, e.g. `[{"name":"LA","electrodes":"1-5"},{"name":"LB","electrodes":"6-10"}]` |
| `group_name` | a group name (as returned by `update_electrode_group`) |
| `reference_type` | `No Reference`, `Common Average Reference`, `White-matter Reference`, or `Bipolar Reference` |
| `reference_channels` | an existing `ref_*` signal, or `[new reference]` |
| `reference_channels_new` | channels to average, or CARLA candidates, e.g. `1-16,18-20` |
| `reference_output_tabset` | `Preview & Export` (to save) |
| `preview_save_name` | profile name: letters, digits, underscore |

| Script | What it does | Returns |
|---|---|---|
| `load_data` | Loads the subject chosen in the loader | `TRUE` |
| `update_electrode_group` | Applies `electrode_group`; resets every group | group names |
| `estimate_carla` | CARLA over the channels in `reference_channels_new` (all LFP channels if blank); writes the selected channels back into it | the channels |
| `generate_reference` | Creates the signal from `reference_channels_new`; selects it in `reference_channels` | `ref_*` name |
| `update_group_reference` | Applies the chosen group (type and signal) | the group's references |
| `save_reference` | Writes `[subject]/meta/reference_<preview_save_name>.csv` | name and create/overwrite |

### Load data

* Set the project: `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "test@bids:ds005953")`
* Set the subject (after the project; retry until `shiny_input_info` shows it): `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "01")`
* Start from a blank profile: `tool("tool__shiny_input_update", inputId = "loader_reference_name", value = "[Blank profile]")`
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`
* Wait until `tool("tool__module_interactive_script_list")` says "Data loaded",
  and `electrode_group` holds the default groups (it starts as one empty row).
  The union of those groups is every LFP channel.

### Configure and run

1. Groups: confirm them with the user, set them if they differ, then apply. For
   bipolar with `17` excluded from `LD` = `16-20`:
   * `tool("tool__shiny_input_update", inputId = "electrode_group", value = "[{\"name\":\"LD\",\"electrodes\":\"16,18-20\"},{\"name\":\"Excluded\",\"electrodes\":\"17\"}]")`
   * `tool("tool__module_interactive_script_run", name = "update_electrode_group")` — returns the group names.
2. For each group:
   * `tool("tool__shiny_input_update", inputId = "group_name", value = "LD")`, then check it.
   * `tool("tool__shiny_input_update", inputId = "reference_type", value = "<type>")`, then check it.
   * **Bipolar** or **No Reference** (e.g. the `Excluded` group): nothing else.
   * **Existing common average signal:** `tool("tool__shiny_input_update", inputId = "reference_channels", value = "ref_1-16,18-20")`
   * **New common average signal from given channels** (all but the excluded ones):
     * `tool("tool__shiny_input_update", inputId = "reference_channels", value = "[new reference]")`
     * `tool("tool__shiny_input_update", inputId = "reference_channels_new", value = "1-16,18-20")`
     * `tool("tool__module_interactive_script_run", name = "generate_reference")` — returns e.g. `ref_1-16,18-20`; wait until `reference_channels` shows it.
   * **New common average signal from CARLA:**
     * `tool("tool__shiny_input_update", inputId = "reference_channels", value = "[new reference]")`
     * `tool("tool__shiny_input_update", inputId = "reference_channels_new", value = "1-16,18-20")` (candidates)
     * `tool("tool__module_interactive_script_run", name = "estimate_carla")` — returns the selected channels (needs an epoch).
     * `tool("tool__module_interactive_script_run", name = "generate_reference")`
   * Apply the group: `tool("tool__module_interactive_script_run", name = "update_group_reference")` — returns the group's references.
     For other groups sharing the same signal, set `reference_channels` to that `ref_*` and apply.
3. Save:
   * `tool("tool__shiny_input_update", inputId = "reference_output_tabset", value = "Preview & Export")`
   * `tool("tool__shiny_input_update", inputId = "preview_save_name", value = "bipolar")`
   * `tool("tool__module_interactive_script_run", name = "save_reference")`

To make several references (e.g. a bipolar and a CAR one), repeat steps 1-3 for
each. There is no need to reload the subject in between, because step 1 resets
the groups.

### Inspect results

* Read the reference table (open the `Preview & Export` tab first): `tool("tool__shiny_query_ui", css_selector = "#reference_module-reference_table_preview", transform_image = FALSE)`
* Plots of the selected group: `tool("tool__shiny_output_result", outputId = "reference_plot_signals")`, `"reference_plot_electrode"`, and `"reference_plot_heatmap"`.
* The save destination line: `tool("tool__shiny_query_ui", css_selector = "#reference_module-preview_save_path")`

---

For implementation details (pipeline targets, settings schema), read the module
source with the `rave-module` skill: `main.Rmd`, `R/module_server.R`, and
`R/module_html.R`.
