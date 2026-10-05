# Epoch Generator Module Reference

The Epoch Generator module (`epoch_generator`) finds trial onsets in a trigger
signal recorded with the experiment, such as a photodiode, an audio track, or a
TTL pulse. You threshold the signal of each recording block, stage the onsets it
finds, and save them as an epoch table of the subject
(`[subject]/rave/meta/epoch_<name>.csv`) that analysis modules load by its
"Epoch" name. The module runs no pipeline.

**Prerequisite:** Import the subject's signals first (the subject needs at least
one block). The trigger signal must be a file in the block's raw folder (MATLAB,
HDF5, EDF, or Blackrock `.nev`; Blackrock NSx channels, `.ns1`-`.ns6`, appear as
channels of the block's `.nev` file), or a channel imported as `Auxiliary`
(BrainVision and other formats are reachable only this way).

**Before you start: ask the user (one message).** An agent cannot finish an
epoch without these answers. Ask them all upfront, in one message (in the
dashboard chat, with `ask_user`). Once the user has named the project and
subject, you may load it and read the module's state while you wait; load a
trigger file only after the user has named it, and stage or save nothing
without the answers. Skip a question only when the user's request already
answers it: a module guess or an estimate is never an answer.

1. **Project and subject.** Never choose one yourself.
2. **Which block(s)** to epoch.
3. **The file or channel that marks the trials,** what kind it is (photodiode,
   audio, TTL...), and what an onset looks like (the photodiode turns dark or
   bright, the sound starts, the pulse rises). Without it you cannot generate an
   epoch. If the user is unsure, show them the files the module offers
   (`epoch_channel_info` lists them, the likely trigger files first) and let
   them choose: the file the module preselects is only a guess.
4. **The sample rate of that signal.**
   * MATLAB (`.mat`): **required**. The module fills in an estimate (see below);
     tell the user the value and ask them to confirm or correct it.
   * HDF5 (`.h5`): optional. Such files are often written at import with the
     rate built in, but the module reads no rate from them and shows the same
     estimate as for MATLAB. Tell the user the value and how it was obtained,
     and ask when unsure (for example, a custom file, or durations that do not
     match).
   * EDF, `.nev` (including NSx channels), and imported `[Channel N]`: the rate
     comes from the file. Tell the user the value.
   * Recording protocols are usually fixed within a project. If the user gave or
     corrected a rate before, or an earlier agent-generated epoch of the project
     records one (`epoch_channel_info` lists them), suggest it, and still
     confirm. No record does not mean no protocol: ask.
5. **The number of trials per block.** Ask whether they remember it, or whether
   they can share the stimulus or condition table (count its rows per block).
   If not, ask how long each trial's stimulus lasts and the rough pause between
   trials, in seconds.
6. **Which trial types the channel marks.** For example, audio-only trials do
   not change a photodiode, and visual-only trials make no sound.

Two more rules hold for the whole run:
* **Never move the threshold only to reach the expected number of trials.** A
  mismatch means asking the user, not tuning (see
  [Check the candidates](#check-the-candidates)).
* **Pause after staging each block,** and let the user check and edit the result
  before the next block or saving (see
  [Pause and let the user check](#pause-and-let-the-user-check)).

## Table of contents

* [Step-by-step guide](#step-by-step-guide)
* [Common procedures](#common-procedures)
* [Caveats](#caveats)
* [Drive the module with MCP tools](#drive-the-module-with-mcp-tools)

## Step-by-step guide

### 1. Load data

Step 1: In the loader screen, choose the RAVE **Project** and **Subject**. (Optional:
use "Sync ..." to copy them from a recently used module or from the Import
Signals module.)
Step 2: Click **"Load subject"**.

### 2. Analysis inputs

Work top to bottom on the left panel, one block at a time.

**"Load epoch channel" card**

* **Load existing epoch** — loads a saved epoch of the subject into the staged
  table, replacing what is staged in **every** block (conditions are dropped).
  Use it to review or edit an epoch. Default `New epoch...` (nothing loaded).
* **Choose a block to generate epoch** — the recording block. Default: the
  subject's first block. Changing it re-lists the files: a file of the same
  name stays selected; otherwise a guess is selected.
* **File containing epoch channel** — the file that holds the trigger signal.
  The list shows the files in the block's raw folder whose names contain `.h5`,
  `.mat`, `.edf`, `.pd`, `.aud`, or `.nev` (or, when that folder has none, the
  subject's whole raw folder), plus `[Channel N]` for each imported `Auxiliary`
  channel. The module preselects a guess.
* **"Load epoch file"** — reads the file and lists its variables (MATLAB/HDF5)
  or channels (EDF/`.nev`).
* **Variable name** — the variable or channel that holds the signal. Default: a
  guess (`DC1`... for EDF, the fastest channel for `.nev`, the first numeric
  vector or `analogTraces` for MATLAB/HDF5, `raw` for an imported channel). It
  must be a numeric signal, not a struct such as `analogInfos`.
* **Epoch channel sample rate** — in Hz.
  * EDF, `.nev`, and imported channels: filled in from the file.
  * MATLAB/HDF5: filled in with an estimate, *channel samples ÷ iEEG samples of
    the block × iEEG sample rate*, which is right only when the trigger file
    covers the same period as the iEEG recording. The estimate is filled in
    only while the field is empty or at most 1; otherwise the field keeps the
    previous file's value.
* **"Load epoch signal"** — loads the signal and plots it. The **Threshold**
  field (below) is cleared, so set a threshold for the new signal.

**"Transform & threshold" card**

* **Plot range** — half-height of the overview plot's y-axis; `0` (default)
  shows the whole range. Display only.
* **Difference plot** — use the sample-to-sample difference of the signal (finds
  sharp edges). Default off.
* **Absolute plot** — use the absolute value of the signal (for audio, which
  swings around zero). Default off.
* **Threshold direction** — `Above` (default): an onset is where the signal
  rises to the threshold or above; `Below`: where it falls to the threshold or
  below.
* **Minimum duration (s)** — crossings separated by shorter gaps merge into one
  onset, and new candidates within this many seconds of onsets already staged
  in the block are dropped. Default `0` (every crossing counts). Example: half
  the pause between trials.
* **Threshold** — the signal value that marks an onset, on the plotted
  (transformed) signal. Double-click the overview plot to fill it in (4
  significant digits), or type a number and press Enter. Empty means no
  candidates.

Thresholding runs again when the threshold, direction, minimum duration, or
sample rate changes, or after unstaging. It does **not** run again when
"Difference plot" or "Absolute plot" changes: set the threshold again.

**Buttons beside the tables**

* **Toggle selection** — inverts the selection of candidate rows: unselected
  rows become selected and selected ones unselected (all rows when none are
  selected). **Clear selection** — unselects them all.
* **Stage selected** — moves the selected candidates into the staged table,
  labelled with the selected block. It refuses candidates found in another
  block's signal.
* **Unstage selected** — removes the rows selected in the staged table.
* **Unstage all** — removes the staged onsets of the selected block only.
* **Discard epoch** — removes the staged onsets of every block.
* **Save & download** (download icon in the card header, or the link in the card
  footer) — opens the "Preview & export" dialog: check the table, type the
  **Epoch name to export** (default `default`), and click **"Save & Download"**.
  It writes `[subject]/rave/meta/epoch_<name>.csv` and downloads a copy. An
  existing file of that name is first renamed to
  `epoch_<name>_[YYYYmmdd_HHMMSS].csv`.

### 3. Outputs

* **Overview plot** (left) — the plotted signal over the whole block, in seconds.
  Blue line: the threshold. Red dashed lines: candidate onsets. Green lines:
  staged onsets of the selected block. Purple: the staged rows you selected.
  Drag across it to zoom into the detail plot.
* **Detail plot** (right) — the raw signal in the dragged time range.
* **Candidate table** (left table) — the onsets found (Order, Time in seconds,
  Diff = gap from the previous onset). The "Epoch threshold" notification
  reports the threshold, direction, minimum duration, and how many onsets were
  found and dropped.
* **Staged table** (right table) — the staged onsets of the selected block,
  sorted by time.
* **Saved epoch** — columns `Block`, `Time` (onset in seconds from the start of
  the block, 4 decimals), `Trial` (1 to N across blocks, in block order), and
  `Condition` (always `NoCondition`: this module sets no conditions).

## Common procedures

Each recipe fills in the "Transform & threshold" card for one kind of trigger
signal. Load the block's file and signal first (step-by-step guide).

### Procedure — Photodiode

Step 1: Keep "Absolute plot" and "Difference plot" off: a photodiode switches
between two levels.
Step 2: Set **Threshold direction** by the polarity: `Below` if the onset makes
the signal drop, `Above` if it rises. Whether a dark or a bright patch lowers
the signal depends on the hardware: check the plot, and ask the user when
unsure.
Step 3: Set **Minimum duration** shorter than the pause between trials (e.g.
`0.5`).
Step 4: Put the **Threshold** about midway between the two levels.

Example (demo subject `demo/YAB`, channel 130, block 010): Below, threshold
`-100`, minimum duration `0.5` finds 56 onsets. They are the trials with a video;
audio-only trials do not change the photodiode.

### Procedure — Audio track

Step 1: Turn on **Absolute plot**: sound swings around zero.
Step 2: Direction `Above`.
Step 3: Set **Minimum duration** longer than the silences inside a trial (between
words), and shorter than the pause between trials (e.g. `1`).
Step 4: Put the **Threshold** a few times above the background noise.

Example (`demo/YAB`, channel 129, block 008): Absolute, Above, threshold `10`,
minimum duration `1` finds 57 onsets at the sound onsets. Visual-only trials make
no sound and are missing; one onset is extra.

These examples only show the procedures: still ask the user which channel to
use and what it records, even for `demo/YAB`.

### Procedure — TTL pulse

Step 1: Raw signal; direction `Above` for a rising pulse (`Below` for a falling
one).
Step 2: Threshold midway between the low and high levels; a small **Minimum
duration** (shorter than the pause between pulses).

### Procedure — Use the trial count, or the trial duration and pause

Step 1: Run `threshold_scan` and choose a threshold inside a plateau (the same
count over a range of thresholds) whose onsets have regular gaps. Never choose a
threshold because its count equals the number the user expects.
Step 2: Compare that count with the expected number of trials *N* per block. With
the stimulus duration *D* and the pause *P* between trials instead of *N*: gaps
between onsets are about *D + P*; the block holds at most about *block duration
÷ (D + P)* trials (fewer with breaks); set **Minimum duration** to about *P / 2*
(shorter than *P*, longer than gaps inside a trial).
Step 3: If the count or the gaps do not fit, report the plateau counts and gaps
to the user and ask (see [Check the candidates](#check-the-candidates)).

### Procedure — Check and edit staged onsets (for people)

Step 1: Look at the overview plot: green lines are the staged onsets of the
selected block. Drag across the plot to zoom into the detail plot.
Step 2: To remove onsets, select their rows in the staged table (right; their
lines turn purple), then click **"Unstage selected"**.
Step 3: To redo the block, click **"Unstage all"**, change the direction,
threshold, or minimum duration, and stage again.

### Procedure — Review or edit an existing epoch

Step 1: Choose the epoch in **Load existing epoch** (this replaces anything
staged).
Step 2: For each block, select the block, then load its file and signal: its
staged onsets show in green over that signal, and in the staged table. (Green
lines are drawn over whatever signal is loaded, even another block's.) Edit as
above.
Step 3: Save it under a new name with "Save & Download" (people), or as a new
agent-generated epoch with `save_agent_epoch` (agents). Conditions are not kept.

## Caveats

* **Events per channel are not trials per block.** A photodiode marks only
  trials with a visual change and an audio track only trials with sound. In
  `demo/YAB`, the existing epoch `YABaOutlier` has 72, 72, 71, and 72 trials in
  blocks 008-012, but each channel marks about 56 per block. Tell the user; do
  not lower the threshold to find more.
* **Onsets differ between channels.** On `demo/YAB`, photodiode onsets come about
  55-375 ms before the onsets in `YABaOutlier` (the sound onsets, for trials
  with sound). Use the channel whose onset the analysis needs.
* **Minimum duration** merges crossings separated by shorter gaps. When it is
  above 0, it also drops new candidates within that many seconds of onsets
  already staged in the block. With `0`, re-thresholding finds staged onsets
  again, and staging them twice duplicates them.
* **The overview plot is downsampled** for long signals, so its peaks look lower
  than they are. The read-only scripts use the full-resolution signal.
* For signals of 20000 samples or fewer, the overview x-axis is in samples, not
  seconds, and dragging a range does not work.
* The MATLAB/HDF5 sample-rate estimate fills the field only while it is empty or
  at most 1, so after one file the next file keeps the previous rate. Check the
  field every time.
* Changing the block re-lists the files: a file of the same name stays
  selected, otherwise a guess is selected. Check the file, and select it again
  if needed.
* **Load existing epoch** replaces the staged onsets of every block and drops
  conditions.
* Saving under an existing epoch name keeps the old file only as a renamed
  backup.
* **Unstage all** affects only the selected block; **Discard epoch** affects all
  blocks.
* Epoch files are CSV tables. Agents must never edit them outside the module;
  if conditions or other columns are needed, tell the user.

## Drive the module with MCP tools

Operate the live module the same way a user would, following the step-by-step
guide above. Each step names an MCP tool; call it as `tool("tool__NAME", arg =
value)`. First ask the questions in
[Before you start](#epoch-generator-module-reference) at the top of this manual.

A script's reply has `result` (its return value: short `name=value` strings) and
`output` (what it printed while it ran). Known problems, such as no file loaded
or no rows selected, come back as the script's error with a message that says
what to change.

**Read-only scripts write nothing: run them without asking.** They are
`epoch_channel_info`, `threshold_scan`, `epoch_status`, and `candidate_times`.

**Inputs round-trip through the browser.** After `shiny_input_update`, confirm
the new value with `tool("tool__shiny_input_info", inputIds = list("<id>"))`
before running a script. Keep the module page visible: the tables do not draw in
a hidden page.

### Load data

* Set the project the user named:
  `tool("tool__shiny_input_update", inputId = "loader_project_name", value = "<project>")`
  (e.g. `"demo"`)
* Wait until the subject list is filled, then set the subject the user named:
  `tool("tool__shiny_input_update", inputId = "loader_subject_code", value = "<subject>")`
  (e.g. `"YAB"`)
* Load: `tool("tool__module_interactive_script_run", name = "load_data")`. Its
  result lists the blocks and the existing epochs. If it fails, an error dialog
  stays open until the user clicks Confirm: ask them to.

### Configure and run

Run these steps for each block the user named, in this order.

1. **Block and file.** Set `block` first (e.g. `value = "008"`), then set
   `epoch_file` to the file the user named and confirm it with
   `shiny_input_info` (send it again if it did not stick). `epoch_channel_info`
   lists the files offered for the block. Run `load_epoch_file`. Its output
   lists the variables, the selected one, and how to treat the sample rate of
   this file type.
2. **Variable and sample rate.** Check `varname` and `sample_rate` with
   `shiny_input_info`, and set `varname` if needed. Run `epoch_channel_info`:
   it shows the duration estimate, rate hints found in the file, and the rates
   of earlier agent-generated epochs; give these to the user when you ask them
   to confirm the rate. Set `sample_rate` to the rate the user confirmed (the
   estimate in the field is not a confirmation for MATLAB files). Run
   `load_epoch_signal`: its output compares the signal's duration with the
   iEEG recording of the block. Durations that differ point to a wrong sample
   rate or file: ask the user.
3. **Look at the signal.** Run `epoch_channel_info` again for the
   full-resolution quantiles of the plotted signal. You can also look at the
   overview plot: `tool("tool__shiny_output_result", outputId = "plot_overall")`.
4. **Transform and settings.** Set `plot_absolute` and `plot_difference`
   (`true`/`false`), `threshold_direction` (`"Above"`/`"Below"`), and
   `num_duration` (seconds), in that order, before the threshold.
5. **Scan.** Run `threshold_scan`: onset counts and gaps at 13 thresholds for the
   current settings.
6. **Threshold.** `threshold` is cleared after each new signal: wait until
   `shiny_input_info` shows it empty, then, in a later call, set it (e.g.
   `value = "10"`). To clear it, send `""` (never `null`). Poll `epoch_status`
   until it reports the new threshold without a `PENDING` warning. After
   changing `plot_absolute` or `plot_difference`, set the threshold again: send
   `""`, wait until it is empty, then send the number.
7. **Check the candidates** (next section), then stage them.

<a id="check-the-candidates"></a>
**Check the candidates.** `epoch_status` reports the count, the gaps (and the
unusual ones), and the times; `candidate_times` returns the times when there are
at most 100.
* If the count and gaps fit what the user told you, stage all candidates.
* If not, **do not tune the threshold to the count.** Tell the user what you
  found and ask. Common causes: reversed polarity (flip
  `threshold_direction`), a threshold too low (false trials between real ones),
  other events on the channel, or trial types this channel cannot mark.

**Stage all candidates.**
* Click `clear_selection` (`tool("tool__shiny_input_update", inputId = "clear_selection", value = "1")`)
  and poll `epoch_status` until it reports `selected=0`.
* Click `toggle_selection` the same way and poll until `selected` equals
  `candidates`; if it does not, clear and toggle again.
* Run `stage_selected`. It refuses when the selected block is not the block of
  the loaded signal, or when nothing is selected.

<a id="pause-and-let-the-user-check"></a>
**Pause and let the user check (after each block).**
* Report the block, file, threshold, direction, minimum duration, transform,
  count, gaps, and anything odd (an extra onset, a long gap).
* Ask the user to look at the overview plot (green lines are the staged onsets
  of the selected block) and give them the edit steps of
  [Check and edit staged onsets](#procedure--check-and-edit-staged-onsets-for-people):
  select rows in the staged table, then "Unstage selected"; or "Unstage all"
  and stage again with other settings. Only people select table rows.
* Continue with the next block, or save, only after the user agrees. Skip these
  pauses only when the user says so.
* Ask before running `unstage_all` (selected block) or `discard_epoch` (all
  blocks), and before setting `load_epoch`, which replaces the staged onsets of
  every block.

### Save

* Show the user the staged count per block (`epoch_status`) and tell them the
  epoch will be saved as `agent_generated_<yymmddTHHMMSS>`. The name marks it as
  generated by an agent.
* Only after they agree, run `save_agent_epoch`. It writes, in
  `[subject]/rave/meta/`:
  * `epoch_agent_generated_<time>.csv`: the epoch table, the same table as
    "Save & Download" writes (nothing is downloaded);
  * `epoch_agent_generated_<time>_description.txt`: a YAML description. For
    each block it lists every step in order: the onsets staged (with the file,
    variable, sample rate, transform, threshold, direction, minimum duration,
    and onset times), the onsets removed, and epochs loaded.
* Report the two file names from its result. The new epoch appears in "Load
  existing epoch" at once.

**Remember** for this project: the sample rate, the trigger file or channel, and
its polarity that the user gave or corrected. Suggest them for the next subject,
and still confirm.

### Inspect results

* `epoch_status`: threshold, candidates, selection, staged counts per block, the
  last staging steps, warnings (`PENDING`, `STALE`, block or file mismatch,
  stale table selection), and the next step.
* `candidate_times`: the candidate times (at most 100).
* `epoch_channel_info` and `threshold_scan`: see above.
* Pictures of the plots: `tool("tool__shiny_output_result", outputId = "plot_overall")`.
  `plot_subset` stays empty until a person drags a range on the overview plot.

### For people only

* Double-clicking and dragging on the plots: set `threshold` instead.
* Selecting rows in the candidate and staged tables: stage all candidates, and
  ask the user to remove single onsets.
* The "Preview & export" dialog and its "Save & Download": save with
  `save_agent_epoch` instead.
