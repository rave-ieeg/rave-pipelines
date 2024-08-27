# Build a sample settings.yaml for debugging
# raveio::pipeline_setup_rmd("voltage_explorer")
# dput(as.list(raveio::load_yaml("./settings.yaml")))

setwd(file.path(rstudioapi::getActiveProject(), "modules/voltage_explorer"))
settings <- list(
  # Variables needed for repository
  project_name = "demo",
  subject_code = "DemoSubject",
  loaded_electrodes = "13-16,24",
  reference_name = "default",
  epoch_choice = "auditory_onset",
  epoch_choice__trial_starts = -1L,
  epoch_choice__trial_ends = 3L,

  # analysis_channels
  analysis_channels = "13-16",

  # Pre-baseline filters
  # list of filters, potentially one of
  #   "demean", "detrend", "decimate", "fir_kaiser", "firls", "fir_remez",
  #   "butter", "cheby1", "cheby2", "ellip", "fir", "iir"
  # stored as list `pre_baseline_filter_config`
  filter_configurations = list(
    list(
      type = "detrend"
    ),
    list(
      type = "baseline",
      windows = c(-1, -0.2)
    ),
    list(
      type = "decimate",
      by = 2
    ),
    list(
      type = "fir",
      # high_pass_freq = 70,
      low_pass_freq = 15
    ),
    list(
      type = "decimate",
      by = 4
    )
  ),

  # trial groups
  condition_groupings = list(
    `1` = list(
      label = "AOnly",
      conditions = c("drive_a",
                     "known_a", "last_a", "meant_a")
    ),
    `2` = list(
      label = "AV",
      conditions = c("last_av",
                     "drive_av", "known_av", "meant_av")
    )
  ),

  # Analysis settings (time)
  analysis_settings = list(`1` = list(
    label = "A",
    event = "Trial Onset",
    time = list(0L, 0.57),
    frequency = c(67L, 152L)
  ))

)

raveio::save_yaml(x = settings, file = "./settings.yaml")
