# A gist file for phase analysis

# load shared functions
raveio::pipeline_setup_rmd(module_id = "phase_explorer")

extdata_path <- file.path(raveio::find_path("modules/phase_explorer/", root_dir = "."), "data")

# ------ Step 1: set up analysis input variables -------------------------------
# Use list2env just because it is easier to copy to main.rmd
list2env(list(
  project_name = "demo",
  subject_code = "YAB",
  loaded_electrodes = "13-16,24",
  epoch_choice = "YABaOutlier",
  epoch_choice__trial_starts = -1,
  epoch_choice__trial_ends = 2,
  reference_name = "noref",
  condition_groups = list(
    list(group_name='A', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
    list(group_name='B', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v'))
  ),
  erp_downsample_rate_to = 100,
  analyze_electrodes = "14-15",
  erp_analysiswindow = c(0, 1),
  erp_baseline = c(-1, 0),
  analysis_time = 0,
  analysis_frequency = 2,

  smooth_erp = 2
), envir=.GlobalEnv)



# ------ Step 2: RAVE load repositories -------------------------------

# Create subject instance
subject <- RAVESubject$new(
  project_name = project_name,
  subject_code = subject_code
)

# check loader inputs
loader_params <- verify_loader_inputs(
  subject = subject,
  loaded_electrodes = loaded_electrodes,
  epoch_choice = epoch_choice,
  reference_name = reference_name,
  epoch_choice__trial_starts = epoch_choice__trial_starts,
  epoch_choice__trial_ends = epoch_choice__trial_ends
)

# start to load phase and voltage

phase_repository <- raveio::prepare_subject_phase(
  subject = subject,
  electrodes = loader_params$electrodes,
  epoch_name = loader_params$epoch,
  reference_name = loader_params$reference,
  time_windows = loader_params$time_window
)

voltage_repository <- raveio::prepare_subject_voltage_with_epoch(
  subject = subject,
  electrodes = loader_params$electrodes,
  epoch_name = loader_params$epoch,
  reference_name = loader_params$reference,
  time_windows = loader_params$time_window
)


# ------ Step 3: validate and clean analysis data ------------------------------

# clean `analyze_electrodes`, make sure the electrodes are integers and loaded
# this code generates variable `requested_electrodes`
requested_electrodes <- dipsaus::parse_svec(analyze_electrodes)
requested_electrodes <- requested_electrodes[
  requested_electrodes %in% phase_repository$electrode_list]

# clean condition_groups
epoch_data <- voltage_repository$epoch_table
group_names <- sapply(seq_along(condition_groups), function(ii) {
  g <- condition_groups[[ii]]
  if(length(g$group_name) == 1 &&
     nzchar(g$group_name)) {
    g$group_name[[1]]
  } else {
    sprintf("Group%s", ii)
  }
})
group_has_trials <- sapply(condition_groups, function(g) {
  cond <- g$group_conditions[g$group_conditions %in% epoch_data$Condition]
  if( length(cond) ) { return(TRUE) } else { return(FALSE) }
})

duplicated_group_names <- group_names[group_has_trials]
duplicated_group_names <- duplicated_group_names[duplicated(duplicated_group_names)]
if(length(duplicated_group_names)) {
  stop("Condition group name must not be duplicated. Please rename group: ",
       paste(duplicated_group_names, collapse = ", "))
}

group_cleaned = dipsaus::drop_nulls(lapply(seq_along(condition_groups), function(idx){
  if( !group_has_trials[[idx]] ) { return() }
  g = condition_groups[[idx]]
  cond <- g$group_conditions[g$group_conditions %in% epoch_data$Condition]

  trial_selection <- epoch_data$Condition %in% unlist(g$group_conditions)
  trial_number <- epoch_data$Trial[ trial_selection ]

  list(
    name = g$group_name,
    group_index = idx,
    conditions = cond,
    trial_number = trial_number
  )
}))
if(!length(group_cleaned)) {
  stop("No valid condition group specified.")
}


# Collapse electrode phase data (inter-channel phase coherence)
inter_electrode_phase_coherence <- 0
lapply(
  phase_repository$phase$data_list[phase_repository$electrode_list %in% requested_electrodes],
  function(arr) {
    # load data into memory
    arr <- arr[reshape = dim(arr)[1:3], dimnames = FALSE]

    # calculate phase as complex representation
    inter_electrode_phase_coherence <<- inter_electrode_phase_coherence + exp(1i * arr)
    return()
  }
)
inter_electrode_phase_coherence <- inter_electrode_phase_coherence / length(requested_electrodes)


# Down-sample ERP voltage
voltage_dnames <- voltage_repository$voltage$dimnames
voltage_time <- voltage_dnames$Time
erp_dsrate <- voltage_repository$sample_rate / erp_downsample_rate_to
erp_timepoint_idx <- round(seq(from = 1L, to = length(voltage_time), by = erp_dsrate))

# Create a file array to store the voltage data (down-sampled)
filebase <- file.path(extdata_path, "erp_downsampled")

voltage_subdim <- voltage_repository$voltage$dim
voltage_subdim[[1]] <- length(erp_timepoint_idx)
voltage_sub <- filearray::filearray_load_or_create(
  filebase = filebase,
  mode = "readonly",
  type = "float",
  dimension = voltage_subdim,
  partition_size = 1L,
  symlink_ok = FALSE,
  initialize = FALSE,

  # header signatures, if all of them match, then array is cached
  signal_data_type = "voltage, epoched",
  repository_signature = voltage_repository$voltage$signature,
  erp_timepoint_idx = erp_timepoint_idx,

  on_missing = function( arr ) {
    dipsaus::lapply_async2(
      voltage_repository$voltage$data_list,
      function(x) {
        # x <- voltage_repository$voltage$data_list[[1]]
        subarray <- x[erp_timepoint_idx, , , drop = FALSE]
        idx <- which(voltage_dnames$Electrode == dimnames(x)$Electrode)
        arr[,,idx] <- subarray
        return()
      },
      plan = FALSE
    )
    voltage_dnames$Time <- voltage_dnames$Time[erp_timepoint_idx]
    dimnames(arr) <- voltage_dnames
  }
)

# erp_analysiswindow = c(0, 1)
# erp_baseline = c(-1, 0)
analysis_voltage <- subset(
  voltage_sub,
  Time ~ Time %within% erp_analysiswindow,
  Electrode ~ Electrode %in% requested_electrodes
)

baseline_voltage <- subset(
  voltage_sub,
  Time ~ Time %within% erp_baseline,
  Electrode ~ Electrode %in% requested_electrodes
)

# ------ Step 4: generate ITPC plot --------------------------------------------
# Ideally `itpc_plot_data` is self-contained (plotting function can rely on less
# this object)
plot_data_itpc_plot_heatmap <- generate_plot_data_itpc_plot_heatmap(
  phase_repository = phase_repository,
  requested_electrodes = requested_electrodes,
  group_cleaned = group_cleaned,
  inter_electrode_phase_coherence = inter_electrode_phase_coherence
)

itpc_plot_heatmap(
  plot_data_itpc_plot_heatmap,
  analysis_time = analysis_time,
  analysis_frequency = analysis_frequency,
  zmax = 0,
  useRaster = TRUE
)


# ------ Step 5: ERP vs Phase Reset plot ---------------------------------------
plot_data_erp_over_time <- list(
  time = dimnames(voltage_sub)$Time,
  electrodes = requested_electrodes,
  ngroups = length(group_cleaned),

  group_names = sapply(group_cleaned, "[[", "name"),
  group_indices = sapply(group_cleaned, "[[", "group_index"),

  group_data = lapply(group_cleaned, function(g) {
    all_data <- subset(
      voltage_sub,
      Trial ~ Trial %in% g$trial_number,
      Electrode ~ Electrode %in% requested_electrodes,
      drop = FALSE
    )
    g$voltage_range <- range(all_data)

    # Trial x Time
    all_data <- ravetools::collapse(all_data, keep = c(2L, 1L), average = TRUE)

    g$mean_se <- apply(all_data, 2, dipsaus::mean_se)

    g$mean_se_range <- range(plus_minus(g$mean_se[1,], g$mean_se[2,]))

    g$N <- nrow(all_data)

    return( g )
  })
)
plot_data_erp_over_time$erp_mean_se_range <- range(
  unlist(
    lapply(plot_data_erp_over_time$group_data,
           "[[", "mean_se_range")
  ),
  na.rm = TRUE
)


# TODO: I don't think this is the final form according to the screenshot sent to me
itpc_erp_time_plot(
  plot_data_erp_over_time = plot_data_erp_over_time,
  plot_data_itpc_plot_heatmap = plot_data_itpc_plot_heatmap,
  merge_plots = FALSE,
  # erp_baseline = c(-1,0),
  # erp_analysiswindow = c(0,1),
  # erp_window_names = c("Baseline", "Analysis"),
  phase_frequency = 2,
  smooth_erp = 2,
  index_time_range = c(0, 1.25)
)

