verify_loader_inputs <- function(
    subject, loaded_electrodes,
    epoch_choice, reference_name,
    epoch_choice__trial_starts,
    epoch_choice__trial_ends
) {
  # check electrodes
  electrodes_to_load <- dipsaus::parse_svec(loaded_electrodes)
  lfp_electrodes <- subject$electrodes[subject$electrode_types == "LFP"]
  electrodes_to_load <- electrodes_to_load[electrodes_to_load %in% lfp_electrodes]
  if( !length(electrodes_to_load) ) {
    stop("There is no valid phase data to load for the selected electrode channels.")
  }

  # Check if notch & wavelet has been applied on all electrodes
  is_loading <- lfp_electrodes %in% electrodes_to_load
  if(!all(subject$notch_filtered[ is_loading ])) {
    stop("Notch filter has not been applied to these electrode channels. Please finish wavelet first.")
  }
  if(!all(subject$has_wavelet[ is_loading ])) {
    stop("Wavelet has not been applied to these electrode channels. Please finish wavelet first.")
  }

  # check epoch existense
  if( !isTRUE(epoch_choice %in% subject$epoch_names) ) {
    stop("Invalid epoch table name")
  }

  # check reference existense
  if( !identical(reference_name, "noref") &&
      !isTRUE(reference_name %in% subject$reference_names) ) {
    stop("Invalid reference table name")
  }

  loader_params <- list(
    electrodes = electrodes_to_load,
    epoch = epoch_choice,
    reference = reference_name,
    time_window = c(epoch_choice__trial_starts, epoch_choice__trial_ends)
  )

  loader_params
}
