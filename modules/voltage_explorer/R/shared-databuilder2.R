build_parameter_grid <- function(repository, condition_groupings, analysis_settings) {
  `%?<-%` <- dipsaus::`%?<-%`
  epoch <- repository$epoch
  epoch_table <- epoch$table
  sample_rate <- repository$sample_rate
  max_time_range <- range(unlist(repository$time_windows), na.rm = TRUE)

  max_time_duration <- max_time_range[[2]] - max_time_range[[1]]

  # Clean `analysis_settings`
  analysis_settings <- unname(lapply(seq_along(analysis_settings), function(ii) {
    # ii <- 1
    # analysis_range <- analysis_settings[[ii]]

    analysis_range <- analysis_settings[[ii]]

    label <- analysis_range$label
    label %?<-% sprintf("A%s", ii)

    event <- paste(analysis_range$event, collapse = " ")
    if( identical(event, "Trial Onset") || !event %in% epoch$available_events ) {
      event <- ""
    }

    if(nzchar(event)) {
      cname <- sprintf("Event_%s", event)
      delta <- epoch_table[[cname]] - epoch_table$Time
    } else {
      delta <- 0
    }
    time_shift_delta <- delta
    time_shift_range <- range(c(0, delta), na.rm = TRUE)

    max_shift <- max(abs(time_shift_range))

    if( max_shift > max_time_duration ) {
      stop(sprintf("Trial event [%s] needs to load at least %.2f seconds of data. Please either load enough data (time duration) or change the analysis event type.", event, max_shift))
    }
    # TODO: need to check if time range is valid with shifts
    # time_range <- raveio::validate_time_window(unlist(analysis_range$time))

    channels <- dipsaus::parse_svec(analysis_range$channels, unique = TRUE)

    if(!length(channels)) {
      channels <- repository$electrode_list
    } else {
      channels <- channels[channels %in% repository$electrode_list]
    }

    if(!length(channels)) {
      stop("Analysis setting ", sQuote(label), " does not contain any valid channels. Please revise the channel for analysis.")
    }

    list(
      # For ordering, plot, ...
      analysis_order = ii,

      # readable label
      label = label,

      # event to use
      event = event,
      # time_range = time_range,
      time_shift = delta,
      shift_range = time_shift_range,

      channels = channels
    )
  }))

  condition_groups <- unname(lapply(seq_along(condition_groupings), function(group_order) {
    # group_order=1
    condition_group <- condition_groupings[[group_order]]
    label <- condition_group$label
    label %?<-% sprintf("Group%d", group_order)

    id <- gsub("[^a-z0-9]+", "_", tolower(label), ignore.case = TRUE)

    conditions <- sort(unique(condition_group$conditions))

    condition_type <- paste(condition_group$condition_type, collapse = " ")
    condition_type <- trimws(condition_type)

    cond_cname <- epoch$get_condition_colname(condition_type)

    condition_column <- epoch_table[[cond_cname]]

    conditions <- conditions[conditions %in% condition_column]
    if(!length(conditions)) {
      # no condition
      return(NULL)
    }

    sel <- condition_column %in% conditions
    trial_num <- epoch_table$Trial[sel]

    list(
      # color, ordering...
      group_order = group_order,

      # for statistical tests
      id = id,

      # readable labels
      label = label,

      condition_type = condition_type,
      conditions = conditions,

      trial_selection = which(sel),
      trial_numbers = trial_num,
      trial_condition = condition_column[sel]

    )
  }))
  condition_groups <- dipsaus::drop_nulls(condition_groups)
  if(!length(condition_groups)) {
    stop("No valid condition specified. Please check inputs for condition groups.")
  }

  structure(
    class = c("voltage_explorer_parameter_grid"),
    list(
      condition_groups = condition_groups,
      analysis_settings = analysis_settings,
      epoch_table = epoch_table
    )
  )
}

subset_filtered_array <- function(filtered_array, condition_group, analysis_setting) {

  epoch_table <- filtered_array$get_header("epoch_table")
  sample_rate <- filtered_array$get_header("sample_rate")
  channels <- analysis_setting$channels
  if(!length(channels)) {
    channels <- filtered_array$get_header("filtered_channels")
  }

  sel <- epoch_table$Trial %in% condition_group$trial_numbers

  delta <- NULL
  if(length(analysis_setting$event) && nzchar(analysis_setting$event)) {
    idx <- which(tolower(names(epoch_table)) %in% tolower(sprintf(c("Event_%s", "Event%s"), analysis_setting$event)))
    if(length(idx)) {
      delta <- round((epoch_table[[idx]] - epoch_table$Time) * sample_rate)
    } else {
      analysis_setting$event <- ""
    }
  }

  # time x trial
  subarray <- subset(
    filtered_array,
    Trial ~ sel,
    Electrode ~ Electrode %in% channels,
    drop = FALSE
  )
  dimnames(subarray) <- NULL

  if(length(delta)) {
    subarray <- ravetools::shift_array(subarray, along_margin = 1L, unit_margin = 2L, shift_amount = delta[sel])
  }
  subarray
}

iterate_condition_analysis <- function(parameter_grid, fun) {

  condition_groups <- dipsaus::drop_nulls(parameter_grid$condition_groups)
  analysis_settings <- dipsaus::drop_nulls(parameter_grid$analysis_settings)

  dtbl <- expand.grid(
    anls_ii = seq_along(analysis_settings),
    cond_jj = seq_along(condition_groups)
  )
  re <- lapply(seq_len(nrow(dtbl)), function(rr) {
    ii <- dtbl$anls_ii[[rr]]
    jj <- dtbl$cond_jj[[rr]]

    re <- fun(condition_groups[[jj]], analysis_settings[[ii]])

    list(
      condition_group = condition_group,
      analysis_setting = analysis_setting,
      result = re
    )
  })
  list(
    n_condition_groups = length(condition_groups),
    n_analysis_settings = length(analysis_settings),
    content = re
  )
}

