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
    time_range <- raveio::validate_time_window(unlist(analysis_range$time))

    list(
      # For ordering, plot, ...
      analysis_order = ii,

      # readable label
      label = label,

      # event to use
      event = event,
      time_range = time_range,
      time_shift = delta,
      shift_range = time_shift_range
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
