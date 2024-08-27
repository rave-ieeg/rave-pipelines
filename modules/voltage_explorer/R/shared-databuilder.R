`%?<-%` <- dipsaus::`%?<-%`

build_analysis_ranges <- function(repository, analysis_settings) {
  epoch <- repository$epoch
  sample_rate <- repository$sample_rate
  unname(lapply(analysis_settings, function(analysis_range) {
    # analysis_range <- analysis_settings[[1]]

    range <- new_analysis_range(name = analysis_range$label)

    event <- paste(analysis_range$event, collapse = " ")
    if( identical(event, "Trial Onset") || !event %in% epoch$available_events ) {
      event <- ""
    }
    range$set_value("event", event)

    if(nzchar(event)) {
      cname <- sprintf("Event_%s", event)
      delta <- epoch$table[[cname]] - epoch$table$Time
    } else {
      delta <- 0
    }

    time_shift_range <- range(c(0, delta), na.rm = TRUE)

    range$set_value("max_time_range", unlist(repository$time_windows))
    range$set_value("time_range", unlist(analysis_range$time))
    range$set_value("time_shift_range", time_shift_range)
    range$set_value("sample_rate", sample_rate)
    range$set_value("filter_frequency", unlist(analysis_range$frequency))
    # range$get_value("max_analysis_time_range")
    range
  }))
}

build_condition_groups <- function(repository, condition_groupings) {

  epoch <- repository$epoch

  groups <- unname(lapply(seq_along(condition_groupings), function( group_order ) {
    # group_order=1
    condition_group <- condition_groupings[[group_order]]
    checkmate::assert_names(names(condition_group), must.include = c("label", "conditions"))

    group <- new_trial_group(epoch = epoch, name = condition_group$label, order = group_order)
    group$conditions <- condition_group$conditions

    group$validate()

    return(group)
  }))

  groups



}

build_parameter_grid <- function(condition_groups, analysis_ranges) {

  # if(missing(baselined_voltage)) {
  #   baselined_voltage <- repository$voltage$baselined
  # }
  collection <- new_param_grid(name = "Voltage explorer parameter grid")

  collection$set_value("condition_groups", condition_groups)
  collection$set_value("analysis_settings", analysis_ranges)

  collection

}


