
# Export settings yaml to power explorer
ravepipeline::pipeline_export_wizard(
  pipeline_name = "power_explorer",
  fun = function(settings) {

    # settings <- yaml::read_yaml("./modules/power_clust/settings.yaml")
    # translate to
    # new_settings = yaml::read_yaml("./modules/power_explorer/settings.yaml")

    settings <- as.list(settings)

    new_settings <- list(
      project_name = settings$project_name,
      subject_code = settings$subject_code,
      loaded_electrodes = settings$loaded_electrodes,

      baseline_settings = list(
        window = list(range(unlist(settings$baseline__windows), na.rm = TRUE)),
        scope = settings$baseline__global_baseline_choice,
        unit_of_analysis = settings$baseline__unit_of_analysis
      ),

      analysis_settings = list(
        list(
          label = "TrialStart",
          event = "Trial Onset",
          time = as.list(range(unlist(settings$analysis_window), na.rm = TRUE)),
          frequency_dd = "Select one",
          frequency = range(settings$frequency_range, na.rm = TRUE)
        )
      ),

      reference_name = settings$reference_name,

      epoch_choice = settings$epoch_choice,
      epoch_choice__trial_starts = settings$epoch_choice__trial_starts,
      epoch_choice__trial_ends = settings$epoch_choice__trial_ends,
      epoch_choice__trial_starts_rel_to_event = settings$epoch_choice__trial_starts_rel_to_event,
      epoch_choice__trial_ends_rel_to_event = settings$epoch_choice__trial_ends_rel_to_event,

      first_condition_groupings = unname(lapply(settings$condition_groups, function(group) {
        list(label = group$group_name, conditions = group$group_conditions)
      }))
    )

    new_settings <- new_settings[!vapply(new_settings, is.null, FUN.VALUE = FALSE)]

    new_settings

  }
)

# Import settings yaml from power explorer
ravepipeline::pipeline_import_wizard(
  pipeline = "power_explorer",
  fun = function(settings) {
    # p1 <- ravepipeline::pipeline("power_explorer", paths = "modules", temporary = TRUE)
    # settings = p1$get_settings()

    analysis_settings <- settings$analysis_settings
    if (length(analysis_settings) > 0) {
      analysis_settings <- as.list(analysis_settings[[1]])
      analysis_window <- list(range(unlist(analysis_settings$time), na.rm = TRUE))
      frequency_range <- list(range(unlist(analysis_settings$frequency), na.rm = TRUE))
    } else {
      analysis_window <- NULL
      frequency_range <- NULL
    }
    condition_groups <- unname(lapply(settings$first_condition_groupings, function(group) {
      list(group_name = group$label, group_conditions = group$conditions)
    }))
    if (!length(condition_groups)) {
      condition_groups <- NULL
    }

    new_settings <- list(
      project_name = settings$project_name,
      subject_code = settings$subject_code,
      loaded_electrodes = settings$loaded_electrodes,

      baseline__windows = list(range(unlist(settings$baseline_settings$window), na.rm = TRUE)),
      baseline__global_baseline_choice = settings$baseline_settings$scope,
      baseline__unit_of_analysis = settings$baseline_settings$unit_of_analysis,

      analysis_window = analysis_window,
      frequency_range = frequency_range,

      reference_name = settings$reference_name,

      epoch_choice = settings$epoch_choice,
      epoch_choice__trial_starts = settings$epoch_choice__trial_starts,
      epoch_choice__trial_ends = settings$epoch_choice__trial_ends,
      epoch_choice__trial_starts_rel_to_event = settings$epoch_choice__trial_starts_rel_to_event,
      epoch_choice__trial_ends_rel_to_event = settings$epoch_choice__trial_ends_rel_to_event,

      condition_groups = condition_groups
    )

    new_settings <- new_settings[!vapply(new_settings, is.null, FUN.VALUE = FALSE)]

    new_settings
  }
)


# Self import: mainly sanity check
ravepipeline::pipeline_import_wizard(
  pipeline = "power_clust",
  fun = function(settings) {
    # p1 <- ravepipeline::pipeline("power_clust", paths = "modules", temporary = TRUE)
    # settings = ravepipeline:::list_to_fastmap2(p1$get_settings())

    extract_range <- function(name) {
      re <- settings[[name]]
      if (length(re) > 0) {
        re <- range(unlist(re), na.rm = TRUE)
      } else {
        re <- NULL
      }
      re
    }

    condition_groups <- unname(lapply(settings$condition_groups, function(group) {
      group <- as.list(group)
      re <- list(group_name = group$group_name,
                 group_conditions = group$group_conditions)
      re <- re[!vapply(re, is.null, FUN.VALUE = FALSE)]
      if (!length(re)) {
        re <- NULL
      }
      re
    }))
    condition_groups <- condition_groups[!vapply(condition_groups, is.null, FUN.VALUE = FALSE)]
    if (!length(condition_groups)) {
      condition_groups <- NULL
    }

    new_settings <- list(
      project_name = settings$project_name,
      subject_code = settings$subject_code,
      loaded_electrodes = settings$loaded_electrodes,

      reference_name = settings$reference_name,
      epoch_choice = settings$epoch_choice,
      epoch_choice__trial_starts = settings$epoch_choice__trial_starts,
      epoch_choice__trial_ends = settings$epoch_choice__trial_ends,
      epoch_choice__trial_starts_rel_to_event = settings$epoch_choice__trial_starts_rel_to_event,
      epoch_choice__trial_ends_rel_to_event = settings$epoch_choice__trial_ends_rel_to_event,

      baseline__windows = list(extract_range("baseline__windows")),
      baseline__global_baseline_choice = settings$baseline__global_baseline_choice,
      baseline__unit_of_analysis = settings$baseline__unit_of_analysis,

      analysis_window = list(extract_range("analysis_window")),
      frequency_range = extract_range("frequency_range"),
      zeta_threshold = settings$zeta_threshold,

      condition_groups = condition_groups
    )

    new_settings <- new_settings[!vapply(new_settings, is.null, FUN.VALUE = FALSE)]

    new_settings
  }
)

