#' This file runs right after `aa.R`. The goal of this script
#' is to generate a bunch of preset input components
#' and create a component container that collate all the components
NULL

# Create a component container
component_container <- ravedash::new_rave_shiny_component_container(
  module_id = module_id, pipeline_name = pipeline$pipeline_name,
  settings_file = "settings.yaml"
)


presets_condition_groups <- with(asNamespace("ravedash"), {
  function (id = "condition_groups", varname = "condition_groups",
            label = "Create Condition Contrast", pipeline_repository = "repository")
  {
    comp <- RAVEShinyComponent$new(id = id, varname = varname)
    comp$repository_name <- pipeline_repository
    get_repo <- function() {
      data_loaded <- isTRUE(shiny::isolate(watch_data_loaded()))
      has_repository <- comp$container$data[["@has"]](pipeline_repository)
      if (!data_loaded) {
        if (has_repository) {
          comp$container$data[["@remove"]](pipeline_repository)
        }
        return(NULL)
      }
      if (!has_repository) {
        logger("Trying to get repository object...", level = "trace")
        repository <- ravepipeline::pipeline_read(var_names = pipeline_repository,
                                                  pipe_dir = comp$container$pipeline_path)
        comp$container$data[[pipeline_repository]] <- repository
      }
      else {
        repository <- comp$container$data[[pipeline_repository]]
      }
      if (!inherits(repository, "rave_repository")) {
        return(NULL)
      }
      repository
    }
    get_subject <- function() {
      repo <- get_repo()
      if (inherits(repo, "rave_repository")) {
        subject <- repo$subject
        return(subject)
      }
      return(NULL)
    }
    get_default <- function(sub_id, missing = NULL, use_cache = TRUE,
                            constraint = NULL) {
      vname <- comp$get_sub_element_varnames(sub_id)
      subject <- get_subject()
      if (inherits(subject, "RAVESubject")) {
        missing <- subject$get_default(vname, default_if_missing = missing,
                                       simplify = TRUE)
      }
      comp$get_settings_value(use_cache = use_cache, default = missing,
                              key = vname, constraint = constraint)
    }
    comp$ui_func <- function(id, value, depends) {
      input_card(
        class_header = "shidashi-anchor",
        title = label,
        href = card_href(label, module_id = comp$container$module_id),
        dipsaus::compoundInput2(
          inputId = comp$get_sub_element_id(with_namespace = TRUE),
          label = "Group",
          initial_ncomp = 1L,
          min_ncomp = 1L,
          max_ncomp = 40L,
          label_color = gray_label_color,
          components = shiny::div(
            shiny::fluidRow(
              shiny::column(
                width = 12L,
                shiny::textInput(inputId = "group_name", label = "Name")
              ),
              shiny::column(
                width = 12L,
                shiny::selectInput(
                  inputId = "group_conditions",
                  label = NULL,
                  choices = "",
                  multiple = TRUE
                )
              ),
              shiny::column(
                width = 6L,
                shiny::selectInput(
                  inputId = "group_start_event",
                  label = "Start event",
                  choices = "Trial Onset",
                  multiple = FALSE
                )
              ),
              shiny::column(
                width = 6L,
                shiny::selectInput(
                  inputId = "group_finish_event",
                  label = "Finish event",
                  choices = "[Analysis end]",
                  multiple = FALSE
                )
              )
            )
          )
        )
      )
    }
    comp$server_func <- function(input, output, session) {
      reset <- function(...) {
        ravepipeline::logger("Reset {id}", level = "trace", use_glue = TRUE)
        repo <- get_repo()
        if (is.null(repo)) {
          return()
        }
        cond_cont <- table(repo$epoch$table$Condition)
        cond_cont <- cond_cont[order(names(cond_cont))]
        conditions <- names(cond_cont)
        default <- list(list(group_name = "All Conditions",
                             group_conditions = conditions))
        value <- get_default(sub_id = NULL, missing = NULL)
        if (!length(value) || !is.list(value) || !all(value$group_conditions %in%
                                                      conditions)) {
          value <- default
        }

        events <- repo$epoch$available_events
        events <- events[!events %in% ""]

        dipsaus::updateCompoundInput2(
          session = session,
          inputId = comp$id,
          initialization = list(
            group_conditions = list(choices = conditions),
            group_start_event = list(choices = c("Trial Onset", events)),
            group_finish_event = list(choices = c("[Analysis end]", events, "Trial Onset"))
          ),
          value = value,
          ncomp = length(value)
        )
      }
      initialize_with_new_data_reactive <- function() {
        shidashi::clear_notifications(class = "_presets_condition_groups_error_",
                                      session = session)
        repository <- get_repo()
        if (is.null(repository)) {
          shidashi::show_notification(title = "Initialization Error",
                                      message = c("Unable to initialize preset input `",
                                                  id, "`. The container repository has not been set up yet. ",
                                                  "This is a module error. Please contact the module author to ",
                                                  "fix this issue."), type = "warning", close = TRUE,
                                      autohide = FALSE, collapse = "", session = session,
                                      class = "_presets_condition_groups_error_")
          return()
        }
        reset()
      }
      comp$set_tool("reset", reset, server_needed = TRUE)
      comp$set_tool("initialize_with_new_data", function() {
        shiny::isolate(initialize_with_new_data_reactive())
      }, server_needed = TRUE)
      comp
    }
    comp
  }
})


# Define components
loader_project <- ravedash::presets_loader_project()
loader_subject <- ravedash::presets_loader_subject()
loader_epoch <- ravedash::presets_loader_epoch()
loader_electrodes <- ravedash::presets_loader_electrodes()
loader_reference <- ravedash::presets_loader_reference()
loader_viewer <- ravedash::presets_loader_3dviewer(height = "100%")

# import_export_pipeline <- ravedash::presets_import_export_subject_pipeline()
# electrode_selector <- ravedash::presets_analysis_electrode_selector2()
baseline_choices <- ravedash::presets_baseline_choices(
  baseline_choices = c("Demean"),
  baseline_along_choices = c("Per trial and electrode")
)
comp_condition_groups <- presets_condition_groups()
# comp_analysis_ranges <- ravedash::presets_analysis_ranges()

# Register the components
component_container$add_components(
  loader_project, loader_subject, loader_epoch,
  loader_electrodes, loader_reference, loader_viewer,
  # electrode_selector, import_export_pipeline,
  baseline_choices,
  comp_condition_groups
  # comp_analysis_ranges
)


