if(getOption("rave.run.if_false", FALSE)){
  .module_id  <-  "power_explorer"
  if(interactive() && !dipsaus::shiny_is_running()){
    setwd(rstudioapi::getActiveProject())
    source('./modules/power_explorer/R/aa.R')
  }
}


### custom 3dViewer during data load screen
get_loader_3dviewer <- function (id = "loader_3d_viewer", height = "100%", loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code", loader_reference_id = "loader_reference_name",
  loader_electrodes_id = "loader_electrode_text", gadgets = c("standalone",
    "download"))
{
  comp <- ravedash:::RAVEShinyComponent$new(id = id)
  comp$depends <- c(loader_project_id, loader_subject_id, loader_electrodes_id,
    loader_reference_id)
  comp$no_save <- TRUE
  gadgets <- gadgets[gadgets %in% c("standalone", "download2")]
  comp$ui_func <- function(id, value, depends) {
    ravedash::output_gadget_container(threeBrain::threejsBrainOutput(outputId = id,
      height = height, reportSize = FALSE), gadgets = gadgets)
  }
  comp$server_func <- function(input, output, session) {
    tools <- ravedash::register_rave_session(session)
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)
    loader_electrodes <- comp$get_dependent_component(loader_electrodes_id)
    loader_reference <- comp$get_dependent_component(loader_reference_id)
    get_subject <- loader_subject$get_tool("get_subject")
    electrode_table <- shiny::bindEvent(shiny::reactive({
      if (!loader_subject$sv$is_valid()) {
        return()
      }
      subject <- get_subject()
      subject_code <- subject$subject_code
      project_name <- subject$project_name
      electrodes_text <- loader_electrodes$current_value
      reference_name <- loader_reference$current_value
      brain <- comp$container$get_cache("loader_subject_brain",
        default = NULL)
      if (!inherits(brain, "rave-brain") || !identical(brain$subject_code,
        subject_code)) {
        ravedash::logger("Re-generate loader's brain", level = "trace")
        brain <- raveio::rave_brain(subject, surfaces = "pial")
      }
      else {
        ravedash::logger("Using cached loader's brain", level = "trace")
      }
      comp$container$set_cache(key = "loader_subject_brain",
        value = brain, expire_after = 100)
      if (is.null(brain)) {
        return()
      }
      electrodes <- dipsaus::parse_svec(electrodes_text)
      all_electrodes <- subject$electrodes
      if (!length(all_electrodes)) {
        return(NULL)
      }
      valid_electrodes <- subject$valid_electrodes(reference_name = reference_name)
      val <- rep("Not Loading", length(all_electrodes))
      val[all_electrodes %in% electrodes] <- "Excluded"
      val[all_electrodes %in% electrodes & all_electrodes %in%
          valid_electrodes] <- "Loading"
      val <- factor(val, levels = c("Loading", "Excluded",
        "Not Loading"))
      tbl <- data.frame(Subject = subject$subject_code,
        Electrode = subject$electrodes, Value = val)
      tbl
    }), loader_project$current_value, loader_subject$current_value,
      loader_electrodes$current_value, loader_reference$current_value,
      ignoreNULL = TRUE, ignoreInit = TRUE)
    viewer <- shiny::bindEvent(shiny::bindCache(shiny::reactive({
      shiny::invalidateLater(500)
      brain <- comp$container$get_cache("loader_subject_brain",
        default = NULL)
      if (!inherits(brain, "rave-brain")) {
        return()
      }
      tbl <- electrode_table()
      if (is.data.frame(tbl) && nrow(tbl)) {
        brain$set_electrode_values(tbl)
      }
      theme <- shidashi::get_theme(tools$theme_event)
      ravedash::logger("Re-generate loader's viewer", level = "trace")
      wg <- brain$plot(volumes = FALSE, start_zoom = 1,
        atlases = FALSE, side_canvas = FALSE, control_display = FALSE,
        background = theme$background, palettes = list(Value = c("orange",
          "pink", "gray30")),
        controllers = list(`Background Color` = theme$background,
          `Show Time` = FALSE))
      wg
    }), shidashi::get_theme(tools$theme_event), electrode_table(),
      cache = "session"), shidashi::get_theme(tools$theme_event),
      electrode_table(), ignoreNULL = TRUE)
    ravedash::register_output(shiny::bindEvent(threeBrain::renderBrain({
      wg <- viewer()
      shiny::validate(shiny::need(!is.null(wg), message = ""))
      return(wg)
    }), viewer(), ignoreNULL = FALSE, ignoreInit = FALSE),
      outputId = "loader_3d_viewer", export_type = "3dviewer",
      session = session)
  }
  comp
}

build_electrode_selector <- function (id = "electrode_text", varname = "analysis_electrodes",
  label = "Select Electrodes", loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code", pipeline_repository = "repository",
  start_simple = FALSE, multiple = TRUE)
{
  comp <- ravedash:::RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$repository_name <- pipeline_repository
  category_str <- "category"
  category_choices_str <- "category_choices"
  reset_str <- "reset"
  selected_electrode_text_str <- "selected_electrode_text"
  # merge_hemisphere_str <- "merge_hemisphere_labels"
  download_str <- "download"
  btn_previous_str <- "previous"
  btn_next_str <- "next"
  css_class_optional <- ifelse(start_simple, "rave-optional soft-hidden",
    "rave-optional")
  comp$no_save <- c(reset_str, category_choices_str, selected_electrode_text_str,
    btn_previous_str, btn_next_str)
  get_repo <- function() {
    if (!comp$container$data[["@has"]](pipeline_repository)) {
      repository <- raveio::pipeline_read(var_names = pipeline_repository,
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
    ravedash::input_card(class_header = "shidashi-anchor",
      href = ravedash::card_href(label, type = "input", module_id = comp$container$module_id),
      title = shiny::tagList(label, " ", shiny::textOutput(comp$get_sub_element_id(selected_electrode_text_str,
        with_namespace = TRUE), inline = TRUE, shiny::tags$small)),
      shiny::selectInput(inputId = comp$get_sub_element_id(category_str,
        with_namespace = TRUE), label = "Electrode categories",
        choices = ""),
      shiny::selectInput(inputId = comp$get_sub_element_id(category_choices_str,
        with_namespace = TRUE), label = "Category levels (multi-select)", choices = "",
        multiple = TRUE)
      ,
      shiny::textInput(inputId = id, label = "Select electrode by number",
        value = "", placeholder = "E.g. 1-30,55-60,88")
      ,
      shiny::div(class = "form-group", shiny::actionLink(inputId = comp$get_sub_element_id(reset_str,
        with_namespace = TRUE), label = "Reset electrode selectors")),
      shiny::div(class = "form-group", shiny::downloadLink(outputId = comp$get_sub_element_id(download_str,
        with_namespace = TRUE), label = "Download copy of meta data for all electrodes"))
    )
  }
  comp$server_func <- function(input, output, session) {
    comp$ready_to_collect <- function() {
      shiny::isolate(isFALSE(ravedash::watch_loader_opened()))
    }
    reset <- function(...) {
      ravedash::logger("Updating {id}", level = "trace", use_glue = TRUE)
      repo <- get_repo()
      if (is.null(repo)) {
        return()
      }
      electrodes <- repo$electrode_list
      electrode_table <- repo$electrode_table
      electrode_table <- electrode_table[electrode_table$Electrode %in%
          electrodes, ]
      electrode_table_names <- names(electrode_table)
      electrode_text <- dipsaus::parse_svec(get_default(sub_id = NULL,
        missing = NULL))
      electrode_text <- electrode_text[electrode_text %in%
          repo$electrode_list]
      if (!length(electrode_text)) {
        electrode_text <- electrodes[[1]]
      }
      if (!multiple) {
        electrode_text <- electrode_text[[1]]
      }
      electrode_text <- dipsaus::deparse_svec(electrode_text)
      electrode_category_selector <- get_default(sub_id = category_str,
        missing = c("freesurferlabel", "FSLabel", comp$get_sub_element_input(category_str)),
        constraint = electrode_table_names)
      ravedash::logger("Updating `{id}__{category_str}`, value: {electrode_category_selector} {length(electrode_table_names)}",
        level = "trace", use_glue = TRUE)
      shiny::updateSelectInput(session = session, inputId = comp$get_sub_element_id(category_str,
        with_namespace = FALSE), choices = electrode_table_names,
        selected = electrode_category_selector)
      electrode_list_text <- dipsaus::deparse_svec(repo$electrode_list,
        collapse = ", ")
      ravedash::logger("Updating `{id}`, value: {electrode_text}, label: Select by number (current: {electrode_list_text})",
        level = "trace", use_glue = TRUE)
      if (multiple) {
        shiny::updateTextInput(session = session, inputId = comp$get_sub_element_id(with_namespace = FALSE),
          label = sprintf("Elec # (available: %s)",
            electrode_list_text), value = electrode_text)
      }
      else {
        shiny::updateSelectInput(session = session, inputId = comp$get_sub_element_id(with_namespace = FALSE),
          label = sprintf("Elec # (available: %s)",
            electrode_list_text), choices = as.character(repo$electrode_list),
          selected = electrode_text)
      }
    }
    dipsaus::sync_shiny_inputs(input, session, c(comp$get_sub_element_id(with_namespace = FALSE),
      comp$get_sub_element_id(with_namespace = FALSE, sub_id = category_choices_str)),
      uniform = list(function(electrode_text) {
        repository <- get_repo()
        if (is.null(repository)) {
          return(NULL)
        }
        electrodes <- dipsaus::parse_svec(electrode_text)
        electrodes <- electrodes[electrodes %in% repository$electrode_list]
        if (!length(electrodes)) {
          electrodes <- NULL
        }
        electrodes
      }, function(category_selected) {
        if (!length(category_selected)) {
          return(NULL)
        }
        repository <- get_repo()
        if (is.null(repository)) {
          return(NULL)
        }
        category_name <- comp$get_sub_element_input(category_str)
        current_electrodes <- dipsaus::parse_svec(comp$current_value)
        current_electrodes <- current_electrodes[current_electrodes %in%
            repository$electrode_list]
        if (!length(current_electrodes)) {
          current_electrodes <- NULL
        }
        if (length(category_name) && category_name %in%
            names(repository$electrode_table) && length(repository$electrode_table[[category_name]])) {
          choices <- repository$electrode_table[[category_name]]
          all_electrodes <- repository$electrode_table$Electrode
          expected_category <- choices[all_electrodes %in%
              current_electrodes]
          if (!setequal(expected_category, category_selected)) {
            electrodes <- all_electrodes[choices %in%
                category_selected & repository$electrode_table$isLoaded]
            return(electrodes)
          }
        }
        return(current_electrodes)
      }), updates = list(function(electrodes) {
        if (length(electrodes)) {
          if (multiple) {
            new_value <- dipsaus::deparse_svec(electrodes)
            if (!identical(new_value, comp$current_value)) {
              val <- dipsaus::deparse_svec(electrodes)
              ravedash::logger("Updating `{id}`, value: {val}",
                level = "trace", use_glue = TRUE)
              shiny::updateTextInput(session = session,
                inputId = id, value = val)
            }
          } else {
            val <- as.character(electrodes[[1]])
            ravedash::logger("Updating `{id}`, value: {val}", level = "trace",
              use_glue = TRUE)
            shiny::updateSelectInput(session = session,
              inputId = id, selected = val)
          }
        }
      }, function(electrodes) {
        if (length(electrodes)) {
          repository <- get_repo()
          if (is.null(repository)) {
            return(NULL)
          }
          input_str <- comp$get_sub_element_id(category_choices_str,
            with_namespace = FALSE)
          category_name <- comp$get_sub_element_input(category_str)
          if (length(category_name) && category_name %in%
              names(repository$electrode_table) && length(repository$electrode_table[[category_name]])) {
            choices <- repository$electrode_table[[category_name]]
            all_electrodes <- repository$electrode_table$Electrode
            expected_category <- choices[all_electrodes %in%
                electrodes]
            category_selected <- comp$get_sub_element_input(category_choices_str)
            if (!setequal(expected_category, category_selected)) {
              if (!length(expected_category)) {
                expected_category <- character(0L)
              }
              ravedash::logger("Updating `{id}__{category_choices_str}` ({length(expected_category)})",
                level = "trace", use_glue = TRUE)
              shiny::updateSelectInput(session = session,
                inputId = input_str, selected = expected_category)
            }
          }
        }
      }))
    initialize_with_new_data_reactive <- function() {
      shidashi::clear_notifications(class = "_presets_analysis_electrode_selector2_error_",
        session = session)
      repository <- get_repo()
      if (is.null(repository)) {
        shidashi::show_notification(title = "Initialization Error",
          message = c("Unable to initialize preset input `",
            id, "`. The container repository has not been set up yet. ",
            "This is a module error. Please contact the module author to ",
            "fix this issue."), type = "warning", close = TRUE,
          autohide = FALSE, collapse = "", session = session,
          class = "_presets_analysis_electrode_selector2_error_")
        return()
      }
      electrodes <- dipsaus::parse_svec(get_default(NULL,
        missing = ""))
      electrodes <- electrodes[electrodes %in% repository$electrode_list]
      if (!length(electrodes)) {
        electrodes <- repository$electrode_list[[1]]
      }
      category <- comp$get_sub_element_input(category_str)
      electrode_table_names <- names(repository$electrode_table)
      if (!length(category) || !isTRUE(category %in% electrode_table_names)) {
        category <- get_default(sub_id = category_str,
          missing = c("freesurferlabel", "FSLabel"),
          constraint = electrode_table_names)
        ravedash::logger("Updating `{id}__{category_str}`, value: {category} ({length(electrode_table_names)})",
          level = "trace", use_glue = TRUE)
        shiny::updateSelectInput(session = session, inputId = comp$get_sub_element_id(category_str,
          with_namespace = FALSE), choices = electrode_table_names,
          selected = category)
      }
      choices <- character(0L)
      if (length(category) && isTRUE(category %in% electrode_table_names)) {
        choices <- repository$electrode_table[[category]]
        if (!length(choices)) {
          choices <- character(0L)
        }
      }
      ravedash::logger("Updating choices of `{id}__{category_choices_str}` ({length(choices)})",
        level = "trace", use_glue = TRUE)
      shiny::updateSelectInput(session = session, inputId = comp$get_sub_element_id(sub_id = category_choices_str,
        with_namespace = FALSE), choices = unique(choices),
        selected = character(0L))
      if (length(electrodes) && !multiple) {
        electrodes <- electrodes[[1]]
      }
      v <- dipsaus::deparse_svec(electrodes)
      if (multiple && identical(v, comp$current_value)) {
        v <- sprintf("%s ", v)
      }
      electrode_list_text <- dipsaus::deparse_svec(repository$electrode_list,
        collapse = ", ")
      ravedash::logger("Updating `{id}`, value: {v}, label: Select electrode by number (currently loaded: {electrode_list_text})",
        level = "trace", use_glue = TRUE)
      if (multiple) {
        shiny::updateTextInput(session = session, inputId = id,
          label = sprintf("Elec # (available: %s)",
            electrode_list_text), value = v)
      }
      else {
        shiny::updateSelectInput(session = session, inputId = id,
          label = sprintf("Elec # (available: %s)",
            electrode_list_text), choices = as.character(repository$electrode_list),
          selected = as.character(v))
      }
    }
    shiny::bindEvent(observe({
      initialize_with_new_data_reactive()
    }), comp$get_sub_element_input(category_str), ignoreNULL = TRUE,
      ignoreInit = TRUE)
    shiny::bindEvent(observe({
      reset()
    }), comp$get_sub_element_input(reset_str), ignoreNULL = TRUE,
      ignoreInit = TRUE)
    output[[comp$get_sub_element_id(selected_electrode_text_str,
      with_namespace = FALSE)]] <- shiny::renderText({
        dipsaus::deparse_svec(dipsaus::parse_svec(comp$current_value))
      })
    shiny::bindEvent(observe({
      repository <- get_repo()
      if (is.null(repository)) {
        return(NULL)
      }
      if (!length(repository$electrode_list)) {
        return(NULL)
      }
      current_electrodes <- dipsaus::parse_svec(comp$current_value)
      if (length(current_electrodes) || any(current_electrodes %in%
          repository$electrode_list)) {
        elec <- current_electrodes[current_electrodes %in%
            repository$electrode_list]
        elec <- elec[[1]]
        idx <- which(repository$electrode_list == elec)
        if (idx == 1) {
          idx <- length(repository$electrode_list)
        }
        else {
          idx <- idx - 1
        }
        elec <- repository$electrode_list[[idx]]
      }
      else {
        elec <- repository$electrode_list[[1]]
      }
      shiny::updateSelectInput(session = session, inputId = id,
        selected = as.character(elec))
    }), comp$get_sub_element_input(btn_previous_str), ignoreNULL = TRUE,
      ignoreInit = TRUE)
    shiny::bindEvent(observe({
      repository <- get_repo()
      if (is.null(repository)) {
        return(NULL)
      }
      if (!length(repository$electrode_list)) {
        return(NULL)
      }
      current_electrodes <- dipsaus::parse_svec(comp$current_value)
      if (length(current_electrodes) || any(current_electrodes %in%
          repository$electrode_list)) {
        elec <- current_electrodes[current_electrodes %in%
            repository$electrode_list]
        elec <- elec[[1]]
        idx <- which(repository$electrode_list == elec)
        if (idx == length(repository$electrode_list)) {
          idx <- 1
        }
        else {
          idx <- idx + 1
        }
        elec <- repository$electrode_list[[idx]]
      }
      else {
        elec <- repository$electrode_list[[1]]
      }
      shiny::updateSelectInput(session = session, inputId = id,
        selected = as.character(elec))
    }), comp$get_sub_element_input(btn_next_str), ignoreNULL = TRUE,
      ignoreInit = TRUE)
    comp$set_tool("reset", reset, server_needed = TRUE)
    comp$set_tool("initialize_with_new_data", function() {
      shiny::isolate(initialize_with_new_data_reactive())
    }, server_needed = TRUE)
    comp
  }
  comp
}


build_epoch_loader <- function (id = "loader_epoch_name", varname = "epoch_choice",
  label = "Epoch and Trial Duration", loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code")
{
  comp <- ravedash:::RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$no_save <- "default"
  pre_varname <- comp$get_sub_element_varnames("trial_starts")
  post_varname <- comp$get_sub_element_varnames("trial_ends")
  comp$ui_func <- function(id, value, depends) {
    pre <- comp$get_settings_value(key = pre_varname, default = -1)
    post <- comp$get_settings_value(key = post_varname, default = 2)
    ravedash::flex_group_box(title = label,
      shidashi::flex_item(size = 2,
        shiny::selectInput(inputId = id, label = "Epoch name",
          choices = c(value, ""), selected = value, multiple = FALSE)
      ),
      shidashi::flex_break(),
      shidashi::flex_item(shiny::p("Pre must be negative if you need a pre-event baseline window."), size=2),
      shidashi::flex_break(),
      shidashi::flex_item(shiny::numericInput(inputId = comp$get_sub_element_id("trial_starts",
        with_namespace = TRUE), label = "Pre", min = -10, step = .1,
        value = pre)),
      shidashi::flex_item(shiny::numericInput(inputId = comp$get_sub_element_id("trial_ends",
        with_namespace = TRUE), label = "Post", min = 0, step = 0.1,
        value = post)),
      shidashi::flex_break(),
      shidashi::flex_item(shinyWidgets::prettyCheckbox(inputId = comp$get_sub_element_id("default",
        with_namespace = TRUE), label = "Set as the default",
        status = "success", shape = "square", animation = "smooth")
      )
    )
  }
  comp$server_func <- function(input, output, session) {
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)
    get_subject <- loader_subject$get_tool("get_subject")
    get_time_window <- function() {
      subject <- get_subject()
      if (inherits(subject, "RAVESubject")) {
        pre <- comp$get_settings_value(key = pre_varname,
          default = {
            subject$get_default(pre_varname, default_if_missing = -1)
          })
        post <- comp$get_settings_value(key = post_varname,
          default = {
            subject$get_default(post_varname, default_if_missing = 2)
          })
      }
      else {
        pre <- comp$get_settings_value(key = pre_varname,
          default = -1)
        post <- comp$get_settings_value(key = post_varname,
          default = 2)
      }
      raveio::validate_time_window(as.vector(rbind(pre,
        post)))
    }
    shiny::bindEvent(observe({
      open_loader <- ravedash::watch_loader_opened(session = session)
      if (!open_loader) {
        return()
      }
      if (!loader_subject$sv$is_valid()) {
        return()
      }
      subject <- get_subject()
      epoch_choices <- subject$epoch_names
      default_epochname <- subject$get_default(id)
      if (length(default_epochname)) {
        default_epochname <- default_epochname[[1]]
        shinyWidgets::updatePrettyCheckbox(session, inputId = comp$get_sub_element_id("default",
          FALSE), label = sprintf("Set as the default (current: %s)",
            default_epochname))
      }
      else {
        shinyWidgets::updatePrettyCheckbox(session, inputId = comp$get_sub_element_id("default",
          FALSE), label = "Set as the default")
      }
      epoch_name <- comp$get_settings_value(default = default_epochname,
        constraint = epoch_choices, use_cache = TRUE)
      shiny::updateSelectInput(session = session, inputId = id,
        choices = epoch_choices, selected = epoch_name)
    }), loader_project$current_value, loader_subject$current_value,
      ravedash::watch_loader_opened(session = session), ignoreNULL = TRUE)
  }
  comp
}


component_container <- ravedash:::RAVEShinyComponentContainer$new(
  module_id = module_id, pipeline_name = pipeline$pipeline_name
)


# Define components
loader_project <- ravedash::presets_loader_project()
loader_subject <- ravedash::presets_loader_subject()
loader_electrodes <- ravedash::presets_loader_electrodes()
loader_reference <- ravedash::presets_loader_reference()
loader_viewer <- get_loader_3dviewer()#ravedash::presets_loader_3dviewer(height = "100%")

# str(loader_viewer$get_settings_value)

import_export_pipeline <- ravedash::presets_import_export_subject_pipeline()

loader_epoch <- build_epoch_loader()#  ravedash::presets_loader_epoch()

# comp_condition_groups <- ravedash::presets_condition_groups()
electrode_selector <- build_electrode_selector()

component_container$add_components(
  loader_project, loader_subject, loader_epoch,
  loader_electrodes, loader_reference, loader_viewer,
  electrode_selector, import_export_pipeline
  # comp_condition_groups#,
)


if(getOption("rave.run.if_false", FALSE)){
  session <- shiny::MockShinySession$new()
  input <- session$input
  output <- session$output
}
