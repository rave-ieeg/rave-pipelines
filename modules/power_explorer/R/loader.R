
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = 'max-width:1444px;',
    input_ui = {
      # project & subject
      ravedash::input_card(
        title = "Data Selection",
        class_header = "",

        ravedash::flex_group_box(
          title = "Project and Subject",

          shidashi::flex_item(
            loader_project$ui_func()
          ),
          shidashi::flex_item(
            loader_subject$ui_func()
          )
        ),

        loader_epoch$ui_func(),

        ravedash::flex_group_box(
          title = "Electrodes and Reference",

          loader_reference$ui_func(),
          shidashi::flex_break(),
          shidashi::flex_item(
            loader_electrodes$ui_func()
          ),
          shidashi::flex_item(
            shiny::fileInput(
              inputId = ns("loader_mask_file"),
              label = "or Mask file"
            ))
        ),

        footer = shiny::tagList(
          ravedash::load_data_button(label = "Load subject", width = "100%")
        )

      )
    },
    output_ui = {
      ravedash::output_card(
        title = "3D Viewer",
        class_body = "no-padding min-height-650 height-650",
        loader_viewer$ui_func()
      )
    }
  )

}

loader_server <- function(input, output, session, ...){

  # list2env(list(session = session, input = input), envir=globalenv())

  # Add validator
  # session <- shiny::MockShinySession$new()
  # loader_project$server_func(input, output, session)
  # loader_subject$server_func(input, output, session)
  # loader_epoch$server_func(input, output, session)
  # loader_electrodes$server_func(input, output, session)
  # loader_reference$server_func(input, output, session)
  # loader_viewer$server_func(input, output, session)
  loader_validator_subject_code <- loader_subject$sv


  # Runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    description = c(
      "Load the power data of the subject, epoch, reference, and electrodes",
      "chosen in the loader (same as clicking 'Load subject'). It reads",
      "`loader_project_name`, `loader_subject_code`, `loader_epoch_name`",
      "(with `loader_epoch_name__trial_starts`, `loader_epoch_name__trial_ends`,",
      "and their anchor events), `loader_reference_name`, and",
      "`loader_electrode_text`; saves them to the pipeline settings; and",
      "builds target `repository`. With 'Set as the default' checked, it also",
      "saves the epoch or reference as the subject's default; with 'Load first",
      "block as single trial' checked, it writes",
      "`meta/epoch_single_trial_<epoch>.csv` into the subject. Returns a",
      "summary: the trials, the loaded electrodes, the conditions (choices of",
      "`first_condition_groupings`), the events (choices of the `event` of",
      "`ui_analysis_settings`), and the time and frequency ranges. A failed",
      "load returns the error; people then see 'Found an error while running",
      "script'."
    ),
    {
      # gather information
      settings <- dipsaus::fastmap2()

      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code",
          "loader_electrode_text",
          "loader_epoch_name",
          "loader_reference_name"
        ),
        map = settings
      )
      pipeline$set_settings(.list = settings)

      default_epoch <- isTRUE(loader_epoch$get_sub_element_input("default"))
      default_reference <- isTRUE(loader_reference$get_sub_element_input(
        "default"
      ))

      # Run the pipeline!
      pipeline$run(
        names = "repository",
        scheduler = "none",
        type = "smart", # parallel
        # async = TRUE,
        callr_function = NULL,
        return_values = FALSE
      )
      if (default_epoch || default_reference) {
        repo <- pipeline$read("repository")
        if (default_epoch) {
          repo$subject$set_default("epoch_name", repo$epoch_name)
        }
        if (default_reference) {
          repo$subject$set_default("reference_name", repo$reference_name)
        }
      }

      # Summary for agents: what was loaded, and the choices of the analysis
      # inputs. It must never make the load fail, and it avoids
      # `repo$power`, which would load the power data here
      tryCatch({
        repo <- pipeline$read("repository")
        epoch_table <- repo$epoch$table
        conditions <- table(epoch_table$Condition)
        conditions <- conditions[order(names(conditions))]
        condition_columns <- names(epoch_table)[
          grepl("Condition", names(epoch_table), fixed = TRUE)
        ]
        sprintf(
          paste(
            "Loaded %s: epoch %s (%d trials, %s to %s s), reference %s,",
            "electrodes %s; conditions in column Condition (trials): %s;",
            "condition columns: %s; events: %s; frequencies %s-%s Hz (%d)"
          ),
          repo$subject$subject_id, repo$epoch_name, nrow(epoch_table),
          min(repo$time_points), max(repo$time_points), repo$reference_name,
          dipsaus::deparse_svec(repo$electrode_list),
          paste(sprintf("%s (%d)", names(conditions), conditions),
                collapse = ", "),
          paste(condition_columns, collapse = ", "),
          paste(get_available_events(columns = repo$epoch$columns),
                collapse = ", "),
          min(repo$frequency), max(repo$frequency), length(repo$frequency)
        )
      }, error = function(e) {
        sprintf("Loaded %s/%s", settings$project_name, settings$subject_code)
      })
    },
    binding_event = "load_data",
    dispatch_event = "data_changed",
    alert_params = list(
      title = "Loading in progress",
      text = "Everything takes time. Some might need more patience than others."
    )
  )

}
