# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()) {

  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = "max-width:1444px;",
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


# Server functions for loader
loader_server <- function(input, output, session, ...) {

  # Runs when `ravedash::load_data_button()` is clicked, or through
  # `server_tools$trigger_script("load_data")` (e.g. from MCP tools)
  server_tools <- ravedash::get_default_handlers(session = session)
  server_tools$set_script(
    "load_data",
    description = c(
      "Load the voltage data of the subject, epoch, reference, and electrodes",
      "chosen in the loader (same as clicking 'Load subject'); only LFP",
      "electrodes are loaded. It reads `loader_project_name`,",
      "`loader_subject_code`, `loader_epoch_name` (with",
      "`loader_epoch_name__trial_starts`, `loader_epoch_name__trial_ends`, and",
      "their anchor events), `loader_reference_name`, and",
      "`loader_electrode_text`; saves them to the pipeline settings; and builds",
      "target `repository`. With 'Set as the default' checked (people only), it",
      "also saves the epoch or reference as the subject's default. Returns a",
      "summary: the trials, the loaded LFP electrodes, the sample rate, the",
      "conditions (choices of `group_conditions` in `condition_groups`), the",
      "events (choices of the groups' start and finish events), and the time",
      "window (bounds of `time_range`). A failed load returns the error; people",
      "then see 'Found an error while running script'."
    ),
    {
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code",
          "loader_electrode_text",
          "loader_epoch_name",
          "loader_reference_name"
        )
      )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      # Check if user has asked to set the epoch & reference to be the default
      default_epoch <- isTRUE(loader_epoch$get_sub_element_input("default"))
      default_reference <- isTRUE(loader_reference$get_sub_element_input("default"))

      # --------------------- Run the pipeline! ---------------------

      # Run the pipeline target `repository`
      pipeline$run(
        names = "repository",
        scheduler = "none",
        type = "smart",  # parallel
        # async = TRUE,
        callr_function = NULL
      )

      # Set epoch and/or reference as default
      if (default_epoch || default_reference) {
        repo <- pipeline$read("repository")
        if (default_epoch) {
          repo$subject$set_default("epoch_name", repo$epoch_name)
        }
        if (default_reference) {
          repo$subject$set_default("reference_name", repo$reference_name)
        }
      }

      ravepipeline::logger("Data has been loaded loaded")

      # Summary for agents: what was loaded, and the choices of the analysis
      # inputs. It must never make the load fail
      tryCatch({
        repo <- pipeline$read("repository")
        epoch_table <- repo$epoch$table
        conditions <- table(epoch_table$Condition)
        conditions <- conditions[order(names(conditions))]
        events <- repo$epoch$available_events
        events <- c("Trial Onset", events[!events %in% ""])
        time_window <- range(unlist(repo$time_windows))
        sprintf(
          paste(
            "Loaded %s: epoch %s (%d trials, %s to %s s), reference %s, LFP",
            "electrodes %s, sample rate %s Hz; conditions (trials): %s; events: %s"
          ),
          repo$subject$subject_id, repo$epoch_name, nrow(epoch_table),
          time_window[[1]], time_window[[2]], repo$reference_name,
          dipsaus::deparse_svec(repo$electrode_list),
          paste(unlist(repo$sample_rates$LFP), collapse = ", "),
          paste(sprintf("%s (%d)", names(conditions), conditions),
                collapse = ", "),
          paste(events, collapse = ", ")
        )
      }, error = function(e) {
        sprintf("Loaded %s/%s", settings$project_name, settings$subject_code)
      })
    },
    binding_event = "load_data",
    # Let the module know the data has been changed
    dispatch_event = "data_changed",
    alert_params = list(
      title = "Loading in progress",
      text = "Everything takes time. Some might need more patience than others."
    )
  )



}
