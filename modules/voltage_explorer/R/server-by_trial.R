# server handlers for by-trial card set
# separate due to large server file to maintain

server_expr_by_trial <- quote({


  # TODO: implement

  # output$figure_alpha_by_channel <- shidashi::renderPlot2({
  #   .output_ready()
  #
  #   crp_results <- pipeline$read(var_names = c("crp_df", "data_placeholder"))
  #   x <- extract_crp_param_per_trial_by_channel(crp_df, "al_p", data_placeholder)
  #
  #   shiny::validate(shiny::need(
  #     inherits(data_by_channel_condition, "data_by_channel_condition"),
  #     message = "No data available"
  #   ))
  #
  #
  #   time_range <- c(input$plot_time_start, input$plot_time_end)
  #   if (!length(time_range) || all(is.na(time_range))) {
  #     time_range <- c(NA, NA)
  #   }
  #   # For By Electrode: when mode is quantile, always use the full data range
  #   # (ignore the user-specified percentile); only apply space when mode is absolute
  #   plot_space <- get_plot_space()
  #   if (plot_space$space_mode == "quantile") {
  #     by_elec_space      <- 1
  #     by_elec_space_mode <- "quantile"
  #   } else {
  #     by_elec_space      <- plot_space$space
  #     by_elec_space_mode <- "absolute"
  #   }
  #   plot_by_channel_condition(
  #     data_by_channel_condition,
  #     group_by           = "condition",
  #     channel_annotation = get_channel_annotation_style(),
  #     cex                = get_cex(),
  #     vertical_marks     = input$plot_onset_mark %||% 0,
  #     time_range         = time_range,
  #     space              = by_elec_space,
  #     space_mode         = by_elec_space_mode,
  #     flip_y             = isTRUE(input$mean_erp_flip_y)
  #   )
  # })

})
