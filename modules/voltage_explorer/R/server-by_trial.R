# server handlers for by-trial card set
# separate due to large server file to maintain

server_expr_by_trial <- function(input, output, session, ...) {

  output$figure_alpha_by_channel <- shidashi::renderPlot2({
    .output_ready()

    data_crp_param <- pipeline$read(var_names = "data_crp_param_alpha_prime")

    shiny::validate(shiny::need(
      inherits(data_crp_param, "data_crp_param_by_trial_channel"),
      message = "No data available"
    ))

    plot_space <- get_plot_space()
    plot_data_crp_param_by_trial_channel_heatmap(
      x                  = data_crp_param,
      electrode_mask     = get_electrode_mask(),
      channel_annotation = get_channel_annotation_style(),
      cex                = get_cex(),
      space              = plot_space$space,
      space_mode         = plot_space$space_mode
    )
  })

}

