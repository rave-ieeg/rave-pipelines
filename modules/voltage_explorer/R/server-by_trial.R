# server handlers for by-trial card set
# separate due to large server file to maintain

server_expr_by_trial <- function(input, output, session, ...) {

  # The three `data_crp_param_*` targets share one class and carry their own
  # parameter name, so the only thing that differs between the three figures is
  # which target to read.
  #
  # `register_output()` substitutes its `expr` and `eval()`s it in the caller's
  # frame, and `renderPlot2()` captures the expression against `env`. Passing
  # `env = environment()` pins that to this call's frame -- the one holding
  # `target_name` -- rather than leaving it to `parent.frame()` to resolve
  # through the intervening `eval()`. Each call gets its own frame, so the three
  # figures do not share a `target_name`; the frame is also what the download
  # handler re-evaluates against, so it lives as long as the output does.
  register_crp_param_output <- function(output_id, target_name, label) {
    force(target_name)
    shidashi::register_output(
      outputId = output_id,
      description = sprintf(
        "CRP parameter (%s) as a trial by channel heatmap, one panel per condition group.",
        label
      ),
      download_type = "image",
      session = session,
      expr = shidashi::renderPlot2({
        .output_ready()

        data_crp_param <- pipeline$read(var_names = target_name)

        shiny::validate(shiny::need(
          inherits(data_crp_param, "data_crp_param_by_trial_channel"),
          message = "No data available"
        ))

        plot_space <- get_plot_space()
        plot_data_crp_param_by_trial_channel_heatmap(
          x                  = data_crp_param,
          electrode_mask     = get_electrode_mask(),
          sort_by            = get_trial_sort_by(),
          channel_annotation = get_channel_annotation_style(),
          cex                = get_cex(),
          space              = plot_space$space,
          space_mode         = plot_space$space_mode,
          col                = get_colormaps()$continuous
        )
      }, env = environment())
    )
  }

  register_crp_param_output(
    output_id = "figure_data_crp_param_alpha_prime",
    target_name = "data_crp_param_alpha_prime",
    label = "alpha prime"
  )

  register_crp_param_output(
    output_id = "figure_data_crp_param_snr",
    target_name = "data_crp_param_snr",
    label = "SNR"
  )

  register_crp_param_output(
    output_id = "figure_data_crp_param_expl_var",
    target_name = "data_crp_param_expl_var",
    label = "R squared"
  )

}
