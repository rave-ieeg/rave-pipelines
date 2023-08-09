
# pe_graphics_settings_cache <- dipsaus::rds_map(path = file.path(ravedash:::temp_dir(persist = "app-session"),
#                                                                 "graphics_settings"))
# default_pegs <- list(
#     rave_cex.main = 1.5,
#     rave_cex.axis = 1.3,
#     # putting this to 1.4 because 1.5 causes some clipping of the axis(2) label, we could also try to increase
#     # the left margin to compensate
#     rave_cex.lab = 1.4,
#     rave_axis_tcl = -0.3,
#     plot_time_range = c(-Inf,Inf),
#     draw_decorator_labels = FALSE,
#     plot_title_options = c('Subject ID', 'Electrode #', 'Condition', 'Frequency Range',
#                            'Sample Size', 'Baseline Window', 'Analysis Window'),
#
#     background_plot_color_hint = 'white',
#
#     color_palette = 'Beautiful Field',
#     invert_colors_in_palette = FALSE,
#     reverse_colors_in_palette = FALSE,
#
#     heatmap_color_palette = get_heatmap_palette(get_palette_names = TRUE)[1],
#     heatmap_number_color_values = 101,
#     invert_colors_in_heatmap_palette = FALSE,
#     reverse_colors_in_heatmap_palette = FALSE,
#
#     show_outliers_on_plots = TRUE,
#
#     log_scale = FALSE,
#     max_zlim = 0,
#     percentile_range = TRUE,
#     sort_trials_by_type = 'Trial Number'
# )
#
# nm <- names(default_pegs) [!pe_graphics_settings_cache$has(names(default_pegs))]
# if(length(nm)) {
#     pe_graphics_settings_cache$mset(.list = default_pegs[nm])
# }
#
#
