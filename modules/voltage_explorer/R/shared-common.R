`%OF%` <- dipsaus::`%OF%`
KEY_MISSING <- ravepipeline::KEY_MISSING

DEFAULT_CEX <- 1.2
DEFAULT_PLOT_SPACE <- 99
DEFAULT_PLOT_SPACE_IS_PERCENTILE <- TRUE

OPTIONS_CHAN_ANNOT <- c("number", "short", "label", "full")
DEFAULT_CHAN_ANNOT <- "number"

OPTIONS_TRIAL_SORT <- c("stimuli", "trial")
DEFAULT_TRIAL_SORT <- "stimuli"

# Rendering shared by every by-electrode figure: stacked traces or an image
OPTIONS_BY_CHANNEL_PLOT_TYPE <- c("multiline", "heatmap")
DEFAULT_BY_CHANNEL_PLOT_TYPE <- "multiline"

OPTIONS_CRP_ONSET_BORDER <- c("disabled", "event_onset", "t_start", "earliest_possible")

# Plotmath labels for the `crp_df` columns that `prepare_data_crp_param_by_trial_channel`
# can extract. Names not listed here fall back to the column name itself.
CRP_PARAM_LABELS <- list(
  al = quote(alpha),
  al_p = quote(alpha * minute),
  snr = "SNR",
  expl_var = quote(R^2),
  tau_R = quote(tau[R]),
  tau_onset = quote(tau[onset])
)
DEFAULT_CRP_PARAMS <- list(
  time_step = 5,
  threshold_quantile = 98,
  onset_border = "disabled"
)


# ---- Preference: colormaps ---------------------------------------------------
pref_discrete_colormap <- ravepipeline::define_preference_discrete_colormap(
  pipeline = pipeline
)
use_discrete_colormap <- function(value = KEY_MISSING) {
  name <- pipeline$use_preference(
    name = pref_discrete_colormap$metadata$key,
    value = value,
    apply_getter = FALSE
  )
  list(
    name = name,
    colors = ravepipeline::DISCRETE_COLORMAPS(name)
  )
}

pref_continuous_colormap <- ravepipeline::define_preference_continuous_colormap(
  pipeline = pipeline
)

use_continuous_colormap <- function(value = KEY_MISSING) {
  name <- pipeline$use_preference(
    name = pref_continuous_colormap$metadata$key,
    value = value,
    apply_getter = FALSE
  )
  list(
    name = name,
    colors = ravepipeline::CONTINUOUS_COLORMAPS(name)
  )
}

# ---- Preference: cex ---------------------------------------------------------
pref_cex <- pipeline$define_preference(
  name = "cex",
  type = "numeric",
  domain = "graphics",
  default = DEFAULT_CEX,
  global = FALSE,
  validator = function(value) {
    if (isTRUE(value > 0)) {
      return(TRUE)
    }
    return("Graphics preference `cex` must be positive.")
  }
)

use_cex <- function(value = KEY_MISSING) {
  pipeline$use_preference(pref_cex$metadata$key, value = value)
}

# ---- Preference: Channel annotation style ------------------------------------
# pipeline <- ravepipeline::pipeline("voltage_explorer")
# ravepipeline::pipeline_setup_rmd("voltage_explorer")
pref_channel_annotation_style <- ravepipeline::define_preference_multichoice(
  pipeline = pipeline,
  name = "channel_annotation_style",
  choices = OPTIONS_CHAN_ANNOT,
  domain = "graphics",
  partial_match = TRUE
)

use_channel_annotation_style <- function(value = KEY_MISSING) {
  style <- pipeline$use_preference(name = pref_channel_annotation_style$metadata$key,
                                   value = value)
  attr(style, "preference_value") <- NULL
  style
}


# use_channel_annotation_style("short")
# ravepipeline:::construct_preference_validator(pref_channel_annotation_style$metadat)("assqwdwq", pipeline)

# ---- Preference: sort trial by number or condition ---------------------------
pref_trial_sort_by <- ravepipeline::define_preference_multichoice(
  pipeline = pipeline,
  name = "trial_sort_by",
  choices = OPTIONS_TRIAL_SORT,
  domain = "graphics",
  partial_match = TRUE
)

use_trial_sort_by <- function(value = KEY_MISSING) {
  trial_sort_by <- pipeline$use_preference(name = pref_trial_sort_by$metadata$key, value = value)
  attr(trial_sort_by, "preference_value") <- NULL
  trial_sort_by
}

# ---- Preference: by-electrode rendering --------------------------------------
pref_by_channel_plot_type <- ravepipeline::define_preference_multichoice(
  pipeline = pipeline,
  name = "by_channel_plot_type",
  choices = OPTIONS_BY_CHANNEL_PLOT_TYPE,
  default = DEFAULT_BY_CHANNEL_PLOT_TYPE,
  domain = "graphics",
  partial_match = TRUE
)

use_by_channel_plot_type <- function(value = KEY_MISSING) {
  plot_type <- pipeline$use_preference(name = pref_by_channel_plot_type$metadata$key,
                                       value = value)
  attr(plot_type, "preference_value") <- NULL
  plot_type
}

# ---- Preference: show CRP decoration -----------------------------------------
pref_show_crp_decoration <- ravepipeline::define_preference_logical(
  pipeline = pipeline,
  name = "show_crp_decoration",
  default = TRUE,
  domain = "graphics"
)

use_show_crp_decoration <- function(value = KEY_MISSING) {
  pipeline$use_preference(pref_show_crp_decoration$metadata$key, value = value)
}

# ---- Preference: spacing between vertical traces -----------------------------
pref_plot_space <- pipeline$define_preference(
  name = "plot_space",
  type = "numeric",
  domain = "graphics",
  default = DEFAULT_PLOT_SPACE,
  global = FALSE,
  getter = function(value) {
    if (value <= 0) {
      value <- 100
    }
    value
  }
)

pref_plot_space_is_percentile <- ravepipeline::define_preference_logical(
  pipeline = pipeline,
  name = "plot_space_is_percentile",
  default = DEFAULT_PLOT_SPACE_IS_PERCENTILE,
  domain = "graphics"
)

use_plot_space <- function(value = KEY_MISSING) {
  plot_space <- pipeline$use_preference(pref_plot_space$metadata$key, value = value)
  attr(plot_space, "preference_value") <- NULL
  plot_space
}

use_plot_space_is_percentile <- function(value = KEY_MISSING) {
  pipeline$use_preference(pref_plot_space_is_percentile$metadata$key, value = value)
}

# ---- CRP analysis preferences ------------------------------------------------
# time_step
pref_crp_params_time_step <- pipeline$define_preference(
  name = "crp_params_time_step",
  type = "numeric",
  domain = "analysis",
  default = DEFAULT_CRP_PARAMS$time_step,
  global = FALSE,
  getter = dipsaus::new_function2(
    args = alist(value = ),
    quote_type = "quote",
    body = bquote({
      value <- as.integer(value)
      if (!isTRUE(value >= 1)) { value <- .(DEFAULT_CRP_PARAMS$time_step) }
      value
    })
  )
)

use_crp_params_time_step <- function(value = KEY_MISSING) {
  time_step <- pipeline$use_preference(pref_crp_params_time_step$metadata$key, value = value)
  attr(time_step, "preference_value") <- NULL
  time_step
}


# threshold_quantile
pref_crp_params_threshold_quantile <- pipeline$define_preference(
  name = "crp_params_threshold_quantile",
  type = "numeric",
  domain = "analysis",
  default = DEFAULT_CRP_PARAMS$threshold_quantile,
  global = FALSE,
  getter = dipsaus::new_function2(
    args = alist(value = ),
    quote_type = "quote",
    body = bquote({
      value <- as.numeric(value)
      if (!isTRUE(value >= 1 && value <= 100)) {
        value <- .(DEFAULT_CRP_PARAMS$threshold_quantile)
      }
      value
    })
  )
)

use_crp_params_threshold_quantile <- function(value = KEY_MISSING) {
  threshold_quantile <- pipeline$use_preference(pref_crp_params_threshold_quantile$metadata$key, value = value)
  attr(threshold_quantile, "preference_value") <- NULL
  threshold_quantile
}

# Onset
pref_crp_params_onset_border <- ravepipeline::define_preference_multichoice(
  pipeline = pipeline,
  name = "crp_params_onset_border",
  choices = OPTIONS_CRP_ONSET_BORDER,
  default = DEFAULT_CRP_PARAMS$onset_border,
  domain = "analysis",
  partial_match = TRUE
)

use_crp_params_onset_border <- function(value = KEY_MISSING) {
  onset_border <- pipeline$use_preference(pref_crp_params_onset_border$metadata$key, value = value)
  attr(onset_border, "preference_value") <- NULL
  onset_border
}

use_crp_params <- function() {
  list(
    time_step = use_crp_params_time_step(),
    threshold_quantile = use_crp_params_threshold_quantile(),
    onset_border = use_crp_params_onset_border()
  )
}

# dummy
use_flipped_y <- function() { FALSE }

reset_analysis_preferences <- function() {
  pipeline$reset_preference(pref_crp_params_time_step$metadata$key)
  pipeline$reset_preference(pref_crp_params_threshold_quantile$metadata$key)
  pipeline$reset_preference(pref_crp_params_onset_border$metadata$key)
}

reset_graphics_preferences <- function() {
  pipeline$reset_preference(pref_discrete_colormap$metadata$key)
  pipeline$reset_preference(pref_cex$metadata$key)
  pipeline$reset_preference(pref_channel_annotation_style$metadata$key)
  pipeline$reset_preference(pref_trial_sort_by$metadata$key)
  pipeline$reset_preference(pref_by_channel_plot_type$metadata$key)
  pipeline$reset_preference(pref_show_crp_decoration$metadata$key)
  pipeline$reset_preference(pref_plot_space$metadata$key)
  pipeline$reset_preference(pref_plot_space_is_percentile$metadata$key)
}

