`%?<-%` <- dipsaus::`%?<-%`
`%OF%` <- dipsaus::`%OF%`

group_palette <- c("#FFA500", "#1874CD", "#006400", "#FF4500", "#A52A2A", "#7D26CD",
                   "#FE00FA", "#16FF32", "#FBE426", "#B00068", "#1CFFCE", "#90AD1C",
                   "#2ED9FF", "#DEA0FD", "#F8A19F", "#325A9B", "#C4451C", "#1C8356",
                   "#85660D", "#B10DA1", "#1CBE4F", "#F7E1A0", "#C075A6", "#AAF400",
                   "#BDCDFF", "#822E1C", "#B5EFB5", "#7ED7D1", "#1C7F93", "#3B00FB"
)
DEFAULT_CEX <- 1.2

OPTIONS_CHAN_ANNOT <- c("number", "short", "label", "full")
DEFAULT_CHAN_ANNOT <- "number"

OPTIONS_TRIAL_SORT <- c("stimuli", "trial")
DEFAULT_TRIAL_SORT <- "stimuli"

if (!pipeline$has_preferences("voltage_explorer.graphics.discrete_palette")) {
  pipeline$set_preferences("voltage_explorer.graphics.discrete_palette" = list(
    name = "Default",
    colors = group_palette
  ))
}

use_discrete_palette <- function(name, colors) {
  if (!missing(name)) {

    if (missing(colors) || length(colors) == 0) {
      name <- "Default"
      colors <- group_palette
    }
    pal <- list(
      name = name,
      colors = colors
    )
    pipeline$set_preferences("voltage_explorer.graphics.discrete_palette" = pal)
  } else {

    pal <- pipeline$get_preferences("voltage_explorer.graphics.discrete_palette")
    if (!is.list(pal) || !all(c("name", "colors") %in% pal)) {
      pal <- list(
        name = "Default",
        colors = group_palette
      )
      pipeline$set_preferences("voltage_explorer.graphics.discrete_palette" = pal)
    }
  }
  pal
}


use_cex <- function(cex) {
  if (!missing(cex)) {
    if (!isTRUE(cex > 0)) {
      cex <- DEFAULT_CEX
    }
    pipeline$set_preferences("voltage_explorer.graphics.cex" = cex)
  } else {
    cex <- pipeline$get_preferences("voltage_explorer.graphics.cex", modes = "numeric", ifnotfound = DEFAULT_CEX)
    if (!isTRUE(cex > 0)) {
      cex <- DEFAULT_CEX
      pipeline$set_preferences("voltage_explorer.graphics.cex" = cex)
    }
  }
  cex
}

use_channel_annotation_style <- function(style = OPTIONS_CHAN_ANNOT) {
  if (!missing(style)) {
    style <- match.arg(style)
    pipeline$set_preferences("voltage_explorer.graphics.channel_annotation_style" = style)
  } else {
    style <- pipeline$get_preferences(
      "voltage_explorer.graphics.channel_annotation_style",
      modes = "character",
      ifnotfound = DEFAULT_CHAN_ANNOT
    ) %OF% OPTIONS_CHAN_ANNOT
  }
  style
}

use_trial_sort_by <- function(trial_sort_by = OPTIONS_TRIAL_SORT) {
  if (!missing(trial_sort_by)) {
    trial_sort_by <- match.arg(trial_sort_by)
    pipeline$set_preferences("voltage_explorer.graphics.trial_sort_by" = trial_sort_by)
  } else {
    trial_sort_by <- pipeline$get_preferences(
      "voltage_explorer.graphics.trial_sort_by",
      modes = "character"
    ) %OF% OPTIONS_TRIAL_SORT
  }
  trial_sort_by
}

use_flipped_y <- function(flip) {
  if (!missing(flip)) {
    flip <- isTRUE(as.logical(flip))
    pipeline$set_preferences("voltage_explorer.graphics.flipped_y" = flip)
  } else {
    flip <- isTRUE(pipeline$get_preferences(
      "voltage_explorer.graphics.flipped_y",
      modes = "logical"
    ))
  }
  flip
}

use_show_crp_decoration <- function(show) {
  if (!missing(show)) {
    show <- isTRUE(as.logical(show))
    pipeline$set_preferences("voltage_explorer.graphics.show_crp_decoration" = show)
  } else {
    show <- isTRUE(pipeline$get_preferences(
      "voltage_explorer.graphics.show_crp_decoration",
      modes = "logical"
    ))
  }
  show
}

reset_graphics_preferences <- function() {
  pipeline$set_preferences(
    "voltage_explorer.graphics.discrete_palette" = list(
      name = "Default",
      colors = group_palette
    ),
    "voltage_explorer.graphics.cex" = DEFAULT_CEX,
    "voltage_explorer.graphics.channel_annotation_style" = DEFAULT_CHAN_ANNOT,
    "voltage_explorer.graphics.trial_sort_by" = DEFAULT_TRIAL_SORT,
    "voltage_explorer.graphics.flipped_y" = FALSE,
    "voltage_explorer.graphics.show_crp_decoration" = TRUE
  )
}
