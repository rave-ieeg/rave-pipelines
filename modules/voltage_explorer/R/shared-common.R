`%?<-%` <- dipsaus::`%?<-%`
`%OF%` <- dipsaus::`%OF%`

group_palette <- c("#FFA500", "#1874CD", "#006400", "#FF4500", "#A52A2A", "#7D26CD",
                   "#FE00FA", "#16FF32", "#FBE426", "#B00068", "#1CFFCE", "#90AD1C",
                   "#2ED9FF", "#DEA0FD", "#F8A19F", "#325A9B", "#C4451C", "#1C8356",
                   "#85660D", "#B10DA1", "#1CBE4F", "#F7E1A0", "#C075A6", "#AAF400",
                   "#BDCDFF", "#822E1C", "#B5EFB5", "#7ED7D1", "#1C7F93", "#3B00FB"
)

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
      cex <- 1.4
    }
    pipeline$set_preferences("voltage_explorer.graphics.cex" = cex)
  } else {
    cex <- pipeline$get_preferences("voltage_explorer.graphics.cex", modes = "numeric", ifnotfound = 1.4)
    if (!isTRUE(cex > 0)) {
      cex <- 1.4
      pipeline$set_preferences("voltage_explorer.graphics.cex" = cex)
    }
  }
  cex
}

use_channel_annotation_style <- function(style = c("number", "short", "label", "full")) {
  if (!missing(style)) {
    style <- match.arg(style)
    pipeline$set_preferences("voltage_explorer.graphics.channel_annotation_style" = style)
  } else {
    style <- pipeline$get_preferences(
      "voltage_explorer.graphics.channel_annotation_style",
      modes = "character",
      ifnotfound = "number"
    ) %OF% c("number", "short", "label", "full")
  }
  style
}

use_trial_sort_by <- function(trial_sort_by = c("stimuli", "trial")) {
  if (!missing(trial_sort_by)) {
    trial_sort_by <- match.arg(trial_sort_by)
    pipeline$set_preferences("voltage_explorer.graphics.trial_sort_by" = trial_sort_by)
  } else {
    trial_sort_by <- pipeline$get_preferences(
      "voltage_explorer.graphics.trial_sort_by",
      modes = "character"
    ) %OF% c("stimuli", "trial")
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
    "voltage_explorer.graphics.cex" = 1.4,
    "voltage_explorer.graphics.channel_annotation_style" = "number",
    "voltage_explorer.graphics.trial_sort_by" = "stimuli",
    "voltage_explorer.graphics.flipped_y" = FALSE,
    "voltage_explorer.graphics.show_crp_decoration" = TRUE
  )
}