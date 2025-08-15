library(dipsaus)
fastmap2 <- dipsaus::fastmap2
parse_svec <- dipsaus::parse_svec
drop_nulls <- dipsaus::drop_nulls
`%OF%` <- dipsaus::`%OF%`
`%?<-%` <- dipsaus::`%?<-%`

LOCALIZATION_METHODS <- list(
  "Re-sampled CT" = "resampled",
  "FSL transform + Raw CT + MRI" = "fsl",
  "CT (IJK) to MR (RAS) transform + Raw CT" = "ijk2ras",
  "Localize without CT" = "no_ct"
)

electrode_prototypes <- fastmap2()
local({
  plist <- threeBrain::list_electrode_prototypes()
  for(nm in names(plist)) {
    electrode_prototypes[[nm]] <- list(
      path = plist[[nm]]
    )
  }
})
electrode_types <- c("iEEG", "ECoG", "Others", names(electrode_prototypes))

get_prototype <- function(pname) {
  if(is.na(pname) || !nzchar(pname)) { return() }
  if( pname %in% ravecore::LOCATION_TYPES ) { return() }
  li <- electrode_prototypes[[pname]]
  if(is.null(li)) { return() }
  if(is.null(li$prototype)) {
    proto <- threeBrain::new_electrode_prototype(base_prototype = li$path)
    proto$name <- toupper(pname)
    electrode_prototypes[[pname]]$prototype <- proto
    return(proto)
  }
  return(li$prototype)
}


deparse_svec <- function(nums, connect = '-', concatenate = TRUE, collapse = ',', max_lag = 1, do_sort = TRUE, na_rm = TRUE){
  if( na_rm ) {
    nums <- nums[is.finite(nums)]
  }
  if(length(nums) == 0){
    return('')
  }
  alag <- seq_len(max(1, max_lag))
  if( do_sort ) {
    nums <- sort(unique(nums), na.last = NA)
  }

  force(connect)
  res <- Reduce(x = nums, right = FALSE, init = list(character(0L), NA, 0L), f = function(res, el){
    if(is.na(res[[2]])) {
      if(is.na(el)) {
        res[[3]] <- res[[3]] + 1L
        return(res)
      }
      if(res[[3]] > 0) {
        s <- c(res[[1]], sprintf("NAx%d", res[[3]]))
      } else {
        s <- res[[1]]
      }
      return(list(s, el, el))
    }
    if(is.na(el) || (el - res[[3]] > max_lag) || (el < res[[2]])) {
      if( res[[2]] == res[[3]] ) {
        s <- c(res[[1]], as.character(res[[2]]))
      } else {
        s <- c(res[[1]], sprintf("%s%s%s", res[[2]], connect, res[[3]]))
      }
      if(is.na(el)) {
        return(list(s, el, 1L))
      }
      return(list(s, el, el))
    }
    if( el > res[[3]] ) {
      return(list(res[[1]], res[[2]], el))
    }
    return(res)
  })

  re <- res[[1]]
  if(is.na(res[[2]])) {
    if(res[[3]] > 0) {
      re <- c(re, sprintf("NAx%d", res[[3]]))
    }
  } else {
    if( res[[2]] == res[[3]] ) {
      re <- c(re, as.character(res[[2]]))
    } else {
      re <- c(re, sprintf("%s%s%s", res[[2]], connect, res[[3]]))
    }
  }
  if(concatenate){
    re <- paste(re, collapse = collapse)
  }
  re
}

parse_svec <- function(text, sep = ',', connect = '-:|', sort = FALSE, unique = TRUE, na_rm = TRUE){
  connect <- unique(unlist(strsplit(connect, '')))
  connect[connect %in% c('|', ':', '~')] <- paste0('\\', connect[connect %in% c('|', ':', '~')])
  if('-' %in% connect) {
    connect <- c(connect[connect != "-"], "-")
  }
  connect <- paste(connect, collapse = '')

  if(length(text) != 1) {
    text <- paste(text, collapse = sep)
  }


  if(length(text) == 0 || !nzchar(trimws(text))){
    return(NULL)
  }

  if(is.numeric(text)){
    if(unique) {
      text <- unique(text)
    }
    if(sort) {
      text <- sort(text)
    }
    return(text)
  }
  s <- unlist(strsplit(text, sep, perl = TRUE))
  s <- trimws(s)
  s <- s[s!='']

  s <- s[grepl(sprintf('^[0-9\\ %s]+$', connect), s) | grepl("^NAx[0-9]+$", s)]

  re <- NULL
  for(ss in s){
    if(grepl(sprintf('[%s]', connect), ss)){
      ss <- unlist(strsplit(ss,  sprintf('[%s]', connect), perl = TRUE))
      ss <- trimws(ss)
      ss <- ss[grepl('^[0-9]+$', ss)]
      ss <- as.numeric(ss)
      ss <- ss[!is.na(ss)]
      if(length(ss) >= 2){
        re <- c(re, (ss[1]:ss[2]))
      }
    } else if ( grepl("^NAx[0-9]+$", ss) ) {
      if( !na_rm ) {
        reps <- gsub("^NAx", "", ss)
        reps <- as.integer(reps)
        if(reps > 0) {
          re <- c(re, rep(NA, reps))
        }
      }
    } else {
      re <- c(re, as.numeric(ss))
    }
  }

  if(unique){
    re <- unique(re)
  }
  if(sort){
    re <- sort(re)
  }
  if(na_rm && anyNA(re)) {
    re <- re[!is.na(re)]
  }

  return(re)
}


read_plan_list <- function( electrode_file, brain = NULL, strict = FALSE, instantiate = FALSE ) {
  default_plan <- list(
    list(
      label = "NoLabel",
      dimension = "1",
      type = "iEEG",
      hemisphere = "auto"
    )
  )

  electrode_file <- electrode_file[file.exists(electrode_file)]
  if(!length(electrode_file)){
    if( strict ) {
      stop("Cannot find valid electrode file.")
    }
    return(default_plan)
  }

  table <- ravecore:::safe_read_csv(electrode_file[[1]])

  table_names <- names(table)
  n <- nrow(table)

  # DIPSAUS DEBUG START
  # electrode_file <- "/Users/dipterix/rave_data/data_dir/YAEL/Precision001/rave/meta/electrodes_unsaved.csv"
  # table <- ravecore:::safe_read_csv(electrode_file[[1]])
  # table$Prototype %?<-% table$Geometry
  # #table <- table[,c("Electrode", "Label")]
  # table_names <- names(table)
  # n <- nrow(table)

  if(!n || !all(c('Electrode', "Label") %in% table_names)) {
    if( strict ) {
      stop("Electrode table must contain column `Electrode` and `Label` (case-sensitive).")
    }
    if(n > 0) {
      if(length(table$Electrode)) {
        default_plan[[1]]$dimension <- deparse_svec(table$Electrode)
      } else {
        default_plan[[1]]$dimension <- as.character(n)
      }
    }
    return(default_plan)
  }

  if(!"LocationType" %in% table_names) {
    table$LocationType <- ravecore::LOCATION_TYPES[[1]]
  } else {
    table$LocationType[!table$LocationType %in% ravecore::LOCATION_TYPES] <- ravecore::LOCATION_TYPES[[1]]
  }
  if(!"Hemisphere" %in% table_names) {
    table$Hemisphere <- "auto"
  }
  if(!"LabelPrefix" %in% table_names) {
    table$LabelPrefix <- gsub("[0-9]+$", "", table$Label)
  }
  if(!"Prototype" %in% table_names) {
    table$Prototype <- ""
  }
  if(!"Dimension" %in% table_names) {
    table$Dimension <- ""
  }
  if(!"Prototype" %in% names(table)) {
    table$Prototype <- ""
  }
  table$Prototype[is.na(table$Prototype)] <- ""
  table$Prototype <- trimws(table$Prototype)

  table_split <- split(table, paste0(table$Prototype, "_", table$LabelPrefix))
  plan <- lapply(seq_along(table_split), function(ii) {
    sub <- table_split[[ii]]

    sub <- sub[order(sub$Electrode), ]
    hemisphere <- sub$Hemisphere[[1]] %OF% c("auto", "left", "right")

    pname <- sub$Prototype[[1]]
    lname <- sub$LabelPrefix[[1]]

    if(is.null(brain)) {
      prototype <- get_prototype(pname)
    } else {
      prototype <- brain$electrodes$add_geometry(
        label_prefix = lname, prototype_name = pname)
    }

    if(is.null(prototype)) {
      pname <- sub$LocationType[[1]]

      re <- list(
        label = lname,
        dimension = deparse_svec(sub$Electrode, do_sort = TRUE),
        type = pname %OF% ravecore::LOCATION_TYPES,
        hemisphere = hemisphere,
        min_channel = min(sub$Electrode)
      )

      if( instantiate ) {
        re$prototype <- NULL
        re$batch_size <- nrow(sub)
        re$prototype_name <- NULL
      }
      # re$coordinate_table = sub
    } else {
      n_channels <- prototype$n_channels
      chans <- sub$Electrode

      if(length(sub$ContactOrder)) {
        corder <- as.integer(sub$ContactOrder)
      } else {
        corder <- seq_along(chans)
      }

      sel <- !is.na(corder) & !duplicated(corder) & corder >= 1 & corder <= n_channels
      corder <- corder[sel]
      if(!length(corder)) {
        re <- list(
          label = lname,
          dimension = "",
          type = pname,
          hemisphere = hemisphere,
          min_channel = NA
        )
      } else {
        channel_number <- rep(NA_integer_, n_channels)
        channel_number[corder] <- chans[sel]

        dimension <- deparse_svec(channel_number, do_sort = FALSE, na_rm = FALSE)
        if(grepl("^[0-9]+$", dimension)) {
          dimension <- sprintf("%s,", dimension)
        }
        re <- list(
          label = lname,
          dimension = dimension,
          type = pname,
          hemisphere = hemisphere,
          min_channel = min(channel_number, na.rm = TRUE)
        )
      }

      if( instantiate ) {
        re$prototype <- prototype
        re$prototype_name <- toupper(pname)
        # used to control the orientation of the electrode
        geometry_table <- prototype$control_points

        if(is.data.frame(geometry_table)) {
          re$batch_size <- nrow(geometry_table)
        } else {
          # still need to manually localize everything
          re$batch_size <- nrow(sub)
        }
      }
    }

    if( instantiate ) {
      # For server functions
      re$group_table <- sub
      re$label_prefix <- re$label


      if( length(sub$Interpolation) && grepl("^[0-9x,. ]+$", sub$Interpolation[[1]] ) ) {
        interpolation_string <- sub$Interpolation[[1]]
      } else {
        if(!is.null(prototype) && isTRUE(is.character(prototype$default_interpolation))) {
          interpolation_string <- prototype$default_interpolation
        } else {
          interpolation_string <- max(c(1L, re$batch_size - 2L))
        }
      }

      re$interpolation_string <- interpolation_string

      # Also need a group name
      re$button_label <- sprintf("%s.%d.%s [%s]", re$label_prefix, nrow(sub), re$type, re$dimension)

    }

    return(re)

  })

  min_chans <- sapply(plan, "[[", "min_channel")
  plan <- plan[order(min_chans, na.last = TRUE)]

  plan
}


summarize_plan_list <- function( plan ) {

  n <- 0L

  lapply(seq_along(plan), function(ii) {
    item <- plan[[ii]]

    if(!is.list(item) || length(item$dimension) != 1) {
      return(NULL)
    }

    if(length(item$label) == 0 || !nzchar(trimws(item$label))) {
      item$label <- sprintf("Group%s_Chan", ii)
    } else {
      item$label <- trimws(item$label)
    }

    dimension <- trimws(item$dimension)
    if(!nzchar(dimension)) {
      item$msg <- "No channel in this group. This group will be skipped."
      item$channels <- ""
      item$n <- 0L
      return(item)
    }
    proto <- get_prototype(item$type)
    proto_specs <- "No details"
    if(inherits(proto, "ElectrodePrototype")) {
      proto_specs <- paste(utils::capture.output({ print(proto, details = FALSE) }), collapse = "\n")
    }
    if(grepl("^[0-9]+$", dimension)) {
      n_channels <- as.integer(dimension)
      if( n_channels == 0 ) {
        item$msg <- "No channel in this group. This group will be skipped."
        item$channels <- ""
        item$n <- 0L
        return(item)
      }
      item$n <- n_channels
      if( n_channels == 1 ) {
        item$channels <- as.character(n + 1)
      } else {
        item$channels <- sprintf("%d-%d", n + 1, n + n_channels)
      }
      n <<- n + n_channels

      if(is.null(proto)) {
        item$msg <- sprintf("%s channel [%s] (nchans=%d)", item$type, item$channels, n_channels)
      } else {
        item$msg <- sprintf("<details><summary>%s channel [%s] (nchans=%d). This electrode prototype has %d channels in total.</summary><pre>%s</pre></details>", item$type, item$channels, n_channels, proto$n_channels, proto_specs)
      }

      return(item)
    }
    if(grepl("^[0-9 ]+x[0-9 ]+$", dimension)) {
      dimension <- parse_svec(dimension, sep = "[x]", unique = FALSE)
      n_channels <- prod(dimension)
      item$n <- n_channels

      if( n_channels == 1 ) {
        item$channels <- as.character(n + 1)
      } else {
        item$channels <- sprintf("%d-%d", n + 1, n + n_channels)
      }
      n <<- n + n_channels

      if(is.null(proto)) {
        item$msg <- sprintf("%s channel [%s] (nchans=%d)", item$type, item$channels, n_channels)
      } else {
        item$msg <- sprintf("<details><summary>%s channel [%s] (nchans=%d). This electrode prototype has %d channels in total.</summary><pre>%s</pre></details>", item$type, item$channels, n_channels, proto$n_channels, proto_specs)
      }
      return(item)
    }
    chans <- parse_svec(dimension, sort = FALSE, unique = FALSE, na_rm = FALSE)
    item$channels <- deparse_svec(chans, do_sort = FALSE, na_rm = FALSE)
    item$n <- length(chans)
    n <<- max(c(n, chans), na.rm = TRUE)

    plugged <- item$n - sum(is.na(chans))
    if(is.null(proto)) {
      item$msg <- sprintf("%s channel [%s] (nchans=%d, unplugged=%d)", item$type, dimension, plugged, item$n - plugged)
    } else {
      item$msg <- sprintf("<details><summary>%s channel [%s] (nchans=%d, unplugged=%d). This electrode prototype has %d channels in total.</summary><pre>%s</pre></details>", item$type, dimension, plugged, item$n - plugged, proto$n_channels, proto_specs)
    }

    return(item)
  })

}

url_neurosynth <- function(x, y, z) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  z <- as.numeric(z)

  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  z[is.na(z)] <- 0
  sprintf("https://neurosynth.org/locations/?x=%.0f&y=%.0f&z=%.0f", x, y, z)
}

