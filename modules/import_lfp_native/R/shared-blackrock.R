#' Class definition to load data from 'BlackRock' 'Micro-systems' files
#' @description Please use R package 'readNSx' for better support. This
#' class is for some legacy pipelines.
#' @export
BlackrockFile <- R6::R6Class(
  classname = "BlackrockFile",
  portable = TRUE,

  private = list(
    .NS_MAX = 6L,

    .path = character(0L),
    .filebase = character(0L),

    .nev_file = character(0L),
    .nev = NULL,

    .ns1_file = character(0L),
    .ns1_data = NULL,

    .ns2_file = character(0L),
    .ns2_data = NULL,

    .ns3_file = character(0L),
    .ns3_data = NULL,

    .ns4_file = character(0L),
    .ns4_data = NULL,

    .ns5_file = character(0L),
    .ns5_data = NULL,

    .ns6_file = character(0L),
    .ns6_data = NULL,

    .initialize = function(path, header_only = TRUE, force = FALSE, verbose = FALSE, nev_data = TRUE) {

      # Get parent directory and file prefix
      path <- gsub(sprintf("\\.(ccf|nev|ns[1-%d])$", private$.NS_MAX),
                   '', path[[1]], ignore.case = TRUE)
      dir_path <- normalizePath(dirname(path), mustWork = TRUE)
      filebase <- basename(path)

      fs <- list.files(dir_path, pattern = sprintf("\\.(nev|ns[1-%d])$", private$.NS_MAX),
                       ignore.case = TRUE, full.names = FALSE,
                       all.files = TRUE, recursive = FALSE,
                       include.dirs = FALSE)

      is_nev <- tolower(fs) == sprintf("%s.nev", tolower(filebase))

      nsx <- seq_len(private$.NS_MAX)
      is_nsx <- tolower(fs) %in% sprintf("%s.ns%.0f", tolower(filebase), nsx)
      nev_path <- file.path(dir_path, fs[is_nev])
      nsx_files <- file.path(dir_path, fs[is_nsx])

      if(length(nev_path) == 0) {
        stop("Cannot find any .nev file with this path: ", path)
      }

      private$.path <- dir_path
      private$.filebase <- filebase
      private$.nev_file <- fs[is_nev][[1]]
      if(!inherits(private$.nev, "fastmap2")) {
        private$.nev <- dipsaus::fastmap2()
      }
      # private$.nev <- NULL

      for(i in seq_len(private$.NS_MAX)) {
        private[[sprintf(".ns%d_file", i)]] <- character(0L)
        private[[sprintf(".ns%d_data", i)]] <- NULL
      }


      lapply(fs[is_nsx], function(fname) {
        nm <- unlist(strsplit(fname, "\\."))
        ext <- tolower(nm[[length(nm)]])
        private[[sprintf(".%s_file", ext)]] <- fname
      })

      # load nev
      headers <- read_nsx_nev(paths = nsx_files, nev_path = nev_path,
                              header_only = header_only, verbose = verbose,
                              ram = FALSE, force_update = force,
                              nev_data = nev_data)

      # save nev data
      if(nev_data || !inherits(private$.nev, "fastmap2")) {
        private$.nev <- headers$nev
      } else {
        private$.nev$basic_header <- headers$nev$basic_header
        private$.nev$extended_header <- headers$nev$extended_header
      }


      # save nsx data
      for(fname in names(headers$nsx)) {
        nm <- unlist(strsplit(fname, "\\."))
        ext <- tolower(nm[[length(nm)]])
        private[[sprintf(".%s_data", ext)]] <- headers$nsx[[fname]]
      }

      return()

    },

    .has_data = function() {
      sapply(paste0("ns", seq_len(private$.NS_MAX)), function(nm) {
        nsx <- private[[sprintf(".%s_data", nm)]]
        if(!length(nsx)) {
          return(FALSE)
        }
        return(inherits(nsx$data, "FileArray"))
      }, simplify = TRUE, USE.NAMES = TRUE)
    },

    .electrode_ids = function() {
      has_nsx <- self$has_nsx
      nms <- names(has_nsx)[has_nsx]

      time_origin <- private$.nev$basic_header$time_origin$value

      re <- lapply(nms, function(nm) {
        header <- private[[sprintf(".%s_data", nm)]]
        if(!length(header$ext_header$CC) || !nrow(header$ext_header$CC)) {
          return()
        }
        global_srate <- header$basic_header$time_resolution_timestamp$value
        channel_srate <- global_srate / header$basic_header$period$value
        # TODO: what if nsx is created the next day of nev? that's weird...
        start_time <- sum((
          header$basic_header$time_origin$value - time_origin
        ) * c(
          0, 0, 0, 0, 3600000, 60000, 1000, 1
        )) / 1000

        # Already tried to convert to uV
        data.frame(
          Electrode = header$ext_header$CC$electrode_id,
          Label = header$ext_header$CC$electrode_label,
          SampleRate = channel_srate,
          NSType = nm,
          NSOrder = seq_along(header$ext_header$CC$electrode_id),
          TimeStart = start_time,
          stringsAsFactors = FALSE
        )
      })
      re <- dipsaus::drop_nulls(re)
      do.call("rbind", re)
    }

  ),

  public = list(

    #' @field block character, session block ID
    block = character(),

    #' @description print user-friendly messages
    print = function() {
      has_nsx <- self$has_nsx
      available_nsx <- names(has_nsx)[has_nsx]

      etable <- self$electrode_table
      sample_rates <- self$sample_rates
      recording_duration <- self$recording_duration
      epoch <- self$get_epoch()

      nsx_info <- lapply(available_nsx, function(nm) {
        sel <- etable$NSType == nm
        elec <- etable$Electrode[sel]
        sprintf(
          "  - %s: sample rate [%.1f Hz], duration [%.2f sec], electrode [%s, n=%d]\n",
          nm,
          sample_rates[[nm]], recording_duration[[nm]],
          dipsaus::deparse_svec(elec), length(elec)
        )
      })


      cat(c(
        "BlackRock Micro-systems file: [", private$.filebase, ']\n',
        "Directory: ", private$.path, "\n",
        "Version: ", paste0(sprintf("%.0f", self$version), collapse = "."), "\n",
        "Block: ", self$block, "\n",
        "# of comments/trial: ", ifelse(is.data.frame(epoch), nrow(epoch), "N/A"), "\n",
        "Available NSx: ", paste0("ns", seq_len(private$.NS_MAX)[self$has_nsx], collapse = ", "), "\n",
        unlist(nsx_info)
      ), sep = "")
    },

    #' @description constructor
    #' @param path the path to 'BlackRock' file, can be with or without file
    #' extensions
    #' @param block session block ID; default is the file name
    #' @param nev_data whether to load comments and 'waveforms'
    initialize = function(path, block, nev_data = TRUE) {
      if(missing(block)) {
        block <- basename(path)
        block <- gsub("\\.(nev|ns[0-9]|ccf)$", "", block, ignore.case = TRUE)
      }
      self$block <- block
      private$.initialize(path, nev_data = nev_data)
    },

    #' @description get 'NEV' file path
    #' @returns absolute file path
    nev_path = function() {
      file.path(private$.path, private$.nev_file)
    },

    #' @description get 'NSx' file paths
    #' @param which which signal file to get, or \code{NULL} to return all
    #' available paths, default is \code{NULL}; must be integers
    #' @returns absolute file paths
    nsx_paths = function(which = NULL) {
      which <- as.integer(which)
      if(!length(which)) {
        which <- which(self$has_nsx)
      } else if (any(is.na(which))) {
        stop("$nsx_paths: parameter `which` must be an integer")
      }
      sapply(sprintf("ns%d", which), function(nm) {
        file.path(private$.path, private[[sprintf(".%s_file", nm)]])
      }, simplify = FALSE, USE.NAMES = TRUE)
    },

    #' @description refresh and load 'NSx' data
    #' @param force whether to force reload data even if the data has been
    #' loaded and cached before
    #' @param verbose whether to print out messages when loading
    #' @param nev_data whether to refresh 'NEV' extended data; default is false
    #' @returns nothing
    refresh_data = function(force = FALSE, verbose = TRUE, nev_data = FALSE) {
      private$.initialize(self$base_path, header_only = FALSE,
                          force = force, verbose = verbose, nev_data = nev_data)
      invisible()
    },

    #' @description get epoch table from the 'NEV' comment data packet
    #' @returns a data frame
    get_epoch = function() {

      srate <- self$sample_rate_nev_timestamp

      re <- lapply(as.list(private$.nev$data_packets), function(packet) {
        if(!identical(packet$value$event, "comment")) {
          return(NULL)
        }
        # extract comment, usually epoch
        data.frame(
          Block = self$block,
          Time = packet$value$timestamp / srate,
          Condition = packet$value$comment,
          stringsAsFactors = FALSE
        )
      })
      re <- dipsaus::drop_nulls(re)
      if(!length(re)) {
        return(data.frame(
          Block = character(0L),
          Time = numeric(0L),
          Condition = character(0L),
          stringsAsFactors = FALSE
        ))
      }
      re <- do.call("rbind", re)
      re
    },

    #' @description get 'waveform' of the spike data
    #' @returns a list of spike 'waveform' (without normalization)
    get_waveform = function() {
      srate_timestamp <- self$sample_rate_nev_timestamp
      wave_table <- private$.nev$extended_header$NEUEVWAV
      srate_waveform <- private$.nev$basic_header$time_resolution_samples$value
      re <- lapply(as.list(private$.nev$data_packets), function(packet) {
        if(!identical(packet$value$event, "spike")) {
          return(NULL)
        }
        re <- packet$value
        re$time <- re$timestamp / srate_timestamp
        re$electrode <- re$packet_id

        sel <- wave_table$electrode_id == re$electrode
        if(!any(sel)) {
          return(NULL)
        }
        spike_width <- wave_table$spike_width[sel][[1]]
        re$sample_rate <- srate_waveform
        re$duration <- spike_width / srate_waveform

        re$timestamp <- NULL
        re$packet_id <- NULL
        re$reserved <- NULL
        re$event <- NULL
        re
      })
      re <- dipsaus::drop_nulls(re)
      attr(re, "NEUEVWAV") <- wave_table
      re
    },

    #' @description get electrode data
    #' @param electrode integer, must be a length of one
    #' @param nstype which signal bank, for example, \code{'ns3'}, \code{'ns5'}
    #' @returns a normalized numeric vector (analog signals with \code{'uV'}
    #' as the unit)
    get_electrode = function(electrode, nstype = NULL) {

      if(length(electrode) != 1) {
        stop("$get_electrode: electrode length must be one")
      }
      elec_table <- private$.electrode_ids()
      sel <- elec_table$Electrode %in% electrode
      if(length(nstype)) {
        sel <- sel & elec_table$NSType %in% nstype
      }
      sel <- which(sel)

      if(!length(sel)) {
        stop("$get_electrode: electrode cannot be found [", electrode, "]")
      }

      sel <- sel[[1]]

      ns_type <- elec_table$NSType[[sel]]
      ns_order <- elec_table$NSOrder[[sel]]
      if(!private$.has_data()[[ns_type]]) {
        self$refresh_data(verbose = FALSE)
      }

      header <- private[[sprintf(".%s_data", ns_type)]]
      re <- header$data[, ns_order]
      attr(re, "meta") <- elec_table[sel, ]
      attr(re, "unit") <- "uV"
      re
    }

  ),

  active = list(

    #' @field base_path absolute base path to the file
    base_path = function() {
      file.path(private$.path, private$.filebase)
    },

    #' @field version 'NEV' specification version
    version = function() {
      private$.nev$basic_header$file_spec$value
    },

    #' @field electrode_table electrode table
    electrode_table = function() {
      private$.electrode_ids()
    },

    #' @field sample_rate_nev_timestamp sample rate of 'NEV' data packet
    #' time-stamps
    sample_rate_nev_timestamp = function() {
      re <- private$.nev$basic_header$time_resolution_timestamp$value
      if(!length(re)) {
        re <- NA
      }
      re[[1]]
    },

    #' @field has_nsx named vector of 'NSx' availability
    has_nsx = function() {
      nms <- paste0("ns", seq_len(private$.NS_MAX))
      structure(
        sapply(nms, function(nm) {
          length(private[[sprintf(".%s_file", nm)]]) == 1
        }, simplify = TRUE, USE.NAMES = TRUE),
        names = nms
      )
    },

    #' @field recording_duration recording duration of each 'NSx'
    recording_duration = function() {
      has_nsx <- self$has_nsx
      nms <- names(has_nsx)[has_nsx]
      sapply(nms, function(nm) {
        header <- private[[sprintf(".%s_data", nm)]]
        ntp <- header$data_header$data_header$value$number_of_data_points
        srate <- header$basic_header$time_resolution_timestamp$value /
          header$basic_header$period$value
        ntp / srate
      })
    },

    #' @field sample_rates sampling frequencies of each 'NSx' file
    sample_rates = function() {
      has_nsx <- self$has_nsx
      nms <- names(has_nsx)[has_nsx]
      sapply(nms, function(nm) {
        header <- private[[sprintf(".%s_data", nm)]]
        header$basic_header$time_resolution_timestamp$value /
          header$basic_header$period$value
      })
    }

  )
)

load_nev_events <- function(nsp, epoch_types) {
  readNSx_get_event <- readNSx::get_event
  epoch_table <- lapply(epoch_types, function(epoch_type){
    event <- readNSx_get_event(nsp, epoch_type)

    if(!is.data.frame(event)) { return() }
    condition <- switch(
      epoch_type,
      "comment" = event$comment,
      "digital_inputs" = event$digital_input,
      "recording" = c("start", "stop", "pause", "resume")[event$event_reason + 1],
      "configuration" = event$config_changed,
      "log" = paste(event$app_name, event$log_comment, sep = "|"),
      "button_trigger" = c("undefined", "button_press", "event_reset")[event$trigger_type + 1],
      "tracking" = sprintf("parent_%s-node_%s-node_count_%s-parent_count_%s-tracking_%s",
                           event$parent_id, event$node_id, event$node_count,
                           event$parent_count, event$tracking_point),
      "video_sync" = sprintf("video_%s-frame_%s-elapsed_%s-source_%s",
                             event$video_file_number, event$video_frame_number,
                             event$video_elapsed_time, event$video_source_id),
      default = {
        "Unknown"
      }
    )

    data.frame(
      SourceFile = event$original_filename,
      Condition = condition,
      EventType = epoch_type,
      AbsoluteTime = event$time_in_seconds
    )
  })

  epoch_table <- dipsaus::drop_nulls(epoch_table)

  if(!length(epoch_table)) { return() }

  epoch_table <- do.call(rbind, epoch_table)

  if(!nrow(epoch_table)) { return() }
  epoch_table <- epoch_table[order(epoch_table$AbsoluteTime), ]
  epoch_table$Specification <- nsp$nev$specification$version
  epoch_table$EventOrder <- seq_len(nrow(epoch_table))

  return(epoch_table)
}

#' Convert 'BlackRock' 'NEV/NSx' files
#' @param file path to any 'NEV/NSx' file
#' @param block the block name, default is file name
#' @param subject subject code to save the files; default is \code{NULL}
#' @param to save to path, must be a directory; default is under the file path.
#' If \code{subject} is provided, then the default is \code{subject} raw
#' directory path
#' @param epoch what type of events should be included in epoch file; default
#' include comment, digital inputs, recording trigger, configuration change,
#' log comment, button trigger, tracking, and video trigger.
#' @param format output format, choices are \code{'mat'} or \code{'hdf5'}
#' @param header_only whether just to generate channel and epoch table; default
#' is false
#' @param ... ignored for enhanced backward compatibility
#' @returns The results will be stored in directory specified by \code{to}.
#' Please read the output message carefully.
#' @export
convert_blackrock <- function(
    file, block = NULL, subject = NULL, to = NULL,
    epoch = c("comment", "digital_inputs", "recording", "configuration",
              "log", "button_trigger", "tracking", "video_sync"),
    format = c("mat", "hdf5"), header_only = FALSE, ...) {

  # DIPSAUS DEBUG START
  # file = '~/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV023/BLOCK024_mTurkWords_run1/PAV023_Datafile_024.nev'
  # format <- "mat"
  # block <- NULL
  # to <- NULL
  # comments <- FALSE
  # epoch = c("comment", "digital_inputs", "recording", "configuration",
  #           "log", "button_trigger", "tracking", "video_sync")
  # subject <- "PAV023"


  format <- match.arg(format)
  epoch_types <- epoch[epoch %in% c("comment", "digital_inputs", "recording", "configuration", "log", "button_trigger", "tracking", "video_sync")]

  if(length(block) != 1 || !nzchar(block)) {
    block <- basename(file)
    block <- gsub("\\.(ccf|nev|ns[1-6])", "", block, ignore.case = TRUE)
  }

  # prepare saving directory
  if(!length(to)) {
    if(length(subject)) {
      root_path <- file.path(raveio_getopt("raw_data_dir"), subject[[1]])
    } else {
      root_path <- dirname(file)
    }
    extraction_prefix <- file.path(root_path, gsub("\\.(nev|ns[0-9])", "", basename(file), ignore.case = TRUE), "block")
    to <- file.path(root_path, block)
  } else {
    extraction_prefix <- file.path(to[[1]], "extraction", "data")
    to <- to[[1]]
  }

  catgl("Loading NEV/NSx files...", level = "INFO")
  suppressWarnings({
    nsp <- tryCatch({
      readNSx::get_nsp(x = extraction_prefix)
    }, error = function(e) {
      readNSx::import_nsp(path = file, prefix = extraction_prefix, exclude_events = "spike", partition_prefix = "_part")
    })
  })

  electrode_list <- lapply(seq_len(9), function(ii) {
    nsx <- nsp[[sprintf("ns%d", ii)]]
    if(!length(nsx)) { return() }
    as.integer(nsx$header_extended$CC$electrode_id)
  })
  names(electrode_list) <- sprintf("ns%d", seq_len(9))
  electrode_list <- electrode_list[!vapply(electrode_list, is.null, FALSE)]

  if(!length(electrode_list)) {
    catgl("No channel found. Exit...", level = "INFO")
    return(invisible(structure(to, extraction_prefix = extraction_prefix, nparts = 1)))
  }

  for(nsx in names(electrode_list)) {
    catgl("Found channel(s) { dipsaus::deparse_svec(electrode_list[[nsx]]) } in [{ nsx }]", level = "INFO")
  }

  to <- normalizePath(to, mustWork = FALSE)

  electrodes <- unname(unlist(electrode_list))
  electrode_list_table <- do.call(rbind, lapply(names(electrode_list), function(nsx) {
    data.frame(
      Electrode = electrode_list[[nsx]],
      NSType = nsx,
      ChannelOrder = order(electrode_list[[nsx]])
    )
  }))
  electrode_table_raw <- readRDS(sprintf("%s_channels.rds", nsp$nev$prefix))

  nparts <- max(c(unlist(lapply(seq_len(9), function(part) {
    nsx <- nsp[[sprintf("ns%s", part)]]
    if(length(nsx)) { return(nsx$nparts) }
    return(0)
  })), 1))

  # generate epoch
  epoch_table <- load_nev_events(nsp, epoch_types)

  lapply(seq_len(nparts), function(part) {
    if(nparts > 1) {
      dir <- sprintf("%s_part%s", to, part)
    } else {
      dir <- to
    }

    dir <- dir_create2(dir)

    flag <- file.path(dir, "rave_conversion_flag.txt")
    if(file.exists(flag)) {
      flag_data <- load_yaml(flag)
      flag_has_header <- isTRUE(flag_data$header_converted)
      flag_has_content <- isTRUE(flag_data$data_converted)
    } else {
      flag_has_header <- FALSE
      flag_has_content <- FALSE
    }
    if(!flag_has_header || !(header_only || flag_has_content)) {

      block_meta <- lapply_async(electrodes, function(e) {

        channel <- readNSx::get_channel(nsp, e)
        partition_data <- channel$channel_detail[[sprintf("part%s", part)]]

        row <- electrode_list_table[which(electrode_list_table$Electrode == e)[[1]], ]
        label <- electrode_table_raw$name[electrode_table_raw$original_channel == e][[1]]
        re <- data.frame(
          Electrode = e,
          Label = label,
          SampleRate = partition_data$meta$sample_rate,
          NSType = row$NSType,
          ChannelOrder = row$ChannelOrder,
          TimeStart = partition_data$meta$relative_time,
          Partition = part,
          Duration = partition_data$meta$duration,
          stringsAsFactors = FALSE
        )

        if(!header_only) {
          info_str <- jsonlite::toJSON(as.list(re), auto_unbox = TRUE)
          if( format == "mat" ) {
            fname <- file.path(dir, sprintf("channel_%s.mat", e))
            s <- as.vector(partition_data$data[])
            # R.matlab::writeMat(fname, data = s, meta = info_str)
            ieegio::io_write_mat(x = list(data = s, meta = info_str), con = fname, method = "R.matlab")
          } else {
            fname <- file.path(dir, sprintf("channel_%s.h5", e))
            s <- as.vector(partition_data$data[])
            save_h5(info_str, file = fname, name = "meta", quiet = TRUE, replace = TRUE, new_file = TRUE)
            save_h5(s, file = fname, name = "data", quiet = TRUE, replace = TRUE)
          }
        }

        return(re)
      }, callback = function(e) {
        sprintf("%s|Electrode %s", ifelse(header_only, "Collecting data", "Writing data"), e)
      })

      electrode_table <- do.call(rbind, block_meta)
      safe_write_csv(electrode_table, file = file.path(dir, "channels.csv"),
                     row.names = FALSE, quiet = TRUE)
      saveRDS(electrode_table, file = file.path(dir, "channels.rds"))


      if(length(epoch_table)) {

        time_start <- min(electrode_table$TimeStart)
        time_end <- max(electrode_table$TimeStart + electrode_table$Duration)
        epoch_table_sub <- epoch_table[
          epoch_table$AbsoluteTime > time_start - 0.01 &
            epoch_table$AbsoluteTime < time_end + 0.01, ]

        if(nrow(epoch_table_sub) > 0) {
          epoch_table_sub$Block <- basename(dir)
          epoch_table_sub$Time <- epoch_table_sub$AbsoluteTime - time_start

          safe_write_csv(
            epoch_table_sub, row.names = FALSE, quiet = TRUE,
            file = file.path(dir, "events.csv")
          )
          saveRDS(object = epoch_table_sub, file = file.path(dir, "events.rds"))
        }
      }


      flag_has_header <- TRUE
      if(header_only) {
        flag_has_content <- FALSE
      } else {
        flag_has_content <- TRUE
      }

      save_yaml(list(
        header_converted = flag_has_header,
        data_converted = flag_has_content,
        comment = "Please remove this file if you want RAVE to overwrite the data here."
      ), file = flag)

    }

  })

  catgl("Conversion done. Please check the output path prefix: [{to}]", level = "INFO")
  return(invisible(structure(to, extraction_prefix = extraction_prefix, nparts = nparts)))
}


validate_spec <- function(name, type, size, n = 1, names = NULL, ...) {
  # check type first
  size_ <- byte_size_lut[[type]]
  if(!missing(size) && length(size) == 1) {
    if(!is.null(size_) && !size %in% size_) {
      stop("Cannot parse name [", name, "]: the type [", type, "] cannot have ", size, " bytes.")
    }
    size_ <- size
  } else if (!length(size_)) {
    stop("Cannot parse name [", name, "]: unknown size for data type: ", type)
  } else {
    size_ <- size_[[1]]
  }
  if(!length(n)) {
    n <- 1L
  } else {
    n <- as.integer(n)
    if (length(n) > 1 || any(is.na(n))) {
      stop("Element length `n` must be an integer")
    }
  }

  re <- list(...)
  re$name <- name
  re$type <- type
  re$size <- size_
  re$n <- n
  re$names <- names
  re$.bytes <- n * size_
  return(re)
}

#' @export
`print.nev-nsx-entry` <- function(x, ...) {
  cat(sprintf("[%s]: %s\n", x$name, deparse1(x$value)))
}

#' @export
`print.nev-nsx-entry-list` <- function(x, ...) {
  for(ii in seq_len(length(x))) {
    print(x[[ii]])
  }
}


parse_uint8 <- function(x, ...) {
  # rawToUInt8(x)
  ravetools::raw_to_uint8(x)
}
parse_int8 <- function(x, ...) {
  # rawToInt8(x)
  ravetools::raw_to_int8(x)
}
parse_uint16 <- function(x, ...) {
  # rawToUInt16(x)
  ravetools::raw_to_uint16(x)
}
parse_int16 <- function(x, ...) {
  # rawToInt16(x)
  ravetools::raw_to_int16(x)
}
parse_uint32 <- function(x, ...) {
  # rawToUInt32(x)
  ravetools::raw_to_uint32(x)
}
parse_int32 <- function(x, ...) {
  # rawToInt32(x)
  ravetools::raw_to_int32(x)
}
parse_uint64 <- function(x, ...) {
  # There is no R data type that can hold uint64
  # luckily, blackrock uses uint64 to store timestamp, which
  # should not exceed the half limit, and int64 should suffice
  ravetools::raw_to_int64(x)
}
parse_int64 <- function(x, ...) {
  # rawToInt64(x)
  ravetools::raw_to_int64(x)
}
parse_float <- function(x, ...) {
  # rawToInt64(x)
  ravetools::raw_to_float(x)
}

parse_string <- function(x, ...) {
  # rawToString(x)
  ravetools::raw_to_string(x)
}
parse_bit <- function(x, ...) {
  rawToBits(x)
}
parse_raw <- function(x, ...) {
  x
}
parse_reserved <- function(x, ...) {
  return()
}
parse_packet <- function(x, item, ...) {
  names <- names(item$specs)
  idx <- 0
  packet <- lapply(names, function(name) {
    sub_specs <- item$specs[[name]]
    sub_specs$name <- name
    sub_specs <- do.call(validate_spec, sub_specs)
    re <- parse_item(
      x[idx + seq_len(sub_specs$.bytes)],
      sub_specs
    )
    idx <<- idx + sub_specs$.bytes
    re$value
  })
  names(packet) <- names
  if(length(item$event)) {
    packet$event <- item$event
  }
  packet
}
parse_comment_packet <- function(x, item, ...) {
  names <- names(item$specs)
  idx <- 0
  packet <- lapply(names, function(name) {
    sub_specs <- item$specs[[name]]
    sub_specs$name <- name
    sub_specs <- do.call(validate_spec, sub_specs)
    re <- parse_item(
      x[idx + seq_len(sub_specs$.bytes)],
      sub_specs
    )
    idx <<- idx + sub_specs$.bytes
    re$value
  })
  if(is.character(packet[[length(packet)]]) && length(x) > length(idx)) {
    s <- parse_string(x[-seq_len(idx)])
    packet[[length(packet)]] <- paste0(packet[[length(packet)]], s)
  }
  names(packet) <- names
  packet$event <- item$event
  packet
}


parse_item <- function(slice_data, item) {
  # item <- section_specs[[ii]]
  # slice_idx <- section_slice_idx[ii, ]
  # slice_data <- section_data[seq(slice_idx[[1]], slice_idx[[2]])]
  parser <- get(sprintf("parse_%s", item$type), mode = "function")
  if(!is.function(parser)) {
    stop("Cannot obtain parser function for type: ", item$type)
  }
  if(item$n > 1) {
    re <- matrix(slice_data, ncol = item$n, byrow = FALSE)
    re <- apply(re, 2, parser, item = item)
  } else {
    re <- parser(slice_data, item = item)
  }

  if(length(item$names)) {
    names(re) <- item$names
  }
  structure(
    list(
      name = item$name,
      raw = slice_data,
      value = re
    ),
    class = c(
      sprintf("nev-nsx-%s", item$name),
      "nev-nsx-entry"
    )
  )
}

parse__sequential <- function(conn, section_specs) {
  dictionary <- section_specs$dictionary
  keys <- names(dictionary)
  specs <- lapply(keys, function(name) {
    item <- dictionary[[name]]
    item$name <- name
    item <- do.call(validate_spec, item)

    data <- readBin(conn, what = "raw", n = item$size * item$n,
                    size = 1L, endian = "little")

    parse_item(slice_data = data, item = item)
  })
  names(specs) <- keys
  structure(
    specs,
    names = keys,
    class = c("nev-nsx-entry-list", "list")
  )
}
parse__with_string_key <- function(conn, section_specs, n_items,
                                   as_data_frame = TRUE) {
  key_rule <- do.call(validate_spec, section_specs$key_rule)
  dictionary <- section_specs$dictionary
  key_parser <- get(sprintf("parse_%s", key_rule$type), mode = "function")

  initial_read <- key_rule$start_byte + key_rule$.bytes

  re <- lapply(seq_len(n_items), function(ii) {

    data <- readBin(conn, what = "raw", n = initial_read,
                    size = 1L, endian = "little")
    key <- key_parser(data[key_rule$start_byte + seq_len(key_rule$.bytes)])
    item <- dictionary[[key]]
    if(is.null(item)) {
      stop("Cannot find specification for keyword: [", key, "]")
    }
    item$name <- key
    item <- do.call(validate_spec, item)

    p2_length <- item$.bytes - initial_read

    if(p2_length < 0) {
      stop("Wrong specification: data packet size is not enough to aquire packet key/ID. To obtain the key, it requires [", initial_read, "] bytes, but the packet size is: [", item$.bytes, "]")
    }
    if(length(p2_length)) {
      data_part2 <- readBin(conn, what = "raw", n = p2_length,
                            size = 1L, endian = "little")
      data <- c(data, data_part2)
    }

    parse_item(slice_data = data, item = item)
  })

  if(!as_data_frame) {
    return(re)
  }

  # make table for each header type
  names <- sapply(re, "[[", "name")
  tables <- lapply(split(re, names), function(li) {
    do.call("rbind", lapply(li, function(x) {
      as.data.frame(x$value, stringsAsFactors = FALSE)
    }))
  })
  names <- sapply(tables, function(x) {
    x[[key_rule$name]][[1]]
  })
  names(tables) <- names

  tables
}
parse__with_numeric_key <- function(conn, section_specs, data_packet_sizes) {
  re <- dipsaus::fastqueue2()
  if(!data_packet_sizes) {
    return(re)
  }
  key_rule <- do.call(validate_spec, section_specs$key_rule)
  key_parser <- get(sprintf("parse_%s", key_rule$type), mode = "function")
  dictionary <- section_specs$dictionary
  keys <- dipsaus::fastmap2(missing_default = NA)
  lapply(names(dictionary), function(key) {
    keys[dipsaus::parse_svec(key)] <- key
    return(key)
  })
  for(key in names(dictionary)) {
    item <- dictionary[[key]]
    item$name <- item$name %||% key
    dictionary[[key]] <- do.call(validate_spec, item)
  }

  # read the read of data
  while(length({
    data <- readBin(conn, what = "raw", n = data_packet_sizes,
                    size = 1L, endian = "little")
  }) == data_packet_sizes) {

    key <- key_parser(data[key_rule$start_byte + seq_len(key_rule$.bytes)])
    dict_key <- keys[[key]]
    if(is.na(dict_key)) {
      stop(sprintf("Unknown [%s=%s]", key_rule$name, key))
    }
    item <- dictionary[[dict_key]]
    if(is.null(item)) {
      stop("Cannot find specification for keyword: [", key, "]")
    }

    packet <- parse_item(slice_data = data, item = item)
    re$add(packet)
  }
  class(re) <- c("nev-nsx-entry-list", "fastqueue2", "list")
  re
}

parse__nev <- function(nev_path, specification, nev_data = TRUE) {
  conn <- file(nev_path, "rb")
  on.exit({
    close(conn)
  })
  basic_header <- parse__sequential(conn, specification[[1]])

  n_ext_headers <- basic_header$number_of_extended_headers$value
  data_packet_sizes <- basic_header$bytes_in_data_packet$value

  ext_header <- parse__with_string_key(
    conn, specification[[2]], n_items = n_ext_headers)

  re <- dipsaus::fastmap2()
  re$basic_header <- basic_header
  re$extended_header <- ext_header

  # TODO: Postprocessing
  data_packets2 <- dipsaus::fastqueue2()

  if( nev_data ) {
    data_packets <- parse__with_numeric_key(
      conn, specification[[3]],
      data_packet_sizes = data_packet_sizes)

    # parse waveform
    waveform_flag <- rawToBits(basic_header$additional_flags$raw)
    waveform_dtype <- NA
    if(length(waveform_flag)) {
      waveform_flag <- as.integer(waveform_flag[[1]])
      if(waveform_flag == 1) {
        waveform_dtype <- "int16"
      }
    }
    electrode_ids <- ext_header$NEUEVWAV$electrode_id
    spike_widths <- ext_header$NEUEVWAV$spike_width
    bytes_per_waveforms <- ext_header$NEUEVWAV$bytes_per_waveform

    while(!is.null({packet <- data_packets$remove()})) {

      if(length(packet$value$waveform) && is.raw(packet$value$waveform)) {

        electrode_id <- packet$value$packet_id
        sel <- electrode_ids == electrode_id
        waveform <- packet$value$waveform
        if(any(sel)) {
          spike_width <- spike_widths[sel]
          bytes_per_waveform <- bytes_per_waveforms[sel]
          if(bytes_per_waveform == 0) {
            bytes_per_waveform <- 1
          }



          # translate waveform
          if(is.na(waveform_dtype)) {

            bytes <- 2^ceiling(log2(bytes_per_waveform))
            waveform <- matrix(waveform, nrow = bytes_per_waveform)
            waveform_dtype <- sprintf("int%s", bytes * 8)
            parser <- get(sprintf("parse_%s", waveform_dtype), mode = "function")
            waveform <- apply(waveform, 2, function(w) {
              w <- c(w, rep(as.raw(0), bytes - bytes_per_waveform))
              parser(w)
            })
          } else {
            parser <- get(sprintf("parse_%s", waveform_dtype), mode = "function")
            waveform <- parser(waveform)
          }

          packet$value$waveform <- waveform
        }

      }
      data_packets2$add(packet)

    }

    class(data_packets2) <- class(data_packets)
  }

  re$data_packets <- data_packets2
  re
}

parse__nsx <- function(nsx_path, specification, header_only = FALSE, verbose = TRUE,
                       filebase = tempfile(), force_update = FALSE) {
  re <- dipsaus::fastmap2()
  conn <- file(nsx_path, "rb")
  on.exit({ close(conn) })
  basic_header <- parse__sequential(conn, specification[[1]])
  re$basic_header <- basic_header

  ext_header <- parse__with_string_key(conn, specification[[2]], basic_header$channel_count$value)
  re$ext_header <- ext_header

  data_header <- parse__sequential(conn = conn, section_specs = specification[[3]])

  conn_data_offset <- basic_header$bytes_in_headers$value + length(data_header$data_header$raw)

  re$data_header <- data_header

  # create signature
  signature <- dipsaus::digest(list(
    basic_header = basic_header,
    ext_header = ext_header,
    data_header = data_header
  ))

  re$header_signature <- signature

  if(!header_only) {
    # Read the rest of data
    data_specs <- specification[[4]]$dictionary$data_points
    n_timepoints <- data_header$data_header$value$number_of_data_points
    n_channels <- basic_header$channel_count$value

    if(n_timepoints == 0) {
      stop("Cannot read BlackRock NSx file. Cannot obtain a positive number of time-points from the NSx headers")
    }


    arr <- tryCatch(
      expr = {
        if(force_update) {
          stop("Force updating NSx data file.")
        }
        filearray::filearray_checkload(
          filebase = filebase, mode = "readwrite",
          symlink_ok = FALSE, signature = signature,
          rave_data_type = "BlackRock NSx data array",
          units = "uV",
          ready = TRUE
        )
      },
      error = function(e) {
        if(file.exists(filebase)) {
          unlink(filebase, recursive = TRUE)
        }
        dir_create2(dirname(filebase))
        arr <- filearray::filearray_create(
          filebase = filebase,
          dimension = c(n_timepoints, n_channels),
          type = "float", partition_size = 1
        )
        arr$.mode <- "readwrite"
        arr$.header$signature <- signature
        arr$.header$rave_data_type <- "BlackRock NSx data array"
        arr$.header$units <- "uV"
        arr
      }
    )
    if(!isTRUE(arr$get_header(key = "ready", default = FALSE))) {

      parition_size <- ceiling(2^21 / n_channels)
      niters <- ceiling(n_timepoints / parition_size)
      data_specs$name <- "data_partition"

      # Calculate digital to analog transform
      min_digit <- ext_header$CC$min_digital_value[seq_len(n_channels)]
      min_analog <- ext_header$CC$min_analog_value[seq_len(n_channels)]
      ratio <- (
        ext_header$CC$max_analog_value - ext_header$CC$min_analog_value
      ) / (
        ext_header$CC$max_digital_value - ext_header$CC$min_digital_value
      )
      ratio <- ratio[seq_len(n_channels)]

      units <- sapply(ext_header$CC$units, function(unit) {
        switch (
          unit,
          "V" = { 1e6 },
          "mV" = { 1e3 },
          { 1 }
        )
      })[seq_len(n_channels)]
      min_analog <- min_analog * units
      ratio <- ratio * units

      parser <- get(sprintf("parse_%s", data_specs$type),
                    mode = "function", envir = asNamespace('raveio'),
                    inherits = FALSE)

      progress <- ravepipeline::rave_progress(
        "Loading NSx",
        max = niters,
        shiny_auto_close = TRUE,
        quiet = !verbose
      )


      pts_total <- n_timepoints * n_channels
      pts_read <- 0
      lapply(seq_len(niters), function(ii) {
        progress$inc(sprintf("Partition %d", ii))

        data_specs$n <- parition_size * n_channels

        if( data_specs$n > pts_total - pts_read ) {
          data_specs$n <- pts_total - pts_read
        }
        pts_read <<- pts_read + data_specs$n
        data_specs <- do.call(validate_spec, data_specs)

        data <- readBin(conn, what = "raw",
                        n = data_specs$.bytes,
                        size = 1L, endian = "little")

        data <- parser(data)
        ntp <- length(data) / n_channels

        if( round(ntp) != ntp ) {
          warning("Number of points is not integer. The data might be incomplete")
          ntp <- floor(ntp)
          if( ntp > 0 ) {
            data <- data[seq_len(n_channels * ntp)]
          }
        }

        if( ntp > 0 ) {
          dim(data) <- c(n_channels, ntp)
          data <- (data - min_digit) * ratio + min_analog

          arr[seq_len(ntp) + parition_size * (ii - 1), ] <- t(data)
        }

        return()
      })

      arr$set_header(key = "ready", value = TRUE)

    }

    re$data <- arr
  }


  re
}

blackrock_specification <- function(path) {
  header <- readBin(path, what = "raw", size = 1L, endian = "little", n = 10)
  file_type <- parse_string(header[seq_len(8)])
  file_version <- parse_uint8(header[c(9, 10)])

  file_type <- switch (
    file_type,
    NEURALEV = { "nev" },
    BREVENTS = { "nev" },
    BRSMPGRP = { "nsx" },
    NEURALCD = { "nsx" },
    NEURALSG = { "nsx" },
    {
      stop("`read_nsx_nev`: Unsupported files format [", file_type, "] in file: ", path)
    }
  )

  # get specification
  spec_file <- system.file("specifications", sprintf("blackrock-%s-%d.%d.yaml", file_type, file_version[[1]], file_version[[2]]), package = "raveio")

  if(!file.exists(spec_file)) {
    stop(sprintf(
      "`read_nsx_nev`: Unable to find file specification file of [.%s] with version [%d.%d]. The version might be too old (<= 2.1) or too new. Please contact RAVE develop team to add file specification.",
      file_type, file_version[[1]], file_version[[2]]))
  }
  spec <- ieegio::io_read_yaml(spec_file)
  list(
    type = file_type,
    version = file_version,
    config = spec,
    file_size = file.size(path)
  )
}

blackrock_postprocess <- function(nsx, nev = NULL) {
  # reserved for future post-processing
  nsx
}

#' @title Read 'BlackRock' event and signal files
#' @description Please use R package 'readNSx' for better support. This
#' function is for some legacy pipelines.
#' @param paths 'NSx' signal files, usually with file extensions such as
#' \code{'.ns1'}, \code{'.ns2'}, \code{'.ns3'}, \code{'.ns4'}, \code{'.ns5'}.
#' @param nev_path 'NEV' event files, with file extension \code{'.nev'}
#' @param header_only whether to load header information only and avoid
#' reading signal arrays
#' @param nev_data whether to load \code{'.nev'} comments and 'waveforms'
#' @param verbose whether to print out progress when loading signal array
#' @param ram whether to load signals into the memory rather than storing
#' with \code{\link[filearray]{filearray}}; default is false
#' @param force_update force updating the channel data even if the headers
#' haven't changed
#' @param temp_path temporary directory to store the channel data
#' @export
read_nsx_nev <- function(paths, nev_path = NULL,
                         header_only = FALSE, nev_data = TRUE,
                         verbose = TRUE, ram = FALSE, force_update = FALSE,
                         temp_path = file.path(tempdir(), "blackrock-temp")) {
  if(!all(file.exists(paths))) {
    stop("read_nsx_nev: at least one path cannot be found.")
  }

  fnames <- basename(paths)

  if(length(nev_path)) {
    nev_info <- blackrock_specification(nev_path)
    if(!identical(nev_info$type, "nev")) {
      stop("`read_nsx_nev`: the given `nev_path` is not a valid neural-event file (.nev): ",
           nev_path)
    }
    nev <- parse__nev(nev_path, nev_info$config$specification, nev_data)
  } else {
    nev <- NULL
  }

  progress <- ravepipeline::rave_progress(
    title = "Reading blackrock",
    max = length(paths),
    shiny_auto_close = TRUE,
    quiet = header_only || !verbose || !length(paths)
  )

  nsx <- structure(
    lapply(paths, function(path) {
      info <- blackrock_specification(path)

      if(!identical(info$type, "nsx")) {
        stop("read_nsx_nev: path is not a valid nsx file (.ns1, .ns2, ..., .ns5): ", path)
      }

      progress$inc(sprintf("Parsing %s", basename(path)))

      filebase <- file.path(temp_path, paste0(basename(path), ".filearray"))

      re <- parse__nsx(path, info$config$specification,
                       header_only = header_only,
                       verbose = verbose, filebase = filebase,
                       force_update = force_update)
      # Post processing
      re <- blackrock_postprocess(nsx = re, nev = nev)

      if(ram && !header_only) {
        re$data <- re$data[drop = FALSE]
      }
      re
    }),
    names = fnames
  )


  list(
    nev = nev,
    nsx = nsx
  )
}
