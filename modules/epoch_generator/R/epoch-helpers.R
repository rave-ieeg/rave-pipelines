# Helpers without a Shiny session: shared by the threshold observer, the
# read-only scripts for agents (`epoch_channel_info`, `threshold_scan`,
# `epoch_status`), and `save_agent_epoch`

# Onset times (seconds) where `signal` crosses `threshold` (`direction` is
# "Above" or "Below"). Crossing runs separated by gaps of at most
# `num_duration` seconds are merged; each onset is the start of a run
find_threshold_onsets <- function(signal, threshold, direction,
                                  num_duration, sample_rate) {
  if (isTRUE(direction == "Below")) {
    breaks <- which(signal <= threshold)
  } else {
    breaks <- which(signal >= threshold)
  }
  # nothing crosses the threshold
  if (!length(breaks)) { return(numeric(0)) }
  # Crossings at most `max_lag` samples apart form one run; an onset is the
  # (0-based) start of a run. (`dipsaus::deparse_svec` gave the same runs, but
  # printed indexes such as 1e+05 in scientific notation, which turned the
  # end of such a run into a bogus onset)
  max_lag <- max(num_duration * sample_rate, 1)
  starts <- breaks[c(TRUE, diff(breaks) > max_lag)]
  (starts - 1) / sample_rate
}

# Length of the iEEG recording of a block, from the first imported
# electrode (the same electrode the module uses to estimate the sample rate
# of MATLAB files); NULL when it cannot be read
ieeg_block_info <- function(subject, block) {
  tryCatch({
    if (length(block) != 1) { return(NULL) }
    imported <- subject$electrodes[subject$preprocess_settings$data_imported]
    if (!length(imported)) { return(NULL) }
    e <- imported[[1]]
    efile <- file.path(subject$preprocess_path, "voltage",
                       sprintf("electrode_%s.h5", e))
    name <- sprintf("raw/%s", block)
    if (!file.exists(efile) || !ieegio::io_h5_valid(efile) ||
        !name %in% gsub("^/", "", ieegio::io_h5_names(efile))) {
      return(NULL)
    }
    samples <- length(ieegio::io_read_h5(efile, name, ram = FALSE))
    sample_rate <- subject$raw_sample_rates[subject$electrodes == e]
    # old subjects may have no rate (NA)
    if (length(sample_rate) != 1 || !isTRUE(sample_rate > 0)) { return(NULL) }
    list(electrode = e, samples = samples, sample_rate = sample_rate,
         duration = samples / sample_rate)
  }, error = function(e) { NULL })
}

# How agents should treat the sample rate of a trigger file
sample_rate_rule <- function(type, extension) {
  if (isTRUE(type %in% c("EDF", "NEV"))) {
    return("Sample rate: read from the file header. Tell the user the value.")
  }
  if (identical(type, "Imported")) {
    return("Sample rate: the imported channel's rate. Tell the user the value.")
  }
  if (identical(extension, "h5")) {
    return(paste(
      "Sample rate: the module reads none from HDF5 files; the field shows the",
      "duration estimate, or the previous file's value when it was above 1.",
      "Tell the user the value and how it was obtained; confirming is",
      "optional, ask when unsure."
    ))
  }
  paste(
    "Sample rate: REQUIRED from the user for MATLAB files. The field shows the",
    "duration estimate, or the previous file's value when it was above 1. Tell",
    "the user the value and ask them to confirm or correct it, then set",
    "`sample_rate`."
  )
}

# Onset counts over 13 thresholds between the median and the extreme of the
# signal (max for "Above", min for "Below")
scan_thresholds <- function(signal, direction, num_duration, sample_rate,
                            fractions = c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2,
                                          0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
  center <- stats::median(signal, na.rm = TRUE)
  if (isTRUE(direction == "Below")) {
    levels <- center - (center - min(signal, na.rm = TRUE)) * fractions
  } else {
    levels <- center + (max(signal, na.rm = TRUE) - center) * fractions
  }
  levels <- unique(signif(levels, 4))
  rows <- lapply(levels, function(level) {
    onsets <- find_threshold_onsets(signal, level, direction,
                                    num_duration, sample_rate)
    gaps <- diff(onsets)
    data.frame(
      threshold = level,
      onsets = length(onsets),
      median_gap = if (length(gaps)) stats::median(gaps) else NA_real_,
      min_gap = if (length(gaps)) min(gaps) else NA_real_
    )
  })
  do.call("rbind", rows)
}

# Gaps between consecutive onsets, and the unusual ones
summarize_onset_gaps <- function(times) {
  if (length(times) < 2) {
    return("Gaps between onsets: fewer than 2 onsets.")
  }
  gaps <- diff(times)
  med <- stats::median(gaps)
  re <- sprintf("Gaps between onsets (s): min %.3f, median %.3f, max %.3f.",
                min(gaps), med, max(gaps))
  unusual <- which(gaps < 0.5 * med | gaps > 1.5 * med)
  if (length(unusual)) {
    shown <- utils::head(unusual, 8)
    re <- c(re, sprintf(
      "Unusual gaps (under 0.5x or over 1.5x the median): %s%s.",
      paste(sprintf("#%d-#%d %.3f", shown, shown + 1, gaps[shown]),
            collapse = ", "),
      if (length(unusual) > length(shown)) {
        sprintf(", and %d more", length(unusual) - length(shown))
      } else ""
    ))
  }
  re
}

# Numeric scalars that look like a sample rate (e.g. `SampleRate`, `srate`,
# `fs`) among the variables of a MATLAB/HDF5 file or the fields of its
# structs. Only a hint: the user still confirms the rate
find_rate_hints <- function(header, max_hints = 5) {
  pattern <- "^(sample[._ ]?rate|sampling[._ ]?(rate|freq|frequency)|srate|fs)$"
  as_scalar <- function(x) {
    x <- tryCatch({
      if (inherits(x, "LazyH5")) {
        if (length(x) != 1) { return(NULL) }
        x <- x[]
      }
      x
    }, error = function(e) { NULL })
    if (is.numeric(x) && length(x) == 1 && is.finite(x)) {
      return(as.numeric(x))
    }
    NULL
  }
  # field names of a variable: an R.matlab struct keeps them as dimnames
  field_names <- function(x) {
    if (inherits(x, "fastmap2") || is.list(x)) {
      nms <- names(x)
      if (!length(nms) && length(dimnames(x))) { nms <- dimnames(x)[[1]] }
      return(nms)
    }
    NULL
  }
  get_field <- function(x, nm) {
    if (inherits(x, "fastmap2")) { return(x[[nm]]) }
    nms <- field_names(x)
    x[[which(nms == nm)[[1]]]]
  }
  hints <- character(0)
  tryCatch({
    for (nm in names(header)) {
      if (length(hints) >= max_hints) { break }
      x <- header[[nm]]
      if (grepl(pattern, nm, ignore.case = TRUE)) {
        v <- as_scalar(x)
        if (length(v)) { hints <- c(hints, sprintf("%s = %s", nm, format(v))) }
        next
      }
      for (field in field_names(x)) {
        if (!length(field) || !grepl(pattern, field, ignore.case = TRUE)) { next }
        v <- as_scalar(get_field(x, field))
        if (length(v)) {
          hints <- c(hints, sprintf("%s$%s = %s", nm, field, format(v)))
        }
      }
    }
  }, error = function(e) { NULL })
  utils::head(hints, max_hints)
}

# Write the sidecar of an agent-generated epoch: comment lines, then YAML
write_epoch_sidecar <- function(path, info, comments = character(0)) {
  lines <- c(paste("#", comments), yaml::as.yaml(info))
  writeLines(lines, path)
  invisible(path)
}

# The newest sidecars of agent-generated epochs in a project: which files,
# variables and sample rates earlier epochs used (event channels usually
# keep the same settings within a project)
find_agent_epoch_sidecars <- function(project_name, n = 5) {
  tryCatch({
    project <- ravecore::as_rave_project(project_name)
    files <- unlist(lapply(project$subjects(), function(subject_code) {
      meta_path <- tryCatch({
        ravecore::RAVESubject$new(project_name = project_name,
                                  subject_code = subject_code,
                                  strict = FALSE)$meta_path
      }, error = function(e) { NULL })
      if (length(meta_path) != 1 || !dir.exists(meta_path)) { return(NULL) }
      fs <- list.files(meta_path, full.names = TRUE,
                       pattern = "^epoch_agent_generated_.*_description\\.txt$")
      names(fs) <- rep(subject_code, length(fs))
      fs
    }))
    if (!length(files)) { return(list()) }
    files <- files[order(file.mtime(files), decreasing = TRUE)]
    files <- utils::head(files, n)
    lapply(seq_along(files), function(ii) {
      info <- tryCatch(yaml::read_yaml(files[[ii]]), error = function(e) { NULL })
      list(subject = names(files)[[ii]], file = basename(files[[ii]]),
           info = info)
    })
  }, error = function(e) { list() })
}

# One line per earlier sidecar: subject, epoch, and the variables and sample
# rates of its stagings (at most 3 per line)
format_agent_epoch_sidecars <- function(sidecars) {
  vapply(sidecars, function(item) {
    tryCatch({
      info <- item$info
      if (!is.list(info)) { info <- list() }
      blocks <- Filter(is.list, as.list(info$blocks))
      used <- unlist(lapply(blocks, function(block) {
        actions <- Filter(function(a) {
          is.list(a) && identical(a$action, "staged")
        }, as.list(block$actions))
        vapply(actions, function(a) {
          sprintf("%s %s Hz", a$variable %||% "?", format(a$sample_rate %||% "?"))
        }, "")
      }))
      used <- unique(used)
      sprintf(
        "%s: %s (%s)", item$subject, info$epoch_name %||% item$file,
        if (length(used)) {
          paste(c(utils::head(used, 3), if (length(used) > 3) "..."), collapse = "; ")
        } else "no staging recorded"
      )
    }, error = function(e) {
      sprintf("%s: %s (unreadable)", item$subject, item$file)
    })
  }, "")
}
