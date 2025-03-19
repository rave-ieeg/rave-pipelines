`%OF%` <- dipsaus::`%OF%`

find_bids_runs <- function(subject_path, BIDS_subject, BIDS_sessions) {
  if(missing(BIDS_sessions) || is.null(BIDS_sessions)) {
    candidates <- list.files(
      path = subject_path,
      recursive = TRUE,
      full.names = FALSE,
      all.files = FALSE,
      include.dirs = FALSE,
      no.. = TRUE,
      ignore.case = TRUE,
      pattern = sprintf(
        "^%s_.{0,}run-[0-9]+_(events|channels|ieeg)\\.(tsv|csv|json|vhdr|eeg|edf|pwb|vmrk|dat|ns[0-6]|nev|mat|h5)$",
        BIDS_subject
      )
    )
  } else {
    candidates <- unlist(lapply(BIDS_sessions, function(sess) {
      re <- list.files(
        path = file.path(subject_path, sess),
        recursive = TRUE,
        full.names = FALSE,
        all.files = FALSE,
        include.dirs = FALSE,
        no.. = TRUE,
        ignore.case = TRUE,
        pattern = sprintf(
          "^%s_.{0,}run-[0-9]+_(events|channels|ieeg)\\.(tsv|csv|json|vhdr|eeg|edf|pwb|vmrk|dat|ns[0-6]|nev|mat|h5)$",
          BIDS_subject
        )
      )
      file.path(sess, re)
    }))
  }

  candidates <- gsub("[/\\\\]+", "/", candidates)

  # get prefix
  prefix <- gsub("(run-[0-9]+)_.*$", "\\1", candidates)
  data_types <- gsub("^.*(run-[0-9]+)_", "", candidates)

  prefix <- unique(prefix)
  data_types <- unique(data_types)

  return(list(
    runs = prefix,
    data_types = data_types
  ))
}

get_BIDS_datasets <- function() {
  BIDS_root <- ravepipeline::raveio_getopt("bids_data_dir")
  dsets <- list.dirs(BIDS_root, full.names = FALSE, recursive = FALSE)
  dsets <- dsets[grepl("^[a-zA-Z0-9]", dsets)]
  dsets
}

get_BIDS_subject <- function(BIDS_dataset) {
  BIDS_root <- ravepipeline::raveio_getopt("bids_data_dir")
  subjects <- list.dirs(file.path(BIDS_root, BIDS_dataset), full.names = FALSE, recursive = FALSE)
  subjects <- subjects[grepl("^sub-", subjects)]
  subjects
}

get_BIDS_subject_sessions <- function(BIDS_dataset, BIDS_subject) {
  BIDS_root <- ravepipeline::raveio_getopt("bids_data_dir")
  sessions <- list.dirs(file.path(BIDS_root, BIDS_dataset, BIDS_subject), full.names = FALSE, recursive = FALSE)
  sessions <- sessions[grepl("^ses-", sessions)]
  sessions
}

suggust_block_name <- function(path) {

  get_value <- function(key) {
    re <- sprintf("%s-[^_/-]+[_/]", key)
    if(grepl(re, path)) {
      sidx <- gregexpr(re, path)[[1]]
      len <- attr(sidx, "match.length")
      sidx <- sidx[[1]]
      len <- len[[1]]
      val <- substr(path, sidx + nchar(key) + 1L, sidx + len - 2L)
      return(val)
    }
    re <- sprintf("%s-[^_/-]+$", key)
    if(grepl(re, path)) {
      sidx <- gregexpr(re, path)[[1]]
      len <- attr(sidx, "match.length")
      sidx <- sidx[[1]]
      len <- len[[1]]
      val <- substr(path, sidx + nchar(key) + 1L, sidx + len - 1L)
      return(val)
    }
    return("")
  }

  # get available keys
  path <- strsplit(path, split = "/|\\\\")[[1]]
  path <- path[[length(path)]]
  keywords <- strsplit(path, "_")[[1]]
  keywords <- keywords[grepl("^[a-zA-Z0-9]+\\-", keywords)]
  keys <- gsub("^([a-zA-Z0-9]+)\\-.*$", "\\1", keywords)
  keys <- keys[!keys %in% c("sub", "ses", "task", "run")]
  keys <- c("ses", "task", keys)

  prefix <- sapply(keys, function(key) {
    val <- get_value(key)
    if(nzchar(val)) {
      val <- sprintf("%s_", val)
    }
    return(val)
  })

  prefix <- paste0(unlist(prefix), collapse = "")

  if(grepl("^[^a-zA-Z]", prefix)) {
    prefix <- sprintf("block_%s", prefix)
  }

  run_str <- get_value("run")
  if(!nzchar(prefix) && nzchar(run_str)) {
    run_str <- sprintf("run%s", run_str)
  }

  sprintf("%s%s", prefix, run_str)
}

get_all_channels <- function(abspath, format = "BrainVision") {
  format <- match.arg(format)

  pdir <- dirname(abspath)
  all_files <- list.files(pdir, all.files = FALSE, full.names = FALSE, recursive = FALSE, include.dirs = FALSE)
  prefix <- basename(abspath)

  all_files <- all_files[startsWith(all_files, prefix)]

  switch(
    format,
    "BrainVision" = {
      header_file <- all_files[grepl("\\.vhdr$", all_files, ignore.case = TRUE)]
      header <- raveio::read_eeg_header(file.path(pdir, header_file))
      seq_len(nrow(header$channels))
    }, {
      stop("Unsupported format")
    }
  )
}
