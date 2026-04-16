targets::tar_option_set(

  packages = c("ravepipeline")

)

`%OF%` <- ravepipeline:::`%OF%`
parse_svec <- ravepipeline:::parse_svec
deparse_svec <- ravepipeline:::deparse_svec
drop_nulls <- ravecore:::drop_nulls
all_formats <- ravecore::IMPORT_FORMATS
all_projects <- c("", ravecore::get_projects(refresh = TRUE), "[New Project]")
current_project <- pipeline$get_settings('project_name', "") %OF% all_projects

native_regexps <- c(
  native_matlab = "(\\.h5|\\.mat|_matlab|_hdf5)$",
  native_matlab2 = "\\.(h5|mat)$",
  native_edf = "\\.(edf)$",
  native_brainvis = "\\.(eeg|dat|vmrk|vhdr)$",
  native_blackrock = "\\.(nev|ns[1-6])$"
)

update_projects <- function(refresh = FALSE) {
  all_projects <<- c("", ravecore::get_projects(refresh = refresh), "[New Project]")
  all_projects
}

all_native_subjects <- function() {
  native_root <- ravepipeline::raveio_getopt("raw_data_dir")
  native_subjects <- list.dirs(native_root, full.names = FALSE, recursive = FALSE)
  native_subjects <- native_subjects[grepl("^[a-zA-Z0-9_-]+$", native_subjects)]
  sort(native_subjects)
}


clean_compose_setup <- function(compose_setup, electrodes = "") {
  electrodes <- parse_svec(electrodes)
  if(!is.list(compose_setup) || !length(compose_setup)) { return(list()) }

  new_channels <- NULL
  re <- lapply(compose_setup, function(item) {
    item$number <- as.integer(item$number)
    if(length(item$number) != 1 || is.na(item$number)) { return() }
    if(item$number <= 0) { stop("Invalid channel number: ", item$number) }
    if(item$number %in% c(electrodes, new_channels)) {
      stop("Cannot compose channel ", item$number, ". Reason: conflict with existing channels.")
    }
    from <- parse_svec(item$from)
    if(!length(from)) {
      stop("Cannot compose channel ", item$number, ". Reason: empty list of channels to compose from.")
    }
    if(!all(from %in% electrodes)) {
      from <- from[!from %in% electrodes]
      stop("Cannot compose channel ", item$number, ". Reason: Invalid `from` channel(s): ", deparse_svec(from))
    }
    weights <- item$weights
    if(is.character(weights)) {
      weights <- trimws(unlist(strsplit(weights, ",")))
      weights <- weights[!weights %in% '']
    }
    if(!length(weights)) {
      weights <- 1 / length(from)
    }
    weights <- as.numeric(weights)
    if(length(weights) == 1) {
      weights <- rep(weights, length(from))
    }
    if(length(from) != length(weights)) {
      stop("Cannot compose channel ", item$number, ". Reason: inequal size of `from` channels and weights.")
    }
    if(anyNA(weights) || any(weights == 0)) {
      stop("Cannot compose channel ", item$number, ". Reason: weights must be non-zero numbers.")
    }
    normalize <- isTRUE(item$normalize) || identical(item$normalize, "yes")
    list(
      number = item$number,
      from = from,
      weights = weights,
      normalize = normalize
    )
  })
  re <- drop_nulls(re)
  if(!length(re)) { return(list()) }
  re
}

# obtain signal files within blocks
find_block_files <- function(subject, blocks, format) {
  if(is.numeric(format) || grepl(" ", as.character(format))) {
    format <- ravecore::IMPORT_FORMATS[[format]]
  }
  regexp <- native_regexps[[format]]
  format_standard <- subject$preprocess_settings$raw_path2_type
  switch(
    format_standard,
    "bids" = {
      bids_subject <- bidsr::bids_subject(
        project = subject$`@impl`@project@parent_path,
        subject_code = subject$subject_code
      )
      bids_root <- bidsr::resolve_bids_path(bids_subject@project, storage = "raw")
      query_results <- bidsr::query_bids(
        bids_subject,
        search_params = list(
          storage = "raw",
          sidecars = FALSE,
          data_types = "ieeg"
        )
      )
      query_results$block <- ravecore:::block_names_from_bids_entities(query_results$parsed)

      block_files <- lapply(blocks, function(block) {
        block_files <- lapply(which(query_results$block == block), function(idx) {
          file.path(bids_root, format(query_results$parsed[[idx]]))
        })
        block_files <- unlist(block_files)
        if(length(block_files)) {
          signal_files <- block_files[grepl(regexp, block_files, ignore.case = TRUE)]
          channel_files <- block_files[endsWith(tolower(block_files), "channels.tsv")]
        }
        return(list(
          signal_files = signal_files,
          channel_files = channel_files
        ))
      })
      return(
        structure(
          block_files,
          names = blocks,
          standard = "bids"
        )
      )
    },
    {
      block_files <- lapply(blocks, function(block) {
        block_path <- file.path(subject$preprocess_settings$raw_path, block)
        signal_files <- list.files(
          block_path,
          pattern = regexp,
          all.files = FALSE,
          full.names = TRUE,
          recursive = FALSE,
          ignore.case = TRUE,
          include.dirs = FALSE
        )
        list(
          signal_files = signal_files
        )
      })
      return(
        structure(
          block_files,
          names = blocks,
          standard = "native"
        )
      )
    }
  )
}

