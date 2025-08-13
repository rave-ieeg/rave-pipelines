byte_size_lut <- list(
  "uint8" = 1, "int8" = 1,
  "uint16" = 2, "int16" = 2,
  "uint32" = 4, "int32" = 4,
  "uint64" = 8, "int64" = 8,
  "float" = 4, "double" = 8
)

IMPORT_FORMATS <- list(
  `.mat/.h5 file per electrode per block` = "native_matlab",
  `Single .mat/.h5 file per block` = "native_matlab2",
  `Single EDF(+) file per block` = "native_edf",
  `Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block` = "native_brainvis",
  `BIDS & EDF(+)` = "bids_edf",
  `BIDS & BrainVision (.vhdr+.eeg, .vhdr+.dat)` = "bids_brainvis",
  `Single BlackRock file (.nev+.nsx) per block` = "native_blackrock"
)

SIGNAL_TYPES <- c("LFP", "Spike", "EKG", "Auxiliary", "Unknown")

MNI305_to_MNI152 <- ravecore::MNI305_to_MNI152
rave_brain <- ravecore::rave_brain
restore_subject_instance <- ravecore:::restore_subject_instance

catgl <- ravepipeline:::catgl
dir_create2 <- ravepipeline::dir_create2
load_yaml <- ravepipeline::load_yaml
save_yaml <- ravepipeline::save_yaml
RAVEPreprocessSettings <- ravecore::RAVEPreprocessSettings
RAVESubject <- ravecore::RAVESubject
raveio_getopt <- ravepipeline::raveio_getopt
raveio_setopt <- ravepipeline::raveio_setopt
read_mat <- ravecore:::read_mat
safe_write_csv <- ravecore:::safe_write_csv
save_meta2 <- ravecore::save_meta2
stopifnot2 <- ravecore:::stopifnot2

with_future_parallel <- function(expr, env = parent.frame(), quoted = FALSE,
         on_failure = 'multisession', max_workers = NA,
         ...){
  if(!quoted){
    expr <- substitute(expr)
  }
  if(!is.na(max_workers) && max_workers >= 1){
    max_workers <- min(as.integer(max_workers), raveio_getopt("max_worker", 1L))
  } else {
    max_workers <- raveio_getopt("max_worker", 1L)
  }
  auto_parallel_old <- getOption("raveio.auto.parallel", default = TRUE)
  options("raveio.auto.parallel" = FALSE)
  dipsaus::make_forked_clusters(
    workers = max_workers,
    on_failure = on_failure, clean = FALSE, ...
  )
  on.exit({
    asNamespace("future")$plan("sequential")
    options("raveio.auto.parallel" = auto_parallel_old)
  }, add = TRUE, after = FALSE)

  re <- eval(expr, envir = env)
  asNamespace("future")$plan("sequential")
  options("raveio.auto.parallel" = auto_parallel_old)

  re
}
lapply_async <- ravetools:::lapply_async
save_h5 <- function(x, file, name, chunk = 'auto', level = 4, replace = TRUE,
                    new_file = FALSE, ctype = NULL, quiet = FALSE, ...){
  ieegio::io_write_h5(
    x = x,
    file = file,
    name = name,
    chunk = chunk,
    level = level,
    replace = replace,
    new_file = new_file,
    ctype = ctype,
    quiet = quiet,
    ...
  )
}
load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE){
  return(ieegio::io_read_h5(
    file = file,
    name = name,
    read_only = read_only,
    ram = ram,
    quiet = quiet
  ))
}


is.blank <- function(x) {
  !nzchar(x)
}


is_valid_ish <- function(x, min_len = 1, max_len = Inf, mode = NA,
         na = TRUE, blank = FALSE, all = FALSE){
  if(!is.na(mode) && mode(x) != mode){
    return(FALSE)
  }
  len <- length(x)
  if(len < min_len || len > max_len){
    return(FALSE)
  }
  if(len == 0){
    return(TRUE)
  }
  if(na){
    if(!all && any(is.na(x))){
      return(FALSE)
    }
    if(all && all(!is.na(x))){
      return(FALSE)
    }
  }
  if(blank && mode(x) == 'character'){
    if(!all && any(is.blank(x), na.rm = TRUE)){
      return(FALSE)
    }
    if(all && all(!is.blank(x), na.rm = TRUE)){
      return(FALSE)
    }
  }
  return(TRUE)
}

