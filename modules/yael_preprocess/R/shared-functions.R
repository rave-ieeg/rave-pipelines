`%OF%` <- dipsaus::`%OF%`

FSL_COST_FUNCTIONS <- c(
  "mutualinfo", "leastsq", "normcorr",
  "corratio", "normmi", "labeldiff", "bbr"
)

path_is_valid <- function(path, dir_ok = FALSE) {
  if (length(path) == 1 && !is.na(path) && file.exists(path)) {
    if (dir.exists(path)) {
      if (dir_ok) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Full path of the raw MRI/CT chosen in the loader. The loader lists paths
# relative to the subject's raw folder and, for 'BIDS' projects, relative to
# the 'BIDS' raw folder; the raw folder is tried first
resolve_raw_image_path <- function(subject, path) {
  re <- file.path(subject$preprocess_settings$raw_path, path)
  if (path_is_valid(re, dir_ok = TRUE)) { return(re) }
  if (identical(subject$preprocess_settings$raw_path2_type, "bids")) {
    re_bids <- file.path(subject$preprocess_settings$raw_path2, path)
    if (path_is_valid(re_bids, dir_ok = TRUE)) { return(re_bids) }
  }
  re
}
