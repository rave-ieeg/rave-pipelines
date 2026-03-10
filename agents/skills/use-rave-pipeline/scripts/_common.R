# _common.R — Shared utilities for use-rave-pipeline skill scripts
# Source this at the top of each script:
#   source("scripts/_common.R", local = TRUE)

# -- Resolve project root --
# wd is skill root (use-rave-pipeline/), 3 levels up
find_project_root <- function() {
  root <- normalizePath(
    file.path(getwd(), "..", "..", ".."),
    mustWork = FALSE
  )
  if (dir.exists(file.path(root, "modules"))) {
    return(root)
  }
  stop("Cannot find project root (no 'modules/' directory found)")
}

# -- Get a ravepipeline pipeline instance for a module --
get_pipeline <- function(module_id) {
  root <- find_project_root()
  ravepipeline::pipeline(
    pipeline_name = module_id,
    paths = file.path(root, "modules"), 
    # Do not mess up registry
    temporary = TRUE
  )
}

# -- Parse --key=value arguments from CLI args --
parse_kv_args <- function(args) {
  kv <- list()
  for (arg in args) {
    if (!grepl("^--", arg)) next
    arg <- sub("^--", "", arg)
    eq_pos <- regexpr("=", arg, fixed = TRUE)
    if (eq_pos < 1L) next
    key <- substring(arg, 1L, eq_pos - 1L)
    val <- substring(arg, eq_pos + 1L)
    kv[[key]] <- coerce_value(val)
  }
  kv
}

# -- Parse a single --key=value by key name --
parse_named_arg <- function(args, name) {
  pattern <- sprintf("^--%s=", name)
  for (arg in args) {
    if (grepl(pattern, arg)) {
      return(sub(pattern, "", arg))
    }
  }
  NULL
}

# -- Parse comma-separated --key=value --
parse_csv_arg <- function(args, name) {
  val <- parse_named_arg(args, name)
  if (is.null(val)) return(NULL)
  strsplit(val, ",", fixed = TRUE)[[1L]]
}

# -- Type coercion for setting values --
coerce_value <- function(val) {
  if (tolower(val) %in% c("true", "yes")) return(TRUE)
  if (tolower(val) %in% c("false", "no")) return(FALSE)
  parts <- strsplit(val, ",", fixed = TRUE)[[1L]]
  nums <- suppressWarnings(as.numeric(parts))
  if (!any(is.na(nums))) return(nums)
  if (length(parts) > 1L) return(parts)
  val
}
