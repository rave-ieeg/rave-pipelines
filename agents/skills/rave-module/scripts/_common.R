# _common.R — Shared utilities for rave-module skill scripts
# Source this at the top of each script:
#   source("scripts/_common.R", local = TRUE)

# -- Resolve project root --
# wd is skill root (rave-module/), 3 levels up
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

# -- Get module directory path --
get_module_path <- function(module_id) {
  root <- find_project_root()
  module_path <- file.path(root, "modules", module_id)
  if (!dir.exists(module_path)) {
    stop(sprintf("Module '%s' not found at: %s", module_id, module_path))
  }
  module_path
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

# ============================================================================
# Source file reading utilities (privacy-protected)
# ============================================================================

# -- Canonical file patterns (allowed to read) --
CANONICAL_PATTERNS <- c(
  # Root-level source files
  "^DESCRIPTION$",
  "^CITATION$",
  "^RAVE-CONFIG$",
  "^main\\.Rmd$",
  "^main\\.html$",
  "^server\\.R$",
  "^configure\\.R$",
  "^common\\.R$",
  "^debug\\.R$",
  "^migrate\\.R$",
  "^module-ui\\.html$",
  "^_targets\\.yaml$",
  "^agents\\.yaml$",
  "^fork-policy$",
  # Report files
  "^report-.*\\.Rmd$",
  "^report-.*\\.html$",
  "^report-list\\.yaml$",
  "^report_styles\\.css$",
  "^report_script\\.js$",
  # Make files
  "^make-.*\\.R$",
  # R source directory
  "^R/.*\\.R$",
  # Python source directory
  "^py/.*\\.py$",
  # Shared utilities
  "^shared/.*\\.R$",
  "^shared/.*\\.py$",
  # Preferences (defaults, not user data)
  "^preferences/.*\\.yaml$",
  "^preferences/.*\\.json$"
)

# Files explicitly blocked (even if they match patterns)
BLOCKED_FILES <- c(
  "settings.yaml",
  ".gitignore",
  ".DS_Store"
)

# Directories blocked from reading
BLOCKED_DIRS <- c(
  "_targets",
  "data",
  "results",
  ".git"
)

# -- Check if a file path is canonical (safe to read) --
is_canonical_file <- function(rel_path) {
  basename_path <- basename(rel_path)
  if (basename_path %in% BLOCKED_FILES) {
    return(FALSE)
  }
  path_parts <- strsplit(rel_path, "/", fixed = TRUE)[[1]]
  if (any(path_parts %in% BLOCKED_DIRS)) {
    return(FALSE)
  }
  for (pattern in CANONICAL_PATTERNS) {
    if (grepl(pattern, rel_path)) {
      return(TRUE)
    }
  }
  FALSE
}

# -- List all canonical files in a module --
list_canonical_files <- function(module_path) {
  all_files <- list.files(
    module_path,
    recursive = TRUE,
    all.files = FALSE,
    include.dirs = FALSE
  )
  canonical <- all_files[sapply(all_files, is_canonical_file)]
  sort(canonical)
}

# -- Read file lines with line numbers --
read_file_lines <- function(module_path, rel_path, start = 1L, nlines = 200L) {
  if (!is_canonical_file(rel_path)) {
    stop(sprintf(
      "File '%s' is not a canonical source file (privacy protection).",
      rel_path
    ))
  }
  full_path <- file.path(module_path, rel_path)
  if (!file.exists(full_path)) {
    stop(sprintf("File not found: %s", rel_path))
  }
  
  all_lines <- readLines(full_path, warn = FALSE)
  total <- length(all_lines)
  
  start <- max(1L, as.integer(start))
  if (start > total) {
    return(list(
      lines = character(0L),
      start = start,
      end = start - 1L,
      total = total
    ))
  }
  
  end <- min(total, start + as.integer(nlines) - 1L)
  lines <- all_lines[start:end]
  
  list(
    lines = lines,
    start = start,
    end = end,
    total = total
  )
}

# -- Truncate a single line if it exceeds max_chars --
truncate_long_line <- function(line, max_chars = 200L) {
  nc <- nchar(line)
  if (nc > max_chars) {
    paste0(substring(line, 1L, max_chars), "... (truncated ", nc - max_chars, " chars)")
  } else {
    line
  }
}

# -- Format lines with line numbers --
format_numbered_lines <- function(lines, start) {
  if (length(lines) == 0L) return(character(0L))
  line_nums <- seq(start, start + length(lines) - 1L)
  width <- nchar(as.character(max(line_nums)))
  lines <- sapply(lines, truncate_long_line, USE.NAMES = FALSE)
  sprintf("%*d: %s", width, line_nums, lines)
}

# -- Grep file with context --
grep_file_lines <- function(module_path, rel_path, pattern, n_before = 5L, n_after = 20L) {
  if (!is_canonical_file(rel_path)) {
    stop(sprintf(
      "File '%s' is not a canonical source file (privacy protection).",
      rel_path
    ))
  }
  full_path <- file.path(module_path, rel_path)
  if (!file.exists(full_path)) {
    stop(sprintf("File not found: %s", rel_path))
  }
  
  all_lines <- readLines(full_path, warn = FALSE)
  total <- length(all_lines)
  
  matches <- grep(pattern, all_lines, perl = TRUE)
  if (length(matches) == 0L) {
    return(list(
      matches = integer(0L),
      regions = list(),
      total = total
    ))
  }
  
  # Build regions around each match
  regions <- list()
  for (m in matches) {
    region_start <- max(1L, m - as.integer(n_before))
    region_end <- min(total, m + as.integer(n_after))
    regions[[length(regions) + 1L]] <- list(
      match_line = m,
      start = region_start,
      end = region_end,
      lines = all_lines[region_start:region_end]
    )
  }
  
  list(
    matches = matches,
    regions = regions,
    total = total
  )
}
