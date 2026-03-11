#!/usr/bin/env Rscript
# grep_source_file.R — Search module source file with context
#
# Usage:
#   Rscript grep_source_file.R <module_id> --file=<path> --pattern=<regex> [--before=5] [--after=20]
#
# Examples:
#   Rscript grep_source_file.R notch_filter --file=R/module_server.R --pattern="bindEvent"
#   Rscript grep_source_file.R notch_filter --file=main.Rmd --pattern="export" --before=2 --after=10
#   Rscript grep_source_file.R power_explorer --file=R/module_html.R --pattern="sliderInput"
#
# Output format:
#   lineno: source line (matching lines marked with >)
#
# Only reads source files that are safe to read (privacy protected).

source("scripts/_common.R", local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript grep_source_file.R <module_id> --file=<path> --pattern=<regex> [--before=5] [--after=20]")
}

module_id <- args[[1L]]
file_arg <- parse_named_arg(args, "file")
pattern_arg <- parse_named_arg(args, "pattern")
before_arg <- parse_named_arg(args, "before")
after_arg <- parse_named_arg(args, "after")

if (is.null(file_arg)) {
  stop("--file=<path> is required. Example: --file=R/module_server.R")
}
if (is.null(pattern_arg)) {
  stop("--pattern=<regex> is required. Example: --pattern=bindEvent")
}

n_before <- if (is.null(before_arg)) 5L else as.integer(before_arg)
n_after <- if (is.null(after_arg)) 20L else as.integer(after_arg)

module_path <- get_module_path(module_id)
result <- grep_file_lines(module_path, file_arg, pattern_arg, n_before, n_after)

cat(sprintf("## grep '%s' in %s/%s\n\n", pattern_arg, module_id, file_arg))

if (length(result$matches) == 0L) {
  cat("No matches found.\n")
} else {
  cat(sprintf("Found %d match(es) at line(s): %s\n\n",
              length(result$matches),
              paste(result$matches, collapse = ", ")))
  
  for (i in seq_along(result$regions)) {
    region <- result$regions[[i]]
    cat(sprintf("--- Match %d (line %d) ---\n", i, region$match_line))
    
    line_nums <- seq(region$start, region$end)
    width <- nchar(as.character(max(line_nums)))
    
    for (j in seq_along(region$lines)) {
      ln <- line_nums[j]
      line <- region$lines[j]
      marker <- if (ln == region$match_line) ">" else " "
      cat(sprintf("%s%*d: %s\n", marker, width, ln, line))
    }
    cat("\n")
  }
}
