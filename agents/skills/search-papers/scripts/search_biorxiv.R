#!/usr/bin/env Rscript

# Search bioRxiv for biology preprints
# Usage: Rscript search_biorxiv.R "<query>" [max_results]
#
# Scrapes the bioRxiv search page for results sorted by relevance.

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L) {
  cat("Usage: Rscript search_biorxiv.R \"<query>\" [max_results]\n")
  quit(status = 1L)
}

query <- args[[1L]]
max_results <- if (length(args) >= 2L) as.integer(args[[2L]]) else 5L

# Build search URL: terms joined with %252B (double-encoded +)
terms <- trimws(strsplit(query, "\\s+")[[1L]])
query_part <- paste(terms, collapse = "%252B")
search_url <- sprintf(
  "https://www.biorxiv.org/search/%s%%20numresults%%3A%d%%20sort%%3Arelevance-rank",
  query_part,
  max_results
)

browser_ua <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/131.0.0.0 Safari/537.36"
)

resp <- tryCatch(
  httr2::request(search_url) |>
    httr2::req_headers(
      "User-Agent" = browser_ua,
      "Accept" = "text/html,application/xhtml+xml",
      "Accept-Language" = "en-US,en;q=0.9"
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 2, backoff = ~ 5) |>
    httr2::req_perform(),
  error = function(e) {
    cat("Error: Failed to fetch bioRxiv search results: ", conditionMessage(e), "\n")
    quit(status = 1L)
  }
)

html <- httr2::resp_body_string(resp)

# Parse search results from HTML using highwire-cite structure
# Each result is in a <li class="... search-result ..."> block
result_blocks <- regmatches(
  html,
  gregexpr('(?s)<li class="[^"]*search-result[^"]*">.*?</li>', html, perl = TRUE)
)[[1L]]

if (length(result_blocks) == 0L) {
  cat("No results found for query:", query, "\n")
  quit(status = 0L)
}

# Helper to extract text content from an HTML tag by class
extract_by_class <- function(text, class_name) {
  pattern <- sprintf('(?s)class="[^"]*%s[^"]*"[^>]*>(.*?)</(?:span|div|a)>', class_name)
  m <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  if (length(m) == 0L || !nzchar(m)) return("N/A")
  # Strip inner HTML tags
  content <- sub(pattern, "\\1", m, perl = TRUE)
  content <- gsub("<[^>]+>", "", content)
  trimws(gsub("\\s+", " ", content))
}

cat(sprintf("bioRxiv search results for: \"%s\"\n", query))
cat(sprintf("Found %d result(s)\n", length(result_blocks)))
cat(paste(rep("=", 60), collapse = ""), "\n\n")

for (block in result_blocks) {
  # Title: from <span class="highwire-cite-title">
  title <- extract_by_class(block, "highwire-cite-title")

  # Authors: from <div class="highwire-cite-authors">
  # Extract individual author names
  author_names <- regmatches(
    block,
    gregexpr('(?s)<span class="nlm-(?:given-names|surname)">(.*?)</span>', block, perl = TRUE)
  )[[1L]]
  if (length(author_names) > 0L) {
    name_parts <- sub('(?s)<span class="nlm-(?:given-names|surname)">(.*?)</span>', "\\1", author_names, perl = TRUE)
    # Pair given-names and surnames
    authors <- character(0L)
    i <- 1L
    while (i <= length(name_parts)) {
      if (i + 1L <= length(name_parts)) {
        authors <- c(authors, paste(name_parts[i], name_parts[i + 1L]))
        i <- i + 2L
      } else {
        authors <- c(authors, name_parts[i])
        i <- i + 1L
      }
    }
    # Also check for collab names
    collabs <- regmatches(
      block,
      gregexpr('(?s)<span class="nlm-collab"[^>]*>(.*?)</span>', block, perl = TRUE)
    )[[1L]]
    if (length(collabs) > 0L) {
      collab_names <- sub('(?s)<span class="nlm-collab"[^>]*>(.*?)</span>', "\\1", collabs, perl = TRUE)
      authors <- c(authors, collab_names)
    }
    authors_str <- paste(authors, collapse = ", ")
  } else {
    authors_str <- "N/A"
  }

  # DOI: from <span class="highwire-cite-metadata-doi">
  doi_match <- regmatches(
    block,
    regexpr("https://doi\\.org/[^\\s<]+", block, perl = TRUE)
  )
  doi <- if (length(doi_match) > 0L && nzchar(doi_match)) {
    trimws(sub("https://doi\\.org/", "", doi_match))
  } else {
    "N/A"
  }

  cat(sprintf("Title: %s\n", title))
  cat(sprintf("Authors: %s\n", authors_str))
  cat(sprintf("DOI: %s\n", doi))
  if (doi != "N/A") {
    cat(sprintf("URL: https://doi.org/%s\n", doi))
  }
  cat(paste(rep("-", 60), collapse = ""), "\n\n")
}
