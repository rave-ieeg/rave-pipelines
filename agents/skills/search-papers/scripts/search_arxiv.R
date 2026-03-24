#!/usr/bin/env Rscript

# Search arXiv for preprints
# Usage: Rscript search_arxiv.R "<query>" [max_results]

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L) {
  cat("Usage: Rscript search_arxiv.R \"<query>\" [max_results]\n")
  quit(status = 1L)
}

query <- args[[1L]]
max_results <- if (length(args) >= 2L) as.integer(args[[2L]]) else 5L

# Build arXiv search query: combine terms with AND
terms <- trimws(strsplit(query, "\\s+")[[1L]])
arxiv_query <- paste(sprintf("all:%s", terms), collapse = "+AND+")

search_url <- sprintf(

  "https://export.arxiv.org/api/query?search_query=%s&start=0&max_results=%d&sortBy=relevance&sortOrder=descending",
  arxiv_query,
  max_results
)

browser_ua <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/131.0.0.0 Safari/537.36"
)

resp <- httr2::request(search_url) |>
  httr2::req_headers("User-Agent" = browser_ua) |>
  httr2::req_perform()

xml_text <- httr2::resp_body_string(resp)

# Parse entries from Atom XML using base R regex
# Entries span multiple lines, so use (?s) flag for dotall mode
entries <- regmatches(xml_text, gregexpr("(?s)<entry>(.*?)</entry>", xml_text, perl = TRUE))[[1L]]

if (length(entries) == 0L) {
  cat("No results found for query:", query, "\n")
  quit(status = 0L)
}

# Helper to extract a single XML tag value (dotall mode for multiline content)
extract_tag <- function(text, tag) {
  pattern <- sprintf("(?s)<%s[^>]*>(.*?)</%s>", tag, tag)
  m <- regmatches(text, regexpr(pattern, text, perl = TRUE))
  if (length(m) == 0L || !nzchar(m)) return("N/A")
  sub(sprintf("(?s)<%s[^>]*>(.*?)</%s>", tag, tag), "\\1", m, perl = TRUE)
}

# Helper to extract all author names
extract_authors <- function(text) {
  names <- regmatches(text, gregexpr("(?s)<name>(.*?)</name>", text, perl = TRUE))[[1L]]
  if (length(names) == 0L) return("N/A")
  authors <- sub("(?s)<name>(.*?)</name>", "\\1", names, perl = TRUE)
  paste(trimws(authors), collapse = ", ")
}

# Helper to extract arXiv ID from the <id> tag
extract_arxiv_id <- function(text) {
  id_val <- extract_tag(text, "id")
  sub("https?://arxiv\\.org/abs/", "", trimws(id_val))
}

# Helper to extract DOI if present (from arxiv:doi tag or doi link)
extract_doi <- function(text) {
  # Try arxiv:doi tag first
  m <- regmatches(text, regexpr("(?s)<arxiv:doi[^>]*>(.*?)</arxiv:doi>", text, perl = TRUE))
  if (length(m) > 0L && nzchar(m)) {
    return(trimws(sub("(?s)<arxiv:doi[^>]*>(.*?)</arxiv:doi>", "\\1", m, perl = TRUE)))
  }
  # Try doi link
  m <- regmatches(text, regexpr('href="https?://doi\\.org/([^"]+)"', text, perl = TRUE))
  if (length(m) > 0L && nzchar(m)) {
    return(sub('href="https?://doi\\.org/([^"]+)"', "\\1", m, perl = TRUE))
  }
  "N/A"
}

cat(sprintf("arXiv search results for: \"%s\"\n", query))
cat(sprintf("Found %d result(s)\n", length(entries)))
cat(paste(rep("=", 60), collapse = ""), "\n\n")

for (entry in entries) {
  title <- extract_tag(entry, "title")
  title <- gsub("\\s+", " ", trimws(title))
  authors <- extract_authors(entry)
  published <- extract_tag(entry, "published")
  published <- sub("T.*", "", trimws(published))
  arxiv_id <- extract_arxiv_id(entry)
  doi <- extract_doi(entry)

  cat(sprintf("arXiv ID: %s\n", arxiv_id))
  cat(sprintf("Title: %s\n", title))
  cat(sprintf("Authors: %s\n", authors))
  cat(sprintf("Published: %s\n", published))
  cat(sprintf("DOI: %s\n", doi))
  cat(sprintf("URL: https://arxiv.org/abs/%s\n", arxiv_id))
  cat(paste(rep("-", 60), collapse = ""), "\n\n")
}
