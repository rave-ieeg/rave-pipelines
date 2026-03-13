#!/usr/bin/env Rscript

# Search PubMed (NCBI) for papers
# Usage: Rscript search_pubmed.R "<query>" [max_results]

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L) {
  cat("Usage: Rscript search_pubmed.R \"<query>\" [max_results]\n")
  quit(status = 1L)
}

query <- args[[1L]]
max_results <- if (length(args) >= 2L) as.integer(args[[2L]]) else 5L

# Step 1: Search for PMIDs
search_url <- sprintf(

  "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=%s&retmax=%d&retmode=json",
  utils::URLencode(query, reserved = TRUE),
  max_results
)

browser_ua <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/131.0.0.0 Safari/537.36"
)

search_resp <- httr2::request(search_url) |>
  httr2::req_headers("User-Agent" = browser_ua) |>
  httr2::req_perform()

search_data <- httr2::resp_body_json(search_resp)
id_list <- search_data$esearchresult$idlist

if (length(id_list) == 0L) {
  cat("No results found for query:", query, "\n")
  quit(status = 0L)
}

ids <- paste(unlist(id_list), collapse = ",")

# Step 2: Fetch summaries for those PMIDs
summary_url <- sprintf(
  "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=%s&retmode=json",
  ids
)

summary_resp <- httr2::request(summary_url) |>
  httr2::req_headers("User-Agent" = browser_ua) |>
  httr2::req_perform()

summary_data <- httr2::resp_body_json(summary_resp)

results <- summary_data$result
uids <- unlist(results$uids)

cat(sprintf("PubMed search results for: \"%s\"\n", query))
cat(sprintf("Found %d result(s)\n", length(uids)))
cat(paste(rep("=", 60), collapse = ""), "\n\n")

for (uid in uids) {
  paper <- results[[uid]]

  title <- paper$title %||% "N/A"
  pub_date <- paper$pubdate %||% "N/A"
  source <- paper$source %||% "N/A"

  # Extract authors
  authors <- if (length(paper$authors) > 0L) {
    paste(vapply(paper$authors, function(a) a$name %||% "", character(1L)), collapse = ", ")
  } else {
    "N/A"
  }

  # Extract DOI from articleids
  doi <- "N/A"
  if (length(paper$articleids) > 0L) {
    for (aid in paper$articleids) {
      if (identical(aid$idtype, "doi")) {
        doi <- aid$value
        break
      }
    }
  }

  cat(sprintf("PMID: %s\n", uid))
  cat(sprintf("Title: %s\n", title))
  cat(sprintf("Authors: %s\n", authors))
  cat(sprintf("Journal: %s\n", source))
  cat(sprintf("Date: %s\n", pub_date))
  cat(sprintf("DOI: %s\n", doi))
  cat(paste(rep("-", 60), collapse = ""), "\n\n")
}
