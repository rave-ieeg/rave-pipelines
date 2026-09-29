#!/usr/bin/env Rscript
# Install / upgrade the shidashi MCP proxy and wire it into .vscode/mcp.json.
#
# The proxy (`mcp-proxy.mjs`) is a stdio MCP server that lets Positron / VS Code
# agents drive a running shidashi (RAVE) app over MCP. `shidashi::setup_mcp_proxy()`
# copies the proxy into the shidashi cache folder and returns its path; this
# script then makes sure the workspace `.vscode/mcp.json` has a `shidashi` server
# pointing at it. Re-run any time to upgrade the proxy to the installed shidashi
# version.
#
# Usage:
#   Rscript agents/skills/build-module-mcp/setup-mcp-proxy.R [project_root]
#
# `project_root` defaults to the repository this script lives in. After running,
# reload the MCP servers in Positron / VS Code so the tools refresh.

args <- commandArgs(trailingOnly = TRUE)

# ---- locate the project root ----------------------------------------------
find_script_path <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) normalizePath(f[[1]], mustWork = FALSE) else NA_character_
}

project_root <- if (length(args) >= 1 && nzchar(args[[1]])) {
  normalizePath(args[[1]], mustWork = TRUE)
} else {
  script <- find_script_path()
  if (!is.na(script)) {
    # <root>/agents/skills/build-module-mcp/setup-mcp-proxy.R -> three up
    normalizePath(file.path(dirname(script), "..", "..", ".."))
  } else {
    normalizePath(getwd())
  }
}

# ---- install / upgrade the proxy ------------------------------------------
if (!requireNamespace("shidashi", quietly = TRUE)) {
  stop("Package 'shidashi' is not installed. Install it, then re-run this script.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install it, then re-run this script.")
}

# `setup_mcp_proxy()` is an internal shidashi helper (not exported).
setup_mcp_proxy <- tryCatch(
  utils::getFromNamespace("setup_mcp_proxy", "shidashi"),
  error = function(e) {
    stop(
      "This shidashi version has no `setup_mcp_proxy()`; upgrade shidashi ",
      "(remotes::install_github('dipterix/shidashi')), then re-run."
    )
  }
)

dest <- setup_mcp_proxy(overwrite = TRUE, verbose = FALSE)
if (is.null(dest) || !nzchar(dest) || !file.exists(dest)) {
  stop("setup_mcp_proxy() did not return a proxy path; check the shidashi install.")
}
dest <- normalizePath(dest)

# ---- merge into .vscode/mcp.json ------------------------------------------
vscode_dir <- file.path(project_root, ".vscode")
dir.create(vscode_dir, showWarnings = FALSE, recursive = TRUE)
mcp_path <- file.path(vscode_dir, "mcp.json")

config <- if (file.exists(mcp_path)) {
  tryCatch(
    jsonlite::read_json(mcp_path, simplifyVector = FALSE),
    error = function(e) list()
  )
} else {
  list()
}
if (is.null(config$servers)) {
  config$servers <- list()
}

# Preserve any other servers; (re)point the `shidashi` server at the proxy.
config$servers$shidashi <- list(
  type = "stdio",
  command = "node",
  args = list(dest)
)

jsonlite::write_json(
  config, mcp_path,
  auto_unbox = TRUE, pretty = TRUE, null = "null"
)

message("Installed shidashi MCP proxy:\n  ", dest)
message("Wired `shidashi` server into:\n  ", mcp_path)
message("\nReload the MCP servers in Positron / VS Code to pick up the changes.")
