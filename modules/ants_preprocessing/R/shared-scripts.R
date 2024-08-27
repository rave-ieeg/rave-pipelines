#' File whose name starts with `shared-` will be automatically
#' loaded by the pipeline. Hence you can put global static objects
#' here, for example,
#'   - define functions that may be used multiple times,
#'   - declare variables that can be re-used
#'   - import packages that will be used via `library()`, or `targets::tar_option_set` (see below)
NULL

rpymat::ensure_rpymat(verbose = FALSE)

