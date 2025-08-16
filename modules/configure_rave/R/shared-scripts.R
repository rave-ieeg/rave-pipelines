
get_basic_info <- function() {
  session_info <- utils::sessionInfo()

  blas <- session_info$BLAS
  if (is.null(blas)) { blas <- "" }
  lapack <- session_info$LAPACK
  if (is.null(lapack)) { lapack <- "" }

  raw_dir <- ravepipeline::raveio_getopt("raw_data_dir", default = "<Missing>")
  data_dir <- ravepipeline::raveio_getopt("data_dir", default = "<Missing>")
  cache_dir <- ravecore::cache_root()

  healthy_directory <- function(path){
    ifelse(dir.exists(path), "[healthy]", "[unable to reach]")
  }

  package_ver <- function(name, version_only = FALSE) {
    suppressWarnings({
      if(name %in% names(session_info$otherPkgs)) {
        desc <- session_info$otherPkgs[[name]]
      } else if(name %in% names(session_info$loadedOnly)) {
        desc <- session_info$loadedOnly[[name]]
      } else {
        desc <- utils::packageDescription(name, drop = TRUE)
      }
    })

    if(!inherits(desc, "packageDescription")) { return(NULL) }

    if(version_only) {
      return(desc$Version)
    }
    sprintf("%s [%s]", desc$Package, desc$Version)

  }


  # get basic information
  cat(
    sep = "",
    "Operating system:\n",
    "  OS type:         ", session_info$platform, "\n",
    "  OS name:         ", session_info$running, "\n",
    "  File separator:  ", .Platform$file.sep, "\n",
    "  Endianess:       ", .Platform$endian, "\n",

    "\nR information\n",
    "  Version:         ", session_info$R.version$version.string, "\n",
    "  Architecture:    ", session_info$R.version$arch, "\n",
    "  Matrix products: ", session_info$matprod, "\n",
    local({
      if (blas == lapack && nzchar(blas))
        c("  BLAS/LAPACK:     ", blas, "\n", sep = "")
      else {
        if (nzchar(blas)) {
          c("  BLAS:            ", blas, "\n", sep = "")
        }
        if (nzchar(lapack)) {
          c("  LAPACK:          ", lapack, "\n", sep = "")
        }
      }
    }),

    "\nRAVE status\n",
    "  Version:        ", package_ver("rave", version_only = TRUE), "\n",
    "  Template brain: ", ravepipeline::raveio_getopt('threeBrain_template_subject', default = "N27"), "\n",
    "  Directories:    ",
    "Raw ", healthy_directory(raw_dir), ", ",
    "Main ", healthy_directory(data_dir), ", ",
    "Session ", healthy_directory(cache_dir), "\n",

    "  Core dependencies (for developer debug purposes): \n",
    "    ",
    package_ver("ravemanager"), ",\t",
    package_ver("rave"), ",\t\t",
    package_ver("ravecore"), ",\t",
    package_ver("ravedash"), ",\t\t",

    "\n    ",
    package_ver("dipsaus"), ",\t\t",
    package_ver("ravetools"), ",\t",
    package_ver("threeBrain"), ",\t",
    package_ver("filearray"), ",\t",

    "\n    ",
    package_ver("shidashi"), ",\t",
    package_ver("rpymat"), ",\t\t",
    package_ver("readNSx"), ",\t",
    package_ver("rpyANTs"), ",\t",

    "\n    ",
    package_ver("rutabaga"), ",\t\t",
    package_ver("ravebuiltins"),

    "\n  Third-party dependencies: \n",

    "    ",
    package_ver("future"), ",\t\t",
    package_ver("shiny"), ",\t\t",
    package_ver("targets"), ",\t",
    package_ver("freesurferformats"), "\t",

    ""
  )

}
