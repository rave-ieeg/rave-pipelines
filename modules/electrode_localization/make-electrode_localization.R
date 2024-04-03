library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
._._env_._.$pipeline <- pipeline_from_path(".")
lapply(sort(list.files(
  "R/", ignore.case = TRUE,
  pattern = "^shared-.*\\.R", 
  full.names = TRUE
)), function(f) {
  source(f, local = ._._env_._., chdir = TRUE)
})
targets::tar_option_set(envir = ._._env_._.)
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_path_ct = targets::tar_target_raw("path_ct", quote({
        settings[["path_ct"]]
    }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_path_mri = targets::tar_target_raw("path_mri", 
        quote({
            settings[["path_mri"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_path_transform = targets::tar_target_raw("path_transform", 
        quote({
            settings[["path_transform"]]
        }), deps = "settings"), input_transform_space = targets::tar_target_raw("transform_space", 
        quote({
            settings[["transform_space"]]
        }), deps = "settings"), `__extern_path_localization_list` = targets::tar_target_raw("settings_path._localization_list_", 
        "./data/localization_list.json", format = "file"), input_localization_list = targets::tar_target_raw("localization_list", 
        quote({
            asNamespace("raveio")$pipeline_load_extdata(name = "localization_list", 
                format = "json", error_if_missing = FALSE, default_if_missing = structure(list(), 
                  class = "key_missing"), pipe_dir = ".")
        }), deps = "settings_path._localization_list_"), `__extern_path_localization_plan` = targets::tar_target_raw("settings_path._localization_plan_", 
        "./data/localization_plan.json", format = "file"), input_localization_plan = targets::tar_target_raw("localization_plan", 
        quote({
            asNamespace("raveio")$pipeline_load_extdata(name = "localization_plan", 
                format = "json", error_if_missing = FALSE, default_if_missing = structure(list(), 
                  class = "key_missing"), pipe_dir = ".")
        }), deps = "settings_path._localization_plan_"), load_FreeSurfer_LUT = targets::tar_target_raw(name = "fslut", 
        command = quote({
            .__target_expr__. <- quote({
                fslut_path <- system.file("palettes", "datacube2", 
                  "FreeSurferColorLUT.json", package = "threeBrain")
                cmap <- threeBrain::load_colormap(fslut_path)
                fslut <- list(cmap = cmap, labels = sapply(cmap$map, 
                  "[[", "Label"))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(fslut)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "fslut", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "fslut", target_expr = quote({
                {
                  fslut_path <- system.file("palettes", "datacube2", 
                    "FreeSurferColorLUT.json", package = "threeBrain")
                  cmap <- threeBrain::load_colormap(fslut_path)
                  fslut <- list(cmap = cmap, labels = sapply(cmap$map, 
                    "[[", "Label"))
                }
                fslut
            }), target_depends = character(0)), deps = character(0), 
        cue = targets::tar_cue("never"), pattern = NULL, iteration = "list"), 
    load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code, strict = FALSE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave-subject", 
            target_export = "subject", target_expr = quote({
                {
                  subject <- raveio::RAVESubject$new(project_name = project_name, 
                    subject_code = subject_code, strict = FALSE)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), find_CT_Nifti_files = targets::tar_target_raw(name = "ct_candidates", 
        command = quote({
            .__target_expr__. <- quote({
                fs_path <- subject$freesurfer_path
                if (length(fs_path) != 1 || is.na(fs_path) || 
                  !dir.exists(fs_path)) {
                  stop("Cannot find surface/volume reconstruction folder.")
                }
                f1 <- list.files(file.path(fs_path, "coregistration"), 
                  pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                  ignore.case = TRUE, include.dirs = FALSE, full.names = FALSE, 
                  all.files = FALSE)
                f2 <- list.files(file.path(fs_path, "..", "coregistration"), 
                  pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                  ignore.case = TRUE, include.dirs = FALSE, full.names = FALSE, 
                  all.files = FALSE)
                files <- c(f1, f2)
                files[duplicated(files)] <- sprintf("%s (2)", 
                  files[duplicated(files)])
                ct_candidates <- structure(files, paths = list(f1 = f1, 
                  f2 = f2))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(ct_candidates)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "ct_candidates", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "ct_candidates", target_expr = quote({
                {
                  fs_path <- subject$freesurfer_path
                  if (length(fs_path) != 1 || is.na(fs_path) || 
                    !dir.exists(fs_path)) {
                    stop("Cannot find surface/volume reconstruction folder.")
                  }
                  f1 <- list.files(file.path(fs_path, "coregistration"), 
                    pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                    ignore.case = TRUE, include.dirs = FALSE, 
                    full.names = FALSE, all.files = FALSE)
                  f2 <- list.files(file.path(fs_path, "..", "coregistration"), 
                    pattern = "nii(?:\\.gz)?$", recursive = FALSE, 
                    ignore.case = TRUE, include.dirs = FALSE, 
                    full.names = FALSE, all.files = FALSE)
                  files <- c(f1, f2)
                  files[duplicated(files)] <- sprintf("%s (2)", 
                    files[duplicated(files)])
                  ct_candidates <- structure(files, paths = list(f1 = f1, 
                    f2 = f2))
                }
                ct_candidates
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    check_localization_plan = targets::tar_target_raw(name = "plan_list", 
        command = quote({
            .__target_expr__. <- quote({
                plan_list <- list()
                plans <- dipsaus::fastmap2()
                electrodes <- sort(subject$preprocess_settings$electrodes)
                lapply(summarize_plan_list(localization_plan), 
                  function(item) {
                    if (!length(item)) {
                      return()
                    }
                    if (!isTRUE(item$n > 0)) {
                      return()
                    }
                    label <- toupper(as.character(item$label))
                    part1 <- plans[[label]]
                    if (length(part1)) {
                      if (!identical(part1$type, item$type)) {
                        stop(sprintf("There exist multiple groups with the same group label [%s] (group labels are case-insensitive) but different electrode types [%s, %s]. I cannot determine which electrode prototype for the localization. Please check and correct.", 
                          label, part1$type, item$type))
                      }
                      part1$channels <- sprintf("%s,%s", part1$channels, 
                        item$channels)
                      part1$n <- part1$n + item$n
                      item <- part1
                    }
                    plans[[label]] <- list(n = item$n, channels = item$channels, 
                      type = item$type, label = label, hemisphere = item$hemisphere, 
                      dimension = item$dimension)
                  })
                plan_table <- lapply(names(plans), function(nm) {
                  item <- plans[[nm]]
                  proto <- get_prototype(item$type)
                  channs <- parse_svec(item$channels, sort = FALSE, 
                    unique = FALSE, na_rm = FALSE)
                  unplug <- is.na(channs)
                  n_unplug <- sum(unplug)
                  n_chans <- length(channs)
                  if (n_chans == n_unplug) {
                    return()
                  }
                  if (!is.null(proto)) {
                    if (n_chans > proto$n_channels) {
                      stop(sprintf("Electrode group [%s] (prototype: %s) has maximum of %d channels by design. However, you have entered %d channels (including %d unplugged channels). Please check & reduce the number of channels.", 
                        item$label, item$type, proto$n_channels, 
                        n_chans, n_unplug))
                    }
                  }
                  corder <- seq_along(channs)[!unplug]
                  channs <- channs[!unplug]
                  if (item$type %in% raveio::LOCATION_TYPES) {
                    ltype <- item$type
                    pname <- ""
                  } else {
                    ltype <- raveio::LOCATION_TYPES[[1]]
                    pname <- item$type
                  }
                  data.frame(Electrode = channs, Label = sprintf("%s%d", 
                    item$label, corder), LabelPrefix = item$label, 
                    Dimension = item$dimension, LocationType = ltype, 
                    Hemisphere = item$hemisphere, Prototype = pname, 
                    ContactOrder = corder)
                })
                plan_table <- drop_nulls(plan_table)
                if (length(plan_table)) {
                  plan_table <- do.call("rbind", unname(plan_table))
                  if (!nrow(plan_table)) {
                    stop("No valid electrodes to localize.")
                  }
                  count <- table(plan_table$Electrode)
                  count <- names(count)[count > 1]
                  if (length(count)) {
                    stop(sprintf("Electrode contact [%s] has duplicated entries. Please remove and continue.", 
                      deparse_svec(as.integer(count))))
                  }
                  plan_table <- plan_table[order(plan_table$Electrode), 
                    ]
                  rownames(plan_table) <- NULL
                  n_planned <- nrow(plan_table)
                  if (length(electrodes) > 0) {
                    if (length(electrodes) != n_planned) {
                      stop(sprintf("The electrode planned (n=%d, for localization) has inconsistent length with registered channel size (n=%d) from the signal pipelines.", 
                        n_planned, length(electrodes)))
                    }
                    plan_table$Electrode <- electrodes
                  }
                  files <- file.path(subject$meta_path, c("electrodes_unsaved.csv", 
                    "electrodes.csv"))
                  files <- files[file.exists(files)]
                  try({
                    if (length(files)) {
                      files <- files[[1]]
                      electrode_table <- raveio::safe_read_csv(files)
                      tname1 <- c("Electrode", "Coord_x", "Coord_y", 
                        "Coord_z")
                      tname2 <- c("Electrode", "Coord_x", "Coord_y", 
                        "Coord_z", "Radius", "MNI305_x", "MNI305_y", 
                        "MNI305_z", "FSIndex", "FSLabel", "FSLabel_aparc_a2009s_aseg", 
                        "FSLabel_aparc_aseg", "FSLabel_aparc_DKTatlas_aseg", 
                        "FSLabel_aseg", "OrigCoord_x", "OrigCoord_y", 
                        "OrigCoord_z", "SurfaceElectrode", "DistanceShifted", 
                        "DistanceToPial", "Sphere_x", "Sphere_y", 
                        "Sphere_z", "Interpolation")
                      if (all(tname1 %in% names(electrode_table))) {
                        tname2 <- tname2[tname2 %in% names(electrode_table)]
                        electrode_table <- electrode_table[, 
                          tname2]
                        missing_electrodes <- as.integer(electrode_table$Electrode)
                        missing_electrodes <- missing_electrodes[!missing_electrodes %in% 
                          as.integer(plan_table$Electrode)]
                        if (length(missing_electrodes)) {
                          stop("Detected channel ", dipsaus::deparse_svec(missing_electrodes), 
                            " from RAVE data. Please include them in the electrode plan.")
                        }
                        plan_table <- merge(electrode_table, 
                          plan_table, by = "Electrode", all.y = TRUE)
                      }
                    }
                  }, silent = TRUE)
                  plan_table$Coord_x %?<-% 0
                  plan_table$Coord_y %?<-% 0
                  plan_table$Coord_z %?<-% 0
                  plan_table$Interpolation %?<-% "default"
                  plan_table$Prototype %?<-% ""
                  plan_table$MNI305_x %?<-% 0
                  plan_table$MNI305_y %?<-% 0
                  plan_table$MNI305_z %?<-% 0
                  plan_table$Sphere_x %?<-% 0
                  plan_table$Sphere_y %?<-% 0
                  plan_table$Sphere_z %?<-% 0
                  plan_table$OrigCoord_x %?<-% plan_table$Coord_x
                  plan_table$OrigCoord_y %?<-% plan_table$Coord_y
                  plan_table$OrigCoord_z %?<-% plan_table$Coord_z
                  has_NA <- is.na(plan_table$Coord_x) | is.na(plan_table$Coord_y) | 
                    is.na(plan_table$Coord_z)
                  if (any(has_NA)) {
                    plan_table$Coord_x[has_NA] <- 0
                    plan_table$Coord_y[has_NA] <- 0
                    plan_table$Coord_z[has_NA] <- 0
                  }
                  has_NA_MNI <- has_NA | is.na(plan_table$MNI305_x) | 
                    is.na(plan_table$MNI305_y) | is.na(plan_table$MNI305_z)
                  if (any(has_NA_MNI)) {
                    plan_table$MNI305_x[has_NA_MNI] <- 0
                    plan_table$MNI305_y[has_NA_MNI] <- 0
                    plan_table$MNI305_z[has_NA_MNI] <- 0
                  }
                  has_NA_sphere <- has_NA | is.na(plan_table$Sphere_x) | 
                    is.na(plan_table$Sphere_y) | is.na(plan_table$Sphere_z)
                  if (any(has_NA_sphere)) {
                    plan_table$Sphere_x[has_NA_sphere] <- 0
                    plan_table$Sphere_y[has_NA_sphere] <- 0
                    plan_table$Sphere_z[has_NA_sphere] <- 0
                    plan_table$OrigCoord_x[has_NA_sphere] <- 0
                    plan_table$OrigCoord_y[has_NA_sphere] <- 0
                    plan_table$OrigCoord_z[has_NA_sphere] <- 0
                    plan_table$DistanceShifted[has_NA_sphere] <- 0
                    plan_table$DistanceToPial[has_NA_sphere] <- 0
                  }
                  default_radius <- local({
                    is_grid <- grepl("^(G$|Grid)", plan_table$LabelPrefix)
                    is_mini <- grepl("mini$", plan_table$LabelPrefix)
                    r <- rep(1, nrow(plan_table))
                    r[is_grid] <- 2
                    r[is_mini] <- 0.5
                    r
                  })
                  plan_table$Radius %?<-% default_radius
                  has_NA <- is.na(plan_table$Radius) | !is.numeric(plan_table$Radius)
                  if (any(has_NA)) {
                    plan_table$Radius[has_NA] <- default_radius[has_NA]
                  }
                  is_surface <- plan_table$LocationType %in% 
                    c("ECoG")
                  plan_table$SurfaceElectrode %?<-% is_surface
                  has_NA <- is.na(plan_table$SurfaceElectrode)
                  if (any(has_NA)) {
                    plan_table$SurfaceElectrode[has_NA] <- is_surface[has_NA]
                  }
                  plan_table$SurfaceType %?<-% "pial"
                  has_NA <- is.na(plan_table$SurfaceType)
                  if (any(has_NA)) {
                    plan_table$SurfaceType[has_NA] <- "pial"
                  }
                  plan_table$FSIndex %?<-% 0
                  plan_table$FSLabel %?<-% "Unknown"
                  has_NA <- is.na(plan_table$FSIndex) | is.na(plan_table$FSLabel)
                  if (any(has_NA)) {
                    plan_table$FSIndex[has_NA] <- 0L
                    plan_table$FSLabel[has_NA] <- "Unknown"
                  }
                  etypes <- subject$preprocess_settings$electrode_types
                  if (!length(etypes)) {
                    etypes <- ifelse(is.na(plan_table$LocationType) | 
                      plan_table$LocationType %in% c("EEG", "Others"), 
                      "Unknown", "LFP")
                  }
                  plan_table$SignalType <- etypes
                  plan_list <- split(plan_table, plan_table$LabelPrefix)
                  raveio::dir_create2(subject$meta_path)
                  utils::write.csv(plan_table, file = file.path(subject$meta_path, 
                    "electrodes_unsaved.csv"), row.names = FALSE)
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plan_list)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plan_list", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plan_list", target_expr = quote({
                {
                  plan_list <- list()
                  plans <- dipsaus::fastmap2()
                  electrodes <- sort(subject$preprocess_settings$electrodes)
                  lapply(summarize_plan_list(localization_plan), 
                    function(item) {
                      if (!length(item)) {
                        return()
                      }
                      if (!isTRUE(item$n > 0)) {
                        return()
                      }
                      label <- toupper(as.character(item$label))
                      part1 <- plans[[label]]
                      if (length(part1)) {
                        if (!identical(part1$type, item$type)) {
                          stop(sprintf("There exist multiple groups with the same group label [%s] (group labels are case-insensitive) but different electrode types [%s, %s]. I cannot determine which electrode prototype for the localization. Please check and correct.", 
                            label, part1$type, item$type))
                        }
                        part1$channels <- sprintf("%s,%s", part1$channels, 
                          item$channels)
                        part1$n <- part1$n + item$n
                        item <- part1
                      }
                      plans[[label]] <- list(n = item$n, channels = item$channels, 
                        type = item$type, label = label, hemisphere = item$hemisphere, 
                        dimension = item$dimension)
                    })
                  plan_table <- lapply(names(plans), function(nm) {
                    item <- plans[[nm]]
                    proto <- get_prototype(item$type)
                    channs <- parse_svec(item$channels, sort = FALSE, 
                      unique = FALSE, na_rm = FALSE)
                    unplug <- is.na(channs)
                    n_unplug <- sum(unplug)
                    n_chans <- length(channs)
                    if (n_chans == n_unplug) {
                      return()
                    }
                    if (!is.null(proto)) {
                      if (n_chans > proto$n_channels) {
                        stop(sprintf("Electrode group [%s] (prototype: %s) has maximum of %d channels by design. However, you have entered %d channels (including %d unplugged channels). Please check & reduce the number of channels.", 
                          item$label, item$type, proto$n_channels, 
                          n_chans, n_unplug))
                      }
                    }
                    corder <- seq_along(channs)[!unplug]
                    channs <- channs[!unplug]
                    if (item$type %in% raveio::LOCATION_TYPES) {
                      ltype <- item$type
                      pname <- ""
                    } else {
                      ltype <- raveio::LOCATION_TYPES[[1]]
                      pname <- item$type
                    }
                    data.frame(Electrode = channs, Label = sprintf("%s%d", 
                      item$label, corder), LabelPrefix = item$label, 
                      Dimension = item$dimension, LocationType = ltype, 
                      Hemisphere = item$hemisphere, Prototype = pname, 
                      ContactOrder = corder)
                  })
                  plan_table <- drop_nulls(plan_table)
                  if (length(plan_table)) {
                    plan_table <- do.call("rbind", unname(plan_table))
                    if (!nrow(plan_table)) {
                      stop("No valid electrodes to localize.")
                    }
                    count <- table(plan_table$Electrode)
                    count <- names(count)[count > 1]
                    if (length(count)) {
                      stop(sprintf("Electrode contact [%s] has duplicated entries. Please remove and continue.", 
                        deparse_svec(as.integer(count))))
                    }
                    plan_table <- plan_table[order(plan_table$Electrode), 
                      ]
                    rownames(plan_table) <- NULL
                    n_planned <- nrow(plan_table)
                    if (length(electrodes) > 0) {
                      if (length(electrodes) != n_planned) {
                        stop(sprintf("The electrode planned (n=%d, for localization) has inconsistent length with registered channel size (n=%d) from the signal pipelines.", 
                          n_planned, length(electrodes)))
                      }
                      plan_table$Electrode <- electrodes
                    }
                    files <- file.path(subject$meta_path, c("electrodes_unsaved.csv", 
                      "electrodes.csv"))
                    files <- files[file.exists(files)]
                    try({
                      if (length(files)) {
                        files <- files[[1]]
                        electrode_table <- raveio::safe_read_csv(files)
                        tname1 <- c("Electrode", "Coord_x", "Coord_y", 
                          "Coord_z")
                        tname2 <- c("Electrode", "Coord_x", "Coord_y", 
                          "Coord_z", "Radius", "MNI305_x", "MNI305_y", 
                          "MNI305_z", "FSIndex", "FSLabel", "FSLabel_aparc_a2009s_aseg", 
                          "FSLabel_aparc_aseg", "FSLabel_aparc_DKTatlas_aseg", 
                          "FSLabel_aseg", "OrigCoord_x", "OrigCoord_y", 
                          "OrigCoord_z", "SurfaceElectrode", 
                          "DistanceShifted", "DistanceToPial", 
                          "Sphere_x", "Sphere_y", "Sphere_z", 
                          "Interpolation")
                        if (all(tname1 %in% names(electrode_table))) {
                          tname2 <- tname2[tname2 %in% names(electrode_table)]
                          electrode_table <- electrode_table[, 
                            tname2]
                          missing_electrodes <- as.integer(electrode_table$Electrode)
                          missing_electrodes <- missing_electrodes[!missing_electrodes %in% 
                            as.integer(plan_table$Electrode)]
                          if (length(missing_electrodes)) {
                            stop("Detected channel ", dipsaus::deparse_svec(missing_electrodes), 
                              " from RAVE data. Please include them in the electrode plan.")
                          }
                          plan_table <- merge(electrode_table, 
                            plan_table, by = "Electrode", all.y = TRUE)
                        }
                      }
                    }, silent = TRUE)
                    plan_table$Coord_x %?<-% 0
                    plan_table$Coord_y %?<-% 0
                    plan_table$Coord_z %?<-% 0
                    plan_table$Interpolation %?<-% "default"
                    plan_table$Prototype %?<-% ""
                    plan_table$MNI305_x %?<-% 0
                    plan_table$MNI305_y %?<-% 0
                    plan_table$MNI305_z %?<-% 0
                    plan_table$Sphere_x %?<-% 0
                    plan_table$Sphere_y %?<-% 0
                    plan_table$Sphere_z %?<-% 0
                    plan_table$OrigCoord_x %?<-% plan_table$Coord_x
                    plan_table$OrigCoord_y %?<-% plan_table$Coord_y
                    plan_table$OrigCoord_z %?<-% plan_table$Coord_z
                    has_NA <- is.na(plan_table$Coord_x) | is.na(plan_table$Coord_y) | 
                      is.na(plan_table$Coord_z)
                    if (any(has_NA)) {
                      plan_table$Coord_x[has_NA] <- 0
                      plan_table$Coord_y[has_NA] <- 0
                      plan_table$Coord_z[has_NA] <- 0
                    }
                    has_NA_MNI <- has_NA | is.na(plan_table$MNI305_x) | 
                      is.na(plan_table$MNI305_y) | is.na(plan_table$MNI305_z)
                    if (any(has_NA_MNI)) {
                      plan_table$MNI305_x[has_NA_MNI] <- 0
                      plan_table$MNI305_y[has_NA_MNI] <- 0
                      plan_table$MNI305_z[has_NA_MNI] <- 0
                    }
                    has_NA_sphere <- has_NA | is.na(plan_table$Sphere_x) | 
                      is.na(plan_table$Sphere_y) | is.na(plan_table$Sphere_z)
                    if (any(has_NA_sphere)) {
                      plan_table$Sphere_x[has_NA_sphere] <- 0
                      plan_table$Sphere_y[has_NA_sphere] <- 0
                      plan_table$Sphere_z[has_NA_sphere] <- 0
                      plan_table$OrigCoord_x[has_NA_sphere] <- 0
                      plan_table$OrigCoord_y[has_NA_sphere] <- 0
                      plan_table$OrigCoord_z[has_NA_sphere] <- 0
                      plan_table$DistanceShifted[has_NA_sphere] <- 0
                      plan_table$DistanceToPial[has_NA_sphere] <- 0
                    }
                    default_radius <- local({
                      is_grid <- grepl("^(G$|Grid)", plan_table$LabelPrefix)
                      is_mini <- grepl("mini$", plan_table$LabelPrefix)
                      r <- rep(1, nrow(plan_table))
                      r[is_grid] <- 2
                      r[is_mini] <- 0.5
                      r
                    })
                    plan_table$Radius %?<-% default_radius
                    has_NA <- is.na(plan_table$Radius) | !is.numeric(plan_table$Radius)
                    if (any(has_NA)) {
                      plan_table$Radius[has_NA] <- default_radius[has_NA]
                    }
                    is_surface <- plan_table$LocationType %in% 
                      c("ECoG")
                    plan_table$SurfaceElectrode %?<-% is_surface
                    has_NA <- is.na(plan_table$SurfaceElectrode)
                    if (any(has_NA)) {
                      plan_table$SurfaceElectrode[has_NA] <- is_surface[has_NA]
                    }
                    plan_table$SurfaceType %?<-% "pial"
                    has_NA <- is.na(plan_table$SurfaceType)
                    if (any(has_NA)) {
                      plan_table$SurfaceType[has_NA] <- "pial"
                    }
                    plan_table$FSIndex %?<-% 0
                    plan_table$FSLabel %?<-% "Unknown"
                    has_NA <- is.na(plan_table$FSIndex) | is.na(plan_table$FSLabel)
                    if (any(has_NA)) {
                      plan_table$FSIndex[has_NA] <- 0L
                      plan_table$FSLabel[has_NA] <- "Unknown"
                    }
                    etypes <- subject$preprocess_settings$electrode_types
                    if (!length(etypes)) {
                      etypes <- ifelse(is.na(plan_table$LocationType) | 
                        plan_table$LocationType %in% c("EEG", 
                          "Others"), "Unknown", "LFP")
                    }
                    plan_table$SignalType <- etypes
                    plan_list <- split(plan_table, plan_table$LabelPrefix)
                    raveio::dir_create2(subject$meta_path)
                    utils::write.csv(plan_table, file = file.path(subject$meta_path, 
                      "electrodes_unsaved.csv"), row.names = FALSE)
                  }
                }
                plan_list
            }), target_depends = c("subject", "localization_plan"
            )), deps = c("subject", "localization_plan"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), check_load_pial_envelop = targets::tar_target_raw(name = "pial_envelope", 
        command = quote({
            .__target_expr__. <- quote({
                pial_envelope <- 0
                tryCatch({
                  fs_path <- subject$freesurfer_path
                  if (!is.na(fs_path) && length(fs_path) == 1 && 
                    file.exists(fs_path)) {
                    lh_envelope_path <- file.path(fs_path, "surf", 
                      "lh.pial-outer-smoothed")
                    rh_envelope_path <- file.path(fs_path, "surf", 
                      "rh.pial-outer-smoothed")
                    lh_pial_path <- file.path(fs_path, "surf", 
                      "lh.pial.T1")
                    rh_pial_path <- file.path(fs_path, "surf", 
                      "rh.pial.T1")
                    if (!file.exists(lh_envelope_path) || !file.exists(rh_envelope_path)) {
                      if (!file.exists(lh_pial_path)) {
                        lh_pial_path <- file.path(fs_path, "surf", 
                          "lh.pial")
                      }
                      if (!file.exists(rh_pial_path)) {
                        rh_pial_path <- file.path(fs_path, "surf", 
                          "rh.pial")
                      }
                      if (file.exists(lh_pial_path)) {
                        threeBrain::generate_smooth_envelope(surface_path = lh_pial_path, 
                          save_as = lh_envelope_path, inflate = 3, 
                          verbose = TRUE, save_format = "bin")
                        pial_envelope <- 1
                      }
                      if (file.exists(rh_pial_path)) {
                        threeBrain::generate_smooth_envelope(surface_path = rh_pial_path, 
                          save_as = rh_envelope_path, inflate = 3, 
                          verbose = TRUE, save_format = "bin")
                        pial_envelope <- pial_envelope + 1
                      }
                    } else {
                      pial_envelope <- 2
                    }
                  }
                }, error = function(e) {
                  warning(e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(pial_envelope)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "pial_envelope", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "pial_envelope", target_expr = quote({
                {
                  pial_envelope <- 0
                  tryCatch({
                    fs_path <- subject$freesurfer_path
                    if (!is.na(fs_path) && length(fs_path) == 
                      1 && file.exists(fs_path)) {
                      lh_envelope_path <- file.path(fs_path, 
                        "surf", "lh.pial-outer-smoothed")
                      rh_envelope_path <- file.path(fs_path, 
                        "surf", "rh.pial-outer-smoothed")
                      lh_pial_path <- file.path(fs_path, "surf", 
                        "lh.pial.T1")
                      rh_pial_path <- file.path(fs_path, "surf", 
                        "rh.pial.T1")
                      if (!file.exists(lh_envelope_path) || !file.exists(rh_envelope_path)) {
                        if (!file.exists(lh_pial_path)) {
                          lh_pial_path <- file.path(fs_path, 
                            "surf", "lh.pial")
                        }
                        if (!file.exists(rh_pial_path)) {
                          rh_pial_path <- file.path(fs_path, 
                            "surf", "rh.pial")
                        }
                        if (file.exists(lh_pial_path)) {
                          threeBrain::generate_smooth_envelope(surface_path = lh_pial_path, 
                            save_as = lh_envelope_path, inflate = 3, 
                            verbose = TRUE, save_format = "bin")
                          pial_envelope <- 1
                        }
                        if (file.exists(rh_pial_path)) {
                          threeBrain::generate_smooth_envelope(surface_path = rh_pial_path, 
                            save_as = rh_envelope_path, inflate = 3, 
                            verbose = TRUE, save_format = "bin")
                          pial_envelope <- pial_envelope + 1
                        }
                      } else {
                        pial_envelope <- 2
                      }
                    }
                  }, error = function(e) {
                    warning(e)
                  })
                }
                pial_envelope
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    load_brain_and_generate_pial_envelope = targets::tar_target_raw(name = "brain", 
        command = quote({
            .__target_expr__. <- quote({
                force(pial_envelope)
                brain <- tryCatch({
                  threeBrain::threeBrain(path = subject$freesurfer_path, 
                    subject_code = subject$subject_code)
                }, error = function(e) {
                  threeBrain::freesurfer_brain2(fs_subject_folder = subject$freesurfer_path, 
                    subject_name = subject$subject_code, use_141 = FALSE)
                })
                geom_def_path <- file.path(subject$meta_path, 
                  "geometry_unsaved.json")
                geom_defs <- list()
                if (file.exists(geom_def_path)) {
                  tryCatch({
                    geom_defs <- raveio::load_json(geom_def_path)
                  }, error = function(e) {
                  })
                }
                if (!is.null(brain)) {
                  lapply(plan_list, function(sub) {
                    if (is.data.frame(sub) && nrow(sub)) {
                      proto <- brain$electrodes$add_geometry(label_prefix = sub$LabelPrefix[[1]], 
                        prototype_name = sub$Prototype[[1]])
                      if (!is.null(proto)) {
                        unsaved_def <- geom_defs[[proto$name]]
                        if (length(unsaved_def) == 1 && is.character(unsaved_def)) {
                          proto$from_json(unsaved_def)
                        }
                        proto$set_contact_channels(sub$Electrode, 
                          sub$ContactOrder)
                      }
                    }
                  })
                }
                brain
            })
            tryCatch({
                eval(.__target_expr__.)
                return(brain)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "brain", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "brain", target_expr = quote({
                {
                  force(pial_envelope)
                  brain <- tryCatch({
                    threeBrain::threeBrain(path = subject$freesurfer_path, 
                      subject_code = subject$subject_code)
                  }, error = function(e) {
                    threeBrain::freesurfer_brain2(fs_subject_folder = subject$freesurfer_path, 
                      subject_name = subject$subject_code, use_141 = FALSE)
                  })
                  geom_def_path <- file.path(subject$meta_path, 
                    "geometry_unsaved.json")
                  geom_defs <- list()
                  if (file.exists(geom_def_path)) {
                    tryCatch({
                      geom_defs <- raveio::load_json(geom_def_path)
                    }, error = function(e) {
                    })
                  }
                  if (!is.null(brain)) {
                    lapply(plan_list, function(sub) {
                      if (is.data.frame(sub) && nrow(sub)) {
                        proto <- brain$electrodes$add_geometry(label_prefix = sub$LabelPrefix[[1]], 
                          prototype_name = sub$Prototype[[1]])
                        if (!is.null(proto)) {
                          unsaved_def <- geom_defs[[proto$name]]
                          if (length(unsaved_def) == 1 && is.character(unsaved_def)) {
                            proto$from_json(unsaved_def)
                          }
                          proto$set_contact_channels(sub$Electrode, 
                            sub$ContactOrder)
                        }
                      }
                    })
                  }
                  brain
                }
                brain
            }), target_depends = c("pial_envelope", "subject", 
            "plan_list")), deps = c("pial_envelope", "subject", 
        "plan_list"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), Loading_brain_and_CT_if_exists = targets::tar_target_raw(name = "localize_data", 
        command = quote({
            .__target_expr__. <- quote({
                force(subject)
                resolve_path <- function(path) {
                  if (length(path) != 1) {
                    stop("Cannot resolve path: ", paste(format(path), 
                      collapse = "\n"))
                  }
                  if (startsWith(path, "{")) {
                    s <- strsplit(path, "/|\\\\")[[1]]
                    s[[1]] <- raveio::glue(s[[1]])
                    s <- do.call(file.path, as.list(s))
                    path <- normalizePath(s, mustWork = TRUE)
                  } else {
                    path <- normalizePath(path, mustWork = TRUE)
                  }
                  path
                }
                has_ct <- FALSE
                ct_path <- character(0L)
                mri_path <- character(0L)
                mri_data <- NULL
                transform_matrix <- NULL
                if (length(path_ct)) {
                  ct_path <- resolve_path(path_ct)
                  has_ct <- TRUE
                  subject$set_default("path_ct", path_ct, namespace = "electrode_localization")
                  ct_header <- threeBrain:::read_nii2(ct_path, 
                    head_only = TRUE)
                  transform_space <- tolower(transform_space)
                  if (transform_space %in% c("fsl")) {
                    mri_path <- resolve_path(path_mri)
                    mri_data <- threeBrain:::read_nii2(mri_path, 
                      head_only = TRUE)
                  }
                  subject$set_default("path_mri", path_mri, namespace = "electrode_localization")
                  if (transform_space %in% c("fsl", "ijk2ras")) {
                    transform_matrix <- as.matrix(read.table(resolve_path(path_transform)))
                    dimnames(transform_matrix) <- NULL
                    if (length(transform_matrix) != 16L || !is.numeric(transform_matrix)) {
                      stop("Invalid transform matrix. Must be a 4x4 matrix.")
                    }
                    subject$set_default("path_transform", path_transform, 
                      namespace = "electrode_localization")
                  } else {
                    transform_space <- "resampled"
                  }
                } else {
                  subject$set_default("path_ct", NULL, namespace = "electrode_localization")
                  ct_header <- NULL
                  transform_space <- "no_ct"
                }
                subject$set_default("transform_space", transform_space, 
                  namespace = "electrode_localization")
                localize_data <- list(transform_space = transform_space, 
                  ct_header = ct_header, ct_path = ct_path, mri_path = mri_path, 
                  mri_data = mri_data, transform_matrix = transform_matrix)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localize_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "localize_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "localize_data", target_expr = quote({
                {
                  force(subject)
                  resolve_path <- function(path) {
                    if (length(path) != 1) {
                      stop("Cannot resolve path: ", paste(format(path), 
                        collapse = "\n"))
                    }
                    if (startsWith(path, "{")) {
                      s <- strsplit(path, "/|\\\\")[[1]]
                      s[[1]] <- raveio::glue(s[[1]])
                      s <- do.call(file.path, as.list(s))
                      path <- normalizePath(s, mustWork = TRUE)
                    } else {
                      path <- normalizePath(path, mustWork = TRUE)
                    }
                    path
                  }
                  has_ct <- FALSE
                  ct_path <- character(0L)
                  mri_path <- character(0L)
                  mri_data <- NULL
                  transform_matrix <- NULL
                  if (length(path_ct)) {
                    ct_path <- resolve_path(path_ct)
                    has_ct <- TRUE
                    subject$set_default("path_ct", path_ct, namespace = "electrode_localization")
                    ct_header <- threeBrain:::read_nii2(ct_path, 
                      head_only = TRUE)
                    transform_space <- tolower(transform_space)
                    if (transform_space %in% c("fsl")) {
                      mri_path <- resolve_path(path_mri)
                      mri_data <- threeBrain:::read_nii2(mri_path, 
                        head_only = TRUE)
                    }
                    subject$set_default("path_mri", path_mri, 
                      namespace = "electrode_localization")
                    if (transform_space %in% c("fsl", "ijk2ras")) {
                      transform_matrix <- as.matrix(read.table(resolve_path(path_transform)))
                      dimnames(transform_matrix) <- NULL
                      if (length(transform_matrix) != 16L || 
                        !is.numeric(transform_matrix)) {
                        stop("Invalid transform matrix. Must be a 4x4 matrix.")
                      }
                      subject$set_default("path_transform", path_transform, 
                        namespace = "electrode_localization")
                    } else {
                      transform_space <- "resampled"
                    }
                  } else {
                    subject$set_default("path_ct", NULL, namespace = "electrode_localization")
                    ct_header <- NULL
                    transform_space <- "no_ct"
                  }
                  subject$set_default("transform_space", transform_space, 
                    namespace = "electrode_localization")
                  localize_data <- list(transform_space = transform_space, 
                    ct_header = ct_header, ct_path = ct_path, 
                    mri_path = mri_path, mri_data = mri_data, 
                    transform_matrix = transform_matrix)
                }
                localize_data
            }), target_depends = c("subject", "path_ct", "transform_space", 
            "path_mri", "path_transform")), deps = c("subject", 
        "path_ct", "transform_space", "path_mri", "path_transform"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), generate_indicator = targets::tar_target_raw(name = "ct_exists", 
        command = quote({
            .__target_expr__. <- quote({
                ct_exists <- isTRUE(!is.null(localize_data$ct_header) && 
                  is.list(localize_data$ct_header))
            })
            tryCatch({
                eval(.__target_expr__.)
                return(ct_exists)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "ct_exists", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "ct_exists", target_expr = quote({
                {
                  ct_exists <- isTRUE(!is.null(localize_data$ct_header) && 
                    is.list(localize_data$ct_header))
                }
                ct_exists
            }), target_depends = "localize_data"), deps = "localize_data", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    generate_localization_viewer = targets::tar_target_raw(name = "viewer", 
        command = quote({
            .__target_expr__. <- quote({
                force(ct_exists)
                viewer <- brain$localize(ct_path = localize_data$ct_path, 
                  transform_space = localize_data$transform_space, 
                  transform_matrix = localize_data$transform_matrix, 
                  mri_path = localize_data$mri_path)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(viewer)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "viewer", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "viewer", target_expr = quote({
                {
                  force(ct_exists)
                  viewer <- brain$localize(ct_path = localize_data$ct_path, 
                    transform_space = localize_data$transform_space, 
                    transform_matrix = localize_data$transform_matrix, 
                    mri_path = localize_data$mri_path)
                }
                viewer
            }), target_depends = c("ct_exists", "brain", "localize_data"
            )), deps = c("ct_exists", "brain", "localize_data"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), merge_localization_list = targets::tar_target_raw(name = "localization_result_initial", 
        command = quote({
            .__target_expr__. <- quote({
                localization_result_initial <- NULL
                prototype_list <- dipsaus::fastmap2()
                re <- lapply(localization_list, function(item) {
                  if (!is.data.frame(item) || !nrow(item)) {
                    return()
                  }
                  prototype_def <- attr(item, "prototype")
                  prototype <- NULL
                  prototype_name <- ""
                  label_prefix <- item$LabelPrefix[[1]]
                  if (!is.null(prototype_def)) {
                    if (!length(item$Prototype) && length(prototype_def$type)) {
                      item$Prototype <- prototype_def$type
                    }
                    prototype <- brain$electrodes$add_geometry(label_prefix = label_prefix, 
                      prototype_name = item$Prototype[[1]])
                    prototype_name <- prototype$name
                    prototype_def$name <- prototype$name
                    prototype$from_list(prototype_def)
                    if (length(item$ContactOrder)) {
                      prototype$set_contact_channels(item$Electrode, 
                        item$ContactOrder)
                    }
                    prototype_list[[prototype_name]] <- prototype$as_json(flattern = TRUE)
                  } else {
                    item$Prototype %?<-% ""
                  }
                  item$FSIndex %?<-% 0
                  item$FSLabel %?<-% "Unknown"
                  item$FSLabel_aparc_a2009s_aseg %?<-% "Unknown"
                  item$FSLabel_aparc_aseg %?<-% "Unknown"
                  item$FSLabel_aparc_DKTatlas_aseg %?<-% "Unknown"
                  item$FSLabel_aseg %?<-% "Unknown"
                  item$Radius %?<-% 1
                  item$Sphere_x %?<-% 0
                  item$Sphere_y %?<-% 0
                  item$Sphere_z %?<-% 0
                  item$DistanceShifted %?<-% NA
                  item$DistanceToPial %?<-% NA
                  item$SurfaceElectrode %?<-% FALSE
                  item$Interpolation %?<-% "default"
                  if (!is.null(prototype)) {
                    contact_tkrras <- t(prototype$get_contact_positions(channels = item$Electrode, 
                      apply_transform = TRUE))
                    contact_tkrras[is.na(contact_tkrras)] <- 0
                    item$Coord_x <- contact_tkrras[1, ]
                    item$Coord_y <- contact_tkrras[2, ]
                    item$Coord_z <- contact_tkrras[3, ]
                    item$OrigCoord_x <- contact_tkrras[1, ]
                    item$OrigCoord_y <- contact_tkrras[2, ]
                    item$OrigCoord_z <- contact_tkrras[3, ]
                  }
                  item$OrigCoord_x %?<-% item$Coord_x
                  item$OrigCoord_y %?<-% item$Coord_y
                  item$OrigCoord_z %?<-% item$Coord_z
                  tbl <- data.frame(Electrode = item$Electrode, 
                    Coord_x = item$Coord_x, Coord_y = item$Coord_y, 
                    Coord_z = item$Coord_z, Label = item$Label, 
                    LabelPrefix = item$LabelPrefix, Prototype = item$Prototype, 
                    ContactOrder = item$ContactOrder, Dimension = item$Dimension, 
                    Interpolation = item$Interpolation, LocationType = item$LocationType, 
                    Radius = item$Radius, Hemisphere = item$Hemisphere, 
                    MNI305_x = item$MNI305_x, MNI305_y = item$MNI305_y, 
                    MNI305_z = item$MNI305_z, FSIndex = item$FSIndex, 
                    FSLabel = item$FSLabel, FSLabel_aparc_a2009s_aseg = item$FSLabel_aparc_a2009s_aseg, 
                    FSLabel_aparc_aseg = item$FSLabel_aparc_aseg, 
                    FSLabel_aparc_DKTatlas_aseg = item$FSLabel_aparc_DKTatlas_aseg, 
                    FSLabel_aseg = item$FSLabel_aseg, OrigCoord_x = item$OrigCoord_x, 
                    OrigCoord_y = item$OrigCoord_y, OrigCoord_z = item$OrigCoord_z, 
                    SurfaceElectrode = item$SurfaceElectrode, 
                    DistanceShifted = item$DistanceShifted, DistanceToPial = item$DistanceToPial, 
                    SurfaceType = "pial", VertexNumber = -1, 
                    Sphere_x = item$Sphere_x, Sphere_y = item$Sphere_y, 
                    Sphere_z = item$Sphere_z)
                  if (!nrow(tbl)) {
                    return(NULL)
                  }
                  tbl
                })
                re <- do.call("rbind", drop_nulls(re))
                if (length(re) && nrow(re)) {
                  rownames(re) <- NULL
                  re <- re[order(re$Electrode), ]
                  empty_sel <- (re$Coord_x)^2 + (re$Coord_y)^2 + 
                    (re$Coord_z)^2
                  empty_sel <- is.na(empty_sel) | empty_sel == 
                    0
                  tkrRAS <- rbind(re$Coord_x, re$Coord_y, re$Coord_z, 
                    1)
                  mr_voxel <- solve(brain$Torig) %*% tkrRAS
                  t1 <- brain$Norig %*% mr_voxel
                  mni305 <- brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*% 
                    tkrRAS
                  mni152 <- raveio:::MNI305_to_MNI152 %*% mni305
                  mni305[, empty_sel] <- 0
                  mni152[, empty_sel] <- 0
                  t1[, empty_sel] <- 0
                  mr_voxel[, empty_sel] <- 0
                  re$MNI305_x <- mni305[1, ]
                  re$MNI305_y <- mni305[2, ]
                  re$MNI305_z <- mni305[3, ]
                  re$T1R <- t1[1, ]
                  re$T1A <- t1[2, ]
                  re$T1S <- t1[3, ]
                  re$MNI152_x <- mni152[1, ]
                  re$MNI152_y <- mni152[2, ]
                  re$MNI152_z <- mni152[3, ]
                  re$MRVoxel_I <- round(mr_voxel[1, ])
                  re$MRVoxel_J <- round(mr_voxel[2, ])
                  re$MRVoxel_K <- round(mr_voxel[3, ])
                  raveio::dir_create2(subject$meta_path)
                  utils::write.csv(x = re, file.path(subject$meta_path, 
                    "electrodes_unsaved.csv"), row.names = FALSE)
                  prototype_list <- as.list(prototype_list)
                  raveio::save_json(x = prototype_list, con = file.path(subject$meta_path, 
                    "geometry_unsaved.json"), serialize = TRUE)
                  localization_result_initial <- list(electrode_table = re, 
                    prototype_list = prototype_list)
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localization_result_initial)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "localization_result_initial", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "localization_result_initial", target_expr = quote({
                {
                  localization_result_initial <- NULL
                  prototype_list <- dipsaus::fastmap2()
                  re <- lapply(localization_list, function(item) {
                    if (!is.data.frame(item) || !nrow(item)) {
                      return()
                    }
                    prototype_def <- attr(item, "prototype")
                    prototype <- NULL
                    prototype_name <- ""
                    label_prefix <- item$LabelPrefix[[1]]
                    if (!is.null(prototype_def)) {
                      if (!length(item$Prototype) && length(prototype_def$type)) {
                        item$Prototype <- prototype_def$type
                      }
                      prototype <- brain$electrodes$add_geometry(label_prefix = label_prefix, 
                        prototype_name = item$Prototype[[1]])
                      prototype_name <- prototype$name
                      prototype_def$name <- prototype$name
                      prototype$from_list(prototype_def)
                      if (length(item$ContactOrder)) {
                        prototype$set_contact_channels(item$Electrode, 
                          item$ContactOrder)
                      }
                      prototype_list[[prototype_name]] <- prototype$as_json(flattern = TRUE)
                    } else {
                      item$Prototype %?<-% ""
                    }
                    item$FSIndex %?<-% 0
                    item$FSLabel %?<-% "Unknown"
                    item$FSLabel_aparc_a2009s_aseg %?<-% "Unknown"
                    item$FSLabel_aparc_aseg %?<-% "Unknown"
                    item$FSLabel_aparc_DKTatlas_aseg %?<-% "Unknown"
                    item$FSLabel_aseg %?<-% "Unknown"
                    item$Radius %?<-% 1
                    item$Sphere_x %?<-% 0
                    item$Sphere_y %?<-% 0
                    item$Sphere_z %?<-% 0
                    item$DistanceShifted %?<-% NA
                    item$DistanceToPial %?<-% NA
                    item$SurfaceElectrode %?<-% FALSE
                    item$Interpolation %?<-% "default"
                    if (!is.null(prototype)) {
                      contact_tkrras <- t(prototype$get_contact_positions(channels = item$Electrode, 
                        apply_transform = TRUE))
                      contact_tkrras[is.na(contact_tkrras)] <- 0
                      item$Coord_x <- contact_tkrras[1, ]
                      item$Coord_y <- contact_tkrras[2, ]
                      item$Coord_z <- contact_tkrras[3, ]
                      item$OrigCoord_x <- contact_tkrras[1, ]
                      item$OrigCoord_y <- contact_tkrras[2, ]
                      item$OrigCoord_z <- contact_tkrras[3, ]
                    }
                    item$OrigCoord_x %?<-% item$Coord_x
                    item$OrigCoord_y %?<-% item$Coord_y
                    item$OrigCoord_z %?<-% item$Coord_z
                    tbl <- data.frame(Electrode = item$Electrode, 
                      Coord_x = item$Coord_x, Coord_y = item$Coord_y, 
                      Coord_z = item$Coord_z, Label = item$Label, 
                      LabelPrefix = item$LabelPrefix, Prototype = item$Prototype, 
                      ContactOrder = item$ContactOrder, Dimension = item$Dimension, 
                      Interpolation = item$Interpolation, LocationType = item$LocationType, 
                      Radius = item$Radius, Hemisphere = item$Hemisphere, 
                      MNI305_x = item$MNI305_x, MNI305_y = item$MNI305_y, 
                      MNI305_z = item$MNI305_z, FSIndex = item$FSIndex, 
                      FSLabel = item$FSLabel, FSLabel_aparc_a2009s_aseg = item$FSLabel_aparc_a2009s_aseg, 
                      FSLabel_aparc_aseg = item$FSLabel_aparc_aseg, 
                      FSLabel_aparc_DKTatlas_aseg = item$FSLabel_aparc_DKTatlas_aseg, 
                      FSLabel_aseg = item$FSLabel_aseg, OrigCoord_x = item$OrigCoord_x, 
                      OrigCoord_y = item$OrigCoord_y, OrigCoord_z = item$OrigCoord_z, 
                      SurfaceElectrode = item$SurfaceElectrode, 
                      DistanceShifted = item$DistanceShifted, 
                      DistanceToPial = item$DistanceToPial, SurfaceType = "pial", 
                      VertexNumber = -1, Sphere_x = item$Sphere_x, 
                      Sphere_y = item$Sphere_y, Sphere_z = item$Sphere_z)
                    if (!nrow(tbl)) {
                      return(NULL)
                    }
                    tbl
                  })
                  re <- do.call("rbind", drop_nulls(re))
                  if (length(re) && nrow(re)) {
                    rownames(re) <- NULL
                    re <- re[order(re$Electrode), ]
                    empty_sel <- (re$Coord_x)^2 + (re$Coord_y)^2 + 
                      (re$Coord_z)^2
                    empty_sel <- is.na(empty_sel) | empty_sel == 
                      0
                    tkrRAS <- rbind(re$Coord_x, re$Coord_y, re$Coord_z, 
                      1)
                    mr_voxel <- solve(brain$Torig) %*% tkrRAS
                    t1 <- brain$Norig %*% mr_voxel
                    mni305 <- brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*% 
                      tkrRAS
                    mni152 <- raveio:::MNI305_to_MNI152 %*% mni305
                    mni305[, empty_sel] <- 0
                    mni152[, empty_sel] <- 0
                    t1[, empty_sel] <- 0
                    mr_voxel[, empty_sel] <- 0
                    re$MNI305_x <- mni305[1, ]
                    re$MNI305_y <- mni305[2, ]
                    re$MNI305_z <- mni305[3, ]
                    re$T1R <- t1[1, ]
                    re$T1A <- t1[2, ]
                    re$T1S <- t1[3, ]
                    re$MNI152_x <- mni152[1, ]
                    re$MNI152_y <- mni152[2, ]
                    re$MNI152_z <- mni152[3, ]
                    re$MRVoxel_I <- round(mr_voxel[1, ])
                    re$MRVoxel_J <- round(mr_voxel[2, ])
                    re$MRVoxel_K <- round(mr_voxel[3, ])
                    raveio::dir_create2(subject$meta_path)
                    utils::write.csv(x = re, file.path(subject$meta_path, 
                      "electrodes_unsaved.csv"), row.names = FALSE)
                    prototype_list <- as.list(prototype_list)
                    raveio::save_json(x = prototype_list, con = file.path(subject$meta_path, 
                      "geometry_unsaved.json"), serialize = TRUE)
                    localization_result_initial <- list(electrode_table = re, 
                      prototype_list = prototype_list)
                  }
                }
                localization_result_initial
            }), target_depends = c("localization_list", "brain", 
            "subject")), deps = c("localization_list", "brain", 
        "subject"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), get_finalized_table = targets::tar_target_raw(name = "localization_result_final", 
        command = quote({
            .__target_expr__. <- quote({
                localization_result_final <- list()
                src <- file.path(subject$meta_path, "electrodes_unsaved.csv")
                prot <- file.path(subject$meta_path, "geometry_unsaved.json")
                if (file.exists(prot)) {
                  proto_defs <- raveio::load_json(prot)
                  localization_result_final$prototype_definitions <- proto_defs
                  if (length(proto_defs)) {
                    geometry_dir <- raveio::dir_create2(file.path(subject$freesurfer_path, 
                      "RAVE", "geometry"))
                    for (nm in names(proto_defs)) {
                      target <- file.path(geometry_dir, sprintf("%s.json", 
                        nm))
                      writeLines(proto_defs[[nm]], target)
                    }
                  }
                }
                if (file.exists(src)) {
                  localization_result_final$electrode_table <- utils::read.csv(src)
                  try({
                    if (isTRUE(localize_data$transform_space %in% 
                      c("fsl", "ijk2ras", "resampled"))) {
                      electrode_table <- localization_result_final$electrode_table
                      scan_ras <- rbind(electrode_table$T1R, 
                        electrode_table$T1A, electrode_table$T1S, 
                        1)
                      switch(localize_data$transform_space, fsl = {
                        ct_ijk2fsl <- localize_data$ct_header$get_IJK_to_FSL()
                        ct_ijk2ras <- localize_data$ct_header$get_IJK_to_RAS()$matrix
                        mr_ijk2fsl <- localize_data$mri_data$get_IJK_to_FSL()
                        mr_ijk2ras <- localize_data$mri_data$get_IJK_to_RAS()$matrix
                        ct_ijk_to_mr_ras <- mr_ijk2ras %*% solve(mr_ijk2fsl) %*% 
                          localize_data$transform_matrix %*% 
                          ct_ijk2fsl
                        ct_ijk <- solve(ct_ijk_to_mr_ras) %*% 
                          scan_ras
                        ct_ras <- ct_ijk2ras %*% ct_ijk
                      }, ijk2ras = {
                        ct_ijk2ras <- localize_data$ct_header$get_IJK_to_RAS()$matrix
                        ct_ijk2mr_ras <- localize_data$transform_matrix
                        ct_ijk <- solve(ct_ijk2mr_ras) %*% scan_ras
                        ct_ras <- ct_ijk2ras %*% ct_ijk
                      }, {
                        ct_ijk2ras <- localize_data$ct_header$get_IJK_to_RAS()$matrix
                        ct_ijk <- solve(ct_ijk2ras) %*% scan_ras
                        ct_ras <- scan_ras
                      })
                      localization_result_final$ct_table <- data.frame(Electrode = electrode_table$Electrode, 
                        CTVoxel_I = ct_ijk[1, ], CTVoxel_J = ct_ijk[2, 
                          ], CTVoxel_K = ct_ijk[3, ], CT_R = ct_ras[1, 
                          ], CT_A = ct_ras[2, ], CT_S = ct_ras[3, 
                          ])
                    }
                  })
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localization_result_final)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "localization_result_final", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "localization_result_final", target_expr = quote({
                {
                  localization_result_final <- list()
                  src <- file.path(subject$meta_path, "electrodes_unsaved.csv")
                  prot <- file.path(subject$meta_path, "geometry_unsaved.json")
                  if (file.exists(prot)) {
                    proto_defs <- raveio::load_json(prot)
                    localization_result_final$prototype_definitions <- proto_defs
                    if (length(proto_defs)) {
                      geometry_dir <- raveio::dir_create2(file.path(subject$freesurfer_path, 
                        "RAVE", "geometry"))
                      for (nm in names(proto_defs)) {
                        target <- file.path(geometry_dir, sprintf("%s.json", 
                          nm))
                        writeLines(proto_defs[[nm]], target)
                      }
                    }
                  }
                  if (file.exists(src)) {
                    localization_result_final$electrode_table <- utils::read.csv(src)
                    try({
                      if (isTRUE(localize_data$transform_space %in% 
                        c("fsl", "ijk2ras", "resampled"))) {
                        electrode_table <- localization_result_final$electrode_table
                        scan_ras <- rbind(electrode_table$T1R, 
                          electrode_table$T1A, electrode_table$T1S, 
                          1)
                        switch(localize_data$transform_space, 
                          fsl = {
                            ct_ijk2fsl <- localize_data$ct_header$get_IJK_to_FSL()
                            ct_ijk2ras <- localize_data$ct_header$get_IJK_to_RAS()$matrix
                            mr_ijk2fsl <- localize_data$mri_data$get_IJK_to_FSL()
                            mr_ijk2ras <- localize_data$mri_data$get_IJK_to_RAS()$matrix
                            ct_ijk_to_mr_ras <- mr_ijk2ras %*% 
                              solve(mr_ijk2fsl) %*% localize_data$transform_matrix %*% 
                              ct_ijk2fsl
                            ct_ijk <- solve(ct_ijk_to_mr_ras) %*% 
                              scan_ras
                            ct_ras <- ct_ijk2ras %*% ct_ijk
                          }, ijk2ras = {
                            ct_ijk2ras <- localize_data$ct_header$get_IJK_to_RAS()$matrix
                            ct_ijk2mr_ras <- localize_data$transform_matrix
                            ct_ijk <- solve(ct_ijk2mr_ras) %*% 
                              scan_ras
                            ct_ras <- ct_ijk2ras %*% ct_ijk
                          }, {
                            ct_ijk2ras <- localize_data$ct_header$get_IJK_to_RAS()$matrix
                            ct_ijk <- solve(ct_ijk2ras) %*% scan_ras
                            ct_ras <- scan_ras
                          })
                        localization_result_final$ct_table <- data.frame(Electrode = electrode_table$Electrode, 
                          CTVoxel_I = ct_ijk[1, ], CTVoxel_J = ct_ijk[2, 
                            ], CTVoxel_K = ct_ijk[3, ], CT_R = ct_ras[1, 
                            ], CT_A = ct_ras[2, ], CT_S = ct_ras[3, 
                            ])
                      }
                    })
                  }
                }
                localization_result_final
            }), target_depends = c("subject", "localize_data"
            )), deps = c("subject", "localize_data"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"))
