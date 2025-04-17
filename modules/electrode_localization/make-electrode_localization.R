library(targets)
library(ravepipeline)
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
    input_transform_space = targets::tar_target_raw("transform_space", 
        quote({
            settings[["transform_space"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_postprocess_surface_target = targets::tar_target_raw("postprocess_surface_target", 
        quote({
            settings[["postprocess_surface_target"]]
        }), deps = "settings"), input_path_transform = targets::tar_target_raw("path_transform", 
        quote({
            settings[["path_transform"]]
        }), deps = "settings"), input_path_mri = targets::tar_target_raw("path_mri", 
        quote({
            settings[["path_mri"]]
        }), deps = "settings"), input_path_ct = targets::tar_target_raw("path_ct", 
        quote({
            settings[["path_ct"]]
        }), deps = "settings"), `__extern_path_localization_plan` = targets::tar_target_raw("settings_path._localization_plan_", 
        "./data/localization_plan.json", format = "file"), input_localization_plan = targets::tar_target_raw("localization_plan", 
        quote({
            asNamespace("ravepipeline")$pipeline_load_extdata(name = "localization_plan", 
                format = "json", error_if_missing = FALSE, default_if_missing = structure(list(), 
                  class = "key_missing"), pipe_dir = ".")
        }), deps = "settings_path._localization_plan_"), `__extern_path_localization_list` = targets::tar_target_raw("settings_path._localization_list_", 
        "./data/localization_list.json", format = "file"), input_localization_list = targets::tar_target_raw("localization_list", 
        quote({
            asNamespace("ravepipeline")$pipeline_load_extdata(name = "localization_list", 
                format = "json", error_if_missing = FALSE, default_if_missing = structure(list(), 
                  class = "key_missing"), pipe_dir = ".")
        }), deps = "settings_path._localization_list_"), load_FreeSurfer_LUT = targets::tar_target_raw(name = "fslut", 
        command = quote({
            .__target_expr__. <- quote({
                fslut <- load_freesurfer_lookup_table()
            })
            tryCatch({
                eval(.__target_expr__.)
                return(fslut)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "fslut", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "fslut", target_expr = quote({
                {
                  fslut <- load_freesurfer_lookup_table()
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
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "rave-subject", 
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
                ct_candidates <- get_image_candidates_from_coregistration_folder(subject)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(ct_candidates)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "ct_candidates", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "ct_candidates", target_expr = quote({
                {
                  ct_candidates <- get_image_candidates_from_coregistration_folder(subject)
                }
                ct_candidates
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    check_localization_plan = targets::tar_target_raw(name = "plan_list", 
        command = quote({
            .__target_expr__. <- quote({
                plan_list <- make_localization_plan_list(subject = subject, 
                  localization_plan = localization_plan, write_table = TRUE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plan_list)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "plan_list", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "plan_list", target_expr = quote({
                {
                  plan_list <- make_localization_plan_list(subject = subject, 
                    localization_plan = localization_plan, write_table = TRUE)
                }
                plan_list
            }), target_depends = c("subject", "localization_plan"
            )), deps = c("subject", "localization_plan"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), check_load_pial_envelop = targets::tar_target_raw(name = "pial_envelope", 
        command = quote({
            .__target_expr__. <- quote({
                pial_envelope <- ensure_pial_envelope(subject = subject)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(pial_envelope)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "pial_envelope", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "pial_envelope", target_expr = quote({
                {
                  pial_envelope <- ensure_pial_envelope(subject = subject)
                }
                pial_envelope
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    load_brain_and_generate_pial_envelope = targets::tar_target_raw(name = "brain", 
        command = quote({
            .__target_expr__. <- quote({
                force(pial_envelope)
                brain <- load_brain_with_electrode_prototypes(subject = subject, 
                  plan_list = plan_list)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(brain)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "brain", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "brain", target_expr = quote({
                {
                  force(pial_envelope)
                  brain <- load_brain_with_electrode_prototypes(subject = subject, 
                    plan_list = plan_list)
                }
                brain
            }), target_depends = c("pial_envelope", "subject", 
            "plan_list")), deps = c("pial_envelope", "subject", 
        "plan_list"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), Loading_brain_and_CT_if_exists = targets::tar_target_raw(name = "localize_data", 
        command = quote({
            .__target_expr__. <- quote({
                localize_data <- collect_localization_data(subject = subject, 
                  path_mri = path_mri, path_ct = path_ct, path_transform = path_transform, 
                  transform_space = transform_space)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localize_data)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "localize_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "localize_data", target_expr = quote({
                {
                  localize_data <- collect_localization_data(subject = subject, 
                    path_mri = path_mri, path_ct = path_ct, path_transform = path_transform, 
                    transform_space = transform_space)
                }
                localize_data
            }), target_depends = c("subject", "path_mri", "path_ct", 
            "path_transform", "transform_space")), deps = c("subject", 
        "path_mri", "path_ct", "path_transform", "transform_space"
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
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "ct_exists", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
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
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "viewer", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
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
                localization_result_initial <- stage_localization(subject = subject, 
                  brain = brain, localization_list = localization_list)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localization_result_initial)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "localization_result_initial", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "localization_result_initial", target_expr = quote({
                {
                  localization_result_initial <- stage_localization(subject = subject, 
                    brain = brain, localization_list = localization_list)
                }
                localization_result_initial
            }), target_depends = c("subject", "brain", "localization_list"
            )), deps = c("subject", "brain", "localization_list"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), get_finalized_table = targets::tar_target_raw(name = "localization_result_final", 
        command = quote({
            .__target_expr__. <- quote({
                force(localization_result_initial)
                localization_result_final <- save_localization(subject = subject, 
                  brain = brain, localize_data = localize_data)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(localization_result_final)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "localization_result_final", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "localization_result_final", target_expr = quote({
                {
                  force(localization_result_initial)
                  localization_result_final <- save_localization(subject = subject, 
                    brain = brain, localize_data = localize_data)
                }
                localization_result_final
            }), target_depends = c("localization_result_initial", 
            "subject", "brain", "localize_data")), deps = c("localization_result_initial", 
        "subject", "brain", "localize_data"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), postprocess_using_ANTs = targets::tar_target_raw(name = "postprocess_ants", 
        command = quote({
            .__target_expr__. <- quote({
                electrode_table <- subject$get_electrode_table()
                is_debug <- getOption("raveio.debug", FALSE)
                if (!is.data.frame(electrode_table)) {
                  stop("Invalid electrode table. Please save the electrode localization results first.")
                }
                if (!rpyANTs::ants_available()) {
                  stop("ANTs is not configured for RAVE. Python environment must be configured through `ravemanager::configure_python()` first.")
                }
                if (!all(c("T1R", "T1A", "T1S") %in% names(electrode_table))) {
                  brain <- raveio::rave_brain(subject)
                  if (is.null(brain)) {
                    stop("The electrode table is missing [`T1R`, `T1A`, `T1S`] columns (they are T1 scanner space in RAS coordinate system.)")
                  }
                  tkr_ras <- as.matrix(electrode_table[, c("Coord_x", 
                    "Coord_y", "Coord_z")])
                  t1_ras <- brain$electrodes$apply_transform_points(positions = tkr_ras, 
                    from = "tkrRAS", to = "scannerRAS")
                  valids <- rowSums(tkr_ras^2) > 0
                } else {
                  t1_ras <- as.matrix(electrode_table[, c("T1R", 
                    "T1A", "T1S")])
                  valids <- rowSums(t1_ras^2) > 0
                }
                if (any(valids)) {
                  yael_process <- raveio::YAELProcess$new(subject_code = subject$subject_code)
                  mapping <- NULL
                  mni152 <- NULL
                  template_name <- NULL
                  for (template_name in c("mni_icbm152_nlin_asym_09b", 
                    "mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09c")) {
                    mapping <- yael_process$get_template_mapping(template_name = template_name)
                    if (!is.null(mapping)) {
                      break
                    }
                  }
                  if (is.null(mapping)) {
                    if (!is_debug) {
                      stop("Unable to find any non-linear mapping files. Please check help documentation of `?raveio::cmd_run_yael_preprocess` on how to normalize to template in RAVE.")
                    }
                  } else {
                    mni152 <- yael_process$transform_points_to_template(native_ras = t1_ras, 
                      template_name = template_name)
                    mni305 <- cbind(mni152, 1) %*% t(solve(raveio::MNI305_to_MNI152))
                    mni152[!valids, ] <- 0
                    mni305[!valids, ] <- 0
                    electrode_table$MNI305_x <- mni305[, 1]
                    electrode_table$MNI305_y <- mni305[, 2]
                    electrode_table$MNI305_z <- mni305[, 3]
                    electrode_table$MNI152_x <- mni152[, 1]
                    electrode_table$MNI152_y <- mni152[, 2]
                    electrode_table$MNI152_z <- mni152[, 3]
                  }
                }
                postprocess_ants <- electrode_table[, c("Electrode", 
                  "T1R", "T1A", "T1S", "MNI152_x", "MNI152_y", 
                  "MNI152_z", "MNI305_x", "MNI305_y", "MNI305_z")]
            })
            tryCatch({
                eval(.__target_expr__.)
                return(postprocess_ants)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "postprocess_ants", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "postprocess_ants", target_expr = quote({
                {
                  electrode_table <- subject$get_electrode_table()
                  is_debug <- getOption("raveio.debug", FALSE)
                  if (!is.data.frame(electrode_table)) {
                    stop("Invalid electrode table. Please save the electrode localization results first.")
                  }
                  if (!rpyANTs::ants_available()) {
                    stop("ANTs is not configured for RAVE. Python environment must be configured through `ravemanager::configure_python()` first.")
                  }
                  if (!all(c("T1R", "T1A", "T1S") %in% names(electrode_table))) {
                    brain <- raveio::rave_brain(subject)
                    if (is.null(brain)) {
                      stop("The electrode table is missing [`T1R`, `T1A`, `T1S`] columns (they are T1 scanner space in RAS coordinate system.)")
                    }
                    tkr_ras <- as.matrix(electrode_table[, c("Coord_x", 
                      "Coord_y", "Coord_z")])
                    t1_ras <- brain$electrodes$apply_transform_points(positions = tkr_ras, 
                      from = "tkrRAS", to = "scannerRAS")
                    valids <- rowSums(tkr_ras^2) > 0
                  } else {
                    t1_ras <- as.matrix(electrode_table[, c("T1R", 
                      "T1A", "T1S")])
                    valids <- rowSums(t1_ras^2) > 0
                  }
                  if (any(valids)) {
                    yael_process <- raveio::YAELProcess$new(subject_code = subject$subject_code)
                    mapping <- NULL
                    mni152 <- NULL
                    template_name <- NULL
                    for (template_name in c("mni_icbm152_nlin_asym_09b", 
                      "mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09c")) {
                      mapping <- yael_process$get_template_mapping(template_name = template_name)
                      if (!is.null(mapping)) {
                        break
                      }
                    }
                    if (is.null(mapping)) {
                      if (!is_debug) {
                        stop("Unable to find any non-linear mapping files. Please check help documentation of `?raveio::cmd_run_yael_preprocess` on how to normalize to template in RAVE.")
                      }
                    } else {
                      mni152 <- yael_process$transform_points_to_template(native_ras = t1_ras, 
                        template_name = template_name)
                      mni305 <- cbind(mni152, 1) %*% t(solve(raveio::MNI305_to_MNI152))
                      mni152[!valids, ] <- 0
                      mni305[!valids, ] <- 0
                      electrode_table$MNI305_x <- mni305[, 1]
                      electrode_table$MNI305_y <- mni305[, 2]
                      electrode_table$MNI305_z <- mni305[, 3]
                      electrode_table$MNI152_x <- mni152[, 1]
                      electrode_table$MNI152_y <- mni152[, 2]
                      electrode_table$MNI152_z <- mni152[, 3]
                    }
                  }
                  postprocess_ants <- electrode_table[, c("Electrode", 
                    "T1R", "T1A", "T1S", "MNI152_x", "MNI152_y", 
                    "MNI152_z", "MNI305_x", "MNI305_y", "MNI305_z")]
                }
                postprocess_ants
            }), target_depends = "subject"), deps = "subject", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    postprocess_surface_mapping = targets::tar_target_raw(name = "postprocess_surface_mapping", 
        command = quote({
            .__target_expr__. <- quote({
                electrode_table <- subject$get_electrode_table()
                if (!is.data.frame(electrode_table)) {
                  stop("Invalid electrode table. Please save the electrode localization results first.")
                }
                if (!all(c("T1R", "T1A", "T1S") %in% names(electrode_table))) {
                  brain <- raveio::rave_brain(subject)
                  if (is.null(brain)) {
                    stop("The electrode table is missing [`T1R`, `T1A`, `T1S`] columns (they are T1 scanner space in RAS coordinate system.)")
                  }
                  tkr_ras <- as.matrix(electrode_table[, c("Coord_x", 
                    "Coord_y", "Coord_z")])
                  t1_ras <- brain$electrodes$apply_transform_points(positions = tkr_ras, 
                    from = "tkrRAS", to = "scannerRAS")
                  valids <- rowSums(tkr_ras^2) > 0
                } else {
                  t1_ras <- as.matrix(electrode_table[, c("T1R", 
                    "T1A", "T1S")])
                  valids <- rowSums(t1_ras^2) > 0
                }
                surface_mapping <- raveio::transform_point_to_template(subject = subject, 
                  positions = t1_ras, space = "scannerRAS", mapping_method = "surface", 
                  flip_hemisphere = FALSE, verbose = TRUE, project_surface = postprocess_surface_target)
                surface_mapping[!valids, ] <- 0
                surface_mapping$Electrode <- electrode_table$Electrode
                postprocess_surface_mapping <- surface_mapping
            })
            tryCatch({
                eval(.__target_expr__.)
                return(postprocess_surface_mapping)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "postprocess_surface_mapping", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "postprocess_surface_mapping", target_expr = quote({
                {
                  electrode_table <- subject$get_electrode_table()
                  if (!is.data.frame(electrode_table)) {
                    stop("Invalid electrode table. Please save the electrode localization results first.")
                  }
                  if (!all(c("T1R", "T1A", "T1S") %in% names(electrode_table))) {
                    brain <- raveio::rave_brain(subject)
                    if (is.null(brain)) {
                      stop("The electrode table is missing [`T1R`, `T1A`, `T1S`] columns (they are T1 scanner space in RAS coordinate system.)")
                    }
                    tkr_ras <- as.matrix(electrode_table[, c("Coord_x", 
                      "Coord_y", "Coord_z")])
                    t1_ras <- brain$electrodes$apply_transform_points(positions = tkr_ras, 
                      from = "tkrRAS", to = "scannerRAS")
                    valids <- rowSums(tkr_ras^2) > 0
                  } else {
                    t1_ras <- as.matrix(electrode_table[, c("T1R", 
                      "T1A", "T1S")])
                    valids <- rowSums(t1_ras^2) > 0
                  }
                  surface_mapping <- raveio::transform_point_to_template(subject = subject, 
                    positions = t1_ras, space = "scannerRAS", 
                    mapping_method = "surface", flip_hemisphere = FALSE, 
                    verbose = TRUE, project_surface = postprocess_surface_target)
                  surface_mapping[!valids, ] <- 0
                  surface_mapping$Electrode <- electrode_table$Electrode
                  postprocess_surface_mapping <- surface_mapping
                }
                postprocess_surface_mapping
            }), target_depends = c("subject", "postprocess_surface_target"
            )), deps = c("subject", "postprocess_surface_target"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
