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
    input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_fsl_path = targets::tar_target_raw("fsl_path", 
        quote({
            settings[["fsl_path"]]
        }), deps = "settings"), input_freesurfer_path = targets::tar_target_raw("freesurfer_path", 
        quote({
            settings[["freesurfer_path"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_path_mri = targets::tar_target_raw("path_mri", 
        quote({
            settings[["path_mri"]]
        }), deps = "settings"), input_skip_recon = targets::tar_target_raw("skip_recon", 
        quote({
            settings[["skip_recon"]]
        }), deps = "settings"), input_afni_path = targets::tar_target_raw("afni_path", 
        quote({
            settings[["afni_path"]]
        }), deps = "settings"), input_acpc_infile = targets::tar_target_raw("acpc_infile", 
        quote({
            settings[["acpc_infile"]]
        }), deps = "settings"), input_params = targets::tar_target_raw("params", 
        quote({
            settings[["params"]]
        }), deps = "settings"), input_dcm2niix_path = targets::tar_target_raw("dcm2niix_path", 
        quote({
            settings[["dcm2niix_path"]]
        }), deps = "settings"), input_path_ct = targets::tar_target_raw("path_ct", 
        quote({
            settings[["path_ct"]]
        }), deps = "settings"), input_skip_coregistration = targets::tar_target_raw("skip_coregistration", 
        quote({
            settings[["skip_coregistration"]]
        }), deps = "settings"), check_commandline_tools = targets::tar_target_raw(name = "cmd_tools", 
        command = quote({
            .__target_expr__. <- quote({
                default_dcm2niix_path <- raveio:::cmd_dcm2niix(error_on_missing = FALSE)
                dcm2niix <- tryCatch({
                  dcm2niix <- raveio::normalize_commandline_path(path = dcm2niix_path, 
                    unset = default_dcm2niix_path)
                  if (!path_is_valid(dcm2niix)) {
                    dcm2niix <- NULL
                  } else if (!identical(default_dcm2niix_path, 
                    dcm2niix)) {
                    ravepipeline::raveio_setopt("dcm2niix_path", 
                      dcm2niix)
                  }
                  dcm2niix
                }, error = function(e) {
                  NULL
                })
                default_fs_path <- raveio:::cmd_freesurfer_home(error_on_missing = FALSE)
                freesurfer <- tryCatch({
                  freesurfer <- raveio::normalize_commandline_path(path = freesurfer_path, 
                    unset = default_fs_path, type = "freesurfer")
                  if (!path_is_valid(freesurfer, dir_ok = TRUE)) {
                    freesurfer <- NULL
                  } else if (!identical(default_fs_path, freesurfer)) {
                    ravepipeline::raveio_setopt("freesurfer_path", 
                      freesurfer)
                  }
                  freesurfer
                }, error = function(e) {
                  NULL
                })
                default_fsl_path <- raveio:::cmd_fsl_home(error_on_missing = FALSE)
                flirt <- tryCatch({
                  fsl <- raveio::normalize_commandline_path(path = fsl_path, 
                    type = "fsl", unset = default_fsl_path)
                  flirt <- NULL
                  if (path_is_valid(fsl, dir_ok = TRUE)) {
                    if (!identical(default_fsl_path, fsl)) {
                      ravepipeline::raveio_setopt("fsl_path", 
                        fsl)
                    }
                    flirt <- file.path(fsl, "bin", "flirt")
                  }
                  flirt
                }, error = function(e) {
                  NULL
                })
                default_afni_path <- raveio:::cmd_afni_home(error_on_missing = FALSE)
                afni <- tryCatch({
                  afni <- raveio::normalize_commandline_path(path = afni_path, 
                    type = "afni", unset = default_afni_path)
                  if (path_is_valid(afni, dir_ok = TRUE)) {
                    if (!identical(default_afni_path, afni)) {
                      ravepipeline::raveio_setopt("afni_path", 
                        afni)
                    }
                  } else {
                    afni
                  }
                  afni
                }, error = function(e) {
                  NULL
                })
                cmd_tools <- list(dcm2niix = dcm2niix, freesurfer = freesurfer, 
                  flirt = flirt, afni = afni)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(cmd_tools)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "cmd_tools", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "cmd_tools", target_expr = quote({
                {
                  default_dcm2niix_path <- raveio:::cmd_dcm2niix(error_on_missing = FALSE)
                  dcm2niix <- tryCatch({
                    dcm2niix <- raveio::normalize_commandline_path(path = dcm2niix_path, 
                      unset = default_dcm2niix_path)
                    if (!path_is_valid(dcm2niix)) {
                      dcm2niix <- NULL
                    } else if (!identical(default_dcm2niix_path, 
                      dcm2niix)) {
                      ravepipeline::raveio_setopt("dcm2niix_path", 
                        dcm2niix)
                    }
                    dcm2niix
                  }, error = function(e) {
                    NULL
                  })
                  default_fs_path <- raveio:::cmd_freesurfer_home(error_on_missing = FALSE)
                  freesurfer <- tryCatch({
                    freesurfer <- raveio::normalize_commandline_path(path = freesurfer_path, 
                      unset = default_fs_path, type = "freesurfer")
                    if (!path_is_valid(freesurfer, dir_ok = TRUE)) {
                      freesurfer <- NULL
                    } else if (!identical(default_fs_path, freesurfer)) {
                      ravepipeline::raveio_setopt("freesurfer_path", 
                        freesurfer)
                    }
                    freesurfer
                  }, error = function(e) {
                    NULL
                  })
                  default_fsl_path <- raveio:::cmd_fsl_home(error_on_missing = FALSE)
                  flirt <- tryCatch({
                    fsl <- raveio::normalize_commandline_path(path = fsl_path, 
                      type = "fsl", unset = default_fsl_path)
                    flirt <- NULL
                    if (path_is_valid(fsl, dir_ok = TRUE)) {
                      if (!identical(default_fsl_path, fsl)) {
                        ravepipeline::raveio_setopt("fsl_path", 
                          fsl)
                      }
                      flirt <- file.path(fsl, "bin", "flirt")
                    }
                    flirt
                  }, error = function(e) {
                    NULL
                  })
                  default_afni_path <- raveio:::cmd_afni_home(error_on_missing = FALSE)
                  afni <- tryCatch({
                    afni <- raveio::normalize_commandline_path(path = afni_path, 
                      type = "afni", unset = default_afni_path)
                    if (path_is_valid(afni, dir_ok = TRUE)) {
                      if (!identical(default_afni_path, afni)) {
                        ravepipeline::raveio_setopt("afni_path", 
                          afni)
                      }
                    } else {
                      afni
                    }
                    afni
                  }, error = function(e) {
                    NULL
                  })
                  cmd_tools <- list(dcm2niix = dcm2niix, freesurfer = freesurfer, 
                    flirt = flirt, afni = afni)
                }
                cmd_tools
            }), target_depends = c("dcm2niix_path", "freesurfer_path", 
            "fsl_path", "afni_path")), deps = c("dcm2niix_path", 
        "freesurfer_path", "fsl_path", "afni_path"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), load_subject = targets::tar_target_raw(name = "subject", 
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
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), check_subject_data = targets::tar_target_raw(name = "check_result", 
        command = quote({
            .__target_expr__. <- quote({
                msgs <- character(0L)
                warns <- character(0L)
                fs_path <- subject$freesurfer_path
                if (!path_is_valid(fs_path, dir_ok = TRUE)) {
                  fs_path <- file.path(subject$path, "fs")
                  fs_reconstructed <- FALSE
                } else {
                  fs_reconstructed <- threeBrain::check_freesurfer_path(fs_path, 
                    autoinstall_template = FALSE, check_volume = TRUE, 
                    check_surface = FALSE)
                }
                mri <- file.path(subject$preprocess_settings$raw_path, 
                  path_mri)
                if (is.null(cmd_tools$dcm2niix) || is.null(cmd_tools$freesurfer)) {
                  warns <- append(warns, "Cannot find command-line `dcm2niix` and/or `FreeSurfer`: the reconstruction will fail. Please make sure these programs paths are entered correctly.")
                } else {
                  if (!path_is_valid(mri, dir_ok = TRUE)) {
                    warns <- append(warns, "No MRI folder found, the reconstruction will result in errors")
                  }
                }
                if (!skip_recon && fs_reconstructed) {
                  warns <- append(warns, sprintf("Found existing FreeSurfer reconstructed directory. `recon-all` will ignore the imported T1 images. Instead, FreeSurfer will continue working on this directory unless you manually remove it: %s", 
                    fs_path))
                }
                ct <- file.path(subject$preprocess_settings$raw_path, 
                  path_ct)
                if (!path_is_valid(ct, dir_ok = TRUE)) {
                  warns <- append(warns, "The CT path is invalid: co-registration will result in errors.")
                }
                if (!skip_recon) {
                  msgs <- append(msgs, sprintf("New FreeSurfer reconstruction will be created from %s", 
                    mri))
                  msgs <- append(msgs, sprintf("MRI default DICOM folder/Nifti file is set: %s", 
                    path_mri))
                }
                if (!skip_coregistration) {
                  msgs <- append(msgs, sprintf("CT will be co-registered to MRI for electrode localization; CT path: %s", 
                    ct))
                  msgs <- append(msgs, sprintf("CT default DICOM folder/Nifti file is set: %s", 
                    path_ct))
                }
                path_temp <- file.path(subject$preprocess_settings$raw_path, 
                  "rave-imaging")
                path_log <- file.path(path_temp, "log")
                check_result <- list(project_name = subject$project_name, 
                  subject_code = subject$subject_code, fs_path = fs_path, 
                  fs_reconstructed = fs_reconstructed, skip_recon = skip_recon, 
                  skip_coregistration = skip_coregistration, 
                  has_dcm2niix = !is.null(cmd_tools$dcm2niix), 
                  has_freesurfer = !is.null(cmd_tools$freesurfer), 
                  has_flirt = !is.null(cmd_tools$flirt), has_3dallineate = !is.null(cmd_tools$afni), 
                  path_mri = mri, path_ct = ct, path_temp = path_temp, 
                  path_log = path_log, messages = msgs, warnings = warns)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(check_result)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "check_result", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "check_result", target_expr = quote({
                {
                  msgs <- character(0L)
                  warns <- character(0L)
                  fs_path <- subject$freesurfer_path
                  if (!path_is_valid(fs_path, dir_ok = TRUE)) {
                    fs_path <- file.path(subject$path, "fs")
                    fs_reconstructed <- FALSE
                  } else {
                    fs_reconstructed <- threeBrain::check_freesurfer_path(fs_path, 
                      autoinstall_template = FALSE, check_volume = TRUE, 
                      check_surface = FALSE)
                  }
                  mri <- file.path(subject$preprocess_settings$raw_path, 
                    path_mri)
                  if (is.null(cmd_tools$dcm2niix) || is.null(cmd_tools$freesurfer)) {
                    warns <- append(warns, "Cannot find command-line `dcm2niix` and/or `FreeSurfer`: the reconstruction will fail. Please make sure these programs paths are entered correctly.")
                  } else {
                    if (!path_is_valid(mri, dir_ok = TRUE)) {
                      warns <- append(warns, "No MRI folder found, the reconstruction will result in errors")
                    }
                  }
                  if (!skip_recon && fs_reconstructed) {
                    warns <- append(warns, sprintf("Found existing FreeSurfer reconstructed directory. `recon-all` will ignore the imported T1 images. Instead, FreeSurfer will continue working on this directory unless you manually remove it: %s", 
                      fs_path))
                  }
                  ct <- file.path(subject$preprocess_settings$raw_path, 
                    path_ct)
                  if (!path_is_valid(ct, dir_ok = TRUE)) {
                    warns <- append(warns, "The CT path is invalid: co-registration will result in errors.")
                  }
                  if (!skip_recon) {
                    msgs <- append(msgs, sprintf("New FreeSurfer reconstruction will be created from %s", 
                      mri))
                    msgs <- append(msgs, sprintf("MRI default DICOM folder/Nifti file is set: %s", 
                      path_mri))
                  }
                  if (!skip_coregistration) {
                    msgs <- append(msgs, sprintf("CT will be co-registered to MRI for electrode localization; CT path: %s", 
                      ct))
                    msgs <- append(msgs, sprintf("CT default DICOM folder/Nifti file is set: %s", 
                      path_ct))
                  }
                  path_temp <- file.path(subject$preprocess_settings$raw_path, 
                    "rave-imaging")
                  path_log <- file.path(path_temp, "log")
                  check_result <- list(project_name = subject$project_name, 
                    subject_code = subject$subject_code, fs_path = fs_path, 
                    fs_reconstructed = fs_reconstructed, skip_recon = skip_recon, 
                    skip_coregistration = skip_coregistration, 
                    has_dcm2niix = !is.null(cmd_tools$dcm2niix), 
                    has_freesurfer = !is.null(cmd_tools$freesurfer), 
                    has_flirt = !is.null(cmd_tools$flirt), has_3dallineate = !is.null(cmd_tools$afni), 
                    path_mri = mri, path_ct = ct, path_temp = path_temp, 
                    path_log = path_log, messages = msgs, warnings = warns)
                }
                check_result
            }), target_depends = c("subject", "path_mri", "cmd_tools", 
            "skip_recon", "path_ct", "skip_coregistration")), 
        deps = c("subject", "path_mri", "cmd_tools", "skip_recon", 
        "path_ct", "skip_coregistration"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), generate_viewer_for_ACPC_alignment = targets::tar_target_raw(name = "viewer_acpc", 
        command = quote({
            .__target_expr__. <- quote({
                path_root <- file.path(subject$preprocess_settings$raw_path, 
                  "rave-imaging")
                mri_path <- file.path(path_root, "inputs", "MRI", 
                  acpc_infile)
                if (length(mri_path)) {
                  acpc_root <- file.path(path_root, "acpc-alignment")
                  if (!isTRUE(file.exists(mri_path))) {
                    stop("Invalid file [", paste(mri_path, collapse = ""), 
                      "] for ACPC alignment.")
                  }
                  acpc_mri_dir <- file.path(acpc_root, "mri")
                  if (file.exists(acpc_mri_dir)) {
                    unlink(acpc_mri_dir, recursive = TRUE)
                  }
                  raveio::dir_create2(acpc_mri_dir)
                  mri <- RNifti::readNifti(mri_path, internal = TRUE)
                  RNifti::writeNifti(mri, file.path(acpc_mri_dir, 
                    "brain.nii.gz"))
                  viewer_acpc <- threeBrain::threeBrain(path = acpc_root, 
                    subject_code = subject$subject_code)
                } else {
                  viewer_acpc <- NULL
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(viewer_acpc)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "viewer_acpc", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = "rave-brain", 
            target_export = "viewer_acpc", target_expr = quote({
                {
                  path_root <- file.path(subject$preprocess_settings$raw_path, 
                    "rave-imaging")
                  mri_path <- file.path(path_root, "inputs", 
                    "MRI", acpc_infile)
                  if (length(mri_path)) {
                    acpc_root <- file.path(path_root, "acpc-alignment")
                    if (!isTRUE(file.exists(mri_path))) {
                      stop("Invalid file [", paste(mri_path, 
                        collapse = ""), "] for ACPC alignment.")
                    }
                    acpc_mri_dir <- file.path(acpc_root, "mri")
                    if (file.exists(acpc_mri_dir)) {
                      unlink(acpc_mri_dir, recursive = TRUE)
                    }
                    raveio::dir_create2(acpc_mri_dir)
                    mri <- RNifti::readNifti(mri_path, internal = TRUE)
                    RNifti::writeNifti(mri, file.path(acpc_mri_dir, 
                      "brain.nii.gz"))
                    viewer_acpc <- threeBrain::threeBrain(path = acpc_root, 
                      subject_code = subject$subject_code)
                  } else {
                    viewer_acpc <- NULL
                  }
                }
                viewer_acpc
            }), target_depends = c("subject", "acpc_infile")), 
        deps = c("subject", "acpc_infile"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), import_T1_MRI = targets::tar_target_raw(name = "import_T1", 
        command = quote({
            .__target_expr__. <- quote({
                import_T1 <- tryCatch({
                  raveio::cmd_run_dcm2niix(subject = subject, 
                    src_path = check_result$path_mri, type = "MRI", 
                    merge = params$dcm2niix$merge %OF% c("Auto", 
                      "No", "Yes"), float = params$dcm2niix$float %OF% 
                      c("Yes", "No"), crop = params$dcm2niix$crop %OF% 
                      c("No", "Yes", "Ignore"), overwrite = TRUE, 
                    verbose = FALSE, dry_run = TRUE, command_path = cmd_tools$dcm2niix)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(import_T1)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "import_T1", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "import_T1", target_expr = quote({
                {
                  import_T1 <- tryCatch({
                    raveio::cmd_run_dcm2niix(subject = subject, 
                      src_path = check_result$path_mri, type = "MRI", 
                      merge = params$dcm2niix$merge %OF% c("Auto", 
                        "No", "Yes"), float = params$dcm2niix$float %OF% 
                        c("Yes", "No"), crop = params$dcm2niix$crop %OF% 
                        c("No", "Yes", "Ignore"), overwrite = TRUE, 
                      verbose = FALSE, dry_run = TRUE, command_path = cmd_tools$dcm2niix)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                import_T1
            }), target_depends = c("subject", "check_result", 
            "params", "cmd_tools")), deps = c("subject", "check_result", 
        "params", "cmd_tools"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), import_CT = targets::tar_target_raw(name = "import_CT", 
        command = quote({
            .__target_expr__. <- quote({
                import_CT <- tryCatch({
                  raveio::cmd_run_dcm2niix(subject = subject, 
                    src_path = check_result$path_ct, type = "CT", 
                    merge = params$dcm2niix$merge %OF% c("Auto", 
                      "No", "Yes"), float = params$dcm2niix$float %OF% 
                      c("Yes", "No"), crop = params$dcm2niix$crop %OF% 
                      c("No", "Yes", "Ignore"), overwrite = TRUE, 
                    verbose = FALSE, dry_run = TRUE, command_path = cmd_tools$dcm2niix)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(import_CT)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "import_CT", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "import_CT", target_expr = quote({
                {
                  import_CT <- tryCatch({
                    raveio::cmd_run_dcm2niix(subject = subject, 
                      src_path = check_result$path_ct, type = "CT", 
                      merge = params$dcm2niix$merge %OF% c("Auto", 
                        "No", "Yes"), float = params$dcm2niix$float %OF% 
                        c("Yes", "No"), crop = params$dcm2niix$crop %OF% 
                        c("No", "Yes", "Ignore"), overwrite = TRUE, 
                      verbose = FALSE, dry_run = TRUE, command_path = cmd_tools$dcm2niix)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                import_CT
            }), target_depends = c("subject", "check_result", 
            "params", "cmd_tools")), deps = c("subject", "check_result", 
        "params", "cmd_tools"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), Image_segmentation = targets::tar_target_raw(name = "image_segmentation", 
        command = quote({
            .__target_expr__. <- quote({
                image_segmentation <- tryCatch({
                  mri_path <- params$nii_t1
                  mri_root <- file.path(check_result$path_temp, 
                    "inputs", "MRI")
                  mri_path <- file.path(mri_root, mri_path)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a Nifti file under ", 
                      mri_root)
                  }
                  mri_postfix <- gsub("^.*\\.(nii|nii\\.gz)$", 
                    "\\1", basename(mri_path))
                  overwrite <- isTRUE(params$segmentation$fresh_start)
                  switch(paste(params$segmentation$program, collapse = ""), 
                    `recon-all` = {
                      autorecon_flags <- c("-autorecon1", "-all", 
                        "-autorecon2", "-autorecon3", "-autorecon2-cp", 
                        "-autorecon2-wm", "-autorecon2-pial")
                      flag <- params$segmentation$flag %OF% autorecon_flags
                      raveio::cmd_run_recon_all(subject = subject, 
                        mri_path = mri_path, args = flag, overwrite = overwrite, 
                        dry_run = TRUE, verbose = FALSE, command_path = cmd_tools$freesurfer)
                    }, `recon-all-clinical.sh` = {
                      raveio::cmd_run_recon_all_clinical(subject = subject, 
                        mri_path = mri_path, overwrite = overwrite, 
                        dry_run = TRUE, verbose = FALSE, command_path = cmd_tools$freesurfer)
                    }, `YAEL+recon-all` = {
                      raveio::cmd_run_yael_preprocess(subject_code = subject$subject_code, 
                        t1w_path = mri_path, normalize_template = c("mni_icbm152_nlin_asym_09b", 
                          "mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09c"), 
                        run_recon_all = TRUE, dry_run = TRUE, 
                        verbose = FALSE)
                    }, `ants-preprocessing` = {
                      raveio::cmd_run_r(dry_run = TRUE, verbose = FALSE, 
                        quoted = TRUE, expr = bquote({
                          subject <- raveio::as_rave_subject(.(subject$subject_id))
                          mri_src <- .(mri_path)
                          ants_dirpath <- file.path(subject$preprocess_settings$raw_path, 
                            "rave-imaging", "ants")
                          raveio::ants_preprocessing(work_path = ants_dirpath, 
                            image_path = mri_src, resample = TRUE, 
                            verbose = TRUE, template_subject = .(ravepipeline::raveio_getopt("threeBrain_template_subject")))
                          deriv_path <- file.path(subject$preprocess_settings$raw_path, 
                            "rave-imaging", "derivative")
                          raveio::dir_create2(deriv_path)
                          file.copy(from = mri_src, to = file.path(deriv_path, 
                            .(sprintf("MRI_RAW.%s", mri_postfix))), 
                            overwrite = TRUE, recursive = FALSE, 
                            copy.mode = TRUE, copy.date = TRUE)
                          file.copy(from = file.path(ants_dirpath, 
                            "mri", "resampled.nii.gz"), to = file.path(deriv_path, 
                            "T1-ants.nii.gz"), overwrite = TRUE, 
                            recursive = FALSE, copy.mode = TRUE, 
                            copy.date = TRUE)
                          message("Done")
                        }))
                    }, {
                      raveio::cmd_run_r(dry_run = TRUE, verbose = FALSE, 
                        quoted = TRUE, expr = bquote({
                          subject <- raveio::as_rave_subject(.(subject$subject_id))
                          image_path <- file.path(subject$preprocess_settings$raw_path, 
                            "rave-imaging")
                          mri_src <- .(mri_path)
                          mri_dirpath <- file.path(image_path, 
                            "fs", "mri")
                          mri_dst <- file.path(mri_dirpath, .(sprintf("brain.%s", 
                            mri_postfix)))
                          raveio::dir_create2(mri_dirpath)
                          file.copy(from = mri_src, to = mri_dst, 
                            overwrite = .(overwrite), recursive = FALSE, 
                            copy.mode = TRUE, copy.date = TRUE)
                          deriv_path <- file.path(image_path, 
                            "derivative")
                          raveio::dir_create2(deriv_path)
                          file.copy(from = mri_src, to = file.path(deriv_path, 
                            .(sprintf("MRI_RAW.%s", mri_postfix))), 
                            overwrite = TRUE, recursive = FALSE, 
                            copy.mode = TRUE, copy.date = TRUE)
                          message("Done")
                        }))
                    })
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(image_segmentation)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "image_segmentation", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "image_segmentation", target_expr = quote({
                {
                  image_segmentation <- tryCatch({
                    mri_path <- params$nii_t1
                    mri_root <- file.path(check_result$path_temp, 
                      "inputs", "MRI")
                    mri_path <- file.path(mri_root, mri_path)
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      stop("Please choose a Nifti file under ", 
                        mri_root)
                    }
                    mri_postfix <- gsub("^.*\\.(nii|nii\\.gz)$", 
                      "\\1", basename(mri_path))
                    overwrite <- isTRUE(params$segmentation$fresh_start)
                    switch(paste(params$segmentation$program, 
                      collapse = ""), `recon-all` = {
                      autorecon_flags <- c("-autorecon1", "-all", 
                        "-autorecon2", "-autorecon3", "-autorecon2-cp", 
                        "-autorecon2-wm", "-autorecon2-pial")
                      flag <- params$segmentation$flag %OF% autorecon_flags
                      raveio::cmd_run_recon_all(subject = subject, 
                        mri_path = mri_path, args = flag, overwrite = overwrite, 
                        dry_run = TRUE, verbose = FALSE, command_path = cmd_tools$freesurfer)
                    }, `recon-all-clinical.sh` = {
                      raveio::cmd_run_recon_all_clinical(subject = subject, 
                        mri_path = mri_path, overwrite = overwrite, 
                        dry_run = TRUE, verbose = FALSE, command_path = cmd_tools$freesurfer)
                    }, `YAEL+recon-all` = {
                      raveio::cmd_run_yael_preprocess(subject_code = subject$subject_code, 
                        t1w_path = mri_path, normalize_template = c("mni_icbm152_nlin_asym_09b", 
                          "mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09c"), 
                        run_recon_all = TRUE, dry_run = TRUE, 
                        verbose = FALSE)
                    }, `ants-preprocessing` = {
                      raveio::cmd_run_r(dry_run = TRUE, verbose = FALSE, 
                        quoted = TRUE, expr = bquote({
                          subject <- raveio::as_rave_subject(.(subject$subject_id))
                          mri_src <- .(mri_path)
                          ants_dirpath <- file.path(subject$preprocess_settings$raw_path, 
                            "rave-imaging", "ants")
                          raveio::ants_preprocessing(work_path = ants_dirpath, 
                            image_path = mri_src, resample = TRUE, 
                            verbose = TRUE, template_subject = .(ravepipeline::raveio_getopt("threeBrain_template_subject")))
                          deriv_path <- file.path(subject$preprocess_settings$raw_path, 
                            "rave-imaging", "derivative")
                          raveio::dir_create2(deriv_path)
                          file.copy(from = mri_src, to = file.path(deriv_path, 
                            .(sprintf("MRI_RAW.%s", mri_postfix))), 
                            overwrite = TRUE, recursive = FALSE, 
                            copy.mode = TRUE, copy.date = TRUE)
                          file.copy(from = file.path(ants_dirpath, 
                            "mri", "resampled.nii.gz"), to = file.path(deriv_path, 
                            "T1-ants.nii.gz"), overwrite = TRUE, 
                            recursive = FALSE, copy.mode = TRUE, 
                            copy.date = TRUE)
                          message("Done")
                        }))
                    }, {
                      raveio::cmd_run_r(dry_run = TRUE, verbose = FALSE, 
                        quoted = TRUE, expr = bquote({
                          subject <- raveio::as_rave_subject(.(subject$subject_id))
                          image_path <- file.path(subject$preprocess_settings$raw_path, 
                            "rave-imaging")
                          mri_src <- .(mri_path)
                          mri_dirpath <- file.path(image_path, 
                            "fs", "mri")
                          mri_dst <- file.path(mri_dirpath, .(sprintf("brain.%s", 
                            mri_postfix)))
                          raveio::dir_create2(mri_dirpath)
                          file.copy(from = mri_src, to = mri_dst, 
                            overwrite = .(overwrite), recursive = FALSE, 
                            copy.mode = TRUE, copy.date = TRUE)
                          deriv_path <- file.path(image_path, 
                            "derivative")
                          raveio::dir_create2(deriv_path)
                          file.copy(from = mri_src, to = file.path(deriv_path, 
                            .(sprintf("MRI_RAW.%s", mri_postfix))), 
                            overwrite = TRUE, recursive = FALSE, 
                            copy.mode = TRUE, copy.date = TRUE)
                          message("Done")
                        }))
                    })
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                image_segmentation
            }), target_depends = c("params", "check_result", 
            "subject", "cmd_tools")), deps = c("params", "check_result", 
        "subject", "cmd_tools"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), CT_MR_coregistration_via_FSL = targets::tar_target_raw(name = "coreg_flirt", 
        command = quote({
            .__target_expr__. <- quote({
                coreg_flirt <- tryCatch({
                  mri_path <- file.path(check_result$path_temp, 
                    "derivative", params$flirt$reference)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    mri_path <- params$nii_t1
                    mri_root <- file.path(check_result$path_temp, 
                      "inputs", "MRI")
                    mri_path <- file.path(mri_root, mri_path)
                  }
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a valid MRI Nifti file")
                  }
                  ct_path <- params$nii_ct
                  ct_root <- file.path(check_result$path_temp, 
                    "inputs", "CT")
                  ct_path <- file.path(ct_root, ct_path)
                  if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                    stop("Please choose a valid CT Nifti file under ", 
                      ct_root)
                  }
                  raveio::cmd_run_flirt(subject = subject, mri_path = mri_path, 
                    ct_path = ct_path, dof = params$flirt$dof, 
                    cost = params$flirt$cost, search = params$flirt$search, 
                    searchcost = params$flirt$searchcost, overwrite = FALSE, 
                    command_path = cmd_tools$flirt, dry_run = TRUE, 
                    verbose = FALSE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(coreg_flirt)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "coreg_flirt", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "coreg_flirt", target_expr = quote({
                {
                  coreg_flirt <- tryCatch({
                    mri_path <- file.path(check_result$path_temp, 
                      "derivative", params$flirt$reference)
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      mri_path <- params$nii_t1
                      mri_root <- file.path(check_result$path_temp, 
                        "inputs", "MRI")
                      mri_path <- file.path(mri_root, mri_path)
                    }
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      stop("Please choose a valid MRI Nifti file")
                    }
                    ct_path <- params$nii_ct
                    ct_root <- file.path(check_result$path_temp, 
                      "inputs", "CT")
                    ct_path <- file.path(ct_root, ct_path)
                    if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                      stop("Please choose a valid CT Nifti file under ", 
                        ct_root)
                    }
                    raveio::cmd_run_flirt(subject = subject, 
                      mri_path = mri_path, ct_path = ct_path, 
                      dof = params$flirt$dof, cost = params$flirt$cost, 
                      search = params$flirt$search, searchcost = params$flirt$searchcost, 
                      overwrite = FALSE, command_path = cmd_tools$flirt, 
                      dry_run = TRUE, verbose = FALSE)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                coreg_flirt
            }), target_depends = c("check_result", "params", 
            "subject", "cmd_tools")), deps = c("check_result", 
        "params", "subject", "cmd_tools"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), CT_MR_coregistration_via_nipy = targets::tar_target_raw(name = "coreg_nipy", 
        command = quote({
            .__target_expr__. <- quote({
                coreg_nipy <- tryCatch({
                  mri_path <- file.path(check_result$path_temp, 
                    "derivative", params$nipy$reference)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    mri_path <- params$nii_t1
                    mri_root <- file.path(check_result$path_temp, 
                      "inputs", "MRI")
                    mri_path <- file.path(mri_root, mri_path)
                  }
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a valid MRI Nifti file")
                  }
                  ct_path <- params$nii_ct
                  ct_root <- file.path(check_result$path_temp, 
                    "inputs", "CT")
                  ct_path <- file.path(ct_root, ct_path)
                  if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                    stop("Please choose a valid CT Nifti file. Current file is missing: ", 
                      ct_path)
                  }
                  raveio::cmd_run_nipy_coreg(subject = subject, 
                    mri_path = mri_path, ct_path = ct_path, clean_source = params$nipy$clean_source, 
                    inverse_target = params$nipy$inverse_target, 
                    precenter_source = params$nipy$precenter_source, 
                    reg_type = params$nipy$reg_type, interp = params$nipy$interp, 
                    similarity = params$nipy$similarity, optimizer = params$nipy$optimizer, 
                    dry_run = TRUE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(coreg_nipy)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "coreg_nipy", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "coreg_nipy", target_expr = quote({
                {
                  coreg_nipy <- tryCatch({
                    mri_path <- file.path(check_result$path_temp, 
                      "derivative", params$nipy$reference)
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      mri_path <- params$nii_t1
                      mri_root <- file.path(check_result$path_temp, 
                        "inputs", "MRI")
                      mri_path <- file.path(mri_root, mri_path)
                    }
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      stop("Please choose a valid MRI Nifti file")
                    }
                    ct_path <- params$nii_ct
                    ct_root <- file.path(check_result$path_temp, 
                      "inputs", "CT")
                    ct_path <- file.path(ct_root, ct_path)
                    if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                      stop("Please choose a valid CT Nifti file. Current file is missing: ", 
                        ct_path)
                    }
                    raveio::cmd_run_nipy_coreg(subject = subject, 
                      mri_path = mri_path, ct_path = ct_path, 
                      clean_source = params$nipy$clean_source, 
                      inverse_target = params$nipy$inverse_target, 
                      precenter_source = params$nipy$precenter_source, 
                      reg_type = params$nipy$reg_type, interp = params$nipy$interp, 
                      similarity = params$nipy$similarity, optimizer = params$nipy$optimizer, 
                      dry_run = TRUE)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                coreg_nipy
            }), target_depends = c("check_result", "params", 
            "subject")), deps = c("check_result", "params", "subject"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), CT_MR_coregistration_via_NiftyReg = targets::tar_target_raw(name = "coreg_niftyreg", 
        command = quote({
            .__target_expr__. <- quote({
                coreg_niftyreg <- tryCatch({
                  mri_path <- file.path(check_result$path_temp, 
                    "derivative", params$nipy$reference)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    mri_path <- params$nii_t1
                    mri_root <- file.path(check_result$path_temp, 
                      "inputs", "MRI")
                    mri_path <- file.path(mri_root, mri_path)
                  }
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a valid MRI Nifti file")
                  }
                  ct_path <- params$nii_ct
                  ct_root <- file.path(check_result$path_temp, 
                    "inputs", "CT")
                  ct_path <- file.path(ct_root, ct_path)
                  if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                    stop("Please choose a valid CT Nifti file. Current file is missing: ", 
                      ct_path)
                  }
                  raveio::cmd_run_niftyreg_coreg(subject = subject, 
                    mri_path = mri_path, ct_path = ct_path, reg_type = params$niftyreg$reg_type, 
                    interp = params$niftyreg$interp, verbose = FALSE, 
                    dry_run = TRUE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(coreg_niftyreg)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "coreg_niftyreg", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "coreg_niftyreg", target_expr = quote({
                {
                  coreg_niftyreg <- tryCatch({
                    mri_path <- file.path(check_result$path_temp, 
                      "derivative", params$nipy$reference)
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      mri_path <- params$nii_t1
                      mri_root <- file.path(check_result$path_temp, 
                        "inputs", "MRI")
                      mri_path <- file.path(mri_root, mri_path)
                    }
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      stop("Please choose a valid MRI Nifti file")
                    }
                    ct_path <- params$nii_ct
                    ct_root <- file.path(check_result$path_temp, 
                      "inputs", "CT")
                    ct_path <- file.path(ct_root, ct_path)
                    if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                      stop("Please choose a valid CT Nifti file. Current file is missing: ", 
                        ct_path)
                    }
                    raveio::cmd_run_niftyreg_coreg(subject = subject, 
                      mri_path = mri_path, ct_path = ct_path, 
                      reg_type = params$niftyreg$reg_type, interp = params$niftyreg$interp, 
                      verbose = FALSE, dry_run = TRUE)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                coreg_niftyreg
            }), target_depends = c("check_result", "params", 
            "subject")), deps = c("check_result", "params", "subject"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), CT_MR_coregistration_via_ANTs = targets::tar_target_raw(name = "coreg_ants", 
        command = quote({
            .__target_expr__. <- quote({
                coreg_ants <- tryCatch({
                  mri_path <- file.path(check_result$path_temp, 
                    "derivative", params$nipy$reference)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    mri_path <- params$nii_t1
                    mri_root <- file.path(check_result$path_temp, 
                      "inputs", "MRI")
                    mri_path <- file.path(mri_root, mri_path)
                  }
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a valid MRI Nifti file")
                  }
                  ct_path <- params$nii_ct
                  ct_root <- file.path(check_result$path_temp, 
                    "inputs", "CT")
                  ct_path <- file.path(ct_root, ct_path)
                  if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                    stop("Please choose a valid CT Nifti file. Current file is missing: ", 
                      ct_path)
                  }
                  raveio::cmd_run_ants_coreg(subject = subject, 
                    mri_path = mri_path, ct_path = ct_path, reg_type = params$ants$reg_type, 
                    aff_metric = params$ants$aff_metric, syn_metric = params$ants$syn_metric, 
                    verbose = FALSE, dry_run = TRUE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(coreg_ants)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "coreg_ants", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "coreg_ants", target_expr = quote({
                {
                  coreg_ants <- tryCatch({
                    mri_path <- file.path(check_result$path_temp, 
                      "derivative", params$nipy$reference)
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      mri_path <- params$nii_t1
                      mri_root <- file.path(check_result$path_temp, 
                        "inputs", "MRI")
                      mri_path <- file.path(mri_root, mri_path)
                    }
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      stop("Please choose a valid MRI Nifti file")
                    }
                    ct_path <- params$nii_ct
                    ct_root <- file.path(check_result$path_temp, 
                      "inputs", "CT")
                    ct_path <- file.path(ct_root, ct_path)
                    if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                      stop("Please choose a valid CT Nifti file. Current file is missing: ", 
                        ct_path)
                    }
                    raveio::cmd_run_ants_coreg(subject = subject, 
                      mri_path = mri_path, ct_path = ct_path, 
                      reg_type = params$ants$reg_type, aff_metric = params$ants$aff_metric, 
                      syn_metric = params$ants$syn_metric, verbose = FALSE, 
                      dry_run = TRUE)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                coreg_ants
            }), target_depends = c("check_result", "params", 
            "subject")), deps = c("check_result", "params", "subject"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), CT_MR_coregistration_via_AFNI = targets::tar_target_raw(name = "coreg_3dallineate", 
        command = quote({
            .__target_expr__. <- quote({
                coreg_3dallineate <- tryCatch({
                  mri_path <- params$nii_t1
                  mri_root <- file.path(check_result$path_temp, 
                    "inputs", "MRI")
                  mri_path <- file.path(mri_root, mri_path)
                  if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                    stop("Please choose a valid MRI Nifti file under ", 
                      mri_root)
                  }
                  ct_path <- params$nii_ct
                  ct_root <- file.path(check_result$path_temp, 
                    "inputs", "CT")
                  ct_path <- file.path(ct_root, ct_path)
                  if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                    stop("Please choose a valid CT Nifti file under ", 
                      ct_root)
                  }
                  raveio::cmd_run_3dAllineate(subject = subject, 
                    mri_path = mri_path, ct_path = ct_path, overwrite = FALSE, 
                    command_path = cmd_tools$afni, dry_run = TRUE, 
                    verbose = FALSE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(coreg_3dallineate)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "coreg_3dallineate", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "coreg_3dallineate", target_expr = quote({
                {
                  coreg_3dallineate <- tryCatch({
                    mri_path <- params$nii_t1
                    mri_root <- file.path(check_result$path_temp, 
                      "inputs", "MRI")
                    mri_path <- file.path(mri_root, mri_path)
                    if (!path_is_valid(mri_path) || dir.exists(mri_path)) {
                      stop("Please choose a valid MRI Nifti file under ", 
                        mri_root)
                    }
                    ct_path <- params$nii_ct
                    ct_root <- file.path(check_result$path_temp, 
                      "inputs", "CT")
                    ct_path <- file.path(ct_root, ct_path)
                    if (!path_is_valid(ct_path) || dir.exists(ct_path)) {
                      stop("Please choose a valid CT Nifti file under ", 
                        ct_root)
                    }
                    raveio::cmd_run_3dAllineate(subject = subject, 
                      mri_path = mri_path, ct_path = ct_path, 
                      overwrite = FALSE, command_path = cmd_tools$afni, 
                      dry_run = TRUE, verbose = FALSE)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                coreg_3dallineate
            }), target_depends = c("params", "check_result", 
            "subject", "cmd_tools")), deps = c("params", "check_result", 
        "subject", "cmd_tools"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), morph_MRI_to_template = targets::tar_target_raw(name = "morphmri_ants", 
        command = quote({
            .__target_expr__. <- quote({
                morphmri_ants <- tryCatch({
                  template_brain <- params$template_brain
                  if (length(template_brain) != 1) {
                    template_brain <- getOption("threeBrain.template_subject", 
                      "fsaverage")
                  }
                  raveio::cmd_run_ants_mri_to_template(subject = subject, 
                    template_subject = template_brain, verbose = FALSE, 
                    dry_run = TRUE)
                }, error = function(e) {
                  list(error = TRUE, condition = e)
                })
            })
            tryCatch({
                eval(.__target_expr__.)
                return(morphmri_ants)
            }, error = function(e) {
                asNamespace("ravepipeline")$resolve_pipeline_error(name = "morphmri_ants", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("ravepipeline")$target_format_dynamic(name = NULL, 
            target_export = "morphmri_ants", target_expr = quote({
                {
                  morphmri_ants <- tryCatch({
                    template_brain <- params$template_brain
                    if (length(template_brain) != 1) {
                      template_brain <- getOption("threeBrain.template_subject", 
                        "fsaverage")
                    }
                    raveio::cmd_run_ants_mri_to_template(subject = subject, 
                      template_subject = template_brain, verbose = FALSE, 
                      dry_run = TRUE)
                  }, error = function(e) {
                    list(error = TRUE, condition = e)
                  })
                }
                morphmri_ants
            }), target_depends = c("params", "subject")), deps = c("params", 
        "subject"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"))
