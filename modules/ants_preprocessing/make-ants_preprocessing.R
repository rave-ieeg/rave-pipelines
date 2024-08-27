library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
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
    input_debug = targets::tar_target_raw("debug", quote({
        settings[["debug"]]
    }), deps = "settings"), input_calculate_cortical_thickness = targets::tar_target_raw("calculate_cortical_thickness", 
        quote({
            settings[["calculate_cortical_thickness"]]
        }), deps = "settings"), input_resample = targets::tar_target_raw("resample", 
        quote({
            settings[["resample"]]
        }), deps = "settings"), input_image_path = targets::tar_target_raw("image_path", 
        quote({
            settings[["image_path"]]
        }), deps = "settings"), collect_paths = targets::tar_target_raw(name = "image_path_normalized", 
        command = quote({
            .__target_expr__. <- quote({
                image_path_normalized <- normalizePath(image_path, 
                  mustWork = TRUE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(image_path_normalized)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "image_path_normalized", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "image_path_normalized", target_expr = quote({
                {
                  image_path_normalized <- normalizePath(image_path, 
                    mustWork = TRUE)
                }
                image_path_normalized
            }), target_depends = "image_path"), deps = "image_path", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    load_original_image = targets::tar_target_raw(name = "image_original", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("image_original = ants.image_read(image_path_normalized)", 
                "if debug:", "  image_original.plot(black_bg=False, nslices=12, ncol=4)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "image_original", paste(e$message, collapse = "\n"), 
                  "image_original", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("image_path_normalized", "debug"
                ))) {
                  args <- structure(names = c("image_path_normalized", 
                  "debug"), lapply(c("image_path_normalized", 
                  "debug"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["image_original"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("image_path_normalized", "debug"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "image_original")), resample_image = targets::tar_target_raw(name = "image_resampled", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("image_resampled = image_original", 
                "if resample == True:", "  image_resampled = ants.resample_image(", 
                "    image = image_original, ", "    resample_params = (256,256,256), ", 
                "    use_voxels = True, ", "    interp_type = 4", 
                "  )", "  if debug:", "    image_resampled.plot(black_bg=False, nslices=12, ncol=4)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "image_resampled", paste(e$message, collapse = "\n"), 
                  "image_resampled", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("resample", "image_original"))) {
                  args <- structure(names = c("resample", "image_original"
                  ), lapply(c("resample", "image_original"), 
                    get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["image_resampled"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("resample", "image_original"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "image_resampled")), register_to_template = targets::tar_target_raw(name = "transforms", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("transforms = antspynet.preprocess_brain_image(", 
                "  image_resampled,", "  truncate_intensity = (0.01, 0.99),", 
                "  brain_extraction_modality = \"t1\",", "  template = \"croppedMni152\",", 
                "  template_transform_type = \"antsRegistrationSyNRepro[a]\",", 
                "  do_bias_correction = True,", "  do_denoising = True", 
                ")", "skull_strip_template = transforms['preprocessed_image'].clone()", 
                "skull_strip_template[transforms['brain_mask'] == 0] = 0", 
                "transforms[\"skull_strip\"] = skull_strip_template"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "transforms", paste(e$message, collapse = "\n"), 
                  "transforms", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length("image_resampled")) {
                  args <- structure(names = "image_resampled", 
                    lapply("image_resampled", get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["transforms"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = "image_resampled", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "transforms")), normalize_brain = targets::tar_target_raw(name = "normalized", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("normalized = ants.apply_transforms(", 
                "  fixed = image_resampled,", "  moving = transforms['preprocessed_image'],", 
                "  transformlist = transforms['template_transforms']['invtransforms'],", 
                "  whichtoinvert = [True], interpolator=\"linear\", verbose = True)", 
                "if debug:", "  normalized.plot(black_bg=False, nslices=12, ncol=4)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "normalized", paste(e$message, collapse = "\n"), 
                  "normalized", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("image_resampled", "transforms", 
                "debug"))) {
                  args <- structure(names = c("image_resampled", 
                  "transforms", "debug"), lapply(c("image_resampled", 
                  "transforms", "debug"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["normalized"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("image_resampled", "transforms", "debug"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "normalized")), generate_brain_mask = targets::tar_target_raw(name = "brain_mask", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("brain_mask = ants.apply_transforms(", 
                "  fixed = image_resampled,", "  moving = transforms['brain_mask'],", 
                "  transformlist=transforms['template_transforms']['invtransforms'],", 
                "  whichtoinvert = [True], interpolator=\"linear\", verbose = True)", 
                "if debug:", "  brain_mask.plot(black_bg=False, nslices=12, ncol=4)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "brain_mask", paste(e$message, collapse = "\n"), 
                  "brain_mask", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("image_resampled", "transforms", 
                "debug"))) {
                  args <- structure(names = c("image_resampled", 
                  "transforms", "debug"), lapply(c("image_resampled", 
                  "transforms", "debug"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["brain_mask"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("image_resampled", "transforms", "debug"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "brain_mask")), strip_skull = targets::tar_target_raw(name = "skull_strip", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("skull_strip = normalized.clone()", 
                "skull_strip[brain_mask == 0] = 0", "if debug:", 
                "  skull_strip.plot(black_bg=False, nslices=12, ncol=4)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "skull_strip", paste(e$message, collapse = "\n"), 
                  "skull_strip", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("normalized", "brain_mask", "debug"
                ))) {
                  args <- structure(names = c("normalized", "brain_mask", 
                  "debug"), lapply(c("normalized", "brain_mask", 
                  "debug"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["skull_strip"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("normalized", "brain_mask", "debug"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "skull_strip")), segmentation_using_Atropos = targets::tar_target_raw(name = "atropos_template", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("atropos_template = antspynet.deep_atropos(", 
                "  t1 = transforms[\"skull_strip\"],", "  do_preprocessing = False, ", 
                "  use_spatial_priors = True, ", "  verbose = True)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "atropos_template", paste(e$message, collapse = "\n"), 
                  "atropos_template", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length("transforms")) {
                  args <- structure(names = "transforms", lapply("transforms", 
                    get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["atropos_template"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = "transforms", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "atropos_template")), obtain_Atropos_on_native_brain = targets::tar_target_raw(name = "atropos_native", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("transform_list = transforms['template_transforms']['invtransforms']", 
                "atropos_native = {}", "atropos_native['segmentation_image'] = ants.apply_transforms(", 
                "  fixed = image_resampled,", "  moving = atropos_template['segmentation_image'],", 
                "  transformlist = transform_list,", "  whichtoinvert = [True], interpolator=\"nearestNeighbor\", verbose = False)", 
                "", "probability_images = []", "atropos_native['probability_images'] = probability_images", 
                "for img in atropos_template['probability_images']:", 
                "  probability_images.append(ants.apply_transforms(", 
                "    fixed = image_resampled,", "    moving = img,", 
                "    transformlist = transform_list,", "    whichtoinvert = [True], interpolator=\"linear\", verbose = False", 
                "  ))")
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "atropos_native", paste(e$message, collapse = "\n"), 
                  "atropos_native", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("image_resampled", "atropos_template", 
                "transforms"))) {
                  args <- structure(names = c("image_resampled", 
                  "atropos_template", "transforms"), lapply(c("image_resampled", 
                  "atropos_template", "transforms"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["atropos_native"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("image_resampled", "atropos_template", "transforms"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "atropos_native")), `segmentation_using_Desikan-Killiang-Tourville_labeling` = targets::tar_target_raw(name = "DKTatlas_template", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("DKTatlas_template = antspynet.desikan_killiany_tourville_labeling(", 
                "  t1 = transforms['skull_strip'],", "  do_preprocessing = False, ", 
                "  return_probability_images = False,", "  do_lobar_parcellation = False, ", 
                "  verbose = True ", ")")
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "DKTatlas_template", paste(e$message, collapse = "\n"), 
                  "DKTatlas_template", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length("transforms")) {
                  args <- structure(names = "transforms", lapply("transforms", 
                    get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["DKTatlas_template"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = "transforms", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "DKTatlas_template")), obtain_DKT_on_native_brain = targets::tar_target_raw(name = "DKTatlas_native", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("DKTatlas_native = ants.apply_transforms(", 
                "  fixed = image_resampled,", "  moving = DKTatlas_template,", 
                "  transformlist=transforms['template_transforms']['invtransforms'],", 
                "  whichtoinvert = [True], interpolator=\"nearestNeighbor\", verbose = True)", 
                "if debug:", "  DKTatlas_native.plot(cmap = \"Set2\", nslices=12, ncol=4)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "DKTatlas_native", paste(e$message, collapse = "\n"), 
                  "DKTatlas_native", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("transforms", "DKTatlas_template", 
                "image_resampled", "debug"))) {
                  args <- structure(names = c("transforms", "DKTatlas_template", 
                  "image_resampled", "debug"), lapply(c("transforms", 
                  "DKTatlas_template", "image_resampled", "debug"
                  ), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["DKTatlas_native"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("transforms", "DKTatlas_template", "image_resampled", 
        "debug"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "DKTatlas_native")), cortical_thickness_using_Kelly_Kapowski = targets::tar_target_raw(name = "cortical_thickness", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("cortical_thickness = None", "", "if calculate_cortical_thickness:", 
                "  # https://www.medrxiv.org/content/10.1101/2020.10.19.20215392v1.full", 
                "  kk_segmentation = ants.image_clone(atropos_native['segmentation_image'])", 
                "  kk_segmentation[kk_segmentation == 4] = 3", 
                "  gray_matter = atropos_native['probability_images'][2]", 
                "  white_matter = (", "    atropos_native['probability_images'][3] + ", 
                "    atropos_native['probability_images'][4]", 
                "  )", "  cortical_thickness = ants.kelly_kapowski(", 
                "    s=kk_segmentation, g=gray_matter, w=white_matter,", 
                "    its=45, r=0.025, m=1.5, x=0, verbose=1)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "cortical_thickness", paste(e$message, collapse = "\n"), 
                  "cortical_thickness", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("atropos_native", "calculate_cortical_thickness"
                ))) {
                  args <- structure(names = c("atropos_native", 
                  "calculate_cortical_thickness"), lapply(c("atropos_native", 
                  "calculate_cortical_thickness"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["cortical_thickness"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("atropos_native", "calculate_cortical_thickness"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "cortical_thickness")), DKT_propagate_through_cortex = targets::tar_target_raw(name = "DKT_propagated", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("DKT_propagated = None", "", "if calculate_cortical_thickness:", 
                "  dtk_cortical_mask = ants.threshold_image(", 
                "    image=DKTatlas_native, low_thresh=1000, ", 
                "    high_thresh=3000, inval=1, outval=0)", "  ", 
                "  dtk = dtk_cortical_mask * DKTatlas_native", 
                "  ", "  kk_mask = ants.threshold_image(", "    image=cortical_thickness, low_thresh=0,", 
                "    high_thresh = 0, inval = 0, outval = 1)", 
                "  ", "  DKT_propagated = ants.iMath(kk_mask, \"PropagateLabelsThroughMask\", kk_mask * dtk)"
                )
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "DKT_propagated", paste(e$message, collapse = "\n"), 
                  "DKT_propagated", paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("DKTatlas_native", "cortical_thickness", 
                "calculate_cortical_thickness"))) {
                  args <- structure(names = c("DKTatlas_native", 
                  "cortical_thickness", "calculate_cortical_thickness"
                  ), lapply(c("DKTatlas_native", "cortical_thickness", 
                  "calculate_cortical_thickness"), get, envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["DKT_propagated"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("DKTatlas_native", "cortical_thickness", 
        "calculate_cortical_thickness"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "DKT_propagated")), get_average_regional_thickness_values = targets::tar_target_raw(name = "cortical_thickness_regional_stats", 
        command = quote({
            .py_error_handler <- function(e, use_py_last_error = TRUE) {
                if (use_py_last_error) {
                  e2 <- asNamespace("reticulate")$py_last_error()
                  if (!is.null(e2)) {
                    e <- e2
                  }
                }
                code <- c("cortical_thickness_regional_stats = None", 
                "", "if calculate_cortical_thickness:", "  cortical_thickness_regional_stats = ants.label_stats(", 
                "    cortical_thickness, DKT_propagated)")
                stop(sprintf("Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------", 
                  "cortical_thickness_regional_stats", paste(e$message, 
                    collapse = "\n"), "cortical_thickness_regional_stats", 
                  paste(code, collapse = "\n")))
            }
            re <- withCallingHandlers(expr = {
                .env <- environment()
                if (length(c("calculate_cortical_thickness", 
                "DKT_propagated", "cortical_thickness"))) {
                  args <- structure(names = c("calculate_cortical_thickness", 
                  "DKT_propagated", "cortical_thickness"), lapply(c("calculate_cortical_thickness", 
                  "DKT_propagated", "cortical_thickness"), get, 
                    envir = .env))
                } else {
                  args <- list()
                }
                module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, 
                  must_work = TRUE)
                target_function <- module$rave_pipeline_adapters["cortical_thickness_regional_stats"]
                re <- do.call(target_function, args)
                cls <- class(re)
                if (length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
                  error_message <- rpymat::py_to_r(re$`__str__`())
                  .py_error_handler(simpleError(error_message), 
                    use_py_last_error = FALSE)
                }
                return(re)
            }, python.builtin.BaseException = .py_error_handler, 
                python.builtin.Exception = .py_error_handler, 
                py_error = .py_error_handler)
            return(re)
        }), deps = c("calculate_cortical_thickness", "DKT_propagated", 
        "cortical_thickness"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "cortical_thickness_regional_stats")))
