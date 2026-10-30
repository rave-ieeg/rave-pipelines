# Test the MCP tools of the MRI/CT Preprocessing module the way an agent
# would, without clicking in the browser. A browser session with the module
# open is still needed: input updates round-trip through it.
#
# PRIVACY: this module may only be tested on YAEL/yael_demo_001. Never point
# this test at another subject.
#
# The test runs real commands on the demo subject (imports, YAEL-reg3d, ANTs and
# NiftyReg co-registration, one report) and then restores it: it copies the subject's
# imaging folder and `notes/`, the module's `settings.yaml` and `_targets.yaml`
# first, puts them back at the end (also when a check fails), checks the
# restored files by checksum, and deletes the report folders it created.
# `do_write <- FALSE` stops after the read-only checks and the error cases.
# Needs `rsync`.

module <- "yael_preprocess"
source("agents/skills/build-module-mcp/test-common.R")  # shared MCP test helpers

# test subject and settings
project_name <- "YAEL"
subject_code <- "yael_demo_001"                        # the only subject allowed
raw_mri      <- "rave-uploads/MRI/MRI_reference.nii.gz" # relative to the raw folder
raw_ct       <- "rave-uploads/CT/CT_RAW.nii.gz"
viewer       <- "acpc_3dviewer"
do_write     <- TRUE

subject <- ravecore::RAVESubject$new(project_name = project_name,
                                     subject_code = subject_code, strict = FALSE)
raw_path     <- subject$preprocess_settings$raw_path
imaging_path <- subject$imaging_path
notes_path   <- subject$note_path
report_path  <- subject$report_path

# ---- helpers ------------------------------------------------------------------

# Value of `key=` in a script's labelled lines
field <- function(lines, key) {
  prefix <- paste0(key, "=")
  sub(prefix, "", lines[startsWith(lines, prefix)], fixed = TRUE)
}

status <- function() run_script("imaging_status", .quiet = TRUE)$result

md5 <- function(path) unname(tools::md5sum(path))

# Written since `since`: the import scripts copy with `copy.date = TRUE`, which
# keeps the source's modification time, so check the inode change time
written_since <- function(path, since) {
  isTRUE(file.exists(path) && file.info(path)$ctime >= since - 1)
}

# Checksums of every file under `root` (relative names)
manifest <- function(root) {
  files <- list.files(root, recursive = TRUE, all.files = TRUE)
  files <- sort(files[basename(files) != ".DS_Store"])
  structure(md5(file.path(root, files)), names = files)
}

rsync <- function(from, to) {
  code <- system2("rsync", c("-a", "--delete", shQuote(paste0(from, "/")),
                             shQuote(paste0(to, "/"))))
  if (!identical(code, 0L)) stop("rsync failed: ", from, " -> ", to)
}

# Wait until the latest command from RAVE has finished, with this log file
wait_command <- function(log_file, timeout) {
  wait_until(function() {
    lines <- run_script("command_status", .quiet = TRUE)$result
    identical(field(lines, "state"), "finished") &&
      identical(field(lines, "log"), log_file)
  }, sprintf("the command logging to %s", log_file), timeout = timeout,
  interval = 3)
  lines <- run_script("command_status")$result
  wait_until(function() on_page("#yael_preprocess-dismiss_modal"), "the log dialog")
  operate("click", target = "dismiss_modal")
  wait_until(function() dialog_title() == "", "the log dialog to close")
  lines
}

conf_coreg <- function() {
  yaml::read_yaml(file.path(imaging_path, "derivative", "conf-coregistration.yaml"))
}

# ---- backup -------------------------------------------------------------------

backup <- tempfile("yael_preprocess_test_")
dir.create(backup)
rsync(imaging_path, file.path(backup, "imaging"))
rsync(notes_path, file.path(backup, "notes"))
file.copy(file.path("modules", module, c("settings.yaml", "_targets.yaml")), backup)
imaging_before <- manifest(imaging_path)
notes_before <- manifest(notes_path)
reports_before <- list.files(report_path)
cat("Backed up the subject's imaging folder and notes to", backup, "\n")

restore <- function() {
  rsync(file.path(backup, "imaging"), imaging_path)
  rsync(file.path(backup, "notes"), notes_path)
  file.copy(file.path(backup, c("settings.yaml", "_targets.yaml")),
            file.path("modules", module), overwrite = TRUE)
  new_reports <- setdiff(list.files(report_path), reports_before)
  unlink(file.path(report_path, new_reports), recursive = TRUE)
  check_all(c(
    "imaging folder restored" = identical(manifest(imaging_path), imaging_before),
    "notes restored" = identical(manifest(notes_path), notes_before),
    "new report folders removed" = setequal(list.files(report_path), reports_before)
  ), "restore")
  cat("Restored the subject; removed report folders:",
      if (length(new_reports)) paste(new_reports, collapse = ", ") else "none", "\n")
}

tryCatch({

  # ---- 0. the app, the module, its tools and scripts ---------------------------

  if (!app_running()) stop("No app at ", base)
  if (!module_open()) stop("Module `", module, "` is not open in a browser session")
  tools <- module_tools()
  scripts <- list_scripts(.quiet = TRUE)$scripts
  script_names <- vapply(scripts, `[[`, "", "name")
  expected_scripts <- c(
    "load_data", "run_analysis", "imaging_status", "command_status",
    "import_t1", "import_ct", "start_acpc_viewer", "save_acpc",
    "save_mri_preprocessing_script", "run_coregistration",
    "save_coregistration_script", "generate_coregistration_report",
    "generate_normalization_report"
  )
  described <- vapply(scripts, function(x) !identical(x$description, x$name), FALSE)
  check_all(c(
    "shiny_ui_operate offered" = "tool__shiny_ui_operate" %in% tools,
    "rave_3dviewer_get offered" = "tool__rave_3dviewer_get" %in% tools,
    "rave_3dviewer_set not offered" = !"tool__rave_3dviewer_set" %in% tools,
    "all scripts registered" = all(expected_scripts %in% script_names),
    "every script described" = all(described)
  ), "module setup")

  # ---- 1. load data --------------------------------------------------------------

  set_input_wait("loader_project_name", project_name)
  set_input_wait("loader_subject_code", subject_code)
  set_input_wait("mri_path", raw_mri, timeout = 60)
  set_input_wait("ct_path", raw_ct, timeout = 60)

  # forbidden inputs are read-only
  refused <- function(id, value) {
    isTRUE(attr(tool("tool__shiny_input_update", inputId = id, value = value,
                     .quiet = TRUE), "is_error"))
  }
  check_all(c(
    "cmd_fs_path read-only" = refused("cmd_fs_path", "/tmp"),
    "cmd_dcm2niix_path read-only" = refused("cmd_dcm2niix_path", "/tmp")
  ), "read-only loader inputs")

  loaded <- run_script("load_data")
  check_all(c(
    "subject" = identical(field(loaded$result, "subject"),
                          sprintf("%s/%s", project_name, subject_code)),
    "raw MRI" = identical(field(loaded$result, "raw_mri"), file.path(raw_path, raw_mri)),
    "raw CT" = identical(field(loaded$result, "raw_ct"), file.path(raw_path, raw_ct)),
    "existing FreeSurfer warning" = any(grepl("existing FreeSurfer",
                                              field(loaded$result, "warning"))),
    "next step named" = any(grepl("loader_ready_btn2", loaded$result))
  ), "load_data result")
  wait_until(function() on_page("#yael_preprocess-loader_ready_btn2"), "the Confirmation dialog")
  operate("click", target = "loader_ready_btn2")
  wait_until(data_loaded, "data to load", timeout = 60)

  # ---- 2. state before any write -------------------------------------------------

  before <- status()
  check_all(c(
    "no MRI imported" = identical(field(before, "imported_mri"), "none"),
    "no CT imported" = identical(field(before, "imported_ct"), "none"),
    "no ACPC" = identical(field(before, "acpc"), "none"),
    "old co-registration reported" = grepl("ants_coreg", field(before, "coregistration")),
    "FreeSurfer version" = grepl("version 8\\.0\\.0", field(before, "freesurfer")),
    "memory vs FreeSurfer" = grepl("FreeSurfer 8.0.0 needs 32 GB: enough", field(before, "memory"))
  ), "imaging_status before")

  check_all(c(
    "float option labelled" = identical(
      page_text("#yael_preprocess-param_dcm2niix_float-label"),
      "Philips precise float scaling"),
    "fresh start read-only" = refused("param_fs_fresh_start", TRUE),
    # on a fresh page; section 6 sets it back at the end
    "YAEL-reg3d is the default program" = identical(
      input_info("coreg_ct_program")$current_value, "YAEL-reg3d")
  ), "inputs")

  # import history now saved under `yael_preprocess`, relative to the raw folder
  # (ravecore >= 0.1.1.19); the module saves it whenever it rebuilds commands
  stored <- ravecore::RAVESubject$new(project_name, subject_code, strict = FALSE)
  check_all(c(
    "raw MRI history" = identical(
      stored$get_default("raw_mri_path", namespace = module), raw_mri),
    "raw CT history" = identical(
      stored$get_default("raw_ct_path", namespace = module), raw_ct)
  ), "import history")

  # ---- 3. error cases (no writes) ------------------------------------------------

  # nothing imported yet: the co-registration command cannot be built (the
  # demo's derivative folder already holds an MR image, so the CT is missing)
  failed <- run_script("run_coregistration")
  expect_output(failed, "Please choose a valid (MRI|CT) Nifti file")
  check_all(c(
    "no job started" = identical(field(run_script("command_status", .quiet = TRUE)$result,
                                       "state"), "none (no command was started from RAVE in this session)"),
    "no dialog" = dialog_title() == ""
  ), "co-registration before import")
  err <- run_script_error("start_acpc_viewer")
  if (!grepl("No MRI file specified|Invalid file .* for ACPC alignment", err)) {
    stop("Unexpected error: ", err)
  }

  if (!do_write) stop("do_write is FALSE: stopping before the imports")

  # ---- 4. imports ------------------------------------------------------------------

  t_import <- Sys.time()
  started <- run_script("import_t1")$result
  wait_command(field(started, "log"), timeout = 120)
  started <- run_script("import_ct")$result
  log_tail <- wait_command(field(started, "log"), timeout = 120)
  for (type in c("MRI", "CT")) {
    src <- file.path(raw_path, if (type == "MRI") raw_mri else raw_ct)
    dst <- file.path(imaging_path, "inputs", type, sprintf("%s_RAW.nii.gz", type))
    copy <- file.path(imaging_path, "derivative", sprintf("%s_RAW.nii.gz", type))
    check_all(structure(c(
      written_since(dst, t_import),
      identical(md5(dst), md5(src)),
      identical(md5(copy), md5(src))
    ), names = paste(type, c("imported now", "same as raw", "derivative copy"))),
    paste(type, "import"))
  }
  if (!any(grepl("File copied from", log_tail))) stop("Import log lacks 'File copied from'")

  set_input("param_fs_refresh", "1")
  set_input("param_coreg_refresh", "1")
  set_input_wait("param_fs_infile", "MRI_RAW.nii.gz")
  set_input_wait("param_coreg_ct", "CT_RAW.nii.gz")
  set_input_wait("param_coreg_mri", "MRI_RAW.nii.gz")
  imported <- status()
  check_all(c(
    "MRI listed" = identical(field(imported, "imported_mri"), "MRI_RAW.nii.gz"),
    "CT listed" = identical(field(imported, "imported_ct"), "CT_RAW.nii.gz")
  ), "imaging_status after imports")

  # ---- 5. MRI preprocessing: saved, never run ------------------------------------

  set_input_wait("param_fs_prog", "YAEL+recon-all")
  t_save <- Sys.time()
  wait_until(function() {
    saved <- run_script("save_mri_preprocessing_script", .quiet = TRUE)$result
    identical(field(saved, "program"), "YAEL+recon-all")
  }, "the YAEL+recon-all script", timeout = 30, interval = 1)
  saved <- run_script("save_mri_preprocessing_script")$result
  script_file <- field(saved, "script")
  check_all(c(
    "input MRI" = identical(field(saved, "mri"), "MRI_RAW.nii.gz"),
    "no fresh start" = identical(field(saved, "fresh_start"), "FALSE"),
    "script in the imaging folder" = identical(
      script_file, file.path(imaging_path, "scripts", "cmd-yael-preprocess-full.R")),
    "script written now" = written_since(script_file, t_save),
    "command runs the script" = grepl(script_file, field(saved, "command"), fixed = TRUE),
    "nothing started" = !identical(field(run_script("command_status", .quiet = TRUE)$result,
                                         "state"), "running"),
    "keeps FreeSurfer work" = "overwrite <- FALSE" %in% readLines(script_file)
  ), "save_mri_preprocessing_script")

  # a native entry: the asymmetric and the symmetric template, without FreeSurfer
  set_input_wait("param_fs_prog", "YAEL-native-mni152a")
  wait_until(function() {
    saved <- run_script("save_mri_preprocessing_script", .quiet = TRUE)$result
    identical(field(saved, "program"), "YAEL-native-mni152a")
  }, "the YAEL-native-mni152a script", timeout = 30, interval = 1)
  saved <- run_script("save_mri_preprocessing_script")$result
  script_file <- field(saved, "script")
  native_script <- readLines(script_file)
  check_all(c(
    "native script" = identical(
      script_file, file.path(imaging_path, "scripts", "cmd-yael-preprocess-native-simple.R")),
    "both templates" = paste0('normalize_template <- c("mni_icbm152_nlin_asym_09a", ',
                              '"mni_icbm152_nlin_sym_09a")') %in% native_script,
    "no FreeSurfer" = "run_recon_all <- 0" %in% native_script,
    "keeps existing work" = "overwrite <- FALSE" %in% native_script
  ), "save_mri_preprocessing_script, native entry")

  # ---- 6. co-registration: YAEL-reg3d, report, ANTs, NiftyReg -------------------

  coreg_dir <- file.path(imaging_path, "coregistration")
  set_input_wait("coreg_ct_program", "YAEL-reg3d")
  set_input_wait("coreg_reg3d_type", "Rigid")
  set_input_wait("coreg_reg3d_cost", "mattes")
  set_input_wait("coreg_reg3d_interp", "trilinear")
  Sys.sleep(1)  # the module rebuilds its commands 0.3 s after an input change
  saved <- run_script("save_coregistration_script")$result
  reg3d_script <- file.path(imaging_path, "scripts", "cmd-reg3d-rigid.R")
  reg3d_lines <- readLines(reg3d_script)
  check_all(c(
    "YAEL-reg3d" = identical(field(saved, "program"), "YAEL-reg3d"),
    "script" = identical(field(saved, "script"), reg3d_script),
    "placeholders filled" = !any(grepl("{{", reg3d_lines, fixed = TRUE)),
    "settings in the script" = all(c('reg_type <- "Rigid"', 'cost <- "mattes"',
                                     'interp <- "trilinear"') %in% reg3d_lines)
  ), "YAEL-reg3d script")

  t_reg3d <- Sys.time()
  started <- run_script("run_coregistration")$result
  if (!identical(field(started, "started"), "RAVE+YAEL-reg3d")) stop("Unexpected start: ", started)
  wait_command(field(started, "log"), timeout = 600)
  conf <- conf_coreg()
  ct_ras_to_mr_ras <- as.matrix(read.table(file.path(coreg_dir, "CT_RAS_to_MR_RAS.txt")))
  ct_ijk_to_mr_ras <- as.matrix(read.table(file.path(coreg_dir, "CT_IJK_to_MR_RAS.txt")))
  # electrode localization reads the images with threeBrain (oro.nifti: NIfTI-1)
  ct_header <- threeBrain:::read_nii2(file.path(coreg_dir, "CT_RAW.nii.gz"), head_only = TRUE)
  mr_header <- threeBrain:::read_nii2(file.path(coreg_dir, "MRI_reference.nii.gz"), head_only = TRUE)
  ct_centre <- (ct_ijk_to_mr_ras %*% c((ct_header$get_shape() - 1) / 2, 1))[1:3]
  mr_corners <- mr_header$get_IJK_to_RAS()$matrix %*%
    rbind(t(as.matrix(expand.grid(lapply(mr_header$get_shape() - 1, function(n) c(0, n))))), 1)
  nifti_dim <- function(name) RNifti::niftiHeader(file.path(coreg_dir, name))$dim[2:4]
  check_all(c(
    "aligned CT written now" = written_since(file.path(coreg_dir, "ct_in_t1.nii.gz"), t_reg3d),
    "transform written now" = written_since(file.path(coreg_dir, "CT_IJK_to_MR_RAS.txt"), t_reg3d),
    "YAEL-reg3d recorded" = identical(conf$command$execute, "ravecore::reg3d_rigid"),
    "Rigid, MI, trilinear" = identical(
      unlist(conf$command$arguments[c("reg_type", "cost", "interp")]),
      c(reg_type = "Rigid", cost = "mattes", interp = "trilinear")),
    "CT input" = identical(conf$input_image$path,
                           file.path(imaging_path, "inputs", "CT", "CT_RAW.nii.gz")),
    "MR image" = identical(conf$reference_image$path,
                           file.path(imaging_path, "derivative", "MRI_RAW.nii.gz")),
    "registration record" = file.exists(file.path(coreg_dir, "reg3d", "reg3d.dcf")),
    # images saved at their original resolution, no down-sampled copies
    "CT at full resolution" = identical(nifti_dim("CT_RAW.nii.gz"), c(512L, 512L, 178L)),
    "no original copies" = !any(file.exists(file.path(
      coreg_dir, c("CT_ORIG.nii.gz", "MRI_ORIG.nii.gz")))),
    "status shows it" = grepl("reg3d_rigid", field(status(), "coregistration")),
    # what electrode localization's loader picks (R/loader.R, load_coreg_params)
    "EL: transform mode" = "CT_IJK_to_MR_RAS" %in% names(conf$outputs),
    "EL: images found" = all(c(basename(conf$input_image$backup),
                               basename(conf$reference_image$backup)) %in%
                               list.files(coreg_dir)),
    "EL: transform found" = basename(conf$outputs$CT_IJK_to_MR_RAS$path) %in% list.files(coreg_dir),
    "EL: voxel transform matches CT_RAW" = max(abs(
      ct_ijk_to_mr_ras %*% solve(ct_header$get_IJK_to_RAS()$matrix) - ct_ras_to_mr_ras)) < 1e-3,
    "EL: CT centre inside the MRI" = all(ct_centre >= apply(mr_corners[1:3, ], 1, min) &
                                           ct_centre <= apply(mr_corners[1:3, ], 1, max))
  ), "YAEL-reg3d co-registration")

  generated <- run_script("generate_coregistration_report")$result
  if (!identical(field(generated, "has_coregistration"), "TRUE")) stop("No co-registration?")
  wait_until(function() {
    line <- field(status(), "coregistration_report")
    grepl("report.html ready", line) &&
      !basename(sub(" \\(.*$", "", line)) %in% reports_before
  }, "the co-registration report", timeout = 900, interval = 5)

  set_input_wait("coreg_ct_program", "ANTs")
  set_input_wait("coreg_ants_type", "Rigid")
  Sys.sleep(1)  # the module rebuilds its commands 0.3 s after an input change
  t_ants <- Sys.time()
  started <- run_script("run_coregistration")$result
  if (!identical(field(started, "started"), "RAVE+ANTs")) stop("Unexpected start: ", started)
  wait_command(field(started, "log"), timeout = 900)
  conf <- conf_coreg()
  check_all(c(
    "aligned CT written now" = written_since(
      file.path(imaging_path, "coregistration", "ct_in_t1.nii.gz"), t_ants),
    "ANTs recorded" = identical(conf$command$execute, "ravecore::cmd_run_ants_coreg"),
    "Rigid" = identical(conf$command$arguments$reg_type, "Rigid"),
    "CT input" = identical(conf$input_image$path,
                           file.path(imaging_path, "inputs", "CT", "CT_RAW.nii.gz")),
    "MR image" = identical(conf$reference_image$path,
                           file.path(imaging_path, "derivative", "MRI_RAW.nii.gz"))
  ), "ANTs co-registration")

  set_input_wait("coreg_ct_program", "NiftyReg")
  set_input_wait("coreg_niftyreg_type", "rigid")
  Sys.sleep(1)
  t_nifty <- Sys.time()
  started <- run_script("run_coregistration")$result
  wait_command(field(started, "log"), timeout = 1800)
  conf <- conf_coreg()
  check_all(c(
    "aligned CT written now" = written_since(
      file.path(imaging_path, "coregistration", "ct_in_t1.nii.gz"), t_nifty),
    "NiftyReg recorded" = identical(conf$command$execute, "ravecore::niftyreg_coreg"),
    "rigid" = identical(conf$command$arguments$reg_type, "rigid"),
    "MR image" = identical(conf$reference_image$path,
                           file.path(imaging_path, "derivative", "MRI_RAW.nii.gz")),
    "status shows it" = grepl("niftyreg_coreg", field(status(), "coregistration"))
  ), "NiftyReg co-registration")

  # a program agents only save: FSL
  set_input_wait("coreg_ct_program", "FSL")
  Sys.sleep(1)
  saved <- run_script("save_coregistration_script")$result
  check_all(c(
    "FSL" = identical(field(saved, "program"), "FSL"),
    "script" = identical(field(saved, "script"),
                         file.path(imaging_path, "scripts", "cmd-fsl-flirt.sh")),
    "MR image" = identical(field(saved, "mr_image"), "MRI_RAW.nii.gz")
  ), "save_coregistration_script")
  set_input_wait("coreg_ct_program", "YAEL-reg3d")  # back to the default

  # ---- 7. ACPC: the viewer starts; the person would place AC and PC ---------------

  set_input("param_acpc_refresh", "1")
  set_input_wait("param_acpc_infile", "MRI_RAW.nii.gz")
  set_input("jump_acpc_realign", "1")
  run_script("start_acpc_viewer")
  wait_until(function() {
    tryCatch({ viewer_get("controllers"); TRUE }, error = function(e) FALSE)
  }, "the ACPC viewer", timeout = 120, interval = 2)
  controllers <- names(viewer_get("controllers")$controllers)
  check_all(c(
    "AC/PC controllers" = all(c("AC (ScanRAS)", "PC (ScanRAS)") %in% controllers),
    "instructions shown" = grepl("Click to Register/Reset AC",
                                 page_text("#yael_preprocess-acpc_results"))
  ), "ACPC viewer")

}, finally = {
  restore()
})
