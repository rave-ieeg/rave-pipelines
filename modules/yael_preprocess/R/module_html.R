

# 1. Information
# 2. Import (convert to nii)
# 3. Surface recon
# 4. CT co-registration
dry_run <- isTRUE(ravepipeline::raveio_getopt("cmd_dry_run", default = FALSE))

module_html <- function() {

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Subject Information",
              class_header = "",
              shiny::uiOutput(ns("basic_info"))
            )
          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,

            ravedash::output_card(
              title = "Import DICOM Folders or Nifti Images",
              start_collapsed = TRUE,
              # class_foot = "no-padding",
              append_tools = FALSE,
              footer = shiny::tagList(
                shiny::tags$details(
                  shiny::tags$summary("Terminal script - Import T1 MRI"),
                  shiny::uiOutput(ns("panel_import_T1"), container = shiny::p)
                ),
                shiny::tags$details(
                  shiny::tags$summary("Terminal script - Import CT"),
                  shiny::uiOutput(ns("panel_import_CT"), container = shiny::p)
                )
              ),
              shiny::div(
                "If your original images have DICOM format, ",
                "the following script will run ",
                shiny::pre(
                  class = "pre-compact no-padding display-inline",
                  "dcm2niix"
                ),
                " to obtain the Nifti format. ",
                shiny::hr(),

                shiny::fluidRow(
                  shiny::column(
                    width = 5L,
                    shiny::div(
                      class = "display-inline",
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("param_dcm2niix_merge"),
                          label = "Merge 2D slices from same series regardless of echo, exposure, etc.",
                          choices = c("Auto", "Yes", "No"),
                          selected = "Auto"
                        ),
                        inputId = "param_dcm2niix_merge",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `Auto` (default), `Yes`, `No`] dcm2niix option for a DICOM import:",
                          "merge 2D slices from the same series regardless of echo, exposure, etc.",
                          "(flag `-m`). Ignored when the raw image is a NIfTI file.",
                          "Read by scripts `import_t1` and `import_ct`."
                        ),
                        tooltip = "dcm2niix: merge 2D slices from the same series regardless of echo, exposure, etc. (DICOM folders only)."
                      )
                    )
                  ),
                  shiny::column(
                    width = 5L,
                    shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns("param_dcm2niix_float"),
                        label = "Philips precise float scaling",
                        choices = c("Yes", "No"),
                        selected = "Yes"
                      ),
                      inputId = "param_dcm2niix_float",
                      update = "shiny::updateSelectInput(value=selected)",
                      description = paste(
                        "[Select: `Yes` (default), `No`] dcm2niix option for a DICOM import:",
                        "Philips precise float (not display) scaling (flag `-p`). Ignored when the",
                        "raw image is a NIfTI file. Read by scripts `import_t1` and `import_ct`."
                      ),
                      tooltip = "dcm2niix: use Philips precise float scaling instead of display scaling (DICOM folders only)."
                    )
                  ),
                  shiny::column(
                    width = 2L,
                    shidashi::register_input(
                      shiny::selectInput(
                        inputId = ns("param_dcm2niix_crop"),
                        label = "Crop 3D acquisitions",
                        choices = c("Yes", "No", "Ignore"),
                        selected = "No"
                      ),
                      inputId = "param_dcm2niix_crop",
                      update = "shiny::updateSelectInput(value=selected)",
                      description = paste(
                        "[Select: `No` (default), `Yes`, `Ignore`] dcm2niix option for a DICOM import:",
                        "crop 3D acquisitions (flag `-x`). Ignored when the raw image is a NIfTI",
                        "file. Read by scripts `import_t1` and `import_ct`."
                      ),
                      tooltip = "dcm2niix: crop 3D acquisitions (DICOM folders only)."
                    )
                  )
                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    shiny::actionButton(
                      ns("btn_dcm2niix_run_t1"),
                      "Run from RAVE (T1 MRI)"
                    ),
                    shiny::actionButton(
                      ns("btn_dcm2niix_run_ct"),
                      "Run from RAVE (CT)"
                    )
                    # dipsaus::actionButtonStyled(ns("btn_dcm2niix_copy"), "Save & run by yourself")
                  )
                )
              )
            ),

            ravedash::output_card(
              title = "ACPC Re-alignment (Optional)",
              start_collapsed = TRUE,
              class_body = "no-padding min-height-300 height-500 resize-vertical full-width",
              threeBrain::threejsBrainOutput(
                outputId = ns("acpc_3dviewer"),
                width = "100%",
                height = "100%"
              ),

              footer = shiny::fluidRow(
                shiny::column(
                  width = 7L,
                  shidashi::register_input(
                    shiny::selectInput(
                      inputId = ns("param_acpc_infile"),
                      label = "MRI file (select one to be ACPC re-aligned)",
                      choices = character(0L)
                    ),
                    inputId = "param_acpc_infile",
                    update = "shiny::updateSelectInput(value=selected)",
                    description = paste(
                      "[Select] The imported MRI (a file in the subject's imaging `inputs/MRI`",
                      "folder) to re-align to the AC-PC line; read by script `start_acpc_viewer`.",
                      "Empty until an MRI is imported; refresh the list with `param_acpc_refresh`.",
                      "Agents coach this step: the person places AC and PC in the viewer."
                    ),
                    tooltip = "The imported MRI to re-align to the AC-PC line."
                  )
                ),
                shiny::column(
                  width = 1L,
                  shidashi::register_input(
                    shiny::actionButton(
                      inputId = ns("param_acpc_refresh"),
                      label = "",
                      icon = ravedash::shiny_icons$refresh,
                      style = "margin-top: 2rem;",
                      width = "100%"
                    ),
                    inputId = "param_acpc_refresh",
                    update = "shiny::updateActionButton",
                    description = paste(
                      "[Button] Refreshes the MRI file lists (`param_acpc_infile`,",
                      "`param_fs_infile`), e.g. after an import. Click it with",
                      "`shiny_input_update` (the value is ignored)."
                    ),
                    tooltip = "Refresh the MRI file list."
                  )
                ),
                shiny::column(
                  width = 4L,
                  dipsaus::actionButtonStyled(
                    inputId = ns("param_acpc_start"),
                    label = "Start re-alignment",
                    style = "margin-top: 2rem;",
                    width = "100%"
                  )
                ),
                shiny::column(
                  width = 12L,
                  shiny::htmlOutput(
                    outputId = ns("acpc_results"),
                    inline = TRUE
                  )
                )
              )
            ),

            ravedash::output_card(
              title = "MRI Preprocessing",
              start_collapsed = TRUE,
              append_tools = FALSE,
              # class_foot = "no-padding",
              footer = shiny::tags$details(
                shiny::tags$summary("Terminal script - Imaging segmentation"),
                shiny::uiOutput(
                  ns("panel_image_segmentation"),
                  container = shiny::p
                )
              ),
              shiny::div(
                "* If you choose to use the FreeSurfer, then the script requires Unix ",
                shiny::pre(
                  class = "pre-compact no-padding display-inline",
                  "bash"
                ),
                " terminals. If you are using Windows, ",
                "please use Window sub-system for Linux [WSL2].",

                shiny::hr(),

                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::plotOutput(
                      outputId = ns("mri_preview"),
                      height = "300px"
                    )
                  ),

                  shiny::column(
                    width = 7L,
                    shiny::div(
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("param_fs_infile"),
                          label = "MRI file (select one with the best quality)",
                          choices = character(0L)
                        ),
                        inputId = "param_fs_infile",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select] Input MRI for MRI preprocessing (a file in the subject's imaging",
                          "`inputs/MRI` folder); read by script `save_mri_preprocessing_script` (and by",
                          "AFNI co-registration). Agents must use the same image as co-registration",
                          "(`param_coreg_mri`): the imported MRI (`MRI_RAW.nii[.gz]` after a NIfTI",
                          "import) when ACPC re-alignment is not part of the pipeline, or",
                          "`MRI_acpc.nii.gz` when it is. Never mix an ACPC image with a non-ACPC one",
                          "(their orientation matrices differ). Refresh with `param_fs_refresh`."
                        ),
                        tooltip = "The MRI to preprocess; choose the one with the best quality."
                      ),
                      shidashi::register_input(
                        shiny::actionLink(
                          inputId = ns("param_fs_refresh"),
                          label = "Refresh"
                        ),
                        inputId = "param_fs_refresh",
                        update = "shiny::updateActionLink",
                        description = paste(
                          "[Link] Refreshes the MRI file lists (`param_fs_infile`,",
                          "`param_acpc_infile`), e.g. after an import. Click it with",
                          "`shiny_input_update` (the value is ignored)."
                        ),
                        tooltip = "Refresh the MRI file list."
                      )
                    )
                  ),
                  shiny::column(
                    width = 5L,
                    shiny::fluidRow(
                      shiny::column(
                        width = 12L,
                        shidashi::register_input(
                          shiny::selectInput(
                            inputId = ns("param_fs_prog"),
                            label = "Command",
                            choices = c(
                              "recon-all -all",
                              "recon-all -autorecon1",
                              "recon-all-clinical.sh",
                              # "ants-preprocessing",
                              "YAEL+recon-all",
                              "YAEL-only",
                              "simple-import"
                            ),
                            selected = "recon-all"
                          ),
                          inputId = "param_fs_prog",
                          update = "shiny::updateSelectInput(value=selected)",
                          description = paste(
                            "[Select] MRI preprocessing command: `recon-all -all`, `recon-all -autorecon1`,",
                            "`recon-all-clinical.sh`, `YAEL+recon-all`, `YAEL-only`, `simple-import`.",
                            "Recommend `YAEL+recon-all`, or `YAEL-only` when FreeSurfer is not needed or",
                            "the computer's memory is short (FreeSurfer 7.x needs 16 GB of RAM, 8.x needs",
                            "32 GB; script `imaging_status` shows both). Agents never run these from RAVE:",
                            "script `save_mri_preprocessing_script` saves the script and returns the",
                            "terminal command."
                          ),
                          tooltip = "Command used to preprocess the MRI."
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(
                        width = 12L,
                        shidashi::register_input(
                          shiny::checkboxInput(
                            inputId = ns("param_fs_fresh_start"),
                            label = "Remove existing work before running the command (if applicable)",
                            value = FALSE
                          ),
                          inputId = "param_fs_fresh_start",
                          update = "shiny::updateCheckboxInput",
                          description = paste(
                            "[Checkbox, read-only for agents] When ticked, recon-all first deletes the",
                            "subject's FreeSurfer folder. Agents must never tick it, and never run a",
                            "saved script made while it was ticked; ask the user instead."
                          ),
                          tooltip = "Delete the subject's previous FreeSurfer work before running the command.",
                          writable = FALSE
                        )
                      )
                    )
                  )
                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    local({
                      if (dry_run) {
                        NULL
                      } else {
                        shiny::conditionalPanel(
                          "!['recon-all-clinical.sh', 'recon-all -all', 'YAEL+recon-all', 'YAEL-only'].includes( input.param_fs_prog )",
                          shiny::actionButton(
                            ns("btn_recon_run"),
                            "Run from RAVE"
                          ),
                          ns = ns,
                          style = "display: inline"
                        )
                      }
                    }),
                    dipsaus::actionButtonStyled(
                      ns("btn_recon_copy"),
                      "Save & run by yourself"
                    )
                  )
                )
              )
            ),

            ravedash::output_card(
              title = "Co-registration CT and T1",
              start_collapsed = TRUE,
              append_tools = FALSE,
              # class_foot = "no-padding",
              footer = shiny::tags$details(
                shiny::tags$summary("Terminal script - CT MRI co-registration"),
                shiny::uiOutput(ns("panel_coreg"), container = shiny::p)
              ),
              shiny::p(
                "This step aligns the CT to MR image. ",
                "For MRI, ",
                shiny::pre(
                  class = "pre-compact no-padding display-inline",
                  "MRI_RAW.nii"
                ),
                " is the original image file, and ",
                shiny::pre(
                  class = "pre-compact no-padding display-inline",
                  "T1.nii"
                ),
                " is the FreeSurfer/ANTs-normalized image.",
              ),
              shiny::div(
                shiny::tags$ul(
                  shiny::tags$li(
                    shiny::pre(
                      class = "pre-compact no-padding display-inline",
                      "NiftyReg"
                    ),
                    " is always available. "
                  ),
                  shiny::tags$li(
                    shiny::pre(
                      class = "pre-compact no-padding display-inline",
                      "ANTs"
                    ),
                    " and ",
                    shiny::pre(
                      class = "pre-compact no-padding display-inline",
                      "img_pipe"
                    ),
                    " require enabling RAVE-Python support"
                  ),
                  shiny::tags$li(
                    shiny::pre(
                      class = "pre-compact no-padding display-inline",
                      "FSL-FLIRT"
                    ),
                    " and ",
                    shiny::pre(
                      class = "pre-compact no-padding display-inline",
                      "AFNI-ALICE"
                    ),
                    " call external scripts that requires Unix ",
                    shiny::pre(
                      class = "pre-compact no-padding display-inline",
                      "bash"
                    ),
                    " terminals. If you are using these two on Windows, ",
                    "please use the Windows sub-system for Linux (WSL2)."
                  )
                ),
                shiny::hr(),

                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::plotOutput(
                      outputId = ns("ct_preview"),
                      height = "300px"
                    )
                  )
                ),

                shiny::fluidRow(
                  shiny::column(
                    width = 5L,
                    shiny::div(
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("param_coreg_ct"),
                          label = "CT file",
                          choices = character(0L)
                        ),
                        inputId = "param_coreg_ct",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select] The imported CT (a file in the subject's imaging `inputs/CT`",
                          "folder) to align to the MRI; read by scripts `run_coregistration` and",
                          "`save_coregistration_script`. Empty until a CT is imported; refresh with",
                          "`param_coreg_refresh`."
                        ),
                        tooltip = "The imported CT to align to the MRI."
                      ),
                      shidashi::register_input(
                        shiny::actionLink(
                          inputId = ns("param_coreg_refresh"),
                          label = "Refresh"
                        ),
                        inputId = "param_coreg_refresh",
                        update = "shiny::updateActionLink",
                        description = paste(
                          "[Link] Refreshes the CT and MR image lists (`param_coreg_ct`,",
                          "`param_coreg_mri`), e.g. after an import or an ACPC save. Click it with",
                          "`shiny_input_update` (the value is ignored)."
                        ),
                        tooltip = "Refresh the CT and MR image lists."
                      )
                    )
                  ),

                  shiny::column(
                    width = 4L,
                    shiny::div(
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("param_coreg_mri"),
                          label = "MR image",
                          choices = character(0L)
                        ),
                        inputId = "param_coreg_mri",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select] The MR image the CT is aligned to (a file in the subject's imaging",
                          "`derivative` folder). Agents must use the same image as MRI preprocessing",
                          "(`param_fs_infile`): `MRI_RAW` when ACPC re-alignment is not part of the",
                          "pipeline, or `MRI_acpc.nii.gz` when it is; never `T1.nii`, never the CT copy",
                          "`CT_RAW`, and never an ACPC image with a non-ACPC one. When the list is empty",
                          "(e.g. after a DICOM import), the module uses `param_fs_infile`. Not used by AFNI."
                        ),
                        tooltip = "The MR image the CT is aligned to."
                      )
                    )
                  ),

                  shiny::column(
                    width = 3L,
                    shiny::div(
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_ct_program"),
                          label = "Program",
                          choices = c(
                            "NiftyReg",
                            "ANTs",
                            "img_pipe",
                            "AFNI",
                            "FSL"
                          ),
                          selected = "NiftyReg"
                        ),
                        inputId = "coreg_ct_program",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select] Co-registration program: `NiftyReg` (default), `ANTs`, `img_pipe`,",
                          "`AFNI`, `FSL`. Recommend `ANTs` (by far the best algorithm). Agents may run",
                          "only `ANTs` or `NiftyReg` from RAVE (script `run_coregistration`, after the",
                          "user confirms); for the others use script `save_coregistration_script`.",
                          "Each program shows its own parameter inputs."
                        ),
                        tooltip = "Program used to align the CT to the MRI."
                      )
                    )
                  )
                ),

                # native: ANTs
                shiny::conditionalPanel(
                  condition = sprintf(
                    "input['%s']==='ANTs'",
                    ns("coreg_ct_program")
                  ),
                  shiny::fluidRow(
                    shiny::column(
                      width = 12L,
                      "Coregistration parameters"
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_ants_type"),
                          label = "Registration type",
                          choices = c("Rigid", "DenseRigid", "Affine", "SyN"),
                          selected = "Rigid"
                        ),
                        inputId = "coreg_ants_type",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `Rigid` (default), `DenseRigid`, `Affine`, `SyN`] ANTs registration",
                          "type; shown when `coreg_ct_program` is `ANTs`. Read by scripts",
                          "`run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "ANTs registration type."
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_ants_aff_metric"),
                          label = "Affine metric",
                          choices = c("mattes", "meansquares", "GC"),
                          selected = "mattes"
                        ),
                        inputId = "coreg_ants_aff_metric",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `mattes` (default), `meansquares`, `GC`] ANTs affine metric; shown when",
                          "`coreg_ct_program` is `ANTs`. Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "ANTs affine metric."
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_ants_syn_metric"),
                          label = "SyN metric (non-linear)",
                          choices = c("mattes", "meansquares", "demons", "CC"),
                          selected = "mattes"
                        ),
                        inputId = "coreg_ants_syn_metric",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `mattes` (default), `meansquares`, `demons`, `CC`] ANTs SyN",
                          "(non-linear) metric, used with registration type `SyN`; shown when",
                          "`coreg_ct_program` is `ANTs`. Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "ANTs SyN (non-linear) metric."
                      )
                    )
                  )
                ),

                # native: NiftyReg
                shiny::conditionalPanel(
                  condition = sprintf(
                    "input['%s']==='NiftyReg'",
                    ns("coreg_ct_program")
                  ),
                  shiny::fluidRow(
                    shiny::column(
                      width = 12L,
                      "Coregistration parameters"
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_niftyreg_type"),
                          label = "Registration type",
                          choices = c("rigid", "affine", "nonlinear"),
                          selected = "rigid"
                        ),
                        inputId = "coreg_niftyreg_type",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `rigid` (default), `affine`, `nonlinear`] NiftyReg registration type;",
                          "shown when `coreg_ct_program` is `NiftyReg`. Read by scripts",
                          "`run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "NiftyReg registration type."
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_niftyreg_interp"),
                          label = "Interpolation",
                          choices = c("trilinear", "cubic", "nearest"),
                          selected = "trilinear"
                        ),
                        inputId = "coreg_niftyreg_interp",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `trilinear` (default), `cubic`, `nearest`] NiftyReg interpolation;",
                          "shown when `coreg_ct_program` is `NiftyReg`. Read by scripts",
                          "`run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "NiftyReg interpolation."
                      )
                    )
                  )
                ),
                # img_pipe
                shiny::conditionalPanel(
                  condition = sprintf(
                    "input['%s']==='img_pipe'",
                    ns("coreg_ct_program")
                  ),
                  shiny::fluidRow(
                    shiny::column(
                      width = 12L,
                      "Coregistration parameters"
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_nipy_reg_type"),
                          label = "Registration type",
                          choices = c("rigid", "affine"),
                          selected = "rigid"
                        ),
                        inputId = "coreg_nipy_reg_type",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `rigid` (default), `affine`] img_pipe registration type; shown when",
                          "`coreg_ct_program` is `img_pipe` (agents only save its script). Read by scripts",
                          "`run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "img_pipe registration type."
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_nipy_interp"),
                          label = "Interpolation",
                          choices = c("pv", "tri"),
                          selected = "pv"
                        ),
                        inputId = "coreg_nipy_interp",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `pv` (default), `tri`] img_pipe interpolation; shown when",
                          "`coreg_ct_program` is `img_pipe`. Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "img_pipe interpolation."
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_nipy_cost"),
                          label = "Cost function",
                          choices = c("crl1", "cc", "cr", "mi", "nmi", "slr"),
                          selected = "crl1"
                        ),
                        inputId = "coreg_nipy_cost",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `crl1` (default), `cc`, `cr`, `mi`, `nmi`, `slr`] img_pipe cost",
                          "function; shown when `coreg_ct_program` is `img_pipe`. Read by scripts",
                          "`run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "img_pipe cost function."
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_nipy_optimizer"),
                          label = "Optimizer",
                          choices = c(
                            "powell",
                            "steepest",
                            "cg",
                            "bfgs",
                            "simplex"
                          ),
                          selected = "powell"
                        ),
                        inputId = "coreg_nipy_optimizer",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `powell` (default), `steepest`, `cg`, `bfgs`, `simplex`] img_pipe",
                          "optimizer; shown when `coreg_ct_program` is `img_pipe`. Read by scripts",
                          "`run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "img_pipe optimizer."
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::checkboxInput(
                          inputId = ns("coreg_nipy_clean_source"),
                          label = "Remove negative CT values",
                          value = TRUE
                        ),
                        inputId = "coreg_nipy_clean_source",
                        update = "shiny::updateCheckboxInput",
                        description = paste(
                          "[Checkbox, default true] img_pipe: remove negative CT values; shown when",
                          "`coreg_ct_program` is `img_pipe`. Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "img_pipe: remove negative CT values."
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::checkboxInput(
                          inputId = ns("coreg_nipy_inverse_target"),
                          label = "Invert MR image color",
                          value = TRUE
                        ),
                        inputId = "coreg_nipy_inverse_target",
                        update = "shiny::updateCheckboxInput",
                        description = paste(
                          "[Checkbox, default true] img_pipe: invert the MR image color; shown when",
                          "`coreg_ct_program` is `img_pipe`. Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "img_pipe: invert the MR image color."
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shidashi::register_input(
                        shiny::checkboxInput(
                          inputId = ns("coreg_nipy_precenter_source"),
                          label = "Pre-center CT (giant move)",
                          value = TRUE
                        ),
                        inputId = "coreg_nipy_precenter_source",
                        update = "shiny::updateCheckboxInput",
                        description = paste(
                          "[Checkbox, default true] img_pipe: pre-center the CT (giant move); shown when",
                          "`coreg_ct_program` is `img_pipe`. Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "img_pipe: pre-center the CT (giant move)."
                      )
                    )
                  )
                ),
                # FSL params
                shiny::conditionalPanel(
                  condition = sprintf(
                    "input['%s']==='FSL'",
                    ns("coreg_ct_program")
                  ),
                  shiny::fluidRow(
                    shiny::column(
                      width = 12L,
                      "FSL parameters"
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_fsl_dof"),
                          label = "DOF",
                          choices = c(
                            "6 (rigid body)",
                            "7 (global rescale)",
                            "9 (traditional)",
                            "12 (affine)"
                          ),
                          selected = "6 (rigid body)"
                        ),
                        inputId = "coreg_fsl_dof",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `6 (rigid body)` (default), `7 (global rescale)`, `9 (traditional)`,",
                          "`12 (affine)`] FSL FLIRT degrees of freedom; shown when `coreg_ct_program` is",
                          "`FSL` (agents only save its script). Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "FSL FLIRT degrees of freedom."
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_fsl_cost"),
                          label = "Cost function",
                          choices = FSL_COST_FUNCTIONS,
                          selected = "mutualinfo"
                        ),
                        inputId = "coreg_fsl_cost",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `mutualinfo` (default), `leastsq`, `normcorr`, `corratio`, `normmi`,",
                          "`labeldiff`, `bbr`] FSL FLIRT cost function; shown when `coreg_ct_program` is",
                          "`FSL`. Read by scripts `run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "FSL FLIRT cost function."
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_fsl_search"),
                          label = "Search range",
                          choices = c(
                            "90",
                            "180"
                          ),
                          selected = "90"
                        ),
                        inputId = "coreg_fsl_search",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select: `90` (default), `180`] FSL FLIRT search range (degrees); shown when",
                          "`coreg_ct_program` is `FSL`. Read by scripts `run_coregistration` and",
                          "`save_coregistration_script`."
                        ),
                        tooltip = "FSL FLIRT search range."
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shidashi::register_input(
                        shiny::selectInput(
                          inputId = ns("coreg_fsl_searchcost"),
                          label = "Search Cost",
                          choices = FSL_COST_FUNCTIONS,
                          selected = "mutualinfo"
                        ),
                        inputId = "coreg_fsl_searchcost",
                        update = "shiny::updateSelectInput(value=selected)",
                        description = paste(
                          "[Select, same choices as `coreg_fsl_cost`, default `mutualinfo`] FSL FLIRT",
                          "search cost; shown when `coreg_ct_program` is `FSL`. Read by scripts",
                          "`run_coregistration` and `save_coregistration_script`."
                        ),
                        tooltip = "FSL FLIRT search cost."
                      )
                    )
                  )
                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    local({
                      if (dry_run) {
                        NULL
                      } else {
                        shiny::actionButton(
                          ns("btn_coreg_run"),
                          "Run from RAVE"
                        )
                      }
                    }),
                    dipsaus::actionButtonStyled(
                      ns("btn_coreg_copy"),
                      "Save & run by yourself"
                    )
                  )
                )
              )
            )

            # ravedash::output_card(
            #   title = "Align MRI to Template",
            #   start_collapsed = TRUE,
            #   tools = list(
            #     shidashi::as_badge("requires `Python`|bg-yellow")
            #   ),
            #   append_tools = FALSE,
            #   shiny::div(
            #     "This *optional* step non-linearly aligns native ",
            #     shiny::pre(class="pre-compact no-padding display-inline", "aparc+aseg"),
            #     "to template brain in MNI space. Please make sure to finish FreeSurfer ",
            #     "has finished for this subject. You can skip this step and proceed to ",
            #     "electrode localization.",
            #     shiny::hr(),
            #
            #     shiny::fluidRow(
            #
            #       shiny::column(
            #         width = 4L,
            #         shiny::selectInput(
            #           inputId = ns("mri_morph_template_subject"),
            #           label = "Template to morph into",
            #           choices = c("fsaverage", "N27", "bert", "cvs_avg35", "cvs_avg35_inMNI152"),
            #           selected = getOption("threeBrain.template_subject", "fsaverage")
            #         )
            #       )
            #     ),
            #
            #     shiny::div(
            #       class = "float-right",
            #       shiny::div(
            #         dipsaus::actionButtonStyled(ns("btn_mri_morph_run"), "Run from RAVE")
            #       )
            #     )
            #   )
            # )
          )
        )
      )
    )
  )


}
