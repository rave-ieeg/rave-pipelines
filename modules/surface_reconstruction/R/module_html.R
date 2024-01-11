

# 1. Information
# 2. Import (convert to nii)
# 3. Surface recon
# 4. CT co-registration
dry_run <- raveio::is_dry_run()
module_html <- function(){

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
                "If your original image files have DICOM format. The following script uses ", shiny::pre(class="pre-compact no-padding display-inline", "dcm2niix"), " external library to convert DICOM images to Nifti format for later purposes. ",
                shiny::br(),
                "* The script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "bash"),
                " terminal. For Windows users, please use Window sub-Linux system (WSL2). If your data is in Nifti format, the file will be copied directly. No external scripts will be used.",

                shiny::hr(),

                shiny::fluidRow(

                  shiny::column(
                    width = 5L,
                    shiny::div(
                      class = "display-inline",
                      shiny::selectInput(
                        inputId = ns("param_dcm2niix_merge"),
                        label = "Merge 2D slices from same series regardless of echo, exposure, etc.",
                        choices = c("Auto", "Yes", "No"),
                        selected = "Auto"
                      )
                    )
                  ),
                  shiny::column(
                    width = 5L,
                    shiny::selectInput(
                      inputId = ns("param_dcm2niix_float"),
                      label = "Merge 2D slices from same series regardless of echo, exposure, etc.",
                      choices = c("Yes", "No"),
                      selected = "Yes"
                    )
                  ),
                  shiny::column(
                    width = 2L,
                    shiny::selectInput(
                      inputId = ns("param_dcm2niix_crop"),
                      label = "Crop 3D acquisitions",
                      choices = c("Yes", "No", "Ignore"),
                      selected = "No"
                    )
                  )

                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    shiny::actionButton(ns("btn_dcm2niix_run_t1"), "Run from RAVE (T1 MRI)"),
                    shiny::actionButton(ns("btn_dcm2niix_run_ct"), "Run from RAVE (CT)")
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
                  shiny::selectInput(
                    inputId = ns("param_acpc_infile"),
                    label = "MRI file (select one to be ACPC re-aligned)",
                    choices = character(0L)
                  )
                ),
                shiny::column(
                  width = 1L,
                  shiny::actionButton(
                    inputId = ns("param_acpc_refresh"),
                    label = "",
                    icon = ravedash::shiny_icons$refresh,
                    style = "margin-top: 2rem;",
                    width = "100%"
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
                shiny::uiOutput(ns("panel_image_segmentation"), container = shiny::p)
              ),
              shiny::div(
                "* If you choose to use the FreeSurfer, then the script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "bash"),
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
                      shiny::selectInput(
                        inputId = ns("param_fs_infile"),
                        label = "MRI file (select one with the best quality)",
                        choices = character(0L)
                      ),
                      shiny::actionLink(
                        inputId = ns("param_fs_refresh"),
                        label = "Refresh"
                      )
                    )
                  ),
                  shiny::column(
                    width = 5L,
                    shiny::fluidRow(
                      shiny::column(
                        width = 12L,
                        shiny::selectInput(
                          inputId = ns("param_fs_prog"),
                          label = "Command",
                          choices = c(
                            "recon-all -all", "recon-all -autorecon1",
                            "recon-all-clinical.sh", "ants-preprocessing",
                            "ants+recon-all",
                            "simple-import"
                          ),
                          selected = "recon-all"
                        )
                      )
                    ),
                    shiny::fluidRow(
                      shiny::column(
                        width = 12L,
                        shiny::checkboxInput(
                          inputId = ns("param_fs_fresh_start"),
                          label = "Remove existing work before running the command (if applicable)",
                          value = FALSE
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
                      if(dry_run) {
                        NULL
                      } else {
                        shiny::conditionalPanel(
                          "!['recon-all-clinical.sh', 'recon-all -all'].includes( input.param_fs_prog )",
                          shiny::actionButton(ns("btn_recon_run"), "Run from RAVE"),
                          ns = ns,
                          style = "display: inline"
                        )
                      }
                    }),
                    dipsaus::actionButtonStyled(ns("btn_recon_copy"), "Save & run by yourself")
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
                shiny::pre(class="pre-compact no-padding display-inline", "MRI_RAW.nii"),
                " is the original image file, and ",
                shiny::pre(class="pre-compact no-padding display-inline", "T1.nii"),
                " is the FreeSurfer/ANTs-normalized image.",
              ),
              shiny::div(
                shiny::tags$ul(
                  shiny::tags$li(
                    shiny::pre(class="pre-compact no-padding display-inline", "NiftyReg"),
                    " is always available. "
                  ),
                  shiny::tags$li(
                    shiny::pre(class="pre-compact no-padding display-inline", "ANTs"),
                    " and ",
                    shiny::pre(class="pre-compact no-padding display-inline", "img_pipe"),
                    " require enabling RAVE-Python support"
                  ),
                  shiny::tags$li(
                    shiny::pre(class="pre-compact no-padding display-inline", "FSL-FLIRT"),
                    " and ",
                    shiny::pre(class="pre-compact no-padding display-inline", "AFNI-ALICE"),
                    " call external scripts that requires Unix ",
                    shiny::pre(class="pre-compact no-padding display-inline", "bash"),
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
                      shiny::selectInput(
                        inputId = ns("param_coreg_ct"),
                        label = "CT file",
                        choices = character(0L)
                      ),
                      shiny::actionLink(
                        inputId = ns("param_coreg_refresh"),
                        label = "Refresh"
                      )
                    )
                  ),

                  shiny::column(
                    width = 4L,
                    shiny::div(
                      shiny::selectInput(
                        inputId = ns("param_coreg_mri"),
                        label = "MR image",
                        choices = character(0L)
                      )
                    )
                  ),

                  shiny::column(
                    width = 3L,
                    shiny::div(
                      shiny::selectInput(
                        inputId = ns("coreg_ct_program"),
                        label = "Program",
                        choices = c("NiftyReg", "ANTs", "img_pipe", "AFNI", "FSL"),
                        selected = "NiftyReg"
                      )
                    )
                  )
                ),

                # native: ANTs
                shiny::conditionalPanel(
                  condition = sprintf("input['%s']==='ANTs'", ns("coreg_ct_program")),
                  shiny::fluidRow(

                    shiny::column(
                      width = 12L,
                      "Coregistration parameters"
                    ),

                    shiny::column(
                      width = 4L,
                      shiny::selectInput(
                        inputId = ns("coreg_ants_type"),
                        label = "Registration type",
                        choices = c("Rigid", "DenseRigid", "Affine", "SyN"),
                        selected = "Rigid"
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shiny::selectInput(
                        inputId = ns("coreg_ants_aff_metric"),
                        label = "Affine metric",
                        choices = c("mattes", "meansquares", "GC"),
                        selected = "mattes"
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shiny::selectInput(
                        inputId = ns("coreg_ants_syn_metric"),
                        label = "SyN metric (non-linear)",
                        choices = c("mattes", "meansquares", "demons", "CC"),
                        selected = "mattes"
                      )
                    )

                  )
                ),

                # native: NiftyReg
                shiny::conditionalPanel(
                  condition = sprintf("input['%s']==='NiftyReg'", ns("coreg_ct_program")),
                  shiny::fluidRow(

                    shiny::column(
                      width = 12L,
                      "Coregistration parameters"
                    ),

                    shiny::column(
                      width = 4L,
                      shiny::selectInput(
                        inputId = ns("coreg_niftyreg_type"),
                        label = "Registration type",
                        choices = c("rigid", "affine", "nonlinear"),
                        selected = "rigid"
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shiny::selectInput(
                        inputId = ns("coreg_niftyreg_interp"),
                        label = "Interpolation",
                        choices = c("trilinear", "cubic", "nearest"),
                        selected = "trilinear"
                      )
                    )

                  )
                ),
                # img_pipe
                shiny::conditionalPanel(
                  condition = sprintf("input['%s']==='img_pipe'", ns("coreg_ct_program")),
                  shiny::fluidRow(

                    shiny::column(
                      width = 12L,
                      "Coregistration parameters"
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_nipy_reg_type"),
                        label = "Registration type",
                        choices = c("rigid", "affine"),
                        selected = "rigid"
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_nipy_interp"),
                        label = "Interpolation",
                        choices = c("pv", "tri"),
                        selected = "pv"
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_nipy_cost"),
                        label = "Cost function",
                        choices = c("crl1", "cc", "cr", "mi", "nmi", "slr"),
                        selected = "crl1"
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_nipy_optimizer"),
                        label = "Optimizer",
                        choices = c("powell", "steepest", "cg", "bfgs", "simplex"),
                        selected = "powell"
                      )
                    ),


                    shiny::column(
                      width = 4L,
                      shiny::checkboxInput(
                        inputId = ns("coreg_nipy_clean_source"),
                        label = "Remove negative CT values",
                        value = TRUE
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shiny::checkboxInput(
                        inputId = ns("coreg_nipy_inverse_target"),
                        label = "Invert MR image color",
                        value = TRUE
                      )
                    ),

                    shiny::column(
                      width = 4L,
                      shiny::checkboxInput(
                        inputId = ns("coreg_nipy_precenter_source"),
                        label = "Pre-center CT (giant move)",
                        value = TRUE
                      )
                    )

                  )
                ),
                # FSL params
                shiny::conditionalPanel(
                  condition = sprintf("input['%s']==='FSL'", ns("coreg_ct_program")),
                  shiny::fluidRow(

                    shiny::column(
                      width = 12L,
                      "FSL parameters"
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_fsl_dof"),
                        label = "DOF",
                        choices = c(
                          "6 (rigid body)",
                          "7 (gloabl rescale)",
                          "9 (traditional)",
                          "12 (affine)"
                        ),
                        selected = "6 (rigid body)"
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_fsl_cost"),
                        label = "Cost function",
                        choices = FSL_COST_FUNCTIONS,
                        selected = "mutualinfo"
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_fsl_search"),
                        label = "Search range",
                        choices = c(
                          "90", "180"
                        ),
                        selected = "90"
                      )
                    ),

                    shiny::column(
                      width = 3L,
                      shiny::selectInput(
                        inputId = ns("coreg_fsl_searchcost"),
                        label = "Search Cost",
                        choices = FSL_COST_FUNCTIONS,
                        selected = "mutualinfo"
                      )
                    )

                  )
                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    local({
                      if(dry_run) {
                        NULL
                      } else {
                        shiny::actionButton(ns("btn_coreg_run"), "Run from RAVE")
                      }
                    }),
                    dipsaus::actionButtonStyled(ns("btn_coreg_copy"), "Save & run by yourself")
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
