# Set pipeline options
targets::tar_option_set(
  packages = c("ravepipeline")
)


ensure_brain_template <- function(template_subject = raveio::raveio_getopt("threeBrain_template_subject")) {
  template_subject <- template_subject[!is.na(template_subject)]
  if(!length(template_subject)) {
    template_subject <- raveio::raveio_getopt("threeBrain_template_subject")
  } else {
    template_subject <- template_subject[[1]]
  }
  template_root <- threeBrain::default_template_directory()
  template_path <- file.path(template_root, template_subject)
  if (!dir.exists(template_path)) {
    # need to download the template brain files
    oldopt <- options(timeout = 3600)
    on.exit({
      options(oldopt)
    })
    threeBrain::download_template_subject(subject_code = template_subject)
  }
  return(invisible(template_path))
}

generate_website <- function(projects, build_target = "build") {

  if (length(projects) == 0) {
    stop("No projects found. Please create a project first.")
  }
  raveio::dir_create2(build_target)

  # remove `build_target`/projects folder
  project_target <- file.path(build_target, "projects")
  if(file.exists(project_target)) {
    unlink(project_target, recursive = TRUE, force = TRUE)
  }
  dir.create(project_target, recursive = TRUE)

  file.copy("R/shared-column-builders.R", file.path(build_target, "columns.R"))

  for (project_name in projects) {
    template <- readLines("project_page_template.qmd")
    template <- gsub("\\{\\{PROJECT_NAME\\}\\}", project_name, template)
    writeLines(
      template,
      file.path(project_target, sprintf("%s.qmd", project_name))
    )
  }

  # create _quarto.yml with projects as sidebar contents
  sidebar_contents <- lapply(projects, function(project_name) {
    list(
      text = project_name,
      href = sprintf("projects/%s.qmd", project_name)
    )
  })

  quarto_yml <- list(
    project = list(
      type = "website"
    ),
    website = list(
      sidebar = list(
        style = "docked",
        contents = sidebar_contents
      )
    )
  )

  yaml_text <- yaml::as.yaml(quarto_yml)
  writeLines(yaml_text, file.path(build_target, "_quarto.yml"))
  quarto::quarto_render(build_target, as_job = FALSE, execute_dir = build_target)

  cache_src_dir <- file.path(build_target, "cache", fsep = "/")
  cache_tgt_dir <- file.path(build_target, "_site", "cache", fsep = "/")
  raveio::dir_create2(cache_tgt_dir)
  for (project_name in projects) {
    file.copy(
      file.path(cache_src_dir, project_name),
      cache_tgt_dir,
      recursive = TRUE,
      overwrite = TRUE
    )
  }

  return(file.path(build_target, "_site"))
}
