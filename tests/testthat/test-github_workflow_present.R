make_github_workflow <- function(source, source_workflow_path) {
  template_path <- "../../inst/github_workflow_template.yaml"
  template <- readLines(template_path)
  newfile <- gsub("_SOURCE_", source, template)
  writeLines(newfile, source_workflow_path)
  message(
    paste("workflow auto created for", source, "at", source_workflow_path)
  )
}

test_github_workflow_found <- function(source) {
  source_workflow_path <- paste0("../../.github/workflows/", source, ".yaml")
  if (!file.exists(source_workflow_path)) {
    make_github_workflow(source, source_workflow_path)
  }
  test_that(
    paste0(source, " has github workflow yaml file"),
    {
      expect_true(
        file.exists(paste0("../../.github/workflows/", source, ".yaml"))
      )
    }
  )
}

skip_on_cran()
purrr::walk(
  get_available_datasets()$class,
  test_github_workflow_found
)
