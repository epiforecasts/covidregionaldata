test_github_workflow_found <- function(source) {
  source_workflow_path <- paste0(".github/workflows/", source, ".yaml")
  if (!file.exists(source_workflow_path)) {
    stop(
      paste0(
        source, " does not have a github workflow yaml file. ",
        "You can create this by calling `make_github_workflow('", source, "')`"
      )
    )
  }
  print(paste(source_workflow_path, " exists"))
}

purrr::walk(
  get_available_datasets()$class,
  test_github_workflow_found
)
