test_github_workflow_found <- function(source) {
  source_workflow_path <- paste0("../../.github/workflows/", source, ".yaml")
  test_that(
    paste0(
      source, " has github workflow yaml file. ",
      "You can create this by calling `make_github_workflow(`'", source, "')`.",
      " cwd = ", getwd()
    ),
    {
      expect_true(
        file.exists(paste0("../../.github/workflows/", source, ".yaml"))
      )
    }
  )
}

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  purrr::walk(
    get_available_datasets()$class,
    test_github_workflow_found
  )
}
