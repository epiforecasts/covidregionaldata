test_that("Test error is thrown for numeric class", {
  expect_error(
    initialise_dataclass(
      class = 1, level = "1",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    ),
    "is.character(class) is not TRUE",
    fixed = TRUE
  )
})
test_that("Test error is thrown for unknown class/source", {
  expect_error(
    initialise_dataclass(
      class = "amadeupclass", level = "1",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    )
  )
})
test_that("Test error is thrown when level not available for data", {
  expect_error(
    initialise_dataclass(
      class = "Italy", level = "2",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    )
  )
})
test_that("Test returns an R6 object", {
  region_class <- initialise_dataclass(
    class = "WHO", level = "1",
    totals = FALSE, localise = TRUE,
    verbose = FALSE, steps = FALSE
  )
  to_check <- class(region_class)
  expect_equal(to_check[length(to_check)], "R6")
})
test_that("Test that datasets with spaces can be found", {
  uk <- initialise_dataclass("United Kingdom")
  expect_equal(class(uk)[1], "UK")
})
test_that("Test that datasets with lower case", {
  ecdc <- initialise_dataclass("ecdc")
  expect_equal(class(ecdc)[1], "ECDC")
})
test_that("Test that datasets with partial name matches", {
  uk <- initialise_dataclass("united kingd")
  expect_equal(class(uk)[1], "UK")
})
