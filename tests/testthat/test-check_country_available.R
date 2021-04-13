test_that("Test error is thrown for numeric country", {
  expect_error(
    check_country_available(
      country = 1, level = "1",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    ),
    "is.character(country) is not TRUE",
    fixed = TRUE
  )
})
test_that("Test error is thrown for unknown country/source", {
  expect_error(
    check_country_available(
      country = "amadeupcountry", level = "1",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    )
  )
})
test_that("Test error is thrown when level not available for data", {
  expect_error(
    check_country_available(
      country = "Italy", level = "2",
      totals = FALSE, localise = TRUE,
      verbose = FALSE, steps = FALSE
    )
  )
})
test_that("Test returns an R6 object", {
  region_class <- check_country_available(
    country = "WHO", level = "1",
    totals = FALSE, localise = TRUE,
    verbose = FALSE, steps = FALSE
  )
  to_check <- class(region_class)
  expect_equal(to_check[length(to_check)], "R6")
})
test_that("Test that datasets with spaces can be found", {
  uk <- check_country_available("United Kingdom")
  expect_equal(class(uk)[1], "UK")
})
test_that("Test that datasets with lower case", {
  ecdc <- check_country_available("ecdc")
  expect_equal(class(ecdc)[1], "ECDC")
})
test_that("Test that datasets with partial name matches", {
  uk <- check_country_available("united kingd")
  expect_equal(class(uk)[1], "UK")
})
