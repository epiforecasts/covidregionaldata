test_that("get_regional_covid_data returns error if country argument is not a string", {
  expect_error(get_regional_covid_data(4))
  expect_error(get_regional_covid_data(TRUE))
})

test_that("get_regional_covid_data returns error if there is no data for the country entered", {
  expect_error(get_regional_covid_data("Narnia"))
  expect_error(get_regional_covid_data("Atlantis"))
})

## Happy case - set up
source("tests/testthat/custom_tests/mock_data_for_get_regional_covid_data.R")

test_that("get_covid_regional_data returns correct data not using totals", {
  returned_data <- with_mock(get_canada_regional_cases = function() return(input_data), get_regional_covid_data("canada"))
  expect_equal(expected_data, returned_data)
})

test_that("get_covid_regional_data returns correct data when using totals", {
  returned_data <- with_mock(get_canada_regional_cases = function() return(input_data), get_regional_covid_data("canada", totals = TRUE))
  expect_equal(totals_data, returned_data)
})
