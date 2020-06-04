test_that("get_regional_covid_data returns error if country argument is not a string", {
  expect_error(get_regional_covid_data(4))
  expect_error(get_regional_covid_data(TRUE))
})

test_that("get_regional_covid_data returns error if there is no data for the country entered", {
  expect_error(get_regional_covid_data("Narnia"))
  expect_error(get_regional_covid_data("Atlantis"))
})

## Happy case - set up
source("custom_tests/mock_data.R")

test_that("get_wide_format_regional_covid_data returns correct wide format data", {
  input_data <- get_input_data_for_get_regional_covid_data_tests()
  expected_data <- get_expected_data_for_get_regional_covid_data_tests()
  iso_codes <- tibble::tibble(iso_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))

  returned_data <- with_mock(get_iso_codes = function(country) return(iso_codes),
                             get_canada_regional_cases = function() return(input_data),
                             get_wide_format_regional_covid_data("canada"))

  expect_equal(expected_data, returned_data)
})

test_that("get_totals_only_regional_covid_data returns correct data", {
  input_data <- get_input_data_for_get_regional_covid_data_tests()
  totals_data <- get_expected_totals_data_for_get_regional_covid_data_tests()
  iso_codes <- tibble::tibble(iso_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))

  returned_data <- with_mock(get_canada_iso_codes = function() return(iso_codes),
                             get_canada_regional_cases = function() return(input_data),
                             get_totals_only_regional_covid_data("canada"))
  expect_equal(totals_data, returned_data)
})

test_that("get_long_format_regional_covid_data returns correct long format data", {
  input_data <- get_input_data_for_get_regional_covid_data_tests()
  expected_data <- convert_to_covid19R_format(get_expected_data_for_get_regional_covid_data_tests())
  iso_codes <- tibble::tibble(iso_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))

  returned_data <- with_mock(get_iso_codes = function(country) return(iso_codes),
                             get_canada_regional_cases = function() return(input_data),
                             get_long_format_regional_covid_data("canada"))

  expect_equal(expected_data, returned_data)
})
