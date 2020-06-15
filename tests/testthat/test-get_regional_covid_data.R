## Errors are handled correctly
test_that("get_regional_covid_data returns error if country argument is not a string", {
  expect_error(get_regional_covid_data(country = 4, totals = TRUE, include_level_2_regions = FALSE))
})

test_that("get_regional_covid_data returns error if there is no data for the country entered", {
  expect_error(get_regional_covid_data(country = "Narnia", totals = TRUE, include_level_2_regions = FALSE))
})

test_that("get_regional_covid_data returns error if totals arg is not logical", {
  expect_error(get_regional_covid_data(country = "Belgium", totals = 3, include_level_2_regions = FALSE))
})

test_that("get_regional_covid_data returns error if totals arg is not logical", {
  expect_error(get_regional_covid_data(country = "Belgium", totals = FALSE, include_level_2_regions = "Yes"))
})

#-----------------------------------------#
#---------- Happy case - set up ----------#
#-----------------------------------------#

## this file contains functions which create data specifically for these tests
source("custom_tests/mock_data.R")

test_that("get_wide_format_regional_covid_data returns correct wide format data - admin level 1 only", {
  # Set up and run
  input_data <- get_input_data_for_get_regional_covid_data_tests_only_level_1_regions()
  iso_codes <- tibble::tibble(iso_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  returned_data <- with_mock(get_canada_iso_codes = function(country) return(iso_codes),
                             get_canada_regional_cases = function() return(input_data),
                             get_wide_format_regional_covid_data("canada", include_level_2_regions = FALSE))

  # expected data
  expected_data <- get_expected_data_for_get_regional_covid_data_tests_only_level_1_regions()

  expect_equal(expected_data, returned_data)
})

test_that("get_wide_format_regional_covid_data returns correct wide format data - admin level 2", {
  # Set up and run
  input_data <- get_input_data_for_get_regional_covid_data_tests_with_level_2_regions()
  iso_codes <- tibble::tibble(iso_code = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))
  level_2_region_codes <- tibble::tibble(level_2_region_code = c("NO", "EA", "SO", "WE", "VA"),
                                      region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  
  returned_data <- with_mock(get_iso_codes = function(country) return(iso_codes),
                             get_level_2_region_codes = function(country)  return(level_2_region_codes),
                             get_belgium_regional_cases_with_level_2 = function() return(input_data),
                             get_wide_format_regional_covid_data("belgium", include_level_2_regions = TRUE))

  # expected data
  expected_data <- get_expected_data_for_get_regional_covid_data_tests_with_level_2_regions()

  expect_equal(expected_data, returned_data)
})

test_that("get_totals_only_regional_covid_data returns correct data - admin level 1", {
  # Set up and run
  input_data <- get_input_data_for_get_regional_covid_data_tests_only_level_1_regions()
  iso_codes <- tibble::tibble(iso_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  returned_data <- with_mock(get_canada_iso_codes = function() return(iso_codes),
                             get_canada_regional_cases = function() return(input_data),
                             get_totals_only_regional_covid_data("canada", include_level_2_regions = FALSE))

  # expected data
  totals_data <- get_expected_totals_data_for_get_regional_covid_data_tests_only_level_1_regions()

  expect_equal(totals_data, returned_data)
})

test_that("get_totals_only_regional_covid_data returns correct data - admin level 2", {
  # Set up and run
  input_data <- get_input_data_for_get_regional_covid_data_tests_with_level_2_regions()
  iso_codes <- tibble::tibble(iso_code = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))
  level_2_region_codes <- tibble::tibble(level_2_region_code = c("NO", "EA", "SO", "WE", "VA"),
                                      region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  
  returned_data <- with_mock(get_iso_codes = function(country) return(iso_codes),
                             get_level_2_region_codes = function(country) return(level_2_region_codes),
                             get_belgium_regional_cases_with_level_2 = function() return(input_data),
                             get_totals_only_regional_covid_data("belgium", include_level_2_regions = TRUE))

  # expected data
  totals_data <- get_expected_totals_data_for_get_regional_covid_data_tests_with_level_2_regions()

  expect_equal(totals_data, returned_data)
})

test_that("get_long_format_regional_covid_data returns correct long format data", {
  # Set up and run
  input_data <- get_input_data_for_get_regional_covid_data_tests_only_level_1_regions()
  iso_codes <- tibble::tibble(iso_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  returned_data <- with_mock(get_canada_iso_codes = function(country) return(iso_codes),
                             get_canada_regional_cases = function() return(input_data),
                             get_long_format_regional_covid_data("canada"))

  # expected data
  expected_data <- convert_to_covid19R_format(get_expected_data_for_get_regional_covid_data_tests_only_level_1_regions())

  expect_equal(expected_data, returned_data)
})
