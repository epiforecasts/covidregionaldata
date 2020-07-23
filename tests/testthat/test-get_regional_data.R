## Errors are handled correctly
test_that("get_regional_data returns error if country argument is not a string", {
  skip_on_cran()
  
  expect_error(get_regional_data(country = 4, totals = TRUE, include_level_2_regions = FALSE))
})

test_that("get_regional_data returns error if there is no data for the country entered", {
  expect_error(get_regional_data(country = "Narnia", totals = TRUE, include_level_2_regions = FALSE))
})

test_that("get_regional_data returns error if totals arg is not logical", {
  skip_on_cran()
  
  expect_error(get_regional_data(country = "Belgium", totals = 3, include_level_2_regions = FALSE))
})

test_that("get_regional_data returns error if totals arg is not logical", {
  skip_on_cran()
  
  expect_error(get_regional_data(country = "Belgium", totals = FALSE, include_level_2_regions = "Yes"))
})

#-----------------------------------------#
#---------- Happy case - set up ----------#
#-----------------------------------------#
test_that("get_regional_data returns correct time series data - admin level 1 regions only", {
  skip_on_cran()
  
  # Set up and run
  input_data <- get_input_data_for_get_regional_data_tests_only_level_1_regions()
  region_codes <- tibble::tibble(level_1_region_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  returned_data <- with_mock("covidregionaldata:::get_canada_region_codes" = function(country) return(region_codes),
                             "covidregionaldata:::get_canada_regional_cases" = function() return(input_data),
                             get_regional_data("canada", include_level_2_regions = FALSE))

  # expected data
  expected_data <- get_expected_data_for_get_regional_data_tests_only_level_1_regions()

  expect_equal(expected_data, returned_data)
})

test_that("get_regional_data returns correct time series - incl. admin level 2 regions", {
  skip_on_cran()
  
  # Set up and run
  input_data <- get_input_data_for_get_regional_data_tests_with_level_2_regions()
  region_codes <- tibble::tibble(level_1_region_code = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))
  level_2_region_codes <- tibble::tibble(level_2_region_code = c("NO", "EA", "SO", "WE", "VA"),
                                      region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  
  returned_data <- with_mock("covidregionaldata:::get_region_codes" = function(country) return(region_codes),
                             "covidregionaldata:::get_level_2_region_codes" = function(country)  return(level_2_region_codes),
                             "covidregionaldata:::get_belgium_regional_cases_with_level_2" = function() return(input_data),
                             get_regional_data("belgium", include_level_2_regions = TRUE))

  # expected data
  expected_data <- get_expected_data_for_get_regional_data_tests_with_level_2_regions()

  expect_equal(expected_data, returned_data)
})

test_that("get_regional_data returns correct totals data - admin level 1 regions only", {
  skip_on_cran()
  
  # Set up and run
  input_data <- get_input_data_for_get_regional_data_tests_only_level_1_regions()
  region_codes <- tibble::tibble(level_1_region_code = c("NO", "EA", "SO", "WE", "VA"),
                              region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  returned_data <- with_mock("covidregionaldata:::get_canada_region_codes" = function() return(region_codes),
                             "covidregionaldata:::get_canada_regional_cases" = function() return(input_data),
                             get_regional_data("canada", totals = TRUE, include_level_2_regions = FALSE))

  # expected data
  totals_data <- get_expected_totals_data_for_get_regional_data_tests_only_level_1_regions()

  expect_equal(totals_data, returned_data)
})

test_that("get_regional_data returns correct totals data - incl. admin level 2 regions", {
  skip_on_cran()
  
  # Set up and run
  input_data <- get_input_data_for_get_regional_data_tests_with_level_2_regions()
  region_codes <- tibble::tibble(level_1_region_code = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))
  level_2_region_codes <- tibble::tibble(level_2_region_code = c("NO", "EA", "SO", "WE", "VA"),
                                      region = c("Northland", "Eastland", "Southland", "Westland", "Virginia"))
  
  returned_data <- with_mock("covidregionaldata:::get_region_codes" = function(country) return(region_codes),
                             "covidregionaldata:::get_level_2_region_codes" = function(country) return(level_2_region_codes),
                             "covidregionaldata:::get_belgium_regional_cases_with_level_2" = function() return(input_data),
                             get_regional_data("belgium", totals = TRUE, include_level_2_regions = TRUE))

  # expected data
  totals_data <- get_expected_totals_data_for_get_regional_data_tests_with_level_2_regions()

  expect_equal(totals_data, returned_data)
})

