# Tests each dataset returned by countries specified in get_info_covidregionaldata
source("custom_tests/get_data.R")

# 1. Test data.frame is returned
test_that("Level 1 data returned in dataframes", {
  skip_on_cran()
  
  level1_dataframes <- purrr::map(data_level_1, ~ class(.x)) %>%
    purrr::keep(~ any(grepl("data.frame", .x))) %>%
    length()
  
  expect_equal(level1_dataframes, length(countries_level_1))
})

# 2. Test essential column types are appropriate
test_that("Level 1 data has correct col types for date, region, cases", {
  skip_on_cran()
  
  for (country in countries_level_1) {
    expect_s3_class(data_level_1[[country]][["date"]], "Date")
    expect_type(data_level_1[[country]][["region_level_1"]], "character")
    expect_type(data_level_1[[country]][["cases_new"]], "double")
    expect_type(data_level_1[[country]][["cases_total"]], "double")
  }
})
  
# Admin level 2 data -----------------------------------------------------------

# Run tests:

# 1. 
test_that("Level 2 data returns results in dataframes", {
  skip_on_cran()
  
  level2_dataframes <- purrr::map(data_level_2, ~ class(.x)) %>%
    purrr::keep(~ any(grepl("data.frame", .x))) %>%
    length()
  
  expect_equal(level2_dataframes, length(countries_level_2))
})

# 2.
test_that("Level 2 data has correct col types for date, region, and cases columns", {
  skip_on_cran()
  
  for (country in countries_level_2) {
    expect_s3_class(data_level_2[[country]][["date"]], "Date")
    expect_type(data_level_2[[country]][["region_level_1"]], "character")
    expect_type(data_level_2[[country]][["region_level_2"]], "character")
    expect_type(data_level_2[[country]][["cases_new"]], "double")
    expect_type(data_level_2[[country]][["cases_total"]], "double")
  }
})
