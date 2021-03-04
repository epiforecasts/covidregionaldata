# Tests each dataset returned by countries specified in get_info_covidregionaldata
source("custom_tests/skip_if_not_scheduled.R")
if (!testthat:::on_cran()) {
  if (lubridate::hour(Sys.time()) == 0) {
    source("custom_tests/get_data.R")
  }
}

# Admin level 1 -----------------------------------------------------------
# 1. Test data.frame is returned
test_that("All Level 1 countries return results", {
  skip_on_cran()
  skip_if_not_scheduled()
  expect_length(data_level_1, length(countries_level_1))
})
# 2. Test essential column types are appropriate
test_that("Level 1 data has correct col types for date, region, cases", {
  skip_on_cran()
  skip_if_not_scheduled()
  for (country in success_level_1) {
    expect_data_type(
      country = country,
      data_list = data_level_1
    )
  }
})
# Admin level 2 -----------------------------------------------------------
# 1.
test_that("All Level 2 countries return results", {
  skip_on_cran()
  skip_if_not_scheduled()
  expect_length(data_level_2, length(countries_level_2))
})
# 2.
test_that("Level 2 data has correct col types for date, region, and cases columns", {
  skip_on_cran()
  skip_if_not_scheduled()
  for (country in success_level_2) {
    expect_data_type(
      country = country,
      data_list = data_level_2,
      level = 2
    )
  }
})
