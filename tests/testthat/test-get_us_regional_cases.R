test_that("get_us_regional_cases data source is unchanged", {
  skip_on_cran()
  
  data <- vroom::vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  expected_colnames = c("date", "state", "fips", "cases", "deaths")
  expect_true(all(expected_colnames %in% colnames(data)))
  
  data <- vroom::vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  expected_colnames = c("date", "county", "state", "fips", "cases", "deaths")
  expect_true(all(expected_colnames %in% colnames(data)))
  
})


test_that("get_us_regional_cases returns correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c("region_level_1", "date", "cases_total", "deaths_total")
  returned_colnames_adm_level_1 <- colnames(get_us_regional_cases_only_level_1())
  expect_true(all(returned_colnames_adm_level_1 %in% expected_colnames_adm_level_1))
  expect_true(all(expected_colnames_adm_level_1 %in% returned_colnames_adm_level_1))
  
  
  expected_colnames_adm_level_2 <- c("region_level_1", "region_level_2", "level_2_region_code",
                                     "date", "cases_total", "deaths_total")
  returned_colnames_adm_level_2 <- colnames(get_us_regional_cases_with_level_2())
  expect_true(all(returned_colnames_adm_level_2 %in% expected_colnames_adm_level_2))
  expect_true(all(expected_colnames_adm_level_2 %in% returned_colnames_adm_level_2))
})


test_that("get_us_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_us_regional_cases_only_level_1()
  
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_total, "numeric")
  
  
  data <- get_us_regional_cases_with_level_2()
  
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$region_level_2, "character")
  expect_is(data$level_2_region_code, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_total, "numeric")
})