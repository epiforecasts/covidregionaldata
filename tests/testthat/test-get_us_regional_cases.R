source('./custom_tests/expect_colname.R')

test_that("get_us_regional_cases cases works as expected", {
  
  base <- get_us_regional_cases(level = 'state', out = 'timeseries')
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$cases) < 0) == 0)
  expect_true(sum(as.numeric(base$deaths) < 0) == 0)
  
  base <- get_us_regional_cases(level = 'county', out = 'total')
  expect_is(base, "data.frame")
  expect_true(sum(as.numeric(base$cases) < 0) == 0)
  expect_true(sum(as.numeric(base$deaths) < 0) == 0)
  
})

test_that("get_us_regional_cases data source is unchanged", {
  
  base <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  
  expected_colnames = c("date", "state", "fips", "cases", "deaths")
  
  sapply(expected_colnames, expect_colname, colnames = colnames(base))
  
  base <- readr::read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  
  expected_colnames = c("date", "county", "state", "fips", "cases", "deaths")
  
  sapply(expected_colnames, expect_colname, colnames = colnames(base))
  
  
})
