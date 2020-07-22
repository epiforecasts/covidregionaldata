# source('./custom_tests/expect_colname.R')

test_that("get_ecdc_cases works as expected", {

  d <- get_ecdc_cases(countries = "France")
  expect_is(d, "data.frame")
  expect_true(all(d$country == "France"))
  expect_true(sum(as.numeric(d$cases) < 0) == 0)

})

# testthat::test_that("get_ecdc_cases data source is unchanged", {
#   
#   base <- readr::read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
#   
#   expected_colnames = c("dateRep", "day", "month", "year", "cases", "deaths", "countriesAndTerritories", "geoId", "countryterritoryCode", "popData2018")
#   
#   sapply(expected_colnames, expect_colname, colnames = colnames(base))
#   
# })
