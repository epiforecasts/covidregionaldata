source('./custom_tests/expect_colname.R')

test_that("get_afghan_regional_cases works as expected", {

  d <- get_afghan_regional_cases()
  expect_is(d, "data.frame")
  expect_is(d$date, "Date")
  expect_is(d$cases, "integer")
  expect_is(d$deaths, "integer")
  expect_is(d$recovered, "integer")
  expect_is(d$province, "character")
  expect_true(all(d$country == "Afghanistan"))
})

test_that("get_afghan_regional_cases data source is unchanged", {
  
  base <- readr::read_csv("https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv")

  expected_colnames = c("Province", "Cases", "Deaths", "Recoveries", "Active Cases", "Date")
  
  sapply(expected_colnames, expect_colname, colnames = colnames(base))
  
})
