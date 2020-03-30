test_that("get_international_linelist cases works as expected", {
  
  base <- get_international_linelist(countries = 'United Kingdom', cities = c('London'))
  expect_is(base, "data.frame")
  expect_is(base$date_onset, "Date")
  expect_is(base$date_confirm, "Date")
  
})

