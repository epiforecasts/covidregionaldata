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
