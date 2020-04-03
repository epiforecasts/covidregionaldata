test_that("get_spain_regional_cases cases works as expected", {
  
  base <- get_spain_regional_cases(dataset = 'all')
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$cases_daily) < 0) == 0)
  expect_true(sum(as.numeric(base$hospital_daily) < 0) == 0)
  expect_true(sum(as.numeric(base$icu_daily) < 0) == 0)
  expect_true(sum(as.numeric(base$deaths_daily) < 0) == 0)
  expect_true(sum(as.numeric(base$recover_daily) < 0) == 0)
  expect_true(sum(as.numeric(base$cases_cum) < 0) == 0)
  expect_true(sum(as.numeric(base$hospital_cum) < 0) == 0)
  expect_true(sum(as.numeric(base$icu_cum) < 0) == 0)
  expect_true(sum(as.numeric(base$deaths_cum) < 0) == 0)
  expect_true(sum(as.numeric(base$recover_daily) < 0) == 0)
  
})