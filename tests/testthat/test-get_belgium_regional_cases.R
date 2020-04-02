test_that("get_belgium_regional_cases cases works as expected", {
  
  base <- get_belgium_regional_cases()
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")

  base <- get_belgium_regional_cases(dataset = "cases_municipal")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")

  base <- get_belgium_regional_cases(dataset = "hospitalisation_provincial")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  
  base <- get_belgium_regional_cases(dataset = "mortality_provincial")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$deaths) < 0) == 0)
  
  base <- get_belgium_regional_cases(dataset = "testing_national")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$tests) < 0) == 0)
  
})