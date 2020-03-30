test_that("get_who_cases cases works as expected", {
  
  base <- get_who_cases(country = 'France')
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$cases) < 0) == 0)
  
})
