test_that("get_germany_regional_cases cases works as expected", {
  
  base <- get_germany_regional_cases()
  expect_is(base, "data.frame")
  expect_true(class(base$date) == 'Date')
  expect_true(sum(as.numeric(base$cases) < 0) == 0)
  
})

