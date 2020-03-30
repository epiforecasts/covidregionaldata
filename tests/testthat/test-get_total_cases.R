test_that("get_total_cases cases works as expected", {
  
  base <- get_total_cases(source = 'WHO')
  expect_is(base, "data.frame")
  expect_true(sum(as.numeric(base$cases[!is.na(base$cases)]) < 0) == 0)
  
  base <- get_total_cases(source = 'ECDC')
  expect_is(base, "data.frame")
  expect_true(sum(as.numeric(base$cases) < 0) == 0)
  
})
