test_that("rweibull_with_mean_sd cases works as expected", {
  
  n = 100
  
  base <- rweibull_with_mean_sd(n, 1, 1)
  expect_is(base, "numeric")
  expect_true(length(base) == n)
  
})

