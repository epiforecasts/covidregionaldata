test_that("get_korea_regional_cases cases works as expected", {
  
  base <- get_korea_regional_cases()
  expect_is(base, "data.frame")
  expect_true(sum(as.numeric(base$cases) < 0) == 0)
  
})
