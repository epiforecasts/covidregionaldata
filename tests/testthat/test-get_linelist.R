test_that("get_linelist works as expected", {
  
  d <- get_linelist()
  expect_is(d, "data.frame")
  
})

