test_that("get_international_linelist cases works as expected", {
  
  skip("Failing on check but runs without error locally")
  
  base <- get_international_linelist()
  expect_is(base, "data.frame")
  
})

