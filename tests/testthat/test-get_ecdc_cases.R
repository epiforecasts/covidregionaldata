test_that("get_ecdc cases works as expected", {

  d <- get_ecdc_cases(countries = "France")
  expect_is(d, "data.frame")
  expect_true(all(d$country == "France"))

})

