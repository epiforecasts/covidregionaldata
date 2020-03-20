test_that("get_france_regional_cases works as expected", {

  d <- get_france_regional_cases()
  expect_is(d, "data.frame")
  expect_is(d$date, "Date")
  expect_is(d$cases, "integer")
  expect_true(all(d$country == "France"))
})
