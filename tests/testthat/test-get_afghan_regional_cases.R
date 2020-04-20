test_that("get_afghan_regional_cases works as expected", {

  d <- get_afghan_regional_cases()
  expect_is(d, "data.frame")
  expect_is(d$date, "Date")
  expect_is(d$cases, "numeric")
  expect_is(d$deaths, "numeric")
  expect_is(d$recovered, "numeric")
  expect_is(d$province, "character")
  expect_true(all(d$country == "Afghanistan"))
})
