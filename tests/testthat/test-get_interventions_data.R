test_that("get_interventions_data works as expected", {

  d <- get_interventions_data()
  expect_is(d, "data.frame")
  expect_is(d$date_implemented, "Date")
})
