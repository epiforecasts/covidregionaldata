test_that("format_ecdc_data works as expected", {

  base <- format_ecdc_data(get_ecdc_cases())
  expect_is(base, data.frame)
  expect_is(base$date, Date)
  expect_true(sum(as.numeric(base$cases) < 0) == 0)

  base2 <- format_ecdc_data(get_ecdc_cases(countries = c("France", "Germany")))
  expect_true(all.equal(unique(base2$region), c("France", "Germany")))
})
