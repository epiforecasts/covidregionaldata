test_that("get_afghan_regional_cases data source is unchanged", {
  data <- readr::read_csv("https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv")
  expected_colnames <- c("Province", "Cases", "Deaths", "Recoveries", "Active Cases", "Date")
  expect_true(all(expected_colnames %in% colnames(data)))
})

test_that("get_afghan_regional_cases returns the correct column names", {
  expected_colnames <- c("region", "date", "cases_total", "deaths_total",
                         "recoveries_total")

  returned_colnames <- colnames(get_afghan_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_afghan_regional_cases returns correct column types", {
  data <- get_afghan_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_total, "numeric")
  expect_is(data$recoveries_total, "numeric")
})
