test_that("get_afghan_regional_cases data source is unchanged", {
  data <- readr::read_csv("https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv")
  expected_colnames <- c("Province", "Cases", "Deaths", "Recoveries", "Active Cases", "Date")
  expect_true(all(expected_colnames %in% colnames(data)))
})

test_that("get_afghan_regional_cases returns the correct column names", {
  expected_colnames <- c("region", "date", "cases_today", "cumulative_cases", "cumulative_deaths",
                         "cumulative_recoveries", "deaths_today", "recoveries_today")

  returned_colnames <- colnames(get_afghan_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_afghan_regional_cases returns correct column types", {
  data <- get_afghan_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_today, "numeric")
  expect_is(data$cumulative_cases, "numeric")
  expect_is(data$deaths_today, "numeric")
  expect_is(data$cumulative_deaths, "numeric")
  expect_is(data$recoveries_today, "numeric")
  expect_is(data$cumulative_recoveries, "numeric")
})
