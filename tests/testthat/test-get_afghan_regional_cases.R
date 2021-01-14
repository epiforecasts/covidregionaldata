test_that("get_afghan_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  data <- vroom::vroom("https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv")
  expected_colnames <- c("Province", "Cases", "Deaths", "Recoveries", "Active Cases", "Date")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(as.Date(data$Date[-1]), na.rm = TRUE) > Sys.Date() - 14)
})

test_that("get_afghan_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames <- c("region_level_1", "date", "cases_total", "deaths_total",
                         "recovered_total")

  returned_colnames <- colnames(get_afghan_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_afghan_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_afghan_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_total, "numeric")
  expect_is(data$recovered_total, "numeric")
})
