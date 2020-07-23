test_that("get_colombia_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  data <- readr::read_csv("https://raw.githubusercontent.com/danielcs88/colombia_covid-19/master/datos/cronologia.csv")
  expected_colnames <- c("fecha", "departamento", "casos")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(lubridate::ymd(data$fecha), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_colombia_regional_cases returns the correct column names", {
  expected_colnames <- c("region_level_1", "date", "cases_total")
  
  returned_colnames <- colnames(get_colombia_regional_cases())
  
  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_colombia_regional_cases returns correct column types", {
  data <- get_colombia_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_total, "numeric")
})
