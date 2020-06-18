test_that("get_colombia_regional_cases data source is unchanged and up to date", {
  data <- readr::read_csv("https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv")
  expected_colnames <- c("fecha", "codigo", "departamento", "pruebas", "casos_confirmados", "casos_fallecido")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(lubridate::mdy(data$fecha), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_colombia_regional_cases returns the correct column names", {
  expected_colnames <- c("region_level_1", "date", "cases_total", "deaths_total",
                         "tested_total", "iso_3166_2")
  
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
  expect_is(data$deaths_total, "numeric")
  expect_is(data$tested_total, "numeric")
  expect_is(data$iso_3166_2, "character")
})
