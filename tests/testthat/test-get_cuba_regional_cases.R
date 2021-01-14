test_that("get_cuba_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  url <- "https://covid19cubadata.github.io/data/covid19-casos.csv"
  data <- vroom::vroom(url)
  expected_colnames <- c("fecha_confirmacion", "provincia")
  expect_true(all(expected_colnames %in% colnames(data)))
  expect_true(max(as.Date(data$fecha_confirmacion), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_cuba_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames <- c("region_level_1", "date", "cases_new")
  returned_colnames <- colnames(get_cuba_regional_cases())
  
  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_cuba_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_cuba_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
})