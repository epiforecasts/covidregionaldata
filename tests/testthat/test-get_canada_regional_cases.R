test_that("get_canada_regional_cases data source is unchanged", {
  data <- readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv")
  expected_colnames <- c("pruid", "prname", "prnameFR", "date", "numconf", "numprob",
                        "numdeaths", "numtotal", "numtested", "numrecover", "percentrecover",
                        "ratetested", "numtoday", "percentoday")
  expect_true(all(expected_colnames %in% colnames(data)))
})

test_that("get_canada_regional_cases returns the correct column names", {
  expected_colnames <- c("region_level_1", "date", "cases_new", "cases_total", "deaths_total", "recoveries_total", "tests_total")

  returned_colnames <- colnames(get_canada_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_canada_regional_cases returns correct column types", {
  data <- get_canada_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$cases_total, "numeric")
  expect_is(data$deaths_total, "numeric")
  expect_is(data$recoveries_total, "numeric")
  expect_is(data$tests_total, "numeric")
})
