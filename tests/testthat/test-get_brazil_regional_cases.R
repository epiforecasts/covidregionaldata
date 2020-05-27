test_that("get_brazil_regional_cases data source is unchanged", {
  data <- readr::read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv")
  expected_colnames <- c("date", "country", "state", "city", "ibgeID", "newDeaths", "deaths",
                         "newCases", "totalCases", "deaths_per_100k_inhabitants", "totalCases_per_100k_inhabitants",
                         "deaths_by_totalCases", "_source")
  expect_true(all(expected_colnames %in% colnames(data)))
})

test_that("get_brazil_regional_cases returns the correct column names", {
  expected_colnames <- c("region", "date", "cases_today", "cumulative_cases", "cumulative_deaths", "deaths_today")

  returned_colnames <- colnames(get_brazil_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_brazil_regional_cases returns correct column types", {
  data <- get_brazil_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_today, "numeric")
  expect_is(data$cumulative_cases, "numeric")
  expect_is(data$deaths_today, "numeric")
  expect_is(data$cumulative_deaths, "numeric")
})
