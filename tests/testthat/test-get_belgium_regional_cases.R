test_that("get_belgium_regional_cases data sources are unchanged", {
  c_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
  h_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  m_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"

  cases_data <- readr::read_csv(c_provincial, col_types = readr::cols())
  cases_expected_colnames <- c("DATE", "PROVINCE", "REGION", "AGEGROUP", "SEX", "CASES")
  expect_true(all(cases_expected_colnames %in% colnames(cases_data)))

  hosp_data <- readr::read_csv(h_provincial, col_types = readr::cols())
  hosp_expected_colnames <- c("DATE", "PROVINCE", "REGION", "NR_REPORTING", "TOTAL_IN", "TOTAL_IN_ICU",
                              "TOTAL_IN_RESP", "TOTAL_IN_ECMO", "NEW_IN", "NEW_OUT")
  expect_true(all(hosp_expected_colnames %in% colnames(hosp_data)))

  deaths_data <- readr::read_csv(m_provincial, col_types = readr::cols())
  deaths_expected_colnames <- c("DATE", "REGION", "AGEGROUP", "SEX", "DEATHS")
  expect_true(all(deaths_expected_colnames %in% colnames(deaths_data)))
})

test_that("get_belgium_regional_cases returns the correct column names", {
  expected_colnames <- c("region", "date", "cases_today", "cumulative_cases", "cumulative_deaths",
                         "cumulative_hospitalisations", "deaths_today", "hospitalisations_today")

  returned_colnames <- colnames(get_belgium_regional_cases())

  expect_true(all(returned_colnames %in% expected_colnames))
  expect_true(all(expected_colnames %in% returned_colnames))
})

test_that("get_belgium_regional_cases returns correct column types", {
  data <- get_belgium_regional_cases()
  expect_is(data, "data.frame")
  expect_is(data$region, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_today, "numeric")
  expect_is(data$cumulative_cases, "numeric")
  expect_is(data$deaths_today, "numeric")
  expect_is(data$cumulative_deaths, "numeric")
  expect_is(data$hospitalisations_today, "numeric")
  expect_is(data$cumulative_hospitalisations, "numeric")
})
