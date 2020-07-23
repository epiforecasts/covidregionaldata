test_that("get_belgium_regional_cases data sources are unchanged and up to date", {
  skip_on_cran()
  
  c_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
  h_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  m_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"

  cases_data <- readr::read_csv(c_provincial, col_types = readr::cols())
  cases_expected_colnames <- c("DATE", "PROVINCE", "REGION", "AGEGROUP", "SEX", "CASES")
  expect_true(all(cases_expected_colnames %in% colnames(cases_data)))
  expect_true(max(as.Date(cases_data$DATE), na.rm = TRUE) > Sys.Date() - 7)

  hosp_data <- readr::read_csv(h_provincial, col_types = readr::cols())
  hosp_expected_colnames <- c("DATE", "PROVINCE", "REGION", "NR_REPORTING", "TOTAL_IN", "TOTAL_IN_ICU",
                              "TOTAL_IN_RESP", "TOTAL_IN_ECMO", "NEW_IN", "NEW_OUT")
  expect_true(all(hosp_expected_colnames %in% colnames(hosp_data)))
  expect_true(max(as.Date(hosp_data$DATE), na.rm = TRUE) > Sys.Date() - 7)
  
  deaths_data <- readr::read_csv(m_provincial, col_types = readr::cols())
  deaths_expected_colnames <- c("DATE", "REGION", "AGEGROUP", "SEX", "DEATHS")
  expect_true(all(deaths_expected_colnames %in% colnames(deaths_data)))
  expect_true(max(as.Date(deaths_data$DATE), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_belgium_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c("region_level_1", "date", "cases_new", "deaths_new", "hosp_new")
  returned_colnames_adm_level_1 <- colnames(get_belgium_regional_cases_only_level_1())
  expect_true(all(returned_colnames_adm_level_1 %in% expected_colnames_adm_level_1))
  expect_true(all(expected_colnames_adm_level_1 %in% returned_colnames_adm_level_1))


  expected_colnames_adm_level_2 <- c("region_level_1", "region_level_2", "date", "cases_new", "hosp_new")
  returned_colnames_adm_level_2 <- colnames(get_belgium_regional_cases_with_level_2())
  expect_true(all(returned_colnames_adm_level_2 %in% expected_colnames_adm_level_2))
  expect_true(all(expected_colnames_adm_level_2 %in% returned_colnames_adm_level_2))
})

test_that("get_belgium_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_belgium_regional_cases_only_level_1()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$deaths_new, "numeric")
  expect_is(data$hosp_new, "numeric")


  data <- get_belgium_regional_cases_with_level_2()

  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$region_level_2, "character")
  expect_is(data$date, "Date")
  expect_is(data$cases_new, "numeric")
  expect_is(data$hosp_new, "numeric")
})


test_that("get_belgium_regional_cases returns correct numbers of regions", {
  skip_on_cran()
  
  adm_1_data <- get_belgium_regional_cases_only_level_1()
  adm_2_data <- get_belgium_regional_cases_with_level_2()

  expect_equal(length(unique(na.omit(adm_1_data$region_level_1))), 4)
  expect_equal(length(unique(na.omit(adm_2_data$region_level_2))), 12)
})
