test_that("get_belgium_regional_cases cases works as expected", {
  
  base <- get_belgium_regional_cases()
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")

  base <- get_belgium_regional_cases(dataset = "cases_municipal")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")

  base <- get_belgium_regional_cases(dataset = "hospitalisation_provincial")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  
  base <- get_belgium_regional_cases(dataset = "mortality_provincial")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$deaths) < 0) == 0)
  
  base <- get_belgium_regional_cases(dataset = "testing_national")
  expect_is(base, "data.frame")
  expect_is(base$date, "Date")
  expect_true(sum(as.numeric(base$tests) < 0) == 0)
  
  base <- get_belgium_regional_cases(dataset = "all")
  expect_is(base, "list")
  lapply(base, expect_is, class='data.frame')
  
})


test_that("get_belgium_regional_cases data sources are unchanged", {
  
  base <- readr::read_csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")
  
  expect_equal(colnames(base), c("DATE", "PROVINCE", "REGION", "AGEGROUP",  "SEX", "CASES"))
  
  base <- readr::read_csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_MUNI.csv")
  
  expect_equal(colnames(base), c("DATE", "NIS5", "TX_DESCR_NL", "TX_DESCR_FR", "TX_ADM_DSTR_DESCR_NL", 
                                 "TX_ADM_DSTR_DESCR_FR", "TX_PROV_DESCR_NL", "TX_PROV_DESCR_FR", "TX_RGN_DESCR_NL", 
                                 "TX_RGN_DESCR_FR", "CASES"))
  
  base <- readr::read_csv("https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv")
  
  expect_equal(colnames(base), c("DATE", "PROVINCE", "REGION", "NR_REPORTING", "TOTAL_IN", "TOTAL_IN_ICU", 
                                 "TOTAL_IN_RESP", "TOTAL_IN_ECMO", "NEW_IN", "NEW_OUT"))
  
  base <- readr::read_csv("https://epistat.sciensano.be/Data/COVID19BE_MORT.csv")
  
  expect_equal(colnames(base), c("DATE", "REGION", "AGEGROUP",  "SEX", "DEATHS"))
  
  base <- readr::read_csv("https://epistat.sciensano.be/Data/COVID19BE_tests.csv")
  
  expect_equal(colnames(base), c("DATE", "TESTS"))
})

