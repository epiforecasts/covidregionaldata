test_that("get_uk_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  
  eng_data <- readr::read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")
  eng_expected_colnames = c("Area name", "Area code", "Area type", "Specimen date", "Daily lab-confirmed cases",
                        "Previously reported daily cases", "Change in daily cases", "Cumulative lab-confirmed cases", 
                        "Previously reported cumulative cases", "Change in cumulative cases", "Cumulative lab-confirmed cases rate")
  expect_true(all(eng_expected_colnames %in% colnames(eng_data)))
  expect_true(max(lubridate::ymd(eng_data$`Specimen date`), na.rm = TRUE) > Sys.Date() - 7)
  
  wales_scot_ni_data <- readr::read_csv("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv")
  wales_scot_ni_expected_colnames = c("Date", "Country", "AreaCode", "Area", "TotalCases")
  expect_true(all(wales_scot_ni_expected_colnames %in% colnames(wales_scot_ni_data)))
  expect_true(max(lubridate::ymd(wales_scot_ni_data$Date), na.rm = TRUE) > Sys.Date() - 7)
})

test_that("get_uk_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c("region_level_1", "date", "iso_code", "cases_new", "cases_total")
  returned_colnames_adm_level_1 <- colnames(get_uk_regional_cases_only_level_1())
  expect_true(all(returned_colnames_adm_level_1 %in% expected_colnames_adm_level_1))
  expect_true(all(expected_colnames_adm_level_1 %in% returned_colnames_adm_level_1))
  
  
  expected_colnames_adm_level_2 <- c("region_level_1", "region_level_2", "date", "iso_code", "level_2_region_code", "cases_new", "cases_total")
  returned_colnames_adm_level_2 <- colnames(get_uk_regional_cases_with_level_2())
  expect_true(all(returned_colnames_adm_level_2 %in% expected_colnames_adm_level_2))
  expect_true(all(expected_colnames_adm_level_2 %in% returned_colnames_adm_level_2))
})

test_that("get_uk_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_uk_regional_cases_only_level_1()
  
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$iso_code, "character")
  expect_is(data$cases_new, "numeric")
  expect_is(data$cases_total, "numeric")
  
  
  data <- get_uk_regional_cases_with_level_2()
  
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$region_level_2, "character")
  expect_is(data$date, "Date")
  expect_is(data$iso_code, "character")
  expect_is(data$level_2_region_code, "character")
  expect_is(data$cases_new, "numeric")
  expect_is(data$cases_total, "numeric")
})

test_that("get_uk_regional_cases returns correct numbers of regions", {
  skip_on_cran()
  
  adm_1_data <- get_uk_regional_cases_only_level_1()
  adm_2_data <- get_uk_regional_cases_with_level_2()
  
  expect_equal(length(unique(na.omit(adm_1_data$region_level_1))), 12)
  expect_equal(length(unique(na.omit(adm_2_data$region_level_2))), 184)
})
