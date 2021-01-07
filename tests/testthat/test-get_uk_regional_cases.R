test_that("get_uk_regional_cases data source is unchanged and up to date", {
  skip_on_cran()
  api <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","areaName":"areaName","areaCode":"areaCode","newCasesByPublishDate":"newCasesByPublishDate","newDeaths28DaysByPublishDate":"newDeaths28DaysByPublishDate"}&format="csv'
  get <- httr::GET(api)
  last_modified <- stringr::str_sub(get[["headers"]][["last-modified"]], start = 6, end = -14)
  
  expect_lt(get$status_code, 399)
  expect_true(lubridate::dmy(last_modified) > Sys.Date() - 7)
  
})

test_that("get_uk_regional_cases returns the correct column names", {
  skip_on_cran()
  
  expected_colnames_adm_level_1 <- c("region_level_1", "date", "level_1_region_code", "cases_new", "cases_total", "deaths_new", "deaths_total")
  returned_colnames_adm_level_1 <- colnames(get_uk_regional_cases_only_level_1())
  expect_true(all(expected_colnames_adm_level_1 %in% returned_colnames_adm_level_1))
  
  
  expected_colnames_adm_level_2 <- c("region_level_2", "date", "level_2_region_code", "cases_new", "cases_total", "deaths_new", "deaths_total")
  returned_colnames_adm_level_2 <- colnames(get_uk_regional_cases_with_level_2())
  expect_true(all(expected_colnames_adm_level_2 %in% returned_colnames_adm_level_2))
})

test_that("get_uk_regional_cases returns correct column types", {
  skip_on_cran()
  
  data <- get_uk_regional_cases_only_level_1()
  
  expect_is(data, "data.frame")
  expect_is(data$region_level_1, "character")
  expect_is(data$date, "Date")
  expect_is(data$level_1_region_code, "character")
  
  
  data <- get_uk_regional_cases_with_level_2()
  
  expect_is(data, "data.frame")
  expect_is(data$region_level_2, "character")
  expect_is(data$date, "Date")
  expect_is(data$level_2_region_code, "character")
})

test_that("get_uk_regional_cases returns correct numbers of regions", {
  skip_on_cran()
  
  adm_1_data <- get_uk_regional_cases_only_level_1()
  adm_2_data <- get_uk_regional_cases_with_level_2()
  
  expect_equal(length(unique(na.omit(adm_1_data$region_level_1))), 13)
  expect_gt(length(unique(na.omit(adm_2_data$region_level_2))), 20)
})

test_that("get_uk_regional_cases returns data by date of release", {
  adm_1_data <- get_uk_regional_cases_only_level_1(release_date = "2020-09-01")
  nhsregions_data <- get_uk_regional_cases_only_level_1(release_date = Sys.Date() - 6, 
                                                        nhsregions = TRUE)
  adm_2_data <- get_uk_regional_cases_with_level_2(release_date = "2020-09-01")
  expect_equal(max(adm_1_data$date), as.Date("2020-09-01"))
  expect_equal(max(nhsregions_data$date), Sys.Date() - 6)
  expect_equal(max(adm_2_data$date), as.Date("2020-09-01"))
  expect_error(get_uk_regional_cases_only_level_1(release_date = "2020-11-01", nhsregions = TRUE))
})
