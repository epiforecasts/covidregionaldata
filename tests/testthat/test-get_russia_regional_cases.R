# test_that("get_russia_regional_cases data source is unchanged and up to date", {
#   skip_on_cran()
#
#   data <- vroom::vroom("https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv")
#   expected_colnames <- c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region",
#                          "Lat", "Long_", "Combined_Key")
#   expect_true(all(expected_colnames %in% colnames(data)))
#   expect_true(max(lubridate::mdy(colnames(data)[12:dim(data)[2]]), na.rm = TRUE) > Sys.Date() - 7)
# })
# 
# test_that("get_russia_regional_cases returns the correct column names", {
#   skip_on_cran()
#
#   expected_colnames <- c("region_level_1", "date", "cases_total")
#   returned_colnames <- colnames(get_russia_regional_cases())
#   
#   expect_true(all(returned_colnames %in% expected_colnames))
#   expect_true(all(expected_colnames %in% returned_colnames))
# })
# 
# test_that("get_russia_regional_cases returns correct column types", {
#   skip_on_cran()
#
#   data <- get_russia_regional_cases()
#   expect_is(data, "data.frame")
#   expect_is(data$region_level_1, "character")
#   expect_is(data$date, "Date")
#   expect_is(data$cases_total, "numeric")
# })
