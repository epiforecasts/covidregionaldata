test_that("get_national_data returns ECDC data", {
  skip_on_cran()

  d <- get_national_data(country = "France")
  
  expect_is(d, "data.frame")
  expect_true(all(d$country == "France"))
  expect_true(sum(as.numeric(d$cases_new) < 0) == 0)

})

test_that("get_national_data returns WHO data", {
  skip_on_cran()
  
  d <- get_national_data(country = "France", source = "WHO")
  
  expect_is(d, "data.frame")
  expect_true(all(d$country == "France"))
  expect_true(sum(as.numeric(d$cases_new) < 0) == 0)
  
})


test_that("get_ecdc_cases works as expected", {
  skip_on_cran()
  
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  ecdc <- try(readr::read_csv(url))
  # If no csv, try excel
  if ("try-error" %in% class(ecdc)) {
    url_xl <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-06-21.xlsx"
    httr::GET(url_xl, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    ecdc <-  readxl::read_excel(tf)
  }
  
  necessary_cols <- c("geoId", "countriesAndTerritories", "cases", "deaths", "popData2019")
  
  expect_is(ecdc, "data.frame")
  expect_true(all(necessary_cols %in% colnames(ecdc)))
  
})


test_that("get_who_cases works as expected", {
  skip_on_cran()
  
  json_url <- "https://dashboards-dev.sprinklr.com/data/9043/global-covid19-who-gis.json"
  who <- jsonlite::fromJSON(json_url, flatten = F)
  who <- as.data.frame(who$rows)
  
  expect_equal(ncol(who), 7)
  
})
