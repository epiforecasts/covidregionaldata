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
  
  url <- "https://covid19.who.int/WHO-COVID-19-global-data.csv"
  who <- readr::read_csv(url)
  
  expect_equal(ncol(who), 8)
  
})


test_that("get_ecdc_cases returns a region for every country", {
  skip_on_cran()
  
  library(dplyr)
  
  all_countries <- get_national_data(source = "ecdc")
  
  all_countries <- all_countries %>%  
    dplyr::filter(is.na(un_region)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::tally()
  
  expect_true(nrow(all_countries) == 0)
})


test_that("get_who_cases returns a region for every country", {
  skip_on_cran()
  
  library(dplyr)
  
  all_countries <- get_national_data(source = "who")
  
  all_countries <- all_countries %>%  
    dplyr::filter(is.na(un_region), !is.na(country)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::tally()
  
  expect_true(nrow(all_countries) == 0)
})
