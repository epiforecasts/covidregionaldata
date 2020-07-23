
# ECDC data ---------------------------------------------------------------
#' ECDC International Case Counts: works within get_national_data
#'
#' @author Sam Abbott @seabbs
#' @author D. van Muijen @dmuijen
#' @author Kath Sherratt @kathsherratt
#' @author Haze Lee @hazealign
#'
#'
#' @return A dataframe of International case counts published by ECDC.
#' @importFrom readr read_csv
#' @importFrom httr GET write_disk
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate rename select arrange filter
#' @importFrom countrycode countryname
#'
#'
get_ecdc_cases <- function(){
  
  # Try csv from ECDC
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  raw <- try(csv_reader(file = url))
  
  # If no csv ,try excel
  if ("try-error" %in% class(raw)) {
    message("ECDC csv unavailable, trying alternative with temp file.")
    url_xl <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-06-21.xlsx"
    httr::GET(url_xl, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    raw <-  readxl::read_excel(tf)
  }
  
  # Clean data
  data <- raw %>%
    dplyr::mutate(date = as.Date(dateRep, format = "%d/%m/%Y")) %>%
    dplyr::rename(iso_code = geoId, country = countriesAndTerritories,
                  cases_new = cases, deaths_new = deaths,
                  population_2019 = popData2019) %>%
    dplyr::select(date, country, iso_code, population_2019, cases_new, deaths_new) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(country != "Cases_on_an_international_conveyance_Japan") %>%
    dplyr::mutate(cases_new = ifelse(cases_new < 0, 0, cases_new),
                  country = stringr::str_replace_all(country, "_", " "),
                  country = countrycode::countryname(country, destination = "country.name.en", warn = FALSE),
                  un_region = countrycode::countrycode(iso_code, origin = "iso2c", destination = "un.region.name", warn = FALSE),
                  # Correct for Kosovo
                  un_region = ifelse(iso_code == "XK", "Europe", un_region))
  
  return(data)
  
}


# WHO data ----------------------------------------------------------------

#' Download the most recent WHO case data
#'
#' @description Downloads the latest WHO case data.
#'
#' @return A tibble of all WHO data by date
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate
#' @importFrom countrycode countrycode


get_who_cases <- function() {
  
  if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      # Set up cache
      ch <- memoise::cache_filesystem(".cache")
      read_fun <- memoise::memoise(jsonlite::fromJSON, cache = ch)
    }
  }else{
    read_fun <- jsonlite::fromJSON
  }
  
  # Get data
  json_url <- "https://dashboards-dev.sprinklr.com/data/9043/global-covid19-who-gis.json"
  
  raw <- read_fun(json_url, flatten = F)
  
  raw <- as.data.frame(raw$rows)
  
  colnames(raw) <- c("date", "iso_code", "who_region", "cases_new", "cases_total", "deaths_new", "deaths_total")
  
  # Reset columns to numeric or date where needed
  cols <- c(1, 4:7)
  raw[,cols] <- sapply(raw[,cols], as.character)
  raw[,cols] <- sapply(raw[,cols], as.numeric)
  
  raw$date <- as.Date(as.POSIXct(raw$date / 1000, origin="1970-01-01"))
  
  # Add standard country names
  who <- raw %>%
    dplyr::mutate(country = countrycode::countrycode(iso_code, 
                                                     origin = "iso2c", destination = "country.name.en", warn = FALSE),
                  un_region = countrycode::countrycode(iso_code, 
                                                       origin = "iso2c", destination = "un.region.name", warn = FALSE),
                  # Correct for Kosovo
                  un_region = ifelse(iso_code == "XK", "Europe", un_region))
  
  return(who)
  
}


