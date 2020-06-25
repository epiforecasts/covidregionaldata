#' ECDC International Case Counts
#'
#' @author Sam Abbott @seabbs
#' @author D. van Muijen @dmuijen
#' @author Kath Sherratt @kathsherratt
#' @author Haze Lee @hazealign
#'
#'
#' @return A dataframe of International case counts published by ECDC.
#' @export
#' @inheritParams get_international_linelist
#' @importFrom memoise cache_filesystem memoise
#' @importFrom readr read_csv
#' @importFrom httr GET write_disk
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate rename select arrange filter
#' @examples
#'
#' ## Get data for france
#' get_ecdc_cases(countries = "France")
#'
#' ## Code
get_ecdc_cases <- function (){
  # Get latest update
  url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

  error <- suppressMessages(
    suppressWarnings(try(readr::read_csv(file = url), silent = TRUE))
  )

  if ("try-error" %in% class(error)) {
    message("csv unavailable, trying alternative with temp file")
    
    url_xl <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-06-21.xlsx"
    httr::GET(url_xl, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
      data <-  readxl::read_excel(tf) %>%
      dplyr::mutate(date = as.Date(dateRep, format = "%d/%m/%Y")) %>%
      dplyr::rename(iso_code = geoId, country = countriesAndTerritories,
                    continent = continentExp, cases_new = cases, deaths_new = deaths,
                    population_2019 = popData2019) %>%
      dplyr::select(date, continent, country, iso_code, population_2019, cases_new, deaths_new) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(cases_new = ifelse(cases_new < 0, 0, cases_new),
                    country = stringr::str_replace_all(country, "_", " "))

    return(data)
    
    if ("try-error" %in% class(error)) {
      stop(paste0("No data found. Check ECDC source here: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"))
    }
    
  }

  data <- csv_reader(file = url) %>%
    dplyr::mutate(date = as.Date(dateRep, format = "%d/%m/%Y")) %>%
    dplyr::rename(iso_code = geoId, country = countriesAndTerritories,
                  continent = continentExp, cases_new = cases, deaths_new = deaths,
                  population_2019 = popData2019) %>%
    dplyr::select(date, continent, country, iso_code, population_2019, cases_new, deaths_new) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(cases_new = ifelse(cases_new < 0, 0, cases_new),
                  country = stringr::str_replace_all(country, "_", " "))


}



#' Format ECDC data for use with EpiNow analysis
#'
#' @author Joel Hellewell
#' @param data data.frame returned by get_ecdc_cases function
#'
#' @return data.frame with formatted ecdc case data by country
#' @importFrom dplyr select arrange filter mutate
#' @importFrom stringr str_replace_all
#' @export
#'
#' @examples
#' format_ecdc_data(get_ecdc_cases(countries = "France"))
format_ecdc_data <- function(data = NULL) {

  out <- data %>%
    dplyr::select(date, region = country, cases = cases_new) %>%
    dplyr::arrange(region, date) %>%
    dplyr::filter(region != "Cases_on_an_international_conveyance_Japan")

  return(out)
}
