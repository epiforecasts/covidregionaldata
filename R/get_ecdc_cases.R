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
#' @importFrom dplyr mutate rename select arrange filter
#' @examples
#'
#' ## Get data for france
#' get_ecdc_cases(countries = "France")
#'
#' ## Code
get_ecdc_cases <- function (countries = NULL){
  # Get latest update
  base_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

  ## Set up caching
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  error <- suppressMessages(
    suppressWarnings(try(readr::read_csv(file = base_url), silent = TRUE))
  )

  if ("try-error" %in% class(error)) {
    stop(paste0("No data found. Check ECDC source here: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"))
  }

  d <- mem_read(base_url) %>%
    dplyr::mutate(date = as.Date(dateRep, format = "%d/%m/%Y")) %>%
    dplyr::rename(geoid = geoId, country = countriesAndTerritories,
           cases = cases, deaths = deaths,
           population_2019 = popData2019) %>%
    dplyr::select(-dateRep) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(cases = ifelse(cases < 0, 0, cases))

  if (!is.null(countries)) {
    d <- d %>% dplyr::filter(country %in% countries)
  }
  return(d)
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
    dplyr::select(date, region = country, cases) %>%
    dplyr::arrange(region, date) %>%
    dplyr::filter(region != "Cases_on_an_international_conveyance_Japan") %>%
    dplyr::mutate(region = stringr::str_replace_all(region, "_", " "))

  return(out)
}
