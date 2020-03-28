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
get_ecdc_cases <- function (countries = NULL)
{
  # Get latest update
  base_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

  ## Set up caching
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  error <- suppressMessages(
    suppressWarnings(try(readr::read_csv(file = base_url), silent = TRUE))
  )

  if (class(error) == "try-error") {
    stop(paste0("No data found. Check ECDC source here: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide"))
  }

  d <- readr::read_csv(base_url) %>%
    dplyr::mutate(date = as.Date(dateRep, format = "%d/%m/%Y")) %>%
    rename(geoid = geoId, country = countriesAndTerritories,
           cases = cases, death = deaths,
           population_2018 = popData2018) %>%
    select(-dateRep) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(cases = ifelse(cases < 0, 0, cases))

  if (!is.null(countries)) {
    d <- d %>% dplyr::filter(country %in% countries)
  }
  return(d)
}
