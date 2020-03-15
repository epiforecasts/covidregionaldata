#' Get International Linelist Data
#'
#' @description This function downloads the latest non-Hubei linelist. It uses `memoise` to cache the
#' results locally. To clear the cache and redownload the data use `reset_cache`. The cache will be located
#' in the directory at which the function is run.
#' @param countries Character vector identifying the countries to extract data for.
#' @param cities Character vector identifying the cities to extract data for.
#' @param clean Logical, defaults to `TRUE`. Should the data returned be cleaned for use.
#' @importFrom dplyr if_else select mutate filter
#' @importFrom readr read_csv
#' @importFrom lubridate dmy
#' @importFrom tibble tibble
#' @importFrom memoise memoise cache_filesystem
#'
#' @return A linelist of case data
#' @export
#' @author Sam Abbott <sam.abbott@lshtm.ac.uk>
#' @examples
#'
#'get_international_linelist
get_international_linelist <- function(countries = NULL, cities = NULL, clean = TRUE) {

  country <- NULL; city <- NULL; travel_history_location <- NULL;
  travel_history_dates <- NULL; date_confirmation <- NULL;
  date_onset_symptoms <- NULL; date_confirm <- NULL;
  date_onset <- NULL; report_delay <- NULL; import_status <- NULL;

  message("Downloading linelist data")


  ch <- memoise::cache_filesystem(".cache")

  gid <- c(outside_hubei = 0)
  url <- paste0("https://docs.google.com/spreadsheets/d/",
                "1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/pub",
                "?single=true&output=csv&gid=", gid)

  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  linelist <- suppressWarnings(
    suppressMessages(
      mem_read(url) %>%
        tibble::as_tibble()
    )
  )

  if (!is.null(countries)) {
    linelist <- linelist %>%
      dplyr::filter(country %in% countries)
  }

  if (!is.null(cities)) {
    linelist <- linelist %>%
      dplyr::filter(city %in% cities)
  }

  if (clean) {
    linelist <- linelist %>%
      dplyr::mutate(travel_history_location = ifelse(travel_history_location %in% "", NA, travel_history_location),
                    travel_history_dates = ifelse(travel_history_dates %in% "", NA, travel_history_dates)) %>%
      dplyr::mutate(import_status =
                      dplyr::if_else(!is.na(travel_history_location) | !is.na(travel_history_dates), "imported", "local"),
                    date_confirm = lubridate::dmy(date_confirmation),
                    date_onset = lubridate::dmy(date_onset_symptoms),
                    report_delay =
                      as.integer(as.Date(date_confirm) - as.Date(date_onset))) %>%
      dplyr::select(date_onset, date_confirm, report_delay, import_status)

  }

  return(linelist)

}
