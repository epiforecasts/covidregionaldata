#' Get International Linelist Data
#'
#' @description This function downloads the latest non-Hubei linelist. It uses `memoise` to cache the
#' results locally. To clear the cache and redownload the data use `reset_cache`. The cache will be located
#' in the directory at which the function is run. As this linelist is experiencing a high user demand it may not always be available.
#' To account for this we keep a cache in the `NCoVUtils` GitHub repo which this function will fall back to with a warning if the source
#' cannot be downloaded.
#' @param countries Character vector identifying the countries to extract data for.
#' @param cities Character vector identifying the cities to extract data for.
#' @param provinces  Character vector identifying the provinces to extract data for.
#' @param clean Logical, defaults to `TRUE`. Should the data returned be cleaned for use.
#' @importFrom dplyr if_else select mutate filter
#' @importFrom readr read_csv
#' @importFrom lubridate dmy
#' @importFrom tibble as_tibble
#' @importFrom tidyr drop_na
#' @importFrom memoise memoise cache_filesystem
#' @importFrom R.utils withTimeout
#' @return A linelist of case data
#' @export
#' @author Sam Abbott <sam.abbott@lshtm.ac.uk>
#' @examples
#'
#'get_international_linelist
get_international_linelist <- function(countries = NULL, cities = NULL, provinces = NULL, clean = TRUE) {

  country <- NULL; city <- NULL; travel_history_location <- NULL;
  travel_history_dates <- NULL; date_confirmation <- NULL;
  date_onset_symptoms <- NULL; date_confirm <- NULL;
  date_onset <- NULL; report_delay <- NULL; import_status <- NULL;

  message("Downloading linelist data")


  ch <- memoise::cache_filesystem(".cache")

  url <- "https://raw.githubusercontent.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.csv"

  mem_read <- memoise::memoise(readr::read_csv, cache = ch)
  linelist <- suppressWarnings(
    suppressMessages(
      try(R.utils::withTimeout(mem_read(url) %>%
        tibble::as_tibble(), timeout = 15, onTimeout = "error"),
        silent = TRUE)
    )
  )

  if (any(class(linelist) %in% "try-error")) {
    warning("Could not access linelist source. Using the NCoVUtils cache, this may not be up to date.
    See the git history to confirm last cache date.")

    url <- "https://raw.githubusercontent.com/epiforecasts/NCoVUtils/master/data-raw/linelist.csv"

      linelist <- suppressWarnings(
        suppressMessages(
          mem_read(url) %>%
            tibble::as_tibble()
        )
      )
  }

  if (!is.null(countries)) {
    linelist <- linelist %>%
      dplyr::filter(country %in% countries)
  }

  if (!is.null(cities)) {
    linelist <- linelist %>%
      dplyr::filter(city %in% cities)
  }

  if (!is.null(provinces)) {
    linelist <- linelist %>%
      dplyr::filter(province %in% provinces)
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
      dplyr::select(date_onset, date_confirm, report_delay, import_status, country) %>%
      tidyr::drop_na(date_confirm)

  }

  return(linelist)

}
