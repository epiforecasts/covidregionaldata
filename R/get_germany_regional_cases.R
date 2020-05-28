#' Get German daily cases by Bundeslander
#'
#' @description Fetches COVID case counts by region in Germany.
#' This data is sourced from the Robert Koch Institute:
#' https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
#' selects and sanitises the relevant columns
#' @return A data.frame of COVID cases by region in Germany, ready to be used by get_regional_covid_data()
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr select group_by mutate summarise %>%
#' @importFrom readr read_csv
#' @importFrom lubridate ymd_hms as_date
#'

get_germany_regional_cases <- function() {

  # Path to data
  url <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"

  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  data <- mem_read(file = url, col_types = readr::cols()) %>%
    dplyr::select(date = Meldedatum,
                  region = Bundesland,
                  cases_today = AnzahlFall,
                  deaths_today = AnzahlTodesfall) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd_hms(date))) %>%
    dplyr::group_by(region, date) %>%
    dplyr::summarise(cases_today = as.numeric(sum(cases_today > 0)),
                     deaths_today = as.numeric(sum(deaths_today > 0)))
  return(data)
}
