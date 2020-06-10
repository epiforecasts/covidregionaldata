#' Get German daily cases by Bundesland only
#'
#' @description Fetches COVID case counts stratified by Bundesland in Germany.
#' This data is sourced from the Robert Koch Institute:
#' https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
#' selects and sanitises the relevant columns
#' @return A data.frame of COVID cases by region in Germany, ready to be used by get_regional_covid_data()
#' @importFrom dplyr select group_by mutate summarise %>%
#' @importFrom lubridate ymd_hms as_date
#'
get_germany_regional_cases_only_level_1 <- function() {

  # Path to data
  url <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"

  data <- csv_reader(file = url) %>%
    dplyr::select(date = Meldedatum,
                  region_level_1 = Bundesland,
                  cases_new = AnzahlFall,
                  deaths_new = AnzahlTodesfall) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd_hms(date))) %>%
    dplyr::group_by(region_level_1, date) %>%
    dplyr::summarise(cases_new = as.numeric(sum(cases_new > 0)),
                     deaths_new = as.numeric(sum(deaths_new > 0))) %>%
    dplyr::ungroup()

  return(data)
}

#' Get German daily cases by Landkreis
#'
#' @description Fetches COVID case counts stratified by Landkreis in Germany.
#' This data is sourced from the Robert Koch Institute:
#' https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0
#' selects and sanitises the relevant columns
#' @return A data.frame of COVID cases by region in Germany, ready to be used by get_regional_covid_data()
#' @importFrom dplyr select group_by mutate summarise %>%
#' @importFrom lubridate ymd_hms as_date
#'
get_germany_regional_cases_with_level_2 <- function() {

  # Path to data
  url <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"

  data <- csv_reader(file = url) %>%
    dplyr::select(date = Meldedatum,
                  region_level_1 = Bundesland,
                  region_level_2 = Landkreis,
                  cases_new = AnzahlFall,
                  deaths_new = AnzahlTodesfall) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd_hms(date)),
                  region_level_2 = gsub("(^[SL]K) (.*)", "\\2 \\(\\1\\)", region_level_2, fixed = FALSE)) %>%
    dplyr::group_by(region_level_1, region_level_2, date) %>%
    dplyr::summarise(cases_new = as.numeric(sum(cases_new > 0)),
                     deaths_new = as.numeric(sum(deaths_new > 0))) %>%
    dplyr::ungroup()

  return(data)
}
