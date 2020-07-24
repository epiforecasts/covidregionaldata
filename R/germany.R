#' German Regional Daily COVID-19 Count Data - Bundesland
#'
#' @description Extracts daily COVID-19 data for Germany, stratified by Bundesland.
#' Data available at  \url{https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0}. 
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by Bundesland in Germany, ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr select group_by mutate summarise %>% ungroup
#' @importFrom lubridate ymd_hms as_date
#'
get_germany_regional_cases_only_level_1 <- function() {

  # Read data --------------------------------------------------------------------
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

#' German Regional Daily COVID-19 Count Data - Landkreis
#'
#' @description Extracts daily COVID-19 data for Germany, stratified by Landkreis.
#' Data available at  \url{https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv}.
#' It is loaded and then sanitised.
#' @return A data.frame of COVID cases by Landkreis in Germany, ready to be used by get_regional_data().
#' @importFrom dplyr select group_by mutate summarise %>% ungroup
#' @importFrom lubridate ymd_hms as_date
get_germany_regional_cases_with_level_2 <- function() {

  # Read data --------------------------------------------------------------------
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
