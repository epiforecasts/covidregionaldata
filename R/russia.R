#' Russian Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for Russia, stratified by Region. 
#' Data available at  
#' \url{https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv}. 
#' It is loaded and then sanitised.
#' @return A data frame of daily COVID cases for Russia by region, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr select mutate %>%
#' @importFrom tidyr pivot_longer last_col
#' @importFrom lubridate mdy
#'
get_russia_regional_cases <- function() {

  # Read data ------------------------------------------------------
  url <- "https://raw.githubusercontent.com/grwlf/COVID-19_plus_Russia/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_RU.csv"

  # Clean data -----------------------------------------------------
  russia <- csv_reader(url) %>%
    tidyr::pivot_longer(cols = 12:tidyr::last_col(), names_to = "date") %>%
    dplyr::select(date, region_level_1 = Province_State, cases_total = value) %>%
    dplyr::mutate(date = lubridate::mdy(date))

  return(russia)

}

