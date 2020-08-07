#' Afghan Regional Daily COVID-19 Count Data
#'
#' @description Fetches daily COVID-19 data for Afghanistan by province.
#' Data from HDX \url{https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province}.
#' The cumulative data is stored in a Google sheet 
#' (\url{https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv}), 
#' which is read as a CSV and sanitised.
#'
#' @author Flavio Finger @ffinger
#' @return A data frame of daily Afghan provincial cases and deaths, stratified by state,
#' to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr %>% transmute mutate recode
#' @importFrom stringr str_replace str_remove_all str_trim
#' @importFrom lubridate dmy
#' @importFrom tidyr drop_na
#' 
get_afghan_regional_cases <- function(){

  # Read & clean data -----------------------------------------------------------------------
  url <- "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv"
  data <- csv_reader(file = url)
  if (data[1,1] == "#adm1+name"){
    data <- data[-1, ]
  }
  
  # Reformat -------------------------------------------------------------------------------
  data <- data %>%
    dplyr::transmute(date = lubridate::ymd(Date),
                     region_level_1 = Province %>% 
                       stringr::str_remove_all("Province") %>% 
                       stringr::str_trim(side = "both"),
                     cases_total = Cases,
                     deaths_total = Deaths,
                     recovered_total = Recoveries) %>%
    dplyr::mutate(cases_total = dplyr::recode(cases_total, "<E2><80><93>" = NA_character_),
                  deaths_total = dplyr::recode(deaths_total, "<E2><80><93>" = NA_character_),
                  recovered_total = dplyr::recode(recovered_total, "<E2><80><93>" = NA_character_)) %>%
    tidyr::drop_na() %>%
  # Transform (remove commas in numbers) ----------------------------------------------------
    dplyr::mutate(cases_total = as.numeric(stringr::str_remove_all(cases_total, ",")),
                  deaths_total = as.numeric(stringr::str_remove_all(deaths_total, ",")),
                  recovered_total = as.numeric(stringr::str_remove_all(recovered_total, ",")))

  return(data)
}
