#' Afghan Provincial Daily Case Counts
#'
#' @description Data from HDX https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province
#' The cumulative data is stored in a google sheet, which is read as a csv and de-cumulated.
#'
#' @author Flavio Finger @ffinger
#' @return A dataframe of daily Afghan provincial cases and deaths to be further processed by \function{get_regional_covid_data()}.
#' @importFrom dplyr %>% transmute mutate group_by
#' @importFrom readr read_csv
#' @importFrom stringr str_replace str_remove_all
#' @examples
#'
#' ## Code
#' get_afghan_regional_cases()

get_afghan_regional_cases <- function(){

  # read in data
  url <- "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv"

  data <- readr::read_csv(url)
  if (data[1,1] == "#adm1+name"){
    data <- data[-1, ]
  }

  data <- data %>%
    #reformat
    dplyr::transmute(date = as.Date(Date),
                     region = stringr::str_replace(Province, " Province", ""),
                     cumulative_cases = Cases,
                     cumulative_deaths = Deaths,
                     cumulative_recoveries = Recoveries) %>%
    #transform (remove commas in numbers)
    dplyr::mutate(cumulative_cases = as.numeric(stringr::str_remove_all(cumulative_cases, ",")),
                  cumulative_deaths = as.numeric(stringr::str_remove_all(cumulative_deaths, ",")),
                  cumulative_recoveries = as.numeric(stringr::str_remove_all(cumulative_recoveries, ","))) %>%
    # get daily cases
    dplyr::group_by(region) %>%
    dplyr::mutate(cases_today = get_daily_from_cumulative(cumulative_cases),
                  deaths_today = get_daily_from_cumulative(cumulative_deaths),
                  recoveries_today = get_daily_from_cumulative(cumulative_recoveries))

  return(data)
}
