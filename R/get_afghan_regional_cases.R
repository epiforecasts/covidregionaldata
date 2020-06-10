#' Afghan Provincial Daily Case Counts
#'
#' @description Data from HDX https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province
#' The cumulative data is stored in a google sheet, which is read as a csv and de-cumulated.
#'
#' @author Flavio Finger @ffinger
#' @return A dataframe of daily Afghan provincial cases and deaths to be further processed by `get_regional_covid_data()`.
#' @importFrom dplyr %>% transmute mutate group_by
#' @importFrom readr read_csv cols
#' @importFrom stringr str_replace str_remove_all
#' @importFrom lubridate dmy
#' @examples
#'
#' ## Code
#' get_afghan_regional_cases()

get_afghan_regional_cases <- function(){

  # Read & clean data
  url <- "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv"
  data <- csv_reader(file = url)
  if (data[1,1] == "#adm1+name"){
    data <- data[-1, ]
  }

  data <- data %>%
    # Reformat
    dplyr::transmute(date = lubridate::ymd(Date),
                     region_level_1 = stringr::str_replace(Province, " Province", ""),
                     cases_total = Cases,
                     deaths_total = Deaths,
                     recoveries_total = Recoveries) %>%
    dplyr::mutate(cases_total = dplyr::recode(cases_total, "–" = NA_character_),
                  deaths_total = dplyr::recode(deaths_total, "–" = NA_character_),
                  recoveries_total = dplyr::recode(recoveries_total, "–" = NA_character_)) %>%
    tidyr::drop_na() %>%
    # Transform (remove commas in numbers)
    dplyr::mutate(cases_total = as.numeric(stringr::str_remove_all(cases_total, ",")),
                  deaths_total = as.numeric(stringr::str_remove_all(deaths_total, ",")),
                  recoveries_total = as.numeric(stringr::str_remove_all(recoveries_total, ",")))

  return(data)
}
