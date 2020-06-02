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

  # Path to data
  url <- "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv"

  # Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  # Read & clean data
  data <- mem_read(file = url, col_types = readr::cols())
  if (data[1,1] == "#adm1+name"){
    data <- data[-1, ]
  }

  data <- data %>%
    # Reformat
    dplyr::transmute(date = lubridate::ymd(Date),
                     region = stringr::str_replace(Province, " Province", ""),
                     cumulative_cases = Cases,
                     cumulative_deaths = Deaths,
                     cumulative_recoveries = Recoveries) %>%
    dplyr::mutate(cumulative_cases = dplyr::recode(cumulative_cases, "–" = NA_character_),
                  cumulative_deaths = dplyr::recode(cumulative_deaths, "–" = NA_character_),
                  cumulative_recoveries = dplyr::recode(cumulative_recoveries, "–" = NA_character_)) %>%
    tidyr::drop_na() %>%
    # Transform (remove commas in numbers)
    dplyr::mutate(cumulative_cases = as.numeric(stringr::str_remove_all(cumulative_cases, ",")),
                  cumulative_deaths = as.numeric(stringr::str_remove_all(cumulative_deaths, ",")),
                  cumulative_recoveries = as.numeric(stringr::str_remove_all(cumulative_recoveries, ",")))

  return(data)
}
