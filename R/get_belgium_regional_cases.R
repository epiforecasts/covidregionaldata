#' Fetch daily COVID cases by region for Belgium.
#' @description Fetches daily COVID data from Sciensano, the Belgian Institute for Health.
#' Data is available at https://epistat.wiv-isp.be/covid/
#' selects the relevant columns, sanitises various columns and gets the cumulative counts from daily count columns.
#' @return A data.frame of COVID cases by region in Belgium, ready to be used by get_regional_covid_data()
#' @importFrom readr read_csv locale cols
#' @importFrom dplyr %>% select group_by tally rename full_join mutate
#' @importFrom tidyr replace_na
#' @importFrom lubridate dmy
#'
get_belgium_regional_cases <- function(){

  c_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
  h_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  m_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"

  cases_data <- readr::read_csv(c_provincial, locale=readr::locale(encoding = "latin1"), col_types=readr::cols())
  hosp_data <- readr::read_csv(h_provincial, locale=readr::locale(encoding = "latin1"), col_types=readr::cols())
  deaths_data <- readr::read_csv(m_provincial, locale=readr::locale(encoding = "latin1"), col_types=readr::cols())


  cases_data <- cases_data %>%
    dplyr::select(DATE, REGION, CASES) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown")) %>%
    dplyr::group_by(DATE, REGION) %>%
    dplyr::tally(CASES)

  hosp_data <- hosp_data %>%
    dplyr::select(DATE, REGION, NEW_IN) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown")) %>%
    dplyr::group_by(DATE, REGION) %>%
    dplyr::tally(wt = NEW_IN)

  deaths_data <- deaths_data %>%
    dplyr::select(DATE, REGION, DEATHS) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown")) %>%
    dplyr::group_by(DATE, REGION) %>%
    dplyr::tally(wt = DEATHS)


  cases_and_hosp_data <- dplyr::full_join(cases_data, hosp_data, by = c("DATE" = "DATE", "REGION" = "REGION"))
  data <- dplyr::full_join(cases_and_hosp_data, deaths_data, by = c("DATE" = "DATE", "REGION" = "REGION"))

  data <- data %>%
    dplyr::rename(date = DATE, region = REGION, cases_today = n.x, hospitalisations_today = n.y, deaths_today = n) %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(cumulative_cases = get_cumulative_from_daily(cases_today),
                  cumulative_hospitalisations = get_cumulative_from_daily(hospitalisations_today),
                  cumulative_deaths = get_cumulative_from_daily(deaths_today))

  return(data)
}

