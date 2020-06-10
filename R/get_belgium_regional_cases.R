#' Fetch daily COVID cases with only Admin Level 1 region (region) for Belgium
#' @description Fetches daily COVID data from Sciensano, the Belgian Institute for Health.
#' Data is available at https://epistat.wiv-isp.be/covid/
#' selects and sanitises the relevant columns
#' @return A data.frame of COVID cases by admin level 1 region in Belgium, ready to be used by get_regional_covid_data()
#' @importFrom readr read_csv locale cols
#' @importFrom dplyr %>% select group_by tally rename full_join mutate
#' @importFrom tidyr replace_na
#' @importFrom lubridate dmy
#'
get_belgium_regional_cases_only_level_1 <- function(){

  # Paths to data
  c_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
  h_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  m_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"

  cases_data <- csv_reader(file = c_provincial, locale=readr::locale(encoding = "UTF-8"))
  hosp_data <- csv_reader(file = h_provincial, locale=readr::locale(encoding = "UTF-8"))
  deaths_data <- csv_reader(file = m_provincial, locale=readr::locale(encoding = "UTF-8"))

  # Clean data
  cases_data <- cases_data %>%
    dplyr::select(DATE, REGION, CASES) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown")) %>%
    dplyr::group_by(DATE, REGION) %>%
    dplyr::tally(CASES) %>%
    dplyr::ungroup()

  hosp_data <- hosp_data %>%
    dplyr::select(DATE, REGION, NEW_IN) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown")) %>%
    dplyr::group_by(DATE, REGION) %>%
    dplyr::tally(wt = NEW_IN) %>%
    dplyr::ungroup()

  deaths_data <- deaths_data %>%
    dplyr::select(DATE, REGION, DEATHS) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown")) %>%
    dplyr::group_by(DATE, REGION) %>%
    dplyr::tally(wt = DEATHS) %>%
    dplyr::ungroup()


  # Join the three datasets and rename columns
  cases_and_hosp_data <- dplyr::full_join(cases_data, hosp_data, by = c("DATE" = "DATE", "REGION" = "REGION"))
  data <- dplyr::full_join(cases_and_hosp_data, deaths_data, by = c("DATE" = "DATE", "REGION" = "REGION")) %>%
          dplyr::rename(date = DATE, region_level_1 = REGION, cases_new = n.x, hospitalisations_new = n.y, deaths_new = n)

  return(data)
}


#' Fetch daily COVID cases including Admin Level 2 region (Province) for Belgium
#' @description Fetches daily COVID data from Sciensano, the Belgian Institute for Health.
#' Data is available at https://epistat.wiv-isp.be/covid/
#' selects and sanitises the relevant columns. Also includes Level 1 region
#' @return A data.frame of COVID cases by admin level 2 region in Belgium, ready to be used by get_regional_covid_data()
#' @importFrom readr read_csv locale cols
#' @importFrom dplyr %>% select group_by tally rename full_join mutate
#' @importFrom tidyr replace_na
#' @importFrom lubridate dmy
#'
get_belgium_regional_cases_with_level_2 <- function(){

  # Paths to data
  c_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv"
  h_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
  m_provincial <- "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"

  cases_data <- csv_reader(file = c_provincial, locale=readr::locale(encoding = "UTF-8"))
  hosp_data <- csv_reader(file = h_provincial, locale=readr::locale(encoding = "UTF-8"))
  deaths_data <- csv_reader(file = m_provincial, locale=readr::locale(encoding = "UTF-8"))

  # Clean data
  cases_data <- cases_data %>%
    dplyr::select(DATE, REGION, PROVINCE, CASES) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown",
                           PROVINCE = "Unknown")) %>%
    dplyr::group_by(DATE, PROVINCE, REGION) %>%
    dplyr::tally(CASES) %>%
    dplyr::ungroup()

  hosp_data <- hosp_data %>%
    dplyr::select(DATE, REGION, PROVINCE, NEW_IN) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE)) %>%
    tidyr::replace_na(list(REGION = "Unknown",
                           PROVINCE = "Unknown")) %>%
    dplyr::group_by(DATE, PROVINCE, REGION) %>%
    dplyr::tally(wt = NEW_IN) %>%
    dplyr::ungroup()

  # Join the three datasets and rename columns
  data <- dplyr::full_join(cases_data, hosp_data, by = c("DATE" = "DATE", "PROVINCE" = "PROVINCE", "REGION" = "REGION")) %>%
    dplyr::rename(date = DATE, region_level_1 = REGION, region_level_2 = PROVINCE, cases_new = n.x, hospitalisations_new = n.y)

  return(data)
}
