#' French Regional Daily COVID-19 Count Data - Région
#'
#' @description Extracts daily COVID-19 data for France, stratified by région.
#' Data available at \url{https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/}.
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by Région in France, ready to be used by \code{get_regional_data()}.
#'
get_france_regional_cases_only_level_1 <- function() {

  # Read data --------------------------------------------------------------------
  cases_url <- "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"

  cases_data <- csv_reader(file = cases_url) %>%
    dplyr::filter(cl_age90 == 0) %>%
    dplyr::select(date = jour,
                  region_level_1 = reg,
                  cases_new = P,
                  tested_new = `T`) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))
  
  data <- cases_data

  return(data)
}

#' French Regional Daily COVID-19 Count Data - Département
#'
#' @description Extracts daily COVID-19 data for France, stratified by département.
#' Data available at \url{https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/}.
#' It is loaded and then sanitised.
#' @return A data.frame of COVID cases by département in France, ready to be used by get_regional_data().
#' 
get_france_regional_cases_with_level_2 <- function() {

  # Read data --------------------------------------------------------------------
  cases_url <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"
  hosp_url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"

  cases_data <- csv_reader(file = cases_url) %>%
    dplyr::filter(cl_age90 == 0) %>%
    dplyr::select(date = jour,
                  region_level_2 = dep,
                  cases_new = P,
                  tested_new = `T`) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))
  
  hosp_data <- csv_reader(file = hosp_url) %>%
    dplyr::select(date = jour,
                  region_level_2 = dep,
                  hosp_new = incid_hosp,
                  deaths_new = incid_dc) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))
  
  data <- dplyr::full_join(cases_data, hosp_data, by = c("date", "region_level_2"))

  return(data)
}
