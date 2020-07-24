#' Canadian Regional Daily COVID-19 Count Data - Provinces
#'
#' @description Extracts daily COVID-19 data for Canada, stratified by province.
#' Data available at  \url{https://health-infobase.canada.ca}. 
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by province in Canada, ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr %>% filter select mutate rename
#' @importFrom tidyr replace_na
#' @importFrom lubridate dmy
#'
get_canada_regional_cases <- function(){

  # Read + clean data --------------------------------------------------------
  url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"

  data <- csv_reader(file = url) %>%
    dplyr::select(pruid, prname, date, numtoday, numtotal, numdeaths, numrecover, numtested) %>%
    dplyr::filter(pruid != 1) %>%
    dplyr::select(-pruid) %>%

  # Transform --------------------------------------------------------------
    dplyr::mutate(prname = gsub("Repatriated travellers", "Repatriated Travellers", prname),
                  date = lubridate::dmy(date),
                  numrecover = as.numeric(numrecover),
                  numdeaths = as.numeric(numdeaths),
                  numtotal = as.numeric(numtotal),
                  numtoday = as.numeric(numtoday),
                  numrecover = as.numeric(numrecover),
                  numtested = as.numeric(numtested)) %>%
    dplyr::rename(region_level_1 = prname, deaths_total = numdeaths, cases_total = numtotal,
                  cases_new = numtoday, recovered_total = numrecover, tested_total = numtested) %>%
    tidyr::replace_na(list(deaths_total = 0, cases_total = 0, recovered_total = 0, tested_total = 0))

  return(data)
}
