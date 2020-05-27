#' Helper function to fetch daily COVID cases by province for Canada and do dataset-specific processing before passing to get_regional_covid_data()
#' @description Fetches daily COVID cases, and deaths by province collated by Provincial Canadian Health Authorities
#' Data is available at https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html
#' selects the relevant columns, sanitises various columns and gets the daily counts from cumulative columns.
#' @return A data.frame of COVID cases by province in Canada, ready to be used by get_regional_covid_data()
#' @importFrom dplyr %>% filter select mutate rename group_by
#' @importFrom tidyr replace_na
#' @importFrom readr read_csv cols
#' @importFrom lubridate dmy

get_canada_regional_cases <- function(){

  # Path to data
  url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"

  # Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  data <- mem_read(file = url, col_types = readr::cols()) %>%
    dplyr::select(pruid, prname, date, numtoday, numtotal, numdeaths, numrecover, numtested) %>%
    dplyr::filter(pruid != 1) %>%
    dplyr::select(-pruid) %>%

    # Transform
    dplyr::mutate(prname = gsub("Repatriated travellers", "Repatriated Travellers", prname),
                  date = lubridate::dmy(date),
                  numrecover = as.numeric(replace(numrecover, numrecover == "N/A", NA))) %>%
    dplyr::rename(region = prname, cumulative_deaths = numdeaths, cumulative_cases = numtotal,
                  cases_today = numtoday, cumulative_recoveries = numrecover, cumulative_tests = numtested) %>%
    tidyr::replace_na(list(cumulative_deaths = 0, cumulative_cases = 0, cumulative_recoveries = 0, cumulative_tests = 0))

  return(data)
}

