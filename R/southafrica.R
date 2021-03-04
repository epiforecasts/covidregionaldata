#' South Africa Regional Daily COVID-19 Count Data
#'
#' @description Fetches daily COVID-19 data
#' Data is available at \url{https://github.com/dsfsi/covid19za}.
#' @return A data frame of COVID cases by province in South Africa ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr bind_rows mutate left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tribble
#' @importFrom lubridate dmy
#'
get_southafrica_regional_cases_only_level_1 <- function() {

  # Get raw data
  cases_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"
  deaths_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv"

  cases <- csv_reader(cases_url)
  deaths <- csv_reader(deaths_url)

  # Set region names
  region_names <- tibble::tribble(
    ~level_1_region_code, ~region_level_1,
    "ZA-EC", "Eastern Cape",
    "ZA-FS", "Free State",
    "ZA-GP", "Gauteng",
    "ZA-KZN", "Kwazulu-Natal",
    "ZA-LP", "Limpopo",
    "ZA-MP", "Mpumalanga",
    "ZA-NC", "Northern Cape",
    "ZA-NW", "North-West",
    "ZA-WC", "Western Cape"
  )

  # Combine data and clean
  data <- dplyr::bind_rows(cases, deaths, .id = "data") %>%
    dplyr::mutate(
      data = factor(data, c(1, 2), c("cases_total", "deaths_total")),
      date = lubridate::dmy(date)
    ) %>%
    dplyr::select(-c(YYYYMMDD, total, source)) %>%
    tidyr::pivot_longer(-c(data, date), names_to = "level_1_region_code") %>%
    tidyr::pivot_wider(names_from = data) %>%
    dplyr::mutate(
      level_1_region_code = paste0("ZA-", level_1_region_code),
      level_1_region_code = dplyr::na_if(level_1_region_code, "ZA-UNKNOWN")
    ) %>%
    dplyr::left_join(region_names, by = "level_1_region_code")

  return(data)
}
