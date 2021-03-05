#' Filter JHU data for target country
#' @description Downloads daily covid data from JHU (via get_jhu_data)
#' and subsets it for a given target country
#' @param target_country A string with the country of interest
#' @return A data frame of COVID cases for the target country
#' @importFrom dplyr %>% filter select
#' @importFrom rlang .data
#'
check_alternate_data_source <- function(target_country) {
  warning(paste0(
    "'",
    target_country,
    "' has no direct data source: using JHU for data"
  ))
  # make first letter uppercase so it can find resource
  target_country_ <- paste0(
    toupper(substr(target_country, 1, 1)),
    substr(target_country, 2, nchar(target_country))
  )

  # check if country is in JHU locations with subnational data
  jhu_admin1 <- csv_reader("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv") %>%
    dplyr::filter(!is.na(.data$Province_State)) %>%
    dplyr::pull(.data$Country_Region) %>%
    unique()

  # return error if no subnational data present
  if (!target_country_ %in% jhu_admin1) {
    stop(paste0(
      "No public data available for '",
      target_country,
      "'. Get national data with get_national_data()"
    ))
  }

  # get full data and filter for country
  data <- get_jhu_data()
  data <- data %>% dplyr::filter(.data$country == target_country_)
  data <- data %>% dplyr::select(-c(.data$country))

  return(data)
}

#' Download and clean data from John Hopkins Univetsity (JHU)
#' @description Downloads daily covid data from JHU
#' (https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data)
#' and formats the data
#' @return A data frame of COVID cases for all countries listed by JHU,
#' by country and region if available
#' @importFrom dplyr %>% select group_by rename mutate ungroup arrange
#' @importFrom tidyr %>% pivot_longer replace_na
#' @importFrom rlang .data
#'
get_jhu_data <- function() {
  # Paths
  main_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
  vals <- c(
    "confirmed",
    "deaths",
    "recovered"
  )

  # load data
  paths <- paste0(main_path, "time_series_covid19_", vals, "_global.csv")
  data_list <- purrr::map(paths, csv_reader)
  names(data_list) <- paste0("daily_", vals)
  data_list <- lapply(data_list, tidyr::pivot_longer, cols = 5:dplyr::last_col(), names_to = "Date", values_to = "value")

  # merge and clean data
  data <- dplyr::bind_rows(data_list, .id = "variable")
  data <- tidyr::pivot_wider(data, names_from = variable, values_from = value)
  data <- data %>%
    dplyr::select(
      .data$Date,
      .data$`Province/State`,
      .data$`Country/Region`,
      .data$daily_confirmed,
      .data$daily_deaths,
      .data$daily_recovered
    ) %>%
    dplyr::mutate(
      Date = lubridate::mdy(.data$Date),
      daily_confirmed = as.numeric(.data$daily_confirmed),
      daily_deaths = as.numeric(.data$daily_deaths),
      daily_recovered = as.numeric(.data$daily_recovered)
    ) %>%
    dplyr::rename(
      date = .data$Date,
      region_level_1 = .data$`Province/State`,
      country = .data$`Country/Region`,
      cases_total = .data$daily_confirmed,
      deaths_total = .data$daily_deaths,
      recovered_total = .data$daily_recovered
    ) %>%
    tidyr::replace_na(list(
      region_level_1 = "Unknown",
      country = "Unknown"
    ))

  # insert place holder level_1_region_code
  data$level_1_region_code <- "Unknown"

  # return data
  return(data)
}
