#' Get UK daily cases
#'
#' @description Get UK regional cases.
#' @param geography Character string identifying the scale at which to extract data. Defaults to "regiona" with "utla" also
#' supported.
#' @return A dataframe of case counts in English and Scottish regions, and Wales and Northern Ireland
#' @export
#' @importFrom dplyr mutate select filter arrange group_by ungroup n lag summarise recode bind_rows
#' @importFrom readr read_csv
#' @importFrom tidyr fill
#' @importFrom lubridate ymd
#' @examples
#' get_uk_regional_cases
#'
#' \dontrun{
#' ## Mapping UK countries with English regions
#' uk_countries <- get_uk_regional_cases(geography = "all countries") %>%
#'   dplyr::filter(date == max(date))
#' uk_map <- sf::st_read("data-raw/uk_NUTS_areas/NUTS_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_the_United_Kingdom.shp") %>%
#'   dplyr::mutate(nuts118nm = stringr::str_remove_all(nuts118nm, "\\s\\(England\\)")) %>%
#'   dplyr::left_join(uk_countries, by = c("nuts118nm" = "region"))
#' uk_map %>%
#'   ggplot2::ggplot(ggplot2::aes(fill = cases)) +
#'   ggplot2::geom_sf()
#'
#' }
#'
#'


get_uk_regional_cases <- function(geography = "all countries") {

  if(!geography %in% c("all countries", "all regions", "England", "Scotland", "Northern Ireland", "Wales")) {
    stop('Please specify geography: "all countries", "all regions", "England", "Scotland", "Northern Ireland", "Wales". Default: "all countries".')
  }

# England ----------------------------------------------------------------
  path_eng <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
  eng_cases <- suppressMessages(readr::read_csv(file = path_eng)) %>%
    dplyr::filter(`Area type` == "Region") %>%
    dplyr::mutate(date = lubridate::ymd(`Specimen date`)) %>%
    dplyr::select(date, region = "Area name", cases = "Daily lab-confirmed cases") %>%
    dplyr::arrange(date)

# Wales & Scotland --------------------------------------------------------
# Data source
  path_wales_scot <- "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv"
  wales_scot_cases <- suppressMessages(readr::read_csv(file = path_wales_scot)) %>%
    dplyr::mutate(TotalCases = suppressWarnings(as.numeric(TotalCases)),
                  date = lubridate::ymd(Date)) %>%
    dplyr::filter(!Area %in% "Golden Jubilee National Hospital")

  # Wales country cases
  wales_cases_country <- dplyr::filter(wales_scot_cases, Country == "Wales") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(TotalCases = sum(TotalCases, na.rm = TRUE)) %>%
    dplyr::mutate(region = "Wales",
                  index = 1:dplyr::n(),
                  cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                  cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::select(date, region, cases)

# Wales regional cases
  wales_cases_regional <- dplyr::filter(wales_scot_cases, Country == "Wales") %>%
    dplyr::mutate(region = Area,
                  index = 1:dplyr::n(),
                  cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                  cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::select(date, region, cases)

# Scotland country cases
  scotland_cases_country <- dplyr::filter(wales_scot_cases, Country == "Scotland") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(TotalCases = sum(TotalCases, na.rm = TRUE)) %>%
    dplyr::mutate(region = "Scotland",
                  index = 1:dplyr::n(),
                  cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                  cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::select(date, region, cases)

# Scotland regional cases
  scotland_cases_regional <- dplyr::filter(wales_scot_cases, Country == "Scotland") %>%
    dplyr::mutate(region = Area,
                  index = 1:dplyr::n(),
                  cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                  cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::select(date, region, cases)


# Northern Ireland --------------------------------------------------------
# Note: only available at country level
  path_ni <- "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-totals-northern-ireland.csv"
  ni_cases <- suppressMessages(readr::read_csv(file = path_ni)) %>%
    dplyr::mutate(index = 1:dplyr::n(),
                  cases = ConfirmedCases - ifelse(index == 1, 0, dplyr::lag(ConfirmedCases)),
                  cases = ifelse(cases < 0 , 0, cases),
                  date = lubridate::ymd(Date),
                  region = "Northern Ireland") %>%
    dplyr::select(date, region, cases)


# Return specified dataset ----------------------------------------------------------

if (geography == "England"){
  return(eng_cases)
}else if (geography == "Wales"){
  return(wales_cases_regional)
}else if (geography == "Scotland"){
  return(scotland_cases_regional)
}else if (geography == "Northern Ireland"){
  return(ni_cases)

}else if (geography == "all countries"){
  country_join <- dplyr::bind_rows(scotland_cases_country, ni_cases, wales_cases_country, eng_cases, .id = "country") %>%
    dplyr::mutate(country = dplyr::recode(country, "1" = "Scotland", "2" = "Northern Ireland", "3" = "Wales", "4" = "England"))
  return(country_join)
}else if (geography == "all regions"){
  regional_join <- dplyr::bind_rows(scotland_cases_regional, ni_cases, wales_cases_regional, eng_cases, .id = "country") %>%
    dplyr::mutate(country = dplyr::recode(country, "1" = "Scotland", "2" = "Northern Ireland", "3" = "Wales", "4" = "England"))
  return(regional_join)
}

}
