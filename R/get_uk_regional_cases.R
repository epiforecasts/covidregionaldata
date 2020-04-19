#' Get UK daily cases
#'
#' @description Get UK regional cases.
#' @param geography Character string identifying the scale at which to extract data. Defaults to "regiona" with "utla" also
#' supported.
#' @return A dataframe of case counts in English and Scottish regions, and Wales and Northern Ireland
#' @export
#' @importFrom dplyr mutate select filter arrange group_by ungroup n lag summarise
#' @importFrom readr read_csv
#' @importFrom tidyr fill
#' @importFrom jsonlite fromJSON
#' @importFrom reshape2 melt
#' @importFrom lubridate ymd
#' @examples
#' get_uk_regional_cases
#'
#' \dontrun{
#' ## Mapping UK regions
#' uk_regions <- get_uk_regional_cases(geography = "regional")
#'
#' # Merge Midlands, and N East and Yorkshire, to match shapefile
#' midlands <- uk_region_data %>%
#'   dplyr::filter(stringr::str_detect(region, "Midlands")) %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise(cases = sum(cases)) %>%
#'   dplyr::mutate(region = "Midlands")
#'
#' ne_yorks <- uk_region_data %>%
#'   dplyr::filter(region %in% c("Yorkshire and The Humber", "North East")) %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise(cases = sum(cases)) %>%
#'   dplyr::mutate(region = "North East and Yorkshire")
#'
#' # Filter to latest date by region
#' uk_regions <- uk_region_data %>%
#'   dplyr::filter(!stringr::str_detect(region, "Midlands")) %>%
#'   dplyr::bind_rows(midlands, ne_yorks) %>%
#'   dplyr::group_by(region) %>%
#'   dplyr::filter(date == max(date)) %>%
#'   dplyr::ungroup()
#'
#' # Bind and map
#' uk_shp <- readRDS("uk_shp.rds")
#' uk_map <- dplyr::left_join(uk_shp, uk_regions, by = c("region" = "region"))
#' uk_map %>%
#'   ggplot2::ggplot(ggplot2::aes(fill = cases)) +
#'   ggplot2::geom_sf()
#' }
#'
#'


get_uk_regional_cases <- function(geography = "regional") {

  if(!geography %in% c("regional", "utla")) {
    stop('Please specify geography: "regional", "utla". Default: "regional".')
  }

  # Path to data
  path_eng <- path_eng <- paste0("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/raw/phe/coronavirus-covid-19-number-of-cases-in-uk-", Sys.Date()-1, ".json")

  path_uk <- "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv"


  uk_regional <- function() {
  # Get cases for England
  # Cleaning: reshape; tidy columns
  eng_js <- jsonlite::fromJSON(path_eng)
  eng_cases <- suppressMessages(reshape2::melt(eng_js$regions)) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  region = value,
                  region = ifelse(L2 == "name", region, NA),
                  value = ifelse(is.na(date), NA, value),
                  cases = as.numeric(value)) %>%
    tidyr::fill(region) %>%
    dplyr::filter(L2 == "dailyConfirmedCases") %>%
    dplyr::select(date, region, cases)

  # Get cases for Wales, Scotland, and Northern Ireland
  # Cleaning: merge Wales & N Ireland regions; reformat to daily; adjust negatives to 0
  uk_cases <- suppressMessages(readr::read_csv(file = path_uk)) %>%
    dplyr::mutate(TotalCases = suppressWarnings(as.numeric(TotalCases)),
                  date = lubridate::ymd(Date))

  wales_cases <- dplyr::filter(uk_cases, Country == "Wales") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(TotalCases = sum(TotalCases, na.rm = TRUE)) %>%
    dplyr::mutate(region = "Wales",
                  index = 1:dplyr::n(),
                  cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                  cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::select(date, region, cases)

  ni_cases <- dplyr::filter(uk_cases, Country == "Northern Ireland") %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(TotalCases = sum(TotalCases, na.rm = TRUE)) %>%
    dplyr::mutate(region = "Northern Ireland",
                  index = 1:dplyr::n(),
                  cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                  cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::select(date, region, cases)

  scotland_cases <- dplyr::filter(uk_cases, Country == "Scotland") %>%
    dplyr::group_by(Area) %>%
    dplyr::mutate(region = Area,
                  index = 1:dplyr::n(),
                  cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                  cases = ifelse(cases < 0 , 0, cases)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, region, cases)

  # Join UK
  uk_cases_join <- dplyr::bind_rows(scotland_cases, ni_cases,
                                    wales_cases, eng_cases)

  ## Remove spurious regions
  uk_cases <- uk_cases_join %>%
    dplyr::filter(!region %in% "Golden Jubilee National Hospital")

  return(uk_cases)
  }

  uk_utla <- function() {
    uk_utla <- suppressMessages(readr::read_csv(file = path_uk)) %>%
    dplyr::mutate(TotalCases = suppressWarnings(as.numeric(TotalCases)),
                  date = lubridate::ymd(Date)) %>%
      dplyr::group_by(Area) %>%
      dplyr::mutate(utla = Area,
                    index = 1:dplyr::n(),
                    cases = TotalCases - ifelse(index == 1, 0, dplyr::lag(TotalCases)),
                    cases = ifelse(cases < 0 , 0, cases)) %>%
      dplyr::ungroup() %>%
      dplyr::select(date, utla, utla_code = AreaCode, cases)
    return(uk_utla)
  }


if (geography == "regional"){
  return(uk_regional())

}else if (geography == "utla"){
  return(uk_utla())
}

}
