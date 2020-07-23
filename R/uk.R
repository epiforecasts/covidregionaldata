#' UK Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region. 
#' Data for England available at  \url{https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv}. 
#' Data for Wales, Scotland and Northern Ireland available at \url{https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv}.
#' It is loaded and then sanitised.
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr mutate select filter arrange group_by ungroup distinct bind_rows left_join arrange %>%
#' @importFrom tidyr replace_na
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_only_level_1 <- function() {
  
  authority_lookup_table <- get_authority_lookup_table() %>% 
    dplyr::select(iso_code, region_level_1) %>% 
    dplyr::distinct()

  # England ----------------------------------------------------------------
  url_eng <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
  eng_regional_data <- csv_reader(url_eng) %>%
    dplyr::filter(`Area type` == "Region") %>%
    dplyr::mutate(date = lubridate::ymd(`Specimen date`)) %>%
    dplyr::select(date, region_level_1 = "Area name", iso_code = "Area code", cases_new = "Daily lab-confirmed cases") %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(region_level_1, iso_code) %>%
    dplyr::mutate(cases_total = get_cumulative_from_daily(cases_new)) %>%
    dplyr::ungroup()

  # Wales, NI & Scotland --------------------------------------------------------
  url_wales_scot_ni <- "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv"
  wales_scot_ni_data <- csv_reader(url_wales_scot_ni) %>%
    dplyr::filter(Country %in% c("Wales", "Scotland", "Northern Ireland")) %>%
    dplyr::distinct() %>%
    tidyr::replace_na(list(TotalCases = 0)) %>%
    dplyr::group_by(Date, Country) %>%
    dplyr::summarise(cases_total = sum(TotalCases), .groups = "drop_last") %>%
    dplyr::mutate(date = lubridate::ymd(Date)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(cases_new = get_daily_from_cumulative(cases_total)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, region_level_1 = "Country", cases_new, cases_total)  %>% 
    dplyr::left_join(authority_lookup_table, by = "region_level_1")
  
  # Return specified dataset ----------------------------------------------------------
  data <- dplyr::bind_rows(eng_regional_data, wales_scot_ni_data) %>%
    dplyr::arrange(date)
  
  return(data)
}

#' UK Regional Daily COVID-19 Count Data - Authority
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region. 
#' Data for England available at  \url{https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv}. 
#' Data for Wales, Scotland and Northern Ireland available at \url{https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv}.
#' It is loaded and then sanitised.
#' @return A data frame of daily COVID cases for the UK by local authority, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr mutate select filter arrange group_by ungroup distinct bind_rows left_join arrange %>%
#' @importFrom tidyr replace_na
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_with_level_2 <- function() {

  authority_lookup_table <- get_authority_lookup_table()
  
  # England ----------------------------------------------------------------
  url_eng <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
  eng_regional_data <- csv_reader(url_eng) %>%
    dplyr::filter(`Area type` == "Upper tier local authority") %>%
    dplyr::mutate(date = lubridate::ymd(`Specimen date`)) %>%
    dplyr::select(date, region_level_2 = "Area name", cases_new = "Daily lab-confirmed cases") %>%
    dplyr::mutate(cases_total = get_cumulative_from_daily(cases_new)) %>% 
    dplyr::left_join(authority_lookup_table, by = "region_level_2") 

  # Wales, NI & Scotland ----------------------------------------------------
  url_wales_scot_ni <- "https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv"
  wales_scot_ni_data <- csv_reader(url_wales_scot_ni) %>%
    dplyr::filter(Country %in% c("Wales", "Scotland", "Northern Ireland")) %>%
    tidyr::replace_na(list(TotalCases = 0)) %>%
    dplyr::group_by(Date, Area, Country) %>%
    dplyr::summarise(cases_total = sum(TotalCases), .groups = "drop_last") %>%
    dplyr::mutate(date = lubridate::ymd(Date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cases_new = get_cumulative_from_daily(cases_total),
                  Area = dplyr::recode(Area, 
                                       "North Down and Ards" = "Ards and North Down",
                                       "Armagh, Banbridge and Craigavon" = "Armagh City, Banbridge and Craigavon",
                                       "Derry and Strabane" = "Derry City and Strabane")) %>%
    dplyr::select(date, region_level_2 = "Area", region_level_1 = "Country", cases_new, cases_total) %>% 
    dplyr::left_join(authority_lookup_table, by = c("region_level_2", "region_level_1"))
  
  # Return specified dataset ----------------------------------------------------------
  data <- dplyr::bind_rows(eng_regional_data, wales_scot_ni_data) %>%
    dplyr::arrange(date)
  
  return(data)
}

#' Lookup table for local authority structure for the UK
#'
#' @description Gets data from \url{https://opendata.arcgis.com/datasets/72e57d3ab1054e169c55afff3c9c1aa4_0.csv}
#' and then uses this to create a table of authorities and their corresponding higher level regions
#' @return A tibble of UK local authorities
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr select %>% distinct filter bind_rows
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
#' 
get_authority_lookup_table <- function() {
  
  # Look-up table for Authority Structures ----------------------------------
  authority_data <- readr::read_csv("https://opendata.arcgis.com/datasets/72e57d3ab1054e169c55afff3c9c1aa4_0.csv",
                                    col_types = readr::cols(WD17NMW = readr::col_character(),
                                                            CTY17CD = readr::col_character(),
                                                            CTY17NM = readr::col_character()))
  
  unitary_auth <- authority_data %>%
    dplyr::select(level_2_region_code = "CTY17CD", region_level_2 = "CTY17NM", 
                  iso_code = "GOR10CD", region_level_1 = "GOR10NM") %>% 
    dplyr::distinct() %>%
    tidyr::drop_na(region_level_2)
  
  upper_tier_auth <- authority_data %>%
    dplyr::select(level_2_region_code = "LAD17CD", region_level_2 = "LAD17NM", 
                  iso_code = "GOR10CD", region_level_1 = "GOR10NM") %>% 
    dplyr::distinct() %>%
    tidyr::drop_na(region_level_2)
  
  ni_auth <- authority_data %>%
    dplyr::select(level_2_region_code = "LAD17CD", region_level_2 = "LAD17NM", 
                  iso_code = "CTRY17CD", region_level_1 = "CTRY17NM") %>% 
    dplyr::filter(region_level_1 == "Northern Ireland") %>%
    dplyr::distinct() %>%
    tidyr::drop_na(region_level_2)
  
  other_auths <- tibble::tibble(level_2_region_code = c("S0800015", "S0800016", "S0800017", "S0800029", "S0800019", "S0800020",
                                                        "S0800031", "S0800022", "S0800032", "S0800024", "S0800025", "S0800026",
                                                        "S0800030", "S0800028", "W11000028", "W11000023", "W11000029",
                                                        "W11000030", "W11000025", "W11000024", "W11000031", "E06000058", "E06000053"),
                                region_level_2 = c("Ayrshire and Arran", "Borders", "Dumfries and Galloway", "Fife",
                                                   "Forth Valley", "Grampian", "Greater Glasgow and Clyde", "Highland",
                                                   "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside", "Western Isles",
                                                   "Aneurin Bevan", "Betsi Cadwaladr", "Cardiff and Vale", "Cwm Taf",
                                                   "Hywel Dda", "Powys", "Swansea Bay", "Bournemouth, Christchurch and Poole",
                                                   "Cornwall and Isles of Scilly"),
                                iso_code = c(rep("S92000003", 14), rep("W92000004", 7), rep("E92000001", 2)),
                                region_level_1 = c(rep("Scotland", 14), rep("Wales", 7), rep("South West", 2)))
  
  # Join tables ---------------------------------------------------
  authority_lookup_table <- dplyr::bind_rows(unitary_auth, upper_tier_auth, ni_auth, other_auths)
  
  return(authority_lookup_table)
}

