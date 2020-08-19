#' UK Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region and nation. 
#' Data source:
#' 
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr mutate rename bind_rows %>%
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_only_level_1 <- function() {
  
# Get UK data -------------------------------------------------------------
  
  # Set up API query
  query_filters <- list(nation = "areaType=nation",
                        region = "areaType=region")
  
  # Get data for nations and regions
  data_list <- purrr::map(query_filters, get_uk_data)

# Reshape for covidregionaldata -------------------------------------------

  # Add or rename standardised variables 
  data <- dplyr::bind_rows(data_list$nation, data_list$region) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  # Cases and deaths are by publish date for Scotland, Wales, NI; 
                  #   but by specimen date and date of death for England
                  cases_new = ifelse(stringr::str_detect(areaCode, "^E"), 
                                      newCasesBySpecimenDate,
                                      newCasesByPublishDate),
                  cases_total = ifelse(stringr::str_detect(areaCode, "^E"), 
                                      cumCasesBySpecimenDate,
                                      cumCasesByPublishDate)) %>%
    # Deaths (28 day), hospitalisations and tested variables are consistent across nations
    dplyr::rename(deaths_new = newDeaths28DaysByPublishDate,
                  deaths_total = cumDeaths28DaysByPublishDate,
                  hosp_new = newAdmissions,
                  hosp_total = cumAdmissions,
                  tested_new = newTestsByPublishDate,
                  tested_total = cumTestsByPublishDate,
                  region_level_1 = areaName,
                  level_1_region_code = areaCode)
  
  return(data)

}


#' UK Regional Daily COVID-19 Count Data - UTLA
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by Upper Tier Local Authority 
#' Data source:
#' 
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr bind_rows mutate rename select left_join %>%
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_with_level_2 <- function() {

# Get UK data -------------------------------------------------------------
  
  # Get data for nations and regions
  data <- get_uk_data(filters = list(utla = "areaType=utla"), progress_bar = TRUE)
  
  # Reshape for covidregionaldata -------------------------------------------
  authority_lookup_table <- get_authority_lookup_table()
  
  data_lv2 <- data %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  # Cases and deaths are by publish date for Scotland, Wales, NI; 
                  #   but by specimen date and date of death for England
                  cases_new = ifelse(stringr::str_detect(areaCode, "^E"), 
                                     newCasesBySpecimenDate,
                                     newCasesByPublishDate),
                  cases_total = ifelse(stringr::str_detect(areaCode, "^E"), 
                                       cumCasesBySpecimenDate,
                                       cumCasesByPublishDate)) %>%
    # Hospitalisations and tested variables are consistent across nations
    dplyr::rename(deaths_new = newDeaths28DaysByPublishDate,
                  deaths_total = cumDeaths28DaysByPublishDate,
                  hosp_new = newAdmissions,
                  hosp_total = cumAdmissions,
                  tested_new = newTestsByPublishDate,
                  tested_total = cumTestsByPublishDate,
                  region_level_2 = areaName,
                  level_2_region_code = areaCode) %>%
  # Join local authority codes to level 1 regions
    dplyr::left_join(authority_lookup_table, by = "region_level_2") %>%
    dplyr::rename(level_2_region_code = level_2_region_code.x) %>%
    dplyr::select(-level_2_region_code.y) %>%
    dplyr::mutate(region_level_1 = ifelse(grepl("^W", level_2_region_code), "Wales",
                                               ifelse(grepl("^S", level_2_region_code), 
                                                      "Scotland", 
                                                      ifelse(grepl("^N", level_2_region_code),
                                                             "Northern Ireland", region_level_1))),
                  level_1_region_code = ifelse(region_level_1 == "Scotland", "S92000003",
                                               ifelse(region_level_1 == "Wales", "W92000004",
                                                      ifelse(region_level_1 == "Northern Ireland", 
                                                             "N92000002", level_1_region_code))))
    
  return(data_lv2)
  
}






# Fetch data function ----------------------------------------------------------------


#' Get UK data - helper function to get data for a single valid area type
#' 
#' @description UK data download helper. Code adapted from PHE Coronavirus API for R:
#'   available at https://github.com/publichealthengland/coronavirus-dashboard-api-R-sdk
#' @param filters Query filters for UK data e.g. "areaType=nation"
#' @param progress_bar Display a progress bar, useful for level 2 which takes a while to load
#' @return A dataframe with all variables available in public UK data
#' @importFrom dplyr %>%  
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr VERB content timeout http_status
#' 

get_uk_data <- function(filters, progress_bar = FALSE) {
  
  api_endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"
  
  uk_variables = list(
    # --- Standard variables for covidregionaldata --- #
    # 
    "date", "areaName", "areaCode",
    # Cases by date of specimen
    "newCasesBySpecimenDate", "cumCasesBySpecimenDate",
    # Cases by date of report
    "newCasesByPublishDate", "cumCasesByPublishDate",
    # deaths
    "newDeaths28DaysByPublishDate", "cumDeaths28DaysByPublishDate",
    # Tests - all
    "newTestsByPublishDate", "cumTestsByPublishDate",
    # Hospital - admissions
    "newAdmissions", "cumAdmissions", 
    #
    # --- Additional non-standard variables --- #
    # Hospital
    "cumAdmissionsByAge", "covidOccupiedMVBeds", 
    "hospitalCases", "plannedCapacityByPublishDate",
    # Tests by pillar
    "newPillarOneTestsByPublishDate", "newPillarTwoTestsByPublishDate", 
    "newPillarThreeTestsByPublishDate", "newPillarFourTestsByPublishDate"
  )
  names(uk_variables) <- uk_variables
  
  results <- list()
  current_page <- 1
  
  if (progress_bar) {
    pb <- txtProgressBar(min = 0, max = 45, style = 3)
  }
  repeat {
    
    response <- httr::VERB("GET", 
                           url = api_endpoint, 
                           query = list(
                             filters = filters, 
                             structure = jsonlite::toJSON(uk_variables, auto_unbox = TRUE, pretty = FALSE),
                             page = current_page), 
                           httr::timeout(20))
    
    if (response$status_code >= 400) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if (response$status_code == 204) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- httr::content(response, "text")
    dt <- jsonlite::fromJSON(json_text)
    results <- rbind(results, dt$data)
    
    if (is.null( dt$pagination$`next`)) {
      break
    }
    
    current_page <- current_page + 1
    
    if (progress_bar) {
      setTxtProgressBar(pb, current_page)
    }
    
  }
  
  return(results)
}


# Join level 1 and 2 codes ------------------------------------------------

#' Lookup table for local authority structure for the UK
#'
#' @description Gets data from \url{https://opendata.arcgis.com/datasets/72e57d3ab1054e169c55afff3c9c1aa4_0.csv}
#' and then uses this to create a table of authorities and their corresponding higher level regions
#' @return A tibble of UK local authorities
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr select %>% distinct filter bind_rows
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble

get_authority_lookup_table <- function() {
  
  # Look-up table for Authority Structures ----------------------------------
  authority_data <- readr::read_csv("https://opendata.arcgis.com/datasets/72e57d3ab1054e169c55afff3c9c1aa4_0.csv",
                                    col_types = readr::cols(WD17NMW = readr::col_character(),
                                                            CTY17CD = readr::col_character(),
                                                            CTY17NM = readr::col_character()))
  
  unitary_auth <- authority_data %>%
    dplyr::select(level_2_region_code = "CTY17CD", region_level_2 = "CTY17NM", 
                  level_1_region_code = "GOR10CD", region_level_1 = "GOR10NM") %>% 
    dplyr::distinct() %>%
    tidyr::drop_na(region_level_2)
  
  upper_tier_auth <- authority_data %>%
    dplyr::select(level_2_region_code = "LAD17CD", region_level_2 = "LAD17NM", 
                  level_1_region_code = "GOR10CD", region_level_1 = "GOR10NM") %>% 
    dplyr::distinct() %>%
    tidyr::drop_na(region_level_2)
  
  ni_auth <- authority_data %>%
    dplyr::select(level_2_region_code = "LAD17CD", region_level_2 = "LAD17NM", 
                  level_1_region_code = "CTRY17CD", region_level_1 = "CTRY17NM") %>% 
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
                                level_1_region_code = c(rep("S92000003", 14), rep("W92000004", 7), rep("E92000001", 2)),
                                region_level_1 = c(rep("Scotland", 14), rep("Wales", 7), rep("South West", 2)))
  
  # Join tables ---------------------------------------------------
  authority_lookup_table <- dplyr::bind_rows(unitary_auth, upper_tier_auth, ni_auth, other_auths)
  
  return(authority_lookup_table)
}


