#' UK Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region and nation. 
#' Data source:
#' 
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @param nhsregions Return subnational English regions using NHS region boundaries instead of PHE boundaries. 
#' Also means that subnational English hospital admissions are "first admissions", excluding re-admissions. Defaults to FALSE
#' @importFrom dplyr mutate rename bind_rows group_by summarise %>%
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom lubridate ymd year month
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom utils download.file
#' 
get_uk_regional_cases_only_level_1 <- function(nhsregions = FALSE) {
  
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
                  # Cases and deaths by specimen date and date of death 
                  #   for all nations + regions
                  cases_new = newCasesBySpecimenDate,
                  cases_total = cumCasesBySpecimenDate,
                  deaths_new = newDeaths28DaysByDeathDate,
                  deaths_total = cumDeaths28DaysByDeathDate) %>%
    # Hospitalisations and tested variables are only available for nations (not regions)
    #   sub-national English regions are available in the NHS data below (with arg nhsregions = TRUE)
    dplyr::rename(hosp_new = newAdmissions,
                  hosp_total = cumAdmissions,
                  tested_new = newTestsByPublishDate,
                  tested_total = cumTestsByPublishDate,
                  region_level_1 = areaName,
                  level_1_region_code = areaCode)
# NHS regions -------------------------------------------------------------
  # Separate NHS data is available for "first" admissions, excluding readmissions.
  #   This is available for England + English regions only.
  #   See: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
  #     Section 2, "2. Estimated new hospital cases"
  if(nhsregions){
    message("Arranging data by NHS region. 
Also adding new variable: hosp_new_first_admissions. This is NHS data for first hospital admissions, which excludes readmissions. This is available for England and English regions only.")
    # Download NHS xlsx
    nhs_url <- paste0("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/",
                      lubridate::year(Sys.Date()), "/",
                      ifelse(lubridate::month(Sys.Date())<10, 
                             paste0(0,lubridate::month(Sys.Date())),
                             lubridate::month(Sys.Date())),
                      "/COVID-19-daily-admissions-",
                      gsub("-", "", as.character(Sys.Date()-1)),
                      ".xlsx")
    
    tmp <- paste0(tempdir(), "\\nhs.xlsx")
    
    download.file(nhs_url, destfile = tmp, mode = "wb")

    # Clean NHS data
    adm_new <- suppressMessages(readxl::read_excel(tmp,
                                                   sheet = 1,
                                                   range = readxl::cell_limits(c(28, 2), c(36, NA))) %>%
                                  t())
    colnames(adm_new) <- adm_new[1,]
    adm_new <- adm_new[2:nrow(adm_new),]
    adm_new <- adm_new %>%
      tibble::as_tibble() %>%
      dplyr::mutate(date = seq.Date(from = as.Date("2020-08-01"), by = 1, length.out = nrow(.))) %>%
      tidyr::pivot_longer(-date, names_to = "region_level_1", values_to = "hosp_new_first_admissions") %>%
      dplyr::mutate(region_level_1 = ifelse(region_level_1 == "ENGLAND", "England", region_level_1),
                    hosp_new_first_admissions = as.numeric(hosp_new_first_admissions))
    

# Merge PHE data into NHS regions -----------------------------------------
data_phe_to_nhs <- data %>%
      dplyr::select(-level_1_region_code) %>%
      dplyr::mutate(region_level_1 = ifelse(region_level_1 == "East Midlands" | region_level_1 == "West Midlands",
                                            "Midlands", region_level_1),
                    region_level_1 = ifelse(region_level_1 == "Yorkshire and The Humber" | region_level_1 == "North East",
                                            "North East and Yorkshire", region_level_1)) %>%
      dplyr::group_by(date, region_level_1) %>%
      dplyr::summarise(cases_new = sum(cases_new, na.rm=T),
                       cases_total = sum(cases_total, na.rm=T),
                       deaths_new = sum(deaths_new, na.rm=T),
                       deaths_total = sum(deaths_total, na.rm=T),
                       hosp_new = sum(hosp_new, na.rm=T),
                       hosp_total = sum(hosp_total, na.rm=T),
                       .groups = "drop")
    
    # Merge PHE and NHS data
    data_merged_nhs <- dplyr::left_join(data_phe_to_nhs, adm_new, by = c("region_level_1", "date")) %>%
      # Create a blended variable that uses "all" hospital admissions (includes readmissions) for devolved nations
      #   and "first" hospital admissions for England + English regions
      dplyr::mutate(hosp_new_blend = ifelse(region_level_1 %in% c("Wales", "Scotland", "Northern Ireland"), 
                                        hosp_new, hosp_new_first_admissions),
                    level_1_region_code = NA)
    
    return(data_merged_nhs)
  } else {
    
    message("Returning UK data by ONS region.
Test and hospital admissions data are unavailable for sub-national ONS regions.
To get hospital admissions data, include argument 'nhsregions = TRUE'. This returns data by NHS region.")
  }
  
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
                                       cumCasesByPublishDate),
                  deaths_new = ifelse(stringr::str_detect(areaCode, "^E"), 
                                      newDeaths28DaysByDeathDate,
                                      newDeaths28DaysByPublishDate),
                  deaths_total = ifelse(stringr::str_detect(areaCode, "^E"), 
                                        cumDeaths28DaysByDeathDate,
                                        cumDeaths28DaysByPublishDate)) %>%
    # Hospitalisations and tested variables are consistent across nations
    dplyr::rename(hosp_new = newAdmissions,
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
    # deaths by date of report
    "newDeaths28DaysByPublishDate", "cumDeaths28DaysByPublishDate",
    # deaths by date of death
    "newDeaths28DaysByDeathDate", "cumDeaths28DaysByDeathDate",
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
  
  country_auth <- authority_data %>%
    dplyr::select(level_2_region_code = "LAD17CD", region_level_2 = "LAD17NM", 
                  level_1_region_code = "CTRY17CD", region_level_1 = "CTRY17NM") %>% 
    dplyr::filter(region_level_1 %in% c("Northern Ireland", "Scotland", "Wales")) %>%
    dplyr::distinct() %>%
    tidyr::drop_na(region_level_2)
  
  other_auths <- tibble::tibble(level_2_region_code = c("E06000058", "E06000052", "E09000012"),
                                region_level_2 = c("Bournemouth, Christchurch and Poole", 
                                                   "Cornwall and Isles of Scilly",
                                                   "Hackney and City of London"),
                                level_1_region_code = c(rep("E92000001", 3)),
                                region_level_1 = c("South West", "South West", "London"))
  
  # Join tables ---------------------------------------------------
  authority_lookup_table <- dplyr::bind_rows(unitary_auth, upper_tier_auth, country_auth, other_auths)
  
  authority_lookup_table <- authority_lookup_table %>% 
    dplyr::arrange(desc(level_1_region_code)) %>% 
    dplyr::distinct(level_2_region_code, region_level_2, .keep_all = TRUE)
  
  return(authority_lookup_table)
}
