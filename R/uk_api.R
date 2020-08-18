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
#' @importFrom dplyr bind_rows mutate rename %>%
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_with_level_2 <- function() {

# Get UK data -------------------------------------------------------------
  
  # Get data for nations and regions
  data_lv2 <- get_uk_data(filters = list(utla = "areaType=utla"), progress_bar = TRUE)
  
  # Reshape for covidregionaldata -------------------------------------------
  data_lv2 <- data_lv2 %>%
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
                  level_2_region_code = areaCode)
  
  return(data_lv2)
  
}






# Fetch data function ----------------------------------------------------------------


#' Get UK data - helper function to get data for a single valid area type
#' 
#' @description UK data download helper. Code adapted from PHE Coronavirus API for R:
#'   available at https://github.com/publichealthengland/coronavirus-dashboard-api-R-sdk
#' @param filters Query filters for UK data e.g. "areaType=nation"
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




