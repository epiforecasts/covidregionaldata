#' UK Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region and nation. 
#' Data source:
#' 
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr mutate rename bind_rows %>%
#' @importFrom purrr map
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_only_level_1 <- function() {
  
  query_filters <- list(
    nation = "areaType=nation",
    region = "areaType=region")

  # Get and combine data for Eng regions and UK nations
  data_list <- purrr::map(query_filters, get_uk_data,
                          progress_bar = FALSE)
  
  # Reshape for covidregionaldata
  data <- dplyr::bind_rows(data_list$nation, data_list$region) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::rename("region" = areaName, "ons_code" = areaCode)
  
  return(data)

}


#' UK Regional Daily COVID-19 Count Data - UTLA
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by Upper Tier Local Authority 
#' Data source:
#' 
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr mutate rename %>%
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_with_level_2 <- function() {

  # Get UTLA data
  data <- get_uk_data(filters = list(utla = "areaType=utla"),
                      progress_bar = TRUE)
    
  # Reshape for covidregionaldata
  data <- data %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::rename("authority" = areaName, "utla_code" = areaCode)
  
  return(data)
  
}


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
  
  variables = list(
    date = "date",
    areaName = "areaName",
    areaCode = "areaCode",
    # Cases by date of specimen
    newCasesBySpecimenDate = "newCasesBySpecimenDate",
    cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
    # Cases by date of report
    newCasesByPublishDate = "newCasesByPublishDate",
    cumCasesByPublishDate = "cumCasesByPublishDate",
    # Cases by demographic
    femaleCases = "femaleCases",
    maleCases = "maleCases",
    # Deaths by date of death
    newDeathsByDeathDate = "newDeathsByDeathDate",
    cumDeathsByDeathDate = "cumDeathsByDeathDate",
    # Deaths by date of death
    newDeathsByPublishDate = "newDeathsByPublishDate",
    cumDeathsByPublishDate = "cumDeathsByPublishDate",
    # Deaths by demographic
    femaleDeaths = "femaleDeaths",
    maleDeaths = "maleDeaths",
    # Hospital - admissions
    newAdmissions = "newAdmissions",
    cumAdmissions = "cumAdmissions",
    cumAdmissionsByAge = "cumAdmissionsByAge",
    # Hospital - prevalence
    covidOccupiedMVBeds = "covidOccupiedMVBeds",
    hospitalCases = "hospitalCases",
    # Tests - all
    newTestsByPublishDate = "newTestsByPublishDate",
    cumTestsByPublishDate = "cumTestsByPublishDate",
    # Tests - by UK pillar
    newPillarOneTestsByPublishDate = "newPillarOneTestsByPublishDate",
    newPillarTwoTestsByPublishDate = "newPillarTwoTestsByPublishDate",
    newPillarThreeTestsByPublishDate = "newPillarThreeTestsByPublishDate",
    newPillarFourTestsByPublishDate = "newPillarFourTestsByPublishDate"
    )
  
  results      <- list()
  current_page <- 1
  
  if (progress_bar) {
  pb <- txtProgressBar(min = 0, max = 45, style = 3)
  }
  repeat {
    
  response <- httr::VERB("GET", 
                         url = api_endpoint, 
                         query = list(
                           filters = filters, 
                           structure = jsonlite::toJSON(variables, auto_unbox = TRUE, pretty = FALSE),
                           page = current_page), 
                         httr::timeout(20))
  
  if ( response$status_code >= 400 ) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  } else if ( response$status_code == 204 ) {
    break
  }
  
  # Convert response from binary to JSON:
  json_text <- httr::content(response, "text")
  dt        <- jsonlite::fromJSON(json_text)
  results   <- rbind(results, dt$data)
  
  if ( is.null( dt$pagination$`next` ) ){
    break
  }
  
  current_page <- current_page + 1
  
  if (progress_bar) {
    setTxtProgressBar(pb, current_page)
  }
  
  }
  
  return(results)
}
