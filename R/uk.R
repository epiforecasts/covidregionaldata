#' UK Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for the UK, stratified by region and nation. 
#' Data source:
#' 
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @param nhsregions Return subnational English regions using NHS region boundaries instead of PHE boundaries. 
#' Also means that subnational English hospital admissions are "first admissions", excluding re-admissions. Defaults to FALSE
#' @inheritParams get_uk_data
#' @importFrom dplyr mutate rename bind_rows group_by summarise %>%
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom lubridate ymd year month
#' @importFrom readxl read_excel
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom utils download.file
#' 
get_uk_regional_cases_only_level_1 <- function(nhsregions = FALSE, release_date = NULL) {
  
# Get UK data -------------------------------------------------------------
  
  # Set up API query
  query_filters <- list(nation = "areaType=nation",
                        region = "areaType=region")
  
  # Get data for nations and regions
  data_list <- purrr::map(query_filters, get_uk_data,
                          release_date = release_date)

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
  
  if (!is.null(release_date)) {
    data <- dplyr::mutate(data, release_date = release_date)
  }
# NHS regions -------------------------------------------------------------
  # Separate NHS data is available for "first" admissions, excluding readmissions.
  #   This is available for England + English regions only.
  #   See: https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
  #     Section 2, "2. Estimated new hospital cases"
  if(nhsregions){
    if (is.null(release_date)) {
      release_date <- Sys.Date() - 1
    }
    if (release_date < (Sys.Date() - 14)) {
      stop("Data by NHS regions is only available in archived form for the last 14 days")
    }
    message("Arranging data by NHS region. 
Also adding new variable: hosp_new_first_admissions. This is NHS data for first hospital admissions, which excludes readmissions. This is available for England and English regions only.")
    # Download NHS xlsx
    nhs_url <- paste0("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/",
                      lubridate::year(release_date), "/",
                      ifelse(lubridate::month(release_date)<10, 
                             paste0(0,lubridate::month(release_date)),
                             lubridate::month(release_date)),
                      "/COVID-19-daily-admissions-and-beds-",
                      gsub("-", "", as.character(release_date)),
                      ".xlsx")
    
    tmp <- file.path(tempdir(), "nhs.xlsx")
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
      dplyr::summarise(cases_new = sum(cases_new, na.rm=TRUE),
                       cases_total = sum(cases_total, na.rm=TRUE),
                       deaths_new = sum(deaths_new, na.rm=TRUE),
                       deaths_total = sum(deaths_total, na.rm=TRUE),
                       hosp_new = sum(hosp_new, na.rm=TRUE),
                       hosp_total = sum(hosp_total, na.rm=TRUE),
                       .groups = "drop")
    
    # Merge PHE and NHS data
    data_merged_nhs <- dplyr::left_join(data_phe_to_nhs, adm_new, by = c("region_level_1", "date")) %>%
      # Create a blended variable that uses "all" hospital admissions (includes readmissions) for devolved nations
      #   and "first" hospital admissions for England + English regions
      dplyr::mutate(hosp_new_blend = ifelse(region_level_1 %in% c("Wales", "Scotland", "Northern Ireland"), 
                                        hosp_new, hosp_new_first_admissions),
                    level_1_region_code = NA, 
                    release_date = release_date)
    
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
#' @inheritParams get_uk_data
#' @return A data frame of daily COVID cases for the UK by region, to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr bind_rows mutate rename select left_join %>%
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' 
get_uk_regional_cases_with_level_2 <- function(release_date = NULL) {

# Get UK data -------------------------------------------------------------
  
  # Get data for nations and regions
  data <- get_uk_data(filters = list(utla = "areaType=utla"), release_date = release_date)
  
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
  
  if (!is.null(release_date)) {
    data_lv2 <- dplyr::mutate(data_lv2, release_date = release_date)
  }
  return(data_lv2)
}






# Fetch data function ----------------------------------------------------------------


#' Get UK data - helper function to get data for a single valid area type
#' 
#' @description UK data download helper. Data extracted using: 
#' https://coronavirus.data.gov.uk/details/download
#' @param filters Query filters for UK data e.g. "areaType=nation"
#' @param release_date Date data was released. Default is to extract latest release. 
#' Dates should be in the format "yyyy-mm-dd".
#' @return A dataframe with all variables available in public UK data
#' @importFrom dplyr %>%  full_join
#' @importFrom purrr map safely reduce compact
#' 
#' 

get_uk_data <- function(filters, release_date = NULL) {
  api_endpoint <- "https://api.coronavirus.data.gov.uk/v2/data"
  
  uk_variables = list(
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
  # build a list of download links as limited to 4 variables per request
  csv_links <- purrr::map(1:(ceiling(length(uk_variables) / 4)), 
                          ~ paste0(api_endpoint, "?", unlist(filters), "&", 
                                   paste(paste0("metric=",
                                                uk_variables[(1 + 4 * (. - 1)):min((4*.), length(uk_variables))]), 
                                         collapse = "&"),
                                   "&format=csv"))
  # add in release data if defined
  if (!is.null(release_date)) {
    csv_links <- purrr::map(csv_links, ~ paste0(., "&release=", release_date))
  }
  # download and link all data into a single data frame
  safe_reader <- purrr::safely(csv_reader)
  csv <- purrr::map(csv_links, ~ safe_reader(.)[[1]])
  csv <- purrr::compact(csv)
  csv <- purrr::reduce(csv, dplyr::full_join, 
                       by = c("date", "areaType", "areaCode", "areaName"))
  if (is.null(csv)) {
    stop("Data retrieval failed")
  }
  # add release date as variable if missing
  if (!is.null(release_date)) {
    csv <- dplyr::mutate(csv, release_date = as.Date(release_date))
  }
  return(csv)
}


# Join level 1 and 2 codes ------------------------------------------------

#' Lookup table for local authority structure for the UK
#'
#' @description Gets data from \url{https://opendata.arcgis.com/datasets/72e57d3ab1054e169c55afff3c9c1aa4_0.csv}
#' and then uses this to create a table of authorities and their corresponding higher level regions
#' @return A tibble of UK local authorities
#' @importFrom readr read_csv cols col_character
#' @importFrom dplyr select %>% distinct filter bind_rows arrange
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
    dplyr::arrange(level_1_region_code) %>% 
    dplyr::distinct(level_2_region_code, region_level_2, .keep_all = TRUE)
  
  return(authority_lookup_table)
}
