#' Get national-level data for countries globally, sourced from the ECDC or WHO.
#' 
#' @description Gets raw data using the source-specific function. Includes ISO country codes. 
#' Then adds columns which were missing from the raw data (calculating cumulative counts from new dailies and vice versa),
#' cleans and sanitises further. Adds rows and columns of NA values so that data is in a standard format. 
#' 
#' @param country Character A string specifying the country to get data from. Not case dependent. Defaults to all countries.
#' Name should be the English name.
#' @param totals Boolean. If TRUE, returns totalled data per country up to today's date. If FALSE, returns the full dataset stratified by date and country.
#' @param source Character A string specifying the data source: "WHO", or "ECDC". Not case dependent. Defaults to ECDC.
#' @return A tibble with data related to cases, deaths, hospitalisations, recoveries and testing.
#' @importFrom dplyr %>% group_by arrange select ungroup do everything
#' @importFrom tidyr drop_na fill
#' @importFrom tibble tibble
#' @export
#' @examples
#' 
#' \dontrun{
#'  get_national_data(country = "canada", totals = TRUE, source = "WHO")
#' }
#' 

get_national_data <- function(country = NULL, totals = FALSE, source = "ecdc"){
  
  # Error handling -------------------------------------------------------------------
  if (!is.null(country) & !(is.character(country))){
    stop("The country variable should be a character variable.")
  }
  
  if (!(is.logical(totals))){
    stop("The totals variable should be a logical (TRUE/FALSE) variable.")
  }

  if (!is.null(source) & !(is.character(source))){
    stop("The source variable should be a character variable ('WHO' or 'ECDC').")
  }
  
  
 source <- tolower(source)
 
# Match country name -----------------------------------------------------
 
  input_country_name <- tolower(country) # Set to a different variable name to avoid mixups with "country" var in data
  if(length(input_country_name) == 0){input_country_name <- NULL} # Reset to NULL to return all countries

  if (!is.null(input_country_name)) {

    name_match <- read.csv(here::here("data-raw", "country_names.csv"), colClasses = "character", na.strings = "")

      if (!input_country_name %in% name_match$name_lower) {
        match1 <- grep(input_country_name, name_match$name_lower)
        match2 <- grep(input_country_name, name_match$name_alternative1)
        match3 <- grep(input_country_name, name_match$name_alternative2)
        if(length(match1) != 0 & length(match1) == 1) {
          input_country_name <- name_match[match1,2]
        } else {
        if(length(match2) != 0 & length(match2) == 1){
          input_country_name <- name_match[match2,2]
          } else {
            if(length(match3) != 0 & length(match3) == 1) {
              input_country_name <- name_match[match3,2]
            } else {
          stop("The country name is not recognised or returns more than one country. Suggest checking against UN standard spelling.")
            }
          }
        }
      }
  }
  
  # Find the correct data-getter and ISO codes ----------------------------------------

    get_data_function <- switch(source,
                                "ecdc" = get_ecdc_cases,
                                "who" = get_who_cases,
                                stop("Unknown source entered"))
    
 
  # Get the data and country ISO codes ------------------------------------
  data <- do.call(get_data_function, list())
 
 # Filter to country -------------------------------------------------------
 
 if (!is.null(input_country_name)){
   
   data <- data %>%
     dplyr::mutate(country_lower = tolower(country)) %>%
     dplyr::filter(country_lower %in% input_country_name) %>%
     dplyr::select(-country_lower)
   
 }  
 
 
  # Group data to country -----------------------------------
    data <- data %>%
      dplyr::group_by(country, iso_code)
  
  # Add columns that aren't there already, clean up data ------------------------------
  data <- data %>%
    dplyr::do(calculate_columns_from_existing_data(.)) %>%
    add_extra_na_cols() %>%
    set_negative_values_to_zero() %>%
    dplyr::ungroup()
  

  # Totalise and return if totals data is requested -------------------------------------------
  if (totals) {
    
    data <- data %>%
      dplyr::rename(region_level_1 = country) %>%
      totalise_data(include_level_2_regions = FALSE) %>%
      dplyr::select(-recovered_total, -hosp_total, -tested_total) %>%
      dplyr::arrange(-cases_total) %>%
      dplyr::rename(country = region_level_1)
    
    return(tibble::tibble(data))
  }

  # Pad the data set ------------------------------------------------------------------
  data <- data %>%
    dplyr::rename("region_level_1" = "country") %>%
    tidyr::drop_na(date) %>%
    fill_empty_dates_with_na() %>%
    complete_cumulative_columns() %>%
    dplyr::rename("country" = "region_level_1")
  
  if(source == "ecdc"){
    
    data <- data %>%
      dplyr::group_by(country) %>%
      tidyr::fill(continent, population_2019, .direction = "up") %>%
      dplyr::ungroup()
    
  }
  
  if(source == "who"){
    
    data <- data %>%
      dplyr::group_by(country) %>%
      tidyr::fill(who_region, .direction = "up") %>%
      dplyr::ungroup()
    
  }
  
  # Arrange and return data -------------------------------------------------------
  
  data <- data %>%
    dplyr::select(date, dplyr::everything(), country, iso_code, cases_new, cases_total, deaths_new, deaths_total,
                  recovered_new, recovered_total, hosp_new, hosp_total,
                  tested_new, tested_total) %>%
    dplyr::arrange(date, country)
  

return(tibble::tibble(data))

}
