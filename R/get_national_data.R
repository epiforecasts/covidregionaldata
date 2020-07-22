#' Get national-level data for countries globally, sourced from the ECDC or WHO.
#' 
#' @description Gets raw data using the source-specific function. Includes ISO country codes. 
#' Then adds columns which were missing from the raw data (calculating cumulative counts from new dailies and vice versa),
#' cleans and sanitises further. Adds rows and columns of NA values so that data is in a standard format. 
#' 
#' @param country Character A string specifying the country to get data from. Not case or language dependent. Defaults to all countries.
#' @param totals Boolean. If TRUE, returns totalled data per country up to today's date. If FALSE, returns the full dataset stratified by date and country.
#' @param source Character A string specifying the data source: "WHO", or "ECDC". Not case dependent. Defaults to ECDC.
#' @return A tibble with data related to cases, deaths, hospitalisations, recoveries and testing.
#' @importFrom dplyr %>% group_by arrange select ungroup do everything
#' @importFrom tidyr drop_na fill
#' @importFrom countrycode countryname
#' @export
#' @examples
#' 
#' \dontrun{
#'  get_national_data(country = "canada", totals = TRUE, source = "WHO")
#' }
#' 

get_national_data <- function(country = NULL, totals = FALSE, source = "ecdc"){
  
  # Error handling -------------------------------------------------------------------

  if (!(is.logical(totals))){
    stop("The totals variable should be a logical (TRUE/FALSE) variable.")
  }

  if (!(is.character(source))){
    stop("The source variable should be a character variable ('WHO' or 'ECDC').")
  }
  
 source <- tolower(source)
 
# Match country name -----------------------------------------------------
 
 # Set to NULL to return all countries 
 if (missing(country)){
   input_country_name <- NULL

    } else {
      
      # Standardise country name 
      input_country_name <- country
      input_country_name <- countrycode::countryname(input_country_name, destination = "country.name.en")
      
      if(is.na(input_country_name)){
       stop("Country name not recognised. Please enter a character string, with no abbreviation.")
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
     dplyr::filter(country %in% input_country_name)
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
      dplyr::rename("region_level_1" = "country",  "level_1_region_code" = "iso_code") %>%
      totalise_data(include_level_2_regions = FALSE) %>%
      dplyr::select(-recovered_total, -hosp_total, -tested_total) %>%
      dplyr::arrange(-cases_total) %>%
      dplyr::rename("country" = "region_level_1", "iso_code" = "level_1_region_code")
    
    return(tibble::tibble(data))
  }

  # Pad the data set ------------------------------------------------------------------
  data <- data %>%
    dplyr::rename("region_level_1" = "country", "level_1_region_code" = "iso_code") %>%
    tidyr::drop_na(date) %>%
    fill_empty_dates_with_na() %>%
    complete_cumulative_columns() %>%
    dplyr::rename("country" = "region_level_1", "iso_code" = "level_1_region_code")
 

# Return data -------------------------------------------------------------

  if(source == "ecdc"){
    
    data <- data %>%
      dplyr::group_by(country) %>%
      tidyr::fill(population_2019, un_region, .direction = "up") %>%
      dplyr::ungroup()
    
    data <- data %>%
      dplyr::select(date, un_region, country, iso_code, population_2019,
                    cases_new, cases_total,
                    deaths_new, deaths_total,
                    recovered_new, recovered_total,
                    hosp_new, hosp_total,
                    tested_new, tested_total) %>%
      dplyr::arrange(date, country)
    
    return(data)
    
  }
  
  if(source == "who"){
    
    data <- data %>%
      dplyr::group_by(country) %>%
      tidyr::fill(who_region, un_region, .direction = "up") %>%
      dplyr::ungroup()
    
    data <- data %>%
      dplyr::select(date, un_region, who_region, country, iso_code, 
                    cases_new, cases_total,
                    deaths_new, deaths_total,
                    recovered_new, recovered_total,
                    hosp_new, hosp_total,
                    tested_new, tested_total) %>%
      dplyr::arrange(date, country)
    
    return(data)
    
  }
  
}
