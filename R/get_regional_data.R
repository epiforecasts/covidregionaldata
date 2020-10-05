#' The main calculation function for covidregionaldata. The majority of the work is done in this function.
#' 
#' @description Gets raw data using the country-specific function. Adds the region codes. 
#' Then adds columns which were missing from the raw data (calculating cumulative counts from new dailies and vice versa),
#' cleans and sanitises further. Adds rows and columns of NA values so that data is in a standard format. 
#' 
#' @param country Character A string specifying the country to get data from. Not case dependent. 
#' Name should be the English name. For a list of options see the README.
#' @param totals Boolean. If TRUE, returns totalled data per region up to today's date. If FALSE, returns the full dataset stratified by date and region.
#' @param include_level_2_regions Boolean. If TRUE, returns data stratified by level 2 regions. If FALSE, stratified by Level 1.
#' Note that Level 2 region data Sis not always available. In these cases the user will get a warning and the Level 1 data will be returned.
#' @param localise_regions Logical, defaults to TRUE. Should region names be localised.
#' @param ... pass additional arguments to regional function calls 
#' @return A tibble with data related to cases, deaths, hospitalisations, recoveries and testing stratified by regions within the given country.
#' @importFrom dplyr %>% group_by arrange select ungroup do mutate everything
#' @importFrom stringr str_trim
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
#' @export
#' @examples
#' 
#' \dontrun{
#'  get_regional_data(country = "canada", totals = TRUE, include_level_2_regions = FALSE)
#' }
#' 
get_regional_data <- function(country, totals = FALSE, include_level_2_regions = FALSE,
                              localise_regions = TRUE,
                              ...){

  # Error handling -------------------------------------------------------------------
  if (!(is.character(country))){
    stop("The country variable should be a character variable.")
  }
 
  if (!(is.logical(totals))){
    stop("The totals variable should be a logical (TRUE/FALSE) variable.")
  }

  if (!(is.logical(include_level_2_regions))){
    stop("The include_level_2_regions variable should be a logical (TRUE/FALSE) variable.")
  }
 
  country <- tolower(country)
  countries_with_level_2_regions <- c("belgium",
                                      "brazil",
                                      "germany",
                                      "usa",
                                      "uk")

  if (include_level_2_regions & !(country %in% countries_with_level_2_regions)) {
    warning("The data for that country doesn't have data at Admin Level 2. Returning data for Admin Level 1 only.")
    include_level_2_regions <- FALSE
  }

  # Find the correct data-getter and region codes ----------------------------------------
  if (include_level_2_regions) {

    get_data_function <- switch(country,
                                "belgium" = get_belgium_regional_cases_with_level_2,
                                "brazil" = get_brazil_regional_cases_with_level_2,
                                "germany" = get_germany_regional_cases_with_level_2,
                                "uk" = get_uk_regional_cases_with_level_2,
                                "usa" = get_us_regional_cases_with_level_2,
                                stop("There is no data for the country entered. It is likely we haven't added data
                                   for that country yet, or there was a spelling mistake."))
    
    region_codes_table <- get_region_codes(country)
    region_level_2_codes_table <- get_level_2_region_codes(country)
    
  } else {

    get_data_function <- switch(country,
                                "afghanistan" = get_afghan_regional_cases,
                                "belgium" = get_belgium_regional_cases_only_level_1,
                                "brazil" = get_brazil_regional_cases_only_level_1,
                                "canada" = get_canada_regional_cases,
                                "colombia" = get_colombia_regional_cases,
                                "germany" = get_germany_regional_cases_only_level_1,
                                "india" = get_india_regional_cases,
                                "italy" = get_italy_regional_cases,
                                "russia" = get_russia_regional_cases,
                                "uk" = get_uk_regional_cases_only_level_1,
                                "usa" = get_us_regional_cases_only_level_1,
                                stop("There is no data for the country entered. It is likely we haven't added data
                                   for that country yet, or there was a spelling mistake."))
    
    region_codes_table <- get_region_codes(country)

  }
  
  # Get the data and region codes for level 1 regions ------------------------------------
  data <- do.call(get_data_function, list(...))
  data <- dplyr::mutate(data, region_level_1 = stringr::str_trim(region_level_1, side = "both"))
  data <- data %>% left_join_region_codes(region_codes_table, 
                                       by = c("region_level_1" = "region")) 
  
  # And add level 2 if needed ---------------------------------------------------------
  if (include_level_2_regions) {
    data <- dplyr::mutate(data, region_level_2 = stringr::str_trim(region_level_2, side = "both"))
    data <- data %>% 
      left_join_region_codes(region_level_2_codes_table,
                                         by = c("region_level_2" = "region")) 
  }

  # Group data, dependent on region levels required -----------------------------------
  if (include_level_2_regions) {
    data <- data %>%
      dplyr::group_by(region_level_1, level_1_region_code, region_level_2, level_2_region_code)
  } else {
    data <- data %>%
      dplyr::group_by(region_level_1, level_1_region_code)
  }

  # Add columns that aren't there already, clean up data ------------------------------
  data <- data %>%
    dplyr::do(calculate_columns_from_existing_data(.)) %>%
    add_extra_na_cols() %>%
    set_negative_values_to_zero() %>%
    dplyr::ungroup()

  # Totalise and return if totals data is requested ----------------------------------
  if (totals) {
    data <- totalise_data(data, include_level_2_regions = include_level_2_regions) %>%
      dplyr::arrange(-cases_total)
  }else{
    # Pad the data set ------------------------------------------------------------------
    data <- data %>%
      tidyr::drop_na(date) %>%
      fill_empty_dates_with_na() %>%
      complete_cumulative_columns()
    
    # Select and arrange the data -------------------------------------------------------
    if (include_level_2_regions) {
      data <- data %>%
        dplyr::select(date, region_level_2, level_2_region_code, region_level_1, level_1_region_code, 
                      cases_new, cases_total, deaths_new, deaths_total,
                      recovered_new, recovered_total, hosp_new, hosp_total,
                      tested_new, tested_total, dplyr::everything()) %>%
        dplyr::arrange(date, region_level_1, region_level_2)
    } else {
      data <- data %>%
        dplyr::select(date, region_level_1, level_1_region_code, cases_new, cases_total, deaths_new, deaths_total,
                      recovered_new, recovered_total, hosp_new, hosp_total,
                      tested_new, tested_total, dplyr::everything()) %>%
        dplyr::arrange(date, region_level_1)
    }
  }
  
  # Rename the region column to country-specific --------------------------------------
  if (localise_regions) {
    data <- rename_region_column(data, country)
  }
  
  data <- rename_region_code_column(data, country)
  
  return(tibble::tibble(data))
}
