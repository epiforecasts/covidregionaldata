#' The main calculation function for covidregionaldata which returns regional, daily-updated data for a given country to be passed into the relevant exported wrapper function.
#' @description Gets Covid-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country.
#' @param country Character String specifying the country to get data from. For options see the documentation.
#' @param totals Boolean. If TRUE, returns only totals count, if FALSE returns the full dataset.
#' @param include_level_2_regions Boolean. If TRUE, returns data stratified by level 2 regions. If FALSE, stratified by Level 1. Note that Level 2 region data
#' is not always available. In these cases the user will get a warning and the Level 1 data will be returned.
#' @return A data.frame with data related to cases, deaths, hospitalisations, recoveries and testing stratified by regions within the given country.
#' @importFrom dplyr %>% group_by left_join arrange select ungroup do
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
get_regional_covid_data <- function(country, totals, include_level_2_regions){

  #----------------------------------------------#
  #-------------- ERROR HANDLING ----------------#
  #----------------------------------------------#

  if (!(is.character(country))){
    stop("The country variable should be a character variable.")
  }

  if (!(is.logical(totals))){
    stop("The totals variable should be a logical (TRUE/FALSE) variable.")
  }

  if (!(is.logical(include_level_2_regions))){
    stop("The include_level_2_regions variable should be a logical (TRUE/FALSE) variable.")
  }

  countries_with_level_2_regions <- c("belgium",
                                      "brazil",
                                      "germany")

  if (include_level_2_regions & !(country %in% countries_with_level_2_regions)) {
    warning("The data for that country doesn't have data at Admin Level 2. Returning data for Admin Level 1 only.")
    include_level_2_regions <- FALSE
  }

  #----------------------------------------------#

  # find the correct data-getter
  if (include_level_2_regions) {

    get_data_function <- switch(country,
                                "belgium" = get_belgium_regional_cases_with_level_2,
                                "brazil" = get_brazil_regional_cases_with_level_2,
                                "germany" = get_germany_regional_cases_with_level_2,
                                stop("There is no data for the country entered. It is likely haven't added data
                                   for that country yet, or there was a spelling mistake."))

  } else {

    get_data_function <- switch(country,
                                "canada" = get_canada_regional_cases,
                                "afghanistan" = get_afghan_regional_cases,
                                "belgium" = get_belgium_regional_cases_only_level_1,
                                "brazil" = get_brazil_regional_cases_only_level_1,
                                "germany" = get_germany_regional_cases_only_level_1,
                                "india" = get_india_regional_cases,
                                "italy" = get_italy_regional_cases,
                                stop("There is no data for the country entered. It is likely haven't added data
                                   for that country yet, or there was a spelling mistake."))

  }

  # get the data and iso codes for level 1 regions
  data <- do.call(get_data_function, list())
  iso_codes_table <- get_iso_codes(country)

  # group data, dependent on admin levels required
  if (include_level_2_regions) {
    data <- data %>%
      dplyr::group_by(region_level_1, region_level_2)
  } else {
    data <- data %>%
      dplyr::group_by(region_level_1)
  }

  # add columns that aren't there already, clean up data
  data <- data %>%
    dplyr::do(calculate_columns_from_existing_data(.)) %>%
    add_extra_na_cols() %>%
    set_negative_values_to_zero() %>%
    dplyr::ungroup()


  if (totals) {
    return(tibble::tibble(data))
  }

  # select correct data, pad the data set and rename the region column to country-specific
  data <- data %>%
    tidyr::drop_na(date) %>%
    fill_empty_dates_with_na() %>%
    complete_cumulative_columns() %>%
    dplyr::left_join(iso_codes_table, by = c("region_level_1" = "region"))

  if (include_level_2_regions) {
    data <- data %>%
      dplyr::select(date, region_level_2, region_level_1, iso_code, cases_new, cases_total, deaths_new, deaths_total,
                    recovered_new, recovered_total, hosp_new, hosp_total,
                    tested_new, tested_total) %>%
      dplyr::arrange(date, region_level_1, region_level_2)

  } else {
    data <- data %>%
    dplyr::select(date, region_level_1, iso_code, cases_new, cases_total, deaths_new, deaths_total,
                  recovered_new, recovered_total, hosp_new, hosp_total,
                  tested_new, tested_total) %>%
    dplyr::arrange(date, region_level_1)
  }

  data <- data %>%
    rename_region_column(country)

  return(tibble::tibble(data))
}

#' Return regional, daily-updated cumulative counts of Covid-19 data for a given country
#' @description Gets cumulative COVID-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country.
#' @param country Character String specifying the country to get data from. For options see the documentation.
#' @param include_level_2_regions Boolean. If TRUE, returns data stratified by level 2 regions. If FALSE, stratified by Level 1. Note that Level 2 region data
#' is not always available. In these cases the user will get a warning and the Level 1 data will be returned.
#' @return A data.frame with data related to cases, deaths, hospitalisations, recoveries and testing for regions within the given country, cumulative up to today's date.
#' @export
#' @importFrom dplyr %>% group_by summarise left_join select ungroup arrange
#' @importFrom tibble tibble
#' @examples
#'
#' \dontrun{
#'
#'  get_totals_only_regional_covid_data(country = "canada")
#'
#' }
get_totals_only_regional_covid_data <- function(country, include_level_2_regions = FALSE) {

  country <- tolower(country)

  data <- get_regional_covid_data(country, totals = TRUE, include_level_2_regions = include_level_2_regions)
  iso_codes_table <- get_iso_codes(country)

  # sum up data if user requests totals
  if (include_level_2_regions) {
    data <- data %>%
      dplyr::group_by(region_level_1, region_level_2)
  } else {
    data <- data %>%
      dplyr::group_by(region_level_1)
  }

  data <- data %>%
    dplyr::summarise(cases_total = sum(cases_new, na.rm = TRUE),
                     deaths_total = sum(deaths_new, na.rm = TRUE),
                     recovered_total = sum(recovered_new, na.rm = TRUE),
                     hosp_total = sum(hosp_new, na.rm = TRUE),
                     tested_total = sum(tested_new, na.rm = TRUE)) %>%
    dplyr::left_join(iso_codes_table, by = c("region_level_1" = "region"))


  if (include_level_2_regions) {
    data <- data %>%
      dplyr::select(region_level_2, region_level_1, iso_code, cases_total, deaths_total,
                    recovered_total, hosp_total, tested_total)
  } else {
    data <- data %>%
      dplyr::select(region_level_1, iso_code, cases_total, deaths_total,
                    recovered_total, hosp_total, tested_total)
  }

  data <- data %>%
    rename_region_column(country) %>%
    dplyr::arrange(-cases_total)

  return(tibble::tibble(data))
}

#' Return regional, daily-updated data for a given country in long (Covid19R / non-time-series) format
#' @description Gets COVID-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country in long format.
#' @param country Character String specifying the country to get data from. For options see the documentation.
#' @param include_level_2_regions Boolean. If TRUE, returns data stratified by level 2 regions. If FALSE, stratified by Level 1. Note that Level 2 region data
#' is not always available. In these cases the user will get a warning and the Level 1 data will be returned.
#' @return A data.frame with data related to cases, deaths, hospitalisations, recoveries and testing for regions within the given country. Either totals only or full data.
#' @importFrom tibble tibble
#' @export
#' @examples
#'
#' \dontrun{
#'
#'  get_long_format_regional_covid_data(country = "canada")
#'
#' }
get_long_format_regional_covid_data <- function(country) {

  country <- tolower(country)
  data <- get_regional_covid_data(country, totals = FALSE, include_level_2_regions = FALSE)

  # Covid19R format
  data <- convert_to_covid19R_format(data)

  return(tibble::tibble(data))
}

#' Return regional, daily-updated data for a given country in wide (time-series) format.
#' @description Gets COVID-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country in wide format.
#' @param country Character String specifying the country to get data from. For options see the documentation.
#' @return A data.frame with data related to cases, deaths, hospitalisations, recoveries and testing for regions within the given country.
#' @export
#' @importFrom tibble tibble
#' @examples
#'
#' \dontrun{
#'
#'  get_wide_format_regional_covid_data(country = "canada")
#'
#' }
get_wide_format_regional_covid_data <- function(country, include_level_2_regions = FALSE) {
  country <- tolower(country)
  data <- get_regional_covid_data(country, totals = FALSE, include_level_2_regions = include_level_2_regions)

  return(tibble::tibble(data))
}

