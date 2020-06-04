#' The main calculation function for NCovUtils which returns regional, daily-updated data for a given country to be passed into the relevant exported wrapper function.
#' @description Gets Covid-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country.
#' @param country Character String specifying the country to get data from. For options see the documentation.
#' @param totals Boolean. If TRUE, returns only totals count, if FALSE returns the full dataset.
#' @return A data.frame with data related to cases, deaths, hospitalisations, recoveries and testing for regions within the given country.
#' @importFrom dplyr %>% group_by left_join arrange select ungroup do
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
get_regional_covid_data <- function(country, totals){

  if (!(is.character(country))){
    stop("The country variable should be a character variable.")
  }

  # get the correct data given the country
  get_data_function <- switch(country,
                              "canada" = get_canada_regional_cases,
                              "afghanistan" = get_afghan_regional_cases,
                              "belgium" = get_belgium_regional_cases,
                              "brazil" = get_brazil_regional_cases,
                              "germany" = get_germany_regional_cases,
                              "india" = get_india_regional_cases,
                              "italy" = get_italy_regional_cases,
                              stop("There is no data for the country entered. It is likely haven't added data
                                   for that country yet, or there was a spelling mistake."))
  data <- do.call(get_data_function, list())
  iso_codes_table <- get_iso_codes(country)

  # add columns that aren't there already, clean up data
  data <- data %>%
    dplyr::group_by(region) %>%
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
    dplyr::left_join(iso_codes_table, by = c("region", "region")) %>%
    dplyr::select(date, region, iso_code, cases_new, cases_total, deaths_new, deaths_total,
                  recoveries_new, recoveries_total, hospitalisations_new, hospitalisations_total,
                  tests_new, tests_total) %>%
    rename_region_column(country) %>%
    dplyr::arrange(date)

  return(tibble::tibble(data))
}

#' Return regional, daily-updated cumulative counts of Covid-19 data for a given country
#' @description Gets cumulative COVID-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country.
#' @param country Character String specifying the country to get data from. For options see the documentation.
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
get_totals_only_regional_covid_data <- function(country) {

  country <- tolower(country)

  data <- get_regional_covid_data(country, totals = TRUE)
  iso_codes_table <- get_iso_codes(country)

  # sum up data if user requests totals
  data <- data %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(cases_total = sum(cases_new, na.rm = TRUE),
                     deaths_total = sum(deaths_new, na.rm = TRUE),
                     recoveries_total = sum(recoveries_new, na.rm = TRUE),
                     hospitalisations_total = sum(hospitalisations_new, na.rm = TRUE),
                     tests_total = sum(tests_new, na.rm = TRUE)) %>%
    dplyr::left_join(iso_codes_table, by = c("region", "region")) %>%
    dplyr::select(region, iso_code, cases_total, deaths_total,
                  recoveries_total, hospitalisations_total, tests_total) %>%
    rename_region_column(country) %>%
    dplyr::arrange(-cases_total)

  return(tibble::tibble(data))
}

#' Return regional, daily-updated data for a given country in long (Covid19R / non-time-series) format
#' @description Gets COVID-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country in long format.
#' @param country Character String specifying the country to get data from. For options see the documentation.
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
  data <- get_regional_covid_data(country, totals = FALSE)

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
get_wide_format_regional_covid_data <- function(country) {
  country <- tolower(country)
  data <- get_regional_covid_data(country, totals = FALSE)

  return(tibble::tibble(data))
}

