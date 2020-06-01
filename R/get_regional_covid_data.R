#' The main function for NCovUtils which returns regional, daily-updated data for a given country - either in full or as a total for each region up to today's date.
#' @description Gets COVID-19 data related to cases, deaths, hospitalisations, recoveries and testing for sub-regions of a given country.
#' @param country Character String specifying the country to get data from. For options see the documentation.
#' @param totals Boolean. If TRUE, returns only totals count, if FALSE (default) returns the full dataset.
#' @return A data.frame with data related to cases, deaths, hospitalisations, recoveries and testing for regions within the given country. Either totals only or full data.
#' @importFrom dplyr %>% group_by summarise arrange select ungroup
#' @examples
#'
#' \dontrun{
#'
#'  get_regional_covid_data(country = "canada")
#'
#' }
get_regional_covid_data <- function(country, totals = FALSE){

  if (!(is.character(country))){
    stop("The country variable should be a character variable.")
  }

  # get the correct data given the country
  get_data_function <- switch(tolower(country),
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

  # sum up data if user requests totals
  if (totals) {
    data <- data %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(cumulative_cases = sum(cases_today, na.rm = TRUE),
                       cumulative_deaths = sum(deaths_today, na.rm = TRUE),
                       cumulative_recoveries = sum(recoveries_today, na.rm = TRUE),
                       cumulative_hospitalisations = sum(hospitalisations_today, na.rm = TRUE),
                       cumulative_tests = sum(tests_today, na.rm = TRUE)) %>%
      dplyr::left_join(iso_codes_table, by = c("region", "region")) %>%
      dplyr::select(region, iso_code, cumulative_cases, cumulative_deaths,
                    cumulative_recoveries, cumulative_hospitalisations, cumulative_tests) %>%
      rename_region_column(country) %>%
      dplyr::arrange(-cumulative_cases)
    return(data)
  }

  # select correct data, pad the data set and rename the region column to country-specific
    data <- data %>%
    tidyr::drop_na(date) %>%
    fill_empty_dates_with_na() %>%
    complete_cumulative_columns() %>%
    dplyr::left_join(iso_codes_table, by = c("region", "region")) %>%
    dplyr::select(date, region, iso_code, cases_today, cumulative_cases, deaths_today, cumulative_deaths,
                  recoveries_today, cumulative_recoveries, hospitalisations_today, cumulative_hospitalisations,
                  tests_today, cumulative_tests) %>%
    rename_region_column(country) %>%
    dplyr::arrange(date)

  return(data.frame(data))
}
