#' Check data sources
#' @description  Check that data are up to date and returning correctly
#' @param countries names of countries as in the "country.R" file name to check
#' @param worldwide report on worldwide datasets - ECDC and WHO, interventions, linelist
#' @return A tibble of latest dates for all sources and data checks for sub-national data
#' @export
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr group_by filter bind_rows summarise pull rename
check_data_sources <- function(countries = c("afghanistan",
                                             "belgium", "brazil",
                                             "canada", "colombia",
                                             "germany",
                                             "india", "italy",
                                             "russia",
                                             "uk", "usa"), 
                               worldwide = TRUE) {
  
# Get data ----------------------------------------------------------------
  # Run each country - level 1 / level 2 where available
  country_data <- purrr::map(countries, 
                             ~ covidregionaldata::get_regional_data(country = .x, 
                                                                    localise_regions = FALSE,
                                                                    include_level_2_regions = TRUE))
  names(country_data) <- countries
  
# Filter to latest data for each subnational region -----------------------
  country_data_latest <- purrr::map(country_data,
                                    ~ .x %>%
                                      dplyr::group_by(region_level_1) %>%
                                      dplyr::filter(!is.na(cases_new)) %>%
                                      dplyr::filter(date == max(date))) %>%
    dplyr::bind_rows(.id = "country") %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(max_date = max(date), 
                     min_date = min(date),
                     iso_na = sum(is.na(iso_3166_2)),
                     cases_new_na = sum(is.na(cases_new)),
                     cases_total_na = sum(is.na(cases_total)),
                     deaths_new_na = sum(is.na(deaths_new)),
                     deaths_total_na = sum(is.na(deaths_total)),
                     .groups = "drop")
  
  # Report ------------------------------------------------------------------
  test_country_out_of_date <- country_data_latest %>%
    dplyr::filter(min_date <= (Sys.Date() - 7)) %>%
    dplyr::pull(country)
  if (length(test_country_out_of_date) > 0) {
    message(writeLines(text = c("* Country data out of date:", test_country_out_of_date)))
  }
  
  test_region_out_of_date <- country_data_latest %>%
    dplyr::filter(min_date <= (max_date - 7)) %>%
    dplyr::pull(country)
  if (length(test_region_out_of_date) > 0) {
    message(writeLines(text = c("* Some regions missing latest data:",test_region_out_of_date)))
  }
  
  test_regions_missing_iso <- country_data_latest %>%
    dplyr::filter(iso_na > 0) %>%
    dplyr::pull(country)
  if (length(test_regions_missing_iso) > 0) {
    message(writeLines(text = c("* Some regions missing ISO codes:",test_regions_missing_iso)))
  }
  
  test_regions_missing_cases <- country_data_latest %>%
    dplyr::filter(cases_new_na > 0 | cases_total_na > 0) %>%
    dplyr::pull(country)
  if (length(test_regions_missing_cases > 0)) {
    message(writeLines(text = c("* Some regions missing new or total cases:", test_regions_missing_cases)))
  }
  
  test_regions_missing_deaths <- country_data_latest %>%
    dplyr::filter(deaths_new_na > 0 | deaths_total_na > 0) %>%
    dplyr::pull(country)
  if (length(test_regions_missing_deaths > 0)) {
    message(writeLines(text = c("* Some regions missing new or total deaths:", test_regions_missing_deaths)))
    }
# National and other data sets --------------------------------------------
  if (worldwide) {
  # Interventions
  interventions <- covidregionaldata::get_interventions_data()
  if (max(interventions$entry_date) < (Sys.Date() - 30)) {
    message("get_interventions_data: last data entry on ", max(interventions$entry_date))
  }
  
  # Linelist
  linelist <- covidregionaldata::get_linelist(clean = TRUE)
  if (max(linelist$date_confirm, na.rm = T) < (Sys.Date() - 30)) {
    message("get_linelist: last reported case on ", max(linelist$date_confirm, na.rm = T))
  }
  
  # National data
  national_who <- covidregionaldata::get_national_data(source = "WHO")
  if (max(national_who$date) < (Sys.Date() - 7)) {
    message("WHO data: last reported case on ", max(national_who$date, na.rm = T))
  }
      
  national_ecdc <- covidregionaldata::get_national_data(source = "ECDC")
  if (max(national_ecdc$date) < (Sys.Date() - 7)) {
    message("ECDC data: last reported case on ", max(national_ecdc$date, na.rm = T))
  }
  
  national_data_latest <- tibble::tibble("source" = c("interventions", "linelist", "who", "ecdc"),
                                         "max_date" = c(max(interventions$entry_date), 
                                                        max(linelist$date_confirm, na.rm = T),
                                                        max(national_who$date),
                                                        max(national_ecdc$date)))
  
  # Return a df with data check results
  latest_checks <- country_data_latest %>%
    dplyr::rename(source = country) %>%
    dplyr::bind_rows(national_data_latest)
  }else {
    latest_checks <- country_data_latest %>%
      dplyr::rename(source = country)
  }
return(latest_checks)
}





