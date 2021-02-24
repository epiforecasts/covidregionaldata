# Get data used in testing 
#  - test-get-info-covidregionaldata
#  - test-get-country-regional-data

# Get countries to test
countries <- get_info_covidregionaldata() %>%
  dplyr::filter(get_data_function == "get_regional_data" & !is.na(source_data_cols))

# Admin level 1 data  -------------------------------------------

countries_level_1 <- countries %>%
  dplyr::pull(country)

data_level_1 <- purrr::map(countries_level_1, 
                           ~ get_regional_data(country = .x, 
                                               localise_regions = FALSE,
                                               include_level_2_regions = FALSE))
names(data_level_1) <- countries_level_1


# Admin level 2 data ------------------------------------------------------

countries_level_2 <- countries %>%
  dplyr::filter(!is.na(level_2_region)) %>%
  dplyr::pull(country)

data_level_2 <- purrr::map(countries_level_2, 
                           ~ get_regional_data(country = .x, 
                                               localise_regions = FALSE,
                                               include_level_2_regions = TRUE))
names(data_level_2) <- countries_level_2

