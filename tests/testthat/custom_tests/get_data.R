# Get data used in testing 
#  - test-get-info-covidregionaldata
#  - test-get-country-regional-data

# Get countries to test
countries <- get_info_covidregionaldata() %>%
  dplyr::filter(get_data_function == "get_regional_data" & !is.na(source_data_cols))
# Return failed attempts as NULL
safely_get_regional_data <- purrr::possibly(get_regional_data, 
                                            otherwise = NULL)

# Admin level 1 data  -------------------------------------------
countries_level_1 <- countries %>%
  dplyr::pull(country)
data_level_1 <- purrr::map(countries_level_1, 
                           ~ safely_get_regional_data(country = .x, 
                                                    localise_regions = FALSE,
                                                    include_level_2_regions = FALSE))
names(data_level_1) <- countries_level_1
data_level_1 <- data_level_1 %>%
  purrr::keep(~ !is.null(.x))
success_level_1 <- names(data_level_1)

# Admin level 2 data ------------------------------------------------------
countries_level_2 <- countries %>%
  dplyr::filter(!is.na(level_2_region)) %>%
  dplyr::pull(country)
data_level_2 <- purrr::map(countries_level_2, 
                           ~ get_regional_data(country = .x, 
                                               localise_regions = FALSE,
                                               include_level_2_regions = TRUE))
names(data_level_2_all) <- countries_level_2
data_level_2 <- data_level_2 %>%
  purrr::keep(~ !is.null(.x))
success_level_2 <- names(data_level_2)

# Function for testing data types ------------------------------------------------------
expect_data_type <- function(country, data_list, level = 1) {
  expect_s3_class(data_list[[country]][["date"]], "Date")
  expect_type(data_list[[country]][["cases_new"]], "double")
  expect_type(data_list[[country]][["cases_total"]], "double")
  expect_type(data_list[[country]][["region_level_1"]], "character")
  if (level == 2) {
    expect_type(data_list[[country]][["region_level_2"]], "character")
  }
}

