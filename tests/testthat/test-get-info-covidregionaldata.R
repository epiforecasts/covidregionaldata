# Get countries in package ---------------------------------------
countries <- get_info_covidregionaldata() %>%
  dplyr::filter(get_data_function == "get_regional_data" & !is.na(source_data_cols))

n_countries <- nrow(countries)

# Test get_info_covidregionaldata
test_that("get_info_covidregionaldata returns dataframe", {
  expect_s3_class(countries, "data.frame")
})

test_that("get_info_covidregionaldata returns at least 13 countries", {
  expect_gte(n_countries, 13)
})

test_that("get_info_covidregionaldata includes data source for all countries", {
  n_url <- dplyr::pull(countries, data_url)
  expect_length(n_url, n_countries)
})
