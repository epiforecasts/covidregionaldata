check_country_fields <- function(regionClass) {
  expected_public_fields <- names(CountryTemplate$public_fields)
  region_public_fields <- names(regionClass$public_fields)
  for (field in expected_public_fields) {
    test_that(
      paste("Expect", field, "is in", class(regionClass)[1], "public fields"),
      {
        expect_true(field %in% region_public_fields)
      }
    )
  }
  return(0)
}

check_all_country_fields <- function() {
  available_country_data <- get_available_datasets()
  available_sources <- available_country_data$country
  for (country in available_sources) {
    regionClass <- get(country)
    check_country_fields(regionClass)
  }
  return(0)
}

check_country_methods <- function(regionClass) {
  expected_public_methods <- names(CountryTemplate$public_methods)
  region_public_methods <- names(regionClass$public_methods)
  for (method in expected_public_methods) {
    test_that(
      paste("Expect", method, "is in", class(regionClass)[1], "public methods"),
      {
        expect_true(method %in% region_public_methods)
      }
    )
  }
  return(0)
}

check_all_country_methods <- function() {
  available_country_data <- get_available_datasets()
  available_sources <- available_country_data$country
  for (country in available_sources) {
    regionClass <- get(country)
    check_country_methods(regionClass)
  }
  return(0)
}

# run tests
check_all_country_fields()
check_all_country_methods()
