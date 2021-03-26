check_fields <- function(field, region_class, country) {
  test_that(
    paste(
      "Expect", field, "is in",
      country, "public fields"
    ),
    {
      expect_true(field %in% names(region_class$public_fields))
    }
  )
}

check_methods <- function(method, region_class, country) {
  test_that(
    paste(
      "Expect", method, "is in",
      country, "public methods"
    ),
    {
      expect_true(method %in% names(region_class$public_methods))
    }
  )
}

check_country_class <- function(country) {
  region_class <- get(country)
  expected_public_fields <- names(CountryTemplate$public_fields)
  expected_public_methods <- names(CountryTemplate$public_methods)
  purrr::walk(
    expected_public_fields,
    check_fields,
    region_class,
    country
  )
  purrr::walk(
    expected_public_methods,
    check_methods,
    region_class,
    country
  )
}

# run tests
available_country_data <- get_available_datasets()
purrr::walk(available_country_data$country, check_country_class)
