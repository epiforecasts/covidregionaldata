check_fields <- function(field, region_class) {
  test_that(
    paste(
      "Expect", field, "is in",
      class(region_class)[1], "public fields"
    ),
    {
      expect_true(field %in% names(region_class$public_fields))
    }
  )
}

check_methods <- function(method, region_class) {
  test_that(
    paste(
      "Expect", method, "is in",
      class(region_class)[1], "public methods"
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
    region_class
  )
  purrr::walk(
    expected_public_methods,
    check_methods,
    region_class
  )
}

# run tests
available_country_data <- get_available_datasets()
purrr::walk(available_country_data$country, check_country_class)
