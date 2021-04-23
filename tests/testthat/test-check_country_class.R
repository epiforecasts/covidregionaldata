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

check_country_class <- function(country) {
  region_class <- get(country)
  expected_public_fields <- c(
    "origin", "supported_levels",
    "supported_region_names",
    "supported_region_codes",
    "common_data_urls", "source_data_cols"
  )
  purrr::walk(
    expected_public_fields,
    check_fields,
    region_class,
    country
  )
}

# run tests
available_country_data <- get_available_datasets()
purrr::walk(available_country_data$class, check_country_class)
