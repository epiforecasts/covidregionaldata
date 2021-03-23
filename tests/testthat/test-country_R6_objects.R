
check_country_fields <- function(regionClass) {
  expected_public_fields <- names(CountryTemplate$public_fields)
  region_public_fields <- names(regionClass$public_fields)
  for (field in expected_public_fields) {
    if (!(field %in% region_public_fields)) {
      stop(
        paste0(
          field, " required but missing in '", regionClass$classname, "'"
        )
      )
    }
  }
  return(0)
}

check_all_country_fields <- function() {
  avalibale_country_data <- get_available_datasets()
  avaliable_sources <- avalibale_country_data$country
  for (country in avaliable_sources) {
    regionClass <- get(country)
    check_country_fields(regionClass)
  }
  return(0)
}

check_country_methods <- function(regionClass) {
  expected_public_methods <- names(CountryTemplate$public_methods)
  region_public_methods <- names(regionClass$public_methods)
  for (method in expected_public_methods) {
    if (!(method %in% region_public_methods)) {
      stop(paste0(
        "function: ",
        method,
        "() required but missing in '",
        regionClass$classname,
        "'"
      ))
    }
  }
  return(0)
}

check_all_country_methods <- function() {
  avalibale_country_data <- get_available_datasets()
  avaliable_sources <- avalibale_country_data$country
  for (country in avaliable_sources) {
    regionClass <- get(country)
    check_country_methods(regionClass)
  }
  return(0)
}

check_clean_cols <- function(data) {
  expect_s3_class(data[["date"]], "Date")
}

check_all_country_cols <- function(level) {
  avalibale_country_data <- get_available_datasets()
  avaliable_sources <- dplyr::filter(
    avalibale_country_data,
    grepl(
      "[A-Z][a-z]",
      country
    )
  )
  countries <- avaliable_sources$country
  for (country in countries) {
    message(country)
    data <- get_regional_data(country, level = level, class = T, verbose = F)
    check_clean_cols(data$region$clean)
  }
}

# run tests
test_that(
  "Test all countries contain expected public fields",
  {
    expect_equal(
      check_all_country_fields(),
      0
    )
  }
)

test_that(
  "Test all countries contain expected public methods",
  {
    expect_equal(
      check_all_country_methods(),
      0
    )
  }
)

test_that(
  "Test all clean country datasets have expected col types for 'Date' and region_level",
  {
    check_all_country_cols(level = 1)
  }
)
