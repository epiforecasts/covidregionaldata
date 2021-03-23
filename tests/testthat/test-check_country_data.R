check_clean_cols <- function(level, name, data) {
  test_that(
    paste("Expect column 'date' for", name, "at level", level, "is of 'Date' type"),
    {
      expect_s3_class(data[["date"]], "Date")
    }
  )
}

check_processed_cols <- function(level, name, data) {
  test_that(
    paste("Expect", name, "at level", level, "has correct colum types"),
    {
      expect_s3_class(data[["date"]], "Date")
      expect_type(data[["cases_new"]], "double")
      expect_type(data[["cases_total"]], "double")
      expect_type(data[["deaths_new"]], "double")
      expect_type(data[["deaths_total"]], "double")
    }
  )
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
  if (level == 2) {
    avaliable_sources <- dplyr::filter(
      avaliable_sources, !(is.na(level_2_region))
    )
  }
  countries <- avaliable_sources$country
  for (country in countries) {
    data <- get_regional_data(country, level = level, class = T, verbose = F)
    check_clean_cols(level, country, data$region$clean)
    check_processed_cols(level, country, data$region$processed)
  }
}

testthat::skip("Skipping data checks as very slow")
check_all_country_cols(level = 1)
check_all_country_cols(level = 2)
