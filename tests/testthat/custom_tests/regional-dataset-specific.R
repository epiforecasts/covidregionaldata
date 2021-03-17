test_ecdc_level_1 <- function(region) {
  test_that("ecdc data has expected format", {
    necessary_cols <- c(
      "geoId", "countriesAndTerritories",
      "cases", "deaths", "popData2019"
    )
    expect_true(all(necessary_cols %in% colnames(region$raw)))
    all_countries <- region$return %>%
      dplyr::filter(is.na(un_region)) %>%
      dplyr::group_by(country) %>%
      dplyr::tally()
    expect_true(nrow(all_countries) == 0)
  })
}

test_who_level_1 <- function(region) {
  test_that("who data has expected format", {
    all_countries <- region$return %>%
      dplyr::filter(is.na(un_region)) %>%
      dplyr::group_by(country) %>%
      dplyr::tally()
    expect_true(nrow(all_countries) == 0)
  })
}

# Add data set specific custom tests here in the format of above using
# variables from test_regional_dataset() environment
