data <- get_available_datasets()
test_that(
  "Test column names are as expected",
  {
    expected_names <- c(
      "country", "level_1_region",
      "level_2_region", "get_data_function",
      "data_url", "source_data_cols"
    )
    expect_true(identical(colnames(data), expected_names))
  }
)
test_that(
  "Test Italy level 1 region is regioni",
  {
    expected <- "regioni"
    actual <- subset(data, data$country == "Italy")$level_1_region
    expect_equal(expected, actual)
  }
)
test_that(
  "Test each country has only one row",
  {
    number_of_rows <- nrow(data)
    number_of_countries <- length(unique(data$country))
    expect_equal(number_of_countries, number_of_rows)
  }
)
