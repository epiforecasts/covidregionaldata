data <- get_available_datasets()
test_that(
  "Test column names are as expected",
  {
    expected_names <- c(
      "country", "level_1_region",
      "level_2_region", "get_data_function",
      "data_url", "source_data_cols"
    )
    expect_identical(colnames(data), expected_names)
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
test_that(
  "Test at least 5 countries are available",
  {
    number_of_rows <- nrow(data)
    number_of_countries <- length(unique(data$country))
    expect_gte(number_of_countries, 5)
  }
)

# test each column for na values
for (name in colnames(data)) {
  if (name == "level_2_region") {
    next
  }
  test_that(
    paste("Test", name, "column has no `na` values"),
    {
      expect_true(!(TRUE %in% is.na(data[name])))
    }
  )
}
