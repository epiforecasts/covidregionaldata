test_that("get_available_datasets works without error", {
  expect_s3_class(get_available_datasets(), "data.frame")
})

data <- get_available_datasets()
test_that(
  "Test column names are as expected",
  {
    expected_names <- c(
      "origin", "class", "level_1_region",
      "level_2_region", "level_3_region", "type",
      "data_urls", "source_data_cols",
      "source_text", "source_url"
    )
    expect_identical(colnames(data), expected_names)
  }
)
test_that(
  "Test Italy level 1 region is regioni",
  {
    expected <- "regioni"
    actual <- subset(data, data$origin == "Italy")$level_1_region
    expect_equal(expected, actual)
  }
)
test_that(
  "Test each origin has only one row",
  {
    number_of_rows <- nrow(data)
    number_of_countries <- length(unique(data$origin))
    expect_equal(number_of_countries, number_of_rows)
  }
)
test_that(
  "Test at least 5 countries are available",
  {
    number_of_rows <- nrow(data)
    number_of_countries <- length(unique(data$origin))
    expect_gte(number_of_countries, 5)
  }
)

# test each column for na values
for (name in colnames(data)) {
  if (name %in% c(
    "level_1_region",
    "level_2_region",
    "level_3_region",
    "source_text",
    "source_url"
  )) {
    next
  }
  test_that(
    paste("Test", name, "column has no `na` values"),
    {
      expect_true(!(TRUE %in% is.na(data[name])))
    }
  )
}

test_that("Regional level datasets can be filtered for", {
  reg <- get_available_datasets("regional")
  expect_equal(nrow(dplyr::filter(reg, class %in% "ECDC")), 0)
  expect_equal(unique(reg$type), "regional")
})

test_that("National level datasets can be filtered for", {
  nat <- get_available_datasets("national")
  expect_equal(nrow(dplyr::filter(nat, class %in% "Italy")), 0)
  expect_equal(unique(nat$type), "national")
})

test_that(
  paste(
    "all_country_data is up to date.",
    "This will fail if a new data source has been added, or an existing one",
    "modified, but the table outlining available data (all_country_data) has",
    "not been re-rendered. Run the code in",
    "data-raw/render_available_datasets.R to update all_country_data"
  ),
  {
    package_avaliable_data <- get_available_datasets()
    rendered_available_data <- get_available_datasets(render = TRUE)
    expect_identical(
      package_avaliable_data,
      rendered_available_data
    )
  }
)
