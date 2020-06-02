source("custom_tests/mock_data.R")

test_that("get_daily_from_cumulative returns correct results", {
  column1 <- c(0, 3, 2, 10, 0, 14, 1)
  cumulative_col <- cumsum(column1)
  calculated_daily <- get_daily_from_cumulative(cumulative_col)

  expect_equal(column1, calculated_daily)
})

test_that("get_cumulative_from_daily returns correct results", {
  column1 <- c(0, 3, 2, NA, 0, 14, 1)
  cumulative_col <- c(0, 3, 5, 5, 5, 19, 20)
  calculated_daily <- get_cumulative_from_daily(column1)

  expect_equal(cumulative_col, calculated_daily)
})

test_that("calculate_columns_from_existing_data returns correct results", {
  cases_today <- c(0, 3, 2, NA_integer_, 0, 14, 1)
  cumulative_deaths <- c(0, 5, 7, 10, 15, 18, 26)
  input_data <- data.frame(cbind(cases_today, cumulative_deaths))

  cumulative_cases <- c(0, 3, 5, 5, 5, 19, 20)
  deaths_today <- c(0, 5, 2, 3, 5, 3, 8)
  expected_data <- data.frame(cbind(cases_today, cumulative_deaths, cumulative_cases, deaths_today))

  expect_equal(calculate_columns_from_existing_data(input_data), expected_data)
})

test_that("add_extra_na_cols is working", {
  extra_col_names <- c("date", "region", "cases_today", "cumulative_cases", "deaths_today", "cumulative_deaths",
                      "recoveries_today", "cumulative_recoveries", "tests_today", "cumulative_tests", "hospitalisations_today",
                      "cumulative_hospitalisations")

  existing_col_names <- colnames(mtcars)

  new_dataset <- add_extra_na_cols(mtcars)

  expect_equal(c(existing_col_names, extra_col_names), colnames(new_dataset))
  expect_true(all(is.na(new_dataset$cases_today)))
})

test_that("rename_region_column does so correctly", {
  df <- data.frame(matrix(rnorm(100), ncol=10))
  colnames(df)[1] <- "region"

  expect_equal(colnames(rename_region_column(df, "canada"))[1], "province")
  expect_error(rename_region_column(df, "test"))
})

test_that("set_negative_values_to_zero works", {
  df <- data.frame(matrix(c(rep(Sys.Date(), 100), 49:-50), ncol=2))
  colnames(df) <- c("date", "cumulative_cases")

  df_expected <- data.frame(matrix(c(rep(Sys.Date(), 100), c(49:0, rep(0, 50))), ncol=2))
  colnames(df_expected) <- c("date", "cumulative_cases")

  df_actual <- set_negative_values_to_zero(df)

  expect_equal(df_actual, df_expected)
})

test_that("fill_empty_dates_with_na fills empty dates with NA", {
  expected_data <- get_expected_data_for_helpers()

  # partial data deletes some rows (i.e. gets rid of some dates - all the ones with NA in cases)
  partial_data <- expected_data[-c(6:9), ]

  expect_equal(fill_empty_dates_with_na(partial_data), expected_data)
})

test_that("complete_cumulative_columns works", {
  data_with_cum_cases_na <- get_input_data_for_complete_cumulative_columns_test()
  data_with_cum_cases_filled <- get_expected_data_for_complete_cumulative_columns_test()

  expect_equal(complete_cumulative_columns(data_with_cum_cases_na), data_with_cum_cases_filled)
})

test_that("convert_to_Covid19R_format converts correctly", {
  input_data <- get_input_data_for_covid19R_converter_test()
  expected_data <- get_expected_data_for_covid19R_converter_test()

  returned <- convert_to_covid19R_format(input_data)

  expect_equal(convert_to_covid19R_format(input_data), expected_data)
})
