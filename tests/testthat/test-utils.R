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
  cases_new <- c(0, 3, 2, NA_integer_, 0, 14, 1)
  deaths_total <- c(0, 5, 7, 10, 15, 18, 26)
  input_data <- tibble::tibble(cases_new, deaths_total)

  cases_total <- c(0, 3, 5, 5, 5, 19, 20)
  deaths_new <- c(0, 5, 2, 3, 5, 3, 8)
  expected_data <- tibble::tibble(cases_new, deaths_total, cases_total, deaths_new)

  expect_equal(calculate_columns_from_existing_data(input_data), expected_data)
})

test_that("add_extra_na_cols is working", {
  extra_col_names <- c("cases_new", "cases_total", "deaths_new", "deaths_total",
                      "recovered_new", "recovered_total", "tested_new", "tested_total", "hosp_new",
                      "hosp_total")

  existing_col_names <- colnames(mtcars)

  new_dataset <- add_extra_na_cols(mtcars)

  expect_equal(c(existing_col_names, extra_col_names), colnames(new_dataset))
  expect_true(all(is.na(new_dataset$cases_new)))
})

test_that("rename_region_column does so correctly", {
  df <- data.frame(matrix(rnorm(100), ncol=10))
  colnames(df)[1] <- "region_level_1"

  expect_error(rename_region_column(df, "test"))
  expect_equal(colnames(rename_region_column(df, "canada"))[1], "province")
  
  colnames(df)[1] <- "region_level_1"
  colnames(df)[2] <- "region_level_2"
  expect_equal(colnames(rename_region_column(df, "belgium"))[1:2], c("region", "province"))
})

test_that("rename_region_code_column does so correctly", {
  df <- data.frame(matrix(rnorm(100), ncol=10))
  colnames(df)[1] <- "level_1_region_code"
  
  expect_error(rename_region_code_column(df, "test"))
  expect_equal(colnames(rename_region_code_column(df, "canada"))[1], "iso_3166_2")
  
  colnames(df)[1] <- "level_1_region_code"
  colnames(df)[2] <- "level_2_region_code"
  expect_equal(colnames(rename_region_code_column(df, "usa"))[1:2], c("iso_3166_2", "fips"))
})

test_that("set_negative_values_to_zero works", {
  dates <- c(rep(Sys.Date(), 100))
  values <- 49:-50
  df <- tibble::tibble(date = dates, cases_total = values)
  colnames(df) <- c("date", "cases_total")

  df_expected <- tibble::tibble(date = dates, cases_total = c(49:0, rep(0, 50)))

  df_actual <- set_negative_values_to_zero(df)

  expect_equal(df_actual, df_expected)
})

test_that("fill_empty_dates_with_na fills empty dates with NA", {
  expected_data <- get_expected_data_for_fill_empty_dates_with_na_test()

  # partial data deletes some rows (i.e. gets rid of some dates - all the ones with NA in cases)
  partial_data <- expected_data[-c(6:9), ]

  expect_equal(fill_empty_dates_with_na(partial_data), expected_data)
})

test_that("complete_cumulative_columns works", {
  input_data <- get_input_data_for_complete_cumulative_columns_test()
  expected_data <- get_expected_data_for_complete_cumulative_columns_test()

  actual_data <- complete_cumulative_columns(input_data)
  
  expect_equal(actual_data, expected_data)
})

#test_that("convert_to_Covid19R_format converts correctly", {
#  skip_on_cran()
#  
#  input_data <- get_input_data_for_covid19R_converter_test()
#  expected_data <- get_expected_data_for_covid19R_converter_test()
#
#  returned <- convert_to_covid19R_format(input_data)
#
#  expect_equal(returned, expected_data)
#})
