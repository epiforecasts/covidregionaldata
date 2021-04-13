# Load mock data functions
source("custom_tests/mock_data.R")

test_that("calculate_columns_from_existing_data returns correct results", {
  input_data <- tibble::tibble(
    "date" = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-07"), by = 1),
    "level_1_region" = c(rep("A", 4), rep("B", 3)),
    "cases_new" = c(0, 1, NA_integer_, 1, 1, 1, 1),
    "deaths_total" = c(0, 1, 2, 3, 1, NA_integer_, 2)
  )

  calculated_data <- calculate_columns_from_existing_data(input_data)
  cases_total <- c(0, 1, 1, 2, 1, 2, 3)
  deaths_new <- c(0, 1, 1, 1, 1, 0, 1)

  expect_equal(calculated_data$cases_total, cases_total)
  expect_equal(calculated_data$deaths_new, deaths_new)
})

test_that("add_extra_na_cols is working", {
  extra_col_names <- c(
    "cases_new", "cases_total", "deaths_new", "deaths_total",
    "recovered_new", "recovered_total", "tested_new", "tested_total",
    "hosp_new", "hosp_total"
  )

  existing_col_names <- colnames(mtcars)
  new_dataset <- add_extra_na_cols(mtcars)

  expect_equal(c(existing_col_names, extra_col_names), colnames(new_dataset))
  expect_true(all(is.na(new_dataset$cases_new)))
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
  # partial data deletes some rows (i.e. gets rid of some dates - all the ones
  # with NA in cases)
  partial_data <- expected_data[-c(6:9), ]
  expect_equal(fill_empty_dates_with_na(partial_data), expected_data)
  expected_data <- dplyr::mutate(
    expected_data,
    level_2_region = level_1_region,
    level_2_region_code = level_1_region_code
  ) %>%
    select(
      date, level_2_region, level_2_region_code,
      level_1_region, level_1_region_code, cases
    )
  partial_data <- expected_data[-c(6:9), ]
  expect_equal(fill_empty_dates_with_na(partial_data), expected_data)
})

test_that("complete_cumulative_columns works", {
  input_data <- get_input_data_for_complete_cumulative_columns_test()
  expected_data <- get_expected_data_for_complete_cumulative_columns_test()
  actual_data <- complete_cumulative_columns(input_data)
  expect_equal(colnames(actual_data), colnames(expected_data))
  expect_true(!any(is.na(actual_data$cases_total)))
})

test_process_regional <- function(level = "1") {
  test_that(paste0("process_regional.crd_level_", level, " works"), {
    mexico <- readRDS(paste0("custom_data/mexico_level_", level, "_snap.rds"))
    expect <- mexico$data$processed
    mexico$process()
    expect_equal(expect, mexico$data$processed)
    mexico$localise <- FALSE
    mexico$process()
    reproc_local <- mexico$data$processed
    expect_true(any(colnames(reproc_local) %in% paste0("level_", level, "_region")))
  })
}

test_process_regional(level = "1")
test_process_regional(level = "2")
