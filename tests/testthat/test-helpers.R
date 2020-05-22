test_that("get_daily_from_cumulative returns correct results", {
  column1 <- c(0, 3, 2, 10, 0, 14, 1)
  cumulative_col <- cumsum(column1)
  calculated_daily <- get_daily_from_cumulative(cumulative_col)

  expect_equal(column1, calculated_daily)
})

test_that("add_extra_na_cols is working", {
  extra_col_names <- c("date", "province", "cases_today", "cumulative_cases", "deaths_today", "cumulative_deaths",
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

## Setup the last two
dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03")
regions <- c("Northland", "Eastland", "Wisconsin")

# full data is data with all dates/regions + some NAs in the cases column
full_data <- data.frame(expand.grid(dates, regions))
colnames(full_data) <- c("date", "region")
full_data$date <- as.Date(full_data$date)
full_data$region <- as.character(full_data$region)
full_data <- full_data %>% dplyr::arrange(date, region)
full_data$cases <- c(1:5, rep(NA, 4), 10:12)

# partial data deletes some rows (i.e. gets rid of some dates - all the ones with NA in cases)
partial_data <- full_data[-c(6:9), ]

test_that("fill_empty_dates_with_na fills empty dates with NA", {
  expect_equal(full_data, fill_empty_dates_with_na(partial_data))
})

test_that("complete_cumulative_columns works", {
  # add cumulative cases to partial data and then add NA rows
  partial_data_with_cum_cases_na <- partial_data %>% dplyr::group_by(region) %>% dplyr::mutate(cumulative_cases = cumsum(cases))
  full_data_with_cum_cases_na <- fill_empty_dates_with_na(partial_data_with_cum_cases_na)

  # manually add cumulative cases to get expected data
  full_data_with_cum_cases_filled <- fill_empty_dates_with_na(partial_data)
  full_data_with_cum_cases_filled <- arrange(full_data_with_cum_cases_filled, region, date)
  full_data_with_cum_cases_filled <- cbind(full_data_with_cum_cases_filled, c(1,5,5,15,2,7,7,18,3,3,3,15))
  colnames(full_data_with_cum_cases_filled)[4] <- "cumulative_cases"

  expect_equal(complete_cumulative_columns(full_data_with_cum_cases_na), full_data_with_cum_cases_filled)
})


