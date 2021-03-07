expect_clean_cols <- function(data, level) {
  expect_s3_class(data[["date"]], "Date")
  expect_type(data[["cases_new"]], "double")
  expect_type(data[["region_level_1"]], "character")
  if (level == "2") {
    expect_type(data[["region_level_2"]], "character")
  }
}

expect_processed_cols <- function(data, level) {
  expect_s3_class(data[["date"]], "Date")
  expect_type(data[["cases_new"]], "double")
  expect_type(data[["cases_total"]], "double")
  expect_type(data[["deaths_new"]], "double")
  expect_type(data[["deaths_total"]], "double")
  expect_type(data[["region_level_1"]], "character")
  if (level == "2") {
    expect_type(data[["region_level_2"]], "character")
  }
}