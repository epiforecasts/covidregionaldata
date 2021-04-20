source("custom_tests/regional-dataset-specific.R")

expect_clean_cols <- function(data, level) {
  expect_s3_class(data[["date"]], "Date")
  expect_type(data[["level_1_region"]], "character")
  if (level == "2") {
    expect_type(data[["level_2_region"]], "character")
  }
}

expect_processed_cols <- function(data, level, localised = TRUE) {
  expect_s3_class(data[["date"]], "Date")
  expect_type(data[["cases_new"]], "double")
  expect_type(data[["cases_total"]], "double")
  expect_type(data[["deaths_new"]], "double")
  expect_type(data[["deaths_total"]], "double")
  if (!localised) {
    expect_type(data[["level_1_region"]], "character")
    if (level == "2") {
      expect_type(data[["level_2_region"]], "character")
    }
  }
}

expect_columns_contain_data <- function(data_name, region) {
  cols_present <- function(col) {
    if (length(region$source_data_cols[grep(
      col, tolower(region$source_data_cols)
    )]) > 0) {
      return(paste0(col, c("_new", "_total")))
    } else {
      return(NULL)
    }
  }
  cols <- c("cases", "deaths", "recovered", "test")
  cols2check <- purrr::map(cols, cols_present)
  cols2check <- unlist(cols2check)
  purrr::walk(
    cols2check,
    ~ {
      test_that(
        paste0(data_name, "column '", .x, "' is not just composed of NA"),
        {
          expect_true(nrow(region$data$processed %>% filter(!is.na(!!.x))) > 0)
        }
      )
    }
  )
}

test_regional_dataset <- function(source, level, download = FALSE) {
  data_name <- paste0(source, " at level ", level)

  region <- eval(parse(
    text = paste0(
      source, "$new(level = '", level,
      "', verbose = FALSE)"
    )
  ))

  raw_path <- paste0("custom_data/", source, "_level_", level, ".rds")
  if (!file.exists(raw_path)) {
    download <- TRUE
  }

  if (download) {
    test_that(paste0(data_name, " downloads sucessfully"), {
      region$download()
      purrr::walk(region$data$raw, function(data) {
        expect_s3_class(data, "data.frame")
        expect_true(nrow(data) > 0)
        expect_true(ncol(data) >= 2)
      })
    })
    region$data$raw <- purrr::map(region$data$raw,
      dplyr::slice_tail,
      n = 250
    )
    saveRDS(region$data$raw, raw_path)
  } else {
    region$data$raw <- readRDS(raw_path)
  }

  test_that(paste0(data_name, " can be cleaned as expected"), {
    region$clean()
    expect_s3_class(region$data$clean, "data.frame")
    expect_true(nrow(region$data$clean) > 0)
    expect_true(ncol(region$data$clean) >= 2)
    expect_clean_cols(region$data$clean, level = level)
  })

  test_that(paste0(data_name, " can be processed as expected"), {
    region$process()
    expect_s3_class(region$data$processed, "data.frame")
    expect_true(nrow(region$data$processed) > 0)
    expect_true(ncol(region$data$processed) >= 2)
    expect_processed_cols(region$data$processed, level = level)
    if (!source %in% c("ECDC", "WHO")) {
      local_region <- region$clone()
      local_region$localise <- FALSE
      local_region$process()
      expect_processed_cols(local_region$data$processed,
        level = level,
        localised = FALSE
      )
    }
  })

  test_that(paste0(data_name, " can be returned as expected"), {
    returned <- region$return()
    if (any(class(returned) %in% "data.frame")) {
      expect_s3_class(returned, "data.frame")
      expect_true(nrow(returned) > 0)
      expect_true(ncol(returned) >= 2)
    }
  })

  expect_columns_contain_data(data_name, region)

  custom_test <- paste0("test_", source, "_level_", level)
  if (!exists(custom_test)) {
    custom_test <- function(region) {

    }
  }
  do.call(custom_test, list(region = region))
}
