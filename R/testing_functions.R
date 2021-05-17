#' Test clean columns contain the correct data and types
#' @description Expect data has cleaned columns. Inherited by child
#' classes so tests run through each class.
#' @param data The data to check
#' @param level character_array the level of the data to check
expect_clean_cols <- function(data, level) {
  testthat::expect_s3_class(data[["date"]], "Date")
  testthat::expect_type(data[["level_1_region"]], "character")
  if (level == "2") {
    testthat::expect_type(data[["level_2_region"]], "character")
  }
}

#' Test that processed columns contain the correct data types
#' @description Expect data has processed columns. Inherited by child
#' classes so tests run through each class.
#' @param data The data to check
#' @param level character_array the level of the data to check
#' @param localised logical to check localised data or not, defaults to
#' TRUE.
expect_processed_cols <- function(data, level = "1", localised = TRUE) {
  testthat::expect_s3_class(data[["date"]], "Date")
  testthat::expect_type(data[["cases_new"]], "double")
  testthat::expect_type(data[["cases_total"]], "double")
  testthat::expect_type(data[["deaths_new"]], "double")
  testthat::expect_type(data[["deaths_total"]], "double")
  if (!localised) {
    testthat::expect_type(data[["level_1_region"]], "character")
    if (level == "2") {
      testthat::expect_type(data[["level_2_region"]], "character")
    }
  }
}

#' Test that cleaned columns contain data and are the correct types
#' @description Expect data has cleaned columns. Inherited by child
#' classes so tests run through each class.
#' @param self The DataClass object (R6Class) object to perform checks on.
#' @param data_name character_array The name of the class and level to
#' check
#' @importFrom purrr map walk
#' @importFrom dplyr filter
#' @importFrom rlang !!
expect_columns_contain_data <- function(self, data_name) {
  cols_present <- function(col) {
    if (length(self$source_data_cols[grep(
      col, tolower(self$source_data_cols)
    )]) > 0) {
      return(paste0(col, c("_new", "_total")))
    } else {
      return(NULL)
    }
  }
  cols <- c("cases", "deaths", "recovered", "test")
  cols2check <- map(cols, cols_present)
  cols2check <- unlist(cols2check)
  walk(
    cols2check,
    ~ {
      testthat::test_that(
        paste0(data_name, "column '", .x, "' is not just composed of NA"),
        {
          testthat::expect_true(
            nrow(self$data$processed %>% filter(!is.na(!!.x))) > 0
          )
        }
      )
    }
  )
}

#' Test download method works correctly
#' @description Test data can be downloaded if download = TRUE, or a requested
#' snapshot file is not found, and store a snap shot at the snapshot_path. If an
#' existing snapshot file is found then just load this data to use in future
#' tests.
#' @param self The DataClass object (R6Class) object to perform checks on.
#' @param download logical check to download or use a snapshot of the data
#' @param data_name character_array The name of the class and level to
#' check
#' @param snapshot_path character_array the path to save the downloaded
#' snapshot to.
#' @importFrom purrr map walk
#' @importFrom dplyr slice_tail
test_download <- function(self, download, data_name, snapshot_path) {
  if (!(nchar(snapshot_path))) {
    snapshot_path <- paste0(
      "custom_data/", class(self)[1],
      "_level_", self$level, ".rds"
    )
  }
  if (!file.exists(snapshot_path)) {
    download <- TRUE
  }
  if (download) {
    testthat::test_that(paste0(data_name, " downloads sucessfully"), {
      self$download()
      walk(self$data$raw, function(data) {
        testthat::expect_s3_class(data, "data.frame")
        testthat::expect_true(nrow(data) > 0)
        testthat::expect_true(ncol(data) >= 2)
      })
    })
    self$data$raw <- map(self$data$raw,
      slice_tail,
      n = 250
    )
    self$data$raw <- map(
      self$data$raw,
      ~ .[, 1:min(100, ncol(.))]
    )
    saveRDS(self$data$raw, snapshot_path)
  } else {
    self$data$raw <- readRDS(snapshot_path)
  }
}

#' Test clean method works correctly
#' @description Test data can be cleaned correctly using the clean method
#' @inheritParams test_download
test_cleaning <- function(self, data_name) {
  testthat::test_that(paste0(data_name, " can be cleaned as expected"), {
    self$clean()
    testthat::expect_s3_class(self$data$clean, "data.frame")
    testthat::expect_true(nrow(self$data$clean) > 0)
    testthat::expect_true(ncol(self$data$clean) >= 2)
    expect_clean_cols(self$data$clean, self$level)
  })
  testthat::test_that(
    paste0(data_name, " can highlight available regions as expected"),
    {
      testthat::expect_error(self$available_regions(), NA)
      testthat::expect_true(class(self$available_regions()) %in% "character")
    }
  )
}

#' Test process method works correctly
#' @description Test data can be processed correctly using the process method
#' @inheritParams test_download
#' @param localise Logical, defaults to TRUE. Should region names be
#' localised for tests.
test_processing <- function(self, data_name, localise = TRUE) {
  testthat::test_that(paste0(data_name, " can be processed as expected"), {
    self$process()
    testthat::expect_s3_class(self$data$processed, "data.frame")
    testthat::expect_true(nrow(self$data$processed) > 0)
    testthat::expect_true(ncol(self$data$processed) >= 2)
    expect_processed_cols(self$data$processed, level = self$level)

    local_region <- self$clone()
    local_region$localise <- FALSE
    local_region$process()
    expect_processed_cols(
      local_region$data$processed,
      level = self$level,
      localised = FALSE
    )
  })
}

#' Test return method works correctly
#' @description Test data can be returned correctly using the return method
#' @inheritParams test_download
test_return <- function(self, data_name) {
  testthat::test_that(paste0(data_name, " can be returned as expected"), {
    returned <- self$return()
    if (any(class(returned) %in% "data.frame")) {
      testthat::expect_s3_class(returned, "data.frame")
      testthat::expect_true(nrow(returned) > 0)
      testthat::expect_true(ncol(returned) >= 2)
    }
  })
  expect_columns_contain_data(self, data_name)
}
