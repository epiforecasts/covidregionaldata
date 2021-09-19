#' Test clean columns contain the correct data and types
#' @description Checks the date column is an s3 class and that region level
#' column is a character in the cleaned data (data$clean)
#' @param data The clean data to check
#' @param level character_array the level of the data to check
#' @family tests
#' @concept tests
#' @export
expect_clean_cols <- function(data, level) {
  testthat::expect_s3_class(data[["date"]], "Date")
  level_region_str <- paste0("level_", level, "_region")
  testthat::expect_type(data[[level_region_str]], "character")
}

#' Test that processed columns contain the correct data and types
#' @description Checks that processed data columns date, cases_new, cases_total,
#' deaths_new, deaths_total and that region level have the correct types.
#' @param data The data to check
#' @param level character_array the level of the data to check
#' @param localised logical to check localised data or not, defaults to
#' TRUE.
#' @family tests
#' @concept tests
#' @export
expect_processed_cols <- function(data, level = "1", localised = TRUE) {
  testthat::expect_s3_class(data[["date"]], "Date")
  testthat::expect_type(data[["cases_new"]], "double")
  testthat::expect_type(data[["cases_total"]], "double")
  testthat::expect_type(data[["deaths_new"]], "double")
  testthat::expect_type(data[["deaths_total"]], "double")
  if (!localised) {
    level_region_str <- paste0("level_", level, "_region")
    testthat::expect_type(data[[level_region_str]], "character")
  }
}

#' Test that cleaned columns contain data/
#' @description Checks that cleaned columns cases, deaths, recovered and test
#' (new and total) are not entirely composed of NAs.
#' @param DataClass_obj The DataClass object (R6Class) to perform checks on.
#' Must be a `DataClass` or `DataClass` child object.
#' @importFrom purrr map walk
#' @importFrom dplyr filter
#' @importFrom rlang !!
#' @family tests
#' @concept tests
#' @export
expect_columns_contain_data <- function(DataClass_obj) {
  cols_present <- function(col) {
    if (length(DataClass_obj$source_data_cols[grep(
      col, tolower(DataClass_obj$source_data_cols)
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
        paste0(
          DataClass_obj$data_name, "column '",
          .x, "' is not just composed of NA"
        ),
        {
          testthat::expect_true(
            nrow(DataClass_obj$data$processed %>% filter(!is.na(!!.x))) > 0
          )
        }
      )
    }
  )
}

#' Test download method works correctly
#' @description Test data can be downloaded if `download = TRUE`, or a requested
#' snapshot file is not found, and store a snap shot in the `snapshot_dir`. If
#' an existing snapshot file is found then load this data to use in future tests
#' @param DataClass_obj The R6Class object to perform checks on.
#' Must be a `DataClass` or `DataClass` child object.
#' @param download Logical check to download or use a snapshot of the data
#' @param snapshot_path character_array the path to save the downloaded
#' snapshot to.
#' @importFrom purrr map walk
#' @importFrom dplyr slice_tail
#' @family tests
#' @concept tests
#' @export
test_download <- function(DataClass_obj, download, snapshot_path) {
  if (!file.exists(snapshot_path)) {
    download <- TRUE
  }
  if (download) {
    testthat::test_that(
      paste0(DataClass_obj$data_name, " downloads sucessfully"),
      {
        DataClass_obj$download()
        walk(DataClass_obj$data$raw, function(data) {
          testthat::expect_s3_class(data, "data.frame")
          testthat::expect_true(nrow(data) > 0)
          testthat::expect_true(ncol(data) >= 2
                                || typeof(data[[1]]) == "list")
        })
      }
    )
    DataClass_obj$data$raw <- map(DataClass_obj$data$raw,
      slice_tail,
      n = 250
    )
    DataClass_obj$data$raw <- map(
      DataClass_obj$data$raw,
      ~ .[, 1:min(100, ncol(.))]
    )
    saveRDS(DataClass_obj$data$raw, snapshot_path)
  } else {
    DataClass_obj$data$raw <- readRDS(snapshot_path)
  }
}

#' Test clean method works correctly
#' @description Test data can be cleaned properly. The clean method is invoked
#' to generate clean data. This data is checked to ensure it is a data.frame,
#' is not empty, has at least two columns and that columns are clean by calling
#' `expect_clean_cols`. Also tests that `avaliable_regions()` are not NA and
#' they are all characters.
#' @inheritParams test_download
#' @family tests
#' @concept tests
#' @export
test_cleaning <- function(DataClass_obj) {
  testthat::test_that(
    paste0(DataClass_obj$data_name, " can be cleaned as expected"),
    {
      DataClass_obj$clean()
      testthat::expect_s3_class(DataClass_obj$data$clean, "data.frame")
      testthat::expect_true(nrow(DataClass_obj$data$clean) > 0)
      testthat::expect_true(ncol(DataClass_obj$data$clean) >= 2)
      expect_clean_cols(DataClass_obj$data$clean, DataClass_obj$level)
    }
  )
  testthat::test_that(
    paste(DataClass_obj$data_name, "highlights available regions as expected"),
    {
      testthat::expect_error(DataClass_obj$available_regions(), NA)
      testthat::expect_true(
        class(DataClass_obj$available_regions()) %in% "character"
      )
    }
  )
}

#' Test process method works correctly
#' @description Test data can be processed correctly using the process method.
#' process is invoked to generate processed data which is then checked to ensure
#' it is a data.frame, which is not empty, has at least 2 columns and calls
#' `expect_processed_columns` to check each column types.
#' @inheritParams test_download
#' @param all Logical. Run tests with all settings (TRUE) or with those
#' defined in the current class instance (FALSE). Defaults to FALSE.
#' @family tests
#' @concept tests
#' @export
test_processing <- function(DataClass_obj, all = FALSE) {
  testthat::test_that(
    paste0(DataClass_obj$data_name, " can be processed as expected"),
    {
      DataClass_obj$process()
      testthat::expect_s3_class(DataClass_obj$data$processed, "data.frame")
      testthat::expect_true(nrow(DataClass_obj$data$processed) > 0)
      testthat::expect_true(ncol(DataClass_obj$data$processed) >= 2)
      expect_processed_cols(
        DataClass_obj$data$processed,
        level = DataClass_obj$level,
        localised = DataClass_obj$localise
      )
      if (all) {
        local_region <- DataClass_obj$clone()
        local_region$localise <- FALSE
        local_region$process()
        expect_processed_cols(
          local_region$data$processed,
          level = local_region$level,
          localised = local_region$localise
        )
      }
    }
  )
}

#' Test return method works correctly
#' @description Test data can be returned correctly using the return method.
#' return is invoked to generate returned data which is then checked to ensure
#' it is a data.frame, not empty and has at least 2 columns. Each column is then
#' checked to ensure it contains data and is not just composed of NAs.
#' @inheritParams test_download
#' @family tests
#' @concept tests
#' @export
test_return <- function(DataClass_obj) {
  testthat::test_that(
    paste0(DataClass_obj$data_name, " can be returned as expected"),
    {
      returned <- DataClass_obj$return()
      if (any(class(returned) %in% "data.frame")) {
        testthat::expect_s3_class(returned, "data.frame")
        testthat::expect_true(nrow(returned) > 0)
        testthat::expect_true(ncol(returned) >= 2)
      }
    }
  )
  expect_columns_contain_data(DataClass_obj)
}
