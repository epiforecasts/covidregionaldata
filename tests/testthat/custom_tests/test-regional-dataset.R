source("custom_tests/regional-dataset-specific.R")

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

test_regional_dataset <- function(source, level, download = FALSE) {
    data_name <- paste0(source, " at level ", level)

    test_that(paste0(data_name, " can be defined as a class"), {
        expect_error(
            new_covidregionaldata(source, level = level, verbose = FALSE),
            NA
        )
    })
    region <- new_covidregionaldata(source, level = level, verbose = FALSE)

    if (download) {
        region <- download_regional(region, verbose = FALSE)
        test_that(paste0(data_name, " downloads sucessfully"), {
            expect_s3_class(region$raw, "data.frame")
            expect_true(nrow(region$raw) > 0)
            expect_true(ncol(region$raw) >= 2)
        })
        region$raw <- dplyr::slice_tail(region$raw, n = 1000)
        saveRDS(region,
                paste0("custom_data/", source, "_level_", level, ".rds"))
    }else{
        region <- readRDS(
            paste0("custom_data/", source, "_level_", level, ".rds")
        )
    }

    region <- clean_regional(region, verbose = FALSE)
    test_that(paste0(data_name, " can be cleaned as expected"), {
        expect_s3_class(region$clean, "data.frame")
        expect_true(nrow(region$clean) > 0)
        expect_true(ncol(region$clean) >= 2)
        expect_clean_cols(region$clean, level = level)
    })

    region <- process_regional(region, verbose = FALSE, localise = TRUE)
    test_that(paste0(data_name, " can be processed as expected"), {
        region <- process_regional(region, verbose = FALSE, localise = FALSE)
        expect_s3_class(region$processed, "data.frame")
        expect_true(nrow(region$processed) > 0)
        expect_true(ncol(region$processed) >= 2)
        expect_processed_cols(region$processed, level = level)
        if (!is.null(region$level)) {
            local_region <- process_regional(region, verbose = FALSE,
                                            localise = TRUE)
            expect_true(!is.null(local_region$processed[[region$level]]))
        }
    })

    region <- return_regional(region, steps = TRUE)
    test_that(paste0(data_name, " can be returned as expected"), {
        if (!any(class(region$return) %in% "data.frame")) {
            expect_s3_class(region$return, "data.frame")
            expect_true(nrow(region$return) > 0)
            expect_true(ncol(region$return) >= 2)
            expect_processed_cols(region$return, level = level)
        }
    })

    custom_test <- paste0("test_", source, "_level_", level)
    if (exists(custom_test)) {
        do.call(custom_test, list())
    }
}