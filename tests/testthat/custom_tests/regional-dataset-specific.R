test_ecdc_level_1 <- function(region) {
  test_that("ecdc data has expected format", {
    necessary_cols <- c(
      "geoId", "countriesAndTerritories",
      "cases", "deaths", "popData2019"
    )
    expect_true(all(necessary_cols %in% colnames(region$data$raw)))
    all_countries <- region$data$return %>%
      dplyr::filter(is.na(un_region)) %>%
      dplyr::group_by(country) %>%
      dplyr::tally()
    expect_true(nrow(all_countries) == 0)
  })
}

test_who_level_1 <- function(region) {
  test_that("who data has expected format", {
    all_countries <- region$data$return %>%
      dplyr::filter(is.na(un_region)) %>%
      dplyr::group_by(country) %>%
      dplyr::tally()
    expect_true(nrow(all_countries) == 0)
  })
}

test_UK_level_1 <- function(region) {
  data_name <- "UK level 1 with 'nhsregions=TRUE'"
  region$nhsregions <- TRUE
  source <- class(region)[1]
  nhs_included_path <- paste0(
    "custom_data/", source,
    "_level_", region$level, "_nhs", ".rds"
  )
  if (!file.exists(nhs_included_path)) {
    download <- TRUE
  } else {
    download <- FALSE
  }
  if (download) {
    test_that(paste(data_name, " downloads sucessfully"), { # nolint
      region$download()
      expect_s3_class(region$data$raw$nhs, "data.frame")
      expect_true(nrow(region$data$raw$nhs) > 0)
      expect_true(ncol(region$data$raw$nhs) >= 2)
    })
    region$nhs_raw <- dplyr::slice_tail(region$data$raw$nhs, n = 1000)
    saveRDS(region$data$raw$nhs, nhs_included_path)
  } else {
    region$data$raw$nhs <- readRDS(nhs_included_path)
  }
  test_that(paste(data_name, "can be cleaned as expected"), {
    region$clean()
    expect_s3_class(region$data$clean, "data.frame")
    expect_true(nrow(region$data$clean) > 0)
    expect_true(ncol(region$data$clean) >= 2)
    expect_clean_cols(region$data$clean, level = region$level)
  })
  test_that(paste(data_name, "can be processed as expected"), {
    region$process()
    expect_s3_class(region$data$processed, "data.frame")
    expect_true(nrow(region$data$processed) > 0)
    expect_true(ncol(region$data$processed) >= 2)
    expect_processed_cols(region$data$processed, level = region$level)
  })
  test_that(paste(data_name, "can be returned as expected"), {
    returned <- region$return()
    if (any(class(returned) %in% "data.frame")) {
      expect_s3_class(returned, "data.frame")
      expect_true(nrow(returned) > 0)
      expect_true(ncol(returned) >= 2)
    }
  })
}

# Add data set specific custom tests here in the format of above using
# variables from test_regional_dataset() environment
