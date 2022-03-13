if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  # load testing function and tools.
  # set up custom tests using:
  # custom_tests/regional-dataset-specific.R # nolint
  source("custom_tests/test-regional-dataset.R")

  # should a single dataset be tested vs all datasets
  # set this when implementing a new dataset.
  # Can also be set using environment variables
  source_of_interest <- NULL
  if (!is.null(getOption("testSource"))) {
    source_of_interest <- getOption("testSource")
  }
  # should downloads be tested (defaults to FALSE)
  # set this to true when implementing a new data set
  # can also be controlled using an environment variable
  download <- FALSE
  if (!is.null(getOption("testDownload"))) {
    download <- getOption("testDownload")
  }

  if (!is.null(source_of_interest)) {
    test_regions <- TRUE
  }else{
    test_regions <- FALSE
  }
  if (!is.null(getOption("testRegions"))) {
    test_regions <- getOption("testRegions")
  }

  if (test_regions) {
    # get datasets for testing
    sources <- get_available_datasets() %>%
      dplyr::filter(.data$type %in%
        c("national", "regional")) %>%
      dplyr::select(
        source = class,
        level_1_region, level_2_region, level_3_region
      ) %>%
      tidyr::pivot_longer(
        cols = -source,
        names_to = "level",
        values_to = "regions"
      ) %>%
      dplyr::mutate(
        level = stringr::str_split(level, "_"),
        level = purrr::map_chr(level, ~ .[2])
      ) %>%
      tidyr::drop_na(regions)

    # filter out target datasets
    if (!is.null(source_of_interest)) {
      sources <- sources %>%
        dplyr::filter(source %in% source_of_interest)
    }

    # apply tests to each data source in turn
    sources %>%
      dplyr::rowwise() %>%
      dplyr::group_split() %>%
      purrr::walk(
        ~ test_regional_dataset(
          source = .$source[[1]],
          level = .$level[[1]],
          download = download
        )
      )
  }
}
