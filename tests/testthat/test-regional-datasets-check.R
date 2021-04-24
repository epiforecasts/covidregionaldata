# load testing function and tools.
# set up custom tests using:
# custom_tests/regional-dataset-specific.R
#source("custom_tests/test-regional-dataset.R")
source("custom_tests/test-get_regional_data_checks.R")

# should a single dataset be tested vs all datasets
# set this when implementing a new dataset.
# Can also be set using environment variables
source_of_interest <- NULL
if (!is.null(getOption("testSource"))) {
  source_of_interest <- getOption("testSource")
}

# get datasets for testing
sources <- get_available_datasets() %>%
  dplyr::filter(.data$type %in%
                  c("regional")) %>%
  dplyr::select(source = class, level_1_region, level_2_region) %>%
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
  dplyr::group_by(source) %>%
  dplyr::summarise(level = max(level)) %>%
  dplyr::rowwise() %>%
  dplyr::group_split() %>%
  purrr::walk(
    ~ test_get_regional_data_region_checks(
      source = .$source[[1]],
      max_level = .$level[[1]]
    )
  )
