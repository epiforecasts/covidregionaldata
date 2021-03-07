# load testing function and tools.
# set up custom tests using:
# custom_tests/regional-dataset-specific.R
source("custom_tests/test-regional-dataset.R")

# sources implemented currently (temporary)
implemented_sources <- c("ecdc", "who", "mexico", "italy")

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

# get datasets for testing
sources <- covidregionaldata::available_datasets %>%
    filter(.data$get_data_function %in%
           c("get_regional_data", "get_national_data")) %>%
    dplyr::select(source = country, level_1_region, level_2_region) %>%
    tidyr::pivot_longer(
        cols = - source,
        names_to = "level",
        values_to = "regions") %>%
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

# temporary - just test implemented datasets
sources <- sources %>%
    dplyr::filter(source %in% implemented_sources) %>%
    dplyr::rowwise() %>%
    dplyr::group_split()

# apply tests to each data source in turn
purrr::walk(sources,
    ~ test_regional_dataset(
        source = .$source[[1]], level = .$level[[1]], download = download
        ))
