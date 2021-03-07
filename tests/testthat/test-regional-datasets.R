source("custom_tests/test-regional-dataset.R")

# test your custom dataset and add a data snapshot using 
# test_regional_dataset(source = "region", level = c("1", "2"), download = TRUE)

# loops over all regional datasets and tests
download <- FALSE
source_of_interest <- NULL
implemented_sources <- c("ecdc", "who", "mexico")

# filter out target datasets for testing
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

# temporary - just test implemented datasets
sources <- sources %>%
    dplyr::filter(source %in% implemented_sources) %>%
    dplyr::rowwise() %>%
    dplyr::group_split()

purrr::walk(sources,
    ~ test_regional_dataset(
        source = .$source[[1]], level = .$level[[1]], download = download
        ))
