#' Daily counts from data that is in cumulative form.
#' @description Gets daily counts from data column that is in cumulative form.
#' @param column A vector of numeric data (e.g. a data column) which corresponds to cumulative counts of a process
#' @return A vector of numeric data which corresponds to daily counts
get_daily_from_cumulative <- function(column) {
  shifted <- c(column[1], diff(column))
  return(shifted)
}

#' Cumulative counts from daily counts.
#' @description Gets cumulative cases/deaths etc. from data which is in daily forms. Similar to cumsum() but deals with NAs by treating them as 0.
#' @param column A vector of numeric data (e.g. a data column) which corresponds to daily counts of a process. Can contain NA
#' @return A vector of numeric data which corresponds to cumulative counts
get_cumulative_from_daily <- function(column) {
  column[which(is.na(column))] <- 0
  cum_sum <- cumsum(column)
  return(cum_sum)
}

#' Add extra columns filled with NA values to a dataset.
#' @description Adds extra columns filled with NAs to a dataset. This ensures that all datasets from the NCovUtils package return datasets
#' of the same underlying structure (i.e. same columns).
#' @param data a data.frame
#' @return a data.frame with relevant NA columns added
add_extra_na_cols <- function(data) {
  expected_col_names <- c("cases_new", "cases_total", "deaths_new", "deaths_total",
                          "recoveries_new", "recoveries_total", "tests_new", "tests_total", "hospitalisations_new",
                          "hospitalisations_total")

  for (colname in expected_col_names) {
    if (!(colname %in% colnames(data))) {
      original_col_names <- colnames(data)
      data$newCol <- rep(NA_integer_, dim(data)[1])
      colnames(data) <- c(original_col_names, colname)
    }
  }
  return(data.frame(data))
}

#' Helper to rename the region column in each dataset to the correct name for each country.
#' @description The package relies on column name 'region' during processing but this often isn't the most sensible name for the column
#' (e.g. state makes more sense for USA). This simply renames the column as the final step in processing before returning data to the user.
#' @param data a data.frame with a region column
#' @param country a string with the country of interest
#' @importFrom dplyr %>% rename
#' @return a data.frame with the column renamed to a sensible name
rename_region_column <- function(data, country) {

  level_1_region_name <- switch(tolower(country),
                               "canada" = "province",
                               "afghanistan" = "province",
                               "belgium" = "region",
                               "brazil" = "state",
                               "germany" = "bundesland",
                               "india" = "state",
                               "italy" = "region")

  data <- data %>% dplyr::rename(!!level_1_region_name := region_level_1)

  if ("region_level_2" %in% colnames(data)) {
    level_2_region_name <- switch(tolower(country),
                                "belgium" = "province",
                                "brazil" = "city")

    data <- data %>% dplyr::rename(!!level_2_region_name := region_level_2)
  }

  return(data.frame(data))
}

#' Set negative data to 0
#' @description Set data values to 0 if they are negative in a dataset. Data in the datasets should always be > 0.
#' @param data a data.frame
#' @importFrom dplyr %>% mutate
#' @return a data.frame with all relevant data > 0.
set_negative_values_to_zero <- function(data) {
  numeric_col_names <- c('deaths_total', 'cases_total', 'recoveries_total', 'hospitalisations_total', 'tests_total',
                         'cases_new', 'deaths_new', 'recoveries_new', 'hospitalisations_new', 'tests_new')

  for (numeric_col_name in numeric_col_names) {
    if (numeric_col_name %in% colnames(data)){
      data <- data %>% dplyr::mutate(!!numeric_col_name := replace(data[, numeric_col_name], data[, numeric_col_name] < 0, 0))
    }
  }

  return(data.frame(data))
}

#' Add rows of NAs for dates where a region does not have any data
#' @description THere are points, particularly early during data collection, where data was not collected for all regions. This function finds dates which have data for some
#' regions, but not all, and adds rows of NAs for the missing regions. This is mainly for reasons of completeness.
#' @param data a data.frame
#' @importFrom dplyr %>%
#' @importFrom tidyr complete full_seq
#' @return a data.frame with rows of NAs added.
fill_empty_dates_with_na <- function(data) {

  if ("region_level_2" %in% colnames(data)) {
    data <- data %>%
      tidyr::complete(date = tidyr::full_seq(data$date, period = 1), tidyr::nesting(region_level_2, region_level_1))
  } else {
    data <- data %>%
      tidyr::complete(date = tidyr::full_seq(data$date, period = 1), region_level_1)
  }

  return(data.frame(data))
}

#' Completes cumulative columns if rows were added with NAs.
#' @description If a dataset had a row of NAs added to it (using fill_empty_dates_with_na) then cumulative data columns will have NAs which can cause
#' issues later. This function fills these values with the previous non-NA value.
#' @param data a data.frame
#' @importFrom dplyr %>% group_by
#' @importFrom tidyr fill
#' @return a data.frame with NAs filled in for cumulative data columns.
complete_cumulative_columns <- function(data) {
  cumulative_col_names <- c('deaths_total', 'cases_total', 'recoveries_total', 'hospitalisations_total', 'tests_total')

  for (cumulative_col_name in cumulative_col_names) {
    if (cumulative_col_name %in% colnames(data)){
      if ("region_level_2" %in% colnames(data)) {
        data <- data %>%
          dplyr::group_by(region_level_1, region_level_2) %>%
          tidyr::fill(cumulative_col_name)
      } else {
        data <- data %>%
          dplyr::group_by(region_level_1) %>%
          tidyr::fill(cumulative_col_name)
      }
    }
  }

  return(data.frame(data))
}


#' Cumulative counts from daily counts or daily counts from cumulative, dependent on which columns already exist
#' @description Checks which columns are missing (cumulative/daily counts) and if one is present and the other not then calculates the second from the first
#' @param data A data frame
#' @return A data frame with extra columns if required
calculate_columns_from_existing_data <- function(data) {
  possible_counts <- c("cases", "deaths", "hospitalisations", "recoveries", "tests")

  for (count in possible_counts) {
    count_today_name <- paste0(count, "_new")
    cumulative_count_name <- paste0(count, "_total")

    if (count_today_name %in% colnames(data) & !(cumulative_count_name %in% colnames(data))) {
      # in this case the daily count is there but there are no cumulative counts
      data <- data %>% dplyr::mutate(!!cumulative_count_name := get_cumulative_from_daily(data[[count_today_name]]))
    } else if (!(count_today_name %in% colnames(data)) & cumulative_count_name %in% colnames(data)) {
      # in this case the cumulative counts are there but no daily counts
      data <- data %>% dplyr::mutate(!!count_today_name := get_daily_from_cumulative(data[[cumulative_count_name]]))
    }
  }

  return(data)
}


#' Convert data to Covid19R package data standard
#' @description Converts wide format (time series) data into long format to meet the Covid19R package standard
#' @param data A data frame / tibble
#' @return A data frame in the Covid19R standard
convert_to_covid19R_format <- function(data) {
  location_type <- colnames(data)[2]

  data <- data %>%
    tidyr::pivot_longer(-c(date, !!location_type, iso_code),  names_to = "data_type", values_to = "value")

  data$location_code_type <- "iso-3166-2"
  data$location_type <- location_type

  data <- data %>%
    dplyr::rename("location_code" = "iso_code",
                  "location" = !!location_type) %>%
    dplyr::select(date,	location,	location_type, location_code, location_code_type,	data_type, value) %>%
    dplyr::arrange(date)

  return(data)
}

#' Custom CSV reading function
#' @description Checks for use of memoise and then uses whichever read_csv function is needed by user
#' @param file A URL or filepath to a CSV
#' @return A data table
#' @importFrom memoise memoise cache_filesystem
#' @importFrom readr read_csv cols
csv_reader <- function(file, ...) {

  read_csv_fun <- readr::read_csv

  if (!is.null(getOption("useMemoise"))) {
    if (getOption("useMemoise")) {
      # Set up cache
      ch <- memoise::cache_filesystem(".cache")
      read_csv_fun <- memoise::memoise(readr::read_csv, cache = ch)
    }
  }

  data <- read_csv_fun(file, col_types = readr::cols(), ...)
  return(data)
}
