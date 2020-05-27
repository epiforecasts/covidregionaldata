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
  expected_col_names <- c("date", "region", "cases_today", "cumulative_cases", "deaths_today", "cumulative_deaths",
                          "recoveries_today", "cumulative_recoveries", "tests_today", "cumulative_tests", "hospitalisations_today",
                          "cumulative_hospitalisations")

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
  new_name <- switch(tolower(country),
                     "canada" = "province",
                     "afghanistan" = "province",
                     "belgium" = "region")

  data <- data %>% dplyr::rename(!!new_name := region)
  return(data.frame(data))
}

#' Set negative data to 0
#' @description Set data values to 0 if they are negative in a dataset. Data in the datasets should always be > 0.
#' @param data a data.frame
#' @importFrom dplyr %>% mutate
#' @return a data.frame with all relevant data > 0.
set_negative_values_to_zero <- function(data) {
  numeric_col_names <- c('cumulative_deaths', 'cumulative_cases', 'cumulative_recoveries', 'cumulative_hospitalisations', 'cumulative_tests',
                         'cases_today', 'deaths_today', 'recoveries_today', 'hospitalisations_today', 'tests_today')

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
  data <- data %>%
    tidyr::complete(date = tidyr::full_seq(data$date, period = 1), region)
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
  cumulative_col_names <- c('cumulative_deaths', 'cumulative_cases', 'cumulative_recoveries', 'cumulative_hospitalisations', 'cumulative_tests')

  for (cumulative_col_name in cumulative_col_names) {
    if (cumulative_col_name %in% colnames(data)){
      data <- data %>%
                dplyr::group_by(region) %>%
                tidyr::fill(cumulative_col_name)
    }
  }

  return(data.frame(data))
}







