#' Add extra columns filled with NA values to a dataset.
#'
#' @description Adds extra columns filled with NAs to a dataset.
#'  This ensures that all datasets from the covidregionaldata package return
#'  datasets of the same underlying structure (i.e. same columns).
#' @param data A data frame
#' @return A tibble with relevant NA columns added
#' @importFrom tibble tibble
add_extra_na_cols <- function(data) {
  expected_col_names <- c(
    "cases_new", "cases_total", "deaths_new", "deaths_total",
    "recovered_new", "recovered_total", "tested_new", "tested_total",
    "hosp_new",  "hosp_total"
  )

  for (colname in expected_col_names) {
    if (!(colname %in% colnames(data))) {
      original_col_names <- colnames(data)
      data$new_col <- rep(NA_integer_, dim(data)[1])
      colnames(data) <- c(original_col_names, colname)
    }
  }
  return(data)
}

#' Set negative data to 0
#'
#' @description Set data values to 0 if they are negative in a dataset. Data in
#' the datasets should always be > 0.
#' @param data A data frame
#' @return A data frame with all relevant data > 0.
set_negative_values_to_zero <- function(data) {
  numeric_col_names <- c(
    "deaths_total", "cases_total", "recovered_total", "hosp_total",
    "tested_total", "cases_new", "deaths_new", "recovered_new", "hosp_new",
    "tested_new"
  )

  for (numeric_col_name in numeric_col_names) {
    if (numeric_col_name %in% colnames(data)) {
      data[which(data[, numeric_col_name] < 0), numeric_col_name] <- 0
    }
  }
  return(data)
}

#' Add rows of NAs for dates where a region does not have any data
#'
#' @description There are points, particularly early during data collection,
#'  where data was not collected for all regions. This function finds dates
#'  which have data for some regions, but not all, and adds rows of NAs for the
#'  missing regions. This is mainly for reasons of completeness.
#' @param data A data frame
#' @return A tibble with rows of NAs added.
#' @importFrom tibble tibble
#' @importFrom tidyr complete full_seq nesting
fill_empty_dates_with_na <- function(data) {
  if ("region_level_2" %in% colnames(data)) {
    data <- data %>%
      complete(
        date = full_seq(data$date, period = 1),
        nesting(
            region_level_2, level_2_region_code,
            region_level_1, level_1_region_code
            )
        )
  } else {
    data <- data %>%
      complete(
        date = full_seq(data$date, period = 1),
        nesting(region_level_1, level_1_region_code)
    )
  }
  return(data)
}

#' Completes cumulative columns if rows were added with NAs.
#'
#' @description If a dataset had a row of NAs added to it (using
#'  fill_empty_dates_with_na) then cumulative data columns will have NAs which
#'  can cause issues later. This function fills these values with the previous
#'  non-NA value.
#' @param data A data frame
#' @return A data tibble with NAs filled in for cumulative data columns.
#' @importFrom dplyr group_by
#' @importFrom tidyr fill
#' @importFrom tidyselect all_of
complete_cumulative_columns <- function(data) {
  cumulative_col_names <- c("deaths_total", "cases_total", "recovered_total",
                            "hosp_total", "tested_total")
  for (cumulative_col_name in cumulative_col_names) {
    if (cumulative_col_name %in% colnames(data)) {
        data <- fill(data, all_of(cumulative_col_name))
    }
  }
  return(data)
}

#' Cumulative counts from daily counts or daily counts from cumulative,
#' dependent on which columns already exist
#'
#' @description Checks which columns are missing (cumulative/daily counts) and
#' if one is present and the other not then calculates the second from the
#'  first.
#' @param data A data frame
#' @return A data frame with extra columns if required
#' @importFrom dplyr mutate group_by_at arrange vars starts_with lag
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @importFrom rlang !! :=
calculate_columns_from_existing_data <- function(data) {
  possible_counts <- c("cases", "deaths", "hosp", "recovered", "tested")

  for (count in possible_counts) {
    count_today_name <- paste0(count, "_new")
    cumulative_count_name <- paste0(count, "_total")

    if (count_today_name %in% colnames(data) &
          !(cumulative_count_name %in% colnames(data))) {
      # in this case the daily count is there but there are no cumulative counts
      data <- data %>%
        group_by_at(vars(starts_with("region_level"))) %>%
        arrange(date, .by_group = TRUE) %>%
        mutate(
            !!cumulative_count_name :=
                cumsum(replace_na(!!as.name(count_today_name), 0))
              )
    }else if (!(count_today_name %in% colnames(data)) &
                 cumulative_count_name %in% colnames(data)) {
      # in this case the cumulative counts are there but no daily counts
      data <- data %>%
        group_by_at(vars(starts_with("region_level"))) %>%
        arrange(date, .by_group = TRUE) %>%
        fill(!!cumulative_count_name) %>% # Fill LOCF for cumulative data
        mutate(
            !!count_today_name :=
                 (!!as.name(cumulative_count_name)) -
                     lag(!!as.name(cumulative_count_name), default = 0))
    }
  }
  return(data)
}

#' Get totals data given the time series data.
#'
#' @description Get totals data given the time series data.
#' @param data A data table
#' @return A data table, totalled up
#' @importFrom dplyr left_join group_by summarise select arrange
#' @importFrom tibble tibble
#'
totalise_data <- function(data) {
  data <- data %>%
    summarise(
      cases_total = sum(.data$cases_new, na.rm = TRUE),
      deaths_total = sum(.data$deaths_new, na.rm = TRUE),
      recovered_total = sum(.data$recovered_new, na.rm = TRUE),
      hosp_total = sum(.data$hosp_new, na.rm = TRUE),
      tested_total = sum(.data$tested_new, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(-cases_total)
  return(data)
}

#' Internal Shared Regional Dataset Processing
#'
#' @description Internal shared regional data cleaning designed to be called
#' by `process_regional`.
#' @param group_vars A character vector of grouping variables. It is assumed
#' that the first entry indicates the main region variable and the second
#' indicates the code for this variable.
#' @inheritParams process_regional
#' @author Sam Abbott
#' @importFrom dplyr do group_by_at ungroup select everything arrange rename
#' @importFrom tidyr drop_na
#' @importFrom tidyselect all_of
#' @importFrom rlang !! :=
process_regional_internal <- function(region, group_vars,
                                     totals = FALSE, localise = FALSE,
                                     verbose = TRUE) {
  dat <- group_by_at(region$clean, .vars = group_vars)

  . <- NULL
  dat <- dat %>%
    do(calculate_columns_from_existing_data(.)) %>%
    add_extra_na_cols() %>%
    set_negative_values_to_zero()

  if (totals) {
    dat <- totalise_data(dat)
    dat <- dat %>%
      select(all_of(
        c(group_vars, "cases_total", "deaths_total", "recovered_total",
          "hosp_total", "tested_total")
      )
    )
  }else {
    dat <- dat %>%
      drop_na(.data$date) %>%
      fill_empty_dates_with_na() %>%
      complete_cumulative_columns() %>%
      select(all_of(c("date", group_vars, "cases_new", "cases_total",
       "deaths_new", "deaths_total", "recovered_new", "recovered_total",
       "hosp_new", "hosp_total", "tested_new", "tested_total")),
        everything()) %>%
      arrange(.data$date, all_of(group_vars[1]))
  }

  dat <- ungroup(dat)

  if (localise) {
    dat <- rename(dat, !!region$level := !!group_vars[1])
  }
  dat <- rename(dat, !!region$code := !!group_vars[2])

  region$processed <- dat
  return(region)
}