#' Add extra columns filled with NA values to a dataset.
#'
#' @description Adds extra columns filled with NAs to a dataset.
#'  This ensures that all datasets from the covidregionaldata package return
#'  datasets of the same underlying structure (i.e. same columns).
#' @param data A data frame
#' @return A tibble with relevant NA columns added
#' @family compulsory_processing
#' @concept compulsory_processing
#' @importFrom dplyr mutate
#' @importFrom rlang !!!
add_extra_na_cols <- function(data) {
  expected_col_names <- c(
    "cases_new", "cases_total", "deaths_new", "deaths_total", "recovered_new",
    "recovered_total", "tested_new", "tested_total", "hosp_new", "hosp_total"
  )

  new_cols <- rep(list(NA_real_), length(expected_col_names))
  names(new_cols) <- expected_col_names
  data <- mutate(
    data,
    !!!new_cols[!(names(new_cols) %in% names(data))]
  )
  return(data)
}

#' Set negative data to 0
#'
#' @description Set data values to 0 if they are negative in a dataset. Data in
#' the datasets should always be > 0.
#' @param data A data frame
#' @return A data frame with all relevant data > 0.
#' @family optional_processing
#' @concept optional_processing
#' @importFrom dplyr mutate_if
set_negative_values_to_zero <- function(data) {
  data <- suppressMessages(
    mutate_if(data, is.numeric, ~ replace(., . < 0, 0))
  )
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
#' @family compulsory_processing
#' @concept compulsory_processing
#' @importFrom tidyr complete full_seq nesting
#' @importFrom dplyr starts_with group_by ungroup group_vars
#' @importFrom rlang !!! syms
fill_empty_dates_with_na <- function(data) {
  regions <- select(data, starts_with("level_")) %>%
    names()

  groups <- group_vars(data)

  data <- data %>%
    ungroup() %>%
    complete(
      date = full_seq(data$date, period = 1),
      nesting(!!!syms(regions))
    ) %>%
    group_by(across(.cols = all_of(groups)))
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
#' @family compulsory_processing
#' @concept compulsory_processing
#' @importFrom tidyr fill
#' @importFrom dplyr all_of
complete_cumulative_columns <- function(data) {
  cumulative_col_names <- c(
    "deaths_total", "cases_total", "recovered_total",
    "hosp_total", "tested_total"
  )
  idx <- cumulative_col_names %in% colnames(data)
  targets <- cumulative_col_names[idx]
  data <- fill(data, all_of(targets))
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
#' @family compulsory_processing
#' @concept compulsory_processing
#' @importFrom dplyr mutate group_by_at arrange vars starts_with lag ends_with
#' @importFrom purrr walk2
#' @importFrom tidyr replace_na
#' @importFrom rlang !! :=
calculate_columns_from_existing_data <- function(data) {
  possible_counts <- c("cases", "deaths", "hosp", "recovered", "tested")
  count_today_name <- paste0(possible_counts, "_new")
  cumulative_count_name <- paste0(possible_counts, "_total")
  idx_1 <- (count_today_name %in% colnames(data) &
    !(cumulative_count_name %in% colnames(data)))
  idx_2 <- (!(count_today_name %in% colnames(data)) &
    cumulative_count_name %in% colnames(data))
  data <- data %>%
    group_by_at(vars(ends_with("_region"))) %>%
    arrange(date, .by_group = TRUE)

  # if new is avaliable but total is not, calculate totals
  walk2(
    count_today_name[idx_1], cumulative_count_name[idx_1],
    ~ {
      data <<- data %>%
        mutate(
          !!.y := cumsum(replace_na(!!as.name(.x), 0))
        )
    }
  )

  # if total is avaliable but new is not, calculate new
  walk2(
    count_today_name[idx_2], cumulative_count_name[idx_2],
    ~ {
      data <<- data %>%
        fill(!!.y) %>%
        mutate(
          !!.x :=
            (!!as.name(.y)) -
            lag(!!as.name(.y), default = 0)
        )
    }
  )
  return(data)
}

#' Get totals data given the time series data.
#'
#' @description Get totals data given the time series data.
#' @param data A data table
#' @return A data table, totalled up
#' @importFrom dplyr left_join group_by summarise select arrange
#' @family optional_processing
#' @concept optional_processing
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
    arrange(-.data$cases_total)
  return(data)
}

#' Default processing steps to run
#' @description The default processing steps to which are always run. Runs on
#' clean data
#' @param data A data table
#' @importFrom dplyr do
#' @concept utility
#' @family processing
run_default_processing_fns <- function(data) {
  . <- NULL
  data <- data %>%
    do(calculate_columns_from_existing_data(.)) %>%
    add_extra_na_cols()
  return(data)
}

#' Optional processing steps to run
#' @description user supplied processing steps which are run after default steps
#' @param data A data table
#' @inheritParams process_internal
#' @concept utility
#' @family processing
run_optional_processing_fns <- function(data, process_fns) {
  if (!missing(process_fns)) {
    if (!is.null(process_fns)) {
      if (!is.na(process_fns)) {
        for (i in seq_along(process_fns)) {
          data <- process_fns[[i]](data)
        }
      }
    }
  }

  return(data)
}

#' Internal Shared Regional Dataset Processing
#'
#' @description Internal shared regional data cleaning designed to be called
#' by `process`.
#' @param clean_data The clean data for a class, e.g. `Italy$data$clean`
#' @param level The level of the data, e.g. 'level_1_region'
#' @param group_vars Grouping variables, used to
#' for grouping and to localise names. It is assumed that the first entry
#' indicates the main region variable and the second indicates the geocode for
#' this variable.
#' @param totals Logical, defaults to `FALSE`. If `TRUE``, returns totalled
#'  data per region up to today's date. If FALSE, returns the full dataset
#'  stratified by date and region.
#' @param localise Logical, defaults to `TRUE`. Should region names be
#' localised.
#' @param verbose Logical, defaults to `TRUE`. Should verbose processing
#' messages and warnings be returned.
#' @param process_fns array, additional functions to be called after default
#' processing steps
#' @concept utility
#' @family processing
#' @importFrom dplyr do group_by_at across ungroup select everything arrange
#' @importFrom dplyr rename all_of
#' @importFrom tidyr drop_na
#' @importFrom rlang !!!
process_internal <- function(clean_data, level, group_vars,
                             totals = FALSE, localise = TRUE,
                             verbose = TRUE,
                             process_fns) {
  if (!any(class(clean_data) %in% "data.frame")) {
    stop("No regional data found to process")
  }
  group_vars_standard <- names(group_vars)

  dat <- group_by(clean_data, across(.cols = all_of(group_vars_standard)))

  dat <- run_default_processing_fns(dat)
  dat <- run_optional_processing_fns(dat, process_fns)

  if (totals) {
    dat <- totalise_data(dat)
    dat <- dat %>%
      select(all_of(
        c(
          group_vars_standard, "cases_total", "deaths_total", "recovered_total",
          "hosp_total", "tested_total"
        )
      ))
  } else {
    dat <- dat %>%
      drop_na(.data$date) %>%
      fill_empty_dates_with_na() %>%
      complete_cumulative_columns() %>%
      select(
        all_of(c(
          "date", group_vars_standard, "cases_new", "cases_total",
          "deaths_new", "deaths_total", "recovered_new", "recovered_total",
          "hosp_new", "hosp_total", "tested_new", "tested_total"
        )),
        everything()
      )
  }

  dat <- ungroup(dat)

  if (localise) {
    old <- names(group_vars)
    names(old) <- group_vars
    dat <- rename(dat, !!!old)
  }

  return(dat)
}
