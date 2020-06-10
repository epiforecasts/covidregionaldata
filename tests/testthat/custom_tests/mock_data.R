##--------------------------------------------------#
##------------------ Test Data ---------------------#
##--------------------------------------------------#
## The functions in this file create input data and
## expected data for tests which require more data
## than simply two or three columns / rows.
##
## The function name indicates which test the data
## is used for - some are used for multiple tests.
##--------------------------------------------------#

# get_regional_covid_data() tests - with only Level 1 regions
get_expected_data_for_get_regional_covid_data_tests_only_level_1_regions <- function() {
  ## Dates/provinces
  dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03", "2020-02-04", "2020-02-05")
  provinces <- c("Northland", "Eastland", "Southland", "Westland", "Virginia")
  ## Fake ISO codes
  iso_codes <- tibble::tibble(iso_code = c("NO", "EA", "SO", "WE", "VA"), region = provinces)

  expected_data_for_provinces <- list()
  for (i in 1:length(provinces)) {
    ## Dates/province
    province <- provinces[i]
    dates_and_province <- data.frame(expand.grid(dates, province))

    ## Count Data
    set.seed(417+i)
    count_data <- sort(sample(1:50, 30, replace = T))
    set.seed(Sys.time())
    count_data_frame <- data.frame(matrix(count_data, ncol=5, byrow=TRUE))

    set.seed(940+i)
    row_for_NA <- sample(1:6, 1, replace=FALSE)
    set.seed(Sys.time())
    count_data_frame[row_for_NA, ] <- 0

    cumulative_counts_data_frame <- cumsum(count_data_frame)
    expected_data_for_province <- cbind(dates_and_province, count_data_frame, cumulative_counts_data_frame)

    colnames(expected_data_for_province) <- c("date", "province", "cases_new", "deaths_new", "recoveries_new",
                                              "hospitalisations_new","tests_new", "cases_total", "deaths_total",
                                              "recoveries_total",  "hospitalisations_total", "tests_total")

    expected_data_for_province <- expected_data_for_province %>%
      dplyr::select(date, province, cases_new, cases_total, deaths_new, deaths_total,
                    recoveries_new, recoveries_total, hospitalisations_new,
                    hospitalisations_total, tests_new, tests_total)

    expected_data_for_province$tests_new <- as.numeric(NA_integer_)
    expected_data_for_province$tests_total <-  as.numeric(NA_integer_)

    expected_data_for_province[row_for_NA, 3:12] <-  as.numeric(NA_integer_)

    expected_data_for_province <- expected_data_for_province %>% tidyr::fill(cases_total,
                                                                             deaths_total,
                                                                             recoveries_total,
                                                                             hospitalisations_total)

    expected_data_for_provinces[[i]] <- expected_data_for_province
  }

  ## Expected Output
  expected_data <- suppressWarnings(dplyr::bind_rows(expected_data_for_provinces)) %>%
    dplyr::mutate(date = as.Date(date),
                  cases_new = as.numeric(cases_new),
                  cases_total = as.numeric(cases_total),
                  deaths_new = as.numeric(deaths_new),
                  deaths_total = as.numeric(deaths_total),
                  recoveries_new = as.numeric(recoveries_new),
                  recoveries_total = as.numeric(recoveries_total),
                  hospitalisations_new = as.numeric(hospitalisations_new),
                  hospitalisations_total = as.numeric(hospitalisations_total)) %>%
    dplyr::arrange(date, province)

  expected_data <- dplyr::left_join(expected_data, iso_codes, by = c("province" = "region")) %>%
    dplyr::select(date, province, iso_code, cases_new, cases_total, deaths_new,
                  deaths_total, recoveries_new, recoveries_total,
                  hospitalisations_new, hospitalisations_total, tests_new,
                  tests_total)

  return(tibble::tibble(expected_data))
}

get_input_data_for_get_regional_covid_data_tests_only_level_1_regions <- function() {
  expected_data <- get_expected_data_for_get_regional_covid_data_tests()

  ## To get the correct input -> delete NA columns, and rows with NAs in them
  input_data <- expected_data[, -c(3, 12, 13)]
  input_data <- input_data[-which(rowSums(is.na(input_data)) > 0), ]
  colnames(input_data)[2] <- "region_level_1"
  return(input_data)
}

get_expected_totals_data_for_get_regional_covid_data_tests_only_level_1_regions <- function() {
  expected_data <- get_expected_data_for_get_regional_covid_data_tests()

  ## Totals data to test function when totals = TRUE
  totals_data <- expected_data[c(26:30), c(2, 3, 5, 7, 9, 11, 13)]
  totals_data[, 7] <- rep(0, 5)
  colnames(totals_data) <- c("province", "iso_code", "cases_total", "deaths_total", "recoveries_total", "hospitalisations_total", "tests_total")
  totals_data <- totals_data %>% dplyr::arrange(-cases_total)

  return(tibble::tibble(totals_data))
}


# get_regional_covid_data() tests - including Level 2 regions
get_input_data_for_get_regional_covid_data_tests_with_level_2_regions <- function() {
  data <- get_input_data_for_get_regional_covid_data_tests()
  colnames(data)[2] <- "region_level_2"
  regions_table <- tibble::tibble(region_level_2 = c("Northland", "Eastland", "Southland", "Westland", "Virginia"),
                                  region_level_1 = c("Oneland", "Oneland", "Twoland", "Twoland", "USA"))

  data <- data %>%
    dplyr::left_join(regions_table, by = "region_level_2") %>%
    dplyr::select(date, region_level_2, region_level_1, cases_new, cases_total, deaths_new,
                  deaths_total, recoveries_new, recoveries_total,
                  hospitalisations_new, hospitalisations_total)

  return(data)
}

get_expected_data_for_get_regional_covid_data_tests_with_level_2_regions <- function() {
  data <- get_expected_data_for_get_regional_covid_data_tests()
  data <- data[, -3]
  data$region <- rep(c("Oneland", "Oneland", "Twoland", "USA", "Twoland"), 6)
  iso_codes <- tibble::tibble(iso_code = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))

  data <- data %>%
    dplyr::left_join(iso_codes, by = "region") %>%
    dplyr::select(date, province, region, iso_code, cases_new, cases_total, deaths_new,
                         deaths_total, recoveries_new, recoveries_total,
                         hospitalisations_new, hospitalisations_total, tests_new,
                         tests_total)
  return(data)
}

get_expected_totals_data_for_get_regional_covid_data_tests_with_level_2_regions <- function() {
  data <- get_expected_totals_data_for_get_regional_covid_data_tests()

  data <- data[, -2]
  data$region <- c("Oneland", "USA", "Twoland", "Twoland", "Oneland")
  iso_codes <- tibble::tibble(iso_code = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))

  data <- data %>%
    dplyr::left_join(iso_codes, by = "region") %>%
    dplyr::select(province, region, iso_code, cases_total, deaths_total,
                  recoveries_total, hospitalisations_total, tests_total)

  return(tibble::tibble(data))
}


# data for two of the helper functions
# (fill empty dates with NA and complete cumulative columns)
get_expected_data_for_fill_empty_dates_with_na_test <- function() {
  ## Setup the last two
  dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03")
  regions <- c("Northland", "Eastland", "Wisconsin")

  # full data is data with all dates/regions + some NAs in the cases column
  expected_data <- data.frame(expand.grid(dates, regions))
  colnames(expected_data) <- c("date", "region_level_1")
  expected_data$date <- as.Date(expected_data$date)
  expected_data$region_level_1 <- as.character(expected_data$region_level_1)
  expected_data <- expected_data %>% dplyr::arrange(date, region_level_1)
  expected_data$cases <- c(1:5, rep(NA, 4), 10:12)
  return(expected_data)
}

get_input_data_for_complete_cumulative_columns_test <- function() {
  expected_data <- get_expected_data_for_helpers()

  # add cumulative cases to partial data and then add NA rows
  partial_data <- expected_data[-c(6:9), ]
  partial_data_with_cum_cases_na <- partial_data %>% dplyr::group_by(region_level_1) %>% dplyr::mutate(cases_total = cumsum(cases))
  full_data_with_cum_cases_na <- fill_empty_dates_with_na(partial_data_with_cum_cases_na)

  return(full_data_with_cum_cases_na)
}

get_expected_data_for_complete_cumulative_columns_test <- function() {
  expected_data <- get_expected_data_for_helpers()
  partial_data <- expected_data[-c(6:9), ]

  # manually add cumulative cases to get expected data
  full_data_with_cum_cases_filled <- fill_empty_dates_with_na(partial_data)
  full_data_with_cum_cases_filled <- arrange(full_data_with_cum_cases_filled, region_level_1, date)
  full_data_with_cum_cases_filled <- cbind(full_data_with_cum_cases_filled, c(1,5,5,15,2,7,7,18,3,3,3,15))
  colnames(full_data_with_cum_cases_filled)[4] <- "cases_total"

  return(full_data_with_cum_cases_filled)
}


# data for convert to Covid19R style function
get_input_data_for_covid19R_converter_test <- function() {
  dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03")
  regions <- c("Northland", "Eastland", "Wisconsin")

  iso_code <- c("NO", "EA", "WI")
  iso_table <- data.frame(cbind(regions, iso_code))

  dates_and_regions <- data.frame(expand.grid(dates, regions, stringsAsFactors = FALSE))
  colnames(dates_and_regions) <- c("date", "region")
  dates_regions_iso <- dplyr::left_join(dates_and_regions, iso_table, by = c("region" = "regions")) %>% arrange(date)

  set.seed(181)
  value <- floor(runif(120, 0, 100))
  set.seed(Sys.time())

  data_values_matrix <- matrix(value, ncol = 10, byrow=TRUE)
  input_data <- cbind(dates_regions_iso, data_values_matrix)
  colnames(input_data)[4:13] <- c("cases_new", "cases_total", "deaths_new","deaths_total",
                                  "recoveries_new", "recoveries_total", "hospitalisations_new",
                                  "hospitalisations_total", "tests_new", "tests_total")

  return(input_data)
}

get_expected_data_for_covid19R_converter_test <- function() {
  dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03")
  date <- sort(rep(dates, 30))
  location <- rep(c(rep("Northland", 10), rep("Eastland", 10), rep("Wisconsin", 10)), 4)
  location_code <- rep(c(rep("NO", 10), rep("EA", 10), rep("WI", 10)), 4)
  exp_data_start <- cbind(date, location, location_code)

  set.seed(181)
  value <- floor(runif(120, 0, 100))
  set.seed(Sys.time())

  expected_data <- cbind(exp_data_start, data.frame(value))
  expected_data$data_type <- rep(c("cases_new", "cases_total", "deaths_new", "deaths_total", "recoveries_new", "recoveries_total",
                                   "hospitalisations_new", "hospitalisations_total", "tests_new", "tests_total"), 12)
  expected_data$location_type <- "region"
  expected_data$location_code_type <- "iso-3166-2"

  expected_data <- expected_data %>% dplyr::select(date,	location,	location_type, location_code, location_code_type,	data_type, value)
  return(tibble::tibble(expected_data))
}

