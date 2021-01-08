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

# get_regional_data() tests - with only Level 1 regions
get_expected_data_for_get_regional_data_tests_only_level_1_regions <- function() {
  ## Dates/provinces
  dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03", "2020-02-04", "2020-02-05")
  provinces <- c("Northland", "Eastland", "Southland", "Westland", "Virginia")
  ## Fake region codes
  region_codes <- tibble::tibble(iso_3166_2 = c("NO", "EA", "SO", "WE", "VA"), region = provinces)

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

    colnames(expected_data_for_province) <- c("date", "province", "cases_new", "deaths_new", "recovered_new",
                                              "hosp_new","tested_new", "cases_total", "deaths_total",
                                              "recovered_total",  "hosp_total", "tested_total")

    expected_data_for_province <- expected_data_for_province %>%
      dplyr::select(date, province, cases_new, cases_total, deaths_new, deaths_total,
                    recovered_new, recovered_total, hosp_new,
                    hosp_total, tested_new, tested_total)

    expected_data_for_province$tested_new <- as.numeric(NA_integer_)
    expected_data_for_province$tested_total <-  as.numeric(NA_integer_)

    expected_data_for_province[row_for_NA, 3:12] <-  as.numeric(NA_integer_)

    expected_data_for_province <- expected_data_for_province %>% tidyr::fill(cases_total,
                                                                             deaths_total,
                                                                             recovered_total,
                                                                             hosp_total)

    expected_data_for_provinces[[i]] <- expected_data_for_province
  }

  ## Expected Output
  expected_data <- suppressWarnings(dplyr::bind_rows(expected_data_for_provinces)) %>%
    dplyr::mutate(date = as.Date(date),
                  cases_new = as.numeric(cases_new),
                  cases_total = as.numeric(cases_total),
                  deaths_new = as.numeric(deaths_new),
                  deaths_total = as.numeric(deaths_total),
                  recovered_new = as.numeric(recovered_new),
                  recovered_total = as.numeric(recovered_total),
                  hosp_new = as.numeric(hosp_new),
                  hosp_total = as.numeric(hosp_total)) %>%
    dplyr::left_join(region_codes, by = c("province" = "region")) %>%
    dplyr::select(date, province, iso_3166_2, cases_new, cases_total, deaths_new,
                  deaths_total, recovered_new, recovered_total,
                  hosp_new, hosp_total, tested_new,
                  tested_total) %>%
    dplyr::arrange(date, province)

  return(tibble::tibble(expected_data))
}

get_input_data_for_get_regional_data_tests_only_level_1_regions <- function() {
  expected_data <- get_expected_data_for_get_regional_data_tests_only_level_1_regions()

  ## To get the correct input -> delete NA columns, and rows with NAs in them
  input_data <- expected_data[, -c(3, 12, 13)]
  input_data <- input_data[-which(rowSums(is.na(input_data)) > 0), ]
  colnames(input_data)[2] <- "region_level_1"
  return(input_data)
}

get_expected_totals_data_for_get_regional_data_tests_only_level_1_regions <- function() {
  expected_data <- get_expected_data_for_get_regional_data_tests_only_level_1_regions()

  ## Totals data to test function when totals = TRUE
  totals_data <- expected_data[c(26:30), c(2, 3, 5, 7, 9, 11, 13)]
  totals_data[, 7] <- rep(0, 5)
  colnames(totals_data) <- c("province", "iso_3166_2", "cases_total", "deaths_total", "recovered_total", "hosp_total", "tested_total")
  totals_data <- totals_data %>% dplyr::arrange(-cases_total)

  return(tibble::tibble(totals_data))
}


# get_regional_data() tests - including Level 2 regions
get_input_data_for_get_regional_data_tests_with_level_2_regions <- function() {
  data <- get_input_data_for_get_regional_data_tests_only_level_1_regions()
  colnames(data)[2] <- "region_level_2"
  regions_table <- tibble::tibble(region_level_2 = c("Northland", "Eastland", "Southland", "Westland", "Virginia"),
                                  region_level_1 = c("Oneland", "Oneland", "Twoland", "Twoland", "USA"))

  data <- data %>%
    dplyr::left_join(regions_table, by = "region_level_2") %>%
    dplyr::select(date, region_level_2, region_level_1, cases_new, cases_total, deaths_new,
                  deaths_total, recovered_new, recovered_total,
                  hosp_new, hosp_total)

  return(data)
}

get_expected_data_for_get_regional_data_tests_with_level_2_regions <- function() {
  data <- get_expected_data_for_get_regional_data_tests_only_level_1_regions()
  data <- data[, -3]
  data$region <- rep(c("Oneland", "Oneland", "Twoland", "USA", "Twoland"), 6)
  region_codes <- tibble::tibble(iso_3166_2 = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))
  level_2_region_codes <- tibble::tibble(iso_3166_2_province = c("NO", "EA", "SO", "WE", "VA"),
                                      region = c("Northland", "Eastland", "Southland", 
                                                 "Westland", "Virginia"))

  data <- data %>%
    dplyr::left_join(region_codes, by = "region") %>%
    dplyr::left_join(level_2_region_codes, by = c("province" = "region")) %>%
    dplyr::select(date, province, iso_3166_2_province, region, iso_3166_2, 
                  cases_new, cases_total, deaths_new, deaths_total, 
                  recovered_new, recovered_total, hosp_new, hosp_total, 
                  tested_new, tested_total) %>%
    dplyr::arrange(date, region, province)

  return(data)
}

get_expected_totals_data_for_get_regional_data_tests_with_level_2_regions <- function() {
  data <- get_expected_totals_data_for_get_regional_data_tests_only_level_1_regions()

  data <- data[, -2]
  data$region <- c("Oneland", "USA", "Twoland", "Twoland", "Oneland")
  region_codes <- tibble::tibble(iso_3166_2 = c("ON", "TW", "US"),
                              region = c("Oneland", "Twoland", "USA"))
  level_2_region_codes <- tibble::tibble(iso_3166_2_province = c("NO", "EA", "SO", "WE", "VA"),
                                      region = c("Northland", "Eastland", "Southland", 
                                                 "Westland", "Virginia"))
  
  data <- data %>%
    dplyr::left_join(region_codes, by = "region") %>%
    dplyr::left_join(level_2_region_codes, by = c("province" = "region")) %>%
    dplyr::select(province, iso_3166_2_province, region, iso_3166_2, cases_total, deaths_total,
                  recovered_total, hosp_total, tested_total)

  return(tibble::tibble(data))
}


# data for two of the helper functions
# (fill empty dates with NA and complete cumulative columns)
get_expected_data_for_fill_empty_dates_with_na_test <- function() {
  ## Setup the last two
  dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03")
  regions <- c("Northland", "Eastland", "Wisconsin")
  
  region_codes <- tibble::tibble(region = regions,
                                 level_1_region_code = c("NO", "EA", "WI"))

  # full data is data with all dates/regions + some NAs in the cases column
  expected_data <- data.frame(expand.grid(dates, regions))
  colnames(expected_data) <- c("date", "region_level_1")
  expected_data$date <- as.Date(expected_data$date)
  expected_data$region_level_1 <- as.character(expected_data$region_level_1)
  expected_data <- expected_data %>%
    dplyr::arrange(date, region_level_1) %>%
    dplyr::left_join(region_codes, by = c("region_level_1" = "region"))
  expected_data$cases <- c(1:5, rep(NA, 4), 10:12)
  return(tibble::tibble(expected_data))
}

get_input_data_for_complete_cumulative_columns_test <- function() {
  expected_data <- get_expected_data_for_fill_empty_dates_with_na_test()

  # add cumulative cases to partial data and then add NA rows
  partial_data <- expected_data[-c(6:9), ]
  partial_data_with_cum_cases_na <- partial_data %>% dplyr::group_by(region_level_1) %>% dplyr::mutate(cases_total = cumsum(cases))
  full_data_with_cum_cases_na <- covidregionaldata:::fill_empty_dates_with_na(partial_data_with_cum_cases_na)

  return(full_data_with_cum_cases_na)
}

get_expected_data_for_complete_cumulative_columns_test <- function() {
  expected_data <- get_expected_data_for_fill_empty_dates_with_na_test()
  partial_data <- expected_data[-c(6:9), ]

  # manually add cumulative cases to get expected data
  full_data_with_cum_cases_filled <- covidregionaldata:::fill_empty_dates_with_na(partial_data)
  full_data_with_cum_cases_filled <- dplyr::arrange(full_data_with_cum_cases_filled, region_level_1, date)
  full_data_with_cum_cases_filled <- cbind(full_data_with_cum_cases_filled, as.integer(c(1,5,5,15,2,7,7,18,3,3,3,15)))
  colnames(full_data_with_cum_cases_filled)[5] <- "cases_total"

  return(tibble::tibble(full_data_with_cum_cases_filled))
}


