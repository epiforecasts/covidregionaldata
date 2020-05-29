## Dates/provinces
dates <- c("2020-01-31", "2020-02-01", "2020-02-02", "2020-02-03", "2020-02-04", "2020-02-05")
provinces <- c("Northland", "Eastland", "Southland", "Westland", "Virginia")

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

  colnames(expected_data_for_province) <- c("date", "province", "cases_today", "deaths_today", "recoveries_today",
                           "hospitalisations_today","tests_today", "cumulative_cases", "cumulative_deaths",
                           "cumulative_recoveries",  "cumulative_hospitalisations", "cumulative_tests")

  expected_data_for_province <- expected_data_for_province %>%
                                           dplyr::select(date, province, cases_today, cumulative_cases, deaths_today, cumulative_deaths,
                                           recoveries_today, cumulative_recoveries, hospitalisations_today,
                                           cumulative_hospitalisations, tests_today, cumulative_tests)

  expected_data_for_province$tests_today <- NA_integer_
  expected_data_for_province$cumulative_tests <- NA_integer_

  expected_data_for_province[row_for_NA, 3:12] <- NA_integer_

  expected_data_for_province <- expected_data_for_province %>% tidyr::fill(cumulative_cases,
                                                               cumulative_deaths,
                                                               cumulative_recoveries,
                                                               cumulative_hospitalisations)

  expected_data_for_provinces[[i]] <- expected_data_for_province
}

## Expected Output
expected_data <- suppressWarnings(dplyr::bind_rows(expected_data_for_provinces)) %>%
             dplyr::mutate(date = as.Date(date),
                           cases_today = as.numeric(cases_today),
                           cumulative_cases = as.numeric(cumulative_cases),
                           deaths_today = as.numeric(deaths_today),
                           cumulative_deaths = as.numeric(cumulative_deaths),
                           recoveries_today = as.numeric(recoveries_today),
                           cumulative_recoveries = as.numeric(cumulative_recoveries),
                           hospitalisations_today = as.numeric(hospitalisations_today),
                           cumulative_hospitalisations = as.numeric(cumulative_hospitalisations)) %>%
              dplyr::arrange(date, province)


## To get the correct input -> delete NA columns, and rows with NAs in them
input_data <- expected_data[, -c(11, 12)]
input_data <- input_data[-which(rowSums(is.na(input_data)) > 0), ]
colnames(input_data)[2] <- "region"


## Totals data to test function when totals = TRUE
totals_data <- expected_data[c(26:30), c(2, 4, 6, 8, 10, 12)]
totals_data[, 6] <- rep(0, 5)
totals_data <- totals_data %>% dplyr::arrange(-cumulative_cases)
