#' Daily counts from data that is in cumulative form.
#' @description Gets daily counts from data column that is in cumulative form.
#' @param column A vector of numeric data (e.g. a data column) which corresponds to cumulative counts of a process
#' @return A vector of numeric data which corresponds to daily counts
#' 
get_daily_from_cumulative <- function(column) {
  shifted <- c(column[1], diff(column))
  return(shifted)
}

#' Cumulative counts from daily counts.
#' @description Gets cumulative cases/deaths etc. from data which is in daily forms. Similar to cumsum() but deals with NAs by treating them as 0.
#' @param column A vector of numeric data (e.g. a data column) which corresponds to daily counts of a process. Can contain NA
#' @return A vector of numeric data which corresponds to cumulative counts
#' 
get_cumulative_from_daily <- function(column) {
  if (all(is.na(column))){
    return(column)
  }
  
  first_non_na <- min(which(!is.na(column)))
  column <- column[first_non_na:length(column)]
  column[which(is.na(column))] <- 0
  cum_sum <- cumsum(column)
  cum_sum <- c(rep(NA, first_non_na - 1), cum_sum)
  return(cum_sum)
}

#' Add extra columns filled with NA values to a dataset.
#' @description Adds extra columns filled with NAs to a dataset. This ensures that all datasets from the covidregionaldata package return datasets
#' of the same underlying structure (i.e. same columns).
#' @param data a data table
#' @return a tibble with relevant NA columns added
#' @importFrom tibble tibble
#' 
add_extra_na_cols <- function(data) {
  expected_col_names <- c("cases_new", "cases_total", "deaths_new", "deaths_total",
                          "recovered_new", "recovered_total", "tested_new", "tested_total", "hosp_new",
                          "hosp_total")

  for (colname in expected_col_names) {
    if (!(colname %in% colnames(data))) {
      original_col_names <- colnames(data)
      data$newCol <- rep(NA_integer_, dim(data)[1])
      colnames(data) <- c(original_col_names, colname)
    }
  }
  return(tibble::tibble(data))
}

#' Helper to rename the region column in each dataset to the correct name for each country.
#' @description The package relies on column name 'region' during processing but this often isn't the most sensible name for the column
#' (e.g. state makes more sense for USA). This simply renames the column as the final step in processing before returning data to the user.
#' @param data a data frame with a region_level_1 column and optionally a region_level_2 column
#' @param country a string with the country of interest
#' @return a tibble with the column renamed to a sensible name
#' @importFrom dplyr %>% rename
#' @importFrom tibble tibble
#' 
rename_region_column <- function(data, country) {

  level_1_region_name <- switch(tolower(country),
                               "afghanistan" = "province",
                               "belgium" = "region",
                               "brazil" = "state",
                               "canada" = "province",
                               "colombia" = "departamento",
                               "germany" = "bundesland",
                               "india" = "state",
                               "italy" = "region",
                               "russia" = "region",
                               "uk" = "region",
                               "usa" = "state")

  data <- data %>% dplyr::rename(!!level_1_region_name := region_level_1)

  if ("region_level_2" %in% colnames(data)) {
    level_2_region_name <- switch(tolower(country),
                                "belgium" = "province",
                                "brazil" = "city",
                                "germany" = "landkreis",
                                "uk" = "authority",
                                "usa" = "county")

    data <- data %>% dplyr::rename(!!level_2_region_name := region_level_2)
  }

  return(tibble::tibble(data))
}

#' Helper to rename the region code column in each dataset to the correct code type for each country (e.g. ISO-3166-2).
#' @description The package relies on column name 'region_level_1_code' etc. during processing but this often isn't the most 
#' sensible name for the column (e.g. iso-3166-2 makes more sense for US states). This simply renames the column as the final step in 
#' processing before returning data to the user.
#' @param data a data frame with a region_level_1_code column and optionally a region_level_2_code column
#' @param country a string with the country of interest
#' @return a tibble with the column(s) renamed to a sensible name
#' @importFrom dplyr %>% rename
#' @importFrom tibble tibble
#' 
rename_region_code_column <- function(data, country) {
  
  level_1_region_code_name <- switch(tolower(country),
                                  "afghanistan" = "iso_3166_2",
                                  "belgium" = "iso_3166_2",
                                  "brazil" = "iso_3166_2",
                                  "canada" = "iso_3166_2",
                                  "colombia" = "iso_3166_2",
                                  "germany" = "iso_3166_2",
                                  "india" = "iso_3166_2",
                                  "italy" = "iso_3166_2",
                                  "russia" = "iso_3166_2",
                                  "uk" = "ons_region_code",
                                  "usa" = "iso_3166_2")
  
  data <- data %>% dplyr::rename(!!level_1_region_code_name := level_1_region_code)
  
  if ("level_2_region_code" %in% colnames(data)) {
    level_2_region_code_name <- switch(tolower(country),
                                  "belgium" = "iso_3166_2_province",
                                  "brazil" = "level_2_region_code",
                                  "germany" = "level_2_region_code",
                                  "uk" = "utla_code",
                                  "usa" = "fips")
    
    data <- data %>% dplyr::rename(!!level_2_region_code_name := level_2_region_code)
  }
  
  return(tibble::tibble(data))
}

#' Set negative data to 0
#' @description Set data values to 0 if they are negative in a dataset. Data in the datasets should always be > 0.
#' @param data a data table
#' @return a tibble with all relevant data > 0.
#' @importFrom dplyr %>% mutate
#' @importFrom tibble tibble
#' 
set_negative_values_to_zero <- function(data) {
  numeric_col_names <- c('deaths_total', 'cases_total', 'recovered_total', 'hosp_total', 'tested_total',
                         'cases_new', 'deaths_new', 'recovered_new', 'hosp_new', 'tested_new')

  for (numeric_col_name in numeric_col_names) {
    if (numeric_col_name %in% colnames(data)){
      data[which(data[, numeric_col_name] < 0), numeric_col_name] <- 0
    }
  }

  return(tibble::tibble(data))
}

#' Add rows of NAs for dates where a region does not have any data
#' @description There are points, particularly early during data collection, where data was not collected for all regions. 
#' This function finds dates which have data for some regions, but not all, and adds rows of NAs for the missing regions. 
#' This is mainly for reasons of completeness.
#' @param data a data table
#' @return a tibble with rows of NAs added.
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom tidyr complete full_seq
#' 
fill_empty_dates_with_na <- function(data) {

  if ("region_level_2" %in% colnames(data)) {
    data <- data %>%
      tidyr::complete(date = tidyr::full_seq(data$date, period = 1), tidyr::nesting(region_level_2, level_2_region_code,
                                                                                    region_level_1, level_1_region_code))
  } else {
    data <- data %>%
      tidyr::complete(date = tidyr::full_seq(data$date, period = 1), tidyr::nesting(region_level_1, level_1_region_code))
  }

  return(tibble::tibble(data))
}

#' Completes cumulative columns if rows were added with NAs.
#' @description If a dataset had a row of NAs added to it (using fill_empty_dates_with_na) then cumulative data columns will have NAs which can cause
#' issues later. This function fills these values with the previous non-NA value.
#' @param data a data table
#' @return a tibble with NAs filled in for cumulative data columns.
#' @importFrom dplyr %>% group_by
#' @importFrom tidyr fill
#' 
complete_cumulative_columns <- function(data) {
  cumulative_col_names <- c('deaths_total', 'cases_total', 'recovered_total', 'hosp_total', 'tested_total')

  for (cumulative_col_name in cumulative_col_names) {
    if (cumulative_col_name %in% colnames(data)){
      if ("region_level_2" %in% colnames(data)) {
        data <- data %>%
          dplyr::group_by(region_level_1, level_1_region_code, region_level_2, level_2_region_code) %>%
          tidyr::fill(all_of(cumulative_col_name)) %>%
          dplyr::ungroup()
      } else {
        data <- data %>%
          dplyr::group_by(region_level_1, level_1_region_code,) %>%
          tidyr::fill(all_of(cumulative_col_name)) %>%
          dplyr::ungroup()
      }
    }
  }

  return(tibble::tibble(data))
}


#' Cumulative counts from daily counts or daily counts from cumulative, dependent on which columns already exist
#' @description Checks which columns are missing (cumulative/daily counts) and if one is present and the other not 
#' then calculates the second from the first.
#' @param data A data frame
#' @return A data frame with extra columns if required
#' @importFrom dplyr %>% mutate
#' @importFrom tibble tibble
#' 
calculate_columns_from_existing_data <- function(data) {
  possible_counts <- c("cases", "deaths", "hosp", "recovered", "tested")

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

  return(tibble::tibble(data))
}

#' Convert data to Covid19R package data standard
#' @description Converts wide format (time series) data into long format to meet the Covid19R package standard
#' @param data A data frame / tibble
#' @return A data frame in the Covid19R standard
#' @importFrom dplyr %>% select rename arrange
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#' 
convert_to_covid19R_format <- function(data) {
  location_type <- colnames(data)[2]
  location_code_type <- colnames(data)[3]

  data <- data %>%
    dplyr::select(-hosp_total, -tested_new) %>%
    tidyr::pivot_longer(-c(date, !!location_type, !!location_code_type),  names_to = "data_type", values_to = "value")

  data <- data %>%
    dplyr::rename("location_code" = !!location_code_type,
                  "location" = !!location_type) 
  
  data$location_code_type <- location_code_type
  data$location_type <- location_type
  
  data <- data %>%
    dplyr::select(date,	location,	location_type, location_code, location_code_type,	data_type, value) %>%
    dplyr::arrange(date)

  return(tibble::tibble(data))
}

#' Custom CSV reading function
#' @description Checks for use of memoise and then uses whichever read_csv function is needed by user
#' @param file A URL or filepath to a CSV
#' @param ... extra parameters to be passed to readr::read_cs
#' @return A data table
#' @importFrom memoise memoise cache_filesystem
#' @importFrom readr read_csv cols
#' @importFrom tibble tibble
#' 
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
  return(tibble::tibble(data))
}

#' Custom left_join function
#' @description Checks if table that is being added is NULL and then uses left_join
#' @param data a data table
#' @param region_codes_table a table of region codes which will be left_join (optionally NULL)
#' @param by see dplyr::left_join() description of by parameter
#' @param ... optional arguments passed into dplyr::left_join()
#' @return A data table
#' @importFrom dplyr left_join
#' @importFrom tibble tibble
#' 
left_join_region_codes <- function(data, region_codes_table, by = NULL, ...) {
  if (is.null(region_codes_table)) {
    return(data)
  }
  
  data <- dplyr::left_join(data, region_codes_table, by = by, ...)
  return(tibble::tibble(data))
}

#' Get totals data given the time series data.
#' 
#' @description Get totals data given the time series data.
#' @param data a data table
#' @param include_level_2_regions Boolean. Are level 2 regions included in the data
#' @return A data table, totalled up
#' @importFrom dplyr left_join group_by %>%  summarise select arrange
#' @importFrom tibble tibble
#' 
totalise_data <- function(data, include_level_2_regions) {
  # Group the data ------------------------------------------------------
  if (include_level_2_regions) {
    data <- data %>%
      dplyr::group_by(region_level_1, level_1_region_code, region_level_2, level_2_region_code)
  } else {
    data <- data %>%
      dplyr::group_by(region_level_1, level_1_region_code)
  }
  
  # Total the data ------------------------------------------------------
  data <- data %>%
    dplyr::summarise(cases_total = sum(cases_new, na.rm = TRUE),
                     deaths_total = sum(deaths_new, na.rm = TRUE),
                     recovered_total = sum(recovered_new, na.rm = TRUE),
                     hosp_total = sum(hosp_new, na.rm = TRUE),
                     tested_total = sum(tested_new, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # Select correct data -------------------------------------------------
  if (include_level_2_regions) {
    data <- data %>%
      dplyr::select(region_level_2, level_2_region_code,
                    region_level_1, level_1_region_code, cases_total, deaths_total,
                    recovered_total, hosp_total, tested_total)
  } else {
    data <- data %>%
      dplyr::select(region_level_1, level_1_region_code, cases_total, deaths_total,
                    recovered_total, hosp_total, tested_total)
  }
  
  
  return(tibble::tibble(data))
}


# Global variables --------------------------------------------------------

utils::globalVariables(c(".", ":=", "AnzahlFall", "Area type", "Specimen date", "casos",
                         "AnzahlTodesfall", "Area", "Bundesland", "CASES", "Cases",
                         "Code", "Country", "DATE", "DEATHS", "Date", "Deaths", "Landkreis", "Meldedatum", 
                         "NEW_IN", "PROVINCE", "Province", "Province_State", "REGION", "Recoveries", "Specimen", 
                         "date", "Status", "TT", "TotalCases", "all_of", "cases", "cases_new", "cases_total", 
                         "casos_confirmados", "casos_fallecido", "city", "countriesAndTerritories",
                         "country", "county", "data", "data_type", "dateRep", "deaths", "deaths_new", 
                         "deaths_total", "deceduti", "denominazione_regione", "departamento", "fecha", "fips", "geoId", 
                         "hosp_new", "hosp_total", "iso_code", "level_1_region_code", "level_2_region_code", 
                         "location", "location_code", "n", "n.x", "n.y", "newCases", "newDeaths", "numdeaths", 
                         "numrecover", "numtested", "numtoday", "numtotal", "popData2019", "population_2019", 
                         "prname", "province", "pruebas", "pruid", "recovered_new", "recovered_total",  "region_level_1",
                         "region_level_2", "state", "state_name", "tamponi", "tested_new",
                         "Area", "type", "ID", "Specimen", "date", "date_admission_hospital", "date_confirm", 
                         "date_confirmation", "date_death_or_discharge", "date_onset",
                         "date_onset_symptoms", "days_onset_to_report", "id", "read.csv", "tested_total",
                         "totalCases", "totale_casi", "un_region", "untar", "value", "who_region",
                         "areaCode", "areaName", "cumAdmissions", "cumCasesByPublishDate",
                         "cumCasesBySpecimenDate", "cumDeaths28DaysByPublishDate",
                         "cumTestsByPublishDate", "data_list", "newAdmissions", "newCasesByPublishDate",
                         "newCasesBySpecimenDate", "newDeaths28DaysByPublishDate",
                         "newTestsByPublishDate", "setTxtProgressBar", "txtProgressBar",
                         "level_2_region_code.x", "level_2_region_code.y", "cumDeaths28DaysByDeathDate", "newDeaths28DaysByDeathDate"))

