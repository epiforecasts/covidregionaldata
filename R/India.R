#' India Class for downloading, cleaning and processing notification data
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for India.
#'
#' @details Inherits from `DataClass`
#' @source https://api.covid19india.org/csv/latest/state_wise_daily.csv
#' @export
#' @examples
#' \dontrun{
#' region <- India$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
India <- R6::R6Class("India",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field country name of country to fetch data for
    country = "India",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "state"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data.
    common_data_urls = list(
      "main" = "https://api.covid19india.org/csv/latest/state_wise_daily.csv"
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new", "recovered_new"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "AN", "AP", "AR", "AS", "BR", "CH", "CT", "DN", "DD",
          "DL", "GA", "GJ", "HR", "HP", "JK", "JH", "KA", "KL",
          "LA", "LD", "MP", "MH", "MN", "ML", "MZ", "NL", "OR",
          "PY", "PB", "RJ", "SK", "TN", "TG", "TR", "UN", "UP", "UT", "WB"
        ),
        level_1_region = c(
          "Andaman and Nicobar", "Andhra Pradesh", "Arunachal Pradesh",
          "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
          "Dadra and Nagar Haveli", "Daman and Diu", "NCT of Delhi", "Goa",
          "Gujarat", "Haryana", "Himachal Pradesh", "Jammu and Kashmir",
          "Jharkhand", "Karnataka", "Kerala", "Ladakh", "Lakshadweep",
          "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", "Mizoram",
          "Nagaland", "Odisha", "Puducherry", "Punjab", "Rajasthan", "Sikkim",
          "Tamil Nadu", "Telangana", "Tripura", "Unknown", "Uttar Pradesh",
          "Uttarakhand", "West Bengal"
        )
      )
    },

    #' @description India state level data cleaning
    #' @importFrom dplyr mutate select filter rename left_join full_join
    #' @importFrom tidyr pivot_longer
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      india_cases <- self$data$raw$main %>%
        filter(Status == "Confirmed") %>%
        select(Date, self$codes_lookup$`1`[["code"]]) %>%
        pivot_longer(-Date, names_to = "state", values_to = "cases_new")

      india_deaths <- self$data$raw$main %>%
        filter(Status == "Deceased") %>%
        select(Date, self$codes_lookup$`1`[["code"]]) %>%
        pivot_longer(-Date, names_to = "state", values_to = "deaths_new")

      india_recoveries <- self$data$raw$main %>%
        filter(Status == "Recovered") %>%
        select(Date, self$codes_lookup$`1`[["code"]]) %>%
        pivot_longer(-Date, names_to = "state", values_to = "recovered_new")

      cases_and_death_data <- full_join(india_cases, india_deaths,
        by = c("Date" = "Date", "state" = "state")
      )
      cases_and_death_data <- full_join(cases_and_death_data,
        india_recoveries,
        by = c("Date" = "Date", "state" = "state")
      )

      self$data$clean <- cases_and_death_data %>%
        mutate(Date = dmy(Date)) %>%
        rename(date = Date) %>%
        left_join(self$codes_lookup$`1`, by = c("state" = "code")) %>%
        mutate(level_1_region_code = paste0("IN-", state)) %>%
        select(-state)
    }
  )
)
