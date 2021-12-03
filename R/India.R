#' India Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for India.
#'
#' @source \url{https://www.covid19india.org}
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- India$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
India <- R6::R6Class("India",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "India",
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
    #' @field source_text Plain text description of the source of the data
    source_text = "COVID19India",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://www.covid19india.org",

    #' @description Set up a table of region codes for clean data
    #' @importFrom dplyr tibble
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
    #' @importFrom purrr map reduce
    #'
    clean_common = function() {
      params <- c("Confirmed", "Deceased", "Recovered")
      data <- map(params, self$get_desired_status)
      cases_deaths_recovered <- data %>%
        reduce(
          full_join,
          by = c(
            "Date" = "Date",
            "state" = "state"
          )
        )
      self$data$clean <- cases_deaths_recovered %>%
        mutate(Date = dmy(Date)) %>%
        rename(date = Date) %>%
        left_join(self$codes_lookup$`1`, by = c("state" = "code")) %>%
        mutate(level_1_region_code = paste0("IN-", state)) %>%
        select(-state)
    },

    #' @description Extract data from raw table
    #' @importFrom dplyr select filter
    #' @importFrom tidyr pivot_longer
    #' @importFrom rlang .data
    #' @param status The data to extract
    #'
    get_desired_status = function(status) {
      conversion <- list(
        "Confirmed" = "cases_new",
        "Deceased" = "deaths_new",
        "Recovered"  = "recovered_new"
      )
      india_cases <- self$data$raw$main %>%
        filter(Status == status) %>%
        select(Date, self$codes_lookup$`1`[["code"]]) %>%
        pivot_longer(
          -Date,
          names_to = "state",
          values_to = conversion[[status]]
        )
    }
  )
)
