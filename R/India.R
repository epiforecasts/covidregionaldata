#' India Class for downloading, cleaning and processing notification data
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for India.
#'
#' @details Inherits from `DataClass`
#' @source https://api.covid19india.org/csv/latest/state_wise_daily.csv # nolint
#' @export
#' @examples
#' \dontrun{
#' region <- India$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
India <- R6::R6Class("India",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "state",
    #' @field data_url link to raw data
    data_url = "https://api.covid19india.org/csv/latest/state_wise_daily.csv", # nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new", "recovered_new"),

    #' @description India specific state level data cleaning
    #' @importFrom dplyr mutate select arrange recode filter
    #' @importFrom tidyr pivot_longer
    #' @importFrom lubridate as_date dmy
    #' @importFrom rlang .data
    #'
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")

      india_cases <- self$data$raw %>%
        filter(Status == "Confirmed") %>%
        select(Date, self$data$codes_lookup[["code"]]) %>%
        pivot_longer(-Date, names_to = "state", values_to = "cases_new")

      india_deaths <- self$data$raw %>%
        filter(Status == "Deceased") %>%
        select(Date, self$data$codes_lookup[["code"]]) %>%
        pivot_longer(-Date, names_to = "state", values_to = "deaths_new")

      india_recoveries <- self$data$raw %>%
        filter(Status == "Recovered") %>%
        select(Date, self$data$codes_lookup[["code"]]) %>%
        pivot_longer(-Date, names_to = "state", values_to = "recovered_new")

      cases_and_death_data <- full_join(india_cases, india_deaths,
        by = c("Date" = "Date", "state" = "state"))
      cases_and_death_data <- full_join(cases_and_death_data,
        india_recoveries, by = c("Date" = "Date", "state" = "state"))

      self$data$clean <- cases_and_death_data %>%
            mutate(Date = dmy(Date)) %>%
            rename(date = Date) %>%
            left_join(self$data$codes_lookup, by = c("state" = "code")) %>%
            mutate(level_1_region_code = paste0("IN-", state)) %>%
            select(-state)

    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  )
)
