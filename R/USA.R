#' USA Class for downloading, cleaning and processing notification data
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for USA.
#'
#' @details Inherits from `DataClass`
#' @source https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv # nolint
#' @source https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv # nolint
#' @export
#' @examples
#' \dontrun{
#' region <- USA$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
USA <- R6::R6Class("USA",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "state",
    #' @field level_2_region the level 2 region name.
    level_2_region = "county",
    #' @field data_url link to raw data for level 1
    data_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", # nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total", "deaths_total"),
    #' @field data_url_level_2 link to raw data for level 2
    data_url_level_2 = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", # nolint

    #' @description USA-specific function for downloading raw data.
    download = function() {
      message_verbose(self$verbose, "Downloading data")
      if (self$level == 1) {
        self$data$raw <- csv_reader(self$data_url, self$verbose)
      } else {
        self$data$raw <- csv_reader(self$data_url_level_2, self$verbose)
      }
    },

    #' @description USA specific state level data cleaning
    #' @importFrom dplyr mutate select arrange recode filter
    #' @importFrom tidyr pivot_longer
    #' @importFrom lubridate as_date dmy
    #' @importFrom rlang .data
    #'
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")

      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description USA Specific State Level Data Cleaning
    #' @importFrom dplyr rename mutate select left_join
    clean_level_1 = function() {
      self$data$clean <- self$data$raw %>%
        rename(
          region_level_1 = state,
          cases_total = cases,
          deaths_total = deaths
        ) %>%
        mutate(
          cases_total = replace(cases_total, cases_total < 0, 0),
          deaths_total = replace(deaths_total, deaths_total < 0, 0)
        ) %>%
        select(date, region_level_1, cases_total, deaths_total) %>%
        left_join(self$data$codes_lookup,
                  by = c("region_level_1"="region"))
    },

    #' @description USA Specific County Level Data Cleaning
    #' @importFrom dplyr mutate rename left_join
    clean_level_2 = function() {
      self$data$clean <- self$data$raw %>%
        rename(
          region_level_2 = county,
          region_level_1 = state,
          level_2_region_code = fips,
          cases_total = cases,
          deaths_total = deaths
        ) %>%
        mutate(
          cases_total = replace(cases_total, cases_total < 0, 0),
          deaths_total = replace(deaths_total, deaths_total < 0, 0)
        ) %>%
        left_join(self$data$codes_lookup,
                  by = c("region_level_1"="region"))
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  )
)
