#' SouthAfrica Class for downloading, cleaning and processing notification data
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for South Africa.
#'
#' @details Inherits from `DataClass`
#' @source https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv # nolint
#' @source https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv # nolint
#' @export
#' @examples
#' \dontrun{
#' region <- SouthAfrica$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
SouthAfrica <- R6::R6Class("SouthAfrica",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "province",
    #' @field data_url link to raw data for cases
    data_url = "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv", # nolint
    #' @field deaths_url additional link to raw data for deaths
    deaths_url = "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv", # nolint

    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new", "recovered_new"),

    #' @description South Africa-specific function for downloading raw data.
    download = function() {
      message_verbose(self$verbose, "Downloading data")
      self$data$raw <- csv_reader(self$data_url, self$verbose)
      self$data$raw_deaths <- csv_reader(self$deaths_url, self$verbose)
    },

    #' @description SouthAfrica specific state level data cleaning
    #' @importFrom dplyr mutate select arrange recode filter bind_rows na_if
    #' @importFrom tidyr pivot_longer pivot_wider
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")

      self$data$clean <- bind_rows(self$data$raw, self$data$raw_deaths,
        .id = "data") %>%
        mutate(
          data = factor(data, c(1, 2), c("cases_total", "deaths_total")),
          date = dmy(date)
          ) %>%
        select(-c(YYYYMMDD, total, source)) %>%
        pivot_longer(-c(data, date), names_to = "level_1_region_code") %>%
        pivot_wider(names_from = data) %>%
        mutate(
          level_1_region_code = paste0("ZA-", level_1_region_code),
          level_1_region_code = na_if(level_1_region_code, "ZA-UNKNOWN")
        ) %>%
        left_join(self$data$codes_lookup, by = "level_1_region_code")
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  )
)
