#' Canada Class containing origin specific attributes and methods
#'
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Canada.
#'
#' @concept dataset
#' @source \url{https://health-infobase.canada.ca}
#' @export
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Canada$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Canada <- R6::R6Class("Canada",
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each parameter for origin specific information)
    #' @field origin name of origin to fetch data for
    origin = "Canada",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "province"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data that are common
    #' across levels.
    # nolint start
    common_data_urls = list(
      "main" = "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_new", "cases_total", "deaths_new",
      "recovered_total", "tested_new"
    ),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      canada_codes <- tibble(
        code = c(
          "CA-AB", "CA-BC", "CA-MB", "CA-NB", "CA-NL",
          "CA-NS", "CA-NT", "CA-NU", "CA-ON", "CA-PE",
          "CA-QC", "CA-SK", "CA-YT"
        ),
        region = c(
          "Alberta", "British Columbia", "Manitoba", "New Brunswick",
          "Newfoundland and Labrador", "Nova Scotia", "Northwest Territories",
          "Nunavut", "Ontario", "Prince Edward Island", "Quebec",
          "Saskatchewan", "Yukon"
        )
      )
      self$codes_lookup <- list("1" = canada_codes)
    },

    #' @description Provincial Level Data
    #' cleaning
    #' @param ... pass additional arguments
    #'
    #' @importFrom dplyr filter select mutate rename
    #' @importFrom tidyr replace_na
    #' @importFrom lubridate dmy
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        select(
          pruid, prname, date,
          numtoday, numtotal, numdeaths, numrecover, numtested
        ) %>%
        filter(pruid != 1) %>%
        select(-pruid) %>%
        mutate(
          prname = gsub(
            "Repatriated travellers",
            "Repatriated Travellers", prname
          ),
          date = dmy(date),
          numrecover = as.numeric(numrecover),
          numdeaths = as.numeric(numdeaths),
          numtotal = as.numeric(numtotal),
          numtoday = as.numeric(numtoday),
          numrecover = as.numeric(numrecover),
          numtested = as.numeric(numtested)
        ) %>%
        rename(
          level_1_region = prname,
          deaths_total = numdeaths,
          cases_total = numtotal,
          cases_new = numtoday,
          recovered_total = numrecover,
          tested_total = numtested
        ) %>%
        full_join(self$codes_lookup[[self$level]],
          by = c("level_1_region" = "region")
        ) %>%
        rename(
          level_1_region_code = code
        ) %>%
        replace_na(list(
          deaths_total = 0, cases_total = 0,
          recovered_total = 0, tested_total = 0
        ))
    }
  )
)
