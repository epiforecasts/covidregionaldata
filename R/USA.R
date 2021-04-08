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
#' region <- USA$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
USA <- R6::R6Class("USA",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field country name of country to fetch data for
    country = "United States of America (USA)",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list(
      "1" = "state",
      "2" = "county"
    ),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "iso_3166_2",
      "2" = "fips"
    ),
    #' @field common_data_urls List of named links to raw data. Since there are
    #'  no common data sources for level 1 and 2, this list is empty.
    common_data_urls = list(),
    #' @field level_data_urls List of named links to raw data that are level
    #' specific.
    level_data_urls = list(
      "1" = list("1" = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"), # nolint
      "2" = list("2" = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") # nolint
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total", "deaths_total"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tribble
    set_region_codes = function() {
      self$codes_lookup$`1` <- usa_codes <- tibble(
        level_1_region_code = c(
          "US-AL", "US-AK", "US-AZ", "US-AR", "US-CA", "US-CO", "US-CT",
          "US-DE", "US-FL", "US-GA", "US-HI", "US-ID", "US-IL", "US-IN",
          "US-IA", "US-KS", "US-KY", "US-LA", "US-ME", "US-MD", "US-MA",
          "US-MI", "US-MN", "US-MS", "US-MO", "US-MT", "US-NE", "US-NV",
          "US-NH", "US-NJ", "US-NM", "US-NY", "US-NC", "US-ND", "US-OH",
          "US-OK", "US-OR", "US-PA", "US-RI", "US-SC", "US-SD", "US-TN",
          "US-TX", "US-UT", "US-VE", "US-VA", "US-WA", "US-WV", "US-WI",
          "US-WY", "US-DC", "US-AS", "US-GU", "US-MP", "US-PR", "US-UM",
          "US-VI"
        ),
        region = c(
          "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
          "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
          "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
          "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
          "New Hampshire", "New Jersey", "New Mexico", "New York",
          "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
          "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
          "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
          "West Virginia", "Wisconsin", "Wyoming", "District of Columbia",
          "American Samoa", "Guam", "Northern Mariana Islands", "Puerto Rico",
          "Minor Outlying Islands", "Virgin Islands"
        )
      )
      self$codes_lookup$`2` <- self$codes_lookup$`1`
    },

    #' @description USA Specific State Level Data Cleaning
    #' @importFrom dplyr rename mutate select left_join
    clean_level_1 = function() {
      self$data$clean <- self$data$raw$`1` %>%
        rename(
          level_1_region = state,
          cases_total = cases,
          deaths_total = deaths
        ) %>%
        mutate(
          cases_total = replace(cases_total, cases_total < 0, 0),
          deaths_total = replace(deaths_total, deaths_total < 0, 0)
        ) %>%
        select(date, level_1_region, cases_total, deaths_total) %>%
        left_join(self$codes_lookup[[1]],
          by = c("level_1_region" = "region"),
          copy = TRUE
        )
    },

    #' @description USA Specific County Level Data Cleaning
    #' @importFrom dplyr mutate rename left_join
    clean_level_2 = function() {
      self$data$clean <- self$data$raw$`2` %>%
        rename(
          level_2_region = county,
          level_1_region = state,
          level_2_region_code = fips,
          cases_total = cases,
          deaths_total = deaths
        ) %>%
        mutate(
          cases_total = replace(cases_total, cases_total < 0, 0),
          deaths_total = replace(deaths_total, deaths_total < 0, 0)
        ) %>%
        left_join(self$codes_lookup[[2]],
          by = c("level_1_region" = "region"),
          copy = TRUE
        )
    }
  )
)
