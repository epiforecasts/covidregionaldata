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
#' region <- SouthAfrica$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
SouthAfrica <- R6::R6Class("SouthAfrica",
  inherit = DataClass,
  public = list(
    # Core Attributes (amend each paramater for country specific infomation)
    #' @field country name of country to fetch data for
    country = "South Africa",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "province"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    common_data_urls = list(
      "main" = "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv" # nolint
    ),
    #' @field level_data_urls List of named links to raw data that are level
    #' specific.
    level_data_urls = list(
      "1" = list("deaths" = "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv") # nolint
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new", "recovered_new"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    #' @importFrom dplyr mutate
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "ZA-EC", "ZA-FS", "ZA-GP", "ZA-KZN", "ZA-LP",
          "ZA-MP", "ZA-NC", "ZA-NW", "ZA-WC"
        ),
        region = c(
          "Eastern Cape", "Free State", "Gauteng", "Kwazulu-Natal", "Limpopo",
          "Mpumalanga", "Northern Cape", "North-West", "Western Cape"
        )
      )
    },

    #' @description South Africa-specific function for downloading raw data.
    download = function() {
      message_verbose(self$verbose, "Downloading data")
      self$data$raw <- csv_reader(self$common_data_urls$main, self$verbose)
      self$data$deaths <- csv_reader(self$level_data_urls$deaths, self$verbose)
    },

    #' @description SouthAfrica specific state level data cleaning
    #' @importFrom dplyr mutate select arrange recode filter bind_rows na_if
    #' @importFrom tidyr pivot_longer pivot_wider
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- bind_rows(self$data$raw$main, self$data$deaths,
        .id = "data"
      ) %>%
        mutate(
          data = factor(.data$data, c(1, 2), c("cases_total", "deaths_total")),
          date = dmy(.data$date)
        ) %>%
        select(-c(YYYYMMDD, .data$total, .data$source)) %>%
        pivot_longer(-c(data, date), names_to = "level_1_region_code") %>%
        pivot_wider(names_from = data) %>%
        mutate(
          level_1_region_code = paste0("ZA-", level_1_region_code),
          level_1_region_code = na_if(level_1_region_code, "ZA-UNKNOWN")
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region_code" = "region")
        )
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      initialise_dataclass(self, ...)
    }
  )
)
