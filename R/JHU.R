#' R6 Class containing specific attributes and methods for John Hopkins
#' University data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for John Hopkins University.
# nolint start
#' @source \url{https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data}
# nolint end
#' @export
#' @concept dataset
#' @examples
#' \dontrun{
#' national <- JHU$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' national$return()
#' }
JHU <- R6::R6Class("JHU", # rename to country name
  inherit = CountryDataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field origin name of country to fetch data for
    origin = "John Hopkins University (JHU)",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "country", "2" = "region"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "iso_3166_1_alpha_3", "2" = "iso_code"
    ),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    # nolint start
    common_data_urls = list(
      "daily_confirmed" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
      "daily_deaths" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
      "daily_recovered" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("confirmed", "deaths", "recovered"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- JHU_codes
    },

    #' @description JHU specific data cleaning
    #' @importFrom dplyr last_col bind_rows mutate rename select
    #' @importFrom tidyr pivot_longer pivot_wider replace_na
    #' @importFrom lubridate mdy
    #' @importFrom rlang .data
    clean_common = function() {
      self$data$clean <- lapply(self$data$raw,
        pivot_longer,
        cols = 5:last_col(),
        names_to = "Date",
        values_to = "value"
      )
      self$data$clean <- self$data$clean %>%
        bind_rows(.id = "variable") %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        select(
          .data$Date,
          .data$`Province/State`,
          .data$`Country/Region`,
          .data$daily_confirmed,
          .data$daily_deaths,
          .data$daily_recovered
        ) %>%
        mutate(
          Date = mdy(.data$Date),
          daily_confirmed = as.numeric(.data$daily_confirmed),
          daily_deaths = as.numeric(.data$daily_deaths),
          daily_recovered = as.numeric(.data$daily_recovered),
          level_2_region_code = NA_real_
        ) %>%
        rename(
          date = .data$Date,
          level_2_region = .data$`Province/State`,
          level_1_region = .data$`Country/Region`,
          cases_total = .data$daily_confirmed,
          deaths_total = .data$daily_deaths,
          recovered_total = .data$daily_recovered
        ) %>%
        replace_na(
          list(
            level_1_region = "Unknown",
            level_2_region = "Unknown"
          )
        ) %>%
        full_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "Country_Region")
        )
    },

    #' @description JHU specific country level data cleaning
    #' @importFrom dplyr select summarise group_by
    #' @importFrom rlang .data
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        select(
          .data$date, .data$level_1_region_code, .data$level_1_region,
          .data$cases_total, .data$deaths_total, .data$recovered_total
        ) %>%
        group_by(
          .data$date, .data$level_1_region_code,
          .data$level_1_region
        ) %>%
        summarise(across(where(is.double), sum))
    },

    #' @description JHU specific state level data cleaning
    #' @importFrom dplyr select
    #' @importFrom rlang .data
    clean_level_2 = function() {
      self$data$clean <- self$data$clean %>%
        select(
          .data$date, .data$level_1_region_code, .data$level_1_region,
          .data$level_2_region_code, .data$level_2_region,
          .data$cases_total, deaths_total, .data$recovered_total
        )
    },

    #' @description Specific return settings for the JHU dataset.
    #' @importFrom dplyr group_by ungroup select arrange
    #' @importFrom tidyr fill
    return = function() {
      self$data$return <- self$data$processed
      if (!self$totals) {
        if (self$level == "1") {
          self$data$return <- self$data$return %>%
            select(
              .data$date, .data$iso_3166_1_alpha_3, .data$country,
              .data$cases_new, .data$cases_total,
              .data$deaths_new, .data$deaths_total, .data$recovered_new,
              .data$recovered_total, .data$hosp_new, .data$hosp_total,
              .data$tested_new, .data$tested_total
            ) %>%
            arrange(.data$date, .data$country)
        } else if (self$level == "2") {
          self$data$return <- self$data$return %>%
            select(
              .data$date, .data$iso_3166_1_alpha_3, .data$country,
              .data$iso_code, .data$region, .data$cases_new, .data$cases_total,
              .data$deaths_new, .data$deaths_total, .data$recovered_new,
              .data$recovered_total, .data$hosp_new, .data$hosp_total,
              .data$tested_new, .data$tested_total
            ) %>%
            arrange(.data$date, .data$country)
        }
      }

      if (self$steps) {
        return(self$data)
      } else {
        return(self$data$return)
      }
    }
  )
)
