#' R6 Class containing specific attributes and methods for John Hopkins
#' University data
#'
#' @description Attributes and methods for COVID-19 data used for the 2019
#' Novel Coronavirus Visual Dashboard operated by the Johns Hopkins University
#' Center for Systems Science and Engineering (JHU CSSE). Supported by ESRI
#' Living Atlas Team and the Johns Hopkins University Applied Physics Lab
#' (JHU APL)
#'
#' @details This dataset support both national and subnational data sources
#' with national level data returned by default. Subnational data is supported
#' for a subset of countries which can be found after cleaning using the
#' `available_regions()` method, see the examples for more details. These data
#' sets are sourced, cleaned, standardised by the JHU team so please see the
#' source repository for further details. Note that unlike many other data sets
#' this means methods applied to this source are not being applied to raw
#' surveillance data but instead to already cleaned data. If using for
#' analysis checking the JHU source for further details is advisable.
#'
#' If using this data please cite:
#' "Dong E, Du H, Gardner L. An interactive web-based dashboard to track
#' COVID-19 in real time.
#' Lancet Inf Dis. 20(5):533-534. doi: 10.1016/S1473-3099(20)30120-1"
# nolint start
#' @source \url{https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data}
# nolint end
#' @export
#' @concept dataset
#' @family aggregations
#' @family national
#' @family subnational
#' @examples
#' # nolint start
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # get all countries data
#' jhu <- JHU$new(level = "1", get = TRUE)
#' jhu$return()
#'
#' # show available regions with data at the second level of interest
#' jhu_level_2 <- JHU$new(level = "2")
#' jhu_level_2$download()
#' jhu_level_2$clean()
#' jhu$available_regions()
#'
#' # get all region data for the uk
#' jhu_level_2$filter("uk")
#' jhu_level_2$process()
#' jhu_level_2$return()
#' }
#' # nolint end
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

    #' @description JHU specific data cleaning. Joins the raw data sets, checks
    #' column types and renames where needed.
    #' @importFrom dplyr last_col bind_rows mutate rename select everything
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
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "Country_Region")
        ) %>%
        select(
          date,
          level_1_region,
          level_1_region_code,
          level_2_region,
          level_2_region_code,
          cases_total,
          deaths_total,
          recovered_total,
          everything()
        )
    },

    #' @description JHU specific country level data cleaning. Aggregates the
    #' data to the country (level 2) level.
    #' @importFrom dplyr select summarise group_by across everything
    #' @importFrom rlang .data
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        group_by(
          .data$date, .data$level_1_region_code,
          .data$level_1_region
        ) %>%
        summarise(across(where(is.double), sum), .groups = "drop")
    }
  )
)
