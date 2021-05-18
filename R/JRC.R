#' R6 Class containing specific attributes and methods for European Commission's
#' Joint Research Centre data
#'
#' @description Information for downloading, cleaning and processing COVID-19
#' region data from the European Commission's Joint Research Centre
#'
#' @source \url{https://github.com/ec-jrc/COVID-19}
#' @concept dataset
#' @family national
#' @export
#' @examples
#' \dontrun{
#' national <- JRC$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' national$return()
#' }
JRC <- R6::R6Class("JRC",
  inherit = CountryDataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "European Commission's Joint Research Centre (JRC)",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "country", "2" = "region"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_code", "2" = "region_code"),
    #' @field level_data_urls List of named links to raw data.
    # nolint start
    level_data_urls = list(
      "1" = list(
        "country" = "https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-country/jrc-covid-19-all-days-by-country.csv"
      ),
      "2" = list(
        "region" = "https://raw.githubusercontent.com/ec-jrc/COVID-19/master/data-by-region/jrc-covid-19-all-days-by-regions.csv"
      )
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "CumulativePositive",
      "CuulativeDeceased",
      "CumulativeRecovered",
      "CurrentlyPositive",
      "Hospitalized",
      "IntensiveCare"
    ),

    #' @description JRC specific data cleaning
    #' @importFrom dplyr mutate rename
    #' @importFrom lubridate ymd
    #' @importFrom rlang .data
    clean_common = function() {
      self$data$clean <- self$data$raw[[names(self$data_urls)]] %>%
        mutate(
          Date = ymd(.data$Date),
          CumulativePositive = as.double(.data$CumulativePositive),
          CumulativeDeceased = as.double(.data$CumulativeDeceased),
          CumulativeRecovered = as.double(.data$CumulativeRecovered),
          CurrentlyPositive = as.double(.data$CurrentlyPositive),
          Hospitalized = as.double(.data$Hospitalized)
        ) %>%
        rename(
          date = Date,
          cases_total = CumulativePositive,
          deaths_total = CumulativeDeceased,
          recovered_total = CumulativeRecovered,
          hosp_total = Hospitalized,
          level_1_region = CountryName,
          level_1_region_code = iso3
        )
    },

    #' @description JRC specific country level data cleaning. Aggregates the
    #' data to the country (level 1) level.
    #' @importFrom dplyr select everything
    #' @importFrom rlang .data
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        select(
          date,
          level_1_region,
          level_1_region_code,
          cases_total,
          deaths_total,
          recovered_total,
          hosp_total,
          everything()
        )
    },

    #' @description JRC specific region level data cleaning. Aggregates the
    #' data to the region (level 2) level.
    #' @importFrom dplyr select everything
    #' @importFrom rlang .data
    clean_level_2 = function() {
      self$data$clean <- self$data$clean %>%
        rename(
          level_2_region = Region,
          level_2_region_code = NUTS
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
          hosp_total,
          everything()
        )
    }
  )
)
