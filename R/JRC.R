#' R6 Class containing specific attributes and methods for European Commission's
#' Joint Research Centre data
#'
#' @description Class for downloading, cleaning and processing COVID-19
#' region data from the European Commission's Joint Research Centre. Subnational
#' data (admin level 1) on numbers of contagious and fatalities by COVID-19,
#' collected directly from the National Authoritative sources (National
#' monitoring websites, when available). For more details see
#' https://github.com/ec-jrc/COVID-19
#'
#' @source \url{https://github.com/ec-jrc/COVID-19}
#' @concept dataset
#' @family national
#' @export
#' @examples
#' \dontrun{
#' # get country level data
#' jrc_level_1 <- JRC$new(level = "1", verbose = TRUE, steps = TRUE, get = TRUE)
#' jrc_level_1$return()
#'
#' # show available regions with data at the first level of interest (country)
#' jrc_level_1$available_regions()
#'
#' # get region level data
#' jrc_level_2 <- JRC$new(level = "2", verbose = TRUE, steps = TRUE, get = TRUE)
#' jrc_level_2$return()
#'
#' # show available regions with data at the second level of interest (region)
#' jrc_level_2$available_regions()
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
    #' @field source_text Plain text description of the source of the data
    source_text = "European Commission Joint Research Centre (JRC)",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://github.com/ec-jrc/COVID-19",


    #' @description JRC specific data cleaning. The raw source data columns are
    #' converted to the correct type and renamed appropriately to match the
    #' standard for general processing.
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

    #' @description JRC specific country level data cleaning. Selects country
    #' level (level 1) columns from the data ready for further processing.
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

    #' @description JRC specific region level data cleaning. Selects country
    #' (level 1) and region (level 2) columns from the data ready for further
    #' processing.
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
