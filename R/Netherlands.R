#' Netherlands Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for the Netherlands.
#'
# nolint start
#' @source \url{https://coronadashboard.government.nl/verantwoording#vaccination}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Netherlands$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Netherlands <- R6::R6Class("Netherlands",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Netherlands",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "regioni"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    # nolint start
    common_data_urls = list(
      "main" = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total", "deaths_total", "tested_total"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {

    },

    #' @description State level data cleaning
    #' @importFrom dplyr mutate select arrange recode group_by ungroup
    #' @importFrom lubridate as_date ymd_hms
    #' @importFrom rlang .data
    #'
    clean_common = function() {

    }
  )
)
