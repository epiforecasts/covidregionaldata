#' CountryTemplate Class for downloading, cleaning and processing
#' notification data
#'
#' @description Information for downloading, cleaning
#'  and processing covid-19 region data for CountryTemplate.
#'
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- CountryTemplate$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
CountryTemplate <- R6::R6Class("CountryTemplate",
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field origin name of country to fetch data for
    origin = "CountryTemplate",
    #' @field supported_levels List of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names List of region names in order of level.
    supported_region_names = list("1" = NA),
    #' @field supported_region_codes List of region codes in order of level.
    supported_region_codes = list("1" = NA),
    #' @field common_data_urls List of named links to raw data.
    common_data_urls = list(main = "url"),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("col_1", "col_2", "col_3", "etc."),

    #' @description Data cleaning common across levels
    #'
    clean_commmon = function() {
      self$data$clean <- self$data$raw[["main"]]
    },

    #' @description Data cleaning specific to level 1
    #'
    clean_level_1 = function() {
      self$data$clean <- self$data$clean
    }
  )
)
