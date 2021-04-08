#' R6 Class containing country specific attributes and methods
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for an example Country.
#'
#' @details Inherits from `DataClass`
#' @examples
#' \dontrun{
#' region <- CountryTemplate$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
CountryTemplate <- R6::R6Class("CountryTemplate", # rename to country name
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field country name of country to fetch data for
    country = "",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = NA),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = NA),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' sometimes only entry, should be named main
    common_data_urls = list(main = "url"),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("col_1", "col_2", "col_3", "etc."),

    #' @description *Country-name* data cleaning common across levels
    #'
    clean_commmon = function() {
    },

    #' @description *Country-name* data cleaning specific to spatial levels
    #'
    clean_level_1 = function() {
    }
  )
)
