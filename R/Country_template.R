#' R6 Class containing country specific attributes and methods
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for *Country-name*
#'
#' @details Inherits from `DataClass`
#' @examples
#' \dontrun{
#' Italy$new(
#'   level = "1", totals = FALSE,
#'   localise = FALSE, verbose = FALSE,
#'   steps = FALSE
#' )
#' }
CountryTemplate <- R6::R6Class("CountryTemplate", # rename to country name
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field level_1_region the level 1 region name.
    level_1_region = "name_of_level_1_regions", # add more levels as needed
    #' @field data_url link to raw data
    data_url = "link_to_some_raw_data",
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("col_1", "col_2", "col_3", "etc."),

    #' @description *Country-name* specific state level data cleaning
    #' @param ... pass additional arguments
    #'
    clean = function(...) {
      # function to clean the data (MUST BE CALLED clean)
      # modify the data variable 'region' in place and add using 'self'
      # e.g. self$region$clean <- something
      # No return statement is required
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
      # Add custom fields here
    }
  )
)
