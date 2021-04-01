#' R6 Class containing specific attributes and methods for Google data
#'
#' @description Google data specific information for downloading, cleaning
#'  and processing covid-19 region data for an example Country.
#'
#' @details Inherits from `DataClass`
#' @examples
#' \dontrun{
#' region <- Italy$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
GoogleData <- R6::R6Class("GoogleData",
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field level_1_region the level 1 region name.
    level_1_region = "country", # add more levels as needed
    #' @field level_2_region the level 2 region name
    level_2_region = "subregion",
    #' @field data_url List of named links to raw data. The first, and
    #' sometimes only entry, should be named main
    data_url = list(
      main = "https://storage.googleapis.com/covid19-open-data/v2/" # nolint
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "new_confirmed",
      "new_deceased",
      "new_recovered",
      "new_tested",
      "total_confirmed",
      "total_deceased",
      "total_recovered",
      "total_tested"
    ),

    #' @description Custom download function
    #' @importFrom purrr map
    download = function() {
      vals <- c("epidemiology", "index")
      paths <- paste0(
        self$data_url$main,
        vals,
        ".csv"
      )
      data_list <- map(paths, csv_reader)
      names(data_list) <- vals
      self$data$raw <- data_list
    },


    #' @description *Country-name* specific state level data cleaning
    #' @param ... pass additional arguments
    #'
    clean = function(...) {
      message_verbose(self$verbose, "Cleaning data")
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
      # Add custom fields here
    }
  )
)
