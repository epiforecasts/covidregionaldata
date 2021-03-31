#' Canada Class containing country specific attributes and methods
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for Canada.
#'
#' Data is sourced from \url{https://health-infobase.canada.ca}.
#' @details Inherits from `DataClass`
#' @source https://health-infobase.canada.ca
#' @export
#' @examples
#' \dontrun{
#' region <- Canada$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
Canada <- R6::R6Class("Canada",
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each parameter for country specific information)
    #' @field level_1_region the level 1 region name.
    level_1_region = "level_1_region", # for brevity - refers to
    # provinces and territories
    #' @field data_url link to raw data
    data_url = list(
      "main" = "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv" # nolint
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_new", "cases_total", "deaths_new",
      "recovered_total", "tested_new"
    ),

    #' @description *Canada* specific provincial/territorial level data
    #' cleaning
    #' @param ... pass additional arguments
    #'
    #' @importFrom dplyr filter select mutate rename
    #' @importFrom tidyr replace_na
    #' @importFrom lubridate dmy
    clean = function(...) {
      # function to clean the data (MUST BE CALLED clean)
      # modify the data variable 'region' in place and add using 'self'
      # e.g. self$data$clean <- something
      # No return statement is required
      # have a statement like this to indicate information to user if requested
      message_verbose(self$verbose, "Cleaning data")

      self$data$clean <- self$data$raw %>%
        select(
          pruid, prname, date,
          numtoday, numtotal, numdeaths, numrecover, numtested
        ) %>%
        filter(pruid != 1) %>%
        select(-pruid) %>%
        mutate(
          prname = gsub(
            "Repatriated travellers",
            "Repatriated Travellers", prname
          ),
          date = dmy(date),
          numrecover = as.numeric(numrecover),
          numdeaths = as.numeric(numdeaths),
          numtotal = as.numeric(numtotal),
          numtoday = as.numeric(numtoday),
          numrecover = as.numeric(numrecover),
          numtested = as.numeric(numtested)
        ) %>%
        rename(
          region_level_1 = prname,
          deaths_total = numdeaths,
          cases_total = numtotal,
          cases_new = numtoday,
          recovered_total = numrecover,
          tested_total = numtested
        ) %>%
        full_join(self$data$codes_lookup,
          by = c("region_level_1")
        ) %>%
        replace_na(list(
          deaths_total = 0, cases_total = 0,
          recovered_total = 0, tested_total = 0
        ))
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
      # Add custom fields here
    }
  )
)
