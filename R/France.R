#' France Class containing country specific attributes and methods
#'
#' @description Country specific information for downloading, cleaning
#' and processing covid-19 region data for France.
#'
#' @details Inherits from `DataClass`
#'
# nolint start
#' @source https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675
#' @source https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c
#' @source https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01
# nolint end
#' @export
#' @examples
#' \dontrun{
#' region <- France$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
France <- R6::R6Class("France",
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field level_1_region the level 1 region name.
    level_1_region = "region",
    #' @field level_2_region the level 1 region name.
    level_2_region = "departement",
    # nolint start
    #' @field data_url link to raw data for cases for level 1
    data_url = "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01", 
    #' @field level_2_data_url link to raw data for cases for level 2
    level_2_data_url = "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",
    #' @field level_2_hosp_url link to raw data for hospitalisations for level 2
    level_2_hosp_url = "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c",
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "tested_new"),

    #' @description France-specific function for downloading raw data.
    download = function() {
      message_verbose(self$verbose, "Downloading data")
      if (self$level == 1) {
        self$data$raw <- csv_reader(self$data_url, self$verbose)
      } else { # self$level is 2
        self$data$raw <- csv_reader(self$level_2_data_url, self$verbose)
        self$data$raw_hosp <- csv_reader(self$level_2_hosp_url, self$verbose)
      }
    },

    #' @description *France* specific provincial/territorial level data
    #' cleaning
    #' @param ... pass additional arguments
    #'
    clean = function(...) {
      # function to clean the data (MUST BE CALLED clean)
      # modify the data variable 'region' in place and add using 'self'
      # e.g. self$data$clean <- something
      # No return statement is required
      # have a statement like this to indicate information to user if requested
      message_verbose(self$verbose, "Cleaning data")

      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description France Specific Region Level Data Cleaning
    #'
    #' @importFrom dplyr filter mutate left_join select
    #' @importFrom lubridate as_date ymd
    clean_level_1 = function() {
      self$data$clean <- self$data$raw %>%
        filter(cl_age90 == 0) %>%
        select(
          date = jour,
          insee_code = reg,
          cases_new = P,
          tested_new = `T`
        ) %>%
        mutate(date = as_date(ymd(date)))  %>%
        left_join(self$data$codes_lookup, by = "insee_code")
      },

    #' @description France Specific Department Level Data Cleaning
    #'
    #' @importFrom dplyr filter mutate left_join rename select full_join
    #' @importFrom lubridate as_date ymd
    clean_level_2 = function() {
      cases_data <- self$data$raw %>%
        filter(cl_age90 == 0) %>%
        select(
          date = jour,
          level_2_region_code = dep,
          cases_new = P,
          tested_new = `T`
        ) %>%
        mutate(date = as_date(ymd(date)))

      if (!is.null(self$data$raw_hosp)) {
        hosp_data <- self$data$raw_hosp %>%
          select(
            date = jour,
            level_2_region_code = dep,
            hosp_new = incid_hosp,
            deaths_new = incid_dc
          ) %>%
          mutate(date = as_date(ymd(date)))

        combined_data <- full_join(cases_data, hosp_data,
          by = c("date", "level_2_region_code"))
      } else {
        combined_data <- cases_data
      }

      self$data$clean <- combined_data %>%
        mutate(level_2_region_code =
                paste0("FR-", level_2_region_code)) %>%
        left_join(self$data$codes_lookup, by = "level_2_region_code")
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
      # Add custom fields here
    }
  )
)
