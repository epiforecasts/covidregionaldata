#' France Class containing country specific attributes and methods
#'
#' @description Country specific information for downloading, cleaning
#' and processing covid-19 region data for France.
#'
#' Data is sourced from \url{https://health-infobase.France.ca}.
#' @details Inherits from `DataClass`
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
    #' @field data_url link to raw data for cases
    data_url = "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675", # nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new",
                         "hosp_new", "tested_new"),
    #' @field hosp_url link to raw data for hospitalisations
    hosp_url = "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c", # nolint


    #' @description France-specific function for downloading raw data.
    download = function() {
      if (self$level == 1) {
        self$data_url <- "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01" # nolint
      }
      if (self$verbose) {
        message("Downloading data")
        self$data$raw <- suppressWarnings(
          csv_reader(self$data_url)
        )
        if (self$level == 2) {
          self$data$raw_hosp <- suppressWarnings(
            csv_reader(self$hosp_url)
          )
        }
      } else {
        self$data$raw <- suppressMessages(
          suppressWarnings(
            csv_reader(self$data_url)
          )
        )
        if (self$level == 2) {
          self$data$raw_hosp <- suppressMessages(
            csv_reader(self$hosp_url)
          )
        }
      }
    },

    #' @description *France* specific provincial/territorial level data
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
      if (self$verbose) {
        message("Cleaning data")
      }

      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description France Specific Region Level Data Cleaning
    #'
    #' @importFrom dplyr filter mutate left_join rename select
   clean_level_1 = function() {
      self$data$clean <- self$data$raw %>%
        dplyr::filter(cl_age90 == 0) %>%
        dplyr::select(
          date = jour,
          insee_code = reg,
          cases_new = P,
          tested_new = `T`
        ) %>%
        dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))  %>%
        dplyr::left_join(self$data$codes_lookup, by = "insee_code") %>%
        dplyr::rename("level_1_region_code" = "iso_code")
      },

      #' @description France Specific Department Level Data Cleaning
      #'
      #' @importFrom dplyr filter mutate left_join rename select full_join
      clean_level_2 = function() {
        cases_data <- self$data$raw %>%
          dplyr::filter(cl_age90 == 0) %>%
          dplyr::select(
            date = jour,
            level_2_region_code = dep,
            cases_new = P,
            tested_new = `T`
          ) %>%
          dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))

        hosp_data <- self$data$raw_hosp %>%
          dplyr::select(
            date = jour,
            level_2_region_code = dep,
            hosp_new = incid_hosp,
            deaths_new = incid_dc
          ) %>%
          dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))

      self$data$clean <- dplyr::full_join(cases_data, hosp_data,
        by = c("date", "level_2_region_code")) %>%
        dplyr::mutate(level_2_region_code =
                        paste0("FR-", level_2_region_code)) %>%
        dplyr::left_join(self$data$codes_lookup, by = "level_2_region_code")
    },


    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
      # Add custom fields here
    }
  )
)
