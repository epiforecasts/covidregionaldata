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
    level_1_region = "level_1_region", # for brevity - refers to
    # provinces and territories
    #' @field data_url link to raw data for cases
    data_url = "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675", #nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "cases_total", "deaths_new", "recovered_total", "tested_new"),
    #' @field hosp_url link to raw data for hospitalisations
    hosp_url = "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c",


    #' @description France-specific function for downloading raw data.
    download = function() {
      if (self$verbose) {
        message("Downloading data")
        self$data$raw <- suppressWarnings(
          csv_reader(self$data_url)
        )
        self$data$raw_hosp <- suppressWarnings(
          csv_reader(self$hosp_url)
        )
      } else {
        self$data$raw <- suppressMessages(
          suppressWarnings(
            csv_reader(self$data_url)
          )
        )
        self$data$raw_hosp <- suppressMessages(
          csv_reader(self$hosp_url)
        )
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

      level_1_lookup <- self$get_region_codes_by_level(1)
      level_2_lookup <- self$get_region_codes_by_level(2)
      
      cases_data <- self$data$raw %>%
        dplyr::filter(cl_age90 == 0) %>%
        dplyr::select(
          date = jour,
          insee_code = dep,
          cases_new = P,
          tested_new = `T`
        ) %>%
        dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))  %>%
        dplyr::left_join(level_1_lookup, by = "insee_code") %>%
        dplyr::rename("level_1_region_code" = "iso_code")

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
        dplyr::mutate(level_2_region_code = paste0("FR-", level_2_region_code)) %>%
        dplyr::left_join(level_2_lookup, by = "level_2_region_code")

    },

    #' @description Return the region codes for a specific level
    #' 
    #' Intended for use in the France class where we need both sets
    #' at once.
    #' 
    #' @param level The level to provide region codes for
    get_region_codes_by_level = function(level = 1) {
      tar_level <- paste0("level_", level, "_region")
      tar_level_name <- self[[tar_level]]
      codes <- covidregionaldata::region_codes %>%
        filter(
          .data$country %in% self$country,
          .data$level %in% tar_level
        )
      
      if (nrow(codes) == 1) {
        codes_lookup <- codes$codes[[1]]
      }
      
      codes_lookup
    },
    
    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
      # Add custom fields here
    }
  )
)
