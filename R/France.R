#' France Class containing origin specific attributes and methods
#'
#' @description Information for downloading, cleaning
#' and processing COVID-19 region data for France.
#'
# nolint start
#' @source \url{https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675}
#' @source \url{https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c}
#' @source \url{https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- France$new(level = "2", verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
France <- R6::R6Class("France",
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each paramater for origin specific infomation)
    #' @field origin name of origin to fetch data for
    origin = "France",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "region", "2" = "department"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2", "2" = "ons_region_code"),
    #' @field common_data_urls List of named links to raw data. This is empty
    #' as for France data is divided into cases by level
    common_data_urls = list(),
    # nolint start
    #' @field level_data_urls List of named links to raw data that are level
    #' specific.
    level_data_urls = list(
      "1" = list("cases" = "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"),
      "2" = list(
        "cases" = "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675",
        "hosp" = "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"
      )
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "tested_new"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    #' @importFrom dplyr select
    set_region_codes = function() {
      self$codes_lookup$`1` <- france_codes %>%
        select(
          .data$level_1_region_code,
          .data$level_1_region,
          .data$insee_code
        )
      self$codes_lookup$`2` <- france_codes
    },

    #' @description Region Level Data Cleaning
    #'
    #' @importFrom dplyr filter mutate left_join select
    #' @importFrom lubridate as_date ymd
    clean_level_1 = function() {
      self$data$clean <- self$data$raw$cases %>%
        filter(.data$cl_age90 == 0) %>%
        select(
          date = .data$jour,
          insee_code = .data$reg,
          cases_new = .data$P,
          tested_new = .data$`T`
        ) %>%
        mutate(date = as_date(ymd(date))) %>%
        left_join(
          self$codes_lookup$`1`,
          insee_code = reg,
          by = c("insee_code")
        )
    },

    #' @description Department Level Data Cleaning
    #'
    #' @importFrom dplyr filter mutate left_join rename select full_join
    #' @importFrom lubridate as_date ymd
    clean_level_2 = function() {
      cases_data <- self$data$raw$cases %>%
        filter(.data$cl_age90 == 0) %>%
        select(
          date = .data$jour,
          level_2_region_code = .data$dep,
          cases_new = .data$P,
          tested_new = .data$`T`
        ) %>%
        mutate(date = as_date(ymd(date)))

      if (!is.null(self$data$raw$hosp)) {
        hosp_data <- self$data$raw$hosp %>%
          select(
            date = jour,
            level_2_region_code = dep,
            hosp_new = incid_hosp,
            deaths_new = incid_dc
          ) %>%
          mutate(date = as_date(ymd(date)))

        combined_data <- full_join(cases_data, hosp_data,
          by = c("date", "level_2_region_code")
        )
      } else {
        combined_data <- cases_data
      }

      self$data$clean <- combined_data %>%
        mutate(
          level_2_region_code =
            paste0("FR-", level_2_region_code)
        ) %>%
        left_join(self$codes_lookup$`2`, by = "level_2_region_code")
    }
  )
)
