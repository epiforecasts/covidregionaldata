#' R6 Class containing specific attributes and methods for Google data
#'
#' @description Google specific information for downloading, cleaning
#'  and processing covid-19 region data for an example Country.
#'
#' @source \url{https://github.com/GoogleCloudPlatform/covid-19-open-data}
#' @export
#' @concept dataset
#' @examples
#' \dontrun{
#' region <- Google$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Google <- R6::R6Class("Google",
  inherit = CountryDataClass,
  public = list(

    #' @field origin name of country to fetch data for
    origin = "Google",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2", "3"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list(
      "1" = "country",
      "2" = "subregion",
      "3" = "subregion2"
    ),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "iso_3166_1_alpha_3", "2" = "iso_code", "3" = "subregion2_code"
    ),
    #' @field common_data_urls List of named links to raw data.
    # nolint start
    common_data_urls = list(
      "epidemiology" = "https://storage.googleapis.com/covid19-open-data/v2/epidemiology.csv",
      "hospitalizations" = "https://storage.googleapis.com/covid19-open-data/v2/hospitalizations.csv",
      "index" = "https://storage.googleapis.com/covid19-open-data/v2/index.csv"
    ),
    # nolint end
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


    #' @description GoogleData specific state level data cleaning
    #' @importFrom dplyr full_join select mutate rename
    #' @importFrom tidyr replace_na
    #' @importFrom lubridate ymd
    #' @importFrom rlang .data
    clean_common = function() {
      self$data$clean <- self$data$raw$epidemiology %>%
        full_join(
          self$data$raw$index,
          by = "key"
        )
      self$data$clean <- self$data$clean %>%
        full_join(
          self$data$raw$hospitalizations,
          by = c("key", "date")
        ) %>%
        select(
          .data$date,
          .data$`3166-1-alpha-3`,
          .data$country_name,
          .data$subregion1_code,
          .data$subregion1_name,
          .data$subregion2_code,
          .data$subregion2_name,
          .data$new_confirmed,
          .data$total_confirmed,
          .data$new_deceased,
          .data$total_deceased,
          .data$new_recovered,
          .data$total_recovered,
          .data$new_tested,
          .data$total_tested,
          .data$new_hospitalized,
          .data$total_hospitalized
        ) %>%
        mutate(
          date = ymd(.data$date),
          new_confirmed = as.numeric(.data$new_confirmed),
          total_confirmed = as.numeric(.data$total_confirmed),
          new_deaths = as.numeric(.data$new_deceased),
          total_deaths = as.numeric(.data$total_deceased),
          new_recovered = as.numeric(.data$new_recovered),
          total_recovered = as.numeric(.data$total_recovered),
          new_tested = as.numeric(.data$new_tested),
          total_tested = as.numeric(.data$total_tested),
          new_hospitalized = as.numeric(.data$new_hospitalized),
          total_hospitalized = as.numeric(.data$total_hospitalized)
        ) %>%
        rename(
          level_1_region_code = .data$`3166-1-alpha-3`,
          level_1_region = .data$country_name,
          level_2_region_code = .data$subregion1_code,
          level_2_region = .data$subregion1_name,
          level_3_region_code = .data$subregion2_code,
          level_3_region = .data$subregion2_name,
          cases_new = .data$new_confirmed,
          cases_total = .data$total_confirmed,
          deaths_new = .data$new_deaths,
          deaths_total = .data$total_deaths,
          tested_new = .data$new_tested,
          tested_total = .data$total_tested,
          recovered_new = .data$new_recovered,
          recovered_total = .data$total_recovered,
          hosp_new = .data$new_hospitalized,
          hosp_total = .data$total_hospitalized
        ) %>%
        replace_na(
          list(
            level_1_region = "Unknown",
            level_2_region = "Unknown"
          )
        )
    },

    #' @description Google specific subregion level data cleaning
    #' @importFrom dplyr select summarise group_by across
    #' @importFrom rlang .data
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        select(
          .data$date,
          .data$level_1_region_code, .data$level_1_region,
          .data$cases_new, .data$cases_total,
          .data$deaths_new, .data$deaths_total,
          .data$tested_new, .data$tested_total,
          .data$recovered_new, .data$recovered_total,
          .data$hosp_new, .data$hosp_total
        ) %>%
        group_by(
          .data$date, .data$level_1_region_code,
          .data$level_1_region
        ) %>%
        summarise(across(where(is.double), sum))
    },

    #' @description JHU specific subregion2 level data cleaning
    #' @importFrom dplyr select summarise group_by across
    #' @importFrom rlang .data
    clean_level_2 = function() {
      self$data$clean <- self$data$clean %>%
        select(
          .data$date,
          .data$level_1_region_code, .data$level_1_region,
          .data$level_2_region_code, .data$level_2_region,
          .data$cases_new, .data$cases_total,
          .data$deaths_new, .data$deaths_total,
          .data$tested_new, .data$tested_total,
          .data$recovered_new, .data$recovered_total,
          .data$hosp_new, .data$hosp_total
        ) %>%
        group_by(
          .data$date, .data$level_1_region_code, .data$level_1_region,
          .data$level_2_region_code, .data$level_2_region
        ) %>%
        summarise(across(where(is.double), sum))
    }
  )
)
