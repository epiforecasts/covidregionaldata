#' R6 Class containing specific attributes and methods for Google data
#'
#' @description Google specific information for downloading, cleaning
#'  and processing covid-19 region data for an example Country. The function
#'  works the same as other national data sources, however, data from
#'  Google supports three subregions (country, subregion and subregion2) which
#'  can be accessed using the 'level' argument. There is also more data
#'  available, such as hospitalisations data. The raw data comes as three
#'  seperate data sets, "epidemiology" which is comprised of cases, tests and
#'  deaths, "index", which holds information about countries linking the other
#'  data sets, and "hospitalizations" which holds data about number of people
#'  in hospital, ICU, etc.
#'
#' @source \url{https://github.com/GoogleCloudPlatform/covid-19-open-data}
#' @export
#' @family aggregations
#' @family national
#' @family subnational
#' @concept dataset
#' @examples
#' # nolint start
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # get all countries
#' national <- Google$new(level = "1", get = TRUE)
#' national$return()
#'
#' # show available regions with data at the second level of interest
#' google_level_2 <- Google$new(level = "2")
#' google_level_2$download()
#' google_level_2$clean()
#' google$available_regions()
#'
#' # get all region data for the uk
#' google_level_2$filter("uk")
#' google_level_2$process()
#' google_level_2$return()
#'
#' # get all regional data for the UK
#' uk <- Google$new(regions = "uk", level = "2", get = TRUE)
#' uk$return()
#'
#' # get all subregional data for the UK
#' uk <- Google$new(regions = "uk", level = "3", get = TRUE)
#' uk$return()
#' }
#' # nolint end
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
    #' @field source_text Plain text description of the source of the data
    source_text = "O. Wahltinez et al.",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://github.com/GoogleCloudPlatform/covid-19-open-data",

    #' @description GoogleData specific subregion2 level data cleaning. This
    #' takes all the raw data, puts into a single data frame, renames some
    #' columns and checks types.
    #' @importFrom dplyr left_join select mutate rename everything
    #' @importFrom tidyr replace_na
    #' @importFrom lubridate ymd
    #' @importFrom rlang .data
    clean_common = function() {
      self$data$clean <- self$data$raw$epidemiology %>%
        left_join(
          self$data$raw$index,
          by = "key"
        )
      self$data$clean <- self$data$clean %>%
        full_join(
          self$data$raw$hospitalizations,
          by = c("key", "date")
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
          deaths_new = .data$new_deceased,
          deaths_total = .data$total_deceased,
          tested_new = .data$new_tested,
          tested_total = .data$total_tested,
          recovered_new = .data$new_recovered,
          recovered_total = .data$total_recovered,
          hosp_new = .data$new_hospitalized,
          hosp_total = .data$total_hospitalized
        ) %>%
        mutate(
          date = ymd(.data$date),
          cases_new = as.numeric(.data$cases_new),
          cases_total = as.numeric(.data$cases_total),
          deaths_new = as.numeric(.data$deaths_new),
          deaths_total = as.numeric(.data$deaths_total),
          recovered_new = as.numeric(.data$recovered_new),
          recovered_total = as.numeric(.data$recovered_total),
          tested_new = as.numeric(.data$tested_new),
          tested_total = as.numeric(.data$tested_total),
          hosp_new = as.numeric(.data$hosp_new),
          hosp_total = as.numeric(.data$hosp_total)
        ) %>%
        select(
          date,
          level_1_region,
          level_1_region_code,
          level_2_region,
          level_2_region_code,
          cases_new,
          cases_total,
          deaths_new,
          deaths_total,
          recovered_new,
          recovered_total,
          hosp_new,
          hosp_total,
          tested_new,
          tested_total,
          everything()
        )
    },

    #' @description Google specific subregion level data cleaning. Takes the
    #' data cleaned by `clean_common` and aggregates it to the country level
    #' (level 1).
    #' @importFrom dplyr select summarise group_by across everything
    #' @importFrom rlang .data
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        group_by(
          .data$date, .data$level_1_region_code,
          .data$level_1_region
        ) %>%
        summarise(across(where(is.double), sum), .groups = "drop")
    },

    #' @description Google specific subregion2 level data cleaning. Takes the
    #' data cleaned by `clean_common` and aggregates it to the subregion level
    #' (level 2).
    #' @importFrom dplyr select summarise group_by across everything
    #' @importFrom rlang .data
    clean_level_2 = function() {
      self$data$clean <- self$data$clean %>%
        group_by(
          .data$date, .data$level_1_region_code, .data$level_1_region,
          .data$level_2_region_code, .data$level_2_region
        ) %>%
        summarise(across(where(is.double), sum), .groups = "drop")
    },
    #' @description custom initialize for Google
    #' @param ... arguments to be passed to `DataClass` and initialize Google
    initialize = function(...) {
      super$initialize(...)
      if (self$level == "3" & is.null(self$target_regions)) {
        msg <- paste(
          "Processing google covid-19 data at level 3 with no target",
          "regions will take a long time and contain data for multiple",
          "countries. Consider not running the 'process' step or filtering",
          "to a specific country using the 'regions' argument."
        )
        message_verbose(self$verbose, msg)
      }
    }
  )
)
