#' Netherlands Class for downloading, cleaning and processing notification data
#' @description Class for downloading, cleaning and processing COVID-19
#' sub-regional data for the Netherlands, provided by RVIM (English: National
#' Institute for Public Health and the Environment). This data contains number
#' of newly reported cases (that have tested positive), number of newly reported
#' hospital admissions and number of newly reported deaths going back to
#' 27/02/2020. Data is provided at both the province and municipality level.
#'
# nolint start
#' @source \url{https://data.rivm.nl/geonetwork/srv/dut/catalog.search#/metadata/5f6bc429-1596-490e-8618-1ed8fd768427?tab=relations}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Netherlands$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Netherlands <- R6::R6Class("Netherlands",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Netherlands",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "province", "2" = "municipality"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2", "2" = "CBS_code"),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    # nolint start
    common_data_urls = list(
      "main" = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),
    #' @field source_text Plain text description of the source of the data
    source_text = "National Institute for Public Health and the Environment (RIVM), Netherlands", # nolint
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://data.rivm.nl/covid-19/",

    #' @description Set up a table of region codes for clean data
    set_region_codes = function() {

    },

    #' @description Common cleaning steps to be applied to raw data, regardless
    #' of level (province or municipality) for raw Netherlands data.
    #' @importFrom dplyr mutate select
    #' @importFrom lubridate ymd
    #' @importFrom rlang .data
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        mutate(
          Date_of_publication = ymd(.data$Date_of_publication),
          Total_reported = as.double(.data$Total_reported),
          Deceased = as.double(.data$Deceased),
          level_1_region_code = sub("[a-z].*-", "", .data$Province),
          level_1_region_code = paste0(
            "NL-", toupper(substr(.data$level_1_region_code, 1, 2))
          )
        ) %>%
        select(
          date = .data$Date_of_publication,
          level_1_region = .data$Province,
          level_1_region_code,
          level_2_region = .data$Municipality_name,
          level_2_region_code = .data$Municipality_code,
          cases_new = .data$Total_reported,
          deaths_new = .data$Deceased #,
          #hosp_new = .data$Hospital_admission
        )
    },

    #' @description Netherlands specific province level data cleaning. Takes
    #' the data cleaned by `clean_common` and aggregates it to the Province
    #' level (level 1).
    #' @importFrom dplyr group_by summarise across
    #' @importFrom rlang .data
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        group_by(
          .data$date, .data$level_1_region_code,
          .data$level_1_region
        ) %>%
        summarise(across(where(is.double), sum), .groups = "drop")
    }
  )
)
