#' Switzerland Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Switzerland
#'
#' @section Liechtenstein:
#' Liechtenstein is not a canton of Switzerland, but is presented in the
#' source data as a peer of Swiss cantons and assigned the two letter code
#' `FL`. `covidregionaldata` modifies this and presents the region code
#' for Liechtenstein as `FL-FL`, consistent with the Swiss ISO 3166-2 codes
#' which are of the form `CH-BE`, `CH-ZH`, `CH-VD`, ...
#'
#' If you do not wish to work with Liechtenstein
#' data, filter out on this code. Note that this is labelled as a ISO 3166-2
#' code but Liechtenstein's real ISO 3166-2 codes refer to sub-national
#' regions.
#'
#' @section Additional data:
#'
#' In addition to the standard `covidregionaldata` columns provided,
#' the OpenDataZH source data provides other figures for ICU occupancy,
#' number of patients on ventilators, and the how many individuals are
#' isolated or quarantined. These columns are passed through unchanged.

#' Further detail on them can be found at
# nolint start
#' \url{https://github.com/openZH/covid_19/#swiss-cantons-and-principality-of-liechtenstein-unified-dataset}
#' @source \url{https://github.com/openZH/covid_19/}
# nolint end
#'
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Switzerland$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Switzerland <- R6::R6Class("Switzerland",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Switzerland",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "canton"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data.
    # nolint start
    common_data_urls = list(
      "main" = "https://github.com/openZH/covid_19/raw/master/COVID19_Fallzahlen_CH_total_v2.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "hosp_new",
      "deaths_total",
      "recovered_total",
      "cases_total",
      "tested_total"
    ),
    #' @field source_text Plain text description of the source of the data
    source_text = "Open Data, Canton of Zurich",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://github.com/openZH/covid_19/",

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "CH-AG", "CH-AR", "CH-AI", "CH-BL", "CH-BS",
          "CH-BE", "CH-FR", "CH-GE", "CH-GL", "CH-GR", "CH-JU", "CH-LU",
          "CH-NE", "CH-NW", "CH-OW", "CH-SG", "CH-SH", "CH-SZ", "CH-SO",
          "CH-TG", "CH-TI", "CH-UR", "CH-VS", "CH-VD", "CH-ZG", "CH-ZH",
          "FL-FL"
        ),
        region = c(
          "Aargau", "Appenzell Ausserrhoden",
          "Appenzell Innerrhoden", "Basel-Landschaft",
          "Basel-Stadt", "Bern",
          "Fribourg", "Gen\u00e8ve",
          "Glarus", "Grisons",
          "Jura", "Luzern", "Neuch\u00e2tel", "Nidwalden",
          "Obwalden", "St. Gallen", "Schaffhausen",
          "Schwyz", "Solothurn", "Thurgau", "Ticino",
          "Uri", "Valais", "Vaud", "Zug",
          "Z\u00fcrich", "Liechtenstein"
        )
      )
    },

    #' @description Switzerland specific state level data cleaning
    #' @importFrom dplyr select filter mutate left_join rename
    #' @importFrom lubridate as_date ymd
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        select(-time, -source) %>%
        mutate(
          level_1_region_code = if_else(
            .data$abbreviation_canton_and_fl == "FL",
            "FL-FL",
            paste0("CH-", .data$abbreviation_canton_and_fl)
          ),
          date = as_date(ymd(.data$date))
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region_code" = "code")
        ) %>%
        select(-abbreviation_canton_and_fl) %>%
        rename(
          level_1_region = .data$region,
          cases_total = .data$ncumul_conf,
          deaths_total = .data$ncumul_deceased,
          hosp_new = .data$new_hosp,
          recovered_total = .data$ncumul_released,
          tested_total = .data$ncumul_tested
        )
    }
  )
)
