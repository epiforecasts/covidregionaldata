#' Switzerland Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Switzerland
#'
# nolint start
#' @source \url{https://github.com/openZH/covid_19/}
# nolint end
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

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "CH-AG", "CH-AR", "CH-AI", "CH-BL", "CH-BS",
          "CH-BE", "CH-FR", "CH-GE", "CH-GL", "CH-GR", "CH-JU", "CH-LU",
          "CH-NE", "CH-NW", "CH-OW", "CH-SG", "CH-SH", "CH-SZ", "CH-SO",
          "CH-TG", "CH-TI", "CH-UR", "CH-VS", "CH-VD", "CH-ZG", "CH-ZH"
        ),
        region = c(
          "Aargau (de)", "Appenzell Ausserrhoden (de)",
          "Appenzell Innerrhoden (de)", "Basel-Landschaft (de)",
          "Basel-Stadt (de)", "Bern (de), Berne (fr)",
          "Fribourg (fr), Freiburg (de)", "Gen\u00e8ve (fr)",
          "Glarus (de)", "Graub\u00fcnden (de), Grigioni (it), Grischun (rm)",
          "Jura (fr)", "Luzern (de)", "Neuch\u00e2tel (fr)", "Nidwalden (de)",
          "Obwalden (de)", "Sankt Gallen (de)", "Schaffhausen (de)",
          "Schwyz (de)", "Solothurn (de)", "Thurgau (de)", "Ticino (it)",
          "Uri (de)", "Valais (fr), Wallis (de)", "Vaud (fr)", "Zug (de)",
          "Z\u00fcrich (de)"
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
        # Data for Liechtenstein is coded as being part of Switzerland
        # in the source data set. Here it is filtered out since it is
        # only available at the national level here, but treated as
        # equivalent to level 1 regional data in the source.
        filter(.data$abbreviation_canton_and_fl != "FL") %>%
        mutate(
          level_1_region_code =
            paste0("CH-", .data$abbreviation_canton_and_fl)
        ) %>%
        mutate(
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
