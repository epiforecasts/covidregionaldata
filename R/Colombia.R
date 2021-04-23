#' Colombia Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Colombia
#'
# nolint start
#' @source \url{https://github.com/danielcs88/colombia_covid-19/}
# nolint end
#' @export
#' @concept dataset
#' @examples
#' \dontrun{
#' region <- Colombia$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Colombia <- R6::R6Class("Colombia",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Colombia",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "departamento"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data.
    # nolint start
    common_data_urls = list(
      "main" = "https://raw.githubusercontent.com/danielcs88/colombia_covid-19/master/datos/cronologia.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    #' @importFrom dplyr mutate
    set_region_codes = function() {
      self$codes_lookup$`1` <- colombia_codes
    },

    #' @description Colombia specific state level data cleaning
    #' @importFrom dplyr select mutate
    #' @importFrom lubridate ymd
    #' @importFrom stringr str_replace_all str_to_sentence str_to_title
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        select(
          date = .data$fecha,
          level_1_region = .data$departamento,
          cases_total = .data$casos
        ) %>%
        mutate(
          date = ymd(.data$date),
          level_1_region = iconv(.data$level_1_region,
            from = "UTF-8",
            to = "ASCII//TRANSLIT"
          ),
          level_1_region = str_replace_all(.data$level_1_region, " D.C.", ""),
          level_1_region = str_replace_all(
            .data$level_1_region,
            "San Andres y Providencia",
            "San Andres, Providencia y Santa Catalina"
          ),
          level_1_region = str_to_sentence(.data$level_1_region),
          level_1_region = str_to_title(.data$level_1_region)
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "level_1_region")
        )
    }
  )
)
