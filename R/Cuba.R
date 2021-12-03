#' Cuba Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Cuba
#'
# nolint start
#' @source \url{https://covid19cubadata.github.io/}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Cuba$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Cuba <- R6::R6Class("Cuba",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Cuba",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "provincia"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data.
    # nolint start
    common_data_urls = list(
      "main" = "https://covid19cubadata.github.io/data/covid19-casos.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new"),
    #' @field source_text Plain text description of the source of the data
    source_text = "COVID19 Cuba Data team",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://covid19cubadata.github.io/#cuba",


    #' @description Set up a table of region codes for clean data
    #' @importFrom dplyr tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "CU-15", "CU-09", "CU-08", "CU-06", "CU-12", "CU-14", "CU-11",
          "CU-03", "CU-10", "CU-04", "CU-16", "CU-01", "CU-07", "CU-13",
          "CU-05", "CU-99"
        ),
        region = c(
          "Artemisa", "Camag\u00fcey", "Ciego de \u00c1vila", "Cienfuegos",
          "Granma", "Guant\u00e1namo", "Holgu\u00edn", "La Habana",
          "Las Tunas", "Matanzas", "Mayabeque", "Pinar del R\u00edo",
          "Sancti Sp\u00edritus", "Santiago de Cuba", "Villa Clara",
          "Isla de la Juventud"
        )
      )
    },

    #' @description Cuba specific state level data cleaning
    #' @importFrom dplyr count select filter mutate left_join rename
    #' @importFrom lubridate as_date ymd
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        count(.data$fecha_confirmacion, .data$provincia) %>%
        select(
          date = .data$fecha_confirmacion,
          level_1_region = .data$provincia,
          cases_new = .data$n
        ) %>%
        filter(!is.na(level_1_region)) %>%
        mutate(
          cases_new = as.numeric(.data$cases_new),
          date = as_date(ymd(.data$date))
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "region")
        ) %>%
        rename(
          level_1_region_code = .data$code,
        )
    }
  )
)
