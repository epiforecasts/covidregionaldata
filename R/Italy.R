#' Italy Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Italy.
#'
# nolint start
#' @source \url{https://github.com/pcm-dpc/COVID-19/}
# nolint end
#' @export
#' @concept dataset
#' @examples
#' \dontrun{
#' region <- Italy$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Italy <- R6::R6Class("Italy",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Italy",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "regioni"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    # nolint start
    common_data_urls = list(
      "main" = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total", "deaths_total", "tested_total"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "IT-21", "IT-23", "IT-25", "IT-32", "IT-34", "IT-36", "IT-42",
          "IT-45", "IT-52", "IT-55", "IT-57", "IT-62", "IT-65", "IT-67",
          "IT-72", "IT-75", "IT-77", "IT-78", "IT-82", "IT-88"
        ),
        region = c(
          "Piemonte", "Valle d'Aosta", "Lombardia", "Trentino-Alto Adige",
          "Veneto", "Friuli Venezia Giulia", "Liguria", "Emilia-Romagna",
          "Toscana", "Umbria", "Marche", "Lazio", "Abruzzo",
          "Molise", "Campania", "Puglia", "Basilicata",
          "Calabria", "Sicilia", "Sardegna"
        )
      )
    },

    #' @description State level data cleaning
    #' @importFrom dplyr mutate select arrange recode group_by ungroup
    #' @importFrom lubridate as_date ymd_hms
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        mutate(
          date = suppressWarnings(as_date(ymd_hms(.data$data))),
          level_1_region = as.character(.data$denominazione_regione),
          cases_total = .data$totale_casi,
          deaths_total = .data$deceduti,
          tested_total = .data$tamponi
        ) %>%
        arrange(.data$date) %>%
        mutate(level_1_region = recode(.data$level_1_region,
          "P.A. Trento" = "Trentino-Alto Adige",
          "P.A. Bolzano" = "Trentino-Alto Adige"
        )) %>%
        group_by(.data$date, .data$level_1_region) %>%
        summarise(
          cases_total = sum(.data$cases_total, na.rm = TRUE),
          deaths_total = sum(.data$deaths_total, na.rm = TRUE),
          tested_total = sum(.data$tested_total, na.rm = TRUE),
        ) %>%
        ungroup() %>%
        full_join(self$codes_lookup[["1"]],
          by = c("level_1_region" = "region")
        ) %>%
        select(.data$date, .data$level_1_region,
          level_1_region_code = .data$code,
          .data$cases_total, .data$deaths_total, .data$tested_total
        )
    }
  )
)
