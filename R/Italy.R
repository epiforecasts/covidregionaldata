#' Italy Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Italy.
#'
# nolint start
#' @source \url{https://github.com/pcm-dpc/COVID-19/}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
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
      "main" = "https://github.com/InPhyT/COVID19-Italy-Integrated-Surveillance-Data/raw/use_initial_conditions/epiforecasts_covidregionaldata/COVID19-Italy-Integrated-Surveillance-Data.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new", "hosp_new"),
    #' @field source_text Plain text description of the source of the data
    source_text = "Pietro Monticone and Claudio Moroni, Interdisciplinary Physics Team (InPhyT)",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://github.com/InPhyT/COVID19-Italy-Integrated-Surveillance-Data/blob/797ceacaee6e0e418715d235506d28656b8333ba/README.md",

    #' @description Set up a table of region codes for clean data
    #' @importFrom dplyr tibble
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
    #' @importFrom dplyr mutate select arrange recode group_by summarise
    #' @importFrom lubridate as_date ymd
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        group_by(date,region,indicator) %>%
        summarise(across(where(is.double), sum), .groups = "drop") %>%
        mutate(
          date = suppressWarnings(as_date(ymd(.data$date))),
          level_1_region = as.character(.data$region)
        ) %>%
        pivot_wider(
          id_cols = c("date", "level_1_region"),
          names_from = c("indicator"),
          values_from = c("count")
        ) %>%
        arrange(.data$date) %>%
        # The region of Trentino-Alto Adige is made up of two self-governing
        # provinces of Trento and Bolzano
        mutate(level_1_region = recode(.data$level_1_region,
          "P.A. Trento" = "Trentino-Alto Adige",
          "P.A. Bolzano" = "Trentino-Alto Adige"
        )) %>%
        full_join(self$codes_lookup[["1"]],
          by = c("level_1_region" = "region")
        ) %>%
        select(.data$date, .data$level_1_region,
          level_1_region_code = .data$code,
          cases_new = .data$confirmed,
          deaths_new = .data$deceased,
          hosp_new = .data$ordinary_hospital_admission,
          symp_new = .data$symptomatic,
          icu_new = .data$ICU_admission
        )
    }
  )
)
