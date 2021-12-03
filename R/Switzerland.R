#' Switzerland Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Switzerland
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
    #' @field common_data_urls List of named links to raw data. This url links
    #' to a JSON file which provides the addresses for the most recently-updated
    #' CSV files, which are then downloaded.
    common_data_urls = list(
      "main" = "https://www.covid19.admin.ch/api/data/context"
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "hosp_new",
      "deaths_total",
      "recovered_total",
      "cases_total",
      "tested_total"
    ),
    #' @field source_text Plain text description of the source of the data
    source_text = "Swiss Federal Office of Public Health FOPH",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://www.covid19.admin.ch/en/overview",

    #' @description Set up a table of region codes for clean data
    #' @importFrom dplyr tibble
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

    #' @description Download function to get raw data. Downloads
    #' the updated list of CSV files using `download_JSON`, filters
    #' that to identify the required CSV files, then uses the parent
    #' method `download` to download the CSV files.
    #' @importFrom purrr keep
    download = function() {
      message_verbose(
        self$verbose,
        paste0("Downloading updated URLs from ", self$common_data_urls$main))

      super$download_JSON()

      self$data_urls <-
        self$data$raw$main$data$sources$individual$csv$daily %>%
        keep(names(.) %in% c("cases", "test", "death", "hosp"))

      super$download()
    },

    #' @description Switzerland specific state level data cleaning
    #' @importFrom dplyr select filter mutate left_join rename full_join
    #' @importFrom lubridate as_date ymd
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      cases <- self$data$raw$cases %>%
        filter(geoRegion != "CH", geoRegion != "CHFL", datum_unit == "day") %>%
        select(geoRegion, datum, entries, sumTotal) %>%
        rename(level_1_region_code = geoRegion,
               date = datum,
               cases_new = entries,
               cases_total = sumTotal)
      hosp <- self$data$raw$hosp %>%
        filter(geoRegion != "CH", geoRegion != "CHFL", datum_unit == "day") %>%
        select(geoRegion, datum, entries, sumTotal) %>%
        rename(level_1_region_code = geoRegion,
               date = datum,
               hosp_new = entries,
               hosp_total = sumTotal)
      deaths <- self$data$raw$death %>%
        filter(geoRegion != "CH", geoRegion != "CHFL", datum_unit == "day") %>%
        select(geoRegion, datum, entries, sumTotal) %>%
        rename(level_1_region_code = geoRegion,
               date = datum,
               deaths_new = entries,
               deaths_total = sumTotal)
      tests <- self$data$raw$test %>%
        filter(geoRegion != "CH", geoRegion != "CHFL", datum_unit == "day") %>%
        # note that the data has entries_pos and entries_neg and we're
        # currently not using it.
        select(geoRegion, datum, entries, sumTotal) %>%
        rename(level_1_region_code = geoRegion,
               date = datum,
               tested_new = entries,
               tested_total = sumTotal)

      self$data$clean <-
        full_join(cases, deaths, by = c("date", "level_1_region_code")) %>%
        full_join(hosp, by = c("date", "level_1_region_code")) %>%
        full_join(tests, by = c("date", "level_1_region_code")) %>%
        mutate(
          level_1_region_code = if_else(
            .data$level_1_region_code == "FL",
            "FL-FL",
            paste0("CH-", .data$level_1_region_code)
          ),
          date = as_date(ymd(.data$date))
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region_code" = "code")
        ) %>%
        rename(level_1_region = region)
    }
  )
)
