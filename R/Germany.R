#' Germany Class for downloading, cleaning and processing notification data
#'
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region level 1 and 2 data for Germany.
#'
# nolint start
#' @source \url{https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Germany$new(verbose = TRUE, steps = TRUE, level = "2", get = TRUE)
#' region$return()
#' }
Germany <- R6::R6Class("Germany",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Germany",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "bundesland", "2" = "landkreis"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    # nolint start
    common_data_urls = list(
      "main" = "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),
    #' @field source_text Plain text description of the source of the data
    source_text = "Robert Koch-Institut (RKI)",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/explore", # nolint


    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    #' @importFrom dplyr mutate
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE", "DE-HH",
          "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL", "DE-SN",
          "DE-ST", "DE-TH"
        ),
        region = c(
          "Brandenburg", "Berlin", "Baden-W\u00FCrttemberg", "Bayern",
          "Bremen", "Hessen", "Hamburg", "Mecklenburg-Vorpommern",
          "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
          "Schleswig-Holstein", "Saarland", "Sachsen", "Sachsen-Anhalt",
          "Th\u00FCringen"
        )
      )
    },

    #' @description Common Data Cleaning
    #' @importFrom dplyr select mutate
    #' @importFrom lubridate as_date ymd_hms
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        select(
          date = .data$Meldedatum,
          level_1_region = .data$Bundesland,
          level_2_region = .data$Landkreis,
          cases_new = .data$AnzahlFall,
          deaths_new = .data$AnzahlTodesfall
        ) %>%
        mutate(date = as_date(ymd_hms(.data$date))) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "region")
        ) %>%
        mutate(
          level_1_region_code = .data$code,
        )
    },

    #' @description Bundesland Level Data Cleaning
    #' @importFrom dplyr group_by summarise ungroup full_join
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        group_by(
          .data$level_1_region, .data$level_1_region_code,
          .data$date
        ) %>%
        summarise(
          cases_new = as.numeric(sum(.data$cases_new)),
          deaths_new = as.numeric(sum(.data$deaths_new))
        ) %>%
        ungroup()
    },

    #' @description Landkreis Level Data Cleaning
    #' @importFrom dplyr mutate group_by summarise ungroup full_join
    #'
    clean_level_2 = function() {
      self$data$clean <- self$data$clean %>%
        mutate(
          level_2_region = gsub("(^[SL]K) (.*)", "\\2 \\(\\1\\)",
            .data$level_2_region,
            fixed = FALSE
          )
        ) %>%
        group_by(
          .data$level_1_region, .data$level_1_region_code,
          .data$level_2_region, .data$date
        ) %>%
        summarise(
          cases_new = as.numeric(sum(.data$cases_new)),
          deaths_new = as.numeric(sum(.data$deaths_new))
        ) %>%
        ungroup()
    }
  )
)
