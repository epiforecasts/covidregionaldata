#' Germany Class for downloading, cleaning and processing notification data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing COVID-19 region level 1 and 2 data for Germany.
#'
#' @details Inherits from `DataClass`
#' @source https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv  # nolint
#' @export
#' @examples
#' \dontrun{
#' region <- Germany$new(verbose = TRUE, steps = TRUE, level = "2")
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
Germany <- R6::R6Class("Germany",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "bundesland",
    #' @field level_2_region the level 2 region name.
    level_2_region = "landkreis",
    #' @field data_url link to raw data
    data_url = "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv", # nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    #' @importFrom dplyr mutate
    set_region_codes = function() {
      message_verbose(
        self$verbose,
        paste(
          "Getting region codes for",
          self$country
        )
      )
      level_1_germany <- tibble(
        level_1_region_code = c(
          "DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE", "DE-HH",
          "DE-MV", "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL", "DE-SN",
          "DE-ST", "DE-TH"
        ),
        region_level_1 = c(
          "Brandenburg", "Berlin", "Baden-W\u00FCrttemberg", "Bayern",
          "Bremen", "Hessen", "Hamburg", "Mecklenburg-Vorpommern",
          "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
          "Schleswig-Holstein", "Saarland", "Sachsen", "Sachsen-Anhalt",
          "Th\u00FCringen"
        )
      )
      germany_codes <- tibble(
        country = "germany",
        level = c("level_1_region", "level_2_region"),
        name = c("iso_3166_2", "code"),
        codes = list(
          level_1_germany,
          mutate(
            level_1_germany,
            level_2_region_code = NA,
          )
        )
      )
      self$region_codes <- germany_codes
    },

    #' @description directs to either level 1 or level 2 processing based on
    #' request.
    #' @importFrom dplyr select mutate
    #' @importFrom lubridate as_date ymd_hms
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")
      self$data$clean <- self$data$raw %>%
        select(
          date = .data$Meldedatum,
          region_level_1 = .data$Bundesland,
          region_level_2 = .data$Landkreis,
          cases_new = .data$AnzahlFall,
          deaths_new = .data$AnzahlTodesfall
        ) %>%
        mutate(date = as_date(ymd_hms(.data$date)))

      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description Germany Specific Bundesland Level Data Cleaning
    #' @importFrom dplyr group_by summarise ungroup full_join
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        group_by(.data$region_level_1, .data$date) %>%
        summarise(
          cases_new = as.numeric(sum(.data$cases_new > 0)),
          deaths_new = as.numeric(sum(.data$deaths_new > 0))
        ) %>%
        ungroup() %>%
        full_join(self$data$codes_lookup, by = "region_level_1")
    },

    #' @description Germany Specific Landkreis Level Data Cleaning
    #' @importFrom dplyr mutate group_by summarise ungroup full_join
    #'
    clean_level_2 = function() {
      self$data$clean <- self$data$clean %>%
        mutate(
          region_level_2 = gsub("(^[SL]K) (.*)", "\\2 \\(\\1\\)",
            .data$region_level_2,
            fixed = FALSE
          )
        ) %>%
        group_by(.data$region_level_1, .data$region_level_2, .data$date) %>%
        summarise(
          cases_new = as.numeric(sum(.data$cases_new > 0)),
          deaths_new = as.numeric(sum(.data$deaths_new > 0))
        ) %>%
        ungroup() %>%
        full_join(self$data$codes_lookup, by = "region_level_1")
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  )
)
