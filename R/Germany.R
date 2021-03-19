#' Germany Class for downloading, cleaning and processing notification data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing COVID-19 region level 1 and 2 data for Germany.
#'
#' @details Inherits from `DataClass`
#' @source https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv  # nolint
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

    #' @description directs to either level 1 or level 2 processing based on
    #' request.
    #' @importFrom dplyr select mutate
    #' @importFrom lubridate as_date ymd_hms
    clean = function() {
      self$region$clean <- self$region$raw %>%
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
      self$region$clean <- self$region$clean %>%
        group_by(.data$region_level_1, .data$date) %>%
        summarise(
          cases_new = as.numeric(sum(.data$cases_new > 0)),
          deaths_new = as.numeric(sum(.data$deaths_new > 0))
        ) %>%
        ungroup() %>%
        full_join(self$region$codes_lookup, by = "region_level_1")
    },

    #' @description Germany Specific Landkreis Level Data Cleaning
    #' @importFrom dplyr mutate group_by summarise ungroup full_join
    #'
    clean_level_2 = function() {
      self$region$clean <- self$region$clean %>%
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
        full_join(self$region$codes_lookup, by = "region_level_1")
    },

    #' @description Set up the country class with attributes set to input
    #' parameters
    #' @param level The region level for the data
    #' @param totals Boolean. If TRUE, returns totalled data per region up to
    #' today's date.
    #' @param localise Boolean. Should region names be localised.
    #' @param verbose Boolean. Display information at various stages.
    #' @param steps Boolean. Keep data from each processing step.
    initialize = function(level = "1", totals = FALSE, localise = TRUE,
                          verbose = FALSE, steps = FALSE) {
      self$level <- level
      self$totals <- totals
      self$localise <- localise
      self$verbose <- verbose
      self$steps <- steps
      self$country <- tolower(class(self)[1])
      self$get_region_codes()
    }
  )
)