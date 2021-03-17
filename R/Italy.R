#' R6 Class containing country specific attributes and methods
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for Italy.
#'
#' @details Inherits from `dataClass`
#' @source https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv  # nolint
#'
Italy <- R6::R6Class("italy",
  inherit = dataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "regioni",
    #' @field level_2_region the level 2 region name.
    level_2_region = NA,
    #' @field data_url link to raw data
    data_url = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv", # nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total", "deaths_total", "tested_total"),

    #' @description Italy specific state level data cleaning
    #' @importFrom dplyr mutate select arrange recode group_by ungroup
    #' @importFrom lubridate as_date ymd_hms
    #' @importFrom rlang .data
    #'
    clean = function() {
      self$region$clean <- self$region$raw %>%
        mutate(
          date = as_date(ymd_hms(.data$data)),
          region_level_1 = as.character(.data$denominazione_regione),
          cases_total = .data$totale_casi,
          deaths_total = .data$deceduti,
          tested_total = .data$tamponi
        ) %>%
        arrange(.data$date) %>%
        mutate(region_level_1 = recode(.data$region_level_1,
          "P.A. Trento" = "Trentino-Alto Adige",
          "P.A. Bolzano" = "Trentino-Alto Adige"
        )) %>%
        group_by(.data$date, .data$region_level_1) %>%
        mutate(cases_total = sum(.data$cases_total, na.rm = TRUE)) %>%
        ungroup() %>%
        full_join(self$region$codes_lookup,
          by = c("region_level_1" = "region")
        ) %>%
        select(.data$date, .data$region_level_1,
          level_1_region_code = .data$code,
          .data$cases_total, .data$deaths_total, .data$tested_total
        )
    },

    #' @description Set up the country class with attributes set to input
    #' parameters
    #' @param level The region level for the data
    #' @param totals Boolean. If TRUE, returns totalled data per region up to
    #' today's date.
    #' @param localise Boolean. Should region names be localised.
    #' @param verbose Boolean. Display information at various stages.
    #' @param steps Boolean. Keep data from each processing step.
    initialize = function(level, totals, localise, verbose, steps) {
      self$level <- level
      self$totals <- totals
      self$localise <- localise
      self$verbose <- verbose
      self$steps <- steps
      self$country <- class(self)[1]
      self$get_region_codes()
    }
  )
)
