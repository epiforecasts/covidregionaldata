#' Italy Class for downloading, cleaning and processing notification data
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for Italy.
#'
#' @details Inherits from `DataClass`
#' @source https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv  # nolint
#' @export
#' @examples
#' \dontrun{
#' region <- Italy$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
Italy <- R6::R6Class("Italy",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "regioni",
    #' @field data_url link to raw data
    data_url = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv", # nolint
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total", "deaths_total", "tested_total"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      message_verbose(
        self$verbose,
        paste(
          "Getting region codes for",
          self$country
        )
      )
      region_data <- covidregionaldata::italy_codes
      self$region_codes <- by(region_data, region_data$level, function(x) x)
      self$code_name <- unique(region_data$name)
    },

    #' @description Italy specific state level data cleaning
    #' @importFrom dplyr mutate select arrange recode group_by ungroup
    #' @importFrom lubridate as_date ymd_hms
    #' @importFrom rlang .data
    #'
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")
      self$data$clean <- self$data$raw %>%
        mutate(
          date = suppressWarnings(as_date(ymd_hms(.data$data))),
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
        full_join(self$region_codes[["level_1_region"]],
          by = c("region_level_1" = "region")
        ) %>%
        select(.data$date, .data$region_level_1,
          level_1_region_code = .data$code,
          .data$cases_total, .data$deaths_total, .data$tested_total
        )
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  )
)
