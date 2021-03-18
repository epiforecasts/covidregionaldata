#' R6 Class containing specific attributes and methods for WHO data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data from the World Health Organisation
#'
#' @details Inherits from `DataClass`
#' @source https://covid19.who.int/WHO-COVID-19-global-data.csv
#' @examples
#' \dontrun{
#' Who$new(
#'   level = "1", totals = FALSE,
#'   localise = FALSE, verbose = FALSE,
#'   steps = FALSE
#' )
#' }
WHO <- R6::R6Class("WHO",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "country",
    #' @field data_url link to raw data
    data_url = "https://covid19.who.int/WHO-COVID-19-global-data.csv",
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_new",
      "cases_total",
      "deaths_new",
      "deaths_total"
    ),

    #' @description WHO specific country level data cleaning
    #' @importFrom dplyr mutate rename
    #' @importFrom countrycode countrycode
    clean = function() {
      self$region$clean <- self$region$raw
      colnames(self$region$clean) <- c(
        "date", "iso_code", "country", "who_region",
        "cases_new", "cases_total", "deaths_new",
        "deaths_total"
      )
      self$region$clean <- self$region$clean %>%
        mutate(
          country = countrycode(.data$iso_code,
            origin = "iso2c", destination = "country.name.en", warn = FALSE
          ),
          un_region = countrycode(.data$iso_code,
            origin = "iso2c", destination = "un.region.name", warn = FALSE
          ),
          un_region = ifelse(.data$iso_code == "XK", "Europe", .data$un_region),
          country = ifelse(.data$iso_code == "XK", "Kosovo", .data$country)
        ) %>%
        rename(
          region_level_1 = .data$country,
          level_1_region_code = .data$iso_code
        )
    },

    #' @description Specific return settings for the WHO dataset.
    #' @importFrom dplyr group_by ungroup select arrange
    #' @importFrom tidyr fill
    return = function() {
      self$region$return <- self$region$processed %>%
        group_by(.data$country) %>%
        fill(.data$who_region, .data$un_region, .direction = "updown") %>%
        ungroup()

      self$region$return <- self$region$return %>%
        select(
          .data$date, .data$un_region, .data$who_region, .data$country,
          .data$iso_code, .data$cases_new, .data$cases_total,
          .data$deaths_new, .data$deaths_total, .data$recovered_new,
          .data$recovered_total, .data$hosp_new, .data$hosp_total,
          .data$tested_new, .data$tested_total
        ) %>%
        arrange(.data$date, .data$country)

      if (self$steps) {
        return(self$region)
      } else {
        return(self$region$return)
      }
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
      self$country <- tolower(class(self)[1])
      self$get_region_codes()
    }
  )
)
