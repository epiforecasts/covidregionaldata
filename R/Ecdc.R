#' R6 Class containing specific attributes and methods for ECDC dataset
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for ECDC
#'
#' @details Inherits from `dataClass`
#'
Ecdc <- R6::R6Class("ecdc",
  inherit = dataClass,
  public = list(

    # Core Attributes
    #' @field level_1_region the level 1 region name.
    level_1_region = "country",
    #' @field level_2_region the level 2 region name.
    level_2_region = NA, # If no level 2 regions, set as NA
    #' @field data_url link to raw data
    data_url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),

    #' @description ECDC specific state level data cleaning
    #' @param ... pass additional arguments
    #' @importFrom dplyr mutate rename select arrange filter
    #' @importFrom stringr str_replace_all
    #' @importFrom countrycode countryname countrycode
    #'
    clean = function(...) {
      long_string <- "Cases_on_an_international_conveyance_Japan"
      self$region$clean <- self$region$raw %>%
        mutate(date = as.Date(.data$dateRep, format = "%d/%m/%Y")) %>%
        rename(
          iso_code = .data$geoId, country = .data$countriesAndTerritories,
          cases_new = .data$cases, deaths_new = .data$deaths,
          population_2019 = .data$popData2019
        ) %>%
        select(
          .data$date, .data$country, .data$iso_code,
          .data$population_2019, .data$cases_new, .data$deaths_new
        ) %>%
        arrange(.data$date) %>%
        filter(.data$country != long_string) %>%
        mutate(
          cases_new = ifelse(.data$cases_new < 0, 0, .data$cases_new),
          country = str_replace_all(.data$country, "_", " "),
          country = countryname(.data$country,
            destination = "country.name.en",
            warn = FALSE
          ),
          iso_code = ifelse(.data$country == "Namibia", "NA", .data$iso_code),
          un_region = countrycode(.data$iso_code,
            origin = "iso2c",
            destination = "un.region.name",
            warn = FALSE
          ),
          un_region = ifelse(.data$iso_code == "XK", "Europe",
            .data$un_region
          ),
          un_region = ifelse(.data$iso_code == "UK", "Europe",
            .data$un_region
          ),
          un_region = ifelse(.data$iso_code == "EL", "Europe",
            .data$un_region
          ),
          un_region = ifelse(.data$iso_code == "TW", "Asia",
            .data$un_region
          )
        ) %>%
        rename(
          region_level_1 = .data$country,
          level_1_region_code = .data$iso_code
        )
    },

    #' @description Specific return settings for the ECDC dataset.
    #' @param ... pass additional arguments
    #' @author Sam Abbott
    #' @importFrom dplyr group_by ungroup select arrange
    #' @importFrom tidyr fill
    return = function(...) {
      self$region$return <- self$region$processed %>%
        group_by(.data$country) %>%
        fill(.data$population_2019, .data$un_region, .direction = "updown") %>%
        ungroup()

      self$region$return <- self$region$return %>%
        select(
          .data$date, .data$un_region, .data$country,
          .data$iso_code, .data$population_2019,
          .data$cases_new, .data$cases_total,
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
      self$country <- class(self)[1]
      self$get_region_codes()
    }
  )
)
