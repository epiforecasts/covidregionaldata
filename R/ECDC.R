#' R6 Class containing specific attributes and methods for the European Centre
#' for Disease Prevention and Control dataset
#'
#' @description Information for downloading, cleaning
#'  and processing the European Centre for
#'  Disease Prevention and Control COVID-19 data.
#'
# nolint start
#' @source \url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}
# nolint end
#' @export
#' @family national
#' @concept dataset
#' @examples
#' \dontrun{
#' national <- ECDC$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' national$return()
#' }
#'
ECDC <- R6::R6Class("ECDC",
  inherit = CountryDataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "European Centre for Disease Control (ECDC)",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "country"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_code"),
    #' @field common_data_urls List of named links to raw data.
    common_data_urls = list(
      "main" = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),

    #' @description ECDC specific state level data cleaning
    #' @importFrom dplyr mutate rename select arrange filter
    #' @importFrom stringr str_replace_all
    #' @importFrom countrycode countryname countrycode
    #'
    clean_common = function() {
      long_string <- "Cases_on_an_international_conveyance_Japan"
      self$data$clean <- self$data$raw[["main"]] %>%
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
          level_1_region = .data$country,
          level_1_region_code = .data$iso_code
        )
    },

    #' @description Specific return settings for the ECDC dataset.
    #' @importFrom dplyr group_by ungroup select arrange
    #' @importFrom tidyr fill
    return = function() {
      self$data$return <- self$data$processed
      if (!self$totals) {
        self$data$return <- self$data$return %>%
          group_by(.data$country) %>%
          fill(.data$population_2019, .data$un_region,
            .direction = "updown"
          ) %>%
          ungroup()

        self$data$return <- self$data$return %>%
          select(
            .data$date, .data$un_region, .data$country,
            .data$iso_code, .data$population_2019,
            .data$cases_new, .data$cases_total,
            .data$deaths_new, .data$deaths_total, .data$recovered_new,
            .data$recovered_total, .data$hosp_new, .data$hosp_total,
            .data$tested_new, .data$tested_total
          ) %>%
          arrange(.data$date, .data$country)
      }

      if (self$steps) {
        return(self$data)
      } else {
        return(self$data$return)
      }
    }
  )
)
