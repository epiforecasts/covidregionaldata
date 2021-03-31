#' R6 Class containing specific attributes and methods for WHO data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data from the World Health Organisation
#'
#' @details Inherits from `DataClass`
#' @source https://covid19.who.int/WHO-COVID-19-global-data.csv
#' @export
#' @examples
#' \dontrun{
#' national <- WHO$new(verbose = TRUE, steps = TRUE)
#' national$download()
#' national$clean()
#' national$process()
#' national$return()
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
      message_verbose(self$verbose, "Cleaning data")
      self$data$clean <- self$data$raw[["main"]]
      colnames(self$data$clean) <- c(
        "date", "iso_code", "country", "who_region",
        "cases_new", "cases_total", "deaths_new",
        "deaths_total"
      )
      self$data$clean <- self$data$clean %>%
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
      self$data$return <- self$data$processed %>%
        group_by(.data$country) %>%
        fill(.data$who_region, .data$un_region, .direction = "updown") %>%
        ungroup()

      self$data$return <- self$data$return %>%
        select(
          .data$date, .data$un_region, .data$who_region, .data$country,
          .data$iso_code, .data$cases_new, .data$cases_total,
          .data$deaths_new, .data$deaths_total, .data$recovered_new,
          .data$recovered_total, .data$hosp_new, .data$hosp_total,
          .data$tested_new, .data$tested_total
        ) %>%
        arrange(.data$date, .data$country)

      if (self$steps) {
        return(self$data)
      } else {
        return(self$data$return)
      }
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  )
)
