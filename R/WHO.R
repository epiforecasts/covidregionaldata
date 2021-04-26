#' R6 Class containing specific attributes and methods for WHO data
#'
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data from the World Health Organisation
#'
#' @source \url{https://covid19.who.int/}
#' @concept dataset
#' @export
#' @examples
#' \dontrun{
#' national <- WHO$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' national$return()
#' }
WHO <- R6::R6Class("WHO",
  inherit = CountryDataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "World Health Organisation (WHO)",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "country"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_code"),
    #' @field common_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    common_data_urls = list(
      "main" = "https://covid19.who.int/WHO-COVID-19-global-data.csv"
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_new",
      "cases_total",
      "deaths_new",
      "deaths_total"
    ),

    #' @description WHO specific data cleaning
    #' @importFrom dplyr mutate rename
    #' @importFrom countrycode countrycode
    clean_common = function() {
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
          level_1_region = .data$country,
          level_1_region_code = .data$iso_code
        )
    },

    #' @description Specific return settings for the WHO dataset.
    #' @importFrom dplyr group_by ungroup select arrange
    #' @importFrom tidyr fill
    return = function() {
      self$data$return <- self$data$processed
      if (!self$totals) {
        self$data$return <- self$data$return %>%
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
      }

      if (self$steps) {
        return(self$data)
      } else {
        return(self$data$return)
      }
    }
  )
)
