#' SouthAfrica Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for South Africa.
#'
# nolint start
#' @source \url{https://github.com/dsfsi/covid19za/}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- SouthAfrica$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
SouthAfrica <- R6::R6Class("SouthAfrica",
  inherit = DataClass,
  public = list(
    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "South Africa",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "province"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data.
    # nolint start
    common_data_urls = list(
      "cases" = "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv",
      "deaths" = "https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_deaths.csv"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new", "recovered_new"),
    #' @field source_text Plain text description of the source of the data
    source_text = "Data Science for Social Impact research group, University of Pretoria", # nolint
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://github.com/dsfsi/covid19za",

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c(
          "ZA-EC", "ZA-FS", "ZA-GP", "ZA-KZN", "ZA-LP",
          "ZA-MP", "ZA-NC", "ZA-NW", "ZA-WC"
        ),
        level_1_region = c(
          "Eastern Cape", "Free State", "Gauteng", "Kwazulu-Natal", "Limpopo",
          "Mpumalanga", "Northern Cape", "North-West", "Western Cape"
        )
      )
    },

    #' @description Province level data cleaning
    #' @importFrom dplyr mutate select bind_rows na_if
    #' @importFrom tidyr pivot_longer pivot_wider
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      deaths_copy <- self$data$raw$deaths
      deaths_copy$total <- as.double(self$data$raw$deaths$total)
      self$data$clean <- bind_rows(self$data$raw$cases,
        deaths_copy,
        .id = "data"
      ) %>%
        mutate(
          data = factor(data, c(1, 2), c("cases_total", "deaths_total")),
          date = dmy(date)
        ) %>%
        select(-c(YYYYMMDD, total, source)) %>%
        pivot_longer(-c(data, date), names_to = "level_1_region_code") %>%
        pivot_wider(names_from = data) %>%
        mutate(
          level_1_region_code = paste0("ZA-", level_1_region_code),
          level_1_region_code = na_if(level_1_region_code, "ZA-UNKNOWN")
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region_code" = "code")
        )
    }
  )
)
