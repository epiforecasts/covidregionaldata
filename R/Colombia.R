#' Colombia Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Colombia
#'
# nolint start
#' @source \url{https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Colombia$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Colombia <- R6::R6Class("Colombia",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Colombia",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list(
      "1" = "departamento",
      "2" = "municipio"
    ),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "iso_3166_2",
      "2" = "codigo_municipio"
    ),
    #' @field common_data_urls List of named links to raw data.
    # nolint start
    common_data_urls = list(
      "main" = "https://www.datos.gov.co/resource/gt2j-8ykr.csv?$select=fecha_diagnostico,ciudad_municipio"
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new"),
    #' @field source_text Plain text description of the source of the data
    source_text = "Datos abiertos Colombia (Colombia open data)",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr", # nolint

    #' @description Set up a table of region codes for clean data
    #' @importFrom dplyr mutate
    set_region_codes = function() {
      self$codes_lookup$`1` <- covidregionaldata::colombia_codes %>%
        select(level_1_region, level_1_region_code) %>%
        unique()
      self$codes_lookup$`2` <- covidregionaldata::colombia_codes
    },

    #' @description Colombia specific download using Socrata API
    #' This uses the `RSocrata` package if it is installed or downloads
    #' a much larger csv file if that package is not available.
    #' @importFrom dplyr select
    download = function() {
      message_verbose(self$verbose,
                      "Downloading Colombia data. This may take a while.")
      # RSocrata package is recommended but not required
      if (requireNamespace("RSocrata", quietly = self$verbose)) {
        self$data$raw$main <- RSocrata::read.socrata(self$data_urls[["main"]])
      } else {
        stop(
          "covidregionaldata::Colombia$download - requires RSocrata package.\n",
             "Please run install.packages(\"RSocrata\")\n", call. = TRUE)
      }
    },

    #' @description Colombia specific data cleaning
    #' @importFrom dplyr select mutate rename summarise group_by n
    #' @importFrom lubridate as_date
    #' @importFrom stringr str_replace_all str_to_sentence str_to_title
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        rename(
          date = .data$fecha_diagnostico,
          level_2_region_code = .data$ciudad_municipio
        ) %>%
        group_by(date, level_2_region_code) %>%
        summarise(cases_new = n(), .groups = "drop") %>%
        mutate(date = as_date(date),
               level_2_region_code = sprintf("%05d", level_2_region_code)) %>%
        left_join(
          self$codes_lookup$`2`,
          by = c("level_2_region_code" = "level_2_region_code")
        )
    },

    #' @description Colombia Specific Department Level Data Cleaning
    #'
    #' Aggregates data to the level 1 (department) regional level. Data is
    #' provided by the source at the level 2 (municipality) regional level.
    #'
    #' @importFrom dplyr group_by summarise ungroup across select
    #' @importFrom tidyselect vars_select_helpers
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        select(-level_2_region_code, -level_2_region) %>%
        group_by(
          .data$date,
          .data$level_1_region, .data$level_1_region_code
        ) %>%
        summarise(
          across(
            tidyselect::vars_select_helpers$where(is.numeric),
            sum
          )
        ) %>%
        ungroup()
    }
  )
)
