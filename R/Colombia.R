#' Colombia Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Colombia
#'
# nolint start
#' @source \url{https://www.datos.gov.co/Salud-y-Protecci-n-Social/Casos-positivos-de-COVID-19-en-Colombia/gt2j-8ykr}
#' @source \url{https://github.com/danielcs88/colombia_covid-19/}
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
      "main" = "https://www.datos.gov.co/resource/gt2j-8ykr.csv?$select=fecha_diagnostico,departamento_nom,ciudad_municipio_nom,ciudad_municipio"
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
    #' @importFrom tibble tibble
    #' @importFrom dplyr mutate
    set_region_codes = function() {
      self$codes_lookup$`1` <- covidregionaldata::colombia_codes
    },

    #' @description Colombia specific download using Socrata API
    #' @importFrom RSocrata read.socrata
    download = function () {
      message_verbose(self$verbose,
                      "Downloading Colombia data. This may take a while.")
      self$data$raw$main <- read.socrata(self$data_urls[["main"]])
    },

    #' @description Colombia specific data cleaning
    #' @importFrom dplyr select mutate rename summarise group_by
    #' @importFrom lubridate dmy_hms as_date
    #' @importFrom stringr str_replace_all str_to_sentence str_to_title
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        rename(
          date = .data$fecha_diagnostico,
          level_1_region = .data$departamento_nom,
          level_2_region = .data$ciudad_municipio_nom,
          level_2_region_code = .data$ciudad_municipio
        ) %>%
        group_by(date, level_1_region, level_2_region, level_2_region_code) %>%
        summarise(cases_new = n(), .groups = "drop") %>%
        mutate(date = as_date(dmy_hms(date)),
               level_1_region = iconv(.data$level_1_region,
                                      from = "UTF-8",
                                      to = "ASCII//TRANSLIT"
               ),
               level_1_region =
                 str_replace_all(.data$level_1_region,
                                 c(" D.C." = "",
                                   "San Andres y Providencia"
                                   = "San Andres, Providencia y Santa Catalina",
                                   "Norte Santander" = "Norte De Santander"
                                 )
               ),
               level_1_region = str_to_sentence(.data$level_1_region),
               level_1_region = str_to_title(.data$level_1_region)) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "level_1_region")
        )
    },
    
    #' @description Colombia Specific Department Level Data Cleaning
    #'
    #' Aggregates data to the level 1 (department) regional level. Data is
    #' provided by the source at the level 2 (municipality) regional level.
    #'
    #' @importFrom dplyr group_by summarise ungroup full_join across if_else
    #' @importFrom tidyselect vars_select_helpers
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
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
