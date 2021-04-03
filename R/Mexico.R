#' Meixco Class for downloading, cleaning and processing notification data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for Mexico.
#'
#'  Notes on region codes:
#'
#'  Level 1 codes = ISO-3166-2,
#'  source: https://en.wikipedia.org/wiki/ISO_3166-2:MX
#'
#'  Level 2 codes = INEGI Mexican official statistics geocoding,
#'   source: raw data
#'
#'  Level 1 INEGI codes are the first 2 characters of Level 2 INEGI codes
#'
#' @details Inherits from `DataClass`
#' @source https://datos.covid-19.conacyt.mx/#DownZCSV
#' @export
#' @examples
#' \dontrun{
#' region <- Mexico$new(verbose = TRUE, steps = TRUE)
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
Mexico <- R6::R6Class("Mexico",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field country name of country to fetch data for
    country = "Mexico",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "estados", "2" = "municipios"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2", "2" = "inegi"),
    #' @field data_url List of named links to raw data. The first, and
    #' only entry, is be named main.
    data_url = list(
      "main" = "https://datos.covid-19.conacyt.mx/",
      "1" = "Downloads/filesDD.php?csvmun",
      "2" = "Downloads/filesDD.php?csvaxd"
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    #' @importFrom dplyr select
    set_region_codes = function() {
      self$codes_lookup$`1` <- covidregionaldata::mexico_codes %>%
        filter(is.na(level_2_region))

      self$codes_lookup$`2` <- covidregionaldata::mexico_codes %>%
        filter(!is.na(level_2_region))
    },

    #' @description Data download function for Mexico data. This replaces the
    #' generic download function in `DataClass`. To get the latest data
    #' use a PHP script from the website.
    #' @importFrom httr POST content
    #' @importFrom xml2 xml_find_first xml_text
    #' @importFrom dplyr select full_join
    #' @importFrom rlang .data
    #' @importFrom tidyr pivot_longer
    #'
    download = function() {
      . <- NULL
      script_url <- file.path(
        self$data_url[["main"]],
        self$data_url[[self$level]]
      )

      confirmed_url <- script_url %>%
        POST(body = "Confirmados", encode = "form", verbose = TRUE) %>%
        content() %>%
        xml_find_first("//script") %>%
        xml_text() %>%
        strsplit("\\n\\t*") %>%
        unlist() %>%
        {
          grep("^a\\.href", ., value = TRUE)
        } %>%
        {
          gsub('^a\\.href\\s+=\\s+"(.*)";', "\\1", .)
        }

      deceased_url <- gsub("Confirmados", "Defunciones", confirmed_url,
        fixed = TRUE
      )

      read_data <- function(target, new_name) {
        message_verbose(self$verbose, "Downloading ", new_name)
        dat <- csv_reader(
          file.path(self$data_url[["main"]], target),
          self$verbose
        )

        dat <- dat %>%
          select(-.data$poblacion) %>%
          pivot_longer(-c("cve_ent", "nombre"),
            names_to = "date", values_to = new_name
          )
      }

      confirmed <- read_data(confirmed_url, "cases_new")
      deceased <- read_data(deceased_url, "deaths_new")
      self$data$raw <- list("confirmed" = confirmed, "deceased" = deceased)
    },

    #' @description directs to either level 1 or level 2 processing based on
    #' request.
    #' @importFrom dplyr mutate select arrange recode group_by ungroup
    #' @importFrom lubridate as_date ymd_hms
    #'
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")

      self$data$raw$clean <- full_join(
        self$data$rawconfirmed,
        self$data$rawdeceased,
        by = c("cve_ent", "nombre", "date")
      )

      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description Mexico Specific Estados Level Data Cleaning
    #' @importFrom dplyr mutate full_join filter rename select distinct
    #' @importFrom stringr str_to_title
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean_level_1 = function() {
      self$data$clean <- self$data$raw$clean %>%
        mutate(
          region_level_1 = str_to_title(.data$nombre),
          region_level_1 = ifelse(.data$region_level_1 == "Distrito Federal",
            "Ciudad de Mexico",
            .data$region_level_1
          ),
          date = dmy(.data$date)
        ) %>%
        left_join(self$codes_lookup[["1"]],
          by = c("level_1_region")
        ) %>%
        filter(.data$level_1_region != "Nacional") %>%
        select(
          date, level_1_region, level_1_region_code,
          cases_new, deaths_new
        ) %>%
        distinct(.keep_all = TRUE)
    },

    #' @description Mexico Specific Municipality Level Data Cleaning
    #' @importFrom dplyr mutate left_join filter rename select
    #' @importFrom stringr str_to_title
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean_level_2 = function() {
      self$data$clean <- self$data$raw$clean %>%
        rename(level_2_region = .data$nombre) %>%
        mutate(date = dmy(.data$date)) %>%
        left_join(self$codes_lookup[["2"]],
          by = "level_2_region"
        ) %>%
        select(
          -.data$inegi_state, -.data$cve_ent,
          -.data$inegi_state, -.data$iso_code
        )
    }
  )
)
