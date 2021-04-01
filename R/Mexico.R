#' Meixco Class for downloading, cleaning and processing notification data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing covid-19 region data for Mexico.
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
    #' @field the localised names for level 1 and 2 regions and geocodes
    localise_regions = list(level_1_region = "estados",
                            level_1_region_code = "iso_3166_2", 
                            level_2_region = "municipios", 
                            level_2_region_code = "inegi"),
    #' @field data_url link to raw data
    data_url = "https://datos.covid-19.conacyt.mx/#DownZCSV",
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    #' @importFrom dplyr select
    set_region_codes = function() {
      message_verbose(
        self$verbose,
        paste(
          "Getting region codes for",
          self$country
        )
      )
      self$region_codes <- mexico_codes
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
      if (self$level == "2") {
        path <- "Downloads/filesDD.php?csvmun"
      } else {
        path <- "Downloads/filesDD.php?csvaxd"
      }
      domain <- "https://datos.covid-19.conacyt.mx/"
      script_url <- file.path(domain, path)

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
        dat <- csv_reader(file.path(domain, target), self$verbose)

        dat <- dat %>%
          select(-.data$poblacion) %>%
          pivot_longer(-c("cve_ent", "nombre"),
            names_to = "date", values_to = new_name
          )
      }

      confirmed <- read_data(confirmed_url, "cases_new")
      deceased <- read_data(deceased_url, "deaths_new")
      self$data$raw <- full_join(confirmed, deceased,
        by = c("cve_ent", "nombre", "date")
      )
    },

    #' @description directs to either level 1 or level 2 processing based on
    #' request.
    #' @importFrom dplyr mutate select arrange recode group_by ungroup
    #' @importFrom lubridate as_date ymd_hms
    #'
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")
      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description Mexico Specific Estados Level Data Cleaning
    #' @importFrom dplyr mutate full_join filter rename select
    #' @importFrom stringr str_to_title
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean_level_1 = function() {
      self$data$clean <- self$data$raw %>%
        mutate(
          level_1_region = str_to_title(.data$nombre),
          level_1_region = ifelse(.data$level_1_region == "Distrito Federal",
            "Ciudad de Mexico",
            .data$level_1_region
          ),
          date = dmy(.data$date)
        ) %>%
        left_join(self$region_codes %>%
                    filter(is.na(level_2_region)),
                  by = c("level_1_region")) %>%
        filter(.data$level_1_region != "Nacional") %>%
        select(date, level_1_region, level_1_region_code,
               cases_new, deaths_new)
    },

    #' @description Mexico Specific Municipality Level Data Cleaning
    #' @importFrom dplyr mutate full_join filter rename select
    #' @importFrom stringr str_to_title
    #' @importFrom lubridate dmy
    #' @importFrom rlang .data
    #'
    clean_level_2 = function() {
      self$data$clean <- self$data$raw %>%
        rename(level_2_region = .data$nombre) %>%
        mutate(date = dmy(.data$date)) %>%
        left_join(self$region_codes %>%
                    filter(!is.na(level_2_region)),
                  by = "level_2_region") %>%
        select(
          date, level_1_region_code, level_1_region,
          level_2_region_code, level_2_region,
          cases_new, deaths_new
        )
    },

    #' @description Initialize the country
    #' @param ... The args passed by [general_init]
    initialize = function(...) {
      general_init(self, ...)
    }
  )
)
