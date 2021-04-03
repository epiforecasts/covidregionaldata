#' Belgium Class for downloading, cleaning and processing notification data
#'
#' @description Country specific information for downloading, cleaning
#'  and processing COVID-19 region level 1 and 2 data for Belgium.
#'
#' @details Inherits from `DataClass`
#' @source https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv  # nolint
#' @export
#' @examples
#' \dontrun{
#' region <- Belgium$new(verbose = TRUE, steps = TRUE, level = "2")
#' region$download()
#' region$clean()
#' region$process()
#' region$return()
#' }
Belgium <- R6::R6Class("Belgium",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field country name of country to fetch data for
    country = "Belgium",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "region", "2" = "province"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2", "2" = "iso_3166_2"),
    #' @field data_url List of named links to raw data.
    data_url = list(
      "main" = "https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv",
      "hosp" = "https://epistat.sciensano.be/Data/COVID19BE_HOSP.csv"
    ),
    #' @field data_url_level_1 List of named links to raw data for level 1;
    #' not used or accessed when generating level 2 results.
    data_url_level_1 = list(
      "deaths" = "https://epistat.sciensano.be/Data/COVID19BE_MORT.csv"
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "deaths_new"),

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble tribble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble::tibble(
        level_1_region_code = c("BE-BRU", "BE-VLG", "BE-WAL"),
        level_1_region = c("Brussels", "Flanders", "Wallonia")
      )
      self$codes_lookup$`2` <- tibble::tribble(
        ~level_2_region_code,    ~level_2_region, ~level_1_region_code,
                    "BE-VAN",        "Antwerpen",      "BE-VLG",
                    "BE-WBR",    "BrabantWallon",      "BE-WAL",
                    "BE-WHT",          "Hainaut",      "BE-WAL",
                    "BE-WLG",       "Li\u00e8ge",      "BE-WAL",
                    "BE-VLI",          "Limburg",      "BE-VLG",
                    "BE-WLX",       "Luxembourg",      "BE-WAL",
                    "BE-WNA",            "Namur",      "BE-WAL",
                    "BE-VOV",   "OostVlaanderen",      "BE-VLG",
                    "BE-VBR",    "VlaamsBrabant",      "BE-VLG",
                    "BE-VWV",   "WestVlaanderen",      "BE-VLG",
                    "BE-BRU",         "Brussels",      "BE-BRU"
        )
    },

    #' @description directs to either level 1 or level 2 processing based on
    #' request.
    #' @importFrom dplyr select mutate
    #' @importFrom lubridate as_date ymd_hms
    clean = function() {
      message_verbose(self$verbose, "Cleaning data")
      if (self$level == "1") {
        self$clean_level_1()
      } else if (self$level == "2") {
        self$clean_level_2()
      }
    },

    #' @description Belgium Specific Region Level Data Cleaning
    # nolint start
    #' @importFrom dplyr group_by summarise ungroup full_join tally mutate select rename
    #' @importFrom tidyr replace_na
    #' @importFrom lubridate ymd
    # nolint end
    clean_level_1 = function() {
      cases_data <- self$data$raw$main %>%
        select(DATE, REGION, CASES) %>%
        mutate(
          DATE = ymd(DATE),
          CASES = as.numeric(CASES)
        ) %>%
        replace_na(list(REGION = "Unknown")) %>%
        group_by(DATE, REGION) %>%
        tally(CASES) %>%
        ungroup()

      hosp_data <- self$data$raw$hosp %>%
        select(DATE, REGION, NEW_IN) %>%
        mutate(
          DATE = ymd(DATE),
          NEW_IN = as.numeric(NEW_IN)
        ) %>%
        replace_na(list(REGION = "Unknown")) %>%
        group_by(DATE, REGION) %>%
        tally(wt = NEW_IN) %>%
        ungroup()

      deaths_data <- self$data$raw$deaths %>%
        select(DATE, REGION, DEATHS) %>%
        mutate(
          DATE = ymd(DATE),
          DEATHS = as.numeric(DEATHS)
        ) %>%
        replace_na(list(REGION = "Unknown")) %>%
        group_by(DATE, REGION) %>%
        tally(wt = DEATHS) %>%
        ungroup()

      # Join the three datasets and rename columns
      cases_and_hosp_data <- full_join(cases_data,
                                       hosp_data,
                                       by = c("DATE", "REGION"))

      all_data <- full_join(cases_and_hosp_data,
                                   deaths_data,
                                   by = c("DATE", "REGION")) %>%
        rename(date = DATE, level_1_region = REGION,
               cases_new = n.x, hosp_new = n.y, deaths_new = n)
      self$data$clean <-
        left_join(all_data, self$codes_lookup[[1]],
                  by = c("level_1_region"),
                  copy = TRUE)
    },

    #' @description Belgium Specific Province Level Data Cleaning
    # nolint start
    #' @importFrom dplyr group_by summarise ungroup full_join tally mutate select rename
    #' @importFrom tidyr replace_na
    #' @importFrom lubridate ymd
    # nolint end
    #'
    clean_level_2 = function() {
      cases_data <- self$data$raw$main %>%
        select(DATE, REGION, PROVINCE, CASES) %>%
        mutate(
          DATE = lubridate::ymd(DATE),
          CASES = as.numeric(CASES)
        ) %>%
        replace_na(list(
          REGION = "Unknown",
          PROVINCE = "Unknown"
        )) %>%
        group_by(DATE, PROVINCE, REGION) %>%
        tally(CASES) %>%
        ungroup()

      hosp_data <- self$data$raw$hosp %>%
        select(DATE, REGION, PROVINCE, NEW_IN) %>%
        mutate(
          DATE = lubridate::ymd(DATE),
          NEW_IN = as.numeric(NEW_IN)
        ) %>%
        replace_na(list(
          REGION = "Unknown",
          PROVINCE = "Unknown"
        )) %>%
        group_by(DATE, PROVINCE, REGION) %>%
        tally(wt = NEW_IN) %>%
        ungroup()

      # Join the two datasets and rename columns
      self$data$clean <- full_join(cases_data, hosp_data,
                                   by = c("DATE",
                                          "PROVINCE",
                                          "REGION")) %>%
        rename(date = DATE,
               level_1_region = REGION,
               level_2_region = PROVINCE,
               cases_new = n.x,
               hosp_new = n.y) %>%
        left_join(self$codes_lookup[[2]],
                  by = c("level_2_region"), copy = TRUE)
    }
  )
)
