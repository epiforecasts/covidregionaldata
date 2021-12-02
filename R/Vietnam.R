#' Vietnam Class for downloading, cleaning and processing
#' notification data
#'
#' @description Information for downloading, cleaning
#'  and processing covid-19 region data for Vietnam.
#'
#' @source \url{https://covid19.ncsc.gov.vn}
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Vietnam$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Vietnam <- R6::R6Class("Vietnam",
  inherit = DataClass,
  public = list(

    # Core Attributes (amend each parameter for country specific information)
    #' @field origin name of country to fetch data for
    origin = "Vietnam",
    #' @field supported_levels List of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names List of region names in order of level.
    supported_region_names = list("1" = "region"),
    #' @field supported_region_codes List of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data.
    common_data_urls = list(
      # nolint start
      "case_by_time" = "https://covid19.ncsc.gov.vn/api/v3/covid/provinces?filter_type=case_by_time",
      "death_by_time" = "https://covid19.ncsc.gov.vn/api/v3/covid/provinces?filter_type=death_by_time",
      "recovered_by_time" = "https://covid19.ncsc.gov.vn/api/v3/covid/provinces?filter_type=recovered_by_time",
      "provinces" = "https://covid19.ncsc.gov.vn/api/v3/covid/provinces"
      # nolint end
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_total", "deaths_total", "recovered_total"
    ),
    #' @field source_text Plain text description of the source of the data
    source_text =
      "Public COVID-19 for Vietnam, curated by NCSC's COVID-19 team",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://covid19.ncsc.gov.vn",

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- covidregionaldata::vietnam_codes
    },

    #' @description Download function to get raw data. Uses the
    #' parent class JSON-specific method for downloads.
    download = function() {
      super$download_JSON()
    },

    #' @description Provincial Level Data
    #' cleaning
    #' @param ... pass additional arguments
    #'
    #' @importFrom dplyr filter select mutate rename tibble as_tibble full_join
    #' @importFrom tidyr replace_na drop_na separate
    #' @importFrom purrr map
    #' @importFrom stringi stri_trans_general stri_trim_both stri_replace_all
    #' @importFrom stringr str_to_title str_replace_all
    #' @importFrom lubridate dmy
    clean_common = function() {
      # The first three elements of self$data$raw are the data
      # tables downloaded and so these can be processed identically
      #
      data_inputs <- self$data$raw[1:3]
      flat_all <- map(
        map(
          data_inputs,
          function(x) as_tibble(unlist(x),
                                rownames = "date")),
        function(y) {
          y %>% separate(date, sep = "[.]+", into = c(NA, "province", "date"))
        }
      )
      self$data$clean <- full_join(
        full_join(
          flat_all$case_by_time, flat_all$death_by_time,
          by = c("province", "date"),
          suffix = c(".cases", ".deaths"),
          copy = TRUE
        ),
        flat_all$recovered_by_time,
        by = c("province", "date"),
        suffix = c("", ".recovered"),
        copy = TRUE
      ) %>%
        # The api uses integer codes for provinces which do not
        # line up with ISO 3166-2 (some of which are not numbers)
        # so we use this as a temporary code to line names up
        # with data.
        select(
          ncsc_region_code = province,
          date,
          cases_total = value.cases,
          deaths_total = value.deaths,
          recovered_total = value) %>%
        mutate(ncsc_region_code = as.numeric(ncsc_region_code)) %>%
        left_join(
          self$data$raw$provinces %>%
            select(ncsc_region_code = id, level_1_region = name),
          by = c("ncsc_region_code")) %>%
        select(-ncsc_region_code) %>%
        mutate(
          date = dmy(date),
          cases_total = as.numeric(cases_total),
          deaths_total = as.numeric(deaths_total),
          recovered_total = as.numeric(recovered_total),
          level_1_region = str_replace_all(level_1_region,
                                        "TP HCM", "Hochiminh"),
        ) %>%
        #
        #tidyr::drop_na(date, region_name) %>%
        mutate(
          level_1_region = stri_trans_general(level_1_region, "latin-ascii"),
          level_1_region = stri_trim_both(level_1_region),
          level_1_region = str_replace_all(level_1_region,
                                           "\\(.*\\)|-| ", ""),
          level_1_region = str_to_title(level_1_region),
          level_1_region = replace_na(level_1_region, "Unknown")
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "level_1_region")
        )
    }
  )
)
