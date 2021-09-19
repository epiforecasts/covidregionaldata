#' Vietnam Class for downloading, cleaning and processing
#' notification data
#'
#' @description Information for downloading, cleaning
#'  and processing covid-19 region data for Vietnam.
#'
#' @source \url{https://covid.ncsc.gov.vn}
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
      "case_by_time" = "https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=case_by_time",
      "death_by_time" = "https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=death_by_time",
      "recovered_by_time" = "https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=recovered_by_time",
      "provinces" = "https://covid.ncsc.gov.vn/api/v3/covid/provinces"
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c(
      "cases_total", "deaths_total", "recovered_total"
    ),
    #' @field source_text Plain text description of the source of the data
    source_text = "Public COVID-19 for Vietnam, curated by NCSC's COVID-19 team",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://covid.ncsc.gov.vn", # nolint

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- covidregionaldata::vietnam_codes
    },

    #' @description download function to get raw data
    #' @importFrom tidyr replace_na drop_na
    #' @importFrom lubridate dmy
    #' @importFrom jsonlite fromJSON
    download = function(){
      super$download_JSON()
    },
    #   function() {
    #   bundles_urls <- list(
    #     "case_by_time" = "https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=case_by_time",
    #     "death_by_time" = "https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=death_by_time",
    #     "recovered_by_time" = "https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=recovered_by_time"
    #   )
    #   Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 4) # Fix VROOM error
    #   provines_url <- "https://covid.ncsc.gov.vn/api/v3/covid/provinces"
    #   bundles <- names(bundles_urls)
    #   provines_data <- fromJSON(provines_url)
    #
    #   get_bundles_data <- function(bundles) {
    #     bundles_data <- list()
    #     for (bundle in bundles) {
    #       url <- paste0("https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=", bundle)
    #       data <- fromJSON(url)
    #       bundles_data <- c(bundles_data, setNames(list(data), bundle))
    #     }
    #     bundles_data
    #   }
    #
    #   bundles_data <- get_bundles_data(bundles)
    #
    #   get_province <- function(id, data) {
    #     row_dat <- provines_data[(id <- id), ]
    #     death_by_time <- do.call(cbind, data$death_by_time[id])
    #     case_by_time <- do.call(cbind, data$case_by_time[id])
    #     recovered_by_time <- do.call(cbind, data$recovered_by_time[id])
    #     if (!identical(row.names(death_by_time), row.names(death_by_time))) {
    #       stop("Dates on case_by_time and death_by_time do not match!")
    #     }
    #     df <- tibble(
    #       date = dmy(row.names(case_by_time)),
    #       id = row_dat$id,
    #       name = row_dat$name,
    #       case_by_time = case_by_time,
    #       death_by_time = death_by_time,
    #       recovered_by_time = recovered_by_time
    #     )
    #     df
    #   }
    #
    #   df <- do.call(rbind, lapply(provines_data$id, function(id) {
    #     get_province(id, bundles_data)
    #   }))
    #   names(df) <- c("date", "id", "region_name", "cases_total", "deaths_total", "recovered_total")
    #   self$data$raw[["main"]] <- df
    # },

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
      data_inputs <- self$data$raw[1:3]
      flat_all <- map(map(vn_data_inputs, function(x) as_tibble(unlist(x), rownames="date")),
                         function(x) {x %>% separate(date, sep="[.]+", into=c(NA, "province", "date"))})
      self$data$clean <- full_join(
        full_join(
          vn_flat_all$case_by_time, vn_flat_all$death_by_time, by=c("province","date"),suffix=c(".cases", ".deaths"),copy=TRUE),
        vn_flat_all$recovered_by_time, by=c("province","date"), suffix=c("",".recovered"), copy=TRUE) %>%
        select(level_1_region_code=province, date, cases_total = value.cases, deaths_total=value.deaths, recovered_total = value) %>%
        mutate(level_1_region_code = as.numeric(level_1_region_code)) %>%
        left_join(self$data$raw$provinces%>%select(level_1_region_code=id,level_1_region=name), by=c("level_1_region_code")) %>%
        #select(date, region_name, cases_total, deaths_total, recovered_total) %>%
        mutate(
          cases_total = as.numeric(cases_total),
          deaths_total = as.numeric(deaths_total),
          recovered_total = as.numeric(recovered_total),
          region_name = stringr::str_replace_all(region_name, "TP HCM", "Hochiminh"),
        ) %>%
        tidyr::drop_na(date, region_name) %>%
        rename(level_1_region = region_name) %>%
        mutate(
          level_1_region = stringi::stri_trans_general(level_1_region, "latin-ascii"),
          level_1_region = stringi::stri_trim_both(level_1_region),
          level_1_region = stringr::str_replace_all(level_1_region, "\\(.*\\)|-| ", ""),
          level_1_region = stringr::str_to_title(level_1_region),
          level_1_region = tidyr::replace_na(level_1_region, "Unknown")
        ) %>%
        left_join(
          self$codes_lookup$`1`,
          by = c("level_1_region" = "level_1_region")
        )
    }
  )
)
