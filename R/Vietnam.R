#' Vietnam Class for downloading, cleaning and processing
#' notification data
#'
#' @description Information for downloading, cleaning
#'  and processing covid-19 region data for Vietnam.
#'
# nolint start
#' @source \url{https://github.com/biocyberman/covidregionaldata/}
# nolint end
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
                         
                         # Core Attributes (amend each paramater for country specific infomation)
                         #' @field origin name of country to fetch data for
                         origin = "Vietnam",
                         #' @field supported_levels List of supported levels.
                         supported_levels = list("1"),
                         #' @field supported_region_names List of region names in order of level.
                         supported_region_names = list("1" = "region"),
                         #' @field supported_region_codes List of region codes in order of level.
                         supported_region_codes = list("1" = "is_3166_2"),
                         #' @field common_data_urls List of named links to raw data.
                         # nolint start
                         common_data_urls = list(
                           "main" = "https://docs.google.com/spreadsheets/d/1_d7oK-SKj-7KrWAW7DbGYEad2JO4TyR7ApsUAuoiH5g/export?format=csv&gid=0"
                         ),
                         # nolint end
                         #' @field source_data_cols existing columns within the raw data
                         source_data_cols = c(
                           "cases_new"
                         ),
                         #' @field source_text Plain text description of the source of the data
                         source_text = "Public COVID-19 data curated by 5F team",
                         #' @field source_url Website address for explanation/introduction of the
                         #' data
                         source_url = "https://datastudio.google.com/u/0/reporting/1cc8d45e-2c74-4084-af70-cbbe60f1660e/page/bLUVC", # nolint
                         
                         #' @description Set up a table of region codes for clean data
                         #' @importFrom tibble tibble
                         set_region_codes = function(){
                           self$codes_lookup$`1` <- covidregionaldata::vietnam_codes
                         },
                         
                         #' @description Provincial Level Data
                         #' cleaning
                         #' @param ... pass additional arguments
                         #'
                         #' @importFrom dplyr filter select mutate rename
                         #' @importFrom tidyr replace_na
                         #' @importFrom lubridate dmy
                         clean_common = function() {
                           self$data$clean <- self$data$raw[["main"]] %>%
                             `colnames<-`(c('date', 'region', 'cases_new', 'case_group')) %>%
                             select(
                               date,
                               region,
                               cases_new
                             ) %>%
                             rename(level_1_region = region) %>%
                             mutate(
                               date = dmy(date),
                               cases_new = as.numeric(cases_new)
                             )%>%
                             mutate(
                               level_1_region = stringi::stri_trans_general(level_1_region, "latin-ascii"),
                               level_1_region = stringi::stri_trim_both(level_1_region),
                               level_1_region = stringr::str_replace_all(level_1_region, '\\(.*\\)|-| ', ''),
                               level_1_region = stringr::str_to_title(level_1_region),
                               level_1_region = tidyr::replace_na(level_1_region, "Unknown")
                             )  %>%
                             left_join(
                               self$codes_lookup$`1`,
                               by = c("level_1_region" = "level_1_region")
                             )
                         }
                       )
)
