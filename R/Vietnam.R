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
                           "case_by_time" = 'https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=case_by_time',
                           "death_by_time" = 'https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=death_by_time',
                           "recovered_by_time" = 'https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=recovered_by_time'
                           
                         ),
                         # nolint end
                         #' @field source_data_cols existing columns within the raw data
                         source_data_cols = c(
                           "cases_total", "deaths_total", "recovered_total"
                         ),
                         #' @field source_text Plain text description of the source of the data
                         source_text = "Public COVID-19 data curated by 5F team",
                         #' @field source_url Website address for explanation/introduction of the
                         #' data
                         source_url = "https://covid.ncsc.gov.vn", # nolint
                         
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
                         #' @importFrom tidyr replace_na drop_na
                         #' @importFrom lubridate dmy 
                         #' @importFrom jsonlite fromJSON
                         clean_common = function() {
                           Sys.setenv("VROOM_CONNECTION_SIZE" = 131072*4) # Fix VROOM error 
                           provines_url = 'https://covid.ncsc.gov.vn/api/v3/covid/provinces'
                           bundles = names(self$data$raw)
                           provines_data = jsonlite::fromJSON(provines_url)
                           
                           get_bundles_data = function(bundles){
                             bundles_data = list()
                             for (bundle in bundles){ 
                               url = paste0('https://covid.ncsc.gov.vn/api/v3/covid/provinces?filter_type=', bundle)
                               data = jsonlite::fromJSON(url)
                               bundles_data = c(bundles_data, setNames(list(data), bundle)) 
                             }
                             bundles_data
                           }
                           
                           bundles_data = get_bundles_data(bundles)
                           
                           get_province = function(id, data){
                             row_dat = provines_data[(id=id),]
                             death_by_time= do.call(cbind, data$death_by_time[id])
                             case_by_time=do.call(cbind, data$case_by_time[id]) 
                             recovered_by_time=do.call(cbind, data$recovered_by_time[id]) 
                             if (!identical(row.names(death_by_time), row.names(death_by_time))) {
                               stop("Dates on case_by_time and death_by_time do not match!")
                             }
                             df = dplyr::tibble(date= lubridate::dmy(row.names(case_by_time)), 
                                                id = row_dat$id,
                                                name = row_dat$name,
                                                case_by_time= case_by_time, 
                                                death_by_time= death_by_time,
                                                recovered_by_time= recovered_by_time)
                             df
                           }
                           
                           df = do.call(rbind, lapply(provines_data$id, function(id){get_province(id, bundles_data)}))
                           names(df) <- c("date", "id", "region_name", "cases_total", "deaths_total", "recovered_total")
                           
                           self$data$clean <- df %>%
                             select( date, region_name, cases_total, deaths_total, recovered_total) %>%
                             mutate(cases_total = as.numeric(cases_total),
                                    deaths_total = as.numeric(deaths_total),
                                    recovered_total = as.numeric(recovered_total),
                                    region_name = stringr::str_replace_all(region_name, 'TP HCM', 'Hochiminh'),
                                    ) %>%
                             tidyr::drop_na(date, region_name) %>%
                             rename(level_1_region = region_name) %>%
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
