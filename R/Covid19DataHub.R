#' R6 Class containing specific attributes and methods for Covid19 Data Hub
#'
#' @description Attributes and methods for COVID-19 data provided by the
#' Covid19 Data Hub
#'
#' @details This dataset supports both national and subnational data sources
#' with national level data returned by default. National data is sourced from
#' John Hopkins University and so we recommend using the JHU class included in
#' this package. Subnational data is supported for a subset of countries which
#' can be found after cleaning using the `available_regions()` method,
#' see the examples for more details. These data sets are minimally cleaned
#' data files hosted by the team at COVID19 Data Hub so please see their
#' source repository for further details
#' (https://github.com/covid19datahub/COVID19/#data-sources)
#' If using for analysis checking the source for further details is
#' strongly advised.
#'
#' If using this class please cite:
#' "Guidotti et al., (2020). COVID-19 Data Hub
#' Journal of Open Source Software, 5(51),
#' 2376, https://doi.org/10.21105/joss.02376"
# nolint start
#' @source \url{https://covid19datahub.io/articles/data.html}
# nolint end
#' @export
#' @concept dataset
#' @family aggregations
#' @family national
#' @family subnational
#' @examples
#' # nolint start
#' \dontrun{
#' # set up a data cache
#' start_using_memoise()
#'
#' # get all countries data
#' cv19dh <- Covid19DataHub$new(level = "1", get = TRUE)
#' cv19dh$return()
#'
#' # show available regions with data at the second level of interest
#' cv19dh_level_2 <- Covid19DataHub$new(level = "2")
#' cv19dh_level_2$download()
#' cv19dh_level_2$clean()
#' cv19dh$available_regions()
#'
#' # get all region data for the uk
#' cv19dh_level_2$filter("uk")
#' cv19dh_level_2$process()
#' cv19dh_level_2$return()
#'
#' # get all regional data for the UK
#' uk <- Covid19DataHub$new(regions = "uk", level = "2", get = TRUE)
#' uk$return()
#'
#' # get all subregional data for the UK
#' uk <- Covid19DataHub$new(regions = "uk", level = "3", get = TRUE)
#' uk$return()
#' }
#' # nolint end
Covid19DataHub <- R6::R6Class("Covid19DataHub",
  inherit = CountryDataClass,
  public = list(

    # Core Attributes (amend each paramater for country specific infomation)
    #' @field origin name of country to fetch data for
    origin = "Covid-19 Data Hub",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2", "3"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list(
      "1" = "country",
      "2" = "region",
      "3" = "subregion"
    ),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "iso_3166_1_alpha_3", "2" = "iso_code", "3" = "subregion_code"
    ),
    #' @field level_data_urls List of named links to raw data. The first, and
    #' only entry, is be named main.
    # nolint start
    level_data_urls = list(
      "1" = list(
        "country" = "https://storage.covid19datahub.io/rawdata-1.csv"
      ),
      "2" = list(
        "region" = "https://storage.covid19datahub.io/rawdata-2.csv"
      ),
      "3" = list(
        "subregion" = "https://storage.covid19datahub.io/rawdata-3.csv"
      )
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("confirmed", "deaths", "recovered", "tested", "hosp"),
    #' @field source_text Plain text description of the source of the data
    source_text = "COVID-19 Data Hub",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://covid19datahub.io",

    #' @description Covid19 Data Hub specific data cleaning.
    #' This takes all the raw data, renames some columns and checks types.
    #' @importFrom dplyr select mutate rename
    #' @importFrom purrr map
    #' @importFrom lubridate ymd
    #' @importFrom rlang .data
    clean_common = function() {
      # do some cleaning
      raw_data <- self$data$raw[[names(self$level_data_urls[[self$level]])]]
      self$data$clean <- raw_data %>%
        rename(
          level_1_region_code = .data$iso_alpha_3,
          level_1_region = .data$administrative_area_level_1,
          level_2_region_code = .data$iso_alpha_2,
          level_2_region = .data$administrative_area_level_2,
          level_3_region_code = .data$iso_numeric,
          level_3_region = .data$administrative_area_level_3,
          cases_new = .data$confirmed,
          deaths_new = .data$deaths,
          tested_new = .data$tests,
          recovered_new = .data$recovered,
          hosp_new = .data$hosp
        ) %>%
        mutate(
          date = ymd(.data$date),
          cases_new = as.numeric(.data$cases_new),
          deaths_new = as.numeric(.data$deaths_new),
          recovered_new = as.numeric(.data$recovered_new),
          tested_new = as.numeric(.data$tested_new),
          hosp_new = as.numeric(.data$hosp_new)
        )

      # remove levels not requested
      all_levels <- paste0("^level_", self$supported_levels, "_*")
      keep_levels <- paste0("^level_", seq_len(as.integer(self$level)), "_*")
      remove_levels <- unlist(map(
        setdiff(all_levels, keep_levels),
        ~ {
          grep(.x, colnames(self$data$clean), value = TRUE)
        }
      ))
      self$data$clean <- self$data$clean %>%
        select(
          -all_of(remove_levels)
        )
    }
  )
)
