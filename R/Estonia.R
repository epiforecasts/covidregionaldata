#' Estonia Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Estonia
#'
# nolint start
#' @source \url{https://www.terviseamet.ee/et/koroonaviirus/avaandmed}
# nolint end
#' @export
#' @concept dataset
#' @family subnational
#' @examples
#' \dontrun{
#' region <- Estonia$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Estonia <- R6::R6Class("Estonia",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Estonia",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list("1" = "county"),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list("1" = "iso_3166_2"),
    #' @field common_data_urls List of named links to raw data.
    # nolint start
    common_data_urls = list(
      "main" = "https://opendata.digilugu.ee/opendata_covid19_test_county_all.csv" # nolint
    ),
    # nolint end
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_new", "tested_new",
                         "cases_total", "tested_total"),
    #' @field source_text Plain text description of the source of the data
    source_text = "Estonian Ministry of Social Affairs",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://www.terviseamet.ee/et/koroonaviirus/avaandmed",


    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tibble
    set_region_codes = function() {
      self$codes_lookup$`1` <- tibble(
        code = c("EE-37",
          "EE-39", "EE-45", "EE-52", "EE-50", "EE-56", "EE-60", "EE-68",
          "EE-64", "EE-71", "EE-74", "EE-79", "EE-81", "EE-84", "EE-87",
          NA),
        region = c("Harju", "Hiiu", "Ida-Viru", "J\u00e4rva",
          "J\u00f5geva", "L\u00e4\u00e4ne", "L\u00e4\u00e4ne-Viru",
          "P\u00e4rnu", "P\u00f5lva", "Rapla", "Saare", "Tartu",
          "Valga", "Viljandi", "V\u00f5ru", "Unknown")
      )
    },

    #' @description Estonia specific state level data cleaning
    #' @importFrom dplyr select mutate transmute
    #' @importFrom tidyr pivot_wider
    #' @importFrom rlang .data
    #'
    clean_common = function() {
      self$data$clean <- self$data$raw[["main"]] %>%
        pivot_wider(
          id_cols = c("LastStatisticsDate", "StatisticsDate", "Country",
                      "CountryEHAK", "County", "CountyEHAK"),
          names_from = ResultValue,
          values_from = c("DailyTests", "TotalTests",
                          "DailyCases", "TotalCases")) %>%
        select(-Country, -CountryEHAK) %>%
        mutate(TestPositivity =
                 ifelse((DailyTests_N + DailyTests_P) > 0,
                        (DailyTests_P / (DailyTests_N + DailyTests_P)),
                        NA)) %>%
      transmute(
        level_1_region = gsub(" maakond", "", .data$County),
        level_1_region_code = gsub("00", "EE-", .data$CountyEHAK),
        date = .data$StatisticsDate,
        cases_new = .data$DailyCases_P,
        tested_new = .data$DailyTests_P + .data$DailyTests_N,
        cases_total = .data$TotalCases_P,
        tested_total = .data$TotalTests_P + .data$TotalTests_N,
        test_positivity = .data$TestPositivity
      )
    }
  )
)
