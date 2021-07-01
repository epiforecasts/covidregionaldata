#' Brazil Class for downloading, cleaning and processing notification data
#' @description Information for downloading, cleaning
#'  and processing COVID-19 region data for Brazil.
#'
#'
#' Data available on Github, curated by Wesley Cota:
#' DOI 10.1590/SciELOPreprints.362
#'
#' @source \url{https://github.com/wcota/covid19br}
#' @concept dataset
#' @family subnational
#' @export
#' @examples
#' \dontrun{
#' region <- Brazil$new(verbose = TRUE, steps = TRUE, get = TRUE)
#' region$return()
#' }
Brazil <- R6::R6Class("Brazil",
  inherit = DataClass,
  public = list(

    # Core Attributes
    #' @field origin name of origin to fetch data for
    origin = "Brazil",
    #' @field supported_levels A list of supported levels.
    supported_levels = list("1", "2"),
    #' @field supported_region_names A list of region names in order of level.
    supported_region_names = list(
      "1" = "state",
      "2" = "city"
    ),
    #' @field supported_region_codes A list of region codes in order of level.
    supported_region_codes = list(
      "1" = "iso_3166_2"
    ),
    #' @field common_data_urls List of named links to raw data. Data is
    #' available at the city level and is aggregated to provide state data.
    common_data_urls = list(
      "main" = "https://github.com/wcota/covid19br/raw/master/cases-brazil-cities-time.csv.gz" # nolint
    ),
    #' @field source_data_cols existing columns within the raw data
    source_data_cols = c("cases_total", "deaths_total"),
    #' @field source_text Plain text description of the source of the data
    source_text = "Wesley Cota",
    #' @field source_url Website address for explanation/introduction of the
    #' data
    source_url = "https://github.com/wcota/covid19br/blob/master/README.en.md",

    #' @description Set up a table of region codes for clean data
    #' @importFrom tibble tribble
    set_region_codes = function() {
      self$codes_lookup <- tibble(
        state_name = c(
          "Acre", "Amap\u00e1", "Amazonas", "Par\u00e1", "Rond\u00f4nia",
          "Roraima", "Tocantins", "Alagoas", "Bahia", "Cear\u00e1",
          "Maranh\u00e3o", "Para\u00edba", "Pernambuco", "Piau\u00ed",
          "Rio Grande do Norte", "Sergipe", "Espirito Santo", "Minas Gerais",
          "Rio de Janeiro", "S\u00e3o Paulo", "Paran\u00e1",
          "Rio Grande do Sul", "Santa Catarina", "Distrito Federal",
          "Goi\u00e1s", "Mato Grosso", "Mato Grosso do Sul"
        ),
        level_1_region_code = c(
          "AC", "AP", "AM", "PA", "RO", "RR", "TO", "AL", "BA", "CE",
          "MA", "PB", "PE", "PI", "RN", "SE", "ES", "MG", "RJ", "SP",
          "PR", "RS", "SC", "DF", "GO", "MT", "MS"
        )
      )
    },

    #' @description Common data cleaning for both levels
    #' @importFrom dplyr mutate filter select left_join group_by summarise
    #' @importFrom lubridate ymd
    clean_common = function() {
      self$data$clean <- self$data$raw$main %>%
        mutate(date = ymd(date)) %>%
        filter(state != "TOTAL") %>%
        left_join(self$codes_lookup,
          by = c("state" = "level_1_region_code")
        ) %>%
        mutate(state = gsub("^", "BR-", state)) %>%
        select(date,
          level_1_region = state_name,
          level_2_region = city,
          level_1_region_code = state,
          cases_new = newCases,
          cases_total = totalCases,
          deaths_new = newDeaths,
          deaths_total = deaths
        )
    },

    #' @description State Level Data Cleaning
    #' @importFrom dplyr select left_join group_by summarise ungroup
    clean_level_1 = function() {
      self$data$clean <- self$data$clean %>%
        select(-level_2_region) %>%
        group_by(date, level_1_region, level_1_region_code) %>%
        summarise(
          cases_new = sum(as.numeric(cases_new)),
          cases_total = sum(as.numeric(cases_total)),
          deaths_new = sum(as.numeric(deaths_new)),
          deaths_total = sum(as.numeric(deaths_total)),
          .groups = "drop_last"
        ) %>%
        ungroup()
    },

    #' @description City Level Data Cleaning
    # nolint start
    #' @importFrom dplyr mutate select left_join group_by summarise recode ungroup
    # nolint end
    clean_level_2 = function() {
      self$data$clean <- self$data$clean %>%
        mutate(level_2_region = gsub("/[A-Z]*", "", level_2_region)) %>%
        mutate(level_2_region = gsub(
          "^CASO SEM.*DEFINIDA",
          "Unknown City",
          level_2_region
        )) %>%
        group_by(date, level_1_region, level_1_region_code, level_2_region) %>%
        summarise(
          cases_new = sum(as.numeric(cases_new)),
          cases_total = sum(as.numeric(cases_total)),
          deaths_new = sum(as.numeric(deaths_new)),
          deaths_total = sum(as.numeric(deaths_total)),
          .groups = "drop_last"
        ) %>%
        ungroup()
    }
  )
)
