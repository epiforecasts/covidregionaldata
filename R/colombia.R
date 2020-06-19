#' Get Colombia daily cases
#'
#'
#' @description Fetches COVID case, death, and test counts by region
#' Sourced from Ideas Col: https://github.com/ideascol/covid19
#' @return A dataframe of case, death and testing counts in Colombian regions
#' @export
#' @importFrom memoise cache_filesystem memoise
#' @importFrom dplyr select group_by ungroup left_join
#' @importFrom lubridate mdy
#' @importFrom readr read_csv
#' @importFrom stringr str_replace_all str_to_sentence
#' @importFrom tidyr complete full_seq fill
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @examples
#'
#'
#'\dontrun{
#'
#' regions <- rnaturalearth::ne_states("Colombia", returnclass = "sf")
#' data <- get_colombia_regional_cases() %>%
#'           dplyr::filter(date == max(date))
#' regions_with_data <- dplyr::left_join(regions, data, by = "iso_3166_2")
#' regions_with_data %>%
#'   ggplot2::ggplot() +
#'   ggplot2::geom_sf(ggplot2::aes(fill = cases))
#'}
#'
get_colombia_regional_cases <- function() {

  # Source data
  url <- "https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv"

  # Read & rename
  colombia <- csv_reader(url) %>%
    dplyr::select(date = fecha, region_level_1 = departamento,
                  cases_total = casos_confirmados, deaths_total = casos_fallecido, tested_total = pruebas) %>%
    dplyr::mutate(date = lubridate::mdy(date),
                  region_level_1 = iconv(region_level_1, from = "UTF-8", to = "ASCII//TRANSLIT"),
                  region_level_1 = stringr::str_replace_all(region_level_1, " D.C.", ""),
                  region_level_1 = stringr::str_replace_all(region_level_1, "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA", "San Andres, Providencia y Santa Catalina"),
                  region_level_1 = stringr::str_to_sentence(region_level_1))

  # Get ISO codes
  region_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:CO"
  iso_table <- region_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
    rvest::html_table()
  iso <- iso_table[[1]] %>%
    dplyr::select(iso_code = Code, region_level_1 = 2) %>%
    dplyr::mutate(region_level_1 = iconv(x = region_level_1, from = "UTF-8", to = "ASCII//TRANSLIT"),
                  region_level_1 = stringr::str_replace_all(region_level_1, "Distrito Capital de ", ""),
                  region_level_1 = stringr::str_to_sentence(region_level_1))

  # Merge ISO codes with data
  colombia <- dplyr::left_join(colombia, iso, by = "region_level_1")

  return(colombia)
}
