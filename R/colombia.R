#' Colombian Regional Daily COVID-19 Count Data - Department
#' 
#' @description Extracts daily COVID-19 data for Colombia, stratified by departamento. 
#' Data available at  \url{https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv}. 
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by department in Colombia, ready to be used by \code{get_regional_covid_data()}.
#' @importFrom dplyr select left_join mutate %>%
#' @importFrom lubridate mdy
#' @importFrom stringr str_replace_all str_to_sentence
#' @importFrom tidyr complete full_seq fill
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' 
get_colombia_regional_cases <- function() {
  
  # Read & rename -----------------------------------------------------------------
  url <- "https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv"

  colombia <- csv_reader(url) %>%
    dplyr::select(date = fecha, region_level_1 = departamento,
                  cases_total = casos_confirmados, deaths_total = casos_fallecido, tested_total = pruebas) %>%
    dplyr::mutate(date = lubridate::mdy(date),
                  region_level_1 = iconv(region_level_1, from = "UTF-8", to = "ASCII//TRANSLIT"),
                  region_level_1 = stringr::str_replace_all(region_level_1, " D.C.", ""),
                  region_level_1 = stringr::str_replace_all(region_level_1, "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA", "San Andres, Providencia y Santa Catalina"),
                  region_level_1 = stringr::str_to_sentence(region_level_1))

  # Get ISO codes -----------------------------------------------------------------
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

  # Merge ISO codes with data ------------------------------------------------------
  colombia <- dplyr::left_join(colombia, iso, by = "region_level_1")

  return(colombia)
}
