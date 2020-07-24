#' Colombian Regional Daily COVID-19 Count Data - Department
#'
#' @description Extracts daily COVID-19 data for Colombia, stratified by departamento.
#' Data available at  \url{https://github.com/danielcs88/colombia_covid-19}.
#' It is loaded and then cleaned.
#' @return A data frame of COVID cases by department in Colombia, ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr select mutate %>%
#' @importFrom lubridate mdy
#' @importFrom stringr str_replace_all str_to_sentence
#'
get_colombia_regional_cases <- function() {

  # Read & rename -----------------------------------------------------------------
  url <- "https://raw.githubusercontent.com/danielcs88/colombia_covid-19/master/datos/cronologia.csv"
  colombia <- csv_reader(url) %>%
    dplyr::select(date = fecha, region_level_1 = departamento, cases_total = casos) %>%
    dplyr::mutate(date = lubridate::ymd(date),
                  region_level_1 = iconv(region_level_1, from = "UTF-8", to = "ASCII//TRANSLIT"),
                  region_level_1 = stringr::str_replace_all(region_level_1, " D.C.", ""),
                  region_level_1 = stringr::str_replace_all(region_level_1, "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA", "San Andres, Providencia y Santa Catalina"),
                  region_level_1 = stringr::str_to_sentence(region_level_1))

  return(colombia)
}
