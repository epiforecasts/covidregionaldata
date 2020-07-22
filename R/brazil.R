#' Brazilian Regional Daily COVID-19 Count Data - States
#' 
#' @description Extracts daily COVID-19 data for Brazil, stratified by state. 
#' Data available on Github, curated by Wesley Cota: DOI 10.1590/SciELOPreprints.362
#' \url{https://github.com/wcota/covid19br}. It is loaded and then sanitised.
#' @return A data frame of daily Brazilian  Covid-19 data stratified by state,
#' to be further processed by \code{get_regional_data()}.
#' @importFrom dplyr %>% mutate filter select left_join group_by summarise
#' @importFrom lubridate ymd
#' @importFrom tibble tibble
#' 
get_brazil_regional_cases_only_level_1 <- function() {

  # Set region names ----------------------------------------------------------------------
  names <- tibble::tibble(
    state_name = c("Acre", "Amap\u00E1", "Amazonas", "Par\u00E1", "Rond\u00F4nia", "Roraima",
                   "Tocantins", "Alagoas", "Bahia", "Cear\u00E1", "Maranh\u00E3o", "Para\u00EDba",
                   "Pernambuco", "Piau\u00ED", "Rio Grande do Norte", "Sergipe", "Espirito Santo",
                   "Minas Gerais", "Rio de Janeiro", "S\u00E3o Paulo", "Paran\u00E1", "Rio Grande do Sul", 
                   "Santa Catarina", "Distrito Federal", "Goi\u00E1s", "Mato Grosso", "Mato Grosso do Sul"),
    state_code = c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "AL", "BA", "CE", "MA", "PB", "PE", 
                   "PI", "RN", "SE", "ES", "MG", "RJ", "SP", "PR", "RS", "SC", "DF", "GO", "MT", "MS"))

  # Read & clean data ---------------------------------------------------------------------
  url <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"

  data <- csv_reader(file = url) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(state != "TOTAL") %>%
    dplyr::left_join(names, by = c("state" = "state_code")) %>%
    dplyr::select(date, region_level_1 = state_name, cases_new = newCases, cases_total = totalCases,
                  deaths_new = newDeaths, deaths_total = deaths) %>%
    dplyr::group_by(date, region_level_1) %>%
    dplyr::summarise(cases_new = sum(as.numeric(cases_new)),
                     cases_total = sum(as.numeric(cases_total)),
                     deaths_new = sum(as.numeric(deaths_new)),
                     deaths_total = sum(as.numeric(deaths_total)), .groups = "drop_last") %>%
    dplyr::ungroup()

  return(data)
}

#' Brazilian Regional Daily COVID-19 Count Data - Cities
#' 
#' @description Extracts regional case and death counts for Brazil, stratified by city. 
#' Data available on Github, curated by Wesley Cota: DOI 10.1590/SciELOPreprints.362
#' \url{https://github.com/wcota/covid19br}. It is loaded and then sanitised.
#' @importFrom dplyr %>% mutate filter select left_join group_by summarise
#' @importFrom lubridate ymd
#' @importFrom tibble tibble
get_brazil_regional_cases_with_level_2 <- function() {

  # Set region names ----------------------------------------------------------------------
  names <- tibble::tibble(
    state_name = c("Acre", "Amap\u00E1", "Amazonas", "Par\u00E1", "Rond\u00F4nia", "Roraima",
                   "Tocantins", "Alagoas", "Bahia", "Cear\u00E1", "Maranh\u00E3o", "Para\u00EDba",
                   "Pernambuco", "Piau\u00ED", "Rio Grande do Norte", "Sergipe", "Espirito Santo",
                   "Minas Gerais", "Rio de Janeiro", "S\u00E3o Paulo", "Paran\u00E1", "Rio Grande do Sul", 
                   "Santa Catarina", "Distrito Federal", "Goi\u00E1s", "Mato Grosso", "Mato Grosso do Sul"),
    state_code = c("AC", "AP", "AM", "PA", "RO", "RR", "TO", "AL", "BA", "CE", "MA", "PB", "PE", 
                   "PI", "RN", "SE", "ES", "MG", "RJ", "SP", "PR", "RS", "SC", "DF", "GO", "MT", "MS"))
  
  # Read & clean data ---------------------------------------------------------------------
  url <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"

  data <- csv_reader(file = url) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(state != "TOTAL") %>%
    dplyr::mutate(city = gsub("/[A-Z]*", "", city)) %>%
    dplyr::mutate(city = dplyr::recode(city, "CASO SEM LOCALIZA<c3><87><c3><83>O DEFINIDA" = "Unknown City")) %>%
    dplyr::left_join(names, by = c("state" = "state_code")) %>%
    dplyr::select(date, region_level_1 = state_name, region_level_2 = city, cases_new = newCases, cases_total = totalCases,
                  deaths_new = newDeaths, deaths_total = deaths) %>%
    dplyr::group_by(date, region_level_1, region_level_2) %>%
    dplyr::summarise(cases_new = sum(as.numeric(cases_new)),
                     cases_total = sum(as.numeric(cases_total)),
                     deaths_new = sum(as.numeric(deaths_new)),
                     deaths_total = sum(as.numeric(deaths_total)), .groups = "drop_last") %>%
    dplyr::ungroup()
  
  return(data)
}

