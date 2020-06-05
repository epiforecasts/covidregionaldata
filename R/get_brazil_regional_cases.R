#' Brazil regional cases and deaths

#' @description Extracts regional case and death counts for Brazil.
#' Data available on Github, curated by Wesley Cota: DOI 10.1590/SciELOPreprints.362
#' [Source](https://github.com/wcota/covid19br)
#' @importFrom readr read_csv cols
#' @importFrom dplyr %>% mutate filter select left_join group_by summarise
#' @importFrom lubridate ymd
#' @importFrom memoise cache_filesystem memoise
#'
get_brazil_regional_cases <- function() {

  # Set region names
  names <- tibble::tibble(
    state_name = c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima","Tocantins","Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe","Espirito Santo","Minas Gerais","Rio de Janeiro","São Paulo","Paraná","Rio Grande do Sul","Santa Catarina","Distrito Federal","Goiás","Mato Grosso","Mato Grosso do Sul"),
    state_code = c("AC","AP","AM","PA","RO","RR","TO", "AL","BA","CE","MA","PB","PE","PI","RN","SE", "ES","MG","RJ","SP", "PR","RS","SC", "DF","GO","MT","MS"))

  # Read & clean data
  url <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"

  data <- csv_reader(file = url) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(state != "TOTAL") %>%
    dplyr::left_join(names, by = c("state" = "state_code")) %>%
    dplyr::select(date, region = state_name, cases_new = newCases, cases_total = totalCases,
                  deaths_new = newDeaths, deaths_total = deaths) %>%
    dplyr::group_by(date, region) %>%
    dplyr::summarise(cases_new = sum(cases_new),
                     cases_total = sum(cases_total),
                     deaths_new = sum(deaths_new),
                     deaths_total = sum(deaths_total)) %>%
    dplyr::ungroup()

  # Return specified data
  return(data)
}



