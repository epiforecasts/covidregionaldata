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

  # Path to data
  url <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"

  # Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  # Read & clean data
  data <- mem_read(file = url, col_types = readr::cols()) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(state != "TOTAL") %>%
    dplyr::left_join(names, by = c("state" = "state_code")) %>%
    dplyr::select(date, region = state_name, cases_today = newCases, cumulative_cases = totalCases,
                  deaths_today = newDeaths, cumulative_deaths = deaths) %>%
    dplyr::group_by(date, region) %>%
    dplyr::summarise(cases_today = sum(cases_today),
                     cumulative_cases = sum(cumulative_cases),
                     deaths_today = sum(deaths_today),
                     cumulative_deaths = sum(cumulative_deaths))

  # Return specified data
  return(data)
}



