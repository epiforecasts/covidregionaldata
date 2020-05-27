#' Brazil regional cases and deaths, daily time-series

#' @description Extracts regional case and death counts for Brazil.
#' Data available on Github, curated by Wesley Cota: DOI 10.1590/SciELOPreprints.362
#' [Source](https://github.com/wcota/covid19br)
#' @param geography Character string specifying geography: "regions": n=5, or "states": n=27, or "municipalities": n=3180. Default: "states".
#' @importFrom readr read_csv
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter select left_join group_by summarise
#' @importFrom lubridate ymd
#' @importFrom memoise cache_filesystem memoise
#' @export
#' @examples
#' get_brazil_regional_cases()
#'
#' \dontrun{
#'
#' # Mapping
#'
#' brazil_latest <- get_brazil_regional_cases(geography = "states") %>%
#'   dplyr::filter(date == max(date))
#'
#' brazil_map <- rnaturalearth::ne_states(country = "Brazil", returnclass = "sf") %>%
#'   dplyr::mutate(state_code = stringr::str_remove_all(iso_3166_2, "BR-"))
#'
#' brazil_map_data <- dplyr::left_join(brazil_map, brazil_latest,
#' by = c("state_code" = "state_code")) %>%
#'   ggplot2::ggplot() +
#'   ggplot2::geom_sf(ggplot2::aes(fill = cases))
#'
#' }

get_brazil_regional_cases <- function(geography = "states") {

  if(!geography %in% c("regions", "states", "municipalities")) {
    stop('Please specify geography: "regions" (n=5), "states" (n=27), "municipalities" (n=3180). Default: "states".')
  }

  # Set region names
  names <- tibble::tibble(
    state_name = c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima","Tocantins",
                   "Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí",
                   "Rio Grande do Norte","Sergipe","Espirito Santo","Minas Gerais","Rio de Janeiro",
                   "São Paulo","Paraná","Rio Grande do Sul","Santa Catarina","Distrito Federal","Goiás",
                   "Mato Grosso","Mato Grosso do Sul"),
    state_code = c("AC","AP","AM","PA","RO","RR","TO", "AL","BA","CE","MA","PB",
                   "PE","PI","RN","SE", "ES","MG","RJ","SP", "PR","RS","SC", "DF",
                   "GO","MT","MS"),
    region_name = c(rep("Norte", 7), rep("Nordeste", 9), rep("Sudeste", 4), rep("Sul", 3), rep("Centro-Oeste", 4)))

  # Path to data
  mcty_path <- "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"

  # Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  # Read & clean data
  brazil_mcty <- suppressMessages(suppressWarnings(mem_read(file = mcty_path))) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(state != "TOTAL") %>%
    dplyr::select(date, deaths = newDeaths, cases = newCases, city_name = city, state_code = state) %>%
    dplyr::left_join(names, by = c("state_code" = "state_code"))

  # Return specified data

  if(geography == "municipalities"){

    return(brazil_mcty)

  }else

  if(geography == "states"){

    brazil_states <- brazil_mcty %>%
      dplyr::group_by(date, state_code, state_name) %>%
      dplyr::summarise(cases = sum(cases),
                       deaths = sum(deaths))

    return(brazil_states)

  }else

  if(geography == "regions"){
    brazil_regions <- brazil_mcty %>%
      dplyr::group_by(date, region_name) %>%
      dplyr::summarise(cases = sum(cases),
                       deaths = sum(deaths))
  }

}
