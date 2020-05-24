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
#' @importFrom rnaturalearth ne_states
#' @examples
#'
#'
#'\dontrun{
#'
#' regions <- rnaturalearth::ne_states("Colombia", returnclass = "sf")
#' data <- get_colombia_regional_cases() %>%
#'           dplyr::filter(date == max(date))
#' region_with_data <- dplyr::left_join(regions, data, by = "iso_3166_2")
#' ggplot2::ggplot(regions_with_data) +
#'   ggplot2::geom_sf(ggplot2::aes(fill = cases))
#'}
#'

get_colombia_regional_cases <- function() {

  # Source data
  path <- "https://raw.githubusercontent.com/ideascol/covid19/master/data/data_dptos_trend.csv"

  # Set cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(readr::read_csv, cache = ch)

  # Read & rename
  colombia <- mem_read(path) %>%
    dplyr::select(date = fecha, region = departamento,
                  cases = casos_confirmados, deaths = casos_fallecido, tests = pruebas) %>%
    dplyr::mutate(
                  date = lubridate::mdy(date),
                  region = stringr::str_replace_all(region, " D.C.", ""),
                  region = stringr::str_replace_all(region, "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA", "SAN ANDRES Y PROVIDENCIA"),
                  region = stringr::str_to_sentence(region)
    )

  # Daily from cumulative
  colombia <- dplyr::group_by(colombia, region) %>%
    dplyr::mutate(
      cases = c(cases[1], diff(cases)),
      cases = ifelse(cases < 0, 0, cases),
      deaths = c(deaths[1], diff(deaths)),
      deaths = ifelse(deaths < 0, 0, deaths),
      tests = c(tests[1], diff(tests)),
      tests = ifelse(tests < 0, 0, tests)) %>%
    dplyr::ungroup()

  # Get region ISO codes
  co_map <- rnaturalearth::ne_states(country = "Colombia")
  co_iso <- co_map@data %>%
    dplyr::select(name, iso_3166_2) %>%
    dplyr::mutate(name = stringr::str_to_sentence(name),
                  name = stringr::str_replace_all(name, "á", "a"),
                  name = stringr::str_replace_all(name, "í", "i"),
                  name = stringr::str_replace_all(name, "ó", "o"),
                  name = stringr::str_replace_all(name, "é", "e")
                  )

  # Merge ISO codes with data
  colombia <- dplyr::left_join(colombia, co_iso, by = c("region" = "name"))

return(colombia)

}
