#' Afghan Provincial Daily Case Counts
#'
#' @description Data from HDX https://data.humdata.org/dataset/afghanistan-covid-19-statistics-per-province
#' The cumulative data is stored in a google sheet, which is read as a csv and de-cumulated.
#'
#'
#' @author Flavio Finger @ffinger
#'
#'
#' @return A dataframe of daily Afghan regional cases, deaths, and recovered
#' @importFrom dplyr transmute arrange mutate group_by ungroup
#' @importFrom tidyr complete full_seq fill
#' @importFrom lubridate ymd
#' @importFrom stringr str_remove_all
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @export
#' @examples
#' \dontrun{
#' ## Code
#' get_afghan_regional_cases()
#' ## Mapping
#' regions <- rnaturalearth::ne_states(geounit = "Afghanistan", returnclass = "sf")
#' data <- get_afghan_regional_cases() %>%
#'   dplyr::filter(date == max(date))
#' regions_with_data <- dplyr::left_join(regions, data, by = "iso_3166_2")
#' regions_with_data %>%
#'   ggplot2::ggplot(ggplot2::aes(fill = cases)) +
#'   ggplot2::geom_sf()
#'}

get_afghan_regional_cases <- function() {

  url <- "https://docs.google.com/spreadsheets/d/1F-AMEDtqK78EA6LYME2oOsWQsgJi4CT3V_G4Uo-47Rg/export?format=csv"

  ## Set up cache
  ch <- memoise::cache_filesystem(".cache")
  mem_read <- memoise::memoise(read.csv, cache = ch)

  #download
  cls <- c(
      Province = "character",
      Cases = "character",
      Deaths = "character",
      Recoveries = "character",
      Active.Cases = "character",
      Date = "character"
  )
  data <- mem_read(url, stringsAsFactors = FALSE, colClass = cls)

  #reformat
  data <- suppressWarnings(dplyr::transmute(data,
    date = lubridate::ymd(Date),
    country = "Afghanistan",
    region = stringr::str_replace(Province, " Province", ""),
    cases = Cases,
    deaths = Deaths,
    recovered = Recoveries
    )) %>%
    dplyr::filter(!region %in% "#adm1+name")

  #transform (remove commas in numbers)
  data <- suppressWarnings(dplyr::mutate(data,
    cases = stringr::str_remove_all(cases, ","),
    cases = as.integer(cases),
    deaths = stringr::str_remove_all(deaths, ","),
    deaths = as.integer(deaths),
    recovered = stringr::str_remove_all(recovered, ","),
    recovered = as.integer(recovered)
  ))

  # put NA where gaps
  data <- tidyr::complete(data,
    date = tidyr::full_seq(date, period  = 1),
    country,
    region,
    fill = list(
        cases = NA_integer_,
        deaths = NA_integer_,
        recovered = NA_integer_
        )
    )

  # fill NA with previous values
  data <- dplyr::ungroup(tidyr::fill(
      dplyr::group_by(data, country, region),
      cases,
      deaths,
      recovered
    ))

  # de-cumulate
  data <- data %>%
    dplyr::group_by(country, region) %>%
    dplyr::mutate(
    cases = c(cases[1], diff(cases)),
    cases = ifelse(cases < 0, 0, cases),
    deaths = c(deaths[1], diff(deaths)),
    deaths = ifelse(deaths < 0, 0, deaths),
    recovered = c(recovered[1], diff(recovered)),
    recovered = ifelse(recovered < 0, 0, recovered)
    ) %>%
    dplyr::ungroup()


  # arrange
  data <- dplyr::arrange(data, date, country, region)

  # get ISO codes
  region_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:AF"
  iso <- region_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
    rvest::html_table()
  iso <- iso[[1]] %>%
    dplyr::select(iso_3166_2 = Code, name_en = 3)

  # join to data & edit manually where needed
  data <- dplyr::left_join(data, iso, by = c("region" = "name_en")) %>%
    dplyr::mutate(
      iso_3166_2 = ifelse(region == "Ghor", "AF-GHO", iso_3166_2),
      iso_3166_2 = ifelse(region == "Wardak", "AF-WAR", iso_3166_2))

return(data)
}
