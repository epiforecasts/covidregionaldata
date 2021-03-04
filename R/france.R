#' French Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for France, stratified by region and departement.
#' Data available at \url{https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/}.
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by Region in France, ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr filter select mutate left_join rename
#' @importFrom lubridate as_date ymd
#' @importFrom tibble tibble

get_france_regional_cases_only_level_1 <- function() {
  level_1_region_code <- tibble::tibble(
    iso_code = c(
      "FR-ARA", "FR-BFC", "FR-BRE", "FR-CVL", "FR-20R", "FR-GES", "FR-GP", "FR-GF", "FR-HDF", "FR-IDF", "FR-RE", "FR-MQ", "FR-YT",
      "FR-NOR", "FR-NAQ", "FR-OCC", "FR-PDL", "FR-PAC", "FR-PM", "FR-BL", "FR-MF"
    ),
    insee_code = c("84", "27", "53", "24", "94", "44", "01", "03", "32", "11", "04", "02", "06", "28", "75", "76", "52", "93", "05", "07", "08"),
    region_level_1 = c(
      "Auvergne-Rhone-Alpes", "Bourgogne-Franche-Comte", "Bretagne", "Centre-Val de Loire", "Corse", "Grand-Est", "Guadeloupe",
      "Guyane (francaise)", "Hauts-de-France", "Ile-de-France", "La Reunion", "Martinique", "Mayotte", "Normandie", "Nouvelle-Aquitaine",
      "Occitanie", "Pays-de-la-Loire", "Provence-Alpes-Cote-d'Azur", "Saint-Barthelemy", "Saint-Martin", "Saint-Pierre et Miquelon"
    )
  )


  # Read data --------------------------------------------------------------------
  cases_url <- "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"

  cases_data <- csv_reader(file = cases_url) %>%
    dplyr::filter(cl_age90 == 0) %>%
    dplyr::select(
      date = jour,
      insee_code = reg,
      cases_new = P,
      tested_new = `T`
    ) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date))) %>%
    dplyr::left_join(level_1_region_code, by = "insee_code") %>%
    dplyr::rename("level_1_region_code" = "iso_code")

  return(cases_data)
}

#' French Regional Daily COVID-19 Count Data - Departement
#'
#' @description Extracts daily COVID-19 data for France, stratified by departement.
#' Data available at \url{https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/}.
#' It is loaded and then sanitised.
#' @return A data.frame of COVID cases by departement in France, ready to be used by get_regional_data().
#' @importFrom dplyr filter select mutate full_join left_join rename bind_rows
#' @importFrom lubridate as_date ymd
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom tibble tibble
#'
get_france_regional_cases_with_level_2 <- function() {

  # Get region names and codes
  level_2_codes_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:FR"
  level_2_codes_table <- level_2_codes_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
    rvest::html_table(fill = TRUE)

  missing_level_2_codes <- tibble::tibble(
    level_1_region_code = c("FR-PM", "FR-BL", "FR-MF"),
    level_2_region_code = c("FR-977", "FR-978", "FR-975"),
    region_level_1 = c("Saint-Barthelemy", "Saint-Martin", "Saint-Pierre et Miquelon"),
    region_level_2 = region_level_1
  )

  level_2_codes <- level_2_codes_table[[2]] %>%
    dplyr::select(
      level_2_region_code = Code,
      region_level_2 = `Subdivision name`,
      level_1_region_code = `In region(since 2016)`
    ) %>%
    dplyr::mutate(level_1_region_code = paste0("FR-", level_1_region_code)) %>%
    dplyr::left_join(level_2_codes_table[[1]], by = c("level_1_region_code" = "Code")) %>%
    dplyr::rename(region_level_1 = `Subdivision name (fr)`) %>%
    dplyr::select(-`Subdivision category`) %>%
    dplyr::bind_rows(missing_level_2_codes)


  # Read data --------------------------------------------------------------------
  cases_url <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"
  hosp_url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"

  cases_data <- csv_reader(file = cases_url) %>%
    dplyr::filter(cl_age90 == 0) %>%
    dplyr::select(
      date = jour,
      level_2_region_code = dep,
      cases_new = P,
      tested_new = `T`
    ) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))

  hosp_data <- csv_reader(file = hosp_url) %>%
    dplyr::select(
      date = jour,
      level_2_region_code = dep,
      hosp_new = incid_hosp,
      deaths_new = incid_dc
    ) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))

  data <- dplyr::full_join(cases_data, hosp_data, by = c("date", "level_2_region_code")) %>%
    dplyr::mutate(level_2_region_code = paste0("FR-", level_2_region_code)) %>%
    dplyr::left_join(level_2_codes, by = "level_2_region_code")

  return(data)
}
