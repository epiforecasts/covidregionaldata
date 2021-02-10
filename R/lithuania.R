#' Lithuania Regional Daily COVID-19 Count Data - Region
#'
#' @description Extracts daily COVID-19 data for Lithuania, stratified by county (apskritis) and municipality.
#' Data available at \url{ftp://atviriduomenys.nvsc.lt/}.
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by county in Lithuania, ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr %>% filter select mutate left_join rename
#' @importFrom lubridate as_date ymd
#' @importFrom tibble tibble

get_france_regional_cases_only_level_1 <- function() {

  level_1_region_code <- tibble::tibble(
    iso_code = c("FR-ARA", "FR-BFC", "FR-BRE", "FR-CVL", "FR-20R", "FR-GES", "FR-GP", "FR-GF", "FR-HDF", "FR-IDF", "FR-RE", "FR-MQ", "FR-YT",
                 "FR-NOR", "FR-NAQ", "FR-OCC", "FR-PDL", "FR-PAC", "FR-PM", "FR-BL", "FR-MF"),
    insee_code = c("84", "27", "53", "24", "94", "44", "01", "03", "32", "11", "04", "02", "06", "28", "75", "76", "52", "93", "05", "07", "08"),
    region_level_1 = c("Auvergne-Rhone-Alpes", "Bourgogne-Franche-Comte", "Bretagne", "Centre-Val de Loire", "Corse", "Grand-Est", "Guadeloupe",
                       "Guyane (francaise)", "Hauts-de-France", "Ile-de-France", "La Reunion", "Martinique", "Mayotte", "Normandie", "Nouvelle-Aquitaine",
                       "Occitanie", "Pays-de-la-Loire", "Provence-Alpes-Cote-d'Azur", "Saint-Barthelemy", "Saint-Martin", "Saint-Pierre et Miquelon"))


  # Read data --------------------------------------------------------------------
  #cases_url <- "https://www.data.gouv.fr/fr/datasets/r/001aca18-df6a-45c8-89e6-f82d689e6c01"
  cases_url <- "https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0.csv"
  
  cases_data <- csv_reader(file = cases_url) %>%
    dplyr::filter(cl_age90 == 0) %>%
    dplyr::select(date = jour,
                  insee_code = reg,
                  cases_new = P,
                  tested_new = `T`) %>%
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
get_lithuania_regional_cases_with_level_2 <- function() {

  # Get region names and codes
  level_2_codes_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:LT"
  level_2_codes_table <- level_2_codes_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
    rvest::html_table(fill = TRUE)


  level_2_lookup <- structure(list(iso_code = c("LT-01", "LT-02", "LT-03", "LT-04", "LT-05", 
                          "LT-06", "LT-07", "LT-08", "LT-09", "LT-10", "LT-11", "LT-12", 
                          "LT-13", "LT-14", "LT-16", "LT-15", "LT-17", "LT-18", "LT-19", 
                          "LT-21", "LT-20", "LT-22", "LT-23", "LT-24", "LT-25", "LT-26", 
                          "LT-27", "LT-28", "LT-29", "LT-30", "LT-31", "LT-32", "LT-33", 
                          "LT-34", "LT-35", "LT-36", "LT-37", "LT-38", "LT-39", "LT-40", 
                          "LT-41", "LT-42", "LT-44", "LT-43", "LT-45", "LT-46", "LT-47", 
                          "LT-48", "LT-49", "LT-50", "LT-51", "LT-52", "LT-53", "LT-54", 
                          "LT-55", "LT-56", "LT-57", "LT-58", "LT-59", "LT-60"),
                 osp_municipality_name = c("Akmenės r. sav.", 
                               "Alytaus m. sav.", "Alytaus r. sav.", "Anykščių r. sav.", 
                               "Birštono sav.", "Biržų r. sav.", "Druskininkų sav.", "Elektrėnų sav.", 
                               "Ignalinos r. sav.", "Jonavos r. sav.", "Joniškio r. sav.", 
                               "Jurbarko r. sav.", "Kaišiadorių r. sav.", "Kalvarijos sav.", 
                               "Kauno r. sav.", "Kauno m. sav.", "Kazlų Rūdos sav.", "Kėdainių r. sav.", 
                               "Kelmės r. sav.", "Klaipėdos r. sav.", "Klaipėdos m. sav.", 
                               "Kretingos r. sav.", "Kupiškio r. sav.", "Lazdijų r. sav.", 
                               "Marijampolės sav.", "Mažeikių r. sav.", "Molėtų r. sav.", 
                               "Neringos sav.", "Pagėgių sav.", "Pakruojo r. sav.", "Palangos m. sav.", 
                               "Panevėžio m. sav.", "Panevėžio r. sav.", "Pasvalio r. sav.", 
                               "Plungės r. sav.", "Prienų r. sav.", "Radviliškio r. sav.", 
                               "Raseinių r. sav.", "Rietavo sav.", "Rokiškio r. sav.", "Šakių r. sav.", 
                               "Šalčininkų r. sav.", "Šiaulių r. sav.", "Šiaulių m. sav.", 
                               "Šilalės r. sav.", "Šilutės r. sav.", "Širvintų r. sav.", 
                               "Skuodo r. sav.", "Švenčionių r. sav.", "Tauragės r. sav.", 
                               "Telšių r. sav.", "Trakų r. sav.", "Ukmergės r. sav.", "Utenos r. sav.", 
                               "Varėnos r. sav.", "Vilkaviškio r. sav.", "Vilniaus m. sav.", 
                               "Vilniaus r. sav.", "Visagino sav.", "Zarasų r. sav."),
                 `Subdivision name` = c("Akmenė", 
                                        "Alytaus miestas", "Alytus", "Anykščiai", "Birštono", "Biržai", 
                                        "Druskininkai", "Elektrėnai", "Ignalina", "Jonava", "Joniškis", 
                                        "Jurbarkas", "Kaišiadorys", "Kalvarijos", "Kaunas", "Kauno miestas", 
                                        "Kazlų Rūdos", "Kėdainiai", "Kelmė", "Klaipėda", "Klaipėdos miestas", 
                                        "Kretinga", "Kupiškis", "Lazdijai", "Marijampolė", "Mažeikiai", 
                                        "Molėtai", "Neringa", "Pagėgiai", "Pakruojis", "Palangos miestas", 
                                        "Panevėžio miestas", "Panevėžys", "Pasvalys", "Plungė", 
                                        "Prienai", "Radviliškis", "Raseiniai", "Rietavo", "Rokiškis", 
                                        "Šakiai", "Šalčininkai", "Šiauliai", "Šiaulių miestas", 
                                        "Šilalė", "Šilutė", "Širvintos", "Skuodas", "Švenčionys", 
                                        "Tauragė", "Telšiai", "Trakai", "Ukmergė", "Utena", "Varėna", 
                                        "Vilkaviškis", "Vilniaus miestas", "Vilnius", "Visaginas", "Zarasai"
                 ),
                 `Subdivision category` = c("district municipality", "city municipality", 
                                            "district municipality", "district municipality", "municipality", 
                                            "district municipality", "municipality", "municipality", "district municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "district municipality", "municipality", "district municipality", 
                                            "city municipality", "municipality", "district municipality", 
                                            "district municipality", "district municipality", "city municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "municipality", "municipality", "district municipality", "city municipality", 
                                            "city municipality", "district municipality", "district municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "district municipality", "municipality", "district municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "city municipality", "district municipality", "district municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "district municipality", "district municipality", "district municipality", 
                                            "district municipality", "city municipality", "district municipality", 
                                            "municipality", "district municipality")),
    row.names = c(NA, -60L), class = "data.frame")
  
  missing_level_2_codes = tibble::tibble(
    level_1_region_code = c("FR-PM", "FR-BL", "FR-MF"),
    level_2_region_code =  c("FR-977", "FR-978", "FR-975"),
    region_level_1 = c("Saint-Barthelemy", "Saint-Martin", "Saint-Pierre et Miquelon"),
    region_level_2 = region_level_1)

  level_2_codes <- level_2_codes_table[[2]] %>%
    dplyr::select(level_2_region_code = Code,
                  region_level_2 = `Subdivision name`,
                  level_1_region_code = `In region(since 2016)`) %>%
    dplyr::mutate(level_1_region_code = paste0("FR-", level_1_region_code)) %>%
    dplyr::left_join(level_2_codes_table[[1]], by = c("level_1_region_code" = "Code")) %>%
    dplyr::rename(region_level_1 = `Subdivision name (fr)`) %>%
    dplyr::select(-`Subdivision category`) %>%
    dplyr::bind_rows(missing_level_2_codes)


  # Read data --------------------------------------------------------------------
  #cases_url <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"
  #hosp_url <- "https://www.data.gouv.fr/fr/datasets/r/6fadff46-9efd-4c53-942a-54aca783c30c"

  cases_url <- "https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0.csv"
  
  cases_data <- csv_reader(file = cases_url) %>%
    dplyr::select(date,
                  region_level_2 = municipality_name,
                  cases_new = confirmed_cases,
                  cases_total = confirmed_cases_cumulative,
                  deaths_new = deaths,
                  deaths_total = deaths_cumulative,
                  recovered_new = recovered_cases,
                  recovered_total = recovered_cases_cumulative) %>%
    dplyr::mutate(date = lubridate::as_date(date))

    ## This is the list of fields which we're trying to generate, copied from get_regional_data.R
    # date, region_level_2, level_2_region_code, region_level_1, level_1_region_code, 
    # cases_new, cases_total, deaths_new, deaths_total,
    # recovered_new, recovered_total, hosp_new, hosp_total,
    # tested_new, tested_total, dplyr::everything())
  
  # hosp_data <- csv_reader(file = hosp_url) %>%
  #   dplyr::select(date = jour,
  #                 level_2_region_code = dep,
  #                 hosp_new = incid_hosp,
  #                 deaths_new = incid_dc) %>%
  #   dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))

  data <- dplyr::full_join(cases_data, hosp_data, by = c("date", "level_2_region_code")) %>%
    dplyr::mutate(level_2_region_code = paste0("FR-", level_2_region_code)) %>%
    dplyr::left_join(level_2_codes, by = "level_2_region_code")

  return(data)
}
