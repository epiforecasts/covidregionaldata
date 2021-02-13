#' Lithuania Regional Daily COVID-19 Count Data - County
#'
#' @description Extracts daily COVID-19 data for Lithuania, stratified by county (apskritis) 
#' and municipality (savivaldybe). Some Lithuanian municipalities share names, there being both a
#' Vilnius city municipality (m. sav.) and a Vilnius regional municipality (r. sav.)
#' Data available at \url{https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0}.
#' It is loaded and then sanitised.
#' @return A data frame of COVID cases by county in Lithuania, ready to be used by \code{get_regional_data()}.
#' @importFrom dplyr %>% filter select mutate left_join rename across
#' @importFrom lubridate as_date ymd
#' @importFrom tibble tibble

get_lithuania_regional_cases_only_level_1 <- function() {

  # Lithuania only publishes data at the municipality level. To provide
  # data for the level 1 regions (Counties, Apskritis) we get the municipality
  # level data and aggregate it according to municipality
  
  # level_1_lookup <- tibble::tibble(level_1_region_code = c("LT-AL", "LT-KU", "LT-KL", "LT-MR", "LT-PN", 
  #                                        "LT-SA", "LT-TA", "LT-TE", "LT-UT", "LT-VL", NA_character_),
  #                region_level_1 = c("Alytaus apskritis", 
  #                                   "Kauno apskritis", "Klaipėdos apskritis", "Marijampolės apskritis", 
  #                                   "Panevėžio apskritis", "Šiaulių apskritis", "Tauragės apskritis", 
  #                                   "Telšių apskritis", "Utenos apskritis", "Vilniaus apskritis", "nenustatyta"),
  #                region_level_1_en = c("Alytus County", "Kaunas County", 
  #                                      "Klaipėda County", "Marijampolė County", "Panevėžys County", 
  #                                      "Šiauliai County", "Tauragė County", "Telšiai County", "Utena County", 
  #                                      "Vilnius County", "unstated"))
  # 
  county_data <- get_lithuania_regional_cases_with_level_2() %>%
    group_by(date,region_level_1) %>%
    summarise(across(where(is.numeric), sum))
  
  return(county_data)
  
}

#' Lithuanian Daily COVID-19 Count Data - Municipalities
#'
#' @description Extracts daily COVID-19 data for Lithuania, by municipality.
#' Data available at \url{https://opendata.arcgis.com/datasets/45b76303953d40e2996a3da255bf8fe8_0}.
#' It is loaded and then sanitised.
#' @return A data.frame of COVID cases by municipality in Lithuania, ready to be used by get_regional_data().
#' @importFrom dplyr filter select mutate full_join left_join rename bind_rows
#' @importFrom lubridate as_date ymd
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
#' @importFrom tibble tibble
#'
get_lithuania_regional_cases_with_level_2 <- function() {

  # The following code, adjusted from a version for France, was initially used to
  # create lookup tables of Lithuanian municipality and country codes.
  # These were then adjusted to match the format used by the 
  # Official Statistics Portal in their open data and are left as 
  # hard-coded tibbles. These codes have not changed in ten years.
  
  # level_2_codes_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:LT"
  # level_2_codes_table <- level_2_codes_url %>%
  #   xml2::read_html() %>%
  #   rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  #   rvest::html_table(fill = TRUE)

  municipality_county_lookup <- tibble::tribble(
    ~region_level_2,          ~region_level_1,
    "Akmenės r. sav.",      "Šiaulių apskritis",
    "Alytaus m. sav.",      "Alytaus apskritis",
    "Alytaus r. sav.",      "Alytaus apskritis",
    "Anykščių r. sav.",       "Utenos apskritis",
    "Birštono sav.",        "Kauno apskritis",
    "Biržų r. sav.",    "Panevėžio apskritis",
    "Druskininkų sav.",      "Alytaus apskritis",
    "Elektrėnų sav.",     "Vilniaus apskritis",
    "Ignalinos r. sav.",       "Utenos apskritis",
    "Jonavos r. sav.",        "Kauno apskritis",
    "Joniškio r. sav.",      "Šiaulių apskritis",
    "Jurbarko r. sav.",     "Tauragės apskritis",
    "Kaišiadorių r. sav.",        "Kauno apskritis",
    "Kalvarijos sav.", "Marijampolės apskritis",
    "Kauno r. sav.",        "Kauno apskritis",
    "Kauno m. sav.",        "Kauno apskritis",
    "Kazlų Rūdos sav.", "Marijampolės apskritis",
    "Kėdainių r. sav.",        "Kauno apskritis",
    "Kelmės r. sav.",      "Šiaulių apskritis",
    "Klaipėdos r. sav.",    "Klaipėdos apskritis",
    "Klaipėdos m. sav.",    "Klaipėdos apskritis",
    "Kretingos r. sav.",    "Klaipėdos apskritis",
    "Kupiškio r. sav.",    "Panevėžio apskritis",
    "Lazdijų r. sav.",      "Alytaus apskritis",
    "Marijampolės sav.", "Marijampolės apskritis",
    "Mažeikių r. sav.",       "Telšių apskritis",
    "Molėtų r. sav.",       "Utenos apskritis",
    "Neringos sav.",    "Klaipėdos apskritis",
    "Pagėgių sav.",     "Tauragės apskritis",
    "Pakruojo r. sav.",      "Šiaulių apskritis",
    "Palangos m. sav.",    "Klaipėdos apskritis",
    "Panevėžio m. sav.",    "Panevėžio apskritis",
    "Panevėžio r. sav.",    "Panevėžio apskritis",
    "Pasvalio r. sav.",    "Panevėžio apskritis",
    "Plungės r. sav.",       "Telšių apskritis",
    "Prienų r. sav.",        "Kauno apskritis",
    "Radviliškio r. sav.",      "Šiaulių apskritis",
    "Raseinių r. sav.",        "Kauno apskritis",
    "Rietavo sav.",       "Telšių apskritis",
    "Rokiškio r. sav.",    "Panevėžio apskritis",
    "Šakių r. sav.", "Marijampolės apskritis",
    "Šalčininkų r. sav.",     "Vilniaus apskritis",
    "Šiaulių r. sav.",      "Šiaulių apskritis",
    "Šiaulių m. sav.",      "Šiaulių apskritis",
    "Šilalės r. sav.",     "Tauragės apskritis",
    "Šilutės r. sav.",    "Klaipėdos apskritis",
    "Širvintų r. sav.",     "Vilniaus apskritis",
    "Skuodo r. sav.",    "Klaipėdos apskritis",
    "Švenčionių r. sav.",     "Vilniaus apskritis",
    "Tauragės r. sav.",     "Tauragės apskritis",
    "Telšių r. sav.",       "Telšių apskritis",
    "Trakų r. sav.",     "Vilniaus apskritis",
    "Ukmergės r. sav.",     "Vilniaus apskritis",
    "Utenos r. sav.",       "Utenos apskritis",
    "Varėnos r. sav.",      "Alytaus apskritis",
    "Vilkaviškio r. sav.", "Marijampolės apskritis",
    "Vilniaus m. sav.",     "Vilniaus apskritis",
    "Vilniaus r. sav.",     "Vilniaus apskritis",
    "Visagino sav.",       "Utenos apskritis",
    "Zarasų r. sav.",       "Utenos apskritis",
    "nenustatyta",            "nenustatyta"
  )
  # 
  # level_2_lookup <- tibble::tibble(
  #   region_2_code = c("LT-01", "LT-02", "LT-03", "LT-04", "LT-05", 
  #                     "LT-06", "LT-07", "LT-08", "LT-09", "LT-10", "LT-11", "LT-12", 
  #                     "LT-13", "LT-14", "LT-16", "LT-15", "LT-17", "LT-18", "LT-19", 
  #                     "LT-21", "LT-20", "LT-22", "LT-23", "LT-24", "LT-25", "LT-26", 
  #                     "LT-27", "LT-28", "LT-29", "LT-30", "LT-31", "LT-32", "LT-33", 
  #                     "LT-34", "LT-35", "LT-36", "LT-37", "LT-38", "LT-39", "LT-40", 
  #                     "LT-41", "LT-42", "LT-44", "LT-43", "LT-45", "LT-46", "LT-47", 
  #                     "LT-48", "LT-49", "LT-50", "LT-51", "LT-52", "LT-53", "LT-54", 
  #                     "LT-55", "LT-56", "LT-57", "LT-58", "LT-59", "LT-60", NA_character_),
  #   osp_municipality_name = c("Akmenės r. sav.", 
  #                             "Alytaus m. sav.", "Alytaus r. sav.", "Anykščių r. sav.", 
  #                             "Birštono sav.", "Biržų r. sav.", "Druskininkų sav.", "Elektrėnų sav.", 
  #                             "Ignalinos r. sav.", "Jonavos r. sav.", "Joniškio r. sav.", 
  #                             "Jurbarko r. sav.", "Kaišiadorių r. sav.", "Kalvarijos sav.", 
  #                             "Kauno r. sav.", "Kauno m. sav.", "Kazlų Rūdos sav.", "Kėdainių r. sav.", 
  #                             "Kelmės r. sav.", "Klaipėdos r. sav.", "Klaipėdos m. sav.", 
  #                             "Kretingos r. sav.", "Kupiškio r. sav.", "Lazdijų r. sav.", 
  #                             "Marijampolės sav.", "Mažeikių r. sav.", "Molėtų r. sav.", 
  #                             "Neringos sav.", "Pagėgių sav.", "Pakruojo r. sav.", "Palangos m. sav.", 
  #                             "Panevėžio m. sav.", "Panevėžio r. sav.", "Pasvalio r. sav.", 
  #                             "Plungės r. sav.", "Prienų r. sav.", "Radviliškio r. sav.", 
  #                             "Raseinių r. sav.", "Rietavo sav.", "Rokiškio r. sav.", "Šakių r. sav.", 
  #                             "Šalčininkų r. sav.", "Šiaulių r. sav.", "Šiaulių m. sav.", 
  #                             "Šilalės r. sav.", "Šilutės r. sav.", "Širvintų r. sav.", 
  #                             "Skuodo r. sav.", "Švenčionių r. sav.", "Tauragės r. sav.", 
  #                             "Telšių r. sav.", "Trakų r. sav.", "Ukmergės r. sav.", "Utenos r. sav.", 
  #                             "Varėnos r. sav.", "Vilkaviškio r. sav.", "Vilniaus m. sav.", 
  #                             "Vilniaus r. sav.", "Visagino sav.", "Zarasų r. sav.", "nenustatyta"),
  #   region_1_code = c("LT-SA", "LT-AL", "LT-AL", "LT-UT", "LT-KU",
  #                     "LT-PN", "LT-AL", "LT-VL", "LT-UT", "LT-KU", #10
  #                     "LT-SA", "LT-TA", "LT-KU", "LT-MR", "LT-KU",
  #                     "LT-KU", "LT-MR", "LT-KU", "LT-SA", "LT-KL", #20
  #                     "LT-KL", "LT-KL", "LT-PN", "LT-AL", "LT-MR",
  #                     "LT-TE", "LT-UT", "LT-KL", "LT-TA", "LT-SA", #30
  #                     "LT-KL", "LT-PN", "LT-PN", "LT-PN", "LT-TE",
  #                     "LT-KU", "LT-SA", "LT-KU", "LT-TE", "LT-PN", #40 
  #                     "LT-MR", "LT-VL", "LT-SA", "LT-SA", "LT-TA",
  #                     "LT-KL", "LT-VL", "LT-KL", "LT-VL", "LT-TA", #50 
  #                     "LT-TE", "LT-VL", "LT-VL", "LT-UT", "LT-AL",
  #                     "LT-MR", "LT-VL", "LT-VL", "LT-UT", "LT-UT", NA_character_),
  #   `Subdivision name` = c("Akmenė", 
  #                          "Alytaus miestas", "Alytus", "Anykščiai", "Birštono", "Biržai", 
  #                          "Druskininkai", "Elektrėnai", "Ignalina", "Jonava", "Joniškis", 
  #                          "Jurbarkas", "Kaišiadorys", "Kalvarijos", "Kaunas", "Kauno miestas", 
  #                          "Kazlų Rūdos", "Kėdainiai", "Kelmė", "Klaipėda", "Klaipėdos miestas", 
  #                          "Kretinga", "Kupiškis", "Lazdijai", "Marijampolė", "Mažeikiai", 
  #                          "Molėtai", "Neringa", "Pagėgiai", "Pakruojis", "Palangos miestas", 
  #                          "Panevėžio miestas", "Panevėžys", "Pasvalys", "Plungė", 
  #                          "Prienai", "Radviliškis", "Raseiniai", "Rietavo", "Rokiškis", 
  #                          "Šakiai", "Šalčininkai", "Šiauliai", "Šiaulių miestas", 
  #                          "Šilalė", "Šilutė", "Širvintos", "Skuodas", "Švenčionys", 
  #                          "Tauragė", "Telšiai", "Trakai", "Ukmergė", "Utena", "Varėna", 
  #                          "Vilkaviškis", "Vilniaus miestas", "Vilnius", "Visaginas", "Zarasai", "unstated"),
  #   `Subdivision category` = c("district municipality", "city municipality", 
  #                              "district municipality", "district municipality", "municipality", 
  #                              "district municipality", "municipality", "municipality", "district municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "district municipality", "municipality", "district municipality", 
  #                              "city municipality", "municipality", "district municipality", 
  #                              "district municipality", "district municipality", "city municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "municipality", "municipality", "district municipality", "city municipality", 
  #                              "city municipality", "district municipality", "district municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "district municipality", "municipality", "district municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "city municipality", "district municipality", "district municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "district municipality", "district municipality", "district municipality", 
  #                              "district municipality", "city municipality", "district municipality", 
  #                              "municipality", "district municipality", NA_character_))
  # 
  # level_1_lookup <- tibble::tibble(level_1_region_code = c("LT-AL", "LT-KU", "LT-KL", "LT-MR", "LT-PN", 
  #                                                          "LT-SA", "LT-TA", "LT-TE", "LT-UT", "LT-VL", NA_character_),
  #                                  region_level_1 = c("Alytaus apskritis", 
  #                                                     "Kauno apskritis", "Klaipėdos apskritis", "Marijampolės apskritis", 
  #                                                     "Panevėžio apskritis", "Šiaulių apskritis", "Tauragės apskritis", 
  #                                                     "Telšių apskritis", "Utenos apskritis", "Vilniaus apskritis", "nenustatyta"),
  #                                  region_level_1_en = c("Alytus County", "Kaunas County", 
  #                                                        "Klaipėda County", "Marijampolė County", "Panevėžys County", 
  #                                                        "Šiauliai County", "Tauragė County", "Telšiai County", "Utena County", 
  #                                                        "Vilnius County", "unstated"))
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

  cases_wider <- left_join(cases_data,municipality_county_lookup, by=c("region_level_2")) %>%
    select(date, region_level_1, region_level_2, #level_2_region_code=region_2_code,
           cases_new, cases_total, deaths_new, deaths_total, recovered_new, recovered_total)
    
    ## This is the list of fields which we're trying to generate, copied from get_regional_data.R
    # date, region_level_2, level_2_region_code, region_level_1, level_1_region_code, 
    # cases_new, cases_total, deaths_new, deaths_total,
    # recovered_new, recovered_total, hosp_new, hosp_total,
    # tested_new, tested_total, dplyr::everything())

  # The French code for loading hospital data is being kept here as a guide
  # for when it is added for Lithuania.
  
  # hosp_data <- csv_reader(file = hosp_url) %>%
  #   dplyr::select(date = jour,
  #                 level_2_region_code = dep,
  #                 hosp_new = incid_hosp,
  #                 deaths_new = incid_dc) %>%
  #   dplyr::mutate(date = lubridate::as_date(lubridate::ymd(date)))

  data <- cases_wider
  return(data)
}
