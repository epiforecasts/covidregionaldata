# global region codes
global_codes <- tibble::tibble(
  country = c("who", "ecdc"),
  level = c("level_1_region"),
  name = c("iso_code"),
  codes = NULL
)

# mexico region codes
mexico_codes <- readr::read_csv("data-raw/mexico_codes.csv")
mexico_codes <- tibble::tibble(
  country = "mexico",
  level = c("level_1_region", "level_2_region"),
  name = c("iso_3166_2", "inegi_code"),
  codes = list(
    dplyr::select(mexico_codes, region_level_1, iso_code) %>%
      unique(),
    mexico_codes
  )
)

# italy codes
italy_codes <- readr::read_csv("data-raw/italy_codes.csv")
italy_codes <- tibble::tibble(
  country = "italy",
  level = c("level_1_region"),
  name = c("iso_3166_2"),
  codes = list(
    italy_codes
  )
)

# germany codes
level_1_germany <- tibble::tibble(
  level_1_region_code = c(
    "DE-BB", "DE-BE", "DE-BW", "DE-BY", "DE-HB", "DE-HE", "DE-HH", "DE-MV",
    "DE-NI", "DE-NW", "DE-RP", "DE-SH", "DE-SL", "DE-SN", "DE-ST", "DE-TH"
  ),
  region_level_1 = c(
    "Brandenburg", "Berlin", "Baden-W\u00FCrttemberg", "Bayern", "Bremen",
    "Hessen", "Hamburg", "Mecklenburg-Vorpommern", "Niedersachsen",
    "Nordrhein-Westfalen",
    "Rheinland-Pfalz", "Schleswig-Holstein", "Saarland", "Sachsen",
    "Sachsen-Anhalt", "Th\u00FCringen"
  )
)
germany_codes <- tibble::tibble(
  country = "germany",
  level = c("level_1_region", "level_2_region"),
  name = c("iso_3166_2", "code"),
  codes = list(
    level_1_germany,
    dplyr::mutate(
      level_1_germany,
      level_2_region_code = NA,
    )
  )
)

# france codes

france_level_1_codes <- tibble::tibble(
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

# Get region names and codes
france_level_2_codes_url <- "https://en.wikipedia.org/wiki/ISO_3166-2:FR"
france_level_2_codes_table <- france_level_2_codes_url %>%
  xml2::read_html() %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  rvest::html_table(fill = TRUE)

france_missing_level_2_codes <- tibble::tibble(
  level_1_region_code = c("FR-PM", "FR-BL", "FR-MF"),
  level_2_region_code = c("FR-977", "FR-978", "FR-975"),
  region_level_1 = c("Saint-Barthelemy", "Saint-Martin", "Saint-Pierre et Miquelon"),
  region_level_2 = region_level_1
)

france_level_2_codes <- france_level_2_codes_table[[2]] %>%
  dplyr::select(
    level_2_region_code = Code,
    region_level_2 = `Subdivision name`,
    level_1_region_code = `In region(since 2016)`
  ) %>%
  dplyr::mutate(level_1_region_code = paste0("FR-", level_1_region_code)) %>%
  dplyr::left_join(france_level_2_codes_table[[1]], by = c("level_1_region_code" = "Code")) %>%
  dplyr::rename(region_level_1 = `Subdivision name (fr)`) %>%
  dplyr::select(-`Subdivision category`) %>%
  dplyr::bind_rows(france_missing_level_2_codes)


france_codes <- tibble::tibble(
  country = "france",
  level = c("level_1_region", "level_2_region"),
  name = c("iso_3166_2", "code"),
  codes = list(
    france_level_1_codes,
    france_level_2_codes
  )
)

# add additional regions in the same format and bind together
region_codes <- purrr::reduce(
  list(
    global_codes, mexico_codes,
    france_codes,
    italy_codes, germany_codes
  ),
  dplyr::bind_rows
)

# update package region_codes
usethis::use_data(region_codes, overwrite = TRUE)
