france_level_1_codes <- tibble::tibble(
  iso_code = c(
    "FR-ARA", "FR-BFC", "FR-BRE", "FR-CVL", "FR-20R", "FR-GES", "FR-GP",
    "FR-GF", "FR-HDF", "FR-IDF", "FR-RE", "FR-MQ", "FR-YT", "FR-NOR",
    "FR-NAQ", "FR-OCC", "FR-PDL", "FR-PAC", "FR-PM", "FR-BL", "FR-MF"
  ),
  insee_code = c(
    "84", "27", "53", "24", "94", "44", "01", "03", "32", "11", "04", "02",
    "06", "28", "75", "76", "52", "93", "05", "07", "08"
  ),
  level_1_region = c(
    "Auvergne-Rhone-Alpes", "Bourgogne-Franche-Comte", "Bretagne",
    "Centre-Val de Loire", "Corse", "Grand-Est", "Guadeloupe",
    "Guyane (francaise)", "Hauts-de-France", "Ile-de-France",
    "La Reunion", "Martinique", "Mayotte", "Normandie", "Nouvelle-Aquitaine",
    "Occitanie", "Pays-de-la-Loire", "Provence-Alpes-Cote-d'Azur",
    "Saint-Barthelemy", "Saint-Martin", "Saint-Pierre et Miquelon"
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
  level_1_region = c(
    "Saint-Barthelemy", "Saint-Martin",
    "Saint-Pierre et Miquelon"
  ),
  level_2_region = level_1_region
)

france_level_2_codes <- france_level_2_codes_table[[2]] %>%
  dplyr::select(
    level_2_region_code = Code,
    level_2_region = `Subdivision name`,
    level_1_region_code = `In region(since 2016)`
  ) %>%
  dplyr::mutate(level_1_region_code = paste0("FR-", level_1_region_code)) %>%
  dplyr::left_join(france_level_2_codes_table[[1]], by = c(
    "level_1_region_code" = "Code"
  )) %>%
  dplyr::rename(level_1_region = `Subdivision name (fr)`) %>%
  dplyr::select(-`Subdivision category`) %>%
  dplyr::bind_rows(france_missing_level_2_codes)

france_codes <- left_join(
  france_level_2_codes,
  france_level_1_codes %>%
    select(-level_1_region),
  by = c("level_1_region_code" = "iso_code")
)

# update package region_codes
usethis::use_data(france_codes, overwrite = TRUE)
