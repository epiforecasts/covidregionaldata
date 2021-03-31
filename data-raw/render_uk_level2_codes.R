# make uk authority table
authority_data <- vroom::vroom(
  "https://opendata.arcgis.com/datasets/72e57d3ab1054e169c55afff3c9c1aa4_0.csv", # nolint
  col_types = c(
    WD17NMW = col_character(),
    CTY17CD = col_character(),
    CTY17NM = col_character()
  )
)

unitary_auth <- authority_data %>%
  dplyr::select(
    level_2_region_code = "CTY17CD",
    region_level_2 = "CTY17NM",
    level_1_region_code = "GOR10CD",
    region_level_1 = "GOR10NM"
  ) %>%
  dplyr::distinct() %>%
  tidyr::drop_na(.data$region_level_2)

upper_tier_auth <- authority_data %>%
  dplyr::select(
    level_2_region_code = "LAD17CD",
    region_level_2 = "LAD17NM",
    level_1_region_code = "GOR10CD",
    region_level_1 = "GOR10NM"
  ) %>%
  dplyr::distinct() %>%
  tidyr::drop_na(.data$region_level_2)

country_auth <- authority_data %>%
  dplyr::select(
    level_2_region_code = "LAD17CD",
    region_level_2 = "LAD17NM",
    level_1_region_code = "CTRY17CD",
    region_level_1 = "CTRY17NM"
  ) %>%
  dplyr::filter(.data$region_level_1 %in% c(
    "Northern Ireland",
    "Scotland",
    "Wales"
  )) %>%
  dplyr::distinct() %>%
  tidyr::drop_na(.data$region_level_2)

other_auths <- tibble::tibble(
  level_2_region_code = c("E06000058", "E06000052", "E09000012"),
  region_level_2 = c(
    "Bournemouth, Christchurch and Poole",
    "Cornwall and Isles of Scilly",
    "Hackney and City of London"
  ),
  level_1_region_code = c(rep("E92000001", 3)),
  region_level_1 = c("South West", "South West", "London")
)

# Join tables ---------------------------------------------------
authority_lookup_table <- dplyr::bind_rows(
  unitary_auth,
  upper_tier_auth,
  country_auth,
  other_auths
)

uk_authority_lookup_table <- authority_lookup_table %>%
  dplyr::arrange(.data$level_1_region_code) %>%
  dplyr::distinct(.data$level_2_region_code,
    .data$region_level_2,
    .keep_all = TRUE
  )

# update package region_codes
usethis::use_data(uk_authority_lookup_table, overwrite = TRUE)
