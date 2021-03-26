# global region codes
global_codes <- tibble::tibble(
  country = c("who", "ecdc"),
  level = c("level_1_region"),
  name = c("iso_code"),
  codes = NULL
)

# mexico region codes
mexico_codes <- vroom::vroom("data-raw/mexico_codes.csv")
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
italy_codes <- vroom::vroom("data-raw/italy_codes.csv")
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


# add additional regions in the same format and bind together
region_codes <- purrr::reduce(
  list(
    global_codes, mexico_codes,
    italy_codes, germany_codes
  ),
  dplyr::bind_rows
)

# update package region_codes
usethis::use_data(region_codes, overwrite = TRUE)
